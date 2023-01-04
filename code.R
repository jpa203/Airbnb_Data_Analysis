# Airbnb Project


## Exploratory Data Analysis

Libraries
```{r, warning= FALSE, results = 'hide', message= FALSE}
library(tidytext)
library(tidyverse)
library(naniar)
library(zoo)
library(plotly)
library(stringr)
library(reshape)
library(arules)
library(caret)
library(leaps)
library(forecast)
library(e1071)
library(rpart)
library(rpart.plot)
library(FNN)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(factoextra)
library(Metrics)
library(ggmap)
library(randomForest)
library(leaflet)
```

## Reading in the dataset

```{r, warning= FALSE}

df <- read_csv('/Users/jazzopardi/Desktop/R/AirBnB Project/barcelona.csv')

# filtering data set for neighborhood

df1 <- df %>%
  filter(host_neighbourhood == 'Sants-Montjuïc')

dim(df1) # 444, 75
```

## Dealing with missing values

```{r, warning= FALSE}
miss_var_summary(df1)

# A decision was made to remove some columns with over 20% missingness. Imputation
# will be used on columns with > 20% missingness that are relevant to the analysis
# and which are numerical, if necessary.

df1[, c("bathrooms", "calendar_updated", "license", "neighborhood_overview", 
        "neighbourhood","host_about","first_review","last_review")]<- list((NULL))

# Host Location is redundant

df1$host_location <- NULL

# We will now remove rows where names and description (both categorical) have NA values

df1[is.na(df1$description),] # we can use the IDs from these columns to help subset
df1[is.na(df1$name),] # we can use the IDs from these columns to help subset

df1 <- subset(df1, !(df1$id == '13683831'), drop = TRUE )
df1 <- subset(df1, !(df1$id == '30636736'), drop = TRUE )

# We are now left with 10 columns that have NA values. We will impute the median for these values

dim(df1) # 442, 66

# Imputing using median - mean is sensitive to outliers

df2 <- df1 %>%
  mutate(across(where(is.numeric), na.aggregate, FUN = median)) %>%
  ungroup

miss_var_summary(df2)

anyNA(df2) # FALSE

```

```{r, warning= FALSE}

# Timet to get rid of some other columns- important since we're dealing with a small dataset
# and a lot of columns (curse of dimensionality)

df2[, c("listing_url", "scrape_id", "last_scraped", "source", 
        "picture_url","host_id","host_url","host_thumbnail_url",
        "host_picture_url", "host_neighbourhood", "neighbourhood_cleansed",
        "calendar_last_scraped")]<- list((NULL))
```


```{r, warning= FALSE}
plt <- as.data.frame(sort(table(df2$host_response_rate), decreasing = TRUE)[1:5])

plt2 <- as.data.frame(sort(table(df2$host_response_time), decreasing = TRUE)[1:5])

plt3 <- as.data.frame(sort(table(df2$host_acceptance_rate), decreasing = TRUE)[1:5])

plot_ly(labels = ~Var1, values = ~Freq, legendgroup = ~Var1,
        textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(data = plt, textposition = 'inside',name = "DF1", domain = list(row = 0, column = 0))%>%
  add_pie(data = plt2, textposition = 'inside', name = "DF2", domain = list(row = 0, column = 1))%>%
  add_pie(data = plt3, textposition = 'inside', name = "DF3", domain = list(row = 1, column = 0))%>%
  layout(title = "NA Values", showlegend = F,
         grid=list(rows=2, columns=3),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

```{r, warning= FALSE}
# We need to deal with these

str(df2$host_response_rate)

length(df2$host_response_rate[df2$host_response_rate == 'N/A']) # 107 columns

length(df2$host_response_time[df2$host_response_rate == 'N/A']) # 107 columns

length(df2$host_acceptance_rate[df2$host_acceptance_rate == 'N/A']) # 90 columns

length(df2$host_response_time[(df2$host_response_rate == 'N/A') & 
                                (df2$host_acceptance_rate == 'N/A') ]) # 89 columns

# It makes sense to remove these columns, since response rate is likely to be an important
# factor, we shouldn't impute these values - will let group decide

df3 <- subset(df2, !(df2$host_response_rate == 'N/A'), drop = TRUE)
```


```{r, warning= FALSE}
df3[, c("minimum_nights", "maximum_nights","minimum_minimum_nights",
        "maximum_minimum_nights","minimum_maximum_nights" ,
        "maximum_maximum_nights", "number_of_reviews_ltm", "number_of_reviews_l30d",
        "calculated_host_listings_count","calculated_host_listings_count_entire_homes",
        "calculated_host_listings_count_private_rooms","calculated_host_listings_count_shared_rooms",
        "host_listings_count","availability_60, availability_90, availability_365", "id", 
        "host_total_listings_count")]<- list((NULL))

df3$reviews_per_month <- round(df3$reviews_per_month) # we will try keep eveerything to 2 decimel places
```
 
 
## Summary Statistics

```{r, warning= FALSE}

#1.
#mean
mean(df3$accommodates,na.rm = TRUE)

# 2. 

#convert currency to numerical 

df3$price <- as.numeric(gsub('[$,]', '', df3$price))


#Quantiles/percentiles 

print(quantile(df3$price,prob=c(.25,.75),na.rm=TRUE))

# 3. 

#count superhost

room <- df3 %>% group_by(room_type) %>% summarise(median_score = median(review_scores_rating))

room

# 4

#xtabs

print(xtabs(~host_response_time+host_is_superhost,data=df3))

# 5

# host_name

sort(table(df3$host_name), decreasing = TRUE)[1:10]

host_name <- df3 %>% group_by(host_name) %>% summarise(total_count=n(),mean = round(mean(price),2)) %>%
  arrange(desc(total_count)) 

print(host_name)


```

# Data Visualization

## Heat Map

```{r, warning= FALSE}
str(df3)

# First converting some columns into numeric 

df3$price <- str_replace_all(df3$price, '[$]', '')
df3$price <- str_replace_all(df3$price, ',', '')

df3$price <- as.numeric(df3$price)

summary(df3$price)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.0    50.0    87.0   118.5   138.5  1000.0 

# Now to create a heat map

nums <- unlist(lapply(df3, is.numeric), use.names = FALSE)  

hmap <- df3[ ,nums]

hmap[ , c("longitude", "latitude")] <- list(NULL)


# to generate input for the plot

cor.mat <- round(cor(hmap), 2) 

# rounded correlation matrix

melted.cor.mat <- melt(cor.mat)

hmap_final <- ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile(color = "white",
            lwd = 1,
            linetype = 1) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ))


hmap_final + labs(x = '', y= '') + ggtitle('Heat Map for Numeric Variables')
```

## Relationship between Price and Accomodation

```{r, warning= FALSE}

ggplot(df3,aes(x = accommodates, y = price))+geom_point(color = 'red') + geom_smooth(color = 'green') + 
  ggtitle('Relationship between Price and Accomodation')+xlab("Accommodates")+ ylab("Price")

```


## Scatterplot


```{r, warning= FALSE}
# Scatterplot

scatter <- ggplot(df3, aes(x = number_of_reviews, y = review_scores_rating, color = host_is_superhost)) + geom_point()

scatter + labs(x = 'Number of Reviews', y = 'Rating' , color = 'Superhost') + ggtitle('Scatterplot of Reviews vs Rating')
```



## Amenities


```{r, warning= FALSE}

amen <- as.data.frame(gsub("\\[|\\]", "", df3$amenities))

colnames(amen) <- 'Text'

amen$Text <- gsub('"', '', amen$Text)

y <- unlist(strsplit(amen$Text,","))

length(y) # 6538

amen_df <- data.frame(sort(table(y), decreasing  =  TRUE)[1:10])

ggplot(amen_df, aes(x=y, y= Freq)) + 
  geom_point(size=4, col = 'orange') + 
  geom_segment(aes(x= y, 
                   xend=y, 
                   y=0, 
                   yend= Freq)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + ylab("Count") + xlab('Amenities') + ggtitle('Most Popular Amenities')
```


## Response Time vs Price


```{r, warning= FALSE}
x <- df3$price

y <- df3$host_response_rate

xy <- data.frame(x,y)

View(xy)

xy$y <- as.numeric(sub("%", "", xy$y))

xy <- xy %>% mutate(new_bin = cut(y, breaks = c(-1,50,75,90,100), 
                                  labels = c('Very Low','Low','Medium','High')))

mean_xy <- xy %>%
  group_by(new_bin) %>%
  summarise(mean_x =mean(x))

plt_xy <-  data.frame(mean_xy)

View(plt_xy)

G4<-ggplot(plt_xy,aes(x=new_bin,y=mean_x))+geom_col(fill='yellowgreen')

G4 + labs(x = 'Response Time', y= 'Price') + ggtitle('Relationship between Response Time and Price')

```


# Mapping

```{r, warning= FALSE}

df1$price <- as.numeric(gsub('[$,]', '', df1$price))

## set map in nbhood
height<-max(df1$latitude)-min(df1$latitude)
width<-max(df1$longitude)-min(df1$longitude)
smborder<- c(bottom  = min(df1$latitude)  - 0.1 * height, 
             top     = max(df1$latitude)  + 0.1 * height,
             left    = min(df1$longitude) - 0.1 * width,
             right   = max(df1$longitude) + 0.1 * width)
mymap<-get_stamenmap(smborder,zoom=10,maptype ='toner-lite')
# mymap<-get_stamenmap(smborder,zoom=10,maptype ='terrain')
mymap
ggmap(mymap)

##price variance in mapping 
ggmap(mymap) +
geom_point(data = df1, mapping = aes(x = longitude, y = latitude,
                                               col = price)) +
          scale_color_distiller(palette = "Dark2", direction = 1)

 nbhood_mean1 <- df1 %>% 
   group_by(host_neighbourhood, room_type) %>% 
   summarise(
     latitude = mean(latitude, na.rm = TRUE),
     longitude = mean(longitude, na.rm = TRUE),
     review_scores_rating=mean(review_scores_rating,na.rm=TRUE),
     number_of_reviews = sum(number_of_reviews, na.rm = TRUE),
     listings = n(),
     price = mean(price, na.rm = TRUE)) %>%
   ungroup() %>%
   arrange(price)
 
 head(nbhood_mean1)
 View(nbhood_mean1)
```
```{r}


df_map <- df1[ , c('latitude', 'longitude', 'price')]

pal <- colorNumeric("plasma", df_map$price)



map <- leaflet() %>% 
  addTiles() %>%  
  addCircles(lng= df_map$longitude,
             lat= df_map$latitude,
             color = pal(df_map$price),
             label = df_map$price) %>%
  addLegend("topleft",
            pal = pal,
            values = df_map$price)




map


```


# Word Cloud


```{r, warning= FALSE}



text <- df2$description
doc <- Corpus(VectorSource(text))
doc <- doc %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeWords, stopwords("spanish"))
doc <- tm_map(doc, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(doc) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df4 <- data.frame(word = names(words),freq=words)
wordcloud2(data=df4, size=1.6, color='random-dark')

```

```{r, warning= FALSE}
df3[ ,c("name", "description" , "host_name", "latitude", "longitude", "reviews_per_month",
                       "availability_60", "availability_90", "availability_365")] <- list((NULL))

data_analysis <- df3

```

```{r, warning= FALSE}
data_analysis <- subset(data_analysis, neighbourhood_group_cleansed =='Sants-Montjuïc')

data_analysis$host_acceptance_rate[158] <- '98%' # for some reason R would not read this value, even though it did on other laptops.

knn <- data_analysis

dtree <- data_analysis

clust <- data_analysis
```

## Prediction

```{r, warning= FALSE}
# Step 1 - Use Domain Knowledge to filter predictors

data_analysis$host_since <- format(as.Date(data_analysis$host_since, format="%Y-%m-%d"),"%Y")

data_analysis$host_response_time <- NULL # same as host response rate

data_analysis$amenities <- NULL # high cardinality

data_analysis$host_verifications <- NULL # all one variable

data_analysis$host_has_profile_pic <- NULL # all one variable 

data_analysis$host_identity_verified <- NULL # all one variable

data_analysis$neighbourhood_group_cleansed <- NULL # all one variable 

data_analysis$property_type <- NULL # there is room type

data_analysis$has_availability <- NULL # all have availability

data_analysis$bathrooms_text <- NULL
```

```{r, warning= FALSE}
nums <- unlist(lapply(data_analysis, is.numeric), use.names = FALSE)  

# checking for correlation

hmap2 <- data_analysis[ ,nums]

cor.mat2 <- round(cor(hmap2), 2) 

melted.cor.mat <- melt(cor.mat2)

hmap_plot <- ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() + geom_text(aes(x = X1, y = X2, label = value)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
      ))

hmap_plot + labs(x = '', y= '')

data_analysis$bedrooms <- NULL # drop columns that are highly correlated with one another 

data_analysis$beds <- NULL # drop columns that are highly correlated with one another

data_analysis <- mutate(data_analysis, mean_review_score = rowMeans(
  select(data_analysis,c(review_scores_accuracy,review_scores_rating,
  review_scores_cleanliness, review_scores_checkin,review_scores_communication,
  review_scores_location,review_scores_value)))) # create a new variable called reviews


data_analysis$host_acceptance_rate <- as.numeric(sub("%", "", data_analysis$host_acceptance_rate))

data_analysis$host_acceptance_rate <- discretize(data_analysis$host_acceptance_rate, method = 'fixed', breaks = c(-Inf, 40, 70, Inf), labels = c('slow', 'medium', 'fast'))

data_analysis$host_response_rate <- as.numeric(sub("%", "", data_analysis$host_response_rate))

data_analysis$host_response_rate <- discretize(data_analysis$host_response_rate, method = 'fixed', breaks = c(-Inf, 40, 70, Inf), labels = c('slow', 'medium', 'fast'))

review_score <- data_analysis$review_scores_rating

data_analysis[ , c("review_scores_rating", "review_scores_accuracy",
                   "review_scores_cleanliness","review_scores_checkin",
                   "review_scores_communication", "review_scores_location",
                   "review_scores_value")] <- list(NULL) # multicolinearity  



nums <- unlist(lapply(data_analysis, is.numeric), use.names = FALSE)  

hmap2 <- data_analysis[ ,nums]

cor.mat2 <- round(cor(hmap2), 2) 

melted.cor.mat <- melt(cor.mat2)

hmap_plot <- ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() + geom_text(aes(x = X1, y = X2, label = value)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ))

hmap_plot + labs(x = '', y= '')

data_analysis <- data_analysis[!data_analysis$price == '1000', ]

relation <- ggplot(data = data_analysis, aes(x = price, y = accommodates)) + geom_point(col = 'red')

relation + stat_smooth(method = "lm",formula = y ~ poly(x, 2),se = FALSE, col = 'blue')
```


```{r, warning= FALSE}
# split into train/test to run model

set.seed(41)

# we create a data partition because room type only has one variable called 'HotelRoom' so

trainIndex <- createDataPartition(data_analysis$room_type, p = .6,
                                  list = FALSE,
                                  times = 1)

train <- data_analysis[ trainIndex,]
test <- data_analysis[-trainIndex,]  

# checking

ggplot(train, aes(price)) + geom_density(fill="orange")
ggplot(train, aes(log(price))) + geom_density(fill="brown")
ggplot(train, aes(sqrt(price))) + geom_density(fill="blue")

# we can see that the data isn't normally distributed - this violates one of the four principles of linear regression 

# The data is skewed, so we need to transform it, log transforming data usually has the effect of spreading out the clumps
# of data and bringing together spread-out data

# without log transformation

lm.model <- lm(price ~ . , data = train)

summary(lm.model) # residuals appear right skewed

plot(lm.model)

#with log

lm.log <- lm(log(price) ~. , data = train)

summary(lm.log)

plot(lm.log)

# run stepwise regression

step <- step(lm.log, direction = 'both')

summary(step)
```

```{r, warning= FALSE}
lm.log2 <- lm(log(price) ~ minimum_nights_avg_ntm + accommodates +
                room_type, data = train) # keeping variables that show statistical significance 

summary(lm.log2)

hist(residuals(lm.log2), col = "orange", main = 'Histogram') # we can see that our residuals are normally distributed here, which means the error is low

{plot(fitted(lm.log2), residuals(lm.log2), xlab = 'Fitted', ylab = 'Residuals') 

abline(h = 0, lty = 2, col = 'red')}

# adding square terms to check for nonlinear 

lm.log3 <- lm(log(price) ~ accommodates + minimum_nights_avg_ntm + room_type + I(accommodates^2) 
              + I(minimum_nights_avg_ntm^2), data = train)

summary(lm.log3)

lm.log4 <- lm(log(price) ~ accommodates + minimum_nights_avg_ntm + room_type 
              + I(minimum_nights_avg_ntm^2), data = train)

summary(lm.log4)

(exp(0.17928835) - 1) * 100 # 1 unit change in accommodates results in a 20% increase in price

# exponentiation is the inverse of a logarithm function

pred1 <- exp(predict(lm.log4, newdata = train))

pred2 <- exp(predict(lm.log4, newdata = test)) # making predictions on test data

forecast::accuracy(pred1, train$price)

forecast::accuracy(pred2, test$price)

residuals <- test$price - pred2

table <- data.frame("Predicted" = pred2, "Actual" = test$price, 'Residuals' = residuals)

{plot(test$price, pred2, col = c('red', 'green'), xlab = 'Real', ylab = 'Predicted', pch = c(4,4))

legend("topright", legend=c("Real", "Predicted"),
       col=c("red", "green"),pch =c(4,4))}

# example with new listing

new_listing <- data.frame('accommodates' = 3, 'minimum_nights_avg_ntm' = 4, 'room_type'= 'Private room')

test_list <- exp(predict(lm.log4, newdata = new_listing ))



```


# Classification

## kNN

```{r, warning= FALSE}
# knn

sample(y, 1) # Dishwasher

knn$amenities <- ifelse(grepl("Dishwasher",knn$amenities),"Yes","No") # Dishwasher

View(knn)

knn_nums <- unlist(lapply(knn, is.numeric), use.names = FALSE)  

amenities <- knn$amenities

knn <- knn[, knn_nums]

View(knn)

knn <- mutate(knn, mean_review_score = rowMeans(select(knn,c(review_scores_accuracy,review_scores_rating,
                         review_scores_cleanliness, review_scores_checkin,review_scores_communication,
                         review_scores_location,review_scores_value)))) # create a new variable called reviews


knn[ , c("review_scores_rating", "review_scores_accuracy",
                   "review_scores_cleanliness","review_scores_checkin",
                   "review_scores_communication", "review_scores_location",
                   "review_scores_value")] <- list(NULL) # multicollinearity  

knn$maximum_nights_avg_ntm <- NULL

knn$amenities <- factor(amenities)


set.seed(41) # dishwasher

sample <- sample(c(TRUE, FALSE), nrow(knn), replace=TRUE, prob=c(0.7,0.3))
train  <- knn[sample, ]
test   <- knn[!sample, ]

train.norm.df <- train
test.norm.df <- test

norm.values <- preProcess(train[, 1:8], method=c("center", "scale")) #  

View(train.norm.df)
train.norm.df[, 1:8] <- predict(norm.values, train[ ,1:8])
test.norm.df[, 1:8] <- predict(norm.values, test[ ,1:8])

nn <- knn(train.norm.df[ ,1:8],test.norm.df[ ,1:8], cl = train.norm.df$amenities,
          k = 3)

test_res <- data.frame(test.norm.df)

class_comparison <- data.frame(nn, test_res$amenities)

names(class_comparison) <- c("Predicted", "Real")

confusionMatrix(as.factor(class_comparison$Predicted), as.factor(class_comparison$Real))

# choosing optimal k using Caret

accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

for(i in 1:20) {
  knn.pred <- knn(train.norm.df[, 1:8], test.norm.df[, 1:8],
                  cl = train.norm.df$amenities, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, test.norm.df$amenities)$overall[1]
}

accuracy.df

par(mfrow = c(1,1))

ggplot(data = accuracy.df, aes(x = k, y = accuracy, group = 1)) + geom_line(color = "red") + geom_point()

nn_tuned <- knn(train.norm.df[ ,1:8],test.norm.df[ ,1:8], cl = train.norm.df$amenities,
          k = 4)

test_res <- data.frame(test.norm.df)

class_comparison <- data.frame(nn_tuned, test_res$amenities)

names(class_comparison) <- c("Predicted", "Real")

confusionMatrix(as.factor(class_comparison$Predicted), as.factor(class_comparison$Real))

apt <- data.frame(accommodates = 4, bedrooms = 2, beds = 2, price = 200,
                  minimum_avg_ntm = 10.0, availability_30 = 5, number_of_reviews = 250,
                  mean_review_score = 4.89)


knn.new.pred <- knn(train.norm.df[ ,1:8], apt , cl = train.norm.df$amenities,
                    k = 4)

knn.new.pred[1] # no dishwasher

```

## Naive Bayes

```{r, warning= FALSE}
nBayesfeatures <- lm(instant_bookable ~. , data = data_analysis)

# run stepwise regression - in hindsight this isn't the approach for such a dependent variable

step <- step(nBayesfeatures, direction = 'both')

summary(nBayesfeatures)


nBayes <- data_analysis[ , c("host_since", "host_response_rate","host_acceptance_rate", "host_is_superhost", "room_type",
                           "accommodates", "price", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm","availability_30",
                           "number_of_reviews", "mean_review_score", "instant_bookable")]

nBayes$accommodates <- discretize(nBayes$accommodates, method = 'fixed', breaks = c(-Inf, 3, 6, Inf), labels = c('few', 'medium', 'many'))

nBayes$number_of_reviews <- discretize(nBayes$number_of_reviews, method = 'fixed', breaks = c(-Inf, 10, 100, Inf), labels = c('few', 'medium', 'many'))

nBayes$price <- discretize(nBayes$price, method = 'fixed', breaks = c(-Inf, 50, 140, Inf), labels = c('low', 'medium', 'high'))

nBayes$mean_review_score<-cut(nBayes$mean_review_score, method = 'fixed', breaks=c(-Inf,4.63,4.8,Inf), labels=c('low','average','high'))

nBayes$minimum_nights_avg_ntm <- discretize(nBayes$minimum_nights_avg_ntm, method = 'fixed', breaks = c(1, 20, 32, 300),
                                          labels = c('few', 'medium', 'many'))

nBayes$maximum_nights_avg_ntm <- discretize(nBayes$maximum_nights_avg_ntm, method = 'fixed', breaks = c(7, 330, 706, 1125),
                                          labels = c('few', 'medium', 'many'))


nBayes$instant_bookable<-as.factor(nBayes$instant_bookable)
nBayes$host_since <- as.factor(nBayes$host_since)
nBayes$number_of_reviews<-as.factor(nBayes$number_of_reviews)
nBayes$accommodates<-as.factor(nBayes$accommodates)
nBayes$room_type<-as.factor(nBayes$room_type)
nBayes$price<-as.factor(nBayes$price)
nBayes$host_response_rate<-as.factor(nBayes$host_response_rate)
nBayes$host_acceptance_rate<-as.factor(nBayes$host_acceptance_rate)
nBayes$host_is_superhost<-as.factor(nBayes$host_is_superhost)
nBayes$minimum_nights_avg_ntm<-as.factor(nBayes$minimum_nights_avg_ntm)
nBayes$availability_30<-as.factor(nBayes$availability_30)
nBayes$maximum_nights_avg_ntm<-as.factor(nBayes$maximum_nights_avg_ntm)
nBayes$mean_review_score<-as.factor(nBayes$mean_review_score)

set.seed(41)

trainIndex <- createDataPartition(nBayes$room_type, p = .6,
                                  list = FALSE,
                                  times = 1)

train <- nBayes[ trainIndex,]
test <- nBayes[-trainIndex,]


#creating model

naive.model <- naiveBayes(instant_bookable ~. , data = train) 
                                                                            
#creating a confusion matrix

naive.model

train_pred <- predict(naive.model, newdata = train)
confusionMatrix(train_pred, train$instant_bookable)
 
#against validation

test_pred  <-predict(naive.model, newdata = test)
confusionMatrix(test_pred, test$instant_bookable)


# fictional apartment 

bigmike_apt <- data.frame(host_since = "2017", host_response_rate = "medium", host_acceptance_rate = "fast",
                        host_is_superhost = "TRUE", accommodates = "medium", room_type = "Entire home/apt",
                        minimun_nights_avg_ntm = "few", maximum_nights_avg_ntm = "medium",
                        availability_30 = "7", number_of_reviews = "many", price = "119",
                        mean_review_score = "average")

predict1 <- predict(naive.model, newdata = bigmike_apt)


predict2 <- predict(naive.model, newdata = bigmike_apt, type = "raw")

```

## Decision Tree

```{r, warning= FALSE}
dtree[ , c("review_scores_accuracy",
                   "review_scores_cleanliness","review_scores_checkin",
                   "review_scores_communication", "review_scores_location",
                   "review_scores_value")] <- list(NULL)

set.seed(41)

trainIndex <- createDataPartition(dtree$room_type, p = .6,
                                  list = FALSE,
                                  times = 1)

train <- dtree[ trainIndex,]
test <- dtree[-trainIndex,]

rf <- randomForest(review_scores_rating ~ ., data = train, ntree = 500,
                   mtry = 4, nodesize = 1, importance = TRUE)

## variable importance plot
varImpPlot(rf, type = 2)

dtree$host_identity_verified <- NULL

dtree$bathrooms_text <- NULL

dtree$amenities <- NULL

dtree$property_type <- NULL

dtree$instant_bookable <- NULL

dtree$has_availability <- NULL

dtree$host_has_profile_pic <- NULL

dtree$neighbourhood_group_cleansed <- NULL

# feature engineering

dtree$host_since <- format(as.Date(dtree$host_since, format="%Y-%m-%d"),"%Y")

dtree$host_since <- as.numeric(dtree$host_since)

dtree$host_since <- discretize(dtree$host_since, method = 'fixed', breaks = c(-Inf, 2013, 2017, 2019),
                                           labels = c('long', 'medium', 'short'))

dtree$host_acceptance_rate <- as.numeric(sub("%", "", dtree$host_acceptance_rate))

dtree$host_acceptance_rate <- discretize(dtree$host_acceptance_rate, method = 'fixed', breaks = c(-Inf, 40, 70, Inf), labels = c('slow', 'medium', 'fast'))

dtree$host_response_rate <- as.numeric(sub("%", "", dtree$host_response_rate))

dtree$host_response_rate <- discretize(dtree$host_response_rate, method = 'fixed', breaks = c(-Inf, 40, 70, Inf), labels = c('slow', 'medium', 'fast'))

dtree$review_scores_rating<-cut(dtree$review_scores_rating,breaks=c(0,4.63,4.8,5),labels=c('low','average','high')) # output

dtree$minimum_nights_avg_ntm <- discretize(dtree$minimum_nights_avg_ntm, method = 'fixed', breaks = c(1, 20, 32, 300),
                                            labels = c('few', 'medium', 'many'))

dtree$maximum_nights_avg_ntm <- discretize(dtree$maximum_nights_avg_ntm, method = 'fixed', breaks = c(7, 330, 706, 1125),
                                            labels = c('few', 'medium', 'many'))

dtree$accommodates <- discretize(dtree$accommodates, method = 'fixed', breaks = c(-Inf, 3, 6, Inf), labels = c('few', 'medium', 'many'))

dtree$price <- discretize(dtree$price, method = 'fixed', breaks = c(-Inf, 50, 140, Inf), labels = c('low', 'medium', 'high'))



set.seed(41)

sample <- sample(c(TRUE, FALSE), nrow(dtree), replace=TRUE, prob=c(0.8,0.2))
train  <- dtree[sample, ]
test   <- dtree[!sample, ]

default.ct <- rpart(review_scores_rating ~ . , data = train, method = 'class')

par(mfrow = c(1,1))

rpart.plot(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

default.ct.point.pred.train <- predict(default.ct,train,type = "class")

confusionMatrix(default.ct.point.pred.train, train$review_scores_rating)

default.ct.point.pred.test <- predict(default.ct,test,type = "class")

confusionMatrix(default.ct.point.pred.test, test$review_scores_rating)

cv.ct <- rpart(review_scores_rating ~ ., data = train, method = "class",
               cp = 0.00001, minsplit = 5, xval = 20)

printcp(cv.ct)

b<-printcp(cv.ct)
b<-data.frame(b)
which.min(b$xerror)

pruned.ct <- prune(cv.ct, cp = 0.0267857)

rpart.plot(pruned.ct, type = 1, extra = 1)

pruned.ct.point.pred.train <- predict(pruned.ct,train,type = "class")

confusionMatrix(pruned.ct.point.pred.train, train$review_scores_rating)

pruned.ct.point.pred.test <- predict(pruned.ct,test,type = "class")

confusionMatrix(default.ct.point.pred.test, test$review_scores_rating)



```

## Clustering

```{r, warning= FALSE}
# remove categorical variables


clust <- mutate(clust, mean_review_score = rowMeans(select(clust,c(review_scores_accuracy,review_scores_rating,
                         review_scores_cleanliness, review_scores_checkin,review_scores_communication,
                         review_scores_location,review_scores_value)))) # create a new variable called reviews

clust <- clust[, c("price", "mean_review_score")]

# elbow method to select optimal k

data_analysis_scaled <- scale(clust)
fviz_nbclust(data_analysis_scaled, kmeans, method = "wss", linecolor = 'darkorange1')

data_analysis_scaled <- as.data.frame(data_analysis_scaled)

# k-means cluster
set.seed(41)
km <- kmeans(data_analysis_scaled, centers = 3)
cluster_info<- aggregate(clust, by=list(cluster=km$cluster), mean)
d_cluster<- cbind(clust, cluster = km$cluster)

# visualization

plot(km$centers, col = c('salmon', 'green','lightblue'), pch = 16) # plotting centroids

d_cluster$cluster<- as.factor(d_cluster$cluster)

ggplot(data = d_cluster, aes(x = price, y = mean_review_score,
                             color = cluster)) + geom_point() + labs(x = 'Price', y = 'Review Rating')


ggplot(data = cluster_info, aes(x = cluster, y = price, fill = mean_review_score)) + geom_col()+scale_fill_gradient(low="blue",high="green")




```

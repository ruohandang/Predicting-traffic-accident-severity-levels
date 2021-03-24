##############
## Library ##
##############
library(tidyverse)
library(lubridate)
library(ROSE)
library(caret)
library(e1071)
library(party)
library(kknn)
library(randomForest)


##########################################
## Import dataset and check ##
##########################################
US_Accidents <- read.csv("US_Accidents_Dec19.csv", header = TRUE)
head(US_Accidents,5)
str(US_Accidents)
summary(US_Accidents)
dim(US_Accidents)
# [1] 2974335      49

##########################################
## Data Preprocessing for EDA ##
##########################################
## Deal with variables
# 1. remove variables with high NA proportion and not useful variables
# variables with NA proportion larger than 50% cannot give enough information to our analysis.
US_Accidents %>% summarise_all(~ mean(is.na(.))) %>% 
  pivot_longer(1:49, names_to = "Variables to drop", values_to = "NA proportion") %>% 
  filter(`NA proportion` >= 0.5)
# so we drop these five variables:
# End_Lat, End_Lng, Number, Wind_Chill.F., Precipitation.in.
na_cols <- c("End_Lat", "End_Lng", "Number", "Wind_Chill.F.", "Precipitation.in.")

# there are some variables will not give us insights about traffic accidents 
# or be useful in predicting severity levels
not_useful <- c("ID", "Source", "Description", "Timezone", "Airport_Code", "Weather_Timestamp", "Wind_Direction")

data_drop <- US_Accidents %>% 
  select(-all_of(na_cols), -all_of(not_useful))

dim(data_drop)
# 2974335 observations and 37 variables now

# 2. location related variables
# There are several variables indicating the location of the accident. 
# Apart from the accurate coordinate, longitude and latitude, 
# the dataset also contains state, city, county and even street address. 
# However, we want to find some nationwide patterns or statewide patterns from this dataset.
# So we can romove them now.
address <- c("Country", "City", "County", "Street", "Zipcode")

data_drop %>%
  select(all_of(address)) %>%
  head(5)

data_US <- data_drop %>% 
  select(-all_of(address))

dim(data_US)
#  2974335      32

# 3. transform time variables
# Time variables are in a format that is difficult to manipulate in the original dataset,
# so we transform two time variables to some new variables can reflect like hourly, weekly or monthly patterns.
data_US <- data_US %>%
  separate(Start_Time, into = c("Date", "Time"), sep = " ") %>%
  mutate("Year" = str_sub(Date, 1, 4), "Month" = str_sub(Date, 6, 7), "Day" = str_sub(Date, 9, 10), 
         "Wday" = as.character(wday(Date)), "Hour" = str_sub(Time, 1, 2)) %>%
  select(-c("Date", "Time", "End_Time")) %>%
  select(TMC, Severity, Year, Month, Day, Hour, Wday, everything())
# after transformation
data_US %>%
  select(Year, Month, Day, Hour, Wday) %>%
  head(5)

dim(data_US)
# 2974335      35

# 4. TMC variable problem
# check the relationship betweet TMC and Severity
data_US %>% 
  ggplot(aes(factor(TMC), ..prop..)) +
  geom_bar(aes(group = Severity, fill = factor(Severity)), show.legend = F) +
  facet_wrap(~ Severity, scales = "free") +
  labs(x = "TMC",
       y = "Proportion",
       title = "TMC distribution in each severity level") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6),
        legend.position = "top") +
  scale_fill_brewer(palette = "Set1")
# we find NA value is an important feature of severity level 4, 
# so we decide to treat NA value as a new level of TMC.
data_US <- data_US %>% 
  mutate(TMC = replace_na(TMC, "NA_TMC"))

# 5. weather condition NA problems
# we can remove all records containing this variable's NA value because all weather variables are related
table(data_US$Weather_Condition)
data_US$Weather_Condition[data_US$Weather_Condition==""] <- "NA"

sum(is.na(data_US$Weather_Condition))

data_US <- data_US %>% 
  filter(!is.na(Weather_Condition))
# data_US$Weather_Condition <- as.character(data_US$Weather_Condition)
# data_US$Weather_Condition <- as.factor(data_US$Weather_Condition)

dim(data_US)
#  2908403      35

# 6. other missing values
summary(data_US)
str(data_US)
# there are still some records containing NA values in continuous variables
# we can replace these NA values with the mean of the corresponding variable
data_US <- data_US %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T)))

# there are still some records containing "NA" values in factor variables
data_US$Side[data_US$Side==" "] <- "NA"
data_US$Sunrise_Sunset[data_US$Sunrise_Sunset==""] <- "NA"
data_US$Civil_Twilight[data_US$Civil_Twilight==""] <- "NA"
data_US$Nautical_Twilight[data_US$Nautical_Twilight==""] <- "NA"
data_US$Astronomical_Twilight[data_US$Astronomical_Twilight==""] <- "NA"
sum(is.na(data_US))

data_US <- data_US %>% 
  filter(!is.na(Side)) %>% 
  filter(!is.na(Sunrise_Sunset)) %>% 
  filter(!is.na(Civil_Twilight)) %>% 
  filter(!is.na(Nautical_Twilight)) %>% 
  filter(!is.na(Astronomical_Twilight))

dim(data_US)
# 2908322      35

# save a tidy data
write_csv(data_US, "tidydata_US.csv")

############################
## Visualization ##
############################
# set correct type to each variable
data_US <- read.csv("tidydata_US.csv", header = TRUE)

data_US <- data_US %>% 
  type_convert() %>%
  mutate(TMC = factor(TMC), Severity = factor(Severity), Year = factor(Year), Wday = factor(Wday)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)
data_US <- data_US %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T)))

# 1. Accident count

# states with maximum number of accidents
data_US %>%
  select(State) %>%
  group_by(State) %>%
  summarise(AccdtCount = sum(duplicated(State))) %>%
  arrange(desc(AccdtCount)) %>%
  ggplot(aes(x=State, y=AccdtCount)) + 
  geom_col(color= "grey")

# 2. Accident count in each severity level
data_US %>%
  group_by(Year, Severity) %>%
  count() %>%
  group_by(Year) %>%
  mutate(sum = sum(n)) %>%
  mutate(Proportion = n / sum) %>%
  ggplot(aes(Severity, Proportion)) +
  geom_col(aes(fill = Year), position = "dodge") +
  labs(x = "Severity",
       y = "Proportion",
       title = "Severity proportion changes by year") 

# 3. Accident account in different months
data_US %>%
  count(Month) %>%
  ggplot(aes(Month, n)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  labs(y = "Count",
       x = NULL,
       title = "Pattern between accident counts and month") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")) 
# 4. Accident account in different day of the week
data_US %>%
  count(Wday) %>%
  ggplot(aes(Wday, n, fill = Wday)) +
  geom_bar(position="dodge",stat = "identity") +
  geom_point() +
  labs(y = "Count",
       x = NULL,
       title = "Pattern between accident counts and day of the week") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thr", "Fri",
                              "Sat", "Sun")) 
# 5. Accident account in different hour of day
data_US %>%
  ggplot(aes(Hour, fill = !Hour %in% c("07", "08", "16", "17"))) +
  geom_bar(show.legend = F) +
  labs(x = "Hour",
       y = "No of Accidents",
       title = "Hourly Distribution of Accidents")

# 6. Weather with Accident
Weather <- data_US %>% group_by(Weather_Condition) %>% count()

Weather_data <- data.frame(
  name=c("Clear", "Haze" , "Heavy Rain", "Light Rain" ,"Overcast",	"Partly Cloudy", 
         "Mostly Cloudy	" ,"Scattered Clouds","Snow") ,  
  value=c(808142,	34314, 12063, 141063, 382475, 295434, 412520, 204656, 4796))

ggplot(Weather_data, aes(fill = name, x=name, y=value)) + 
  geom_bar(stat = "identity")




################################
## Preprocessing for modeling ##
################################
# 1. Narrow down the data - California
# Because of the large size of this dataset and the limited computing power, 
# we choose California as our target state. 
data_CA <- data_US %>%
  filter(State == "CA") %>% 
  select(-State)
dim(data_CA)
# 650316     34

table(data_CA$TMC)
table(data_CA$Weather_Condition)

# 2. remove some levels with few recordings or only one level
# Some weather condition or TMC levels only have a few records, 
# which may cause imbalanced problems when we split the dataset into training and test data.
# So we will drop some of them according to the number of records (remove value of records less than 20)
TMC_drop <- data_CA %>% 
  count(TMC) %>% 
  filter(n < 20) %>% 
  select(TMC)
TMC_drop <- TMC_drop$TMC %>% unlist()

data_CA <- data_CA %>% 
  filter(!TMC %in% TMC_drop) %>% 
  mutate(TMC = factor(TMC))

weather_drop <- data_CA %>% 
  count(Weather_Condition) %>% 
  filter(n < 20) %>% 
  select(Weather_Condition)
weather_drop <- weather_drop$Weather_Condition %>% unlist()

data_CA <- data_CA %>% 
  filter(!Weather_Condition %in% weather_drop) %>% 
  mutate(Weather_Condition = factor(Weather_Condition))

dim(data_CA)
# 650185     34

# 3. check the balance of the target variable 
ggplot(data_CA, aes(x = Severity, fill = Severity)) + geom_bar() + 
  labs(y = "Count", title = "Unbalanced severity levels")
# the data is seriously unbalanced in different severity levels 
# and most of the accidents are classified as level 2 and level 3, 
# so we decide to group the 4 levels into 2 levels. 
# Level 1 and level 2 will be grouped as "Not Severe", 
# and level 3 and level 4 will be grouped as "Severe".

data_group <- data_CA %>%
  mutate("Status" = factor(ifelse(Severity == "3" | Severity == "4", "Severe", "Not Severe"), 
                           levels = c("Not Severe", "Severe"))) %>%
  select(-Severity)

ggplot(data_group, aes(Status, fill = !Status == "Severe")) + geom_bar() +
  scale_fill_discrete(name = "Severity", labels = c("Severe", "Not Severe")) +
  labs(y = "Count", x = "Severity", title = "More balanced severity levels")
# the data looks better, then we can slipt the data into training and test data

# 4. check the balance of independent categorical variables 
summary(data_group[,18:33])

# from the result, we can see most of the data have the same values for some predictors,
# What's worse, when we split the dataset, the levels in training dataset and test dataset may not match.
# so we can remove them.
zero_variance <- c("Amenity", "Bump", "Crossing", "Give_Way", "No_Exit", 
                   "Railway", "Roundabout", "Station", "Stop", "Traffic_Calming", "Turning_Loop")

data_group <- data_group %>%
  select(-all_of(zero_variance))
dim(data_group)
# 650185     23

write_csv(data_group, "tidydata_CA.csv")

# 5.Split the data
# we split the data into two parts

data_group <- read.csv("tidydata_CA.csv", header = TRUE)
data_group <- data_group %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T))) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)


set.seed(1234) 
index <- sample(x=nrow(data_group), size=.80*nrow(data_group))
data_train <- data_group[index, ]
data_test <- data_group[-index, ]
dim(data_train)
# 520148     24
dim(data_test)
# 130037     24

# 6. sampling
# we reduce the data size to a scale that is more easily to manipulate.
# and create possibly balanced samples
data_train_sample <- ovun.sample(Status~., data = data_train, method = "both", p = 0.5, N = 50000, seed = 1)$data
###########################################
#### Applying Machine Learning Models #####
###########################################

# Medeling#
# Naive Bayes
bnc <- naiveBayes(Status~., data=data_train_sample)
bnc$tables

# Decision Trees
ctree <- ctree(Status~., data=data_train_sample)
ctree
# KNN
knn <- kknn(Status~.,train = data_train_sample,test = data_test,na.action = na.omit(), 
            k=223, scale=TRUE, distance = 2)  # k= sqrt(N)
knn
# Random Forest
rf <- randomForest(Status~., data = data_train_sample, importance=TRUE)

rf$ntree
rf$mtry
rf$importance
rf$err.rate


#### Training set predictions

# Naive Bayes
pred_train_bnc <- predict(bnc, data_train_sample)
p_train_bnc <- mean(pred_train_bnc == data_train_sample$Status)

# Decision Trees
pred_train_ctree <- predict(ctree, data_train_sample)
p_train_ctree <- mean(pred_train_ctree == data_train_sample$Status)

# KNN does not have the process of training the model, 
# because the test data acctually needs to be predicted through
# the training data set. 
p_train_knn <- 1


# Random Forest
pred_train_rf <- predict(rf, data_train_sample)
p_train_rf <- mean(pred_train_rf == data_train_sample$Status)


### Test set predictions

# Naive Bayes
pred_test_bnc <- predict(bnc, data_test)
p_bnc <- mean(pred_test_bnc == data_test$Status)

# Decision Trees
pred_test_ctree <- predict(ctree, data_test)
p_ctree <- mean(pred_test_ctree == data_test$Status)

# KNN
pred_knn <- fitted(knn)
p_knn <- mean(pred_knn == data_test$Status)


# Random Forest
pred_test_rf <- predict(rf, data_test)
p_rf <- mean(pred_test_rf == data_test$Status)

# Confusion Matrix Analysis
confusionMatrix(table(pred_test_bnc, data_test$Status))
confusionMatrix(table(pred_test_ctree, data_test$Status))
confusionMatrix(table(pred_knn, data_test$Status))
confusionMatrix(table(pred_test_rf, data_test$Status))



####################
##### Conclusion ###
####################
##Training set predictions
p_train <- as.vector(c(p_train_bnc, p_train_ctree, p_train_knn, p_train_rf))
p_train <- round(p_train,4)
barplot(p_train, ylim = c(0,1), main = "Training set scores", ylab = "Accuracy",
        xlab = "Models", names.arg = c("Naive Bayes","Decision Trees","KNN","Randomforest"),
        col = c("darkslategray3", "gold2", "lightsalmon1", "mediumseagreen"))
text(p_train, labels = as.character(p_train), pos = 1, cex = 0.75)

# plot  
p <- as.vector(c(p_bnc, p_ctree, p_knn, p_rf))
p_round <- round(p,4)
barplot(p_round, ylim = c(0,1), main = "Model comparison", ylab = "Accuracy",
        xlab = "Models", names.arg = c("Naive Bayes","Decision Trees","KNN","Randomforest"),
        col = c("darkslategray3", "gold2", "lightsalmon1", "mediumseagreen"))
text(p, labels = as.character(p_round), pos = 3, cex = 0.75)
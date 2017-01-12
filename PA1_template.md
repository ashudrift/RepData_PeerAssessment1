

## Reading Data

```{r}
unzip("activity.zip")
activity <- read.csv("activity.csv")
str(activity)
```
## Total No of Steps
### Mean Total No. of Steps taken each day
```{r}
library(dplyr)
Steps <- activity %>% group_by(date) %>% summarise(Total_steps = sum(steps,na.rm = T))
Steps
```
### Histogram of Total No of steps each day
```{r}
hist(Steps$Total_steps, main = "Total No of Steps each day", xlab = "Total No of Steps", col = "red")
```

### Mean No of Total Steps each day
```{r}
mean(Steps$Total_steps)
```
### Median No of Total Steps each day
```{r}
median(Steps$Total_steps)
```
## Daily Activity Pattern
### Average Daily Activity Pattern
```{r}
Daily_Activity <- activity %>% group_by(interval) %>% summarise(Daily_Activity_Pattern = mean(steps,na.rm = T))
plot(x = Daily_Activity$interval,y = Daily_Activity$Daily_Activity_Pattern,type = "l" )
```
### Maximum Steps Interval
```{r}
Daily_Activity[(which.max(Daily_Activity$Daily_Activity_Pattern)),]
```
## Imputing Missing Values
### Total No of Missing Values = 2304
```{r}
summary(activity)
```
### Strategy for imputing missing values
Since missing values are present only in steps
So, substitue the steps with average number of steps for that interval
### Imputing the missing values and creating a new dataset
```{r}
Avg_no_of_steps <- activity %>% group_by(interval) %>% summarise (steps_treated = mean(steps,na.rm = T))
Treated_activity <- left_join(activity, Avg_no_of_steps, by.x = activity$interval, by.y = Avg_no_of_steps$interval)
for (i in 1:length(Treated_activity$steps))
  {
  if(is.na(Treated_activity$steps[i]))
    {
    Treated_activity$steps[i] <- Treated_activity$steps_treated[i]
    }
  }
Treated_activity$steps_treated = NULL
summary(Treated_activity)
```
###
### Histogram of Total No of steps each day by Treated dataset
```{r}
Treated_Steps <- Treated_activity %>% group_by(date) %>% summarise(Treated_Total_steps = sum(steps))
hist(Treated_Steps$Treated_Total_steps, main = "Total No of Steps each day", xlab = "Total No of Steps", col = "blue")
```

### Mean No of Total Steps each day
```{r}
mean(Treated_Steps$Treated_Total_steps)
```
### Median No of Total Steps each day
```{r}
median(Treated_Steps$Treated_Total_steps)
```
### Comparing Untreated and treated activity
```{r}

hist(Treated_Steps$Treated_Total_steps, main = "Total No of Steps each day", xlab = "Total No of Steps", border = "blue", breaks = 10)
hist(Steps$Total_steps, main = "Total No of Steps each day", xlab = "Total No of Steps",border = "red" ,breaks = 10, add = T)
```
### Differnce in mean and median of treated and untreated data
```{r}
mean(Treated_Steps$Treated_Total_steps) - mean(Steps$Total_steps)
median(Treated_Steps$Treated_Total_steps) - median(Steps$Total_steps)
```
## Analysis of weekday and weekend activity
### Creating Weekday and Weekend flag
```{r}
Treated_activity$date <- as.Date(Treated_activity$date)
Treated_activity$Day <- weekdays(Treated_activity$date)
weekends <- c("Sunday","Saturday")
Treated_activity$Day <- factor((Treated_activity$Day %in% weekends), levels = c(TRUE,FALSE), labels = c("Weekends","Weekdays"))
```
### Time Series Plot
```{r}
Treated_Daily_Activity <- Treated_activity %>% group_by(interval,Day) %>% summarise(Daily_Activity_Pattern = mean(steps,na.rm = T))
library(ggplot2)
ggplot(Treated_Daily_Activity,aes(x = interval, y =Daily_Activity_Pattern ), fill = Day) + geom_line() + facet_grid(.~Day)
```

<p style= "text-align:center;">Thanks</p>
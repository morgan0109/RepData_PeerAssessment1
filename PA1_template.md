---
title: "PA1_template"
author: "morgan"
date: "2/1/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Title : Reproducible Research week2 assignment
##This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
--------
##Loading and preprocessing the data
##1.Load the data
```{r echo=TRUE}
activity <- read.csv("activity.csv")
head(activity)
```
##2.Process/transform the data (if necessary) into a format suitable for your analysis
```{r echo=TRUE}
data <- aggregate(steps~date,data=activity,sum,na.rm=TRUE)
```
--------
##What is mean total number of steps taken per day?
##1.Make a histogram of the total number of steps taken each day
```{r echo=TRUE}
hist(data$steps)
```
##2.Calculate and report the mean and median total number of steps taken per day
```{r echo=TRUE}
mean(data$steps)
median(data$steps)
```
-------
##What is the average daily activity pattern?
##1.Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
```{r echo=TRUE}
stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```
##2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE}
max(stepsInterval$steps)
stepsInterval[which.max(stepsInterval$steps),]$interval
```
-------
##Imputing missing values
##1.Calculate and report the total number of missing values in the dataset
```{r echo=TRUE}
sum(is.na(activity$steps))
```
##2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
```{r echo=TRUE}
interval2 <-function(interval){
  stepsInterval[stepsInterval$interval==interval,]$steps
}
```
##3.Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r echo=TRUE}
activity1 <- activity
count=0
for(i in 1:nrow(activity1)){
  if(is.na(activity1[i,]$steps)){
    activity1[i,]$steps<-interval2(activity1[i,]$interval)
    count=count+1
  }
}
cat("Total ",count, "NA values were filled.\n\r")
```
##4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r echo=TRUE}
stepsInterval2 <- aggregate(steps~date,data=activity1,sum)
hist(stepsInterval2$steps)
mean(stepsInterval2$steps)
median(stepsInterval2$steps)
##The mean value is the same as the value before imputing missing data because we put the mean value for that particular 5-min interval. The median value shows a little difference , but it depends on where the missing values are
```
##Are there differences in activity patterns between weekdays and weekends?
##1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r echo=TRUE}
activity1$day=ifelse(as.POSIXlt(as.Date(activity1$date))$wday%%6==0, "weekend","weekday")
activity1$day=factor(activity1$day,levels=c("weekday","weekend"))
```
##2.Make a panel plot containing a time series plotof the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 
```{r echo=TRUE}
stepsInterval3=aggregate(steps~interval+day,activity1,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval3,aspect=1/2,type="l")
```

library(dplyr)
library(ggplot2)

activity_data <- read.csv("activity.csv")
head(activity_data)

databydate <- activity_data %>% 
    select(date, steps) %>% 
    group_by(date) %>%
    summarise(tsteps=sum(steps)) %>%
    filter(!is.na(tsteps))

hist(databydate$tsteps, xlab="Total daily steps", 
     main="Frequency of total daily steps", breaks = 20)


mean(databydate$tsteps)

median(databydate$tsteps)

databyinterval <- activity_data %>%
    select(interval, steps) %>% 
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarise(tsteps=mean(steps))

g <- ggplot(databyinterval, aes(x=interval, y=tsteps)) +
        geom_line() 
    
g + ggtitle("Time series of average number of steps taken") +
    theme(plot.title = element_text(hjust=0.5))
    
most_steps <- databyinterval[which(databyinterval$tsteps == max(databyinterval$tsteps)),] 
most_steps$interval

num_of_na_values <- sum(is.na(activity_data))

new_data <- activity_data

head(new_data)

for(i in 1:ncol(new_data)){
    new_data[is.na(new_data[,i]), i] <- mean(new_data[,i], na.rm = TRUE)
}


new_databydate <- new_data %>% 
    select(date, steps) %>% 
    group_by(date) %>%
    summarise(tsteps=sum(steps))

hist(new_databydate$tsteps, xlab="Total daily steps", 
     main="Frequency of total daily steps", breaks = 20)

new_mean = mean(new_databydate$tsteps)
new_median = median(new_databydate$tsteps)

new_data$date <- as.Date(new_data$date)
new_data$weekday <- weekdays(new_data$date)
new_data$weekend <- ifelse(new_data$weekday=="Saturday" | new_data$weekday=="Sunday", 
                           "Weekend", "Weekday")

averages <- aggregate(steps ~ interval + weekend, data =new_data, mean)

ggplot(averages, aes(x=interval, y=steps, color=weekend)) + 
    geom_line() + facet_grid(weekend ~ .) +
    labs(x="5-minute interval", y="Mean no of steps",
         title="Comparison of average number of steps in each interval")+
    theme(plot.title = element_text(hjust=0.5))


averages

library(dplyr)
actall <- read.csv("activity.csv")
actall$date <- as.Date(actall$date, format="%Y-%m-%d")
#Add fields for day of the week and the weekend flag
mutate(actall, actall$day <- weekdays(date, abbreviate=TRUE))
mutate(actall, actall$weekend <- ifelse(day %in% c("Sat", "Sun"), "y", "n"))
act <- filter(actall, is.na(steps) == FALSE)

bydate <- group_by(act, date)
bydate <- summarize(bydate, sum(steps))
names(bydate) <- c("date", "totalsteps")
# Histogram without NAs
hist(bydate$totalsteps, xlab = "Total Steps", main = "Histogram of Total Steps\n(missing data ignored)", col = "red")
print(summary(bydate$totalsteps))

# Average daily activity pattern
bydate <- group_by(act, interval)
bydate <- summarise(bydate, round(mean(steps), digits=1))
names(bydate) <- c("interval", "averagesteps")

plot(bydate, type = "l", main = "Average Daily Activity Pattern\n(missing data ignored)", ylab = "Average Steps", col = "blue")
print(summary(bydate))

# Interval with the maximum average steps
print(filter(bydate, bydate$averagesteps == max(bydate$averagesteps)))
# Count of NA rows
print(nrow(subset(actall, is.na(actall$steps) == TRUE)))

# Reconfigure to allow NA rows

act <- actall
# Replace NA steps with average for that interval across the table
for(i in unique(act$interval))
{
        x <- filter(act, interval == i & is.na(steps) == FALSE)
        act <- mutate(act, steps = ifelse(is.na(steps) == TRUE & interval == i, round(mean(x$steps), digits=1), steps))
}

#do it again

bydate <- group_by(act, date)
bydate <- summarize(bydate, sum(steps))
names(bydate) <- c("date", "totalsteps")
# Histogram without NAs
hist(bydate$totalsteps, xlab = "Total Steps", main = "Histogram of Total Steps", col = "red")
print(summary(bydate$totalsteps))
mean(bydate$totalsteps)

# Average daily activity pattern
bydate <- group_by(act, interval)
bydate <- summarise(bydate, round(mean(steps), digits=1))
names(bydate) <- c("interval", "averagesteps")

plot(bydate, type = "l", main = "Average Daily Activity Pattern", ylab = "Average Steps", col = "blue")
print(summary(bydate))

# Interval with the maximum average steps
m <- filter(bydate, bydate$averagesteps == max(bydate$averagesteps))
print(m$interval)
# Count of NA rows
print(nrow(subset(act, is.na(act$steps) == TRUE)))

par(mfrow = c(2, 1))

# Ave for weekend only
actw <- filter(act, weekend=="y")
bydate <- group_by(actw, interval)
bydate <- summarise(bydate, round(mean(steps), digits=1))
plot(bydate, type = "l", main = "Average Daily Activity Pattern\nWeekend Days", ylab = "Average Steps", col = "blue")
print(summary(bydate))
# Ave for non weekend only
actw <- filter(act, weekend=="n")
bydate <- group_by(actw, interval)
bydate <- summarise(bydate, round(mean(steps), digits=1))
plot(bydate, type = "l", main = "Non Weekend Days", ylab = "Average Steps", col = "blue")
print(summary(bydate))




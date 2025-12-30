bgg <- BGG_Data_Set_MODIFIED

summary(bgg)

sd(bgg$Rating.Average)
sd(bgg$Complexity.Average)
sd(bgg$Play.Time)
sd(bgg$Year.Published)
  # Calculated in Excel, resulted in SD = 199.6

tsample <- c(5.24, 8.12, 5.28, 6.00, 5.59, 
             7.55, 7.38, 7.05, 5.20, 8.00, 
             5.39, 7.24, 6.51, 6.13, 6.65, 
             7.49, 5.65, 5.07, 5.80, 6.17,
             8.05, 6.67, 5.84, 5.56, 7.52)
mean(tsample)
sd(tsample)
pt(mean(tsample), 24, lower.tail = FALSE)

hist(bgg$Rating.Average,
     main="Average Community Rating of Board Games",
     xlab="Average Rating of Game",
     ylab="Number of Games",
     col="orchid1")

boxplot(bgg$Rating.Average,
        main="Community Ratings Box Plot",
        ylab="Board Game Community Rating",
        col = "orchid1")

hist(bgg$Complexity.Average,
     main="Complexity of Board Games",
     xlab="Complexity of Game",
     ylab="Number of Games",
     col="orchid1")

boxplot(bgg$Complexity.Average,
        main="Complexity Box Plot",
        ylab="Board Game Complexity",
        col = "orchid1")

# Looking only at rows with Play.Time <= 12 hours
hist(bgg$Play.Time,
     main="Board Game Play Time",
     xlab="Play Time",
     ylab="Number of Games",
     breaks = seq(0, 60000, l=2001),
     xlim = c(0, 720),
     xaxp = c(0, 720, 6),
     col="orchid1")

hist(bgg$Play.Time,
     main="Board Game Play Time",
     xlab="Play Time",
     ylab="Number of Games",
     breaks = seq(0, 60000, l=181),
     xlim = c(0, 60000),
     xaxp = c(0, 60000, 6),
     col="orchid1")

playTimeSD = sd(bgg$Play.Time)
playTimeMean = mean(bgg$Play.Time)
table(bgg$Play.Time >= (1 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (2 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (3 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (4 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (5 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (6 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (7 * playTimeSD) + playTimeMean)
table(bgg$Play.Time >= (8 * playTimeSD) + playTimeMean)

hist(bgg$Year.Published,
     main="Board Game Release Dates",
     xlab="Release Year",
     ylab="Number of Games",
     breaks = 250,
     #xlim = c(1950, 2030),
     #xaxp = c(-3500, 2022, 6),
     col="orchid1")

boxplot(bgg$Year.Published,
        main="Complexity Box Plot",
        ylab="Board Game Complexity",
        col = "orchid1")

plot(bgg$Year.Published, bgg$Play.Time, 
     pch=16, 
     cex=0.4,
     xlab="Year Published",
     ylab="Play Time",
     col="orchid")
plot(bgg$Year.Published, bgg$Play.Time, 
     pch=16,      
     cex=0.4,
     xlab="Year Published",
     ylab="Play Time",
     xlim=c(1850, 2022),
     ylim=c(0, 1440),
     col="orchid")
abline(lm(bgg$Play.Time ~ bgg$Year.Published), col = "blue", lwd = 2)
cor(bgg$Year.Published, bgg$Play.Time)

plot(bgg$Year.Published, bgg$Complexity.Average, 
     pch=16, 
     cex=0.4,
     xlab="Year Published",
     ylab="Complexity",
     col="orchid")
plot(bgg$Year.Published, bgg$Complexity.Average, 
     pch=16, 
     cex=0.4,
     xlab="Year Published",
     ylab="Complexity",
     xlim=c(1850, 2022),
     col="orchid")
abline(lm(bgg$Complexity.Average ~ bgg$Year.Published), col = "blue", lwd = 2)
cor(bgg$Year.Published, bgg$Complexity.Average)

plot(bgg$Complexity.Average, bgg$Play.Time, 
     pch=16, 
     cex=0.4,
     xlab="Complexity",
     ylab="Play Time",
     col="orchid")
plot(bgg$Complexity.Average, bgg$Play.Time, 
     pch=16, 
     cex=0.4,
     xlab="Complexity",
     ylab="Play Time",
     ylim = c(0, 720),
     col="orchid")
abline(lm(bgg$Play.Time ~ bgg$Complexity.Average), col = "blue", lwd = 2)
cor(bgg$Play.Time, bgg$Complexity.Average)
summary(lm(bgg$Play.Time ~ bgg$Complexity.Average))

plot(bgg$Rating.Average, bgg$Complexity.Average, 
     pch=16, 
     cex=0.4,
     xlab="Rating",
     ylab="Complexity",
     col="orchid")
abline(lm(bgg$Complexity.Average ~ bgg$Rating.Average), col = "blue", lwd = 2)
cor(bgg$Rating.Average, bgg$Complexity.Average)
summary(lm(bgg$Complexity.Average ~ bgg$Rating.Average))














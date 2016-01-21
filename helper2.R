review <- read.csv("D:/UTDallasStudy/Coursework/Fall_15/Big_Data/project/category-output.csv")
colnames(review) <- c('date', 'category', 'review_count')
review_na <- review[which(review$category != ''),]
review_na <- review_na[which(review_na$category != 'NA'),]

review_na$category <- na.omit(review_na$category)
review$category <- gsub('[[]]', 'NA', review$category, ignore.case = TRUE)
review$category <- gsub('[\\]', 'NA', review$category, ignore.case = TRUE)

fit <- kmeans(restReview, 6)

plot(fit)
library(zoo)

View(review)
restReview <- review[which(review$Category == 'restaurants'),]
restReview1 <- restReview[,c('Date', 'No_Of_Reviews')]
restReview1$Date <- ts(as.Date(as.yearmon(restReview1$Date, "YYYY-mm"), frac = 1))




plot(1:10)


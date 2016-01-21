review <- read.csv("D:/UTDallasStudy/Coursework/Fall_15/Big_Data/project/data/categories_output.csv")
review <- na.omit(review)
colnames(review) <- c('category', 'category_id', 'count', 'season','year')

plot((review$season), review$review_no)

review_clust <- review[, c('date', 'review_no')]

review_clust_13 <- review[15:26, c('date','category', 'review_no')]


review_clust$date <- ts(as.Date(review_clust$date))

fit <- kmeans(review_clust, 7)
library(fpc)
plotcluster(review, fit$cluster)

clusplot(review, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plot(review_clust_13[c("date", "review_no")])
plot(factor(review_clust_13$date), review_clust_13$review_no)

####################
library(cluster)
library(HSAUR)
data(pottery)
km    <- kmeans(pottery,3)
dissE <- daisy(pottery) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)
########################################################################

review <- read.csv("D:/UTDallasStudy/Coursework/Fall_15/Big_Data/project/data/categories_output.csv")
review <- na.omit(review)
colnames(review) <- c('category', 'category_id', 'count', 'season','year')


## Fall + restaurants analysis
rest_review <- review[which(review$category == 'restaurants'),]
rest_review <- rest_review[which(rest_review$season == 'fall'),]
plot(factor(rest_review$year), rest_review$count)


## season + restaurants analysis
rest_review <- review[which(review$category == 'restaurants'),]
#plot(factor(rest_review$season), rest_review$count)
x <- aggregate(rest_review$count, by=list(rest_review$season), FUN = mean)
plot(x$Group.1, x$x)
lines(x$Group.1, x$x)


## year + restaurants analysis
rest_review <- review[which(review$category == 'restaurants'),]
plot(factor(rest_review$year), mean(rest_review$count), type="n", xlab="Year",ylab="no_of_reviews")


########################################################################

review <- read.csv("D:/UTDallasStudy/Coursework/Fall_15/Big_Data/project/data/category-output-1.csv")
review <- na.omit(review)
colnames(review) <- c('category', 'category_id', 'count', 'season','month','year')
rest_review <- review[which(review$category == 'restaurants'),]

plot(factor(rest_review$month), rest_review$count)
View(review)


x <- aggregate(rest_review$count, by=list(rest_review$month), FUN = mean)
plot(x$Group.1, x$x)
lines(x$Group.1, x$x)


review_ext <- review[,c('category', 'count', 'season')]
plot(review_ext)

last <- data.frame()
plot(review_ext$category, review_ext$season,review_ext$count)
output-merged








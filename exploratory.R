

handle_package <- function(x) {
  a <- require(x, character.only = TRUE)
  if(!a) {
    install.packages(x)
    require(x, character.only = TRUE)
  }
}

#load dependancies
pkg <- c("ggplot2","dplyr","R.utils","fdrtool","caret","randomForest","pROC", 
         "readr", "stringr", "corrgram", "corrplot")

out <- lapply(pkg, handle_package)

class_articles <- read_csv(file = "./data/traindata.csv", col_names = TRUE)
problems(class_articles)

# create binary class - popular
class_articles$popular <- as.numeric(class_articles$shares >= 1400)

summary(class_articles$popular)
sd(class_articles$shares)

class_articles <- class_articles %>% filter(shares > 300, shares < 10000)

class_articles %>%
  filter(shares < 1400)


#lapply(dev.list(), dev.off)

a <- ggplot(class_articles, aes(shares))
a + geom_histogram(binwidth = .05, aes(fill = ..count..)) + 
  scale_x_log10()

#matrix for correlation matrix
df <- as.data.frame(class_articles)


str(df)

x <- as.data.frame(cor(x = na.omit(df[, -1], as.double), use = "everything", method = "pearson"))


abs(x$popular) > 0.2

str(x)





corrgram(x, order = , panel=, lower.panel=, upper.panel=, text.panel=, diag.panel=)


library('corrplot') #package corrplot
corrplot(df, method = "number") #plot matrix

# Second Correlogram Example
library(corrgram)
corrgram(class_articles[, c("is_weekend","popular", "data_channel_is_socmed")], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="News Article Shares Data in PC2/PC1 Order")


str(class_articles)



#, aes(y = ..density..)) + geom_density() +
#  scale_fill_gradient("Shares", low = "green", high = "red")
  
#  qplot(class_articles$shares,
#        geom="histogram",
#        binwidth = 0.05,  
#        main = "Histogram for Shares", 
#        xlab = "shares",  
#        fill = I("blue"), 
#        col = I("red"), 
        #alpha = I(.2),
#        xlim = c(0, 5000),
#        ylim = c(100, 10000),
#        )
  



handle_package <- function(x) {
  a <- require(x, character.only = TRUE)
  if(!a) {
    install.packages(x)
    library(x)
  }
}

#load dependancies
pkg <- c("ggplot2","dplyr","R.utils","fdrtool","caret","randomForest","pROC", "readr", "stringr")

out <- lapply(pkg, handle_package)

class_articles <- read_csv(file = "./traindata.csv", col_names = TRUE)
problems(class_articles)

str(class_articles)

# create binary class - popular
class_articles$popular <- as.numeric(class_articles$shares >= 1400)

summary(class_articles$shares)
sd(class_articles$shares)

class_articles <- class_articles %>% filter(shares < 10000)


a <- ggplot(class_articles, aes(shares))
a + geom_histogram(binwidth = .) + scale_x_log10()#, aes(y = ..density..)) + geom_density() +
  scale_fill_gradient("Shares", low = "green", high = "red")



handle_package <- function(x) {
  a <- require(x, character.only = TRUE)
  if(!a) {
    install.packages(x)
    require(x, character.only = TRUE)
  }
}

#load dependancies
pkg <- c("ggplot2","dplyr","R.utils","fdrtool","caret","randomForest","pROC", 
         "readr", "stringr", "corrgram", "corrplot", "aod")

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

summary(class_articles)
class_articles %>%
  select(min_negative_polarity)

#matrix for correlation matrix
df <- as.data.frame(class_articles)


str(df)

x <- as.data.frame(cor(x = na.omit(df[, -1], as.double), use = "everything", method = "pearson"))


pop_rows <- which(abs(x$popular) > 0.1)

row.names(x)[pop_rows]

# Potential Explanatory Variables - Logistic Regression
vars <- c("data_channel_is_entertainment",
"data_channel_is_socmed",
"data_channel_is_tech",
"data_channel_is_world",
"kw_avg_avg",
"is_weekend",
"LDA_02",
"LDA_04")

xtabs(~popular + data_channel_is_world, data = class_articles)
xtabs(~popular + LDA_02, data = class_articles)

#Nothing works here
mylogit <- glm(popular ~ data_channel_is_entertainment + data_channel_is_socmed + 
               data_channel_is_socmed + data_channel_is_tech + data_channel_is_world +
               kw_avg_avg + is_weekend + LDA_02 + LDA_04, 
               data = class_articles, family = "binomial")

summary(mylogit)

corrgram(x, order = , panel=, lower.panel=, upper.panel=, text.panel=, diag.panel=)




# Second Correlogram Example
library(corrgram)
corrgram(class_articles[, c("is_weekend","popular", "data_channel_is_socmed", "n_tokens_title")], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="News Article Shares Data in PC2/PC1 Order")


corrgram(x, data = 'cor', lower.panel=panel.shade, upper.panel=NULL)


str(class_articles)

lapply(dev.list(), dev.off)
dev.next()



#PCA

#Take Sample
trainIndex <- createDataPartition( class_articles$popular, 
                                   p = 0.25, 
                                   list = FALSE, 
                                   times = 1)

str(trainIndex)

train_data <- class_articles[ trainIndex, -1]

a <- ggplot(class_articles[trainIndex,], aes(shares))
a + geom_histogram(binwidth = .05, aes(fill = ..count..)) + 
  scale_x_log10()


## try http if https is not available
source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")

source("http://pastebin.com/raw.php?i=UyDBTA57")

#PC Components
args<-list(  pca.data        = trainIndex,
             pca.algorithm   = "svd",
             pca.components  = 6,
             pca.center      = FALSE,
             pca.scaling     = "none",
             pca.cv          = "q2"
)

res<-devium.pca.calculate(args,return="list",plot=TRUE)

pca.results<-pcaMethods::pca(as.matrix(train_data), method = "svd", 
                             nPcs = 3, center = TRUE, scale = "none", 
                             cv = "q2", seed = 123)

scores <- as.data.frame(pca.results@scores)
loadings <- as.data.frame(pca.results@loadings)
eigenvalues <- data.frame(eigenvalues=pca.results@R2)

# Plot eigenvalues


# Decision Tree

# Find Greatest Variance to Split on - Top 12
str(std_dev)

names(std_dev) <- c("sd")

std_dev <- as.data.frame(sapply(class_articles, sd))

top_12 <- dplyr::arrange(std_dev, desc(sd))[1:12, ]#row_number(arrange(std_dev, desc(sd))[1:12, ])


top_12

std_dev$sd == 216383.929



str(train_data)

#library('corrplot') #package corrplot
#corrplot(df, method = "number") #plot matrix

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
  

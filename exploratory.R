

handle_package <- function(x) {
  a <- require(x, character.only = TRUE)
  if(!a) {
    install.packages(x)
    require(x, character.only = TRUE)
  }
}

#load dependancies
pkg <- c("ggplot2","dplyr","R.utils","fdrtool","caret","randomForest","pROC", 
         "readr", "stringr", "corrgram", "corrplot", "aod", "rvest")

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



#Validate Share Counts
class_articles %>%
  select(url, shares, updated_shares) %>%
  mutate(updated_shares = get_updated_shares(url))

str

article_html <- html("http://mashable.com/2013/01/07/amazon-instant-video-browser/")

str_detect(article_html, "div em")

shares_validation <- tbl_df(data.frame(class_articles$url, class_articles$shares ))
names(shares_validation) <- c("url", "shares")

head(shares_validation)

str(shares_validation)
str(class_articles)

sapply(class_articles[ 2, "url" ], get_updated_shares)

get_updated_shares <- function (x) {
  print( x$url )
  article_html <- html(x$url)
  
  return( updated_shares <- article_html %>%
                              html_node("div em") %>%
                              html_text() %>%
                              as.numeric()
        )
  
}

for( i in 1000:length(class_articles$url) ) {
  
  class_articles[ i, "updated_shares" ] <- get_updated_shares(class_articles[ i, "url" ])
  
  if( i == 2 ) {
    break
  }
  
}

str(class_articles)

class_articles %>%
  filter( shares > updated_shares) %>%
  select( url, shares, updated_shares)

class_articles$updated_shares

class_articles <- class_articles[, -63]
class_articles[1, "updated_shares"] <- "5"


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
                                   p = 0.5, 
                                   list = FALSE, 
                                   times = 1)

str(trainIndex)

train_data <- class_articles[ trainIndex, ]

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

std_dev <- as.data.frame(sapply(train_data, sd))

BiocGenerics::order(std_dev, desc(sd))

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
  

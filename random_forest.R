"popular ~ kw_max_max + kw_avg_max + kw_min_max + self_reference_max_shares + self_reference_avg_sharess +  self_reference_min_shares
kw_max_avg
kw_max_min
kw_avg_avg
kw_min_avg
kw_avg_min
n_tokens_content
timedelta
kw_min_min
num_hrefs
num_imgs
num_videos
num_self_hrefs
n_tokens_title
num_keywords
average_token_length"

# resopnse factors
popular <- (factor(c(0, 1)))

popular ~ kw_avg_max + self_reference_avg_sharess +  
  self_reference_min_shares + n_tokens_content + 
  timedelta + num_hrefs + num_imgs + num_videos + 
  num_self_hrefs + n_tokens_title + 
  num_keywords + average_token_length

predictors <- train_data %>%
                select(kw_avg_max, self_reference_avg_sharess,   
                  self_reference_min_shares, n_tokens_content,
                  timedelta, num_hrefs, num_imgs, num_videos,
                  num_self_hrefs, n_tokens_title,
                  num_keywords, average_token_length)

predictors <- as.data.frame(predictors)

response <- as.factor(train_data$popular)

class_rrf <- randomForest(x = predictors, y = response, ntree=100, proximity=TRUE, maxnodes = 250)

str(class_rrf)

class_rrf$predicted

test_set <- read_csv("./data/testdata.csv")

submission <- predict(class_rrf, test_set)

test_set[1, "ID"]
submission[1]

final_submission <- data.frame(0, 0)
names(final_submission) <- c("id", "response")
final_submission <- tbl_df(final_submission)


final_submission$id[2] <- as.numeric(test_set[2, "ID"])
final_submission$response[2] <- as.character(submission[[2]])

submit <- as.data.frame(final_submission)
str(submit)

write_csv(submit, "./data/submission_eop.csv")

for( i in 1:length(submission) ) {
  final_submission[ i, "id" ] <- as.numeric(test_set[i, "ID"])
  final_submission[ i, "response"] <- as.character(submission[[i]])
}


length(predictors)
length(response)


## test proximity in regression
iris.rrf <- randomForest(iris[-1], iris[[1]], ntree=101, proximity=TRUE, oob.prox=FALSE)
str(iris.rrf$proximity)






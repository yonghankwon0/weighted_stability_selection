weighted_stabiltiy_selection <- function(data, y, B = 50) {
  
  library(glmnet); library(ROCR)
  
  n <- dim(data)[1]
  p <- dim(data)[2]-1
  
  weighted_freq <- list()
  
  for (i in 1:B) {
    
    # split 1
    data_split <- sample(1:n,floor(n/2),replace = F)
    
    data_subsample1 <- as.matrix(data[data_split,])
    cv_glmnet_1 <- cv.glmnet(x = data_subsample1[,!(colnames(data_subsample1) %in% y)], y = data_subsample1[,y],
                             family = "binomial",
                             parallel = T)
    
    # split 2
    data_subsample2 <- as.matrix(data[-data_split,])
    cv_glmnet_2 <- cv.glmnet(x = data_subsample2[,!(colnames(data_subsample2) %in% y)], y = data_subsample2[,y],
                             family = "binomial",
                             parallel = T)
    
    coef1 <- coef(cv_glmnet_1, s = "lambda.min")
    coef2 <- coef(cv_glmnet_2, s = "lambda.min")
    
    coef_intersect <- intersect(coef1@Dimnames[[1]][coef1@i + 1],
                                coef2@Dimnames[[1]][coef2@i + 1])
    
    # if there is no intersect try it again and again
    while (length(coef_intersect) < 2){
      # split 1
      data_split <- sample(1:n,floor(n/2),replace = F)
      
      data_subsample1 <- as.matrix(data[data_split,])
      cv_glmnet_1 <- cv.glmnet(x = data_subsample1[,1:p], y = data_subsample1[,p+1],
                               family = "binomial",
                               parallel = T)
      
      # split 2
      data_subsample2 <- as.matrix(data[-data_split,])
      cv_glmnet_2 <- cv.glmnet(x = data_subsample2[,1:p], y = data_subsample2[,p+1],
                               family = "binomial",
                               parallel = T)
      
      coef1 <- coef(cv_glmnet_1, s = "lambda.min")
      coef2 <- coef(cv_glmnet_2, s = "lambda.min")
      
      coef_intersect <- intersect(coef1@Dimnames[[1]][coef1@i + 1],
                                  coef2@Dimnames[[1]][coef2@i + 1])
    }
    
    
    coef_intersect_no_intercept <- coef_intersect[!(coef_intersect %in% "(Intercept)")]
    formula <- as.formula(paste(y, "~", paste(paste0("`",coef_intersect_no_intercept,"`"), collapse=" + ")))
    
    glm_1 <- glm(formula,
                 data=data,
                 family = binomial())
    glm_1_summary <- glm_1 %>% summary()
    filtered_feature <- coef_intersect_no_intercept[which(glm_1_summary$coefficients[,"Pr(>|z|)"]<0.05)-1]
    
    if(length(filtered_feature) == 0){
      filtered_feature <- coef_intersect_no_intercept
    }
    
    formula2 <- as.formula(paste(y, "~", paste(paste0("`",filtered_feature,"`"), collapse=" + ")))
    
    glm_2 <- glm(formula2,
                 data=data,
                 family = binomial())
    
    predict1<- predict(glm_2,
                       newdata = data,
                       type = "response")
    
    pr <- prediction(predict1, data[,p+1])
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    
    weighted_freq[[i]] <- data.frame(filtered_feature,1 * auc)
    
    
    print(i)
    print(filtered_feature)
    
  }
  
  weighted_freq_df <- do.call(rbind.data.frame, weighted_freq)
  weighted_freq_df_sum <- aggregate(weighted_freq_df$X1...auc, by=list(variable=weighted_freq_df$filtered_feature), FUN=sum)
  weighted_freq_df_sum <- arrange(weighted_freq_df_sum, desc(weighted_freq_df_sum$x))
  colnames(weighted_freq_df_sum)[2] <- "weighted_count_AUC"
  
  return(data.frame(weighted_freq_df_sum))
}

source("config.R")


### Logistic regression:
# create data matrix for use in logistic regression
X1 <- edges_endstate[, c("semester", "night", "mfam_night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "sender_fam_id", "receiver_fam_id")]
X0 <- nonedges_endstate[, c("semester", "night", "mfam_night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "sender_fam_id", "receiver_fam_id")]

# add indicator variables
create_indicators <- function(X){
  tmp <- X$sender_role=="mentee"
  X$I_sender_mentee <- 0
  X$I_sender_mentee[tmp] <- 1
  tmp <- X$sender_role=="mentor"
  X$I_sender_mentor <- 0
  X$I_sender_mentor[tmp] <- 1
  X$I_sender_ment <- X$I_sender_mentee + X$I_sender_mentor
  
  tmp <- X$sender_role=="mentee" & X$receiver_role=="mentee"
  X$I_mentee2mentee <- 0
  X$I_mentee2mentee[tmp] <- 1
  tmp <- X$sender_role=="mentee" & X$receiver_role=="mentor"
  X$I_mentee2mentor <- 0
  X$I_mentee2mentor[tmp] <- 1
  tmp <- X$sender_role=="mentor" & X$receiver_role=="mentee"
  X$I_mentor2mentee <- 0
  X$I_mentor2mentee[tmp] <- 1
  tmp <- X$sender_role=="mentor" & X$receiver_role=="mentor"
  X$I_mentor2mentor <- 0
  X$I_mentor2mentor[tmp] <- 1
  
  tmp <- X$receiver_role=="mentee"
  X$I_receiver_mentee <- 0
  X$I_receiver_mentee[tmp] <- 1
  tmp <- X$receiver_role=="mentor"
  X$I_receiver_mentor <- 0
  X$I_receiver_mentor[tmp] <- 1
  X$I_receiver_ment <- X$I_receiver_mentee + X$I_receiver_mentor
 
  tmp <- X$sender_gender=="Male"
  X$I_sender_male <- 0 
  X$I_sender_male[tmp] <- 1
  tmp <- X$sender_gender=="Female"
  X$I_sender_female <- 0 
  X$I_sender_female[tmp] <- 1
  X$I_sender_gender_defined <- X$I_sender_male + X$I_sender_female
  
  tmp <- X$receiver_gender=="Male"
  X$I_receiver_male <- 0 
  X$I_receiver_male[tmp] <- 1
  tmp <- X$receiver_gender=="Female"
  X$I_receiver_female <- 0 
  X$I_receiver_female[tmp] <- 1
  X$I_receiver_gender_defined <- X$I_receiver_male + X$I_receiver_female
  
  tmp <- X$dyad
  X$I_same_dyad <- 0
  X$I_same_dyad[tmp] <- 1
 
  X$I_same_mfam <- X$sender_fam_id == X$receiver_fam_id
  X$I_same_mfam_diff_dyad <- (1-X$I_same_dyad) * X$I_same_mfam
 
  X$I_sender_mentee_female <- X$I_sender_female * X$I_sender_mentee 
  X$I_sender_mentee_male <- X$I_sender_male * X$I_sender_mentee 
  X$I_receiver_mentee_female <- X$I_receiver_female * X$I_receiver_mentee 
  X$I_receiver_mentee_male <- X$I_receiver_male * X$I_receiver_mentee 
  
  X$I_same_gender <- X$I_sender_male * X$I_receiver_male + X$I_sender_female * X$I_receiver_female
  
  X$I_mfam_night <- X$mfam_night
  return(X)
}

logistic_model <- function(df, form, pred_cases, do=c("summary", "anova", "conf", "R2", "plot", "roc", "resid"), sp=0.5, jit=1, alpha=0){
  # construct model object
  m <- glm(form, family=binomial(link='logit'), data=df)
  m_null <- glm(edge~1, family=binomial(link='logit'),data=df)
  if("summary" %in% do){
    print(summary(m))
  }
  if("anova" %in% do){
    print(anova(m))
  }
  if("conf" %in% do){
    print(exp(confint(m)))
  }
  if("R2" %in% do){
    # mcfadden R2
    mfr2 <- 1-logLik(m)/logLik(m_null)
    writeLines(c("","McFadden R2:", round(mfr2, 2)))
  }
  if("roc" %in% do){
    # roc curve
    simple_roc <- function(labels, scores){
      labels <- labels[order(scores, decreasing=TRUE)]
      data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
    }
    roc_df <- simple_roc(df$edge, fitted.values(m))
    ggplot(roc_df, aes(x=FPR, y=TPR)) + geom_point() + geom_abline(slope=1) + ggtitle("ROC Curve")
  }
  if("plot" %in% do){
    plot(m)
  }
  if("resid" %in% do){
    par(mfrow=c(2,1))
    scatter.smooth(jitter(predict(m, type="response"),amount=jit), jitter(residuals(m, type="deviance"),amount=jit), span=sp, cex=0.1, col=rgb(0, 0, 0, alpha), main="Estimated Probability vs Deviance")
    scatter.smooth(jitter(predict(m),amount=jit), jitter(residuals(m, type="deviance"),amount=jit), span=sp, cex=0.1, col=rgb(0, 0, 0, alpha), main="Linear Predictor vs Deviance")
    par(mfrow=c(1,1))
    #loess(predict(m, type="response") ~ residuals(m))
  }
  if("obs" %in% do){
    X_obs2 <- aggregate(edge ~ I_same_gender + I_same_dyad + I_same_mfam_diff_dyad, data=X_sub, FUN=length)
    colnames(X_obs2)[length(names(X_obs2))] <- "counts"
    X_obs <- join(X_obs, X_obs2)
    #X_obs <- merge(X_obs, X_obs2, by=cI_sender_mentee_female + I_sender_mentee_male + I_receiver_mentee_female + I_receiver_mentee_male + I_same_dyad + I_same_mfam_diff_dyad, data=X_sub)
    rm(X_obs2)
    X_obs
  }
  if("pred" %in% do){
    X_obs <- aggregate(form, data=df, FUN=mean)
    X_obs2 <- aggregate(form, data=df, FUN=length)
    X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), c(0,1))
    colnames(X_pred) <- colnames(X_obs)[1:(dim(X_obs)[2]-2)]
    X_pred[, "prob"] <- predict(m, X_pred, type="response")
    X_pred
  }
  return(m)
}

X1 <- create_indicators(X1)
X0 <- create_indicators(X0)

X1$edge <- 1
X0$edge <- 0

X <- rbind(X0, X1)
#X <- X[complete.cases(X),]

# Model each relation type separately:
Xe2r <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentee2mentor,]
Xe2e <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentee2mentee,]
Xr2e <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentor2mentee,]
Xr2r <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentor2mentor,]
f1 <- formula("edge ~ I_same_gender + I_same_dyad * I_mfam_night + I_same_mfam_diff_dyad * I_mfam_night")
todo <- c("summary", "conf", "resid", "R2", "ROC")
al <- 0.01
jitter <- 0.05
m <- logistic_model(Xe2r, f1, do=todo, sp=1, jit=jitter, alpha=al)
m <- logistic_model(Xr2e, f1, do=todo, sp=1, jit=jitter, alpha=al)

f2 <- formula("edge ~ I_same_gender + I_same_mfam_diff_dyad * I_mfam_night")
m <- logistic_model(Xe2e, f2, do=todo, sp=1, jit=jitter, alpha=al)
m <- logistic_model(Xr2r, f2, do=todo, sp=1, jit=jitter, alpha=al)



#X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), c(0,1))
X_pred <- as.data.frame(matrix(c()))
X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), c(0,1))
colnames(X_pred) <- colnames(X_obs)[1:(dim(X_obs)[2]-2)]
#X_pred <- set_types(X_pred)

X_pred[, "prob"] <- predict(m, X_pred, type="response")
X_pred

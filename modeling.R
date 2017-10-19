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

X1 <- create_indicators(X1)
X0 <- create_indicators(X0)

X1$edge <- 1
X0$edge <- 0

X <- rbind(X0, X1)
#X <- X[complete.cases(X),]
X <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1,]

# generate weights
n_edges <- sum(X$edge)
n_non_edges <- sum(1-X$edge)
n_examples <- n_edges + n_non_edges
wt_edges <- round(n_non_edges/n_edges)
X$weight <- X$edge*wt_edges + (1-X$edge)


#m_null <- glm(edge~1, weights=weight, family=binomial(link='logit'),data=X)
#summary(m_null)
m_fam_night <- 1
X_sub <- subset(X, I_mfam_night==1)

m <- glm(edge~I_sender_mentee_female+I_sender_mentee_male+I_receiver_mentee_female+I_receiver_mentee_male+I_same_dyad+I_same_mfam_diff_dyad, family=binomial(link='logit'),data=X_sub)
m_null <- glm(edge~1, family=binomial(link='logit'),data=X_sub)
summary(m)
anova(m) 
#plot(m)

exp(confint(m))
mcfadden_r2 <- 1-logLik(m)/logLik(m_null)
mcfadden_r2

#scatter.smooth(jitter(predict(m),amount=0), jitter(residuals(m),amount=0), cex=0.1)
#scatter.smooth(predict(m, type="response"), residuals(m, type="deviance"), span=0.1, cex=0.1, main="Estimated Probability vs Deviance")
#plot(predict(m, type="response"), residuals(m, type="deviance"), cex=0.1, main="Estimated Probability vs Deviance")
#loess(predict(m, type="response") ~ residuals(m))
#scatter.smooth(predict(m), weighted.residuals(m), span=0.1, cex=0.1, main="Linear Predictor vs Deviance")
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}
roc_df <- simple_roc(X_sub$edge, fitted.values(m))
ggplot(roc_df, aes(x=FPR, y=TPR)) + geom_point() + geom_abline(slope=1) + ggtitle("ROC Curve")

X_obs <- aggregate(edge ~ I_sender_mentee_female + I_sender_mentee_male + I_receiver_mentee_female + I_receiver_mentee_male + I_same_dyad + I_same_mfam_diff_dyad, data=X_sub, FUN=mean)
X_obs2 <- aggregate(edge ~ I_sender_mentee_female + I_sender_mentee_male + I_receiver_mentee_female + I_receiver_mentee_male + I_same_dyad + I_same_mfam_diff_dyad, data=X_sub, FUN=length)
colnames(X_obs2)[length(names(X_obs2))] <- "counts"
X_obs <- join(X_obs, X_obs2)
#X_obs <- merge(X_obs, X_obs2, by=cI_sender_mentee_female + I_sender_mentee_male + I_receiver_mentee_female + I_receiver_mentee_male + I_same_dyad + I_same_mfam_diff_dyad, data=X_sub)
rm(X_obs2)
X_obs

X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), c(0,1))
colnames(X_pred) <- colnames(X_obs)[1:(dim(X_obs)[2]-2)]
#X_pred <- set_types(X_pred)

X_pred[, "prob"] <- predict(m, X_pred, type="response")
X_pred

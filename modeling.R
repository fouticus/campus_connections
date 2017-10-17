source("config.R")


### Logistic regression:
# create data matrix for use in logistic regression
X1 <- edges_endstate[, c("semester", "night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "m_fam")]
X0 <- nonedges_endstate[, c("semester", "night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "m_fam")]

# add indicator variables
create_indicators <- function(X){
  X[,"sender_role_X"] <- NA
  X[X[,"sender_role"]=="mentee", "sender_role_X"] <- 0
  X[X[,"sender_role"]=="mentor", "sender_role_X"] <- 1
  
  X[,"receiver_role_X"] <- NA
  X[X[,"receiver_role"]=="mentee", "receiver_role_X"] <- 0
  X[X[,"receiver_role"]=="mentor", "receiver_role_X"] <- 1
  
  X[,"sender_gender_X"] <- NA
  X[X[,"sender_gender"]=="Male", "sender_gender_X"] <- 0
  X[X[,"sender_gender"]=="Female", "sender_gender_X"] <- 1
  X[,"receiver_gender_X"] <- NA
  X[X[,"receiver_gender"]=="Male", "receiver_gender_X"] <- 0
  X[X[,"receiver_gender"]=="Female", "receiver_gender_X"] <- 1
  
  X[, "dyad_X"] <- 0
  X[X[,"dyad"], "dyad_X"] <- 1
  
  return(X)
}

X1 <- create_indicators(X1)
X0 <- create_indicators(X0)

X1[,"edge"] <- 1
X0[,"edge"] <- 0

X <- rbind(X0, X1)
X <- X[complete.cases(X),]

# generate weights
n_edges <- sum(X[,"edge"] == 1)
n_non_edges <- sum(X[,"edge"] == 0)
n_examples <- n_edges + n_non_edges
wt_edges <- round(n_non_edges/n_edges)
X[,"weight"] <- X[,"edge"]*wt_edges + (1-X[,"edge"])

set_types <- function(df){
  df$sender_gender_X <- factor(df$sender_gender_X, levels=c(0, 1), labels=c("Male", "Female"))
  df$receiver_gender_X <- factor(df$receiver_gender_X, levels=c(0, 1), labels=c("Male", "Female"))
  df$sender_role_X <- factor(df$sender_role_X, levels=c(0, 1), labels=c("mentee", "mentor"))
  df$receiver_role_X <- factor(df$receiver_role_X, levels=c(0, 1), labels=c("mentee", "mentor"))
  df$dyad_X <- factor(df$dyad_X, levels=c(0, 1), labels=c("non-dyad", "dyad"))
  df$m_fam <- factor(df$m_fam, levels=c(0, 1), labels=c("non-mfam", "mfam"))
  return(df)
}
X <- set_types(X)

#m <- glm(edge~sender_role_X*receiver_role_X*dyad+dyad_X*m_fam, weights=weight, family=binomial(link='logit'),data=X)
#m <- glm(edge~sender_gender_X*receiver_gender_X*sender_role_X*receiver_role_X*dyad_X*m_fam, weights=weight, family=binomial(link='logit'),data=X)
m_null <- glm(edge~1, weights=weight, family=binomial(link='logit'),data=X)
#m <- glm(edge~sender_gender_X*sender_role_X+receiver_gender_X*receiver_role_X+sender_role_X*receiver_role_X+dyad_X*m_fam, weights=weight, family=binomial(link='logit'),data=X)
m <- glm(edge~sender_gender_X*sender_role_X+receiver_gender_X*receiver_role_X+sender_role_X*receiver_role_X+dyad_X*m_fam, family=binomial(link='logit'),data=X)
#m <- glm(edge~dyad_X, weights=weight, family=binomial(link='logit'),data=X)
#m <- glm(edge~sender_role_X*receiver_role_X+dyad_X*m_fam, family=binomial(link='logit'),data=X)
#m <- glm(edge~sender_role_X*receiver_role_X + dyad_X*m_fam + sender_role_X*dyad_X + receiver_role_X*dyad_X, family=binomial(link='logit'),data=X)
#m <- glm(edge~sender_role_X*receiver_role_X*dyad_X + dyad_X*m_fam, family=binomial(link='logit'),data=X)
summary(m_null)
summary(m)
anova(m) 
#exp(confint(m))
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
roc_df <- simple_roc(X$edge, fitted.values(m))
ggplot(roc_df, aes(x=FPR, y=TPR)) + geom_point() + geom_abline(slope=1) + ggtitle("ROC Curve")
#plot(m)

X_obs <- aggregate(edge ~ sender_role_X + receiver_role_X + dyad_X + m_fam, data=X, FUN=mean)
X_obs2 <- aggregate(edge ~ sender_role_X + receiver_role_X + dyad_X + m_fam, data=X, FUN=length)
X_obs <- merge(X_obs, X_obs2, by=c("sender_role_X", "receiver_role_X", "dyad_X", "m_fam"))
rm(X_obs2)
X_obs

X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1), c(0,1), c(0,1))
colnames(X_pred) <- c("sender_role_X", "receiver_role_X", "dyad_X", "m_fam", "sender_gender_X", "receiver_gender_X")
X_pred <- set_types(X_pred)

X_pred[, "prob"] <- predict(m, X_pred, type="response")
X_pred

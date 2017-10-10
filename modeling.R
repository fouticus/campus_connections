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

X1 <- create_indicators(X)
X0 <- create_indicators(X0)

X1[,"edge"] <- 1
X0[,"edge"] <- 0

X <- rbind(X0, X1)
X <- X[complete.cases(X),]

set_types <- function(df){
  df$sender_role_X <- factor(df$sender_role_X, levels=c(0, 1), labels=c("mentee", "mentor"))
  df$receiver_role_X <- factor(df$receiver_role_X, levels=c(0, 1), labels=c("mentee", "mentor"))
  df$dyad_X <- factor(df$dyad_X, levels=c(0, 1), labels=c("non-dyad", "dyad"))
  df$m_fam <- factor(df$m_fam, levels=c(0, 1), labels=c("mfam", "non-mfam"))
  return(df)
}
X <- set_types(X)

m <- glm(edge~sender_gender_X+receiver_gender_X+sender_role_X*receiver_role_X+dyad_X*m_fam, family=binomial(link='logit'),data=X)
m <- glm(edge~sender_role_X*receiver_role_X + dyad_X*m_fam + sender_role_X*dyad_X + receiver_role_X*dyad_X, family=binomial(link='logit'),data=X)
m <- glm(edge~sender_role_X*receiver_role_X*dyad_X + dyad_X*m_fam, family=binomial(link='logit'),data=X)
summary(m)
anova(m) 

X_pred <- expand.grid(c(0,1), c(0,1), c(0,1), c(0,1))
#X_pred <- as.data.frame(matrix(c(0, 1, 0, 0,   0, 1, 1, 0,   0, 1, 0, 1,   0, 1, 1, 1), 4, byrow=TRUE))
#X_pred <- rbind(X_pred, matrix(c(1, 0, 0, 0,   1, 0, 1, 0,   1, 0, 0, 1,   1, 0, 1, 1), 4, byrow=TRUE))
colnames(X_pred) <- c("sender_role_X", "receiver_role_X", "dyad_X", "m_fam")
X_pred <- set_types(X_pred)

X_pred[, "prob"] <- predict(m, X_pred[1:4])
X_pred

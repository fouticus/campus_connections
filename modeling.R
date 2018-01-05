########################################################################################
## modeling.R
## fits statistical network models to end state network data
########################################################################################

# run config file
source("config.R")

##########################
#### Helper Functions ####
##########################
# add indicator variables to dataframe
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

# fit a logistic model and perform whatever actions are specified in "do"
logistic_model <- function(df, form, pred_cases, do=c("summary", "anova", "conf", "R2", "plot", "roc", "resid"), sp=0.5, jit=1, alpha=0){
  ret_env <- new.env()  # This environment will be returned by this function
  vars <- all.vars(form)
  # construct model object and null model
  m <- glm(form, family=binomial(link='logit'), data=df)
  m_null <- glm(edge~1, family=binomial(link='logit'),data=df)
  ret_env$m <- m
  # perform various operations depending on the array "do"
  if("summary" %in% do){
    ret_env$summary <- summary(m)
    print(ret_env$summary)
  }
  if("anova" %in% do){
    ret_env$anova <- anova(m)
    print(ret_env$anova)
  }
  if("conf" %in% do){
    ret_env$conf <- exp(confint(m))
    print(ret_env$conf)
  }
  if("R2" %in% do){
    # mcfadden R2
    ret_env$mfr2 <- 1-logLik(m)/logLik(m_null)
    writeLines(c("","McFadden R2:", round(ret_env$mfr2, 2)))
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
  # compute residuals and plot them
  if("resid" %in% do){
    par(mfrow=c(2,1))
    scatter.smooth(jitter(predict(m, type="response"),amount=jit), jitter(residuals(m, type="deviance"),amount=jit), span=sp, cex=0.1, col=rgb(0, 0, 0, alpha), main="Estimated Probability vs Deviance")
    scatter.smooth(jitter(predict(m),amount=jit), jitter(residuals(m, type="deviance"),amount=jit), span=sp, cex=0.1, col=rgb(0, 0, 0, alpha), main="Linear Predictor vs Deviance")
    par(mfrow=c(1,1))
    #loess(predict(m, type="response") ~ residuals(m))
  }
  # calculate the observed probability of each variable setting in the model
  if("obs" %in% do){
    sum_form <- formula(paste(vars[1], "~", paste(vars[2:length(vars)], collapse=" + ")))
    X_obs  <- aggregate(sum_form, data=df, FUN=mean)
    X_obs2 <- aggregate(sum_form, data=df, FUN=length)
    colnames(X_obs)[length(names(X_obs))] <- "observed_prob"
    colnames(X_obs2)[length(names(X_obs2))] <- "counts"
    X_obs <- join(X_obs, X_obs2)
    ret_env$X_obs <- X_obs
    if(!("pred" %in% do)){
      print(X_obs)
    }
  }
  # use the model to predict probabilities for the cases specified in pred_cases
  if("pred" %in% do){
    X_pred <- pred_cases
    colnames(X_pred) <- vars[2:length(vars)]
    X_pred$predicted_prob <- predict(m, X_pred, type="response")
    ret_env$X_pred <- X_pred
    if(!("obs" %in% do)){
      print(X_pred)
    }
  }
  # combine predicted and observed if we calculated both
  if("pred" %in% do & "obs" %in% do){
    X_comb <- join(X_pred, X_obs, type='full')
    X_comb$diff <- X_comb$observed_prob - X_comb$predicted_prob
    ret_env$X_comb <- X_comb
    print(X_comb)
  }
  return(ret_env)
}
#  Write results of the model to file. optionally specify which items to save using "items"
write_model_results <- function(m, fname, items=NA){
  f<-file(paste(output_dir, fname, ".txt", sep=""))
  if(is.na(items)){
    items <- sort(names(m))
  }
  sink(file=f)  # Direct output to file
  print(fname)
  for(item in items){
    #write(str(m[[item]]), f)
    #dput(m[[item]], f)
    if(item != "m"){
      writeLines(c(" ", paste("###############", item, "###############", " ")), con=f)
      print(m[[item]])
    }
  }
  sink()  # Reset output
  close(f)
}

###################
#### Data Prep ####
###################
# create data matrix for use in logistic regression
X1 <- edges_endstate[, c("semester", "night", "mfam_night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "sender_fam_id", "receiver_fam_id", "sender_final_id", "receiver_final_id")]
X0 <- nonedges_endstate[, c("semester", "night", "mfam_night", "sender_role", "receiver_role", "dyad", "sender_gender", "receiver_gender", "sender_fam_id", "receiver_fam_id", "sender_final_id", "receiver_final_id")]

X1 <- create_indicators(X1)
X0 <- create_indicators(X0)

X1$edge <- 1
X0$edge <- 0
# combine edges and non-edges
X <- rbind(X0, X1)


# plot settings
al <- 0.01  # alpha level (transparency)
jitter <- 0.05  # staggering of points so we can better estimate quantity/density
todo <- c("summary", "conf", "R2", "obs", "pred")


################################################
#### SBM with 2 blocks: mentors and mentees ####
################################################
# extract edges between mentors and mentees for which we know the gender
X_sbm <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1,]
# which cases to generate predictions for using the fit model
pred_cases_sbm <- data.frame(rbind(c(0, 0),  # Mentor to mentor
                                   c(0, 1),  # Mentor to mentee, etc.
                                   c(1, 0),
                                   c(1, 1)))
# train model and write results to file
f_sbm <- formula("edge ~ I_sender_mentee * I_receiver_mentee")  # 2 block model
cat("###########################\n#### SBM with 2 blocks ####\n###########################\n")
m_sbm <- logistic_model(X_sbm, f_sbm, pred_cases_sbm, do=todo, sp=1, jit=jitter, alpha=al)
write_model_results(m_sbm, "LR_results_2block_SBM")


##################################################################
#### SBM with 2 blocks: mentors and mentees, and MF Condition ####
##################################################################
# which cases to generate predictions for using the fit model
pred_cases_sbm2 <- data.frame(rbind(c(0, 0, 0),  # Mentor to Mentor in control group
                                    c(0, 0, 1),
                                    c(0, 1, 0),
                                    c(0, 1, 1),
                                    c(1, 0, 0),
                                    c(1, 0, 1),  # Mentee to mentor in treatment group
                                    c(1, 1, 0),
                                    c(1, 1, 1)))
# train model and write results to file
f_sbm2 <- formula("edge ~ I_mfam_night * I_sender_mentee * I_receiver_mentee")  # All possible interactions
cat("############################################\n#### SBM with 2 blocks and MF Condition ####\n############################################\n")
m_sbm2 <- logistic_model(X_sbm, f_sbm2, pred_cases_sbm2, do=todo, sp=1, jit=jitter, alpha=al)
write_model_results(m_sbm2, "LR_results_2block_SBM_w_condition")


########################################################
########## Model Each relation type separately. ########
#### Like SBM but taking other factors into account ####
########################################################
# set up a different data set for each relation type
# Mentee to Mentor
Xe2r <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentee2mentor,]
# Mentee to Mentee
Xe2e <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentee2mentee,]
# Mentor to Mentee
Xr2e <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentor2mentee,]
# Mentee to Mentee
Xr2r <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$I_mentor2mentor,]
# which cases to generate predictions for using the fit model
# these cases used for cross-group relationships (mentee->mentor and mentor->mentee)
pred_cases <- data.frame(rbind(c(0, 0, 0, 0),  # control group, diff dyad, diff family, diff gender
                               c(0, 0, 0, 1),
                               c(0, 0, 1, 0),
                               c(0, 0, 1, 1),
                               c(0, 1, 0, 0), 
                               c(0, 1, 0, 1),
                               c(1, 0, 0, 0),
                               c(1, 0, 0, 1),
                               c(1, 0, 1, 0),  # treatment group, diff dyad, same family, diff gender
                               c(1, 0, 1, 1),
                               c(1, 1, 0, 0),
                               c(1, 1, 0, 1)))
# these cases used for within-group relationships (mentee->mentee and mentor->mentor)
pred_cases2 <- data.frame(rbind(c(0, 0, 0),  # control group, diff family, diff gender
                                c(0, 0, 1),
                                c(0, 1, 0),
                                c(0, 1, 1),
                                c(1, 0, 0),
                                c(1, 0, 1),
                                c(1, 1, 0),
                                c(1, 1, 1)))  # treatment group, same family, same gender
# train models and write results to file
f1 <- formula("edge ~ I_mfam_night * I_same_dyad + I_mfam_night * I_same_mfam_diff_dyad + I_same_gender")
f2 <- formula("edge ~ I_mfam_night * I_same_mfam_diff_dyad + I_same_gender")
cat("##########################\n#### Mentee to Mentor ####\n##########################\n")
m_e2r <- logistic_model(Xe2r, f1, pred_cases, do=todo, sp=1, jit=jitter, alpha=al)
cat("##########################\n#### Mentor to Mentee ####\n##########################\n")
m_r2e <- logistic_model(Xr2e, f1, pred_cases, do=todo, sp=1, jit=jitter, alpha=al)
cat("##########################\n#### Mentee to Mentee ####\n##########################\n")
m_e2e <- logistic_model(Xe2e, f2, pred_cases2, do=todo, sp=1, jit=jitter, alpha=al)
cat("##########################\n#### Mentor to Mentor ####\n##########################\n")
m_r2r <- logistic_model(Xr2r, f2, pred_cases2, do=todo, sp=1, jit=jitter, alpha=al)
write_model_results(m_e2r, "LR_results_Mentee2Mentor")
write_model_results(m_r2e, "LR_results_Mentor2Mentee")
write_model_results(m_e2e, "LR_results_Mentee2Mentee")
write_model_results(m_r2r, "LR_results_Mentor2Mentor")


## do this per night just for kicks (doesn't give great results)
#todo <- c("summary", "conf", "obs")
#for(sem in c("Fa15", "Sp16", "Fa16", "Sp17")){
#  for(nigt in c("Mon", "Tue", "Wed", "Thu")){
#    print(paste("####  Semester:", sem, "  Night:", nigt, " ####"))
#    Xsn <- X[X$semester == sem & X$night == nigt & X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 &  X$I_sender_ment ==1 & X$I_receiver_ment == 1,]
#    Xe2r <- Xsn[Xsn$I_mentee2mentor,]
#    Xe2e <- Xsn[Xsn$I_mentee2mentee,]
#    Xr2e <- Xsn[Xsn$I_mentor2mentee,]
#    Xr2r <- Xsn[Xsn$I_mentor2mentor,]
#    f4 <- formula("edge ~ I_same_gender + I_same_mfam_diff_dyad + I_same_dyad")
#    m_e2r <- logistic_model(Xe2r, f4, do=todo, sp=1, jit=jitter, alpha=al)
#    m_r2e <- logistic_model(Xr2e, f4, do=todo, sp=1, jit=jitter, alpha=al)
#    f5 <- formula("edge ~ I_same_gender + I_same_mfam_diff_dyad")
#    m_e2e <- logistic_model(Xe2e, f5, do=todo, sp=1, jit=jitter, alpha=al)
#    m_r2r <- logistic_model(Xr2r, f5, do=todo, sp=1, jit=jitter, alpha=al)
#  }
#}

    
    
###############################################################
###################### Model Per Night ########################
####### Blocks are (mentor, male mentee, female mentee) #######
#### Other factors are same/diff dyad and same/diff family ####
###############################################################
pred_cases3 <- data.frame(rbind(c(0, 0, 0, 0, 0, 0),  # sender mentor, receiver mentor, diff dyad, diff family
                                c(0, 0, 0, 0, 0, 1),
                                c(1, 0, 0, 0, 0, 0),  
                                c(1, 0, 0, 0, 0, 1),  # sender male mentee, receiver mentor, diff dyad, same family
                                c(1, 0, 0, 0, 1, 0),
                                c(0, 1, 0, 0, 0, 0),
                                c(0, 1, 0, 0, 0, 1),
                                c(0, 1, 0, 0, 1, 0),
                                c(0, 0, 1, 0, 0, 0),
                                c(0, 0, 1, 0, 0, 1),
                                c(0, 0, 1, 0, 1, 0),
                                c(1, 0, 1, 0, 0, 0),
                                c(1, 0, 1, 0, 0, 1),
                                c(0, 1, 1, 0, 0, 0),
                                c(0, 1, 1, 0, 0, 1),  # sender mentee female, receiver mentee female, diff dyad, same family
                                c(0, 0, 0, 1, 0, 0),
                                c(0, 0, 0, 1, 0, 1),  
                                c(0, 0, 0, 1, 1, 0),
                                c(1, 0, 0, 1, 0, 0),
                                c(1, 0, 0, 1, 0, 1),
                                c(0, 1, 0, 1, 0, 0),
                                c(0, 1, 0, 1, 0, 1)))
todo <- c("summary", "conf", "R2", "obs", "pred")
f3 <- formula("edge ~ I_sender_mentee_male + I_sender_mentee_female + I_receiver_mentee_male + I_receiver_mentee_female + I_sender_mentee_male*I_receiver_mentee_male + I_sender_mentee_female*I_receiver_mentee_male + I_sender_mentee_male*I_receiver_mentee_female + I_sender_mentee_female*I_receiver_mentee_female + I_same_dyad + I_same_mfam_diff_dyad")
ms <- list()  # list of all trained models
# fit this model on each nights
for(sem in c("Fa15", "Sp16", "Fa16", "Sp17")){
  for(nigt in c("Mon", "Tue", "Wed", "Thu")){
    # determine if mfam condition or not
    cat(paste("#######################################\n####  Semester:", sem, "  Night:", nigt, " ####\n#######################################\n"))
    Xsn <- X[X$I_sender_gender_defined * X$I_receiver_gender_defined == 1 & X$I_sender_ment == 1 & X$I_receiver_ment == 1 & X$semester == sem & X$night == nigt,]
    mfam <- if(Xsn$mfam_night[1] == 1) "Yes" else "No"
    print(paste("mfam?", mfam))
    m <- logistic_model(Xsn, f3, pred_cases3, todo, sp=1, jit=jitter, alpha=al)
    m$mfam <- mfam
    m$name <- paste("LR_results", sem, nigt, sep="_")
    write_model_results(m, m$name)
    ms <- c(ms, m)
  }
}
# join the observed and predicted probabilites from each model into a single table and write to file
master <- NA
for(m in ms){
  X_comb <- m$X_comb
  X_comb$name <- m$name
  if(is.na(master)){
    master <- X_comb
  } else {
    master <- rbind(master, X_comb)
  }
}
write.csv(master, paste(output_dir, "LR_results_by_night_pred_obs.csv", sep=""))


#################
#### Cleanup ####
#################
rm(create_indicators)
rm(logistic_model)
rm(write_model_results)
rm(al)
rm(f_sbm)
rm(f_sbm2)
rm(f1)
rm(f2)
rm(f3)
rm(f4)
rm(f5)
rm(jitter)
rm(mfam)
rm(nigt)
rm(sem)
rm(todo)

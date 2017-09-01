# clear workspace
rm(list=ls())
gc()

# load data
edges <- read.csv("../data/CC_Edgelist.csv")
mentees <- read.csv("../data/Mentee_Attributes.csv")
staff <- read.csv("../data/Staff_Attributes.csv")

# clean
staff$role1[staff$role1 == "Instructor"] <- "instructor"
mentees <- rename(mentees, c("Role"="role"))

# collect those people that were active on second nights
staff2 <- staff[staff$night2!="",]
colnames(staff2)[4] <- "night"
colnames(staff2)[7] <- "role"
staff2$nightnum <- 2
staff2 <- subset(staff2, select=-c(night1, role1))


fixrole <- function(dataframe){
  for(row in staff2){
    # if mentor on night 2, then mentee = 1 and room number is not blank
    if(row$role == "mentor"){
      row$mentee <- 1
    }
    # if not mentor on night 2, then mentee = 0 and room number is NA
    else{
      row$mentee <- 0
      row$room <- NA
    }
  }
}




colnames(staff)[3] <- "night"
colnames(staff)[6] <- "role"
staff2$nightnum <- 1



# add labels, etc.
add_labels <- function(df)
{
  df$gender = factor(df$gender, levels=c(-1, 0, 1), labels=c("Other", "Female", "Male"))
  df$mfcond= factor(df$mfcond, levels=c(1, 0), labels=c("Mentor Family", "No Mentor Family"))
  df$room = factor(df$room, levels=c(144, 145), labels=c("Room 144", "Room 145"))
  # df$semester = factor(df$semester, levels=c("F15", "S16", "F16"), labels=c("Fall 15", "Spring 16", "Fall 16"))
  if ("night" %in% colnames(df)){
    df$night = factor(df$night, levels=c("Monday", "Tuesday", "Wednesday", "Thursday"), labels=c("Mon", "Tue", "Wed", "Thu"))
  } else {
    df$night1 = factor(df$night1, levels=c("monday", "tuesday", "wednesday", "thursday"), labels=c("Mon", "Tue", "Wed", "Thu"))
    df$night2 = factor(df$night2, levels=c("monday", "tuesday", "wednesday", "thursday"), labels=c("Mon", "Tue", "Wed", "Thu"))
    df$role1 = factor(df$role1, levels=c("mentor", "mentor coach", "lead mentor coach", "instructor"), labels=c("mentor", "coach", "lead", "instr"))
  }
  return(df)
}

staff <- add_labels(staff)
mentees <- add_labels(mentees)
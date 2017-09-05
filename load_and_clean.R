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

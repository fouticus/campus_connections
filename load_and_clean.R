# clear workspace
rm(list=ls())
gc()

source("config.R")

# convert data to lowercase with python script
pyscript = "python ../data/tolowercase.py"
system(paste(pyscript, paste(data_dir, "CC_Edgelist_foutfixed.csv", sep="")))
system(paste(pyscript, paste(data_dir, "Mentee_Attributes.csv", sep="")))
system(paste(pyscript, paste(data_dir, "Staff_Attributes.csv", sep="")))

# load data
#edges <- read.csv("../data/CC_Edgelist.csv")
edges <- read.csv(paste(data_dir, "CC_Edgelist_foutfixed.csv", sep=""))
mentees <- read.csv(paste(data_dir, "Mentee_Attributes.csv", sep=""))
staff <- read.csv(paste(data_dir, "Staff_Attributes.csv", sep=""))

# collect those people that were active on second nights
staff2 <- staff[staff$night2!="",]
colnames(staff2)[4] <- "night"  # rename columns
colnames(staff2)[7] <- "role"
staff2$nightnum <- 2  # create factor for whether this is night1 or night2
staff2 <- subset(staff2, select=-c(night1, role1))  # remove unused columns
colnames(staff)[3] <- "night"  # rename columns
colnames(staff)[6] <- "role"
staff$nightnum <- 1  # create factor for whether this is night1 or night2
staff <- subset(staff, select=-c(night2, role2))  # remove unused columns

# mentee and room reflect night1 role, so correct if night 2 person is in non-mentor role
fixrole <- function(dataframe){
  rowfun <- function(row, role_col, mentee_col, room_col){
    # if mentor on night 2, then mentee = 1 and room number is not blank
    if(row[role_col] == "mentor"){
      row[mentee_col] <- 1
    }
    # if not mentor on night 2, then mentee = 0 and room number is NA
    else{
      row[mentee_col] <- 0
      row[room_col] <- NA
    }
    return(row)
  }
  role_col = which(colnames(dataframe)=="role")
  mentee_col = which(colnames(dataframe)=="mentee")
  room_col = which(colnames(dataframe)=="room")
  # loop through each row and apply rowfun
  dataframe = data.frame(t(apply(dataframe, 1, rowfun, role_col=role_col, mentee_col=mentee_col, room_col=room_col)))
  return(dataframe)
}
# apply function
staff2 <- fixrole(staff2)
staff <- fixrole(staff)

# stack them on top of one another.
staff <- rbind(staff, staff2)
rm(staff2)

# fix gender column which was changed into character type
staff$gender <- as.numeric(as.character(staff$gender))


# add labels, etc.
add_labels <- function(df)
{
  df$gender = factor(df$gender, levels=c(-1, 0, 1), labels=c("Other", "Female", "Male"))
  df$mfcond= factor(df$mfcond, levels=c(1, 0), labels=c("Mentor Family", "No Mentor Family"))
  df$room = factor(df$room, levels=c(144, 145), labels=c("Room 144", "Room 145"))
  df$night = factor(df$night, levels=c("monday", "tuesday", "wednesday", "thursday"), labels=c("Mon", "Tue", "Wed", "Thu"))
  df$semester = factor(df$semester, levels=c("f15", "s16", "f16", "s17"), labels=c("Fa15", "Sp16", "Fa16", "Sp17"))
  return(df)
}

mentees <- add_labels(mentees)
staff <- add_labels(staff)
staff$role = factor(staff$role, levels=c("mentor", "mentor coach", "lead mentor coach", "instructor"), labels=c("mentor", "coach", "lead", "instr"))
 
# addd labels for edge list
add_edge_labels <- function(df)
{
  df$night = factor(df$night, levels=c("monday", "tuesday", "wednesday", "thursday"), labels=c("Mon", "Tue", "Wed", "Thu"))
  df$semester = factor(df$semester, levels=c("f15", "s16", "f16", "s17"), labels=c("Fa15", "Sp16", "Fa16", "Sp17"))
  return(df)
}
edges <- add_edge_labels(edges)
edges$sender_final_id <- as.character(edges$sender_final_id)
edges$receiver_final_id <- as.character(edges$receiver_final_id)

# join mentees and staff for use in graph
staff2 <- staff
staff2$date_dropped <- factor(NA)
mentees2 <- mentees
mentees2$nightnum <- factor(NA)
mentees2$mentee <- factor(NA)

participants <- smartbind(staff2, mentees2)
rm(mentees2, staff2)
role_nums <- as.factor(participants$role)
levels(role_nums) <- 1:length(role_nums)
participants$role_num <- as.numeric(role_nums)
participants <- participants[with(participants, order(role, final_id)),] # order them for consistency in plotting graphs
participants$final_id <- as.character(participants$final_id)

staff = subset(participants, participants$role != "mentee")
mentees = subset(participants, participants$role == "mentee")

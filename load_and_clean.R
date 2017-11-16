# clear workspace
rm(list=ls())
gc()

source("config.R")

file_suffix <- "_09.27.17"
file_suffix2 <- "_10.13.17"
edgelist_file <- paste("CC_Edgelist", file_suffix2, ".csv", sep="")
valid_edgelist_file <- paste("CC_Edgelist_valid", file_suffix2, ".csv", sep="")
mentee_atr_file <- paste("Mentee_Attributes", file_suffix, ".csv", sep="")
staff_atr_file <- paste("Staff_Attributes", file_suffix, "_fout.csv", sep="")
conditions_file <- paste("TX_Assign_fout", ".csv", sep="")
mentee_master_file <- paste("MASTER", ".csv", sep="")
#mentor_master_file <- paste("Staff_Master", ".csv", sep="")
mentor_survey_file <- paste("MENTORSURVEY", ".csv", sep="")

# filter out edges to those that are valid
pyscript2 = paste("python", "../data/valid_edges.py", paste(data_dir, edgelist_file, sep=""), paste(data_dir, valid_edgelist_file, sep=""), sep=" ") 
system(pyscript2)

# convert data to lowercase with python script
pyscript = "python ../data/tolowercase.py"
#system(paste(pyscript, paste(data_dir, "CC_Edgelist_foutfixed.csv", sep="")))
#system(paste(pyscript, paste(data_dir, "CC_Edgelist.csv", sep="")))
system(paste(pyscript, paste(data_dir, edgelist_file, sep="")))
system(paste(pyscript, paste(data_dir, mentee_atr_file, sep="")))
system(paste(pyscript, paste(data_dir, staff_atr_file, sep="")))
system(paste(pyscript, paste(data_dir, mentee_master_file, sep="")))
#system(paste(pyscript, paste(data_dir, mentor_master_file, sep="")))
system(paste(pyscript, paste(data_dir, conditions_file, sep="")))


# load data
edges <- read.csv(paste(data_dir, valid_edgelist_file, sep=""))
mentees <- read.csv(paste(data_dir, mentee_atr_file, sep=""))
staff <- read.csv(paste(data_dir, staff_atr_file, sep=""))

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


# exclude mentees/edges with no-start equal to 1
mentees <- mentees[which(is.na(mentees$no_start)),]
all_ids <- c(as.character(unique(staff$final_id)), as.character(unique(mentees$final_id)))
edges <- edges[which(with(edges, sender_final_id %in% all_ids & receiver_final_id %in% all_ids)),]


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
  df$semester = factor(df$semester, levels=c("f15", "s16", "f16", "s17", "f17", "s18"), labels=c("Fa15", "Sp16", "Fa16", "Sp17", "Fa17", "Sp18"))
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

# create valid edges dataset
valid_edges <- edges[with(edges, sender_missing==0 & sn1==1 & !is.na(sn2) & sender_final_id!=receiver_final_id), ]
roles <- participants[,c("final_id", "role", "night")]
valid_edges <- merge(valid_edges, roles, by.x=c("sender_final_id", "night"), by.y=c("final_id", "night"))
colnames(valid_edges)[length(colnames(valid_edges))] <- "sender_role"
valid_edges <- merge(valid_edges, roles, by.x=c("receiver_final_id", "night"), by.y=c("final_id", "night"))
colnames(valid_edges)[length(colnames(valid_edges))] <- "receiver_role"
valid_edges$pair_id <- paste(valid_edges$sender_final_id, valid_edges$receiver_final_id, sep="->")
valid_edges$dyad <- substr(valid_edges$sender_final_id, 2, 999) == substr(valid_edges$receiver_final_id, 2, 999)

valid_edges$sender_role=factor(valid_edges$sender_role, levels=role_labels)
valid_edges$receiver_role=factor(valid_edges$receiver_role, levels=role_labels)


### Create end-state network edge list
# select only edges from the last two surveys
edges_45 <- valid_edges[valid_edges$survnum==4 | valid_edges$survnum==5,]

##### We changed our minds, no longer are we excluding people who send no relationships, now just exclude if they are not present on both weeks
# count how many times each relationship is mentioned (max 2 if mentioned in both surveys) (put answer in sn1)
#count_45 <- aggregate(sn1~receiver_final_id+night+sender_final_id+semester, edges_45, length)
# count a person as being present on both nights if any outgoing relationship is indicated in both weeks (take max over sn1)
#present_45 <- aggregate(sn1~semester+night+sender_final_id, count_45, max) 
# include a person if they were present on both nights (sn1 should be 2 for these senders)
#present_45 <- present_45[which(present_45$sn1==2),1:dim(present_45)[2]-1]  # omit sn1 column
##### New way of doing this, include people if they were present on both nights
present_45 <- aggregate(sn1~semester+night+sender_final_id+survnum, edges_45, sum)[,1:4]
present_45 <- aggregate(survnum~semester+night+sender_final_id, present_45, sum)
present_45 <- present_45[present_45$survnum==9,][1:3]


# select edges where sender and receiver are in this population. This must be specific to a semester/night, since some mentors serve multiple nights in a semester
# these are inner joins, so they keep only senders and receivers in the present_45 list.
colnames(present_45) <- c("semester", "night", "final_id")
edges_45 <- merge(edges_45, present_45, by.x=c("night", "semester", "sender_final_id"), by.y=c("night", "semester", "final_id"))
edges_45 <- merge(edges_45, present_45, by.x=c("night", "semester", "receiver_final_id"), by.y=c("night", "semester", "final_id"))
edges_endstate <- edges_45

# ensure the specific relationship (not just the sender/receiver) is present in both weeks
relation_present <- aggregate(sn2~semester+night+pair_id, edges_endstate, length)
relation_present <- relation_present[relation_present$sn2==2, 1:dim(relation_present)[2]-1]
# select edges where the relationship is in both lists (inner merge, so keeps only relationships in relation_present)
edges_endstate <- merge(edges_endstate, relation_present, by.x=c("semester", "night", "pair_id"), by.y=c("semester", "night", "pair_id"))
# average last two surveys
edges_endstate <- aggregate(sn2~receiver_final_id+night+sender_final_id+semester+sn1+sender_missing+receiver_missing+sender_role+receiver_role+pair_id+dyad, edges_endstate, mean)

# summarize the people that are excluded from the end state networks:
senders <- as.data.frame(unique(edges_endstate$sender_final_id))
receivers <- as.data.frame(unique(edges_endstate$receiver_final_id))
colnames(senders) <- colnames(receivers) <- c("final_id")
present_endstate <- join(senders, receivers, type="full")
participants_not_in_endstate_network <- anti_join(participants, present_endstate)
write.csv(participants_not_in_endstate_network, file=paste(output_dir, "participants_not_in_end_state_network.csv", sep=""))


## build list of non-edges
# create all pairs
nonedges_endstate <- data.frame(sender_final_id=character(0), receiver_final_id=character(0))
for(sem in unique(present_45$semester)){
  for(night in unique(present_45[present_45$semester==sem,]$night)){
    p45_sem_night <- present_45[which(present_45$semester==sem & present_45$night==night), c("final_id"), drop=FALSE]
    senders <- p45_sem_night
    receivers <- p45_sem_night
    names(senders) <- c("sender_final_id")
    names(receivers) <- c("receiver_final_id")
    combos <- cbind(sem, night, expand.grid(t(senders), t(receivers)))
    colnames(combos) <- c("semester", "night", "sender_final_id", "receiver_final_id")
    nonedges_endstate <- rbind(nonedges_endstate, combos) 
  }
}
# compute pair id and exclude edges in the edges_endstate set
nonedges_endstate$pair_id <- paste(nonedges_endstate$sender_final_id, nonedges_endstate$receiver_final_id, sep="->")
nonedges_endstate <- anti_join(nonedges_endstate, edges_endstate, by=c("pair_id"))
# exclude loops (self edges)
nonedges_endstate <- with(nonedges_endstate, nonedges_endstate[sender_final_id != receiver_final_id,])
# complete rest of table
nonedges_endstate <- merge(nonedges_endstate, participants[, c("final_id","role","semester","night")], by.x=c("sender_final_id","semester","night"), by.y=c("final_id","semester","night"))
nonedges_endstate <- merge(nonedges_endstate, participants[, c("final_id","role","semester","night")], by.x=c("receiver_final_id","semester","night"), by.y=c("final_id","semester","night"))
colnames(nonedges_endstate)[colnames(nonedges_endstate)=="role.x"] <- c("sender_role")
colnames(nonedges_endstate)[colnames(nonedges_endstate)=="role.y"] <- c("receiver_role")
nonedges_endstate$dyad <- substr(nonedges_endstate$sender_final_id, 2, 999) == substr(nonedges_endstate$receiver_final_id, 2, 999)


### Load some supplemental data
conditions <- read.csv(paste(data_dir, conditions_file, sep=""))
conditions <- add_edge_labels(conditions)
colnames(conditions)[3] <- "mfam_night"
mentee_master <- read.csv(paste(data_dir, mentee_master_file, sep=""))
#staff_master <- read.csv(paste(data_dir, mentor_master_file, sep=""))

### family id from supplemental data
smart_join <- function(df1, df2, splitsize=1000){
  merged_dfs <- list()
  for(i in 1:ceiling(dim(df1)[1]/splitsize)){
    merged_dfs[[i]] <- join(df1[(splitsize*(i-1)+1):min(dim(df1)[1],(splitsize*i)),], df2)
    #if(!exists("merged_df")){
    #  merged_df <- join(df1[1:splitsize,], df2)
    #} else {
    #  merged_df <- rbind(merged_df, join(df1[(splitsize*(i-1)+1):min(dim(df1)[1],(splitsize*i)),], df2))
    #}
  }
  merged_df <- do.call("rbind", merged_dfs)
  return(merged_df)
}
add_fam_id <- function(df){
  mentee_family_ids <- mentee_master[, c("final_id", "menfamid")]
  fam_labels <- levels(mentee_family_ids$menfamid)
  fam_labels <- c(fam_labels[2:length(fam_labels)], "")  # "" (empty string) label is at front of list, but NA's use the last label, so move to end
  mentee_family_ids$menfamid <- as.numeric(mentee_family_ids$menfamid)
  
  colnames(mentee_family_ids) <- c("sender_final_id", "sender_mentee_fam_id")
  df <- smart_join(df, mentee_family_ids)
  colnames(mentee_family_ids) <- c("receiver_final_id", "receiver_mentee_fam_id")
  df <- smart_join(df, mentee_family_ids)
  
  mentor_family_ids <- data.frame("receiver_final_id"=sapply(mentee_family_ids[,"receiver_final_id"], FUN=function(x) paste("m",substr(x, 2, 100000),sep="")), "receiver_mentor_fam_id"=as.numeric(mentee_family_ids[,"receiver_mentee_fam_id"]))
  df <- smart_join(df, mentor_family_ids)
  colnames(mentor_family_ids) <- c("sender_final_id", "sender_mentor_fam_id")
  df <- smart_join(df, mentor_family_ids)
  
  df$sender_fam_id <- as.character(factor(rowMeans(df[, c("sender_mentor_fam_id", "sender_mentee_fam_id")], na.rm=TRUE), labels=fam_labels))
  df$receiver_fam_id <- as.character(factor(rowMeans(df[, c("receiver_mentor_fam_id", "receiver_mentee_fam_id")], na.rm=TRUE), labels=fam_labels))
  df[, c("sender_mentor_fam_id", "sender_mentee_fam_id", "receiver_mentor_fam_id", "receiver_mentee_fam_id")] <- list(NULL)
  return(df)
}

# gender information
add_gender <- function(df){
  staff_genders <- staff[, c("final_id", "gender")]
  colnames(staff_genders) <- c("sender_final_id", "sender_gender_mentor")
  df <- join(df, staff_genders)
  colnames(staff_genders) <- c("receiver_final_id", "receiver_gender_mentor")
  df <- join(df, staff_genders)
  
  mentee_genders <- mentees[, c("final_id", "gender")]
  colnames(mentee_genders) <- c("sender_final_id", "sender_gender_mentee")
  df <- join(df, mentee_genders)
  colnames(mentee_genders) <- c("receiver_final_id", "receiver_gender_mentee")
  df <- join(df, mentee_genders)
  
  df[, "sender_gender"] <- apply(df[, c("sender_gender_mentor", "sender_gender_mentee")], 1, function(x) x[!is.na(x)])
  df[, "receiver_gender"] <- apply(df[, c("receiver_gender_mentor", "receiver_gender_mentee")], 1, function(x) x[!is.na(x)])
  df <- df[, !(names(df) %in% c("sender_gender_mentor", "sender_gender_mentee", "receiver_gender_mentor", "receiver_gender_mentee"))]
  return(df)
}


### remove_things that pollute the environment
rm(edges_45)
#rm(count_45)
rm(relation_present)
rm(p45_sem_night)
rm(senders)
rm(receivers)
rm(present_endstate)
rm(participants_not_in_endstate_network)

# add gender information to end_state edge lists
edges_endstate <- add_gender(edges_endstate)
nonedges_endstate <- add_gender(nonedges_endstate)

# Mentor family information
edges_endstate <- add_fam_id(edges_endstate)
nonedges_endstate <- add_fam_id(nonedges_endstate)

# conditions
edges_endstate <- join(edges_endstate, conditions)
nonedges_endstate <- join(nonedges_endstate, conditions)

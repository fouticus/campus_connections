source("config.R")
source("functions.R")


scale_factor = 3
semester <- "Fa15";
nights <- c("Mon", "Tue", "Wed", "Thu")
strengths <- 0:10;
survey_nos <- 1:5 ;

###### end state (5th survey) ######
# all edges
survey_no <- 5 ;
filename = paste(paste(paste(output_dir,semester, sep=""),"netplot","allnights", "allstrengths", paste("survey", toString(survey_no), sep=""), sep="_"), ".pdf", sep="")
pdf(filename, height=length(nights)*scale_factor, width=length(strengths)*scale_factor)
om = scale_factor * 2
par(mfrow=c(length(nights), length(strengths)), oma=c(om, om, om, om), mar=c(0, 0, 0, 0))
for(night in nights){
  for(strength in strengths){
    net <- make_graph(participants, edges, semester, night, strength, survey_no, strength_mode="within", strength_err=0.5)
    plot_graph(net)
  }
}
add_mtext("Strength", strengths, "Night", nights, scale_factor)
dev.off()

# only edges for a particular role
for(role in c("mentor", "mentee")){
  filename = paste(paste(paste(output_dir,semester, sep=""),"netplot","allnights", "allstrengths", paste("survey", toString(survey_no), sep=""), role, sep="_"), ".pdf", sep="")
  pdf(filename, height=length(nights)*scale_factor, width=length(strengths)*scale_factor)
  om = scale_factor * 2
  par(mfrow=c(length(nights), length(strengths)), oma=c(om, om, om, om), mar=c(0, 0, 0, 0))
  for(night in nights){
    for(strength in strengths){
      net <- make_graph(participants, edges, semester, night, strength, survey_no, sender_role=role, strength_mode="within", strength_err=0.5)
      plot_graph(net)
    }
  }
  add_mtext("Strength", strengths, "Night", nights, scale_factor)
  dev.off()
}


### Each night, all weeks ###
# all weeks, per night
for(night in nights){
  filename=paste(paste(paste(output_dir,semester, sep=""),"netplot",night,"allstrengths", "allsurveys", sep="_"), ".pdf", sep="")
  pdf(filename, width=length(strengths)*scale_factor, height=length(survey_nos)*scale_factor)
  par(mfrow=c(length(survey_nos), length(strengths)), mar=c(0, 0, 0, 0), oma=c(om, om, om, om))
  for(survey_no in survey_nos){
    for(strength in strengths){
      net <- make_graph(participants, edges, semester, night, strength, survey_no, strength_mode="within", strength_err=0.5)
      plot_graph(net)
    }
  }
  add_mtext("Strength", strengths, "Survey Number", survey_nos, scale_factor)
  dev.off()
}

# only edges for a particular role
for(role in c("mentor", "mentee")){
  for(night in nights){
    filename=paste(paste(paste(output_dir,semester, sep=""),"netplot", night,"allstrengths", "allsurveys", role, sep="_"), ".pdf", sep="")
    pdf(filename, width=length(strengths)*scale_factor, height=length(survey_nos)*scale_factor)
    par(mfrow=c(length(survey_nos), length(strengths)), mar=c(0, 0, 0, 0), oma=c(om, om, om, om))
    for(survey_no in survey_nos){
      for(strength in strengths){
        net <- make_graph(participants, edges, semester, night, strength, survey_no, sender_role=role, strength_mode="within", strength_err=0.5)
        plot_graph(net)
      }
    }
    add_mtext("Strength", strengths, "Survey Number", survey_nos, scale_factor)
    dev.off()
  }
}
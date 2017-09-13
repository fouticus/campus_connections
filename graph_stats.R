source("config.R")
source("functions.R")


# get statistics for various types of graphs
#semesters <- c("Fa15", "Sp16", "Fa16", "Sp17")
semesters <- c("Fa15")
nights <- c("Mon", "Tue", "Wed", "Thu")
strengths <- 0:10
survey_nos <- 1:5
max_deg <- 70
degs            <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentor_degs     <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentor_in_degs  <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentor_out_degs <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentee_degs     <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentee_in_degs  <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))
mentee_out_degs <- array(0, dim=c(length(semesters), length(nights), length(strengths), length(survey_nos), max_deg))

concat_degree <- function(raw_deg, fit_shape){
  if(length(raw_deg) > fit_shape){
    ret_degs <- numeric(fit_shape)
    ret_degs[0:(fit_shape-1)] <- raw_deg[0:(fit_shape-1)]
    ret_degs[fit_shape] <- sum(raw_deg[fit_shape:length(raw_deg)])
    return(ret_degs)
  } else {
    return(c(raw_deg, numeric(fit_shape - length(raw_deg))))
  }
}

for(i in 1:length(semesters)){
  for(j in 1:length(nights)){
    for(k in 1:length(strengths)){
      for(l in 1:length(survey_nos)){
        net = make_graph(participants, edges, semesters[i], nights[j], strengths[k], survey_nos[l], strength_mode="atleast")
        return_val <- graph_stats(net)
        degs[i, j, k, l, ] <- concat_degree(return_val[[1]], max_deg)
        mentor_degs[i, j, k, l, ] <- concat_degree(return_val[[2]], max_deg)
        mentor_in_degs[i, j, k, l, ] <- concat_degree(return_val[[3]], max_deg)
        mentor_out_degs[i, j, k, l, ] <- concat_degree(return_val[[4]], max_deg)
        mentee_degs[i, j, k, l, ] <- concat_degree(return_val[[5]], max_deg)
        mentee_in_degs[i, j, k, l, ] <- concat_degree(return_val[[6]], max_deg)
        mentee_out_degs[i, j, k, l, ] <- concat_degree(return_val[[7]], max_deg)
      }
    }
  }
}


# plot node degree distributions for each night
scale_factor <- 3
om <- scale_factor/2
ma <- scale_factor/1.5
plot_rows <- 2
plot_cols <- 2
colfunc <- colorRampPalette(c("black", "green", "red"))
colfunc <- function(n){return(rgb(0, 0, 0, 1:n/n))}

#day_degree_plot(degs, "deg", "Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentor_degs, "mentor_deg", "Mentor Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentor_in_degs, "mentor_in_deg", "Mentor In-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentor_out_degs, "mentor_out_deg", "Mentor Out-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentee_degs, "mentee_deg", "Mentee Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentee_in_degs, "mentee_in_deg", "Mentee In-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
#day_degree_plot(mentee_out_degs, "mentee_out_deg", "Mentee Out-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)


colrs <- c("red", "blue", "green", "orange")
# plot average node degree over time
n = ceiling(sqrt(length(strengths)))

day_ave_degree_plot(degs, "", "Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_degs, "mentor", "Mentor Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_in_degs, "mentor_in", "Mentor In-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_out_degs, "mentor_out", "Mentor Out-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_degs, "mentee", "Mentee Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_in_degs, "mentee_in", "Mentee In-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_out_degs, "mentee_out", "Mentee Out-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)

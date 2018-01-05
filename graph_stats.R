########################################################################################
## graph_stats.R
## calculate statistics for various types of graphs and make some plots
########################################################################################

# run config file
source("config.R")

# get graph functions (shared with other scripts)
source("graph_functions.R")


##########################
#### Helper Functions ####
##########################
# function to make all degree distributions the same length. 
# if raw_deg is too long, the sum of the highest degrees is put in the last element
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

role_degree_distribution <- function(net, role_a, ...){
  degs = degree(net, V(net)[V(net)$role==role_a], ...)
  tabulate(degs+1, nbins=max(degs)-min(degs)+1) # the +1 here is because tabulate starts at 1, but we want to count zero degree vertices as well
}

graph_stats <- function(net){
  degdist = degree_distribution(net, loops=FALSE, mode="total")
  mentor_degdist = role_degree_distribution(net, "mentor", loops=FALSE, mode="total")
  mentor_in_degdist = role_degree_distribution(net, "mentor", loops=FALSE, mode="in")
  mentor_out_degdist = role_degree_distribution(net, "mentor", loops=FALSE, mode="out")
  mentee_degdist = role_degree_distribution(net, "mentee", loops=FALSE, mode="total")
  mentee_in_degdist = role_degree_distribution(net, "mentee", loops=FALSE, mode="in")
  mentee_out_degdist = role_degree_distribution(net, "mentee", loops=FALSE, mode="out")
  return(list(degdist, mentor_degdist, mentor_in_degdist, mentor_out_degdist, mentee_degdist, mentee_in_degdist, mentee_out_degdist))
}
day_degree_plot <- function(degs, file_prefix, title_prefix, scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg){
  for(i in 1:length(semesters)){
    for(k in 1:length(strengths)){
      pdf(paste(paste(paste(output_dir, "deg_dists/", file_prefix, "_dist", sep="") ,semesters[i],"all-nights", toString(strengths[k]), "all-surv", sep="_"), ".pdf", sep=""), width=plot_cols*scale_factor, height=plot_rows*scale_factor)
      par(mfrow=c(plot_rows, plot_cols), mar=c(ma, ma, ma, ma), oma=c(om, om, om, om))
      ymax = max(degs[i, , k, , ])
      for(j in 1:length(nights)){
        matplot(1:max_deg, t(degs[i, j, k, , ]), main=nights[j], pch=".", type="l", lty=1, ylab=NA, xlab=NA, col=colfunc(length(survey_nos)), ylim=c(0, ymax))
      }
      title = paste(title_prefix, "Distribution for", semesters, "with a threshold of:", toString(strengths[k]), sep=" ")
      mtext(title, side=3, adj=0.5, line=0, outer=TRUE, cex=scale_factor/4)
      dev.off()
    }
  }
}
day_ave_degree_plot <- function(degs, file_suffix, title_prefix, scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg){
  for(i in 1:length(semesters)){
    if(file_suffix != ""){
      filename=paste(paste(paste(output_dir, semesters[i], sep=""), "degree_ave", "allnights", "allsurv", file_suffix, sep="_"), ".pdf", sep="")
    } else {
      filename=paste(paste(paste(output_dir, semesters[i], sep=""), "degree_ave", "allnights", "allsurv", sep="_"), ".pdf", sep="")
    }
    pdf(filename, width=plot_cols*scale_factor, height=plot_rows*scale_factor)
    strength_plots = vector(mode="list", length=length(strengths))
    for(k in 1:length(strengths)){
      df <- data.frame(1:length(survey_nos))
      colnames(df) <- c("surv")
      for(j in 1:length(nights)){
        means <- (degs[i, j, k, , ] %*% 0:(max_deg-1))[,1]/sum(degs[i, j, k, , ])
        df[nights[j]] <- means
      }
      df <- melt(df, id.vars="surv")
      p <- ggplot(data=df, aes(x=surv, y=value, col=variable)) + geom_point() + geom_line()
      p <- p + ggtitle(toString(strengths[k])) + ylab(NULL) + xlab(NULL) + theme(legend.position="none")
      strength_plots[[k]] <- p 
    }
    strength_plots[[k+1]] <- ggplot(data=df, aes(x=surv, y=value, col=variable)) + geom_point() + geom_line()
    print(grid.arrange(grobs=strength_plots))
    dev.off()
  }
}
strength_histo <- function(edges, semester_a, night_a, scale_factor){
  edges_of_interest <- with(edges, edges[semester==semester_a & night==night_a & sn1==1, ])
  edges_of_interest$survey_nice <- paste("Survey ", edges_of_interest$survnum, sep="")
  
  filen <- paste(paste(paste(output_dir, semester_a, "_strength_histo", sep="") , night_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=scale_factor*length(unique(edges_of_interest$survnum)), height=scale_factor*length(unique(edges_of_interest$sender_role)))
  p <- ggplot(edges_of_interest, aes(sn2, fill=sender_role))
  p <- p + geom_histogram(color="black")
  p <- p + scale_fill_manual(values=role_colors2)
  p <- p + facet_grid(sender_role~survey_nice)
  p <- p + labs(x="Relationship Strength", y="Frequency", fill="Sender Role")
  p <- p + theme(legend.position="bottom", panel.grid.major=element_line(linetype=2, color="grey25"), panel.border=element_rect(colour="black", fill=NA, size=1)) 
  p <- p + scale_y_continuous(breaks=seq(0,40,10), limits=c(0,50)) + scale_x_continuous(breaks=seq(0,10,2), limits=c(0,10))
  print(p)
  dev.off()
}


#########################
#### Calculate Stats ####
#########################
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


## graph stats based on edges at or above a given strength
for(i in 1:length(semesters)){
  for(j in 1:length(nights)){
    print(paste("sem:", semesters[i], "| night:", nights[j]))
    for(k in 1:length(strengths)){
      for(l in 1:length(survey_nos)){
        # make graph
        net = make_graph(participants, edges, semesters[i], nights[j], strengths[k], survey_nos[l], strength_mode="atleast")
        # compute net statistics
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

dir.create(file.path(output_dir, "/deg_dists"))
day_degree_plot(degs, "deg", "Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentor_degs, "mentor_deg", "Mentor Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentor_in_degs, "mentor_in_deg", "Mentor In-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentor_out_degs, "mentor_out_deg", "Mentor Out-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentee_degs, "mentee_deg", "Mentee Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentee_in_degs, "mentee_in_deg", "Mentee In-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)
day_degree_plot(mentee_out_degs, "mentee_out_deg", "Mentee Out-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, semesters, nights, strengths, survey_nos, max_deg)


# plot average node degree over time
colrs <- c("red", "blue", "green", "orange")
n = ceiling(sqrt(length(strengths)))

day_ave_degree_plot(degs, "", "Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_degs, "mentor", "Mentor Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_in_degs, "mentor_in", "Mentor In-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentor_out_degs, "mentor_out", "Mentor Out-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_degs, "mentee", "Mentee Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_in_degs, "mentee_in", "Mentee In-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)
day_ave_degree_plot(mentee_out_degs, "mentee_out", "Mentee Out-Degree", scale_factor, colrs, semesters, nights, strengths, survey_nos, max_deg)

# strength histogram
for(i in 1:length(semesters)){
  for(j in 1:length(nights)){
    strength_histo(valid_edges, semesters[i], nights[j], scale_factor=2)
  }
}


#################
#### Cleanup ####
#################
rm(concat_degree)
rm(day_ave_degree_plot)
rm(day_degree_plot)
rm(graph_stats)
rm(make_graph)
rm(role_degree_distribution)
rm(strength_histo)
rm(colfunc)

rm(strengths)
rm(survey_nos)
rm(i)
rm(j)
rm(k)
rm(l)
rm(n)
rm(ma)
rm(max_deg)
rm(filename)
rm(degs)
rm(net)
rm(plot_cols)
rm(plot_rows)
rm(return_val)
rm(om)
rm(colrs)

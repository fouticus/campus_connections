
source("config.R")

make_graph <- function(participants, edges, semester_a, night_a, strength_a, survey_no_a, sender_role=NA, receiver_role=NA, strength_mode="exact"){
  # construct adjacency and vertices
  verts <- participants[with(participants, semester==semester_a & night==night_a), ]
  #verts <- participants[with(participants, semester==semester_a), ]
  #verts <- subset(verts, !duplicated(verts$final_id))
  #verts <- unique(verts, incomparables=colnames(verts)[2:length(colnames)])
  
  adj <- edges[with(edges, semester==semester_a & survnum==survey_no_a & night==night_a & sender_missing==0 & receiver_missing==0 & sn1==1 & !is.na(sn1) & !is.na(sn2)), ]
  # what strength edges to include?
  if(strength_mode=="atleast"){
    adj <- adj[with(adj, sn2>=strength_a), ]
  }
  else if(strength_mode=="atmost"){
    adj <- adj[with(adj, sn2<=strength_a), ]
  }
  else if(strength_mode=="exact"){
    adj <- adj[with(adj, sn2==strength_a), ]
  } else {
    stop(paste("Invalid strength_mode:", strength_mode))
  }
  # select only specific edges based on role?
  if(!is.na(sender_role) || !is.na(sender_role)){
    tmp_df <- subset(verts, select=c("final_id","role"))
  }
  if(!is.na(sender_role)){
    colnames(tmp_df)[1] <- "sender_final_id"
    tmp_adj <- join(adj, tmp_df)
    adj <- tmp_adj[tmp_adj$role==sender_role,]
  }
  if(!is.na(receiver_role)){
    colnames(tmp_df)[1] <- "receiver_final_id"
    tmp_adj <- join(adj, tmp_df)
    adj <- tmp_adj[tmp_adj$role==receiver_role,]
  }
  # reorder columns to work with igraph
  adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]
  
  # check that all necessry vertices exist
  v_ids <- unique(verts$final_id)
  es_ids <- unique(adj$sender_final_id)
  er_ids <- unique(adj$receiver_final_id)
  if(!all(es_ids %in% v_ids)){
    print(paste("Sender IDs not in vertices:", toString(subset(es_ids, !(es_ids %in% v_ids)))))
  }
  if(!all(er_ids %in% v_ids)){
    print(paste("Receiver IDs not in vertices:", toString(subset(er_ids, !(er_ids %in% v_ids)))))
  }
  
  # create network
  net <- graph_from_data_frame(adj, vertices=verts)
  net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
  return(net)
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

plot_graph <- function(net, arrow_size=0.3, vertex_size=10){
  V(net)$color <- role_pallete[V(net)$role_num]
  alph=0.5
  E(net)$color <- unname(sapply(role_pallete[tail_of(net, E(net))$role_num], function(x){x = col2rgb(x, alpha=TRUE); return(rgb(x[1]/255, x[2]/255, x[3]/255, alph))}))
 
  # plot it 
  lay = layout.circle
  plot(net, edge.arrow.size=arrow_size, vertex.size=vertex_size, vertex.label=NA, layout=lay, vertex.frame=FALSE)
}

add_mtext <- function(x_title, x_labs, y_title, y_labs, scale_factor=1.0){
  mtext(x_title, side=3, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
  for(i in 1:length(x_labs)){
    mtext(x_labs[i], side=3, adj=i/length(x_labs) - 1/(2*length(x_labs)), outer=TRUE, cex=scale_factor/1.5)
  }
  mtext(y_title, side=2, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
  for(i in 1:length(y_labs)){
    mtext(y_labs[length(y_labs)-i+1], side=2, adj=i/length(y_labs) - 1/(2*length(y_labs)), outer=TRUE, cex=scale_factor/1.5)
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
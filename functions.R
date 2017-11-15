source("config.R")

make_graph <- function(participants, edges, semester_a, night_a, strength_a, survey_no_a=NA, sender_role=NA, receiver_role=NA, strength_mode="within", strength_err=0.5){
  # construct adjacency and vertices
  verts <- participants[which(with(participants, semester==semester_a & night==night_a)), ]
  #verts <- participants[with(participants, semester==semester_a), ]
  #verts <- subset(verts, !duplicated(verts$final_id))
  #verts <- unique(verts, incomparables=colnames(verts)[2:length(colnames)])
  
  adj <- edges[which(with(edges, semester==semester_a & night==night_a & sender_missing==0 & receiver_missing==0 & sn1==1 & !is.na(sn1) & !is.na(sn2))), ]
  if(!is.na(survey_no_a)){
    adj <- adj[which(with(adj, survnum==survey_no_a )), ]
  }
  # what strength edges to include?
  if(strength_mode=="atleast"){
    adj <- adj[with(adj, sn2>=strength_a), ]
  } else if(strength_mode=="atmost"){
    adj <- adj[with(adj, sn2<=strength_a), ]
  } else if(strength_mode=="exact"){
    adj <- adj[with(adj, sn2==strength_a), ]
  } else if(strength_mode=="within"){
    adj <- adj[with(adj, sn2<strength_a + strength_err & sn2 >= strength_a - strength_err), ]
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
  edge_columns <- c("sender_final_id", "receiver_final_id")
  other_columns <- colnames(adj)[!colnames(adj) %in% edge_columns]
  adj <- adj[, c(edge_columns, other_columns)]
  
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


plot_outedges_byperson <- function(edges, semester_a, night_a, role_a, scale_factor=3, point_size=0.5, alpha_a=0.5){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & sender_role==role_a),]
  # order them so lines plot correctly
  #edges_subset <- edges_subset[with(edges_subset, order(sender_final_id, !dyad, receiver_final_id, survnum)),]
  edges_subset <- edges_subset[with(edges_subset, order(sender_final_id, receiver_final_id, survnum)),]
  # setup plot
  grid_size <- as.integer(sqrt(length(unique(edges_subset$sender_final_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_outedges_byperson", sep="") , night_a, role_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=grid_size*scale_factor, height=grid_size*scale_factor)
  # plot
  #dyad_edges = subset(edges_subset, dyad==TRUE)
  #non_dyad_edges = subset(edges_subset, dyad==FALSE)
  #pd <- position_dodge(0.1)
  p <- ggplot(edges_subset, aes(x=survnum, y=sn2, color=dyad, group=receiver_final_id))
  #p <- ggplot(edges_subset)
  #p <- p + geom_point(aes(x=survnum, y=sn2), data=non_dyad_edges, alpha=alpha_a, size=point_size, position=pd)
  #p <- p + geom_line(aes(x=survnum, y=sn2, group=pair_id), data=non_dyad_edges, alpha=alpha_a, position=pd)
  #p <- p + geom_point(aes(x=survnum, y=sn2), data=dyad_edges, alpha=alpha_a, size=point_size, position=pd)
  #p <- p + geom_line(data=dyad_edges, aes(x=survnum, y=sn2, group=pair_id), alpha=alpha_a, position=pd)
  p <- p + geom_point(alpha=alpha_a, size=point_size)
  p <- p + geom_line(alpha=alpha_a)
  p <- p + theme(legend.position="none", panel.grid.major.y=element_line(linetype=2, color="grey50")) + dyad_color + xlab("Suvey Number") + ylab("Relationship Strength")
  p <- p + scale_y_continuous(breaks=seq(0,10,2)) + scale_x_continuous(breaks=seq(0,5,1))
  p <- p + facet_wrap(~sender_final_id)
  p <- p + scale_alpha(guide="none")    
  print(p)
  # close the pdf
  dev.off()
}

plot_outedges_dyadonly <- function(edges, semester_a, night_a, role_a, scale_factor=3, point_size=1, alpha_a=0.5){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & sender_role==role_a & dyad==TRUE),]
  # order them so lines plot correctly
  edges_subset <- edges_subset[with(edges_subset, order(semester, night, sender_final_id, receiver_final_id, survnum)),]
  # setup plot
  grid_size <- as.integer(sqrt(length(unique(edges_subset$sender_final_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_outedges_dyadonly", sep="") , night_a, role_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=grid_size*scale_factor, height=grid_size*scale_factor)
  # plot
  p <- ggplot(edges_subset, aes(x=survnum, y=sn2, color=dyad))
  p <- p + geom_point(alpha=alpha_a) + geom_line(aes(group=edges_subset$pair_id), alpha=alpha_a)
  p <- p + theme(legend.position="none", panel.grid.major.y=element_line(linetype=2, color="grey50")) + dyad_color + xlab("Suvey Number") + ylab("Relationship Strength")
  p <- p + scale_y_continuous(breaks=seq(0,10,2)) + scale_x_continuous(breaks=seq(0,5,1))
  p <- p + scale_alpha(guide="none")    
  print(p)
  # close the pdf
  dev.off()
}

dyad_trajectory <- function(edges, semester_a, night_a, scale_factor=3, point_size=1){
  # get subset of edges
  edges_subset <- edges[with(edges, semester==semester_a & night==night_a & dyad==TRUE),]
  # setup plot
  edges_subset$dyad_id <- substr(edges_subset$sender_final_id, 2, 999)
  # take only the columns we need
  edges_subset <- edges_subset[, c("dyad_id", "sender_role", "survnum", "sn2")]
  # reshape
  edges_subset = dcast(edges_subset, dyad_id + survnum ~ sender_role, value.var="sn2", fun.aggregate=sum)
  # order them so lines plot correctly
  edges_subset <- edges_subset[with(edges_subset, order(dyad_id, survnum)),]
 
  # plot each dyad separately 
  grid_size <- as.integer(sqrt(length(unique(edges_subset$dyad_id))))+1
  filen <- paste(paste(paste(output_dir, semester_a, "_dyad_trajectory", sep="") , night_a, sep="_"), ".pdf", sep="")
  pdf(filen, width=scale_factor*5, height=scale_factor*5)
  # plot
  colormap = palette(viridis(5, option="viridis"))
  p <- ggplot(edges_subset, aes(x=mentee, y=mentor, color=as.factor(survnum)))
  p <- p + geom_point() + geom_path(aes(group=edges_subset$dyad_id))
  p <- p + theme(legend.position="bottom", panel.grid.major=element_line(linetype=2, color="grey25"), panel.border=element_rect(colour="black", fill=NA, size=1)) 
  p <- p + labs(x="Mentee Strength", y="Mentor Strength", color="Survey Number")
  p <- p + scale_y_continuous(breaks=seq(0,10,5), limits=c(0,10)) + scale_x_continuous(breaks=seq(0,10,5), limits=c(0,10))
  p <- p + scale_color_manual(values=colormap)
  p <- p + facet_wrap(~edges_subset$dyad_id)
  print(p)
  dev.off()
 
  # plot average behavior instead
  averages <- aggregate(edges_subset[,c("survnum", "mentor", "mentee")], by=list(edges_subset$survnum), FUN="mean")
  filen <- paste(paste(paste(output_dir, semester_a, "_dyad_trajectory", sep="") , night_a, sep="_"), "_average.pdf", sep="")
  pdf(filen, width=scale_factor*3, height=scale_factor*3)
  # plot
  p <- ggplot(averages, aes(x=mentee, y=mentor, color=as.factor(survnum)))
  p <- p + geom_point() + geom_path(color=as.factor(averages$survnum))
  p <- p + theme(legend.position="bottom", panel.grid.major=element_line(linetype=2, color="grey25"), panel.border=element_rect(colour="black", fill=NA, size=1)) 
  p <- p + labs(x="Mentee Strength", y="Mentor Strength", color="Survey Number")
  p <- p + scale_y_continuous(breaks=seq(0,10,2), limits=c(0,10)) + scale_x_continuous(breaks=seq(0,10,2), limits=c(0,10))
  #p <- p + ylim(0, 10) + xlim(0, 10)
  p <- p + scale_color_manual(values=colormap)
  print(p)
  dev.off()
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

betweenness_centrality <- function(adj){
  # algorithm implemented from:
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.2024&rep=rep1&type=pdf
  # create neighbor lists
  nbors <- alply(split(adj, seq(NROW(adj))), 1, function(x){which(x!=0)})
  n <- dim(adj)[1]
  CB <- vector(length=n)
  for(v in 1:n){
    CB[v] <- 0
  }
  for(s in 1:n){
    S <- rstack()
    Pi <- rep(1, n)
    P <- list()
    for(t in 1:n){
      P[[t]] <- list()
    }
    sigm <- rep(0, n)
    sigm[s] <- 1
    d <- rep(-1, n)
    d[s] <- 0
    #Q <- rpqueue()
    Q <- rdeque()
    Q <- insert_back(Q, s)
    while(!rstackdeque::empty(Q)){
      v <- peek_front(Q)
      Q <- without_front(Q)
      S <- insert_top(S, v)
      for(w in nbors[[v]]){
        if(d[w] < 0){
          Q <- insert_back(Q, w)
          d[w] <- d[v] + 1
        }
        if(d[w] == d[v] + 1){
          sigm[w] <- sigm[w] + sigm[v]
          P[[w]][[Pi[w]]] <- v
          Pi[w] = Pi[w] + 1
        }
      }
    }
    delt <- rep(0 ,n)
    while(!rstackdeque::empty(S)){
      w <- peek_top(S)
      S <- without_top(S)
      for(v in P[[w]]){
        delt[v] <- delt[v] + sigm[v]/sigm[w] * (1+delt[w])
      }
      if(w != s){
        CB[w] <- CB[w] + delt[w]
      }
    }
  }
  return(CB)
}

adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow=4)
adj <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0), nrow=5)
c <- r <- 10
adj <- round(matrix(runif(r*c), r, c))

print(betweenness_centrality(adj))

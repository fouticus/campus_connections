library('igraph')

# load data
source("load_and_clean2.R")
source("colors_and_styles.R")

plot_graph_upto <- function(sem, nigt, thresh, surf, arrow_size=0.3, vertex_size=10){
  # construct adjacency and vertices
  adj <- edges[with(edges, semester==sem & survnum==surv & night==nigt & sender_missing==0 & receiver_missing==0 & sn1==1 & sn2>=thresh & !is.na(sn1) & !is.na(sn2)), ]
  verts <- participants[with(participants, semester==sem & night==nigt), ]
  
  # reorder columns to work with igraph
  adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]
  
  # create network
  net <- graph_from_data_frame(adj, vertices=verts)
  net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
  V(net)$color <- role_pallete[V(net)$role_num]
 
  # plot it 
  #lay = layout.fruchterman.reingold
  lay = layout.auto
  lay = layout.circle
  plot(net, edge.arrow.size=arrow_size, edge.color="black", vertex.size=10, vertex.label=NA, layout=lay)
}
plot_graph_at <- function(sem, nigt, stren, surf, arrow_size=0.3, vertex_size=9){
  # construct adjacency and vertices
  adj <- edges[with(edges, semester==sem & survnum==surv & night==nigt & sender_missing==0 & receiver_missing==0 & sn1==1 & sn2==thresh & !is.na(sn1) & !is.na(sn2)), ]
  verts <- participants[with(participants, semester==sem & night==nigt), ]
  
  # reorder columns to work with igraph
  adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]
  
  # create network
  net <- graph_from_data_frame(adj, vertices=verts)
  net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
  V(net)$color <- role_pallete[V(net)$role_num]
  alph=0.5
  E(net)$color <- unname(sapply(role_pallete[tail_of(net, E(net))$role_num], function(x){x = col2rgb(x, alpha=TRUE); return(rgb(x[1]/255, x[2]/255, x[3]/255, alph))}))
  #Ecolors <- role_pallete[tail_of(net, E(net))$role_num]
 
  # plot it 
  #lay = layout.fruchterman.reingold
  lay = layout.auto
  lay = layout.circle
  #plot(net, edge.arrow.size=arrow_size, vertex.size=vertex_size, vertex.label=NA, layout=lay, edge.curved=seq(-0.5, 0.5, length = ecount(net)))
  plot(net, edge.arrow.size=arrow_size, vertex.size=vertex_size, vertex.label=NA, layout=lay, vertex.frame=FALSE)
}



#### End of functions ###

scale_factor = 3
om = scale_factor * 2
# visualize survey at different thresholds
sem <- "Fa15";
nigts <- c("Mon", "Tue", "Wed", "Thu")
threshes <- 1:10;
survs <- 1:5 ;

# end state (5th survey)
surv <- 5 ;
pdf(paste(paste("../output/netplot",sem,"all-nights", "all-thresh", toString(surv), sep="_"), ".pdf", sep=""), width=length(threshes)*scale_factor, height=length(nigts)*scale_factor)
par(mfrow=c(4, 10), oma=c(om, om, om, om), mar=c(0, 0, 0, 0))
for(nigt in nigts){
  for(thresh in threshes){
    plot_graph_at(sem, nigt, thresh, surv)
  }
}
# add column and row labels
mtext("Night", side=2, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
for(i in 1:length(nigts)){
  mtext(nigts[length(nigts)-i+1], side=2, adj=i/length(nigts) - 1/(2*length(nigts)), outer=TRUE, cex=scale_factor/1.5)
}
mtext("Strength", side=3, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
for(i in 1:length(threshes)){
  mtext(threshes[i], side=3, adj=i/length(threshes) - 1/(2*length(threshes)), outer=TRUE, cex=scale_factor/1.5)
}
dev.off()

# all weeks, per night
for(nigt in nigts){
  pdf(paste(paste("../output/netplot",sem,nigt,"all-thresh", "all-surv", sep="_"), ".pdf", sep=""), width=length(threshes)*scale_factor, height=length(survs)*scale_factor)
  par(mfrow=c(5, 10), mar=c(0, 0, 0, 0), oma=c(om, om, om, om))
  for(surv in survs){
    for(thresh in threshes){
      plot_graph_at(sem, nigt, thresh, surv)
    }
  }
  # add column and row labels
  for(i in 1:length(survs)){
    mtext(survs[length(survs)-i+1], side=2, adj=i/length(survs) - 1/(2*length(survs)), outer=TRUE, cex=scale_factor/1.5)
  }
  mtext("Survey Number", side=2, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
  for(i in 1:length(threshes)){
    mtext(threshes[i], side=3, adj=i/length(threshes) - 1/(2*length(threshes)), outer=TRUE, cex=scale_factor/1.5)
  }
  mtext("Strength", side=3, adj=0.5, line=3, outer=TRUE, cex=scale_factor/1.5)
  dev.off()
}
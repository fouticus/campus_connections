# make an igraph object 
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


plot_graph <- function(net, arrow_size=0.3, vertex_size=10){
  V(net)$color <- role_colors[V(net)$role_num]
  alph=0.5
  E(net)$color <- unname(sapply(role_colors[tail_of(net, E(net))$role_num], function(x){x = col2rgb(x, alpha=TRUE); return(rgb(x[1]/255, x[2]/255, x[3]/255, alph))}))
 
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
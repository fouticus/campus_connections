library('igraph')

# load data
source("load_and_clean2.R")
source("colors_and_styles.R")

plot_graph <- function(sem, nigt, thresh, surf){
  # construct adjacency and vertices
  adj <- edges[with(edges, semester==sem & survnum==surv & night==nigt & sender_missing==0 & receiver_missing==0 & sn1==1 & sn2>=thresh & !is.na(sn1) & !is.na(sn2)), ]
  verts <- participants[with(participants, semester==sem & night==nigt), ]
  
  # reorder columns to work with igraph
  adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]
  
  # create network
  net <- graph_from_data_frame(adj, vertices=verts)
  net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
  V(net)$color <- mentor_role_colors[V(net)$role_num]
 
  # plot it 
  #lay = layout.fruchterman.reingold
  lay = layout.auto
  lay = layout.circle
  plot(net, edge.arrow.size=0.1, edge.color="black", vertex.size=5, vertex.label=NA, layout=lay)
}
graph_stats <- function(sem, nigt, thresh, surv){
  # construct adjacency and vertices
  adj <- edges[with(edges, semester==sem & survnum==surv & night==nigt & sender_missing==0 & receiver_missing==0 & sn1==1 & sn2>=thresh & !is.na(sn1) & !is.na(sn2)), ]
  verts <- participants[with(participants, semester==sem & night==nigt), ]
  
  # reorder columns to work with igraph
  adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]
  
  # create network
  net <- graph_from_data_frame(adj, vertices=verts)
  net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
  
  degdist_in = degree_distribution(net, loops=FALSE, mode="in")
  degdist_out = degree_distribution(net, loops=FALSE, mode="out")
  degdist = degree_distribution(net, loops=FALSE, mode="out")
  return(list(degdist, degdist_in, degdist_out))
}
  


# visualize survey at different thresholds
sem <- "Fa15";
nigts <- c("Mon", "Tue", "Wed", "Thu")
threshes <- 1:10;
surv <- 5 ;

par(mfrow=c(4, 10), mar=c(0, 0, 0, 0))
for(nigt in nigts){
  for(thresh in threshes){
    plot_graph(sem, nigt, thresh, surv)
  }
}

sem <- "Fa16";
nigt <- "Mon"
threshes <- 1:10;
survs <- 1:5 ;
par(mfrow=c(5, 10), mar=c(0, 0, 0, 0))
for(surv in survs){
  for(thresh in threshes){
    plot_graph(sem, nigt, thresh, surv)
  }
}

# get statistics for various types of graphs
#sems <- c("Fa15", "Fa16", "Sp16")
sems <- c("Fa15")
nigts <- c("Mon", "Tue", "Wed", "Thu")
threshes <- 1:3
survs <- 1:5

n_cases = length(sems)*length(nigts)*length(threshes)*length(survs)
degs = numeric(n_cases)
in_degs = numeric(n_cases)
out_degs = numeric(n_cases)
i=1
for(sem in sems){
  for(nigt in nigts){
    j <- 0
    for(thresh in threshes){
      for(surv in survs){
        return_val <- graph_stats(sem, nigt, thresh, surv)
        print(length(return_val))
        degs[i] <- return_val[1]
        in_degs[i] <- return_val[2]
        out_degs[i] <- return_val[3]
        i <- i + 1       
        j <- j + 1
      }
    }
  }
}
stats = data.frame(degs, in_degs, out_degs, stringsAsFactors=FALSE)

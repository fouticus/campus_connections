library('igraph')

# load data
source("load_and_clean.R")

colnames(edges)
sem <- "F15"; thresh <- 7; surv <-5 ; nigt = "monday";
adj <- edges[with(edges, semester==sem & survnum==surv & night==nigt & Sender_missing==0 & Receiver_Missing==0 & sn1==1 & sn2>=thresh & !is.na(sn1) & !is.na(sn2)), ]

# reorder columns to work with igraph
adj <- adj[c(2, 5, 1, 3:4, 6:ncol(adj))]

net <- graph_from_data_frame(adj) 
net <- simplify(net, remove.multiple=TRUE, remove.loops=TRUE)
plot(net, edge.arrow.size=0.2, edge.color="black", vertex.size=5, vertex.label=NA, layout=layout.fruchterman.reingold)

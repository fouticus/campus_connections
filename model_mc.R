set.seed(983182)


setRefClass(
  "GraphModel"
)
SBM <- R6Class(
  "StochBlockModel",
  public = list(
    groups=2,
    probs=matrix(c(1.0, 0.5, 0.5, 1.0), ncol=2),
    initialize = function(groups, probs){
      self$groups <- groups
      self$probs <- probs
    },
    realize = function(){
      n <- sum(self$groups)
      adj <- matrix(nrow=n, ncol=n)
      # simulate all edges between types
      idx1 <- 1
      for(i in 1:length(self$groups)){
        n_g1 <- self$groups[i]
        idx2 <- 1
        for(j in 1:length(self$groups)){
          n_g2 <- self$groups[j]
          adj[idx1:(idx1+n_g1-1),idx2:(idx2+n_g2-1)] <- matrix(runif(n_g1*n_g2), nrow=n_g1, ncol=n_g2) < self$probs[i, j]
          if(!i==j){
            adj[idx2:(idx2+n_g2-1),idx1:(idx1+n_g1-1)] <- matrix(runif(n_g2*n_g1), nrow=n_g2, ncol=n_g1) < self$probs[j, i]
          }
          idx2 <- idx2 + n_g2
        }
        idx1 <- idx1 + n_g1
      }
      return(adj)
    }
  )
)



network_stats <- function(adj, groups=NA){
  # compute different statistics about a model
  n_stats <- new.env()
  n_stats$out_degrees <- rowSums(adj)
  n_stats$in_degrees <- colSums(adj)
  n_stats$ave_out_degree <- mean(n_stats$out_degrees)
  n_stats$ave_in_degree <- mean(n_stats$in_degrees)
  if(!is.na(groups)){
    group_ave_out_degree <- vector(length=length(groups))
    group_ave_in_degree <- vector(length=length(groups))
    idx <- 1
    for(i in 1:length(groups)){
      n_g <- groups[i]
      group_ave_out_degree[i] <- drop(rowSums(adj[idx:(idx+n_g-1),]))
      group_ave_in_degree[i] <- drop(colSums(adj[,idx:(idx+n_g-1)]))
      idx <- idx + n_g
    }
    n_stats$group_ave_out_degree <- group_ave_out_degree
    n_stats$group_ave_in_degree <- group_ave_in_degree
  }
  return(n_stats)
}

#specify a graph model
groups <- c(10, 10)
probs <- matrix(c(1.0, 0.0, 0.0, 0.0), ncol=2, byrow=TRUE)
m <- SBM$new(groups=groups, probs=probs)

ave_out_degs <- vector(length=N)
ave_in_degs <- vector(length=N)
group_ave_in_degs <- matrix(ncol=2, nrow=N)
group_ave_out_degs <- matrix(ncol=2, nrow=N)
for(i in 1:N){
  # make a realization of this graph model
  n_stats <- network_stats(m$realize(), groups)
  ave_out_degs[i] <- n_stats$ave_out_degree
  ave_in_degs[i] <- n_stats$ave_in_degree
  group_ave_out_degs[i,] <- n_stats$group_ave_out_degree
  group_ave_in_degs[i,] <- n_stats$group_ave_in_degree
}

hist(group_ave_out_degs[,1])
hist(group_ave_in_degs[,1])

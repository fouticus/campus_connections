set.seed(983182)


N = 1000

setRefClass(
  "GraphModel"
)
SBM <- R6Class(
  "StochBlockModel",
  public = list(
    groups = c(5, 5),
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

GDI <- R6Class(
  "GeneralizedDyadIndependent",
  public = list(
    groups = c(1, 1),
    probs=matrix(c(1.0, 0.5, 0.5, 1.0), ncol=2),
    initialize = function(groups, probs){
      self$groups <- groups
      self$probs <- probs
    },
    realize = function(){
      n <- dim(self$probs)[1]
      adj <- matrix(nrow=n, ncol=n)
      # simulate all edges between types
      adj <- matrix(runif(n*n), nrow=n, ncol=n) < self$probs
      return(adj)
    }
  )
)

network_stats <- function(adj, groups=NULL){
  # compute different statistics about a model
  n_stats <- new.env()
  n_stats$out_degrees <- rowSums(adj)
  n_stats$in_degrees <- colSums(adj)
  n_stats$ave_out_degree <- mean(n_stats$out_degrees)
  n_stats$ave_in_degree <- mean(n_stats$in_degrees)
  if(!is.null(groups)){
    group_ave_out_degree <- vector(length=length(groups))
    group_ave_in_degree <- vector(length=length(groups))
    idx <- 1
    for(i in 1:length(groups)){
      n_g <- groups[i]
      group_ave_out_degree[i] <- mean(drop(rowSums(adj[idx:(idx+n_g-1),])))
      group_ave_in_degree[i] <- mean(drop(colSums(adj[,idx:(idx+n_g-1)])))
      idx <- idx + n_g
    }
    n_stats$group_ave_out_degree <- group_ave_out_degree
    n_stats$group_ave_in_degree <- group_ave_in_degree
  }
  n_stats$CB <- betweenness_centrality(adj)
  return(n_stats)
}

mc_sim <- function(net_model, N, ...){
  t_start = Sys.time()
  retenv <- new.env()
  retenv$ave_out_degs <- vector(length=N)
  retenv$ave_in_degs <- vector(length=N)
  retenv$group_ave_in_degs <- matrix(ncol=2, nrow=N)
  retenv$group_ave_out_degs <- matrix(ncol=2, nrow=N)
  retenv$ave_CB <- vector(length=N)
  cat("Progress:")
  for(i in 1:N){
    # make a realization of this graph model
    n_stats <- network_stats(net_model$realize(), ...)
    retenv$ave_out_degs[i] <- n_stats$ave_out_degree
    retenv$ave_in_degs[i] <- n_stats$ave_in_degree
    retenv$group_ave_out_degs[i,] <- n_stats$group_ave_out_degree
    retenv$group_ave_in_degs[i,] <- n_stats$group_ave_in_degree
    retenv$ave_CB[i] <- mean(n_stats$CB)
    cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
    cat("Progress: ", round(i/N*100), "%, Elapsed:", round(Sys.time()-t_start, 1), " seconds", sep="")
  }
  cat(", Done\n")
  return(retenv)
}



#specify some models
n_groups <- 2
groups <- c(20, 20)
probs <- matrix(c(0.8, 0.2, 0.0, 0.0), ncol=2, byrow=TRUE)
m <- SBM$new(groups=groups, probs=probs)

probs <- matrix(runif(1600), ncol=40, byrow=TRUE)
m2 <- GDI$new(groups=groups, probs=probs)

N <- 10
results <- mc_sim(m2, N, groups)

hist(group_ave_out_degs[,1], breaks=50)
hist(group_ave_in_degs[,1], breaks=50)
hist(ave_CB, breaks=50)

set.seed(983182)

source("functions.R")


N = 1000

### Some classes defining different types of graph models
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

##### some functions #####
# compute different statistics about a model
network_stats <- function(adj, groups=NULL){
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
  n_stats$ave_CB <- mean(n_stats$CB)
  return(n_stats)
}

# run a simulation of models
mc_sim <- function(net_model, N, par=FALSE, ...){
  run_sim <- function(i, net_m){
    # make a realization of this graph model
    n_stats <- network_stats(net_m$realize(), net_m$groups)
    env <- new.env()
    env$ave_out_degs <- n_stats$ave_out_degree
    env$ave_in_degs <- n_stats$ave_in_degree
    env$group_ave_out_degs <- n_stats$group_ave_out_degree
    env$group_ave_in_degs <- n_stats$group_ave_in_degree
    env$ave_CB <- n_stats$ave_CB
    return(env)
  }
  if(par){
    envs <- mclapply(1:N, run_sim, net_model, mc.cores=3)
    #envs <- mclapply2(1:N, run_sim, net_model, mc.cores=3)
  } else {
    envs <- lapply(1:N, run_sim, net_model)
  }
  cat("Done\n")
  # combine all results
  retenv <- new.env()
  retenv$ave_out_degs <- vector(length=N)
  retenv$ave_in_degs <- vector(length=N)
  retenv$group_ave_in_degs <- matrix(ncol=length(groups), nrow=N)
  retenv$group_ave_out_degs <- matrix(ncol=length(groups), nrow=N)
  retenv$ave_CB <- vector(length=N)
  for(i in 1:length(envs)){
    env <- envs[[i]]
    retenv$ave_out_degs[i] <- env$ave_out_degs
    retenv$ave_in_degs[i] <- env$ave_in_degs
    retenv$group_ave_in_degs[i,] <- env$group_ave_in_degs
    retenv$group_ave_out_degs[i,] <- env$group_ave_out_degs
    retenv$ave_CB[i] <- env$ave_CB
  }
  return(retenv)
}

plot_results <- function(sim_results, true_stats, bins, group_labels=NULL){
  plot_hist <- function(dat, vert, xlabel, main=NULL){
    xmin <- min(vert, dat)
    xmax <- max(vert, dat)
    xmin <- xmin-0.1*(xmax-xmin)
    xmax <- xmax+0.1*(xmax-xmin)
    hist(dat, breaks=bins, xlim=c(xmin, xmax), xlab=xlabel, main=main)
    abline(v=vert, col="red", lty=2)
  }
  panel_hist <- function(dats, verts, xlabel, mains=NULL, main=NULL){
    ng <- length(verts)
    if(is.null(mains)){
      mains <- sprintf("Group %d", 1:ng)
    }
    par(mfrow=c(round(sqrt(ng))+1, round(sqrt(ng))+1), oma=c(0, 0, 2, 0))
    for(i in 1:ng){
      plot_hist(dats[,i], verts[i], xlabel, mains[i])
    }
    title(main, outer=TRUE)
    par(mfrow=c(1,1), oma=c(0,0,0,0))
  }
  # plot in and out degrees
  plot_hist(sim_results$ave_in_degs, true_stats$ave_in_degree, "", "Average In Degree")
  plot_hist(sim_results$ave_out_degs, true_stats$ave_out_degree, "", "Average Out Degree")
  panel_hist(sim_results$group_ave_in_degs, true_stats$group_ave_in_degree, "", group_labels, "Average In Degree")
  panel_hist(sim_results$group_ave_out_degs, true_stats$group_ave_out_degree, "", group_labels, "Average Out Degree")
  # plot betweenness centrality
  plot_hist(sim_results$ave_CB, true_stats$ave_CB, "", "Average Betweenness Centrality")
}

##### end functions #####

# test some models
n_groups <- 2
groups <- c(20, 20)
probs <- matrix(c(0.8, 0.2, 0.0, 0.0), ncol=2, byrow=TRUE)
m <- SBM$new(groups=groups, probs=probs)

probs <- matrix(runif(1600), ncol=40, byrow=TRUE)
m2 <- GDI$new(groups=groups, probs=probs)

N <- 100
results <- mc_sim(m, N, groups)
results2 <- mc_sim(m2, N, groups)

hist(results$group_ave_out_degs[,1], breaks=N/10)
hist(results$group_ave_in_degs[,1], breaks=N/10)
hist(results$ave_CB, breaks=N/10)

# Simulate some SBM's based on the SBM LR we ran
night <- "Mon"
sem <- "Fa15"

# construct probability matrix
vertices <- present_45[present_45$semester==sem & present_45$night==night, ]
vertices <- merge(participants, vertices, by.x=c("semester", "night", "final_id"), by.y=c("semester", "night", "final_id"))
vertices <- vertices[vertices$role == "mentor" | vertices$role == "mentee",]
vertices <- vertices[order(vertices$role),]
n_verts <- nrow(vertices)
p <- matrix(rep(0, n_verts*n_verts), nrow=n_verts)
for(i in 1:n_verts){
  for(j in 1:n_verts){
    if(i==j){next}
    sender <- vertices[i,]
    receiver <- vertices[j,]
    pred_row <- data.frame("I_sender_mentee"=(sender$role=="mentee")*1, "I_receiver_mentee"=(receiver$role=="mentee")*1)
    prob <- drop(merge(m_sbm$X_pred, pred_row)[, "predicted_prob"])
    p[i, j] <- prob
  }
}
# construct adjacency matrix
vert_map <- hashmap(vertices$final_id, 1:n_verts)
edges_ <- X_sbm[X_sbm$semester==sem & X_sbm$night==night & X_sbm$edge==1, ]
senders <- edges_$sender_final_id
receivers <- edges_$receiver_final_id
A <- matrix(rep(0, n_verts*n_verts), nrow=n_verts)
for(i in 1:dim(edges)[1]){
  A[vert_map[[senders[i]]], vert_map[[receivers[i]]]] <- 1
}

N <- 300
groups <- as.data.frame((vertices %>% count(role)))[,2]
m_sbm_gen <- SBM$new(groups=groups, probs=p)
results <- mc_sim(m_sbm_gen, N, groups, par=TRUE)
bins=30
plot_results(results, network_stats(A, groups), bins, c("Mentors", "Mentees"))
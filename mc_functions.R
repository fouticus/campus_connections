########################################################################################
## mc_functions.R
## Helper functions for MC simulation
########################################################################################

# run config file
source("config.R")

# efficiently compute betweenness centrality of a graph for the given adjacency matrix
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

## Test code ##
#adj <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0), nrow=4)
#adj <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0), nrow=5)
#c <- r <- 10
#adj <- round(matrix(runif(r*c), r, c))
#print(betweenness_centrality(adj))

#-------------------------------------------------------------------------------
#' Wrapper around mclapply to track progress
#' 
#' Based on http://stackoverflow.com/questions/10984556
#' 
#' @param X         a vector (atomic or list) or an expressions vector. Other
#'                  objects (including classed objects) will be coerced by
#'                  ‘as.list’
#' @param FUN       the function to be applied to
#' @param ...       optional arguments to ‘FUN’
#' @param mc.preschedule see mclapply
#' @param mc.set.seed see mclapply
#' @param mc.silent see mclapply
#' @param mc.cores see mclapply
#' @param mc.cleanup see mclapply
#' @param mc.allow.recursive see mclapply
#' @param mc.progress track progress?
#' @param mc.style    style of progress bar (see txtProgressBar)
#'
#' @examples
#' x <- mclapply2(1:1000, function(i, y) Sys.sleep(0.01))
#' x <- mclapply2(1:3, function(i, y) Sys.sleep(1), mc.cores=1)
#' 
#' dat <- lapply(1:10, function(x) rnorm(100)) 
#' func <- function(x, arg1) mean(x)/arg1 
#' mclapply2(dat, func, arg1=10, mc.cores=2)
#-------------------------------------------------------------------------------
mclapply2 <- function(X, FUN, ..., 
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                      mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                      mc.progress=TRUE, mc.style=3) 
{
  if (!is.vector(X) || is.object(X)) X <- as.list(X)
  
  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel:::mcfork()
    pb <- txtProgressBar(0, length(X), style=mc.style)
    setTxtProgressBar(pb, 0) 
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length(X)) {
        readBin(f, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress) 
      }
      cat("\n")
      parallel:::mcexit()
    }
  }
  tryCatch({
    result <- mclapply(X, ..., function(...) {
      res <- FUN(...)
      if (mc.progress) writeBin(1, f)
      res
    }, 
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
    )
    
  }, finally = {
    if (mc.progress) close(f)
  })
  result
}

# compute the factors of the number x
get_factors <- function(x) {
  if(x==1){
    return(numeric(0))
  }
  x <- as.integer(x)
  div <- 1+seq_len(abs(x)-2)
  factors <- div[x %% div == 0L]
  return(factors)
}

#################
#### Classes ####
#################
setRefClass(
  "GraphModel"
)
# stochastic block model
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
      return(adj*1)
    }
  )
)

# general dyad independent model (any probability matrix)
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
      return(adj*1)
    }
  )
)

###########################
#### various other fns ####
###########################
# compute different statistics about a model
network_stats <- function(adj, groups=NULL){
  n <- dim(adj)[1]
  n_stats <- new.env()
  n_stats$out_degree <- t(as.array(rowSums(adj), dim=c(1, n)))
  n_stats$in_degree <- t(as.array(colSums(adj), dim=c(1, n)))
  n_stats$out_degree_ave <- mean(n_stats$out_degree)
  n_stats$in_degree_ave <- mean(n_stats$in_degree)
  n_stats$out_degree_stddev <- sqrt(var(t(n_stats$out_degree)))
  n_stats$in_degree_stddev <- sqrt(var(t(n_stats$in_degree)))
  if(!is.null(groups)){
    n_grps <- length(groups)
    group_out_degree <- list(length=n_grps)
    group_in_degree <- list(length=n_grps)
    group_out_degree_ave <- array(dim=c(1, n_grps))
    group_in_degree_ave <- array(dim=c(1, n_grps))
    group_out_degree_stddev <- array(dim=c(1, n_grps))
    group_in_degree_stddev <- array(dim=c(1, n_grps))
    idx <- 1
    for(i in 1:length(groups)){
      n_g <- groups[i]
      if(n_g==1){
        group_out_degree[[i]] <- as.array(t(sum(adj[idx:(idx+n_g-1),])))
        group_in_degree[[i]] <- as.array(t(sum(adj[,idx:(idx+n_g-1)])))
        group_out_degree_stddev[i] <- 0
        group_in_degree_stddev[i] <- 0
      } else {
        group_out_degree[[i]] <- as.array(t(rowSums(adj[idx:(idx+n_g-1),])))
        group_in_degree[[i]] <- as.array(t(colSums(adj[,idx:(idx+n_g-1)])))
        group_out_degree_stddev[i] <- sqrt(var(t(group_out_degree[[i]])))
        group_in_degree_stddev[i] <- sqrt(var(t(group_out_degree[[i]])))
      }
      group_out_degree_ave[i] <- mean(group_out_degree[[i]])
      group_in_degree_ave[i] <- mean(group_in_degree[[i]])
      idx <- idx + n_g
    }
    n_stats$group_out_degree <- group_out_degree
    n_stats$group_in_degree <- group_in_degree
    n_stats$group_out_degree_ave <- group_out_degree_ave
    n_stats$group_in_degree_ave <- group_in_degree_ave
    n_stats$group_out_degree_stddev <- group_out_degree_stddev
    n_stats$group_in_degree_stddev <- group_in_degree_stddev
  }
  n_stats$CB <- t(as.array(betweenness_centrality(adj)))
  n_stats$CB_ave <- mean(t(n_stats$CB))
  n_stats$CB_stddev <- sqrt(var(t(n_stats$CB)))
  n_stats$cycles <- list()
  n_stats$cycles_ave <- list()
  n_stats$cycles_stddev <- list()
  A <- adj
  for(i in 1:3){
    A <- A %*% adj
    n_stats$cycles[[i]] <- t(as.array(diag(A)))
    n_stats$cycles_ave[[i]] <- sum(n_stats$cycles[[i]])
    n_stats$cycles_stddev[[i]] <- sqrt(var(t(n_stats$cycles[[i]])))
  }
  return(n_stats)
}

# run a simulation of models
mc_sim <- function(net_model, N, par=FALSE, cores=3){
  run_sim <- function(i, net_m){
    return(network_stats(net_m$realize(), net_m$groups))
  }
  if(par){
    envs <- mclapply(1:N, run_sim, net_model, mc.cores=cores)
    #envs <- mclapply2(1:N, run_sim, net_model, mc.cores=3)
  } else {
    envs <- lapply(1:N, run_sim, net_model)
  }
  # combine all results
  retenv <- combine_envs(envs)
  return(retenv)
}

# combines different environments (useful for combining results from different models)
combine_envs <- function(envs, along=1, ignore=c()){
  var_names <- names(envs[[1]])
  retenv <- new.env()
  first <- T
  for(j in 1:length(envs)){
    for(i in 1:length(var_names)){
      nam <- var_names[i]
      if(nam %in% ignore){next}
      tmp <- get(nam, envs[[j]])
      if(is.atomic(tmp)){ # Not a list:
        if(first){
          assign(nam, tmp, retenv)
        } else {
          assign(nam, abind(get(nam, retenv), tmp, along=along), retenv)
        }
      } else { # A list:
        if(first){
          assign(nam, tmp, retenv)
        } else {
          tmp2 <- get(nam, retenv)
          for(idx in 1:length(tmp2)){
            tmp2[[idx]] <- abind(tmp2[[idx]], tmp[[idx]], along=along)
          }
          assign(nam, tmp2, retenv)
        }
      }
    }
    first <- F
  }
  return(retenv)
}

# Calculate variance of each row of a df
rowVar <- function(x){
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

# Calculate average and standard deviation of each row of a df
row_summary <- function(env){
  var_names <- names(env)
  retenv <- new.env()
  first <- T
  for(i in 1:length(var_names)){
    nam <- var_names[i]
    tmp <- get(nam, env)
    if(is.atomic(tmp)){ # It's not a list
      assign(paste(nam, "_ave", sep=""), rowMeans(tmp), retenv)
      assign(paste(nam, "_stddev", sep=""), sqrt(rowVar(tmp)), retenv)
    } else { # It's a list
      tmp_ave <- list()
      tmp_stddev <- list()
      for(idx in 1:length(tmp)){
        tmp_ave[[idx]] <- rowMeans(tmp[[idx]])
        tmp_stddev[[idx]] <- sqrt(rowVar(tmp[[idx]]))
      }
      assign(paste(nam, "_ave", sep=""), tmp_ave, retenv)
      assign(paste(nam, "_stddev", sep=""), tmp_stddev, retenv)
    }
  }
  return(retenv)
}

# Plot the results.
plot_results <- function(sim_results, true_stats, bins, group_labels=NULL, save=FALSE, fprefix=""){
  plot_hist <- function(dat, vert, xlabel, main=NULL, save=FALSE, fprefix="", fpostfix="hist"){
    if(save){pdf(sprintf("%s/%s/%s_%s.pdf", output_dir, "mc_models", fprefix, fpostfix))}
    xmin <- min(vert, dat)
    xmax <- max(vert, dat)
    xmin <- xmin-0.1*(xmax-xmin)
    xmax <- xmax+0.1*(xmax-xmin)
    hist(dat, breaks=bins, xlim=c(xmin, xmax), xlab=xlabel, main=main)
    abline(v=vert, col="red", lty=2)
    if(save){dev.off()}
  }
  panel_hist <- function(dats, verts, xlabel, mains=NULL, main=NULL, save=FALSE, fprefix="", fpostfix="panel_hist"){
    if(save){pdf(sprintf("%s/%s/%s_%s.pdf", output_dir, "mc_models", fprefix, fpostfix))}
    ng <- length(verts)
    if(is.null(mains)){
      mains <- sprintf("Group %d", 1:ng)
    }
    rows <- round(sqrt(ng))
    cols <- round(ng/rows)
    par(mfrow=c(cols, rows), oma=c(0, 0, 2, 0))
    for(i in 1:ng){
      if(is.atomic(dats)){
        plot_hist(dats[,i], verts[i], xlabel, mains[i], save=FALSE, fprefix=fprefix)
      } else {
        plot_hist(dats[[i]], verts[[i]], xlabel, mains[i], save=FALSE, fprefix=fprefix)
      }
    }
    title(main, outer=TRUE)
    par(mfrow=c(1,1), oma=c(0,0,0,0))
    if(save){dev.off()}
  }
  # plot ave in and out degrees
  plot_hist(sim_results$in_degree_ave, true_stats$in_degree_ave, "", "Average In Degree", save, fprefix, "in_deg_ave")
  plot_hist(sim_results$out_degree_ave, true_stats$out_degree_ave, "", "Average Out Degree", save, fprefix, "out_deg_ave")
  panel_hist(sim_results$group_in_degree_ave, true_stats$group_in_degree_ave, "", group_labels, "Average In Degree", save, fprefix, "group_in_deg_ave")
  panel_hist(sim_results$group_out_degree_ave, true_stats$group_out_degree_ave, "", group_labels, "Average Out Degree", save, fprefix, "group_out_deg_ave")
  # plot stddev in and out degrees
  plot_hist(sim_results$in_degree_stddev, true_stats$in_degree_stddev, "", "Standard Deviaion In Degree", save, fprefix, "in_deg_stddev")
  plot_hist(sim_results$out_degree_stddev, true_stats$out_degree_stddev, "", "Standard Deviaion Out Degree", save, fprefix, "out_deg_stddev")
  panel_hist(sim_results$group_in_degree_stddev, true_stats$group_in_degree_stddev, "", group_labels, "Standard Deviation In Degree", save, fprefix, "group_in_deg_stddev")
  panel_hist(sim_results$group_out_degree_stddev, true_stats$group_out_degree_stddev, "", group_labels, "Standard Deviaion Out Degree", save, fprefix, "group_out_deg_stddev")
  # plot betweenness centrality
  plot_hist(sim_results$CB_ave, true_stats$CB_ave, "", "Average Betweenness Centrality", save, fprefix, "ave_bet_cent")
  # plot cycles
  cycle_labels <- paste(2:(length(true_stats$cycles_ave)+1), "-cycles Average", sep="")
  panel_hist(sim_results$cycles_ave, true_stats$cycles_ave, "", cycle_labels, "Cycles", save, fprefix, "cycles_ave")
  cycle_labels <- paste(2:(length(true_stats$cycles_stddev)+1), "-cycles Standard Deviation", sep="")
  panel_hist(sim_results$cycles_stddev, true_stats$cycles_stddev, "", cycle_labels, "Cycles", save, fprefix, "cycles_stddev")
}

#########################
#### Run simulations ####
#########################
SBM_modeling <- function(night, semester, N, bins, par=FALSE, cores=NULL){
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
  
  # create model
  groups <- as.data.frame((vertices %>% count(role)))[,2]
  m_gdi_gen <- GDI$new(groups=groups, probs=p)
  # run simulation
  results <- mc_sim(m_gdi_gen, N, par=par, cores=cores)
  empirical <- network_stats(A, groups)
  plot_results(results, empirical, bins, c("Mentors", "Mentees"), save=TRUE, fprefix=sprintf("%s_%d_%s_%s", "SBM", N, sem, night))
  return(list(results, empirical))
}

SBM2_modeling <- function(night, semester, N, bins, par=FALSE, cores=NULL){
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
      pred_row <- data.frame("I_mfam_night"=conditions[conditions$semester==sem & conditions$night==night, 3], "I_sender_mentee"=(sender$role=="mentee")*1, "I_receiver_mentee"=(receiver$role=="mentee")*1)
      prob <- drop(merge(m_sbm2$X_pred, pred_row)[, "predicted_prob"])
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
  
  # create model
  groups <- as.data.frame((vertices %>% count(role)))[,2]
  m_gdi_gen <- GDI$new(groups=groups, probs=p)
  # run simulation
  results <- mc_sim(m_gdi_gen, N, par=par, cores=cores)
  empirical <- network_stats(A, groups)
  plot_results(results, empirical, bins, c("Mentors", "Mentees"), save=TRUE, fprefix=sprintf("%s_%d_%s_%s", "SBM2", N, sem, night))
  return(list(results, empirical))
}

DI_modeling <- function(night, semester, N, bins, par=FALSE, cores=NULL){
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
      sender_dyad <- substr(sender$final_id, 2, 999)
      receiver_dyad <- substr(receiver$final_id, 2, 999)
      sender_mfam <- sender$menfamid
      receiver_mfam <- receiver$menfamid
      if(sender$role == receiver$role){
        pred_row <- data.frame("I_mfam_night"=conditions[conditions$semester==sem & conditions$night==night, 3], 
                               "I_same_mfam_diff_dyad"= (sender_dyad!=receiver_dyad & sender_mfam==receiver_mfam)*1,
                               "I_same_gender"=(sender$gender==receiver$gender)*1)
      } else {
        pred_row <- data.frame("I_mfam_night"=conditions[conditions$semester==sem & conditions$night==night, 3], 
                               "I_same_dyad"= (sender_dyad==receiver_dyad)*1,
                               "I_same_mfam_diff_dyad"= (sender_dyad!=receiver_dyad & sender_mfam==receiver_mfam)*1,
                               "I_same_gender"=(sender$gender==receiver$gender)*1)
      }
      pred_row[, is.na(pred_row)] <- FALSE
      if(sender$role=="mentor" & receiver$role=="mentor"){
        rel_model <- m_r2r
      } else if(sender$role=="mentor" & receiver$role=="mentee"){
        rel_model <- m_r2e
      } else if(sender$role=="mentee" & receiver$role=="mentor"){
        rel_model <- m_e2r
      } else if(sender$role=="mentee" & receiver$role=="mentee"){
        rel_model <- m_e2e
      }
      prob <- drop(merge(rel_model$X_pred, pred_row)[, "predicted_prob"])
      p[i, j] <- prob
    }
  }
  # construct adjacency matrix (can still use X_sbm in this case)
  vert_map <- hashmap(vertices$final_id, 1:n_verts)
  edges_ <- X_sbm[X_sbm$semester==sem & X_sbm$night==night & X_sbm$edge==1, ]
  senders <- edges_$sender_final_id
  receivers <- edges_$receiver_final_id
  A <- matrix(rep(0, n_verts*n_verts), nrow=n_verts)
  for(i in 1:dim(edges)[1]){
    A[vert_map[[senders[i]]], vert_map[[receivers[i]]]] <- 1
  }
  
  # create model
  groups <- as.data.frame((vertices %>% count(role)))[,2]
  m_gdi_gen <- GDI$new(groups=groups, probs=p)
  # run simulation
  results <- mc_sim(m_gdi_gen, N, par=par, cores=cores)
  empirical <- network_stats(A, groups)
  plot_results(results, empirical, bins, c("Mentors", "Mentees"), save=TRUE, fprefix=sprintf("%s_%d_%s_%s", "DI", N, sem, night))
  return(list(results, empirical))
}

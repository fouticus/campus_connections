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
      return(adj*1)
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
      return(adj*1)
    }
  )
)

##### some functions #####
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

rowVar <- function(x){
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

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

##### end functions #####

# test some models
#n_groups <- 2
#groups <- c(20, 20)
#probs <- matrix(c(0.8, 0.2, 0.0, 0.0), ncol=2, byrow=TRUE)
#m <- SBM$new(groups=groups, probs=probs)

#probs <- matrix(runif(1600), ncol=40, byrow=TRUE)
#m2 <- GDI$new(groups=groups, probs=probs)

#N <- 100
#results <- mc_sim(m, N, groups)
#results2 <- mc_sim(m2, N, groups)

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

# Return to the best location

# Simulate some SBM's based on the SBM LR we ran
N <- 500
bins=50

k <- 1
results <- list(length=length(semesters)*length(nights))
empirical <- list(length=length(semesters)*length(nights))
for(sem in semesters){
  for(night in nights){
    t_start <- Sys.time()
    cat(sprintf("%s: Semester: %s, Night: %s", t_start, sem, night))
    modeling_results <- SBM_modeling(night, sem, N, bins, par=T, cores=3)
    results[[k]] <- modeling_results[[1]]
    empirical[[k]] <- modeling_results[[2]]
    cat(sprintf(", Duration: %s minutes\n", round(difftime(Sys.time(), t_start, units="min"), 2)))
    k <- k + 1
  }
}
stat_names <- names(results[[1]])
ignore = c(stat_names[grep("stddev", stat_names)], stat_names[grep("ave", stat_names)])
global_results <- row_summary(combine_envs(results, along=2, ignore=ignore))
global_empirical <- row_summary(combine_envs(empirical, along=2, ignore=ignore))
plot_results(global_results, global_empirical, bins, c("Mentees", "Mentors"), save=T, fprefix=sprintf("%s_%d_%s", "SBM", N, "global"))


night <- "Mon"
sem <- "Fa15"
for(sem in semesters){
  for(night in nights){
    t_start <- Sys.time()
    cat(sprintf("%s: Semester: %s, Night: %s", t_start, sem, night))
    modeling_results <- SBM2_modeling(night, sem, N, bins, par=T, cores=3)
    results[[k]] <- modeling_results[[1]]
    empirical[[k]] <- modeling_results[[2]]
    cat(sprintf(", Duration: %s minutes\n", round(difftime(Sys.time(), t_start, units="min"), 2)))
    k <- k + 1
  }
}
stat_names <- names(results[[1]])
ignore = c(stat_names[grep("stddev", stat_names)], stat_names[grep("ave", stat_names)])
global_results <- row_summary(combine_envs(results, along=2, ignore=ignore))
global_empirical <- row_summary(combine_envs(empirical, along=2, ignore=ignore))
plot_results(global_results, global_empirical, bins, c("Mentees", "Mentors"), save=T, fprefix=sprintf("%s_%d_%s", "SBM2", N, "global"))
# load packages
library(gmodels)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plyr)
theme_set(theme_classic())

# load data
source("load_and_clean2.R")
source("colors_and_styles.R")

# gender breakdown
pdf("../output/participant_gender.pdf", width=10, height=6)
p_staff_gender <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=gender), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("grey", "pink", "cyan")) + ggtitle("Mentors")
p_mentee_gender <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=gender), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + scale_fill_manual(values=c("pink", "cyan")) + ggtitle("Mentees")
p_gender <- grid.arrange(p_mentee_gender, p_staff_gender, ncol=2)
p_gender
dev.off()

# role breakdown
pdf("../output/participant_roles.pdf", width=10, height=6)
p_staff_role <- ggplot(staff, aes(x=night)) + geom_bar(aes(fill=role), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + ggtitle("Mentors")
p_staff_role <- p_staff_role + role_fill
p_mentee_role <- ggplot(mentees, aes(x=night)) + geom_bar(aes(fill=role), color="black") + facet_grid(semester ~ room) + theme(legend.position="bottom") + ggtitle("Mentees")
p_mentee_role <- p_mentee_role + role_fill
p_role <- grid.arrange(p_mentee_role, p_staff_role, ncol=2)
p_role
dev.off()



# graph statistics
library('igraph')
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



# get statistics for various types of graphs
#sems <- c("Fa15", "Fa16", "Sp16")
sems <- c("Fa15")
nigts <- c("Mon", "Tue", "Wed", "Thu")
threshes <- 1:10
survs <- 1:5
#n_cases = length(sems)*length(nigts)*length(threshes)*length(survs)
max_deg <- 30
degs <- array(0, dim=c(length(sems), length(nigts), length(threshes), length(survs), max_deg))
in_degs <- array(0, dim=c(length(sems), length(nigts), length(threshes), length(survs), max_deg))
out_degs <- array(0, dim=c(length(sems), length(nigts), length(threshes), length(survs), max_deg))

for(i in 1:length(sems)){
  for(j in 1:length(nigts)){
    for(k in 1:length(threshes)){
      for(l in 1:length(survs)){
        return_val <- graph_stats(sems[i], nigts[j], threshes[k], survs[l])
        if(length(return_val[[1]]) > max_deg){
          degs[i, j, k, l, 1:(max_deg-1)] <- return_val[[1]][1:(max_deg-1)]
          degs[i, j, k, l, max_deg] <- sum(return_val[[1]][max_deg:length(return_val[[1]])])
        } else {
          degs[i, j, k, l, 1:length(return_val[[1]])] <- return_val[[1]]
        }
        if(length(return_val[[2]]) > max_deg){
          in_degs[i, j, k, l, 1:(max_deg-1)] <- return_val[[2]][1:(max_deg-1)]
          in_degs[i, j, k, l, max_deg] <- sum(return_val[[2]][max_deg:length(return_val[[2]])])
        } else {
          in_degs[i, j, k, l, 1:length(return_val[[2]])] <- return_val[[2]]
        }
        if(length(return_val[[3]]) > max_deg){
          out_degs[i, j, k, l, 1:(max_deg-1)] <- return_val[[3]][1:(max_deg-1)]
          out_degs[i, j, k, l, max_deg] <- sum(return_val[[3]][max_deg:length(return_val[[3]])])
        } else {
          out_degs[i, j, k, l, 1:length(return_val[[3]])] <- return_val[[3]]
        }
      }
    }
  }
}


# plot node degree distributions for each night
scale_factor <- 3
om <- scale_factor/2
ma <- scale_factor/1.5
plot_rows <- 2
plot_cols <- 2
colfunc <- colorRampPalette(c("black", "green", "red"))
colfunc <- function(n){return(rgb(0, 0, 0, 1:n/n))}

day_degree_plot <- function(degs, file_prefix, title_prefix, scale_factor, om, ma, plot_rows, plot_cols, colfunc, sems, nigts, threshes, survs, max_deg){
  for(i in 1:length(sems)){
    for(k in 1:length(threshes)){
      pdf(paste(paste(paste("../output/", file_prefix, "_dist", sep="") ,sems[i],"all-nights", toString(threshes[k]), "all-surv", sep="_"), ".pdf", sep=""), width=plot_cols*scale_factor, height=plot_rows*scale_factor)
      par(mfrow=c(plot_rows, plot_cols), mar=c(ma, ma, ma, ma), oma=c(om, om, om, om))
      ymax = max(degs[i, , k, , ])
      for(j in 1:length(nigts)){
        matplot(1:max_deg, t(degs[i, j, k, , ]), main=nigts[j], pch=".", type="l", lty=1, ylab=NA, xlab=NA, col=colfunc(length(survs)), ylim=c(0, ymax))
      }
      title = paste(title_prefix, "Distribution for", sems, "with a threshold of:", toString(threshes[k]), sep=" ")
      mtext(title, side=3, adj=0.5, line=0, outer=TRUE, cex=scale_factor/4)
      dev.off()
    }
  }
}
day_degree_plot(degs, "deg", "Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, sems, nigts, threshes, survs, max_deg)
day_degree_plot(in_degs, "in_deg", "In-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, sems, nigts, threshes, survs, max_deg)
day_degree_plot(out_degs, "out_deg", "Out-Degree", scale_factor, om, ma, plot_rows, plot_cols, colfunc, sems, nigts, threshes, survs, max_deg)


colrs <- c("red", "blue", "green", "orange")
# plot average node degree over time
library(ggplot2)
library(reshape2)
n = ceiling(sqrt(length(threshes)))
day_ave_degree_plot <- function(degs, file_prefix, title_prefix, scale_factor, colrs, sems, nigts, threshes, survs, max_deg){
  for(i in 1:length(sems)){
    pdf(paste(paste(paste("../output/", file_prefix, "_ave", sep="") ,sems[i],"all-nights", toString(threshes[k]), "all-surv", sep="_"), ".pdf", sep=""), width=plot_cols*scale_factor, height=plot_rows*scale_factor)
    thresh_plots = vector(mode="list", length=length(threshes))
    for(k in 1:length(threshes)){
      df <- data.frame(1:length(survs))
      colnames(df) <- c("surv")
      for(j in 1:length(nigts)){
        means <- (degs[i, j, k, , ] %*% 1:max_deg)[,1]
        df[nigts[j]] <- means
      }
      df <- melt(df, id.vars="surv")
      p <- ggplot(data=df, aes(x=surv, y=value, col=variable)) + geom_point() + geom_line()
      p <- p + ggtitle(toString(threshes[k])) + ylab(NULL) + xlab(NULL) + theme(legend.position="none")
      thresh_plots[[k]] <- p 
    }
    thresh_plots[[k+1]] <- ggplot(data=df, aes(x=surv, y=value, col=variable)) + geom_point() + geom_line()
    print(grid.arrange(grobs=thresh_plots))
    dev.off()
  }
}
day_ave_degree_plot(degs, "deg", "Degree", scale_factor, colrs, sems, nigts, threshes, survs, max_deg)
day_ave_degree_plot(in_degs, "in_deg", "In-Degree", scale_factor, colrs, sems, nigts, threshes, survs, max_deg)
day_ave_degree_plot(out_degs, "out_deg", "Out-Degree", scale_factor, colrs, sems, nigts, threshes, survs, max_deg)

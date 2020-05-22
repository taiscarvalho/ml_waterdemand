#
# This algorithm performs clusterization using the Self-organizing map method with
# the variables selected with the Random Forest algorithm
#
# xdim, ydim <- vector, size of the SOM grid
# groups     <- vector, number of clusters to evaluate
# cutoff     <- int, cutoff to choose variables from the ranking
# n_var      <- int, number of explanatory variables
# data       <- dataframe, input data containg the dependent variable in the last column
# n_clusters <- final number of clusters, should be chosen by the user by visual inspection
#               of the plots for the cluster measures
#

require(kohonen)
require(dplyr)
require(clValid)
require(patchwork)

n_var <- nrow(data) - 1
cutoff <- n_var - floor(0.45*n_var)
x_dim <- 6
y_dim <- 6
groups <- c(2:6)

ranking <- variable_ranking(data)

data = data[,ranking[seq(cutoff)]] %>%
  scale() %>%
  as.matrix()

som_grid <- somgrid(x_dim, y_dim, topo = "hexagonal")
som_model <- kohonen::som(data, grid = som_grid, keep.data = TRUE)

cluster_assignment <- matrix(nrow = nrow(data), ncol = length(groups))

for(i in 1:length(groups)){
  df = as.data.frame(som_model$codes)
  som_model.hc = cutree(hclust(dist(df)), groups[i])
  cluster_assignment[,i] = som_model.hc[som_model$unit.classif]
}

indices = data.frame()

for(i in 1:length(groups)){
  clusters = cluster_assignment[,i]
  
  #Silhouette
  si <- silhouette(clusters, dist = dist(data))
  indices[i,1] <- summary(si)$avg.width
  
  #Dunn index
  indices[i,2] = dunn(distance = dist(data, method = "euclidean"), clusters = clusters, method = "euclidean")
  
}

colnames(indices) <- rep("ind", 2)
indices$number_clusters <- groups
names <- c("Silhouette coefficient", "Dunn index")

# Plot - measures number of clusters

plot_clusters <- list()
for(i in 1:2){
  plot_clusters[[i]] = ggplot(indices[,c(i,3)], aes(x = number_clusters, y = ind)) +
    geom_line(color = "firebrick2") +
    geom_point(color = "firebrick2") +
    ylab(names[i]) +
    xlab("Number of clusters") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray90"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          plot.title = element_text(size = 16, hjust = 0.5))
}

(plot_clusters[[1]] / plot_clusters[[2]])

n_clusters <- 5

# Plot - Heatmaps

colors_cb <- palette(value = c("1" = "#FFF3B0", "2" = "#C5E1B1", "3" = "#AED9F4", "4" = "#F6CF65", "5" = "#F6A75D"))

par(mfrow = c(4, 3), 
    oma = c(0, 0, 0, 0),
    mar = c(1, 1, 0, 0),
    mgp = c(1, 0, 0), 
    xpd = NA)

plot(som_model, type = "dist.neighbours", main = "U-matrix", shape = "straight")
add.cluster.boundaries(som_model, som_model.hc,lwd = 5)

for(i in 1:cutoff){
  plot(som_model, type = "property", property = getCodes(som_model)[,i], shape = "straight")
  add.cluster.boundaries(som_model, som_model.hc, lwd = 5)
}

#Legend cluster
plot(som_model, type = "mapping", shape = "straight", 
     bgcol = colors_cb[som_model.hc], col = "transparent", 
     codeRendering = "stars", main = "Clusters")
add.cluster.boundaries(som_model, som_model.hc, lwd = 5)

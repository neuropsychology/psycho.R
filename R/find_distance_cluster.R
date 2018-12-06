#' Find the distance of a point with its kmean cluster.
#'
#' Find the distance of a point with its kmean cluster.
#'
#' @param df Data
#' @param km kmean object.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_distance_cluster <- function(df, km){

  myDist <- function(p1, p2) sqrt((p1[,1]-p2[,1])^2+(p1[,2]-p2[,2])^2)

  data <- df %>%
    as.data.frame() %>%
    select(one_of(colnames(km$centers)))

  n_clusters <- nrow(km$centers)

  data$Distance <- NA
  for(clust in 1:n_clusters){
    data$Distance[km$cluster==clust] <- myDist(data[km$cluster==clust,], km$centers[clust,,drop=FALSE])
  }

  return(data$Distance)
}

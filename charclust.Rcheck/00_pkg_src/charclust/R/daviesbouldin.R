#' Compute Davies Bouldin's index
#'
#' Function computing the Davies Bouldin's index which evaluate the clustering quality
#'
#' @param obj an object of type objcharac
#'
#' @return db_index returns the Davies Bouldin index
#'
#' @export
#' @importFrom fields rdist
#'
#' @examples
#' \dontrun{
#' X = c(10, 11, 12, 28)
#' Y = c(20, 23, 28, 15)
#' cl = c(1, 1, 1, 2)
#' data = as.data.frame(cbind(X, Y, cl))
#' colnames(data) = c("X", "Y", "cluster")
#' obj = categorisation(data, data[, -3], NULL, data$cluster)
#' db = db_index(obj)
#' print(db)
#' ------------------
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' db2 = db_index(obj2)
#' print(db2)
#' }
#'
db_index = function(obj){
  # if(!is.categorisation(obj)){
  #   stop("L'argument obj n'est pas de type categorisation")
  # }
  cluster = obj$grp #the clusters
  n = length(cluster) #cluster size
  k = length(unique(obj$grp))

  centre=matrix(nrow=k,ncol=ncol(obj$act)) #compute centroid
  for(i in 1:k){
    for(j in 1:ncol(obj$act)){
      centre[i,j]=mean(obj$act[cluster==i,j])
    }
  }
  #distance between the point and the center
  s=c()
  for(i in 1:k){
    c=centre[i,]
    c_bis = sapply(c, rep, nrow(obj$act[obj$grp==i,]))
    #the average distance between each point of cluster  and the centroid
    s[i] = mean(sqrt(apply((obj$act[obj$grp==i,] - c_bis)^2,1,sum))^2)^(1/2)
  }

  #the distance between cluster centroid
  M = rdist(centre)
  r = matrix(0,nrow=k, ncol = k)
  ri = c()
  for (i in 1:k){
    for (j in 1:k){
      #similarity
      r[i,j] = (s[i] + s[j])/M[i,j]
      if(r[i,j]==Inf | is.na(r[i,j]==Inf)){r[i,j]=0}
    }
    ri[i] = max(r[i,])
  }
  # Compute DB index
  db = mean(ri)
  names(db) = c("DB Index")
  return(db)
}

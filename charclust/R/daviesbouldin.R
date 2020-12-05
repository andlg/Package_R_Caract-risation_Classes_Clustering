#' Compute Davies Bouldin's index
#'
#' @param obj a categorisation
#'
#' @return the Davies Bouldin index
#'
#' @export
#' @importFrom fields rdist
#'
#' @examples
#' X = c(10, 11, 12, 28)
#' Y = c(20, 23, 28, 15)
#' cl = c(1, 1, 1, 2)
#' data = as.data.frame(cbind(X, Y, cl))
#' colnames(data) = c("X", "Y", "cluster")
#' obj = categorisation(data, data[, -3], NULL, data$cluster)
#' db.categorisation(obj)
#'
db.categorisation = function(obj){
  # if(!is.categorisation(obj)){
  #   stop("L'argument obj n'est pas de type categorisation")
  # }
  cluster = obj$grp
  n = length(cluster)
  k = length(unique(obj$grp))

  centre=matrix(nrow=k,ncol=ncol(obj$act)) #centroides
  for(i in 1:k){
    for(j in 1:ncol(obj$act)){
      centre[i,j]=mean(obj$act[cluster==i,j])
    }
  }
  s=c() #distance entre le point et le centroide
  for(i in 1:k){
    c=centre[i,]
    c_bis = sapply(c, rep, nrow(obj$act[obj$grp==i,]))
    s[i] = mean(sqrt(apply((obj$act[obj$grp==i,] - c_bis)^2,1,sum))^2)^(1/2)
  }

  M = rdist(centre) #distance entre centroides
  r = matrix(0,nrow=k, ncol = k)
  ri = c()
  for (i in 1:k){
    for (j in 1:k){
      r[i,j] = (s[i] + s[j])/M[i,j]
      if(r[i,j]==Inf | is.na(r[i,j]==Inf)){r[i,j]=0}
    }
    ri[i] = max(r[i,])
  }

  db = mean(ri)
  return(db)
}

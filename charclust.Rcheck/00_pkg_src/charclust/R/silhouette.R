#' Compute silhouette information from clustering
#'
#' Function computing the silhouette which evaluate the clustering quality
#'
#'
#' @param obj a categorisation
#'
#' @return sil returns a list containing:
#' \describe{
#'  \item{$silclus}{the mean silhouette coefficients for each clusters}
#'  \item{$silglob}{the total mean silhouette coefficient}
#'  \item{$plot}{a plot of cluster means of silhouette}
#' }
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' X = c(10, 11, 12, 28)
#' Y = c(20, 23, 28, 15)
#' cl = c(1, 1, 1, 2)
#' data = as.data.frame(cbind(X, Y, cl))
#' colnames(data) = c("X", "Y", "cluster")
#' obj = categorisation(data, data[, -3], NULL, data$cluster)
#' s = sil(obj)
#' s$silclus
#' s$silglob
#' s$plot
#' ------------------
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' s2 = sil(obj2)
#' s2$silclus
#' s2$silglob
#' s2$plot
#' }
#'
sil = function(obj){
  # if(!is.categorisation(obj)){
  #   stop("L'argument obj n'est pas de type categorisation")
  # }

  var_grp = obj$grp #cluster
  my_data = obj$act
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  col = colnames(my_data)
  #get numeric variables
  for (i in 1:ncol(my_data)){
    var = col[i]
    if(is.numeric(my_data[,var])){
      df_quanti[var] = my_data[,var]
    }
  }
  col_quanti = colnames(df_quanti)
  my_data = obj$act[col_quanti]

  #distance matrix
  m_dist = rdist(my_data)
  #distance matrix with clusters
  m_cplt = cbind(m_dist, obj$grp)
  sil = c()
  #compute foreach points the silhouette coef
  for (i in 1:length(var_grp)) {
    cluster = var_grp[i]
    #a: average distance from the point to its group
    a = list()
    for (j in 1:(ncol(m_cplt)-1)) {
      ik = length(var_grp[var_grp==cluster])
      ai = 0
      if(m_cplt[j,ncol(m_cplt)] == cluster){
        a = append(a, m_cplt[i,j])
      }
      ai = sum(unlist(a))
      if(ik != 0){
        ai = ai * (1/(ik-1))
      }
      if (is.na(ai)) {
        ai = 0
      }
      #b: average distance from the point to its neighboring group
      b_ = list()
      bi = 0
      for (k in 1:length(unique(var_grp))) {
        b = 0
        if(cluster != unique(var_grp)[k]){
          m = m_cplt[m_cplt[,ncol(m_cplt)]==unique(var_grp)[k], -ncol(m_cplt)]
          ik_ = length(var_grp[var_grp==unique(var_grp)[k]])
          if(ncol(as.matrix(m))==1){m = rbind(0, m)}
          b = colSums(m)[i]/ik_
          b_ = append(b_, b)
        }
      }
      bi = min(unlist(b_))
    }
    #silhouette coefficient of the point
    sil[i] = (bi-ai)/(max(bi,ai))
    if(is.na(sil[i])){
      sil[i] = 0
    }
  }
  out = data.frame(cbind(sil,var_grp))
  colnames(out) = c("sil", "cluster")
  #Compute the mean silhouette coefficients for each clusters
  sil_clust = tapply(out$sil, out$cluster, mean)
  #Compute the mean silhouette coefficient
  sil_glob = mean(sil_clust)
  silhouette = list(sil_clust, sil_glob)

  p = as.data.frame(silhouette[[1]])
  p = cbind(p, as.numeric(rownames(p)))
  colnames(p) = c("sil", "cluster")
  #Graph which represents the mean silhouette coefficients for each cluster
  graph = ggplot(data=p ,aes(x=sil, y=rownames(p))) +
    geom_bar(stat="identity", color = "black", fill = "steelblue") +
    xlim(-1, 1) +
    xlab("sil") + ylab("cluster") +
    theme_minimal()

  output = list(silclus = sil_clust, silglob = sil_glob, plot = graph)
  return(output)
}

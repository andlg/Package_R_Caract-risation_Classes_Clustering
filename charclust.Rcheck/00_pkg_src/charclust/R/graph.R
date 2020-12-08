#' Plot some graphs about characterisation of clusters
#'
#' This fucntion returns graphs from an objcharac object.
#'
#'
#' @param obj an object of type objcharac
#' @param type a string indicating the graph wanted : "illus" : distribution for illustrative variables - "act" : boxplots for active variables - "pca" : factorial representation
#' @param profile Optionnally, a string indicating which profile to see for the "illus" graph : "l" : line - "c": column. The default is "l"
#'
#' @import ggplot2 FactoMineR factoextra
#'
#' @return charac_graph returns some graphs about characterisation of clusters
#' \describe{
#' \item{Case type "illus"}{boxplots or barplots depending on the type of illustrative variables}
#' \item{Case type "act"}{boxplots or barplots depending on the type of active variables}
#' \item{Case type "pca"}{PCA graphs for variables and individuals}
#' }
#'
#' @examples
#' \dontrun{
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' #graph for active variables
#' g_act <- charac_graph(obj, type = "act")
#' print(g_act)
#' print(g_act$taux_dinsertion)
#' #graph for illustrative variables, distribution foreach cluster
#' g_illus <- charac_graph(obj, type = "illus", profile = "l")
#' print(g_illus)
#' print(g_illus$domaine)
#' #graph for illustrative variables, distribution of clusters foreach modality
#' g_illus2 <- charac_graph(obj, type = "illus", profile = "c")
#' print(g_illus2)
#' print(g_illus2$domaine)
#' #PCA graph
#' g_pca <-  charac_graph(obj, type = "pca")
#' g_pca
#' #Contribution of variables foreach components
#' g_pca$var
#' #Individuals and clusters in PCA chart
#' g_pca$ind
#' #Individuals and clusters in PCA chart with labels
#' g_pca$ind_lab
#' }
#'
#' @export
charac_graph<-function(obj,type,profile="l"){
  if(class(obj) != "objcharac"){
    stop("The argument obj is not an objcharac")
  }

  df_uni<-cbind(obj$data,grp=obj$grp)
  if (type=="illus"){
    graph_uni_illus<-lapply(colnames(obj$illus) , graph_draw,df=df_uni,clust=as.factor(df_uni$grp),profil=profile)
    names(graph_uni_illus)<-colnames(obj$illus)
    return(graph_uni_illus)
  }

  if (type=="act"){
    graph_uni_act<-lapply(colnames(obj$act) , graph_draw,df=df_uni,clust=as.factor(df_uni$grp),profil=profile)
    names(graph_uni_act)<-colnames(obj$act)
    return(graph_uni_act)
  }

  if (type=="pca"){
    col_quanti<-which(sapply(obj$data, is.numeric) ==T)
    graph_pca(obj$data[,col_quanti],obj$grp)
  }
}

graph_draw<-function(df,col,clust,profil){

  #Case numeric variable
  if (is.numeric(df[,col])){
    if(length(levels(as.factor(df[,col])))>2){
      plot_quanti<-(ggplot(df, aes(x = clust,y = df[,col], color=clust)) +
                      geom_boxplot() +
                      ggtitle("Box-plots") +
                      xlab("Clusters") +
                      ylab(col) +
                      theme(legend.position = "none") +
                      stat_summary(fun=mean, geom="point", shape=4, size=2, color="black")
      )
      return (plot_quanti)
      #Case numeric variable corresponding to qualitative ==> dummy
    } else{
      # transform 1 - 0 into "1" - "0"
      df[,col] <- as.factor(df[,col])
    }
  }

  #Case categorical illustrative variable
  #Distribution of modalities inside clusters
  if(profil=="l"){
    ggplot(df, aes_string(x = clust,fill = col)) +
      geom_bar(position = "stack" ) +
      xlab("Clusters") +
      ggtitle("Distribution of modalities inside clusters")
    #Distribution of clusters for each modality
  } else if (profil=="c"){
    ggplot(df, aes_string(x = col,fill = clust)) +
      geom_bar(position = "stack" ) +
      labs(fill = "Clusters", x=col)+
      ggtitle("Distribution of clusters for each modality")
  }
}



graph_pca<-function(data,cluster){

  cluster<-as.factor(cluster)
  #Compute Principal Component Analysis
  res.pca<-PCA(data, scale.unit = TRUE, ncp = 5, graph = F)

  #Graph for variables
  gvar<-fviz_pca_var(res.pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=T)

  #-------Version factoexra
  #gind<-fviz_pca_ind(res.pca, col.ind =cluster,geom.ind = "point")#graphique des individus selon leurs coordonnees et contribution
  #gind2<-fviz_pca_ind(res.pca, col.ind =cluster)

  #-------Version ggplot2
  eig.val <- get_eigenvalue(res.pca)
  #Building principal components
  pc1 <- res.pca$ind$coord[, 1]
  pc2 <- res.pca$ind$coord[, 2]
  #Labels with Inertia
  pc1_lab <- paste("PC1",round(eig.val[1,2],2),"%")
  pc2_lab <- paste("PC2",round(eig.val[2,2],2),"%")

  #Graph 1
  gind <- ggplot(data = data, aes(x = pc1, y = pc2, color = cluster, shape = cluster)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 0, lty = 2) +
    geom_point(alpha = 0.8) +xlab(pc1_lab)+ylab(pc2_lab) +
    ggtitle("Individuals and clusters in PCA chart")

  #Graph 2
  gind2<-gind+geom_text(label=rownames(data)) +
  ggtitle("Individuals and clusters in PCA chart (with labels)")

  #Output
  list(var=gvar,ind=gind,ind_lab=gind2)

}



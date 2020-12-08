#' Test Value Indicator
#'
#' This fucntion returns characterisation of clusters using the test value indicator.
#'
#'
#' @param obj an object of type objcharac
#'
#' @import ggplot2 reshape2 questionr
#'
#' @return testval returns a list with:
#' \describe{
#'  \item{$num}{numeric results of value test}
#'  \item{$graph}{radar chart of values test for numeric variables}
#'  }
#'
#' @examples
#' \dontrun{
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' t = testval(obj)
#' #print all numerical results
#' print(t$num)
#' #plot radar chart of values test for numeric variables
#' t$graph
#' #print result for numeric variables
#' tquanti <- t$num[[1]]
#' print(tquanti)
#' #print result for one numeric variable
#' print(tquanti[,"taux_dinsertion"])
#' #print result for qualitative variables
#' tquali <- t$num[[2]]
#' print(tquali)
#' #print result for one qualitative variable
#' print(tquali$domaine)
#'
#'  }
#'
#' @export

testval = function(obj){

  if(class(obj) != "objcharac"){
    stop("The argument obj is not an objcharac")
  }

  suppressWarnings(if(nrow(obj$illus)==0){my_data = obj$act} else{my_data = cbind(obj$act, obj$illus)})
  var_grp = obj$grp
  #create a df for qualitative variables and a df for quantitative variables
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  df_quali = as.data.frame(obj$data[obj$var_grp])
  rename.variable(data.frame(df_quali), colnames(data.frame(df_quali)), "var_grp")
  col = colnames(my_data)

  for (i in 1:ncol(my_data)) {
    var = col[i]
    if(is.numeric(my_data[,var])){ #quantitative variables
      df_quanti[var] = my_data[,var]
    } else{
      df_quali[var] = my_data[,var] #qualitative variables
    }
  }
  col_quali = colnames(df_quali)
  vt_quali = sapply(my_data[,col_quali], vtest_quali, y=var_grp) #test value for qualitative variables
  col_quanti = colnames(df_quanti)
  vt_quanti = sapply(my_data[col_quanti], vtest_quanti, y=var_grp) #test value for quantitative variables
  vt = list(vt_quanti, vt_quali)
  if(length(vt[[1]]) == 0){vt = vt[[2]]}
  if(length(vt[[2]]) == 0){vt = vt[[1]]}
  #apply graph_vtest function on test values of quantitative variables
  graph<-graph_vtest(vt[[1]])
  #return numeric and graph results
  return(list(num = vt,graph = graph))

}

vtest_quanti = function(x, y){ #compute test value for quantitative variables
  g = length(unique(y)) #nb clusters
  n = length(x) #nb obs
  moy = mean(x) #global mean
  ng = table(y) #conditional size
  mg = tapply(x,y,mean) #conditional means
  var = var(x)

  vt = c((mg-moy)/sqrt(((n-ng)/(n-1))*(var/ng))) #test value
  names(vt) = c(paste("G",1:g, "vt"))
  return(vt)
}

vtest_quali=function(x,y){ ##compute test value for qualitative variables
  g = length(unique(y)) #nb clusters
  n = length(x) #nb obs
  pl_perc = lprop(table(y,x), percent = TRUE)
  pl=colSums(table(y,x))
  vt=matrix(nrow = g, ncol = ncol(lprop(table(y,x)))-1)
  colnames(vt) = colnames(pl_perc)[-ncol(lprop(table(y,x)))]
  row.names(vt) = rownames(pl_perc)[-nrow(lprop(table(y,x)))]
  for (i in 1:g) {
    for (j in 1:ncol(vt)) {
      vt[i,j] = sqrt(pl[j]) * ((pl_perc[i,j]/100-pl_perc[g+1,j]/100)/sqrt(((n-pl[j])/(n-1))*pl_perc[g+1,j]/100*(1-pl_perc[g+1,j]/100)))
    }
  }
  return(vt)
}

#
graph_vtest<-function(v){ #radar chart of values test for numeric variables

  rvt<-as.data.frame(v)
  #sort columns by name for the chart
  order<-sort(colnames(rvt))
  rvt<-rvt[,order]
  #create df with columns containing names of variables
  rvt2<-data.frame(ID= as.vector(colnames(rvt)))

  #create column for each clusters
  for(i in 1:nrow(rvt)) {
    #get test value for each cluster
    new <- as.double(rvt[i,])
    #assign new as a new column
    rvt2[ , ncol(rvt2) + 1] <- new
    #give to the column the name of the cluster
    colnames(rvt2)[ncol(rvt2)] <- paste0("gr", i)
  }

  #melt to unable the graph
  df.vt <- melt(rvt2,
                id.vars= order(colnames(rvt2)),
                measure.vars= c(colnames(rvt2[,c(2:ncol(rvt2))])),
                variable.name= "Cluster",
                value.name=    "val"
  )

  graph<-ggplot(data=df.vt,  aes(x=ID, y=val, group= Cluster, colour=Cluster, fill=Cluster)) +
    geom_point(size=2) +
    #join the points
    geom_polygon(size = 1, alpha= 0) +
    ggtitle("Test Values")  +
    scale_x_discrete("") +
    scale_y_continuous("")+
    theme_light()+
    coord_radar()

}

#http://www.cmap.polytechnique.fr/~lepennec/en/post/radar/radarandparallelplots/
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

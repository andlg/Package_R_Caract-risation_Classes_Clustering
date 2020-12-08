#' Univariate characterization of classes resulting from a clustering
#'
#' @param obj an object of type objcharac
#'
#' @return the cramers'V or the correlation for each variable and cluster depending on the variable's type
#' \describe{
#'  \item{Case qualitative variables}{compute khi2, p-value, cramer's V}
#'  \item{Case quantitative variables}{compute conditional means, correlation}
#' }
#' @export
#' @importFrom stats chisq.test
#'
#' @examples
#' \dontrun{
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' uni = charac_uni(obj)
#' #show results for all variables
#' print(uni)
#' #example for the variable "domaine":
#' print(uni$domaine)
#' }
#'
#'
charac_uni = function(obj){

  if(class(obj) != "objcharac"){
    stop("The argument obj is not an objcharac")
  }

  if(length(obj$illus) != 0){my_data = cbind(obj$act, obj$illus)} else{my_data = obj$act}
  #apply for each column: output = list
  sortie_uni = lapply(my_data, calcul_uni, cluster=obj$grp)
  return(sortie_uni)
}


calcul_uni = function(col, cluster){
  #case quali
  if (!is.numeric(col)){
    f<-length(levels(as.factor(col)))-1
    cl<-length(levels(as.factor(cluster)))-1
    test<-suppressWarnings(chisq.test(cluster,col)) #compute chisq test
    n<-length(col)
    denom<- n*min(c(cl, f))
    khival<-test$statistic #chi-squared test statistic
    cramer<-sqrt(khival/denom) #cramer's v
    p_value<-test$p.value #p-value for the test

    result = c(khival, p_value, cramer)
    names(result) = c("khi2", "p_value", "cramer")
    return(result)

  }else { #case quanti
    #http://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf
    g = length(unique(cluster)) #nb clusters
    n = length(col) #nb obs
    moy = mean(col) #global mean

    sct = sum((col-moy)^2) #total variability
    ng = table(cluster) #conditional size
    mg = tapply(col,cluster,mean) #conditional mean
    #inter variability
    sce = sum(ng * (mg - moy)^2) #

    #mean vectors + correlation
    cor = c(mg,100.0*sce/sct)
    names(cor) = c(paste("G",1:g),"% epl.")
    return(cor)
  }
}




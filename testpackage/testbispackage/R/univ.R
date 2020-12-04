

#' Title
#'
#' @param col une colonne
#' @param cluster un cluster
#'
#'
#'
#'@importFrom stats chisq.test
#'
calculs_uni = function(col, cluster){
  #print(is.numeric(col))
  if (length(col)==length(cluster) && !is.numeric(col)){
    f<-length(levels(col))-1
    cl<-length(levels(as.factor(cluster)))-1
    test<-suppressWarnings(chisq.test(cluster,col))
    ddl<-test$parameter
    n<-length(col)
    denom<- n*ddl
    khival<-test$statistic
    cramer<-sqrt(khival/denom)
    p_value<-test$p.value

    result = c(khival, ddl, p_value, cramer)
    names(result) = c("khi2","ddl","p_value", "cramer")
    return(result)

  }else {
    g = length(unique(cluster)) #nb groupes
    n = length(col) #nb obs
    moy = mean(col) #moyenne generale

    sct = sum((col-moy)^2) #variabilit? totale
    ng = table(cluster) #effectifs conditionnels
    mg = tapply(col,cluster,mean) #moyennes conditionnelles
    #variabilit? inter
    sce = sum(ng * (mg - moy)^2)

    #vecteur moyennes + rapport correlation
    cor = c(mg,100.0*sce/sct)
    #nommer les ?lements du vecteur
    names(cor) = c(paste("G",1:g),"% epl.")
    return(cor)
  }
}


#' Title
#'
#' @param obj un obj
#'
#' @return blabla
#' @export
#'
#'
univariee.categorisation = function(obj){
  #sortie_uni = sapply(obj$data[obj$act], calculs_uni, cluster=obj$grp)
  my_data = cbind(obj$act, obj$illus)
  sortie_uni = lapply(my_data, calculs_uni, cluster=obj$grp)
  return(sortie_uni)
}

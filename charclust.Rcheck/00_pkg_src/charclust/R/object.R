#' objcharac
#'
#' @param df a dataframe with all the data
#' @param var_act a dataframe with active variables
#' @param var_illus a dataframe with illustrative variables
#' @param var_grp a dataframe or a list with the clusters
#'
#' @return an object of type objcharac
#' \describe{
#'  \item{$data}{dataframe}
#'  \item{$act}{dataframe with active variables for clustering}
#'  \item{$illus}{dataframe with illustrative variables, default=NULL}
#'  \item{$grp}{clusters for each observations}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(charclust)
#' data(iris)
#' obj <- objcharac(iris, iris[,-5], NULL, iris$Species)
#' -----------------
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' }
#'
objcharac = function(df, var_act, var_illus=NULL, var_grp){
  #controles
  if(!is.data.frame(df)){
    stop("the argument df is not a data frame")
  }
  if(nrow(df) != length(var_grp)){
    stop("df and var_grp have not the same size")
  }

  #initialize
  instance = list()
  instance$data = df
  instance$act = data.frame(var_act)
  instance$illus = data.frame(var_illus)
  instance$grp = var_grp

  class(instance) = "objcharac"

  return(instance)
}

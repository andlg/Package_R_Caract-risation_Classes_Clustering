#' Categorisation
#'
#' @param df a dataframe with all datas
#' @param var_act a dataframe with active variables
#' @param var_illus a dataframe with illustrative variables
#' @param var_grp a dataframe with the clusters
#'
#' @return a categorisation object
#'
#' @export
#'
#' @examples
#' library(charclust)
#' data(iris)
#' obj = categorisation(iris, iris[,-5], NULL, iris$Species)
#'
categorisation = function(df, var_act, var_illus, var_grp){
  #controles
  if(!is.data.frame(df)){
    stop("L'argument df n'est pas un data frame")
  }

  #creation de l'instance
  instance = list()
  instance$data = df
  instance$act = data.frame(var_act)
  instance$illus = data.frame(var_illus)
  #colonne des clusters
  instance$grp = var_grp
  #liste des clusters
  instance$n_grp = sort(unique(var_grp))

  class(instance) = "categorisation"

  return(instance)
}



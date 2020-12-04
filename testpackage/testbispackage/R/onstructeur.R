#' Title
#'
#' @param df un df
#' @param var_act un truc
#' @param var_illus un truc
#' @param var_grp un truc
#'
#'
#' @export
#'
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

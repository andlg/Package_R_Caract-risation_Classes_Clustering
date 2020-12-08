#'  Integration after master's in 2012 in France
#' @source Based on \url{https://www.data.gouv.fr/fr/datasets/insertion-professionnelle-des-diplomes-de-master-en-universites-et-etablissements-assimil-}
#' @format A data frame with columns:
#' \describe{
#'  \item{diplome}{character giving the type of the diplome}
#'  \item{etablissement}{character giving the establishment name}
#'  \item{academie}{character giving the regional education authority}
#'  \item{code_du_domaine}{character giving the zip code field}
#'  \item{domaine}{character giving the fields name: "droit, economie, gestion (DEG)"="law and economics"; "sciences, technologies et santé (STS)"="sciences, tecnologies and health"; "lettres, langues et arts(LLA)"="letters, languages and arts"; "sciences humaines et sociales (SHS)"="human social sciences"; "masters enseignement (MEEF)"="teaching"}
#'  \item{discipline}{character giving the discipline name}
#'  \item{taux_dinsertion}{Numeric values between 0 and 100 giving the integration rate}
#'  \item{emplois_cadres_ou_professions_intermediaires}{Numeric values between 0 and 100 giving the executive employment rate}
#'  \item{emplois_stables}{Numeric values between 0 and 100 giving the stable job rate}
#'  \item{emplois_a_temps_plein}{Numeric values between 0 and 100 giving full time employment rate}
#'  \item{salaire_brut_annuel_estime}{Numeric values giving the annual gross income estimated}
#'  \item{femmes}{Numeric values giving between 0 and 100 giving the women rate}
#' }
#'
"insertion_master"

#' Linear Discriminant Analysis on clustering results
#'
#' This fucntion predicts clusters using Linear Discriminant Analysis (LDA). It allow interpreation of clustering results showing the influence of variables.
#'
#'
#' @param obj an object of type objcharac
#'
#' @return
#' \describe{
#'  \item{$coef_LDA}{coefficients of LDA}
#'  \item{$confusion_matrix}{groups predict by LDA VS clusters}
#'  \item{$eval}{statistic F and p-value of Fisher test}
#' }
#'
#' @examples
#' \dontrun{
#' data(insertion_master)
#' data.illu <- insertion_master[,c(1:6,12)]
#' data.act <-insertion_master[,7:11]
#' res.kmeans<-kmeans(data.act,centers=3,nstart=5)
#' obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
#' adl = clustlda(obj)
#' #the coefficients of LDA model
#' print(adl$coef_LDA)
#' #the confusion matrix
#' print(adl$confusion_matrix)
#' #the model evaluation
#' print(adl$eval)
#' }
#'
#' @import stats MASS
#'
#' @export

#Source: http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_LDA_MASS_R.pdf
clustlda = function(obj){

  if(class(obj) != "objcharac"){
    stop("The argument obj is not an objcharac")
  }

  var_grp = obj$grp
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  col = colnames(obj$act) #nom des colonnes

  #get numeric variables
  for (i in 1:ncol(obj$act)){
    var = col[i]
    if(is.numeric(obj$act[,var])){
      df_quanti[var] = obj$act[,var]
    }
  }

  col_quanti = colnames(df_quanti)

  if(length(col_quanti)==0){
    return(NULL)
  }

  my_data = cbind(obj$act[col_quanti], grp = obj$grp)
  #sampling
  split = sort(sample(nrow(my_data), nrow(my_data)*0.75))
  train = my_data[split, ]
  test = my_data[-split,]

  #Define features and target foreach samples
  Xtrain = train[, -ncol(my_data)]
  ytrain = as.factor(train[, ncol(my_data)])
  Xtest = test[, -ncol(my_data)]
  ytest = as.factor(test[, ncol(my_data)])

  #compute LDA model
  my_adl = lda(grp ~ ., data = train)
  coef_adl = my_adl$scaling
  #prediction
  pred = predict(my_adl, newdata = Xtest)
  #confusion matrix
  mc = table(ytest, pred$class)

  #Statistic evaluation
  p = ncol(Xtrain)
  n = nrow(train)
  k = nlevels(ytrain)

  # total co-variance matrix
  m_tot = cov(Xtrain)
  #intra co-variace matrix: the variable to evaluate is removing
  m_intra = (1.0/(n-k))*Reduce("+",lapply(levels(ytrain),function(niveau){(sum(ytrain==niveau)-1)*cov(Xtrain[ytrain==niveau,])})) #reduce applies iteratively a function

  WITprim = (n-k)/n*m_intra
  TOTprim = (n-1)/n*m_tot

  #Wilks lambda
  LW = det(WITprim)/det(TOTprim)

  ddlSuppNum = k - 1
  ddlSuppDenom = n - k - p + 1
  FTest = numeric(p)
  pvalue = numeric(p)

  #Compute Ftest and pvalue
  for (j in 1:p){
    #Lambda
    LWvar = det(WITprim[-j,-j])/det(TOTprim[-j,-j])
    #F
    FTest[j] = ddlSuppDenom / ddlSuppNum * (LWvar/LW - 1)
    #pvalue
    pvalue[j] = pf(FTest[j],ddlSuppNum,ddlSuppDenom,lower.tail=FALSE)
  }
  out = data.frame(var=colnames(Xtrain),FValue=FTest,pvalue=round(pvalue,6))

  r = list(coef_adl, mc, out)
  names(r) = c("coef_LDA", "confusion_matrix", "eval")
  return(r)
}

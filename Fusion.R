setwd("C:/Users/Axelle/Desktop/M/03_SISE/01_PROGRAMMATION R/PROJET/ex")

#librairies
library(readxl)
library(questionr)
library(ggplot2)
library("FactoMineR")
library("factoextra")
library(ggfortify)
library(reshape2)
library(MASS)
library(fields)
library(biganalytics)



###################Constructeur
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

#On surcharge la méthode print
print.categorisation = function(obj){
  cat("variables actives: ", colnames(obj$act), "\n") #CELLE LIGNE NE MARCHE PAS QUAND ON A UNE SEUL VARIABLE!!!!
  cat("variables illustratives: ", colnames(obj$illus), "\n") #CELLE LIGNE NE MARCHE PAS QUAND ON A UNE SEUL VARIABLE!!!!
  cat("Nombre de groupes: ", length(obj$n_grp), "\n")
}

#On surcharge la méthode summary
summary.categorisation = function(obj){
  return(print(obj))
}


#On surcharge la méthode is
is.categorisation = function(obj){
  return(class(obj) == "categorisation")
}

#----------------------------------Fonctions pour la caract?risation univari?e--------------------------------------------
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


univariee.categorisation = function(obj){
  if(length(obj$illus)!=0){my_data = cbind(obj$act, obj$illus)} else{my_data = obj$act}
  sortie_uni = lapply(my_data, calculs_uni, cluster=obj$grp)
  return(sortie_uni)
}



#Valeur test
vtest_quanti = function(x, y){ #pour les variables quantitatives
  g = length(unique(y)) #nb groupes
  n = length(x) #nb obs
  moy = mean(x) #moyenne generale
  ng = table(y) #effectifs conditionnels
  mg = tapply(x,y,mean) #moyennes conditionnelles
  var = var(x)

  vt = c((mg-moy)/sqrt(((n-ng)/(n-1))*(var/ng)))
  #print((mg-moy))
  names(vt) = c(paste("G",1:g, "vt"))
  return(vt)
}

vtest_quali=function(x,y){ #pour les variables qualitatives
  g = length(unique(y)) #nb groupes
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

#GENERALISATION VTEST POUR LES DEUX TYPES
vtest.categorisation = function(obj){
  if(!is.categorisation(obj)){
    stop("L'argument obj n'est pas de type categorisation")
  }

  suppressWarnings(if(nrow(obj$illus)==0){my_data = obj$act} else{my_data = cbind(obj$act, obj$illus)})
  var_grp = obj$grp
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  df_quali = as.data.frame(obj$data[obj$var_grp])
  rename.variable(data.frame(df_quali), colnames(data.frame(df_quali)), "var_grp")
  #print(df_quali)
  col = colnames(my_data) #nom des colonnes
  #print(col)
  for (i in 1:ncol(my_data)) {
    var = col[i]
    if(is.numeric(my_data[,var])){ #on recupere les variables quanti
      df_quanti[var] = my_data[,var]
    } else{
      df_quali[var] = my_data[,var] #on recupere les variables quali
    }
  }
  col_quali = colnames(df_quali)
  vt_quali = sapply(my_data[,col_quali], vtest_quali, y=var_grp)
  col_quanti = colnames(df_quanti)
  vt_quanti = sapply(my_data[col_quanti], vtest_quanti, y=var_grp)
  vt = list(vt_quanti, vt_quali)
  if(length(vt[[1]]) == 0){vt = vt[[2]]}
  if(length(vt[[2]]) == 0){vt = vt[[1]]}
  #return(vt)
  graph<-graph_vtest(vt[[1]])


  return(list(num = vt,graph = graph))

}

graph_vtest<-function(v){
  rvt<-as.data.frame(v)

  order<-sort(colnames(rvt))
  rvt<-rvt[,order]

  rvt2<-data.frame(ID= as.vector(colnames(rvt)) #,
                   # gr1=c(as.double( rvt[1,])),
                   # gr2=c(as.double(rvt[2,]))
  )

  for(i in 1:nrow(rvt)) {                                   # Head of for-loop
    new <- as.double(rvt[i,])                      # Create new column
    rvt2[ , ncol(rvt2) + 1] <- new                  # Append new column
    colnames(rvt2)[ncol(rvt2)] <- paste0("gr", i)  # Rename column name
  }


  df.vt <- melt(rvt2,
                id.vars= order(colnames(rvt2)),
                measure.vars= c(colnames(rvt2[,c(2:ncol(rvt2))])),
                variable.name= "Cluster",
                value.name=    "val"
  )

  # plot
  #ylim(-2.0, 2.0) +
  graph<-ggplot(data=df.vt,  aes(x=ID, y=val, group= Cluster, colour=Cluster, fill=Cluster)) +
    geom_point(size=2) +
    geom_polygon(size = 1, alpha= 0.2) +

    ggtitle("Valeurs test")  +
    scale_x_discrete("") +
    scale_y_continuous("")+
    theme_light()+
    #scale_color_manual(values= c("yellow", "blue"))+
    # scale_fill_manual(values= c("yellow", "blue"))+
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


objet<-categorisation(data.reduite,data.reduite,NULL,res.kmeans$cluster)
objet$data
objet$grp

sortieuni <-univariee.categorisation(objet)
sortieuni

sortievt<-vtest.categorisation(objet,objet$grp)
sortievt$num
sortievt[[2]]
sortievt[[1]]

#--------------------------------------SORTIES GRAPHIQUES-------------------------------------------------------
graph_uni_quali<-function(df,col,clust,profil){

  #bar <- ggplot (dfpack, aes(x = clust, fill = col))
  #bar + geom_bar (position = "stack") # pr?cise que les ZAU sont "empil?es"
  if (is.numeric(df[,col])){
    graph_uni_quanti(df,df[,col],clust)
  }else{
    if(profil=="l"){
      ggplot(df, aes_string(x = clust,fill = col)) +
        geom_bar(position = "stack" ) +
        #xlab("Cluster")
        xlab(class(df[,col]))
    }else if (profil=="c"){
      ggplot(df, aes_string(x = col,fill = clust)) +
        geom_bar(position = "stack" ) +
        #xlab("Cluster")
        xlab(class(df[,clust]))
    }

  }

}

graph_uni_quanti<-function(df,col,clust){
  c<-as.character(names(col))
  #bar <- ggplot (dfpack, aes(x = clust, fill = col))
  #bar + geom_bar (position = "stack") # pr?cise que les ZAU sont "empil?es"
  ggplot(df, aes(clust,col)) +
    geom_boxplot()

}

#ajouter cos2 contrib ...?????
fun_acp<-function(data,cluster){

  cluster<-as.factor(cluster)

  #numillu<-match(illu,colnames(data))
  #supl<-which(sapply(data[,numillu], is.numeric) ==T)
  #supl<-as.numeric(supl)

  # data_acp<-cbind(data[,supl],data[,act])

  #https://huboqiang.cn/2016/03/03/RscatterPlotPCA
  #res.pca<-PCA(data_acp,quanti.sup = 1, scale.unit = TRUE, ncp = 5, graph = F)
  res.pca<-PCA(data, scale.unit = TRUE, ncp = 5, graph = F)
  #res.pca <- prcomp(act, scale. = TRUE)

  #par(mfrow = c(1,2))
  var <- get_pca_var(res.pca) #Creation d'une variable "var" avec tous les resultats concernants les variables
  gvar<-fviz_pca_var(res.pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=T)


  ind <- get_pca_ind(res.pca) #Creation d'une variable "ind" avec tous les resultats concernants les variables
  #-------Version factoexra
  #ok#  gind<-fviz_pca_ind(res.pca, col.ind =cluster,geom.ind = "point")#graphique des individus selon leurs coordonnees et contribution
  #gvar<-autoplot(res.pca, data = act, colour = cluster)
  #gind<-  gvar<-autoplot(res.pca, data = act, colour = cluster, loadings=T)

  #ok#  gind2<-fviz_pca_ind(res.pca, col.ind =cluster)

  #-------Version ggplot2
  eig.val <- get_eigenvalue(res.pca)
  pc1 <- res.pca$ind$coord[, 1] # indexing the first column
  pc2 <- res.pca$ind$coord[, 2]
  pc1_lab <- paste("PC1",round(eig.val[1,2],2),"%")
  pc2_lab <- paste("PC2",round(eig.val[2,2],2),"%")

  gind <- ggplot(data = data, aes(x = pc1, y = pc2, color = cluster, shape = cluster)) +

    geom_hline(yintercept = 0, lty = 2) +

    geom_vline(xintercept = 0, lty = 2) +

    geom_point(alpha = 0.8) +xlab(pc1_lab)+ylab(pc2_lab)


  gind2<-gind+geom_text(label=rownames(data))

  list(var=gvar,ind=gind,ind2=gind2)

}

sortie_graph<-function(obj,type,profil="l"){
  if (type=="illus"){
    df_quali<-cbind(obj$data,grp=obj$grp)
    graph_uni_quali<-lapply(colnames(obj$illus) , graph_uni_quali,df=df_quali,clust=as.factor(df_quali$grp),profil=profil)
    names(graph_uni_quali)<-colnames(obj$illus)
    return(graph_uni_quali)
  }
  if (type=="act"){
    df_quanti<-cbind(obj$data,grp=obj$grp)
    graph_uni_quanti<-lapply(df_quanti[colnames(obj$act)] , graph_uni_quanti,df=df_quanti,clust=as.factor(df_quanti$grp))
    #names(graph_uni_quanti)<-obj$act
    return(graph_uni_quanti)
  }
  if (type=="acp"){
    # fun_acp(obj$data[obj$act],obj$data[obj$illus],obj$grp)
    col_quanti<-which(sapply(obj$data, is.numeric) ==T)
    #fun_acp(obj$data,obj$act,obj$illus,obj$grp)
    fun_acp(obj$data[,col_quanti],obj$grp)
  }
}



#--------------------------------------ADL-------------------------------------------------------
#Source: http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_LDA_MASS_R.pdf
adl.categorisation = function(obj){
  if(!is.categorisation(obj)){
    stop("L'argument obj n'est pas de type categorisation")
  }
  var_grp = obj$grp
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  col = colnames(obj$act) #nom des colonnes
  for (i in 1:ncol(obj$act)){
    var = col[i]
    if(is.numeric(obj$act[,var])){ #on recupere les variables quanti
      df_quanti[var] = obj$act[,var]
    }
  }
  col_quanti = colnames(df_quanti)
  if(length(col_quanti)==0){
    return(NULL)
  }

  my_data = cbind(obj$act[col_quanti], grp = obj$grp)
  split = sort(sample(nrow(my_data), nrow(my_data)*0.75))
  train = my_data[split, ]
  test = my_data[-split,]

  Xtrain = train[, -ncol(my_data)]
  ytrain = as.factor(train[, ncol(my_data)])
  Xtest = test[, -ncol(my_data)]
  ytest = as.factor(test[, ncol(my_data)])

  my_adl = lda(grp ~ ., data = train)
  coef_adl = my_adl$scaling
  pred = predict(my_adl, newdata = Xtest)
  mc = table(ytest, pred$class)

  p = ncol(Xtrain)
  n = nrow(train)
  k = nlevels(ytrain)

  #matrice de co-variance totale
  m_tot = cov(Xtrain)
  #matrice de co-variace intra: a chaque fois on enleve la variable que l'on cherche a evaluer
  m_intra = (1.0/(n-k))*Reduce("+",lapply(levels(ytrain),function(niveau){(sum(ytrain==niveau)-1)*cov(Xtrain[ytrain==niveau,])})) #reduce applique de maniere iterative une fonction

  WITprim = (n-k)/n*m_intra
  TOTprim = (n-1)/n*m_tot

  #lambda de Wilks
  LW = det(WITprim)/det(TOTprim)

  ddlSuppNum = k - 1
  ddlSuppDenom = n - k - p + 1
  FTest = numeric(p)
  pvalue = numeric(p)

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
  names(r) = c("Coef LDA", "confusion_matrix", "eval")
  return(r)
}


#--------------------Silhouette---------------------------------------
#Le coefficient de silhouette varie entre -1 (pire classification) et 1 (meilleure classification)
sil.categorisation = function(obj){
  if(!is.categorisation(obj)){
    stop("L'argument obj n'est pas de type categorisation")
  }

  var_grp = obj$grp #cluster
  my_data = obj$act
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  col = colnames(my_data) #nom des colonnes
  for (i in 1:ncol(my_data)){
    var = col[i]
    if(is.numeric(my_data[,var])){ #on recupere les variables quanti
      df_quanti[var] = my_data[,var]
    }
  }
  col_quanti = colnames(df_quanti)
  my_data = obj$act[col_quanti]

  m_dist = rdist(my_data) #matrice des distances
  m_cplt = cbind(m_dist, obj$grp) #matrice des distances avec les clusters
  sil = c()
  for (i in 1:length(var_grp)) { #pour chaque point
    cluster = var_grp[i]
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
    sil[i] = (bi-ai)/(max(bi,ai))
    if(is.na(sil[i])){
      sil[i] = 0
    }
  }
  out = data.frame(cbind(sil,var_grp))
  colnames(out) = c("sil", "cluster")
  sil_clust = tapply(out$sil, out$cluster, mean)
  sil_glob = mean(sil_clust)
  silhouette = list(sil_clust, sil_glob)

  p = as.data.frame(silhouette[[1]])
  p = cbind(p, as.numeric(rownames(p)))
  colnames(p) = c("sil", "cluster")

  graph = ggplot(data=p ,aes(x=sil, y=rownames(p))) +
    geom_bar(stat="identity", color = "black", fill = "steelblue") +
    xlim(-1, 1) +
    xlab("sil") + ylab("cluster") +
    theme_minimal()

  output = list(silclus = sil_clust, silglob = sil_glob, plot = graph)
  return(output)
}


#--------------------Davies Bouldin---------------------------------------
db.categorisation = function(obj){
  if(!is.categorisation(obj)){
    stop("L'argument obj n'est pas de type categorisation")
  }
  cluster = obj$grp
  n = length(cluster)
  k = length(unique(obj$grp))

  centre=matrix(nrow=k,ncol=ncol(obj$act)) #centroides
  for(i in 1:k){
    for(j in 1:ncol(obj$act)){
      centre[i,j]=mean(obj$act[cluster==i,j])
    }
  }
  s=c() #distance entre le point et le centroide
  for(i in 1:k){
    c=centre[i,]
    c_bis = sapply(c, rep, nrow(obj$act[obj$grp==i,]))
    s[i] = mean(sqrt(apply((obj$act[obj$grp==i,] - c_bis)^2,1,sum))^2)^(1/2)
  }

  M = rdist(centre) #distance entre centroides
  r = matrix(0,nrow=k, ncol = k)
  ri = c()
  for (i in 1:k){
    for (j in 1:k){
      r[i,j] = (s[i] + s[j])/M[i,j]
      if(r[i,j]==Inf | is.na(r[i,j]==Inf)){r[i,j]=0}
    }
    ri[i] = max(r[i,])
  }

  db = mean(ri)
  return(db)
}


#--------------------Test Antho---------------------------------------
setwd("/Volumes/KINGSTON/M2/Prog R/Projet/")
data<-read.csv2("master.csv",header=T,sep=";",dec=".")
str(data)
data2<-subset(data,annee==2012 & situation=="30 mois apres le diplome"& remarque=="",select=c(2,4,6:8,10,16:19,21,27))
data.reduite<- na.omit(data2) #on enleve les lignes avec valeurs manquantes
data.illu<- data.reduite[,c(1:6,12)]
d.classif<-data.reduite[,7:11]

res.kmeans<-kmeans(d.classif,centers=2,nstart=5)
res.kmeans<-kmeans(d.classif,centers=3,nstart=5)


#Cr?ation d'un objet en distinguant quali et quanti
objet<-categorisation(data.reduite,d.classif,data.illu,res.kmeans$cluster)

objet$act

objet$illus

objet$n_grp

objet$grp


#Test ACP
resacp<-sortie_graph(objet,"acp")
resacp$var
resacp$ind
resacp$ind2

#Exemple graphique uni variable illus pour chaque cluster
gr<-sortie_graph(objet,"illus")
#Affichage grapphique domaine/cluster
gr$code_du_domaine
gr$femmes

#Exemple graphique uni variable illus pour chaque cluster
gr<-sortie_graph(objet,"illus",profil = "c")
#Affichage grapphique domaine/cluster
gr$code_du_domaine
gr$femmes


#Exemple graphique uni variable active
gr2<-sortie_graph(objet,"act")
gr2

categouni<-univariee.categorisation(objet)
categouni$code_du_domaine
categouni$taux_dinsertion
categouni
#on ne peut pas utiliser $ --> y a t il une solution ?

#Valeur test
#$groupe passer en argument ????
sortievtest<-vtest.categorisation(objet)
sortievtest$num
sortievtest$graph

#plot_data_column = function (data, column,grp) {
# ggplot(data, aes_string(x = grp,fill = column)) +
#  geom_bar(position = "stack" ) +
# xlab(column)
#}

#dftry<-cbind(objet$data,grp=objet$grp)
#myplots <- lapply(objet$illus, plot_data_column, data = dftry,grp=as.factor(dftry$grp))
#names(myplots)<-objet$illus
#myplots$diplome


###################--------------------------------Test--------------------------------------------
#import des donn?es
datas = read_excel("auto.xlsx")
autos = datas[,2:8]
row.names(autos) = datas$Modele
autos.cr = scale(autos, center = T, scale = T)
d.autos = dist(autos.cr)
cah = hclust(d.autos, method="ward.D2")
groupes.cah <- cutree(cah, k=4)
my_clust = cbind(datas[,-1], groupes.cah)

cate = categorisation(my_clust, my_clust[,1:7], my_clust[,7:11], my_clust$groupes.cah)
print(cate)
univariee.categorisation(cate)
test = vtest.categorisation(cate)
test$num
test$graph
test = sortie_graph(cate, type="illus")
test$poids

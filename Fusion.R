setwd("C:/Users/Axelle/Desktop/M/03_SISE/01_PROGRAMMATION R/PROJET/ex")

#librairies 
library(readxl)
library(questionr)
library(ggplot2)
library("FactoMineR")
library("factoextra") 
library(ggfortify)
library(reshape2)



###################Constructeur
categorisation = function(df, var_act, var_illus, var_grp){
  #controles
  if(!is.data.frame(df)){
    stop("L'argument df n'est pas un data frame")
  }
  
  #creation de l'instance
  instance = list()
  #Données 
  instance$data = df
  #Variables actives
  if(length(data.frame(var_act)) == 1){rename.variable(data.frame(var_act),colnames(data.frame(var_act)) , "var_act")}
  #Variables illustratives
  if(length(data.frame(var_illus)) == 1){rename.variable(data.frame(var_illus),colnames(data.frame(var_illus)) , "var_illus")}
  #Nom des variables actives 
  instance$act = colnames(data.frame(var_act))
  #Nom des variables illustratives 
  instance$illus = colnames(data.frame(var_illus))
  #Label des clusters 
  instance$n_grp = sort(unique(var_grp))
  #Colonne des clusters
  instance$grp = var_grp
  
  class(instance) = "categorisation"
  
  return(instance)
}

#On surcharge la méthode print
print.categorisation = function(obj){
  cat("variables actives: ", obj$act, "\n") #CELLE LIGNE NE MARCHE PAS QUAND ON A UNE SEUL VARIABLE!!!!
  cat("variables illustratives: ", obj$illus, "\n") #CELLE LIGNE NE MARCHE PAS QUAND ON A UNE SEUL VARIABLE!!!!
  cat("Nombre de groupes: ", length(obj$n_grp), "\n")
}

#On surcharge la méthode is
is.categorisation = function(obj){
  return(class(obj) == "categorisation")
}

#----------------------------------Fonctions pour la caractérisation univariée--------------------------------------------
######AXELLE##############################################################


#########################################################################


#Rapport de correlation
#source: http://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf
correlation.categorisation = function(x,y){
  g = length(unique(y)) #nb groupes
  n = length(x) #nb obs
  moy = mean(x) #moyenne generale
  
  
  sct = sum((x-moy)^2) #variabilité totale
  ng = table(y) #effectifs conditionnels
  mg = tapply(x,y,mean) #moyennes conditionnelles
  #variabilité inter
  sce = sum(ng * (mg - moy)^2)
  
  #vecteur moyennes + rapport correlation
  cor = c(mg,100.0*sce/sct)
  #nommer les élements du vecteur
  names(cor) = c(paste("G",1:g),"% epl.")
  return(cor)
}

corr.categorisation = function(obj, var_grp){
  df_quanti = as.data.frame(obj$data[obj$var_grp])
  col = colnames(obj$data[obj$act]) #nom des colonnes
  for (i in 1:ncol(obj$data[obj$act])) { 
    var = col[i]
    if(is.numeric(obj$data[,var])){ #on recupere les variables quanti
      df_quanti[var] = obj$data[,var]
    }
  }
  col_quanti = colnames(df_quanti)
  return(sapply(obj$data[col_quanti],correlation.categorisation,y=var_grp))
}

#Test du khi2
#Verifier les sorties suivant quanti / quali ??? et regarder si des fonctions inutiles
calculs_uni = function(col, cluster){
  if (length(col)==length(cluster) && is.factor(col)){
    f<-length(levels(col))-1
    cl<-length(levels(as.factor(cluster)))-1
    denom<- n*min(c(f,cl))
    ddl<-f*cl
    test<-chisq.test(cluster,col)
    khival<-test$statistic
    cramer<-sqrt(khival/denom)
    p_value<-test$p.value
    # return(chisq.test(cluster,col))
    #return(cramer)
    # return(table(cluster,col))
    
    result = c(khival, ddl, p_value, cramer)
    names(result) = c("khi2","ddl","p_value", "cramer")
    return(result)
    
  }else if (length(col)==length(cluster)){
    g = length(unique(cluster)) #nb groupes
    n = length(col) #nb obs
    moy = mean(col) #moyenne generale
    
    
    sct = sum((col-moy)^2) #variabilité totale
    ng = table(cluster) #effectifs conditionnels
    mg = tapply(col,cluster,mean) #moyennes conditionnelles
    #variabilité inter
    sce = sum(ng * (mg - moy)^2)
    
    #vecteur moyennes + rapport correlation
    cor = c(mg,100.0*sce/sct)
    #nommer les élements du vecteur
    names(cor) = c(paste("G",1:g),"% epl.")
    return(cor)
  }
  
}


univariee.categorisation = function(obj){
  #sortie_uni = sapply(obj$data[obj$act], calculs_uni, cluster=obj$grp)
  sortie_uni = sapply(obj$data, calculs_uni, cluster=obj$grp)
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
vtest.categorisation = function(obj, var_grp){
  df_quanti = as.data.frame(obj$data[obj$var_grp]) 
  df_quali = as.data.frame(obj$data[obj$var_grp])
  rename.variable(data.frame(df_quali), colnames(data.frame(df_quali)), "var_grp")
  #print(df_quali)
  col = colnames(obj$data[obj$act]) #nom des colonnes
  #print(col)
  for (i in 1:ncol(obj$data[obj$act])) { 
    var = col[i]
    if(is.numeric(obj$data[,var])){ #on recupere les variables quanti
      df_quanti[var] = obj$data[,var]
    } else{
      df_quali[var] = obj$data[,var] #on recupere les variables quali
    }
  }
  col_quali = colnames(df_quali)
  vt_quali = sapply(obj$data[,col_quali], vtest_quali, y=var_grp) 
  col_quanti = colnames(df_quanti) 
  vt_quanti = sapply(obj$data[col_quanti], vtest_quanti, y=var_grp)
  vt = list(vt_quanti, vt_quali)
  
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

#ajouter cos2 contrib ...?????
fun_acp<-function(act,illu,cluster){
  #https://huboqiang.cn/2016/03/03/RscatterPlotPCA
  res.pca<-PCA(act, scale.unit = TRUE, ncp = 5, graph = F)
  #res.pca <- prcomp(act, scale. = TRUE)

  #par(mfrow = c(1,2))
  var <- get_pca_var(res.pca) #Creation d'une variable "var" avec tous les resultats concernants les variables
  gvar<-fviz_pca_var(res.pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel=T)
  
 ind <- get_pca_ind(res.pca) #Creation d'une variable "ind" avec tous les resultats concernants les variables
  gind<-fviz_pca_ind(res.pca, col.ind =cluster,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.ind.sup = "purple")#graphique des individus selon leurs coordonnees et contribution 
  #gvar<-autoplot(res.pca, data = act, colour = cluster)
  #gind<-  gvar<-autoplot(res.pca, data = act, colour = cluster, loadings=T)

  list(var=gvar,ind=gind)
  
}

graph_uni_quali<-function(df,col,clust){
  
  #bar <- ggplot (dfpack, aes(x = clust, fill = col)) 
  #bar + geom_bar (position = "stack") # précise que les ZAU sont "empilées"
  if (is.numeric(df[,col])){
    graph_uni_quanti(df,df[,col],clust)
  }else{
  ggplot(df, aes_string(x = clust,fill = col)) +
    geom_bar(position = "stack" ) +
    #xlab("Cluster")
      xlab(class(df[,col]))
  }
  
}

graph_uni_quanti<-function(df,col,clust){
  c<-as.character(names(col))
  #bar <- ggplot (dfpack, aes(x = clust, fill = col)) 
  #bar + geom_bar (position = "stack") # précise que les ZAU sont "empilées"
  ggplot(df, aes(clust,col)) +
    geom_boxplot()
  
}



sortie_graph<-function(obj,type){
  if (type=="illus"){
    df_quali<-cbind(obj$data,grp=obj$grp)
    graph_uni_quali<-lapply(obj$illus , graph_uni_quali,df=df_quali,clust=as.factor(df_quali$grp))
    names(graph_uni_quali)<-obj$illus
    return(graph_uni_quali)
  }
  if (type=="act"){
    df_quanti<-cbind(obj$data,grp=obj$grp)
    graph_uni_quanti<-lapply(df_quanti[obj$act] , graph_uni_quanti,df=df_quanti,clust=as.factor(df_quanti$grp))
    #names(graph_uni_quanti)<-obj$act
    return(graph_uni_quanti)
  }
  if (type=="acp"){
    fun_acp(obj$data[obj$act],obj$data[obj$illus],obj$grp)
  }
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


#Création d'un objet en distinguant quali et quanti 
objet<-categorisation(data.reduite,d.classif,data.illu,res.kmeans$cluster)

objet$act

objet$illus

objet$n_grp

objet$grp


#Test ACP
resacp<-sortie_graph(objet,"acp")
resacp$var
resacp$ind

#Exemple graphique uni variable illus pour chaque cluster 
gr<-sortie_graph(objet,"illus")
#Affichage grapphique domaine/cluster
gr$code_du_domaine
gr$femmes

#Exemple graphique uni variable active
gr2<-sortie_graph(objet,"act")
gr2

categouni<-univariee.categorisation(objet)
categouni
#on ne peut pas utiliser $ --> y a t il une solution ? 

#Valeur test 
#$groupe passer en argument ????
sortievtest<-vtest.categorisation(objet,objet$grp)
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
#import des données
datas = read_excel("auto.xlsx")
autos = datas[,2:8]
row.names(autos) = datas$Modele
autos.cr = scale(autos, center = T, scale = T)
d.autos = dist(autos.cr)
cah = hclust(d.autos, method="ward.D2")
groupes.cah <- cutree(cah, k=4)
my_clust = cbind(datas, groupes.cah)

cate = categorisation(my_clust, my_clust[,-12], NULL, my_clust$groupes.cah)
corr.categorisation(cate, my_clust$groupes.cah)
cramer.categorisation(cate, my_clust$groupes.cah)
vtest.categorisation(cate, my_clust$groupes.cah)



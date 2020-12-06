pkgname <- "charclust"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "charclust-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('charclust')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("charac_graph")
### * charac_graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: charac_graph
### Title: Plot some graphs about characterisation of clusters
### Aliases: charac_graph

### ** Examples

## Not run: 
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D #graph for active variables
##D g_act <- charac_graph(obj, type = "act")
##D print(g_act)
##D print(g_act$taux_dinsertion)
##D #graph for illustrative variables, distribution foreach cluster
##D g_illus <- charac_graph(obj, type = "illus", profile = "l")
##D print(g_illus)
##D print(g_illus$domaine)
##D #' #graph for illustrative variables, distribution of clusters foreach modality
##D g_illus2 <- charac_graph(obj, type = "illus", profile = "c")
##D print(g_illus2)
##D print(g_illus2$domaine)
##D #PCA graph
##D g_pca <-  charac_graph(obj, type = "pca")
##D g_pca
##D #Contribution of variables foreach components
##D g_pca$var
##D #Individuals and clusters in PCA chart
##D g_pca$ind
##D #Individuals and clusters in PCA chart with labels
##D g_pca$ind_lab
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("charac_graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("charac_uni")
### * charac_uni

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: charac_uni
### Title: Univariate characterization of classes resulting from a
###   clustering
### Aliases: charac_uni

### ** Examples

## Not run: 
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D uni = charac_uni(obj)
##D #show results for all variables
##D print(uni)
##D #example for the variable "domaine":
##D print(uni$domaine)
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("charac_uni", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("clustlda")
### * clustlda

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: clustlda
### Title: Linear Discriminant Analysis on clustering results
### Aliases: clustlda

### ** Examples

## Not run: 
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D adl = clustlda(obj)
##D #the coefficients of LDA model
##D print(adl$coef_LDA)
##D #the confusion matrix
##D print(adl$confusion_matrix)
##D #the model evaluation
##D print(adl$eval)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("clustlda", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("db_index")
### * db_index

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: db_index
### Title: Compute Davies Bouldin's index
### Aliases: db_index

### ** Examples

## Not run: 
##D X = c(10, 11, 12, 28)
##D Y = c(20, 23, 28, 15)
##D cl = c(1, 1, 1, 2)
##D data = as.data.frame(cbind(X, Y, cl))
##D colnames(data) = c("X", "Y", "cluster")
##D obj = categorisation(data, data[, -3], NULL, data$cluster)
##D db = db_index(obj)
##D print(db)
##D ------------------
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D db2 = db_index(obj2)
##D print(db2)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("db_index", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("objcharac")
### * objcharac

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: objcharac
### Title: objcharac
### Aliases: objcharac

### ** Examples

## Not run: 
##D library(charclust)
##D data(iris)
##D obj <- objcharac(iris, iris[,-5], NULL, iris$Species)
##D -----------------
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("objcharac", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sil")
### * sil

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sil
### Title: Compute silhouette information from clustering
### Aliases: sil

### ** Examples

## Not run: 
##D X = c(10, 11, 12, 28)
##D Y = c(20, 23, 28, 15)
##D cl = c(1, 1, 1, 2)
##D data = as.data.frame(cbind(X, Y, cl))
##D colnames(data) = c("X", "Y", "cluster")
##D obj = categorisation(data, data[, -3], NULL, data$cluster)
##D s = sil(obj)
##D s$silclus
##D s$silglob
##D s$plot
##D ------------------
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj2 <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D s2 = sil(obj2)
##D s2$silclus
##D s2$silglob
##D s2$plot
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sil", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("testval")
### * testval

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: testval
### Title: Test Value Indicator
### Aliases: testval

### ** Examples

## Not run: 
##D data(insertion_master)
##D data.illu <- insertion_master[,c(1:6,12)]
##D data.act <-insertion_master[,7:11]
##D res.kmeans<-kmeans(data.act,centers=3,nstart=5)
##D obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)
##D t = testval(obj)
##D #print all numerical results
##D print(t$num)
##D #plot radar chart of values test for numeric variables
##D t$graph
##D #print result for numeric variables
##D tquanti <- t$num[[1]]
##D print(tquanti)
##D #print result for one numeric variable
##D print(tquanti[,"taux_dinsertion"])
##D #print result for qualitative variables
##D tquali <- t$num[[2]]
##D print(tquali)
##D #print result for one qualitative variable
##D print(tquali$domaine)
##D 
##D  
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("testval", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

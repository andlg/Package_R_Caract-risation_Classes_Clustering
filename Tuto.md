
#TUTORIAL CHARCLUST PACKAGE #
-----
-----

##Introduction##
-----

The goal of this tutorial is to show how to use the charclust package and interpret the results. The dataset used is the “insertion_master” available in the package. It contains data on the occupational integration of master's graduates in France in 2012 (https://www.data.gouv.fr/fr/datasets/insertion-professionnelle-des-diplomes-de-master-en-universites-et-etablissements-assimil-0/).  
After performing an automatic classification algorithm on the data, we are going to evaluate the quality of the results and to describe the clusters got using the charclust package.
   
##Loading the package and data
-----
   We start by loading the package and the “insertion_master” data. Then we perform the k-means algorithm for our classification.


    library(devtools)
    
    install_github("andlg/Package_R_Caracterisation_Classes_Clustering/charclust")

    library(charclust)
    data(insertion_master)
    data.illu <- insertion_master[,c(1:6,12)]
    data.act <-insertion_master[,7:11]
    res.kmeans<-kmeans(data.act,centers=3,nstart=5)

##Object creation
-------------------
The objcharac function allows to build an object of type objcharac.  
This objet contains the result of the clustering and the elements that made it possible to build it.


    obj <- objcharac(insertion_master, data.act, data.illu, res.kmeans$cluster)

##Clustering evalutation
------------------------

###**Davies Bouldin's index**

The db_index function computes the Davies Bouldin's index which evaluates the clustering quality.  
The Davies Bouldin's index varies between 0 et infinity. A model with a low index is a model with a good separation of clusters. 


    db <- db_index(obj)
    print(db)

    ##  DB Index 
    ## 0.6033811

###**Silhouette coefficient**

The silhouette coefficient is another index which evaluate the clustering quality. This index varies between –1 and 1.  
The sil function allows to compute the total silhouette coefficient of the partition.


    silhouette <- sil(obj)
    print(silhouette$silglob)

    ## [1] 0.5930815

The global coefficient is close to 1 so we have a pretty good score.  
We can also compute the silhouette coefficient by group to compare their homogeneities.  
The groups with the strongest silhouette coefficients are the most homogeneous.

    print(silhouette$silclus)

    ##         1         2         3 
    ## 0.5475537 0.6226540 0.6090367

The most homogeneous groups seem to be groups 1 and 3.
We can also display the graph of the silhouette coefficients by group.

    silhouette$plot

![](Tuto_files/figure-markdown_strict/unnamed-chunk-5-1.png) 


##Univariate characterization
-------------------------

The package also allows you to characterize the different groups created by clustering.  We start by taking an interest on variables used for the creation of the groups and especially “taux_dinsertion” (occupational integration rate). 

    graph_uni_act <- charac_graph(obj, type = "act", profile = "l")
    graph_uni_act$taux_dinsertion

![](Tuto_files/figure-markdown_strict/unnamed-chunk-6-1.png) 

We see that the occupational integration rate seems higher in the cluster 2 and lower in the cluster 3 according to de median and the mean (cf. the cross). However, the range for the cluster 3 is high.  
  
  
Now, look at the illustrative variables and particularly the field of the master’s degree. 

    graph_uni_illus <- charac_graph(obj, type = "illus", profile = "l")
    graph_uni_illus$code_du_domaine

![](Tuto_files/figure-markdown_strict/unnamed-chunk-7-1.png)

We can see that the clusters 1 and 2 are mainly composed by the DEG (Law and Economics) and the STS (Science, Technology, Health) fields instead cluster 3 is mainly composed of SHS (Human and Social Sciences).  So, we can imagine that the occupational integration rate is better in the fields DEG and STS than in the other fields. 

    graph_uni_illus2 <- charac_graph(obj, type = "illus", profile = "c")
    graph_uni_illus2$code_du_domaine

![](Tuto_files/figure-markdown_strict/unnamed-chunk-8-1.png)

This second graph shows us that the masters’ in the fields LLA, MEEF (Teaching) and SHS are mainly in the cluster 3, so the cluster with the worst occupational integration rate.  
  
  We check numerical results to confirm our first feelings: 


    univariate = charac_uni(obj)
    print(univariate)

    ## $taux_dinsertion
    ##       G 1       G 2       G 3    % epl. 
    ## 92.759259 90.988095 88.824742  7.517836 
    ## 
    ## $emplois_cadre_ou_professions_intermediaires
    ##      G 1      G 2      G 3   % epl. 
    ## 93.68519 88.82738 81.07216 20.18582 
    ## 
    ## $emplois_stables
    ##      G 1      G 2      G 3   % epl. 
    ## 83.55556 79.77381 66.88660 26.96383 
    ## 
    ## $emplois_a_temps_plein
    ##      G 1      G 2      G 3   % epl. 
    ## 97.92593 97.36310 88.36598 31.14120 
    ## 
    ## $salaire_brut_annuel_estime
    ##         G 1         G 2         G 3      % epl. 
    ## 35322.22222 30773.21429 26122.16495    84.37342 
    ## 
    ## $diplome
    ##         khi2      p_value       cramer 
    ## 3.609294e+01 1.453840e-08 2.945537e-01 
    ## 
    ## $etablissement
    ##         khi2      p_value       cramer 
    ## 2.516395e+02 3.312681e-09 5.499557e-01 
    ## 
    ## $academie
    ##         khi2      p_value       cramer 
    ## 1.269567e+02 8.238328e-08 3.906306e-01 
    ## 
    ## $code_du_domaine
    ##         khi2      p_value       cramer 
    ## 1.488700e+02 3.373612e-28 4.230015e-01 
    ## 
    ## $domaine
    ##         khi2      p_value       cramer 
    ## 1.488700e+02 3.373612e-28 4.230015e-01 
    ## 
    ## $discipline
    ##         khi2      p_value       cramer 
    ## 2.022341e+02 8.239040e-26 4.930211e-01 
    ## 
    ## $femmes
    ##      G 1      G 2      G 3   % epl. 
    ## 47.12963 48.06548 70.73711 34.06873
    
Within the variables used for clustering, we see the “salaire_brut_annuel_estime” (annual estimated wage) has the large contribution (correlation coefficient = 0.84).  
  
  Within the illustrative variables the women rate is the most contributor (0.34). 


    print(univariate$taux_dinsertion)

    ##       G 1       G 2       G 3    % epl. 
    ## 92.759259 90.988095 88.824742  7.517836
    
The correlation between integration rate and groups is weak (0.075)


    print(univariate$code_du_domaine)

    ##         khi2      p_value       cramer 
    ## 1.488700e+02 3.373612e-28 4.230015e-01

The link between field and groups is weak/medium (0.42).  
  
  We look at the results giving by the test value to compare: 

    vtest = testval(obj)
    #print result for numeric variables
    tquanti <- vtest$num[[1]]
    print(tquanti)

    ##        taux_dinsertion emplois_cadre_ou_professions_intermediaires
    ## G 1 vt        3.902054                                    5.878595
    ## G 2 vt        2.539999                                    4.768985
    ## G 3 vt       -5.127112                                   -8.651083
    ##        emplois_stables emplois_a_temps_plein salaire_brut_annuel_estime
    ## G 1 vt        5.430371              4.506094                  13.793643
    ## G 2 vt        6.866863              8.449083                   7.572277
    ## G 3 vt      -10.412509            -11.346030                 -16.740760
    ##           femmes
    ## G 1 vt -4.570777
    ## G 2 vt -8.939882
    ## G 3 vt 11.872340

We obtain the same analysis that with the graph about integration rate. 

    vtest$graph

![](Tuto_files/figure-markdown_strict/unnamed-chunk-12-1.png)

We note the clusters 1 et 2 are similar in relation to numerical variables. They have higher test values for each one except for the woman rate. Indeed, the distinctive feature of the cluster 3 is his high women rate. Besides, the wage value test for the cluster 3 is really limited but the integration rate is not too bad (cf. Graph analysis). 

    #print result for qualitative variables
    tquali <- vtest$num[[2]]
    #print(tquali)
    #print result for one qualitative variable
    print(tquali$domaine)

    ##   Droit, economie et gestion Lettres, langues, arts Masters enseignement
    ## 1                   4.560795             -0.9749608            -1.691125
    ## 2                   3.996125             -0.9749608            -1.839795
    ## 3                  -4.730061              1.1156768             2.063950
    ##   Sciences humaines et sociales Sciences, technologies et sante
    ## 1                     -4.283108                        4.192541
    ## 2                     -3.250187                        3.730284
    ## 3                      4.006800                       -4.397345

The results match with the graph analysis. 

##Multivariate characterization
---------------------------

To complete the analysis, we are going to use multivariate methods to characterize the results of clustering.

###**Linear Discriminant Analysis**

The LDA methods can be used to characterize the results of clustering by considering the classes predicted by the classification algorithm used as the target variable.  
  
  The clustlda function compute the LDA method to explain and predict an individual's membership of a class.  
    
   The function returns the confusion matrix which compares the observed and predicted classes


    lda = clustlda(obj)
    #print(lda$coef_LDA)
    #the confusion matrix
    print(lda$confusion_matrix)

    ##      
    ## ytest  1  2  3
    ##     1 14  2  0
    ##     2  0 37  0
    ##     3  0  2 49

The LDA fairly well reproduces the constitution.  
  
  To evaluate the model clstlda function also returns the test statistic and the p-value which are used to test the contribution of the variables.

    #the model evaluation
    print(lda$eval)

    ##                                           var     FValue   pvalue
    ## 1                             taux_dinsertion   4.164302 0.016433
    ## 2 emplois_cadre_ou_professions_intermediaires   2.050221 0.130477
    ## 3                             emplois_stables   3.456044 0.032795
    ## 4                       emplois_a_temps_plein   9.104840 0.000144
    ## 5                  salaire_brut_annuel_estime 444.820354 0.000000

It seems the variables “emplois_a_temps_plein” and “emplois_stable” are not significant.
  
  ###**Principal Components Analysis**  
    
   The Principal Components Analysis is also a good way to interpret links between variables and to have a synthetic viewing. 



    pca <- charac_graph(obj, type = "pca")
    pca$var

![](Tuto_files/figure-markdown_strict/unnamed-chunk-14-1.png)

We note that the women rate and the integration rate are not correlated but on the opposite wage and women rate are negatively correlated. This assessment make sense with the characteristics of the cluster 3.  

    pca$ind

![](Tuto_files/figure-markdown_strict/unnamed-chunk-15-1.png)

Thanks to this chart we see again that the cluster 1 and 2 are close since we can’t be really separated in the space. The master’s degrees are not linked to a high wage (axis 1) but some of them can be associated with a good integration rate (axis 2).  
  
  Through this example we have pointed out a way to use our package and the results of different functions included inside. 



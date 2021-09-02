set.seed(1)

library(VIM)
library(class)
library(corrplot)
library(MASS)
library(FactoMineR)
library(stargazer)
library(ggplot2)
library(lmtest)
library(gplots)
library(factoextra)


siege<-read.table("Sieges_auto2.csv",header=T,sep=";")

summary(siege)
str(siege)

sapply(siege,sd)

par(mfrow=c(2,2))

hist(siege$Ventes,main="Répartition des ventes unitaires",
     xlab="Ventes unitaires",ylab="Effectifs",breaks=30,col="green")
hist(siege$Publicite,main="Répartition des bugdets publicitaires",
     xlab="Bugdets publicitaires",ylab="Effectifs",breaks=40,col="brown")
hist(siege$PrixConc,main="Répartition des prix pratiqué par le concurrent",
     xlab="Prix du concurrent",ylab="Effectifs",breaks=40,col="pink")
hist(siege$Prix,main="Répartition des prix fixés par l'entreprise",
     xlab="Prix fixé par l'entreprise",ylab="Effectifs",xlim=c(0,200),breaks=40,col="cyan")

par(mfrow=c(2,2))
plot(siege$Ventes~siege$Publicite,xlab="Publicité",
     ylab="Nombre des Ventes",pch=19,col="red")
title(main=paste("Nombre des Ventes en fonction","\n",sep=""),cex.main=1)
title(main=paste("\n","du budget publicitaire",sep=""),cex.main=1)

plot(siege$Ventes~siege$PrixConc,xlab="PrixConc",
     ylab="Nombre des Ventes",pch=19,col="yellow")
title(main=paste("Nombre des Ventes en fonction","\n",sep=""),cex.main=1)
title(main=paste("\n","des prix du concurrent",sep=""),cex.main=1)

plot(siege$Ventes~siege$Prix,xlab="Prix",
     ylab="Nombre des Ventes",pch=19,col="navy")
title(main=paste("Nombre des Ventes en fonction","\n",sep=""),cex.main=1)
title(main=paste("\n","des prix de l'entreprise",sep=""),cex.main=1)


plot(siege$Ventes~siege$Revenu,xlab="Revenu",
     ylab="Nombre des Ventes",pch=19,col="pink")
title(main=paste("Nombre des Ventes en fonction","\n",sep=""),cex.main=1)
title(main=paste("\n","des revenus",sep=""),cex.main=1)

par(mfrow=c(1,1))
plot(siege$Ventes~siege$AgePop,xlab="Âge de la population",
     ylab="Nombre des Ventes",pch=19,col="purple")
title(main=paste("Nombre des Ventes en fonction","\n",sep=""),cex.main=1)
title(main=paste("\n","de l'âge de la population",sep=""),cex.main=1)


par(mfrow=c(2,2))
plot(siege$Ventes~siege$USA,data=siege,xlab="USA",
     ylab="Effectifs",main="Les Ventes en fonction du lieu de vente",
     col="orange")

plot(siege$Ventes~siege$Urbain,data=siege,xlab="Urbain",
     ylab="Effectifs",main="Les Ventes en fonction du milieu de vente",
     col="brown")

plot(siege$Ventes~siege$QualEmp,data=siege,xlab="Qualité de l'emplacement"
     ,ylab="Effectifs",main="Les Ventes en fonction de l'emplacement",
     col="lightblue")

#Nous constatons que le niveau des ventes est plus élevé lorsquz le magasin est
#implanté aux USA contrairement à ceux qui sont en dehors des USA.

#Nous constatons que le niveau des ventes de sièges auto pour enfants est 
#légèrement plus élevé dans le milieu non urbain soit rural.


#Dans cette boxplot, nous voyons que la qualité de l'emplacement des étagères pour 
#les sièges auto pour enfants sur chaque site joue un rôle assez important sur
#les ventes; car lorsque la qualité est bonne,le niveau des ventes est élevé 
#tandis qu'il est bas lorsque la qualité est mauvaise et un peu élevé lorsque la
#qualité est moyenne.

par(mfrow=c(1,1))
ind.quant<-sapply(siege,is.numeric) #sapply permet de détecter les variables 
#numériques
ind.quant
boxplot(scale(siege[,ind.quant]),las=2,col="yellow")

#Matrice des corrélations
M<-cor(siege[,ind.quant])
corrplot(M,method="number",type="upper",diag=FALSE,tl.col="black",
         tl.cex=0.7,number.cex = 0.7)

#Nous avons construit notre matrice des corrélations en ne gardant que les
#variables quantitatives puisque les variables qualitatives ne sont pas prises
#en compte.

#Dans cette matrice des corrélations, nous voyons que le prix pratiqué par le
#concurrent à chaque emplacement "PrixConc" est fortement corrélé aux prix 
#fixés par l’entreprise pour les sièges auto pour enfants sur chaque site.
#**************************************************************************
#Les ventes unitaires (en milliers) à chaque emplacement sont fortement corrélées
#aux prix fixés par l'entreprise pour les sièges auto pour enfants sur chaque site.
#Cette forte corrélation s'explique par le fait que les ventes sont liées aux 
#prix fixés par une entreprise; ainsi plus les prix sont élevés, plus ou moins 
#les ventes le seront aussi. Et plus les prix sont faibles, plus ou moins les 
#ventes le seront. En d'autres termes, le niveau des prix a un impact assez
#important sur les ventes des produits.
#**************************************************************************
#Nous avons aussi une corrélation entre les ventes et la publicité; cette 
#corrélation s'explique par l'influence que peut avoir la publicité sur
#sur les consommateurs pour les inciter à acheter un bien et ainsi augmenter
#ou non le niveau des ventes d'un produit. Nous voyons que le revenu est corrélé 
#aux ventes, car un ménage qui a un revenu élévé sera prét à investir davantage
#pour obtenir un siège auto de "bonne" qualité pour son enfant.
#l'âge de la population. Mais également une corréaltion entre la taille de la
#population et la publicité.

#Réalisation d'une ACP
resacp<-PCA(siege,quali.sup =c(7,8,11))
library(explor)
explor(resacp)

dimdesc(resacp, axes=c(1,2))

fviz_pca_biplot(resacp, 
                # Individus
                geom.ind = "point",
                fill.ind = siege$USA, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "USA", color = "Contrib",
                                    alpha = "Contrib")
)

resacp1<-PCA(siege,quali.sup =c(7,8,11),ncp=5)
explor(resacp1)

#Classification hiérarchique
#Réalisation
rescah<-HCPC(resacp1,graph=FALSE,consol=FALSE)
rescah<-HCPC(resacp1,graph=FALSE,consol=TRUE)
rescah<-HCPC(resacp1,nb.clust=-1 )

#Nous avons 3 groupes

plot(rescah,choice="tree",title="CAH données brutes")
plot(rescah,choice="tree",title="CAH données brutes",tree.barplot=FALSE)

plot(rescah,choice="bar")
plot(rescah,choice="bar",label="none") #comment les groupes se forment
plot(rescah,choice="bar",label="none",draw.tree=FALSE)
plot(rescah,choice="3D.map")

rescah

#Extraction sur les inerties intra,inter
rescah$desc.var$quanti.var #resultat
rescah$desc.var$quanti #resultat des speudo test

rescah$data.clust

rescah$desc.var

#Nous voyons que pour la modalité USA=OUI, 41.47% des magasins localisés aux USA
#sont dans la classe 1 et 83.59% des magasins de la classe 1 sont localisés aux
#USA. A contrario, pour la modalité USA=Non, seulement 14.78% des magasins
#localisés en dehors des USA sont la classe 1 et 16.4% des magasins de la classe
#1 sont localisés en dehors des USA.
#Pour la modalité QualEmp=Bon, 56.47% des étagères ont une bonne qualité
#d'emplacement dans la classe 1 et 37.5% des étagères de la classe 1 ont un bon
#emplacement. Pour ce qui est de la modalité QualEmp=Mauvais, seulement 19.79% 
#des étagères ont une mauvaise qualité d'emplacement dans la classe 1.

#Pour nos variables quantitatives, nous allons tenir compte de la "v.test", ainsi
#si un signe positif (resp. négatif) de ce v.test indique que les variables de nos classes
#prennent des valeurs supérieures (resp. inférieures) à leurs moyennes.
#Ainsi nous voyons que la variable "Ventes" a une v.test positive et la moyenne
#des ventes dans cette classe est plus grande que la moyenne des ventes dans
#toutes les classes. Et nous remarquons que dans cette classe nous retrouvons les
#autres variables telles quue "Publicité","Revenu" qui se retrouvaient dans la
#même dimension dans l'ACP. D'autre part nous avons la variable "Prix" qui a une
#v.test négative et une moyenne du prix dans la classe 1 inférieure à la 
#moyenne du prix dans l'ensemble de notre base des données. Et il en va de même 
#pour le prix fixé par l'entreprise ainsi que l'âge moyen de la population.


rescah$desc.var$category
rescah$desc.axes
#Les individus de la classe 1 dans la dimensions 2 ont des coordonnés plus élévés 
#que les autres.
#Dans la classe 2 se sont les individus situés dans la dimension 3 et dans la 
#classe 3 se sont les individus situés dans les dimensions 1 et 2.

rescah$desc.axes$quanti
rescah$desc.axes$quanti.var

rescah$desc.var

rescah$desc.ind #nous donne une description par les individus

rescah$desc.ind$para
rescah$desc.ind$dist


#Modèle linéaire
lm1<-lm(Ventes~., data=siege)
summary(lm1)
summary(lm3)
summary(lm4)

#Le coefficient de détermination a en effet diminué (de 0,8734 pour le modèle 
#linéaire à 0,872 pour le modèle quadratique) mais une augmentation du 
#coefficient de détermination est inévitable lorsqu’on augmente le nombre de 
#paramètres. La mesure d’Akaike tient compte du nombre de paramètres et montre 
#bien que le modèle quadratique n’est pas meilleur :
#car la valeur de l'AIC baisse lorsqu'on ajoute le terme quadratique, un indice 
#de qualité moindre. On n'a donc pas intérêt de l'inclure dans notre modèle.

lm2<-lm(Ventes~PrixConc+Prix+Revenu+Publicite+QualEmp+AgePop,data=siege)
lm3<-lm(Ventes~PrixConc+Prix+Revenu+Publicite+I(Publicite^2)+QualEmp+AgePop,
        data=siege)
summary(lm3)

stargazer(lm3,lm4,title="Modèle de régression linéaire multiple",type="text")

AIC(lm1)
AIC(lm2)
AIC(lm3)

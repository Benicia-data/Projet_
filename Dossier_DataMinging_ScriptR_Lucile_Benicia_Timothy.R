##################################### Script Dossier Data Mining ##################################
#################### Lucile Mulot, Bénicia  Ekuba Kabuiku et Timothy Hervier #####################
############################################ Mécen 1 ##############################################

library(VIM)
library(class)
library(corrplot)
library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(ada)
library(caret)
library(MASS)
library(doParallel)
library(ISLR)
library(FactoMineR)
library(explor)
library(ade4)
library(stargazer)
library(ggplot2)
library(missMDA)
library(lmtest)
library(boot)
library(ROCR)
library(rlang)
library(gplots)
library(factoextra)

data<-read.table("credit-consommation.csv",header=T,sep=";",na.strings=c("n/a","NA"),dec=".")

###Observation de la base :
#Notre base des données "Credit-consommation" contient 10000 observations et 41 variables.
#On commence par enlever des variables qui se présentent comme redondantes.
summary(data)
data<-data[,-c(8,9,10)]

#On additionne les 2 variables car subgrade représente le chiffre après la virgule de grade.
data$grade<-data$grade_num+data$sub_grade_num

#On additionne les recovery car ils representent la même chose.
data$recovery<-data$recoveries+data$collection_recovery_fee

#On enlève les variables que l'on vient de modifier et la 20ème car elle est présente en double.
data<-data[,-c(20,26,27,28,31,32)]

#Autres nettoyages dans la base : 
levels(data$is_inc_v)
levels(data$is_inc_v)[1]<-0 #not verified
levels(data$is_inc_v)[c(2,3)]<-1 #verified
levels(data$term)[1]<-0 #36 months
levels(data$term)[2]<-1 #60 months
levels(data$initial_list_status)[1]<-0 #F
levels(data$initial_list_status)[2]<-1 #W
data$total_rec_late_fee[data$total_rec_late_fee=="0"]<-0
data$total_rec_late_fee[data$total_rec_late_fee>0]<-1
#is_inc_v = 0 correspond à la modalité not verified et is_inc_v = 1 à la modalité verified.
#term = 0 correspond à 36 mois et term = 1 à 60 mois.
#initial_list_status = 0 correspond à la modalité F et W sinon. 
  
#Nous procédons ensuite au découpage de la base de données : 
set.seed(1)
Npop<-nrow(data)
taille.test<-Npop/3
test.ind<-sample(1:Npop,taille.test) 
appr.ind<-setdiff(1:Npop,test.ind)
appr<-data[appr.ind,]
test_1<-data[test.ind,]

#Observons les données manquantes sur la base appr.
sum(is.na(appr))
#On observe aucune donnée manquante dans cette base.

attach(test_1)
#Faisons de même pour la base test.
sum(is.na(test_1))
pred.test<-test_1[!is.na(test_1$BadLoan),-1]
pred.test_knn<-kNN(pred.test,k=10)
pred.test_knn<-pred.test_knn[,1:33]
test<-cbind(BadLoan,pred.test_knn)
sum(is.na(test))

#Nous allons créer des sous bases sans facteurs, contenant donc que des variables quantitatives.
apprnum<-appr[,-c(6,8,10,11,18)]
testnum<-test[,-c(6,8,10,11,18)]

#Nous réalisons une matrice des corrélations : 
M<-cor(apprnum[,-1])
corrplot(M,type="upper",diag=FALSE,tl.col ="black",tl.cex =0.7,number.cex =0.7)

#Nous allons désormais procéder à la réalisation d'une ACP qui permet d'explorer
#des données dites multivariées (données avec plusieurs variables). 
#Nous en réalisons une sur les données apprentissages numériques et une autre 
#sur la partie test.
resacp1<-PCA(apprnum,quali.sup=1,graph=FALSE)
resacp2<-PCA(testnum,quali.sup=1,graph=FALSE)

#On peut maintenant sélectionner les coordonnées des individus que l'on injecte respectivement dans apprnumbis et dans testnumbis.
apprnumbis<-resacp1$ind$coord
apprnumbis<-as.data.frame(apprnumbis)
BadLoan<-apprnum[,1]
DATA1<-cbind(apprnumbis,BadLoan)
  
testnumbis<-resacp2$ind$coord
testnumbis<-as.data.frame(testnumbis)
BadLoan<-testnum[,1]
DATA2<-cbind(testnumbis,BadLoan)

    
##Nous pouvons maintenant réaliser une analyse factorielle discriminante.
lda_mod<-lda(BadLoan~.,data=DATA1)
res_lda<-predict(lda_mod,newdata=DATA2)

#On cherche à construire une matrice de confusion.
Prediction<-res_lda$class 
Realite<-DATA2[,6]
Mat_lda<-table(Realite,Prediction)
Mat_lda
addmargins(Mat_lda)

sum(res_lda$posterior[,2]>0.5)
#Le nombre d'individus classés BadLoan est de 279 au seuil de 0.5%.

#Nous pouvons observer l'évolution de nos 3 types d'erreur en fonction de l'évolution du seuil sur ces graphiques : 
N<-sum(DATA1$BadLoan=="N")
P<-sum(DATA1$BadLoan=="Y")
Error<-NULL
ErrorI<-NULL
ErrorII<-NULL
for(i in 1:101){
  c<-(i-1)/200
  Prediction<-rep("N",3333)
  Prediction[res_lda$posterior[,2]>c]<-"Y"
  Error[i]<-sum(Prediction!=Realite)/3333
  ErrorI[i]<-sum((Prediction=="Y")&(Realite=="N"))/N
  ErrorII[i]<-sum((Prediction=="N")&(Realite=="Y"))/P
}
par(cex=0.7)
plot((0:100)/200,Error,type="l",xlim=c(0,0.5),ylim=c(0,1),
     ylab="Taux d'erreur",xlab="Seuil",main="Evolution des 3 types d'erreur")
par(new=T)
plot((0:100)/200,ErrorI,type="l",xlim=c(0,0.5),ylim=c(0,1),
     ylab="",xlab="",xaxt="n",yaxt="n",col="orange")
par(new=T)
plot((0:100)/200,ErrorII,type="l",xlim=c(0,0.5),ylim=c(0,1),
     ylab="",xlab="",xaxt="n",yaxt="n",col="blue",lty="dashed")
#On retrouve en orange la courbe de l'erreur I, en bleu celle de l'erreur II et en noir le troisième type d'erreur.

#Etant dans le cas du domaine bancaire, nous cherchons à diminuer le taux de faux négatifs. 
#C'est la raison pour laquelle nous baissons le seuil à 0.2.
  
class20<-rep("N",3333)
class20[res_lda$posterior[,2]>0.2]<-"Y"
MatCnf<-table(Realite,class20)
addmargins(MatCnf)

#Puisque que l'on définit 3 types d'erreurs à savoir : l'erreur global de classement, l'erreur de type I, l'erreur de type II, on peut obtenir ce tableau qui nous donnent ces différents taux ci-dessous:
round(prop.table(table(Realite,class20),margin=1)*100,1)


#On peut également obtenir une courbe ROC qui permet d'observer simultanemment l'évolution de la sensibilité en fonction de l'erreur de type I ainsi que le taux de faux positif.
#On obtient alors ce graphique : 
par(cex=0.7)
plot(c(ErrorI,0),c(1-ErrorII,0),main="Courbe Roc de l'évolution de la sensibilité en fonction des faux positifs",
     ylab="Taux de Vrai Positif-Sensibilite",
     xlab = "Taux de Faux positif",type="l",asp=1,col="red",xlim=c(0,1),ylim=c(0,1))
segments(0,0,0.5,1)

#Réalisons une courbe ROC pour la LDA : 
pred<-prediction(res_lda$posterior[,2],DATA2[,6])
roc_lda<-performance(pred,"tpr","fpr")
plot(roc_lda,asp=1,col="red",main="Courbe ROC de la lda")
segments(0,0,1,1)
perf<-performance(pred,"auc");perf@y.values[[1]]
#Le niveau de performance obtenu pour la lda est de 0.7867676.

  
#Nous pouvons comparer nos résultats en réalisant une analyse quadratique discriminante (QDA).
qda_mod<-qda(BadLoan~.,data=DATA1)
res_qda<-predict(qda_mod,newdata=DATA2)
Prediction_qda<-res_qda$class
Mat_qda<-table(Realite,Prediction_qda)
addmargins(Mat_qda)

#Matrice de confusion au seuil de 0.2 comme pour la lda.
class20_qda<-rep("N",3333)
class20_qda[res_qda$posterior[,2]>0.2]<-"Y"
MatCnf_qda<-table(Realite,class20_qda)
addmargins(MatCnf_qda)
  
addmargins(MatCnf)
#Comparaison des deux matrices de confusion

  
#Comparaison des deux courbes ROC
pred_qda<-prediction(res_qda$posterior[,2],DATA2[,6])
roc_qda<-performance(pred_qda,"tpr","fpr")
par(mfrow=c(1,2))
plot(roc_qda,main="Courbe ROC de la qda",asp=1,col="green")
segments(0,0,1,1)
plot(roc_lda,asp=1,col="red",main="Courbe ROC de la lda")
segments(0,0,1,1)
perf_qda<-performance(pred_qda,"auc");perf_qda@y.values[[1]]
#Le niveau de performance obtenu pour la qda est de 0.7859605.
#On obtient une performance similaire mais légérement meilleur pour la lda. 
  
#Réalisation de la prédiction avec la méthode des k plus proches voisins. Nous obtenons ainsi une matrice de confusion et le taux d'erreur.
knn_pred<-knn(apprnum[,-1],testnum[,-1],apprnum[,1],k=5)
Realite_knn<-testnum[,1]
addmargins(table(Realite_knn,knn_pred))
mean(testnum[,1]!=knn_pred)
#Notre taux d'erreur est de 0.1680168 % lorsque nous prenons un k=5.
 
#Nous pouvons mettre en place une boucle cherchant la valeur de 'k' qui permet de diminuer le taux d'erreur ainsi rendre plus performant notre modèle.
Errorknn<-NULL
for(i in 1:10){
  knn_pred<-knn(apprnum[,-1],testnum[,-1],apprnum[,1],k=i)
  Errorknn[i]<-mean(testnum[,1]!=knn_pred)
}
par(mfrow=c(1,1))
plot(Errorknn,type="b")
kbest<-which.min(Errorknn);kbest
knn_pred<-knn(apprnum[,-1],testnum[,-1],apprnum[,1],k=kbest)
addmargins(table(Realite_knn,knn_pred))
mean(testnum[,1]!=knn_pred)
#La valeur de 'k' qui permet de diminuer le taux d'erreur est 1.
#Ainsi notre taux d'erreur est passé de 0.1680168 à 0.160216%. 


##### Mise en place d'une regression "logistique" sur nos données d'apprentissage #####
glm1 = glm(BadLoan~.,data=DATA1, family=binomial(link=logit))
stargazer(glm1,title="Modèle de régression logistique",type="latex")

#Matrice de confusion
predict0<-predict(glm1,type = 'response',newdata=DATA2)
Matconf<-addmargins(table(DATA2$BadLoan,predict0>0.2))
Matconf

#ROC curve
pred0 <- prediction(predict0 ,DATA2$BadLoan)
roc <- performance(pred0,"tpr","fpr")
plot(roc,main="ROC Curve-logit model", col="red",lwd=2)
abline(0,1)
perfr<-performance(pred0,"auc");perfr@y.values[[1]]
#Le performance (aire sous la courbe ROC) du modèle logit est de 0.7862.
  

###On commence par faire une première réalisation d'arbre de décision. 
control.max<-rpart.control(cp=0,max.depth=0,minbucket=1,minsplit=1)
tree<-rpart(BadLoan~.,data=appr,control=control.max,parms=list(split="information"))
plot(tree)

#On peut ensuite réaliser un élagage de cet arbre pour l'améliorer. Pour cela, on cherche à obtenir des informations sur le paramètre de complexité par ces commandes :
plotcp(tree)
tree$cp

tree.tune<-tune.rpart(BadLoan~.,data=appr,parms=list(split="information"))
tree.tune$best.performance

#Une fois la valeur optimale du paramètre de compléxité choisie, il faut construire le nouvel arbre.
treebis<-rpart(BadLoan~. ,data=appr,control=rpart.control(cp=tree.tune$best.performance),parms=list(split="information"))
treebis

#On peut améliorer la représentation graphique avec le package rpart.plot et la fonction prp.
prp(treebis,type=0,extra=0,split.box.col="lightblue",cex=0.6)
prp(treebis,type=1,extra=1,split.box.col="lightblue",cex=0.6,main="Arbre de décision")

#Une fois l'arbre obtenu, nous pouvons réaliser une prévision et une estimation de l'erreur sur l'ensemble de test.
pred.tree<-predict(treebis, newdata=test[,-1],type="class")
Tab_arbre<-table(pred.tree,test[,1])
addmargins(Tab_arbre)
mean(pred.tree!=test[,1])
#Nous obtenons une erreur sur le test de 0,0120012.



############# RF ##############
rf<-randomForest(BadLoan~.,data=appr,method="class",
                 parms=list(split="gini"))

############ Bagging ##########
nvar<-ncol(appr)-1
bag<-randomForest(BadLoan~.,data=appr,method="class",
                  parms=list(split="gini"),mtry=nvar)

############ Représentation de l'erreur OOB ##########
plot(bag$err.rate[,1],type="l",ylim=c(0.001,0.03),
     xlab="nombre d'itérations",
     ylab="erreur",col="red")
lines(rf$err.rate[,1],col="blue")
legend("topright", legend = c( "Bagging","RF"), 
       col = c("red","blue"), pch = 15,bty = "n", pt.cex = 1, 
       cex = 0.8, horiz = FALSE, inset = c(0.1, 0.1))

############ Choix de mtry (nombre de variables) ########
rf.tune.mtry<-tune.randomForest(BadLoan~.,data=appr,
                                mtry=1:nvar)
rf.tune.mtry$best.parameters # On garde 20 variables
rf.tune.mtry$performances

############ Choix de profondeur en bagging #########

bag.tune.nodessize<-tune.randomForest(BadLoan~.,data=appr,
                                      mtry=nvar,nodesize=10:20)
#bag.tune.nodessize$performances
bag.tune.nodessize$best.parameters #On garde nodesize=10

########## Choix profondeur RF #########
rf.tune.nodessize<-tune.randomForest(BadLoan~.,data=appr,
                                     mtry=20,nodesize=10:20)

rf.tune.nodessize$best.parameters #On garde nodesize=13

###### Evolution du taux d'erreur en fonction du nombre d'arbres #####

plot(bag$err.rate[,1],type="l",ylim=c(0.001,0.035),xlab="nombre d'itérations",
     ylab="erreur",main="Evolution du taux d'erreur 
     en fonction du nombre d'arbres")
lines(rf$err.rate[,1],type="l",lwd=2,col="red")
legend("topright", legend = c( "Bagging","RF"), 
       col = c("black","red"), pch = 15,bty = "n", pt.cex = 1, 
       cex = 0.8, horiz = FALSE, inset = c(0.1, 0.1))

(minerr<-min(rf$err.rate[,"OOB"])) #minimum des erreurs OOB = 0.0052


############# Importance des variables ############
best.rf<-randomForest(BadLoan~.,data=appr,method="class",
                      mtry=20,nodesize=13,
                      parms=list(split="gini"),keep.forest=TRUE,
                      importance=TRUE)

#importance(best.rf)
varImpPlot(best.rf,main="Importance des variables Random Forest",cex=0.8)


best.bag<-randomForest(BadLoan~.,data=appr,method="class",
                       mtry=nvar,nodesize=10,
                       parms=list(split="gini"))
importance(best.bag)
varImpPlot(best.bag,main="Importance des variables Bagging",cex=0.8)

########## Boosting ##########
boost<-ada(BadLoan~.,data=appr,type="discrete",
           loss="exponential", 
           control=rpart.control(cp=0),iter=200,nu=1)
boost
############# Amélioration du boosting ##########
adaGrid<-expand.grid(maxdepth=c(1,2,5,10), iter=(1:10)*100,nu=c(0.01,0.1))
ctrlCv<-trainControl(method="repeatedcv",repeats=3)
#(parallelise le calcul)
detectCores(logical=FALSE)
nc<-detectCores(logical=TRUE)-1
registerDoParallel(cores=nc)

boosttune<-train(BadLoan~.,data=appr,method="ada",
                 trControl=ctrlCv,tuneGrid=adaGrid)

boosttune$bestTune #il faut mettre 1000 itérations, 10 de maxdepth et 0.01 en pénalisation

stopImplicitCluster()

best.boost<-ada(BadLoan~.,data=appr,type="discrete",loss="exponential", 
                control=rpart.control(maxdepth=10,cp=-1,minsplit=0,xval=0),
                iter=1000,nu=0.01)



#################################################################
#### Erreurs en test ############################################
#################################################################

###pour prédiction et erreur en test de la meilleur forêt aléatoire
pred.rfp<-predict(best.rf,test[,-1],type="prob");head(pred.rfp)
pred.rf<-predict(best.rf,test[,-1],type="class");head(pred.rf)
table(pred.rf,test[,1])
pred.bag<-(predict(best.bag,test[,-1],type="class")!=test[,1])
table(pred.bag,test[,1])
pred.boost<-(predict(best.boost,newdata=test)!=test[,1])
table(pred.boost,test[,1])
####### Synthèse ###### 
mean(pred.rf!=test[,1])
#l'erreur moyenne du baging est 0.00540054
mean(predict(best.bag,test[,-1],type="class")!=test[,1])
#l'erreur moyenne du baging est 0.00510051
mean(predict(best.boost,newdata=test)!=test[,1])
#l'erreur moyenne du boosting est 0.00390039
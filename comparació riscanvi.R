
#LLIBRERIES
library(readxl)
library(visdat)
library(dplyr)
library(tidyverse)
library(Rgraphviz)
library(dplyr)
library(gRain)
library(bnlearn)
library(arules)


#LECTURA DE LA BASE DE DADES)
data <- read_excel("C:/Users/ainaa/Desktop/UNI/4T GEA/TFG/bdd/Base_de_dades_Excarcerats_2014-2016.xlsx")
View(data)


#CREACIÓ DEL NOU DATASET:
names(data[,c("V2_TIPUSVIOLDELICTESPB", "V4_TIPUSORTIDA", "V5_INADAPTAT", "V7_TERRITORIFISCALIA", "V9_SEXE", "V11_ESTRANGERS", "V12_AREAGEOG", "V14_ESTUDIS", "V15_CATALA", "V16_CASTELLA","V17_EXPULSIO")]) #var personals
names(data[,64:89]) #var penitenciaries
df <- data[,c("V2_TIPUSVIOLDELICTESPB", "V4_TIPUSORTIDA", "V5_INADAPTAT", "V7_TERRITORIFISCALIA","V9_SEXE",
"V11_ESTRANGERS", "V12_AREAGEOG", "V14_ESTUDIS", "V15_CATALA", "V16_CASTELLA","V17_EXPULSIO")]
df <- data.frame(df, data[,64:89])
df <- data.frame(df, data[,c("V165_NINGRESPOSTERIOR", "V170_TEMPS_REINCIDIRCATEG", "V176_REINCIDENCIAEXECUCIOPENAL")])
names(df)
df <- df[, - c(32,34,36)] #treiem var V80, V82 i V84
View(df)
names(df) <- c("V2","V4","V5","V7","V9","V11","V12","V14","V15","V16","V17","V60","V61","V62","V63","V64","V65","V66","V67","V68","V69",                
"V70","V71","V72","V73","V74","V75","V76","V77","V78","V79","V81","V83", "V85","V165","V170","V176")

#agrupar V11 i V12
table(df$V11)
table(df$V12) #1(1)=Espanya, 4(2)=Magrib, 6(3)= centre i sud america, 8(4)=resta del mon

for (i in 1:length(df$V12)){
  if (df$V12[i]==1) {df$V12[i]=1} else
    if (df$V12[i]==4) {df$V12[i]=2} else
      if (df$V12[i]==6) {df$V12[i]=3} else
      {df$V12[i]=4}}
table(df$V12)
df <- df[,-6] #elimino V11

#recodificar V165 i V170

table(df$V165, useNA="always")
for (i in 1:length(df$V165)){
  if (df$V165[i]==0) {df$V165[i]=0} else
    if (df$V165[i]==1) {df$V165[i]=1} else
      {df$V165[i]=2}}
table(df$V165)
which(df$V176==1 & df$V165==0)

table(df$V170)
table(df$V176)
table(df$V170, df$V176, useNA = "always")
which(is.na(df$V170) & df$V176==1) #casos amb els que no se que fer
#95 132 138 224 280 294 296 326 453 659 (10 desconeguts)


for (i in 1:length(df$V170)){
  if (is.na(df$V170[i]) & df$V176[i]==1) {df$V170[i]="desc"} else
    if (!is.na(df$V170[i])){
    if (df$V170[i]==1) {df$V170[i]=1} else
      if (df$V170[i]==2) {df$V170[i]=2} else
        {df$V170[i]=3}}}
table(df$V170, useNA="always")
table(df$V176, df$V170)

#recod V5 (riscanvi) 
for (i in 1:length(df$V5)){
  if (is.na(df$V5[i])){df$V5[i] = 3} else
    if (df$V5[i]==2){df$V5[i]=0}
}
table(df$V5)


#ELIMINAR MISSINGS 
for (i in 1:length(df$V2)) {
  if (is.na(df$V2[i])){df$V2[i] = "desc" }
}

for (i in 1:length(df$V7)) {
  if (is.na(df$V7[i])){df$V7[i] = "desc"}
}

table(data$V14_ESTUDIS, data$V20_X18.NIVELLEDUCATIU, useNA = "always")
sum(is.na(df$V14))
for (i in 1:length(data$V14_ESTUDIS)) {
  if (is.na(df$V14[i]) & data$V20_X18.NIVELLEDUCATIU[i] == 1){df$V14[i]=1}
  if (is.na(df$V14[i]) & data$V20_X18.NIVELLEDUCATIU[i] == 2){df$V14[i]=2}
}


table(df$V15, df$V12, useNA = "always")
for (i in 1:length(df$V15)) {
  if (is.na(df$V15[i])){df$V15[i] = "desc"}
}

table(df$V16, df$V12, useNA = "always")
for (i in 1:length(df$V16)) {
  if (is.na(df$V16[i])){df$V16[i] = "desc"}
}

table(df$V17,df$V176, useNA = "always")
for (i in 1:length(df$V17)) {
  if (is.na(df$V17[i])){df$V17[i] = 2}
}


table(df$V60, df$V61, useNA = "always")
for (i in 1:length(df$V61)) {
  if (is.na(df$V61[i])){df$V61[i] = 0}
}
#elimino V60 perque es inecessaria = V61
df<- df[, -which(names(df) == "V60")]

table( df$V62, useNA = "always")
df[265,]
which(is.na(df$V62))
for (i in 1:length(df$V62)) {
  if (is.na(df$V62[i])){df$V62[i] = "desc"}
}

table( df$V63, useNA = "always")
df[is.na(df$V63),]
for (i in 1:length(df$V63)) {
  if (is.na(df$V63[i])){df$V63[i] = 3}
}

table( df$V64, useNA = "always")
for (i in 1:length(df$V64)) {
  if (is.na(df$V64[i])){df$V64[i] = "desc"}
}

table( df$V65, useNA = "always")
for (i in 1:length(df$V65)) {
  if (is.na(df$V65[i])){df$V65[i] = "desc"}
}

table( df$V66, useNA = "always")
for (i in 1:length(df$V66)) {
  if (is.na(df$V66[i])){df$V66[i] = "desc"}
}

table( df$V67, useNA = "always")
for (i in 1:length(df$V67)) {
  if (is.na(df$V67[i])){df$V67[i] = 4}
}

table( df$V68, useNA = "always")
for (i in 1:length(df$V68)) {
  if (is.na(df$V68[i])){df$V68[i] = 0}
}


table(df$V63, df$V71, useNA = "always")
for (i in 1:length(df$V71)) {
  if (is.na(df$V71[i])){df$V71[i] = "desc"}
}

table( df$V72,df$V73, useNA = "always")
for (i in 1:length(df$V72)) {
  if (is.na(df$V72[i])){df$V72[i] = 0}
}

table( df$V72,df$V73, useNA = "always")
for (i in 1:length(df$V73)) {
  if (is.na(df$V73[i])){df$V73[i] = 0}
}

table( df$V72,df$V74, useNA = "always")
table(df$V74, useNA = "always")
for (i in 1:length(df$V74)) {
  if (is.na(df$V74[i])){df$V74[i] = 0}
}


table( df$V76, useNA = "always")
for (i in 1:length(df$V76)) {
  if (is.na(df$V76[i])){df$V76[i] = 0}
  if (df$V76[i]== 2){df$V76[i]= 0}
}


table( df$V78, useNA = "always")
for (i in 1:length(df$V78)) {
  if (is.na(df$V78[i])){df$V78[i] = 0}
}

table( df$V79, useNA = "always")
for (i in 1:length(df$V79)) {
  if (is.na(df$V79[i])){df$V79[i] = 0}
}

table( df$V81, useNA = "always")
for (i in 1:length(df$V81)) {
  if (is.na(df$V81[i])){df$V81[i] = 0}
}

table( df$V83, useNA = "always")
for (i in 1:length(df$V83)) {
  if (is.na(df$V83[i])){df$V83[i] = 0}
}

table( df$V85, useNA = "always")
for (i in 1:length(df$V85)) {
  if (is.na(df$V85[i])){df$V85[i] = 0}
}

table( df$V165, useNA = "always")
table( df$V165, df$V176, useNA = "always")
#V165 no la predim perquè tè molts valors mal codificats, si reincidencia= 1 no pot ser que el nº ingressos = 0 o al reves
#eliminar V165
df <- df[, -which(names(df) == "V165")]


#eliminar casos en que V176=0 i V170 >= 1
table( df$V170, df$V176, useNA = "always")
which(df$V170!= 0 & df$V176==0)
df <- df[-which(df$V170!= 0 & df$V176==0), ]

#passem les variables booleanes a factor
for (i in 1:dim(df)[2]){df[[i]]<-as.factor(df[[i]])}


#K-FOLD CROSS VALIDATION K=5
library(caret)
library(tidyverse)
set.seed(1110) 

n <- nrow(df)/5
folds <- split(df,sample(rep(1:5,n)))

val1 <- folds$`1`
train1 <- rbind(folds$`2`,folds$`3`,folds$`4`,folds$`5`)

val2 <- folds$`2`
train2 <- rbind(folds$`1`,folds$`3`,folds$`4`,folds$`5`)

val3 <- folds$`3`
train3 <- rbind(folds$`1`,folds$`2`,folds$`4`,folds$`5`)

val4 <- folds$`4`
train4 <- rbind(folds$`1`,folds$`2`,folds$`3`,folds$`5`)

val5 <- folds$`5`
train5 <- rbind(folds$`1`,folds$`2`,folds$`3`,folds$`4`)
class(val5)
class(train5)



#LLIBRERIES PER TREBALLAR AMB LES XARXES BAYESIANES
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(gRbase)
library(graph)
library(e1071)
library(caret)

#FUNCIONS D'ENTRENAMENT DE LES XARXES I VALIDACIÓ 

xarxes<- function(train, whitelist, blacklist){
  xarxa <- hc(train,score="bic", whitelist=whitelist, blacklist=blacklist )
  plot.xarxa <- graphviz.plot(xarxa)
  xarxa.estimada <- bn.fit(xarxa, train ,method="mle")
  xarxa.grain <- suppressWarnings(as.grain(xarxa.estimada))
  logLik <- logLik(xarxa,train)
  BIC <- BIC(xarxa,train)
  list(xarxa=xarxa,plot=plot,xarxa.estimada=xarxa.estimada,xarxa.grain=xarxa.grain, BIC=BIC,logLik=logLik)
}


validacio <- function(xarxa, val, response) {
  prediccio <- NULL
  CL <- NULL
  prueba <- NULL
  distribucio <- NULL
  
  for (j in 1:nrow(val)) {
    if (is.numeric(predict(xarxa$xarxa.grain, response = response, val[j,], predictors = atributes, type = "dist")$pred[[1]][1, 1]) == FALSE) {
      prediccio[[j]] <- NA
      CL[[j]] <- 0
      distribucio[[j]] <- c(rep(0, 2))
    } else {
      prueba[[j]] <- predict(xarxa$xarxa.grain, response = response, val[j,], predictors = atributes, type = "dist")
      distribucio[[j]] <- prueba[[j]]$pred[[1]]
      
      # Obtener las categorías ordenadas por probabilidad descendente
      categorias_ordenadas <- order(distribucio[[j]], decreasing = TRUE)
      
      # Verificar si la predicción inicial es "desc"
      if (which.max(distribucio[[j]]) == 4) {
        if (length(which(distribucio[[j]][-4]==max(distribucio[[j]][-4])))>1){
          prediccio[[j]] <- sample(which(distribucio[[j]][-4]==max(distribucio[[j]][-4])),1)
          CL[[j]]<-max(distribucio[[j]][-4]) 
        }
        else {siguiente_categoria <- dimnames(distribucio[[j]])[[2]][categorias_ordenadas[2]]
        prediccio[[j]] <- siguiente_categoria
        CL[[j]]<-max(distribucio[[j]][-4]) 
        } } else {
          prediccio[[j]] <- dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
          CL[[j]] <- max(distribucio[[j]])   
        }
    }}
  return(prediccio)
}


#ATRIBUTS, WL, BL, PER FER LES XARXES
names(df)
atributes=colnames(df[-c(3,33:34)]) #menys V5 i les variables predictores V170 i V176

bl <- data.frame(from = atributes, to = rep(atributes, each = length(atributes)))  # black list
wl1 <- data.frame(from = rep("V176", length(atributes)), to = atributes)  # white list V176
wl2 <- data.frame(from = rep("V170", length(atributes)), to = atributes)  # white list 2 V170


#NAIVE BAYES
#XARXES VARIABLE V176 NB
xarxa1_nb <- xarxes(train1[,-c(3, 33)], wl1, bl)
xarxa2_nb <- xarxes(train2[,-c(3, 33)], wl1, bl)
xarxa3_nb <- xarxes(train3[,-c(3, 33)], wl1, bl) 
xarxa4_nb <- xarxes(train4[,-c(3, 33)], wl1, bl)
xarxa5_nb <- xarxes(train5[,-c(3, 33)], wl1, bl) 



#COMPARACIÓ(confusion matrix) XARXES ESCOLLIDES PER VARIABLES AMB LA VARIABLE V5 = RISCANVI
sum(df$V5==3)
val1_ris<- val1[val1$V5!=3,]
dim(val1)
dim(val1_ris)
cm_ris1 <- table(val1_ris$V5, val1_ris$V176)[-3,] #comparació ris vs V176
predict_naivebayes1 <-  validacio(xarxa1_nb, val1_ris, "V176") 
cm_pred1 <-table(unlist(predict_naivebayes1), val1_ris$V176) #comparació valors predits vs V176


val2_ris<- val2[val2$V5!=3,]
dim(val2)
dim(val2_ris)
cm_ris2 <- table(val2_ris$V5, val2_ris$V176)[-3,] #comparacio ris vs V176
predict_naivebayes2 <-  validacio(xarxa2_nb, val2_ris, "V176") 
cm_pred2 <- table(unlist(predict_naivebayes2), val2_ris$V176) #comparació valors predits vs V176

val3_ris<- val3[val3$V5!=3,]
dim(val3); dim(val3_ris)
cm_ris3 <- table(val3_ris$V5, val3_ris$V176)[-3,] #comparacio ris vs V176
predict_naivebayes3 <-  validacio(xarxa3_nb, val3_ris, "V176") 
cm_pred3 <- table(unlist(predict_naivebayes3), val3_ris$V176) #comparació valors predits vs V176

val3_ris<- val3[val3$V5!=3,]
dim(val3)
dim(val3_ris)
cm_ris3 <- table(val3_ris$V5, val3_ris$V176)[-3,] #comparacio ris vs V176
predict_naivebayes3 <-  validacio(xarxa3_nb, val3_ris, "V176") 
cm_pred3 <- table(unlist(predict_naivebayes3), val3_ris$V176) #comparació valors predits vs V176

val4_ris<- val4[val4$V5!=3,]
dim(val4)
dim(val4_ris)
cm_ris4 <- table(val4_ris$V5, val4_ris$V176)[-3,] #comparacio ris vs V176
predict_naivebayes4 <-  validacio(xarxa4_nb, val4_ris, "V176") 
cm_pred4 <- table(unlist(predict_naivebayes4), val4_ris$V176) #comparació valors predits vs V176

val5_ris<- val5[val5$V5!=3,]
dim(val5);dim(val5_ris)
cm_ris5 <- table(val5_ris$V5, val5_ris$V176)[-3,] #comparacio ris vs V176
predict_naivebayes5 <-  validacio(xarxa5_nb, val5_ris, "V176") 
cm_pred5 <- table(unlist(predict_naivebayes5), val5_ris$V176) #comparació valors predits vs V176


#MATRIUS DE CONFUSIÓ DINS DE LLISTES PER VARIABLES 
matrius_pred <- list("naive bayes 1" = cm_pred1, 
                     "naive bayes 2" = cm_pred2, 
                     "naive bayes 3" = cm_pred3, 
                     "naive bayes 4" = cm_pred4, 
                     "naive bayes 5" = cm_pred5)

matrius_ris <- list( "riscanvi 1" = cm_ris1 ,
                     "riscanvi 2" = cm_ris2 ,
                     "riscanvi 3" = cm_ris3 ,
                     "riscanvi 4" = cm_ris4 ,
                     "riscanvi 5" = cm_ris5 )



#METRIQUES DE VALIDACIÓ 

#ACCURACY 
calculate_accuracy <- function(confusion_matrix) {
  correct_predictions <- sum(diag(confusion_matrix))
  total_predictions <- sum(confusion_matrix)
  accuracy <- correct_predictions / total_predictions
  
  accuracy
}

accuracy_pred <- list()
for (key in names(matrius_pred)) {
  confusion_matrix <- matrius_pred[[key]]
  accuracy <- calculate_accuracy(confusion_matrix)
  accuracy_pred[[key]] <- accuracy
}
print(accuracy_pred)
accuracy_pred <- unlist(accuracy_pred)

accuracy_ris <- list()
for (key in names(matrius_ris)) {
  confusion_matrix <- matrius_ris[[key]]
  accuracy <- calculate_accuracy(confusion_matrix)
  accuracy_ris[[key]] <- accuracy
}
print(accuracy_ris)
accuracy_ris <- unlist(accuracy_ris)


#MCC 
calculate_mcc <- function(confusion_matrix) {
  tp <- confusion_matrix[2, 2]  # Verdaderos positivos
  tn <- confusion_matrix[1, 1]  # Verdaderos negativos
  fp <- confusion_matrix[2, 1]  # Falsos positivos
  fn <- confusion_matrix[1, 2]  # Falsos negativos
  
  numerator <- (tp * tn) - (fp * fn)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- numerator / denominator
  mcc
}

mcc_pred <- list()
for (key in names(matrius_pred)) {
  confusion_matrix <- matrius_pred[[key]]
  mcc <- calculate_mcc(confusion_matrix)
  mcc_pred[[key]] <- mcc
}
print(mcc_pred)
mcc_pred <- unlist(mcc_pred)

mcc_ris <- list()
for (key in names(matrius_ris)) {
  confusion_matrix <- matrius_ris[[key]]
  mcc <- calculate_mcc(confusion_matrix)
  mcc_ris[[key]] <- mcc
}
print(mcc_ris)
mcc_ris <- unlist(mcc_ris)

#F1-SCORE 
calculate_f1_score <- function(confusion_matrix) {
  tp <- confusion_matrix[2, 2]  # Verdaderos positivos
  tn <- confusion_matrix[1, 1]  # Verdaderos negativos
  fp <- confusion_matrix[2, 1]  # Falsos positivos
  fn <- confusion_matrix[1, 2]  # Falsos negativos
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  f1_score
}

f1_pred <- list()
for (key in names(matrius_pred)) {
  confusion_matrix <- matrius_pred[[key]]
  f1_score <- calculate_f1_score(confusion_matrix)
  f1_pred[[key]] <- f1_score
}
print(f1_pred)
f1_pred <- unlist(f1_pred)

f1_ris <- list()
for (key in names(matrius_ris)) {
  confusion_matrix <- matrius_ris[[key]]
  f1_score <- calculate_f1_score(confusion_matrix)
  f1_ris[[key]] <- f1_score
}
print(f1_ris)
f1_ris <- unlist(f1_ris)


#TEST HIPOTESIS COMPARACIÓ XARXA NB I RISCANVI 


shapiro.test(accuracy_pred-accuracy_ris)
mean(accuracy_pred);mean(accuracy_ris)
t.test(accuracy_pred,accuracy_ris, paired = TRUE, alternative = "greater")
#pvalor significatiu, millor la nostra predicció, mitjana d'acc major estadisticament significativa


shapiro.test(mcc_pred-mcc_ris)
mean(mcc_pred);mean(mcc_ris)
t.test(mcc_pred,mcc_ris, paired = TRUE, alternative = "greater")
#pvalor no significatiu, no hi ha diferencies entre els 2

shapiro.test(f1_pred-f1_ris)
mean(f1_pred);mean(f1_ris)
t.test(f1_pred,f1_ris, paired = TRUE, alternative = "greater")
#pvalor no significatiu, no hi ha diferencies entre els 2


#ALTERNATIVA COMBINACIÓ XARXA I RIS

calculate_combinacio <- function(predict_naivebayes, val_ris) {
  predict_combinacio <- vector()
  for (j in 1:nrow(val_ris)){
    if (predict_naivebayes[j]=="1" | val_ris$V5[j]=="1"){
      predict_combinacio[j] <- "1"
    } else {
      predict_combinacio[j] <- "0"
    }
  } 
  cm_combinacio <- table(unlist(predict_combinacio), val_ris$V176) # comparació combinació vs V176
  comb <- list ("matriu de confusió" = cm_combinacio,
                "accuracy" = calculate_accuracy(cm_combinacio),
                "MCC" = calculate_mcc(cm_combinacio),
                "F1 SCORE" = calculate_f1_score(cm_combinacio) )
}

combinacio1 <- calculate_combinacio(predict_naivebayes1, val1_ris)
combinacio2 <- calculate_combinacio(predict_naivebayes2, val2_ris)
combinacio3 <- calculate_combinacio(predict_naivebayes3, val3_ris)
combinacio4 <- calculate_combinacio(predict_naivebayes4, val4_ris)
combinacio5 <- calculate_combinacio(predict_naivebayes5, val5_ris)

#comparació accuracy
acc_comb <- unlist(c(combinacio1[2],combinacio2[2],combinacio3[2],combinacio4[2],combinacio5[2]))
shapiro.test(accuracy_pred - acc_comb )
mean(accuracy_pred);mean(acc_comb)
t.test(accuracy_pred,acc_comb, paired = T, alternative = "greater")
#millor accuracy prediccions sense combinació 

shapiro.test(accuracy_ris - acc_comb )
mean(accuracy_ris);mean(acc_comb)
t.test(accuracy_ris,acc_comb, paired = T, alternative = "greater")
#millor ris 

#comparació mcc
mcc_comb <- unlist(c(combinacio1[3],combinacio2[3],combinacio3[3],combinacio4[3],combinacio5[3]))
shapiro.test(mcc_pred - mcc_comb )
mean(mcc_pred);mean(mcc_comb)
t.test(mcc_pred,mcc_comb, paired = T, alternative = "greater")
#no hi ha dif significatives

shapiro.test(mcc_ris - mcc_comb )
mean(mcc_ris);mean(mcc_comb)
t.test(mcc_ris,mcc_comb, paired = T, alternative = "less")
#no hi ha dif


#comparació f1score
f1_comb <- unlist(c(combinacio1[4],combinacio2[4],combinacio3[4],combinacio4[4],combinacio5[4]))
shapiro.test(f1_pred - f1_comb ) 
mean(f1_pred);mean(f1_comb)
t.test(f1_pred,f1_comb, paired = T, alternative = "less")
#no hi ha dif significatives

shapiro.test(f1_ris - f1_comb )
mean(f1_ris);mean(f1_comb)
t.test(f1_ris,f1_comb, paired = T, alternative = "less")
#millor combinacio


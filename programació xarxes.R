
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

#DESCRIPCCIÓ 1 DE LES DADES: 

sum(complete.cases(data))
colSums(is.na(data))
glimpse(data)


summary(data[1:9])
vis_miss(data[1:9])
table(data$V5_INADAPTAT) #valoració del RisCanvi: 392 = inadaptat / 674 = adaptat / 6 = na's

summary(data[10:47]) #variables personals
vis_miss(data[10:47])

summary(data[48:62]) # variables penals 
vis_miss(data[48:62])
table(data$V48_DELICTEPBVIOL, data$V51_X1.DELICTEVIOLENT3)
table(data$V48_DELICTEPBVIOL)

summary(data[63:97]) #variables penitencaries 
vis_miss(data[63:97])



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

#DESCRIPCIÓ DE LA NOVA BDD: 
glimpse(df)
vis_miss(df)
round(colSums(is.na(df))/length(df$V2)*100, 3) #variables amb més na's: V2, V15, V16, V61
apply(df,2,table, useNA = "always")
str(df)


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

#eliminar V61 perque te massa NA's ?? (no caldria perque els missings son que no hi ha cap ingres, com s'observa a la V60)
table(df$V61, useNA="always")
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


#mirem si hi ha missings un altre cop i anàlisi descriptiva
glimpse(df)
vis_miss(df)
round(colSums(is.na(df))/length(df$V2)*100, 3) 
summary(df)

barplot(table(df$V176),main="Reincidència expresoners", col=c("coral","#98F5FF"))
legend(x = "topright", legend = c("0 = no reincideix", "1 = sí reincideix"),fill = c("coral","#98F5FF"))
prop.table(table(df$V176))
barplot(table(df$V170),main="Temps fins a la reincidència", col="#98F5FF")
legend(x = "topright", legend = c("1 = fins a 1 any", "2 = de 1 a 2 anys ", "3 = 2 anys o més"))
prop.table(table(df$V170))
barplot(table(df$V5),main="Reincidència expresoners", col=c("coral","#98F5FF","pink"))
legend(x = "topright", legend = c("0 = no reincideix", "1 = sí reincideix", "3 = no codificat"), fill=c("coral","#98F5FF","pink"))
prop.table(table(df$V5))

prop.table(table(df$V9,df$V176))
prop.table(table(df$V9)) #1 = home, 2 =dona
prop.table(table(df$V9, df$V176), margin = 1)


tabla1 <- table(df$V2,df$V176)
plot(tabla1, col = c("#FF7256", "#FFF8DC"), main = "reincidencia vs. tipologia violenta" )
legend(x = "bottomright", legend = c("No", "Sí"), fill = c("#FF7256", "#FFF8DC"), 
       title = "reincideincia")


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
dim(val5)


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
      if (response=="V176"){distribucio[[j]] <- c(rep(0, 2))}
      if (response=="V170"){distribucio[[j]] <- c(rep(0, 3))}
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
#XARXES I VALIDACIÓ VARIABLE V176 NB
xarxa1_nb <- xarxes(train1[,-c(3, 33)], wl1, bl)
xarxa2_nb <- xarxes(train2[,-c(3, 33)], wl1, bl)
xarxa3_nb <- xarxes(train3[,-c(3, 33)], wl1, bl) 
xarxa4_nb <- xarxes(train4[,-c(3, 33)], wl1, bl)
xarxa5_nb <- xarxes(train5[,-c(3, 33)], wl1, bl) 

predict_naivebayes1 <-  validacio(xarxa1_nb, val1, "V176") 
predict_naivebayes2 <-  validacio(xarxa2_nb, val2, "V176") 
predict_naivebayes3 <-  validacio(xarxa3_nb, val3, "V176") 
predict_naivebayes4 <-  validacio(xarxa4_nb, val4, "V176") 
predict_naivebayes5 <-  validacio(xarxa5_nb, val5, "V176") 

conf_matrix_nb1 <- as.matrix(table(unlist(predict_naivebayes1), val1$V176))
conf_matrix_nb2 <- as.matrix(table(unlist(predict_naivebayes2), val2$V176))
conf_matrix_nb3 <- as.matrix(table(unlist(predict_naivebayes3), val3$V176))
conf_matrix_nb4 <- as.matrix(table(unlist(predict_naivebayes4), val4$V176))
conf_matrix_nb5 <- as.matrix(table(unlist(predict_naivebayes5), val5$V176))



#XARXES I VALIDACIÓ VARIABLE V170 NB

xarxa1nb_V170 <- xarxes(train1[!is.na(train1$V170),-c(3,34)], wl2, bl)
xarxa2nb_V170 <- xarxes(train2[!is.na(train2$V170),-c(3,34)], wl2, bl)
xarxa3nb_V170 <- xarxes(train3[!is.na(train3$V170),-c(3,34)], wl2, bl)
xarxa4nb_V170 <- xarxes(train4[!is.na(train4$V170),-c(3,34)], wl2, bl)
xarxa5nb_V170 <- xarxes(train5[!is.na(train5$V170),-c(3,34)], wl2, bl)

pred1nb_V170 <- validacio(xarxa1nb_V170, val1[!is.na(val1$V170),-c(3,34)], "V170")
pred2nb_V170 <- validacio(xarxa2nb_V170, val2[!is.na(val2$V170),-c(3,34)], "V170")
pred3nb_V170 <- validacio(xarxa3nb_V170, val3[!is.na(val3$V170),-c(3,34)], "V170")
pred4nb_V170 <- validacio(xarxa4nb_V170, val4[!is.na(val4$V170),-c(3,34)], "V170")
pred5nb_V170 <- validacio(xarxa5nb_V170, val5[!is.na(val5$V170),-c(3,34)], "V170")

cm1_nb_170<- as.matrix(table(unlist(pred1nb_V170), val1$V170[!is.na(val1$V170)])); cm1_nb_170 <- cm1_nb_170[,-4]
cm2_nb_170<- as.matrix(table(unlist(pred2nb_V170), val2$V170[!is.na(val2$V170)])); cm2_nb_170 <- cm2_nb_170[,-4]
cm3_nb_170<- as.matrix(table(unlist(pred3nb_V170), val3$V170[!is.na(val3$V170)])); cm3_nb_170 <- cm3_nb_170[,-4]
cm4_nb_170<- as.matrix(table(unlist(pred4nb_V170), val4$V170[!is.na(val4$V170)])); cm4_nb_170 <- cm4_nb_170[,-4]
cm5_nb_170<- as.matrix(table(unlist(pred5nb_V170), val5$V170[!is.na(val5$V170)])); cm5_nb_170 <- cm5_nb_170[,-4]


#AUGMENTED NAIVE BAYES 

##XARXES I VALIDACIÓ VARIABLE V176 ANB
xarxa1_anb <- xarxes(train1[,-c(3,33)], wl1, NULL); xarxa1_anb #trec V5 i V170
xarxa2_anb <- xarxes(train2[,-c(3,33)], wl1, NULL) 
xarxa3_anb <- xarxes(train3[,-c(3,33)], wl1, NULL)
xarxa4_anb <- xarxes(train4[,-c(3,33)], wl1, NULL)
xarxa5_anb <- xarxes(train5[,-c(3,33)], wl1, NULL)


pred1_anb<- validacio(xarxa1_anb, val1, "V176") 
pred2_anb<- validacio(xarxa2_anb, val2, "V176")
pred3_anb<- validacio(xarxa3_anb, val3, "V176")
pred4_anb<- validacio(xarxa4_anb, val4, "V176")
pred5_anb<- validacio(xarxa5_anb, val5, "V176")

conf_matrix_anb1 <- as.matrix(table(unlist(pred1_anb), val1$V176))
conf_matrix_anb2 <- as.matrix(table(unlist(pred2_anb), val2$V176))
conf_matrix_anb3 <- as.matrix(table(unlist(pred3_anb), val3$V176))
conf_matrix_anb4 <- as.matrix(table(unlist(pred4_anb), val4$V176))
conf_matrix_anb5 <- as.matrix(table(unlist(pred5_anb), val5$V176))


##XARXES I VALIDACIÓ VARIABLE V170 ANB
xarxa1anb_V170 <- xarxes(train1[!is.na(train1$V170),-c(3,34)], wl2, NULL)
xarxa2anb_V170 <- xarxes(train2[!is.na(train2$V170),-c(3,34)], wl2, NULL)
xarxa3anb_V170 <- xarxes(train3[!is.na(train3$V170),-c(3,34)], wl2, NULL)
xarxa4anb_V170 <- xarxes(train4[!is.na(train4$V170),-c(3,34)], wl2, NULL)
xarxa5anb_V170 <- xarxes(train5[!is.na(train5$V170),-c(3,34)], wl2, NULL)

pred1anb_V170 <- validacio(xarxa1anb_V170, val1[!is.na(val1$V170),-c(3,34)], "V170")
pred2anb_V170 <- validacio(xarxa2anb_V170, val2[!is.na(val2$V170),-c(3,34)], "V170")
pred3anb_V170 <- validacio(xarxa3anb_V170, val3[!is.na(val3$V170),-c(3,34)], "V170")
pred4anb_V170 <- validacio(xarxa4anb_V170, val4[!is.na(val4$V170),-c(3,34)], "V170")
pred5anb_V170 <- validacio(xarxa5anb_V170, val5[!is.na(val5$V170),-c(3,34)], "V170")

cm1_anb_170<- as.matrix(table(unlist(pred1anb_V170), val1$V170[!is.na(val1$V170)])); cm1_anb_170 <- cm1_anb_170[,-4]
cm2_anb_170<- as.matrix(table(unlist(pred2anb_V170), val2$V170[!is.na(val2$V170)])); cm2_anb_170 <- cm2_anb_170[,-4]
cm3_anb_170<- as.matrix(table(unlist(pred3anb_V170), val3$V170[!is.na(val3$V170)])); cm3_anb_170 <- cm3_anb_170[,-4]
cm4_anb_170<- as.matrix(table(unlist(pred4anb_V170), val4$V170[!is.na(val4$V170)])); cm4_anb_170 <- cm4_anb_170[,-4]
cm5_anb_170<- as.matrix(table(unlist(pred5anb_V170), val5$V170[!is.na(val5$V170)])); cm5_anb_170 <- cm5_anb_170[,-4]


#MATRIUS DE CONFUSIÓ DINS DE LLISTES PER VARIABLES 
matrius_176 <- list( "naive bayes 1" = conf_matrix_nb1, "augmented nb 1" = conf_matrix_anb1,
                     "naive bayes 2" = conf_matrix_nb2, "augmented nb 2" = conf_matrix_anb2,
                     "naive bayes 3" = conf_matrix_nb3, "augmented nb 3" = conf_matrix_anb3,
                     "naive bayes 4" = conf_matrix_nb4, "augmented nb 4" = conf_matrix_anb4,
                     "naive bayes 5" = conf_matrix_nb5, "augmented nb 5" = conf_matrix_anb5)

matrius_170 <- list( "naive bayes 1" = cm1_nb_170 , "augmented nb 1" = cm1_anb_170 ,
                     "naive bayes 2" = cm2_nb_170 , "augmented nb 2" = cm2_anb_170 ,
                     "naive bayes 3" = cm3_nb_170 , "augmented nb 3" = cm3_anb_170 ,
                     "naive bayes 4" = cm4_nb_170 , "augmented nb 4" = cm4_anb_170 ,
                     "naive bayes 5" = cm5_nb_170 , "augmented nb 5" = cm5_anb_170 )




#METRIQUES DE VALIDACIÓ 

#ACCURACY V176
calculate_accuracy <- function(confusion_matrix) {
  correct_predictions <- sum(diag(confusion_matrix))
  total_predictions <- sum(confusion_matrix)
  accuracy <- correct_predictions / total_predictions
  
  accuracy
}

accuracy_results <- list()
for (key in names(matrius_176)) {
  confusion_matrix <- matrius_176[[key]]
  accuracy <- calculate_accuracy(confusion_matrix)
  
  accuracy_results[[key]] <- accuracy
}
print(accuracy_results)


#MCC V176
calculate_mcc <- function(confusion_matrix) {
  tp <- confusion_matrix[2, 2]  # Verdaderos positivos
  tn <- confusion_matrix[1, 1]  # Verdaderos negativos
  fp <- confusion_matrix[2, 1]  # Falsos positivos
  fn <- confusion_matrix[1, 2]  # Falsos negativos
  
  numerator <- (tp * tn) - (fp * fn)
  denominator <- sqrt(as.numeric((tp + fp) * (tp + fn)) * as.numeric((tn + fp) * (tn + fn)))
  mcc <- numerator / denominator
  mcc
}

mcc_results <- list()

for (key in names(matrius_176)) {
  confusion_matrix <- matrius_176[[key]]
  mcc <- calculate_mcc(confusion_matrix)
  mcc_results[[key]] <- mcc
}

print(mcc_results)

#F1-SCORE V176
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

f1_score_results <- list()

for (key in names(matrius_176)) {
  confusion_matrix <- matrius_176[[key]]
  f1_score <- calculate_f1_score(confusion_matrix)
  f1_score_results[[key]] <- f1_score
}

print(f1_score_results)

#MAE V170
calculate_mae <- function(confusion_matrix) {
  suma <- 0
  for (i in 1:nrow(confusion_matrix)) {
    for (j in 1:ncol(confusion_matrix)) {
      suma <- suma + confusion_matrix[i,j]*abs(i-j)
    }
  }
  mae <- suma / sum(confusion_matrix)
  mae
}

mae_results_170 <- list()

for (key in names(matrius_170)) {
  confusion_matrix <- matrius_170[[key]]
  mae <- calculate_mae(confusion_matrix)
  
  mae_results_170[[key]] <- mae
}

print(mae_results_170)



#TESTS HIPOTESIS PER COMPARAR XARXES

acc_NB_V176 <- unlist(accuracy_results[ c(1,3,5,7,9)])
acc_ANB_V176 <- unlist(accuracy_results[ c(2,4,6,8,10)])
shapiro.test(acc_NB_V176-acc_ANB_V176)
mean(acc_NB_V176);mean(acc_ANB_V176)
t.test(acc_NB_V176,acc_ANB_V176, paired = TRUE, alternative = "less")

mcc_NB_V176 <- unlist(mcc_results[ c(1,3,5,7,9)])
mcc_ANB_V176 <- unlist(mcc_results[ c(2,4,6,8,10)])
shapiro.test(mcc_NB_V176-mcc_ANB_V176)
mean(mcc_NB_V176);mean(mcc_ANB_V176)
t.test(mcc_NB_V176,mcc_ANB_V176, paired = TRUE, alternative = "greater")


f1_NB_V176 <- unlist(f1_score_results[ c(1,3,5,7,9)])
f1_ANB_V176 <- unlist(f1_score_results[ c(2,4,6,8,10)])
shapiro.test(f1_NB_V176-f1_ANB_V176)
mean(f1_NB_V176);mean(f1_ANB_V176)
t.test(f1_NB_V176,f1_ANB_V176, paired = TRUE, alternative = "greater")

mae_NB_V170 <- unlist(mae_results_170[ c(1,3,5,7,9)])
mae_ANB_V170 <- unlist(mae_results_170[ c(2,4,6,8,10)])
shapiro.test(mae_NB_V170-mae_ANB_V170)
median(mae_NB_V170);median(mae_ANB_V170)
wilcox.test(mae_NB_V170, mae_ANB_V170, paired = T, alternative = "less") #pvalue en el limit de significació, millor nb que te mediana menor

#V176 naive bayes i V170 naive bayes

#XARXES FINALS (amb tota la base de dades)  I PREDICCIONS 

#V176 --> NAIVE BAYES
X176_NB <- xarxes(df[,-c(3,33)], wl1, bl); X176_NB


#prediccions, V2= 4(més d'una tipologia), V9= 2 (dona), V14= 1 (primaria), V61 = 1 (1 ingres ant), V69 = 1 (si)
xarxa.evid<-setEvidence(X176_NB$xarxa.grain, nodes=c("V2","V9", "V14", "V61", "V69"),
                        states=c("4","2", "1", "1", "1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("V176"),type="marginal")
distribucio<-qq$V176
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)

Resultat<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat=as.data.frame(Resultat)
colnames(Resultat)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
Resultat

#prediccions, V2= 2(violencia de genere), V9= 1 (home), V12= 1 (espanya),V14= 1 (primaria), V61 = 1 (1 ingres ant), V69 = 1 (si),V70= 1 (si expedients),V71= 1 (preso), V76 = 1 (si psiquiatria)
xarxa.evid<-setEvidence(X176_NB$xarxa.grain, nodes=c("V2","V9","V12", "V14", "V61", "V69", "V70", "V71","V76"),
                        states=c("2","1", "1","1", "1", "1", "1", "1", "1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("V176"),type="marginal")
distribucio<-qq$V176
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)

Resultat<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat=as.data.frame(Resultat)
colnames(Resultat)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
Resultat


#V170 -->  NB
X170_NB <- xarxes(df[!is.na(df$V170),-c(3,34)], wl2, bl); X170_NB

#prediccions,  V2= 2(violencia de genere), V9= 1 (home), V12= 1 (espanya),V14= 1 (primaria), V61 = 1 (1 ingres ant), V69 = 1 (si),V70= 1 (si expedients),V71= 1 (preso), V76 = 1 (si psiquiatria), V81= 0 (no tract)
xarxa.evid<-setEvidence(X170_NB$xarxa.grain, nodes=c("V2","V9","V12", "V14", "V61", "V69", "V70", "V71","V76", "V81"),
                        states=c("2","1", "1","1", "1", "1", "1", "1", "1", "0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("V170"),type="marginal")
distribucio<-qq$V170
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),3)


Resultat<-matrix(c(prediccio, CL),nrow=1)
Resultat=as.data.frame(Resultat)
colnames(Resultat)=c("Predicció","Confidence Level (CL) en %")
Resultat

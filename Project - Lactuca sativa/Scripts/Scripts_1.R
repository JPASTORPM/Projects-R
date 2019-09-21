############################################################
## Database by Orion                                      ##
## Date: 2017-10-12                                       ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Scripts by Junior Pastor Pérez-Molina                  ##
## Date: 2018-10-12                                       ##
############################################################


############################################################ OK
## Loading
############################################################
rm(list = ls()) #Remove all objects
graphics.off()  #Remove all graphics
cat("\014")     #Remove script in windows console
if(!grepl("Proyecto - Lechuga", getwd())){x= cat(prompt = "Please set the working directory to the project folder")}
############################################################


############################################################
## Install packages
############################################################
library(Rmisc)
library(car)
library(multcompView)
library(multcomp)
library(lsmeans)   #Source: http://rcompanion.org/rcompanion/d_08.html
library(car)
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.25, col=col)}
function.mean<-function(data, variable){
    sum = summarySE(data, measurevar= variable, groupvars=c("Tratamiento_2"), na.rm=TRUE)
    sum<-sum[c(1,3,5,6)]
    sum<-data.frame(sum)
    names(sum)<-c("Tratamiento","Mean","S.E.","C.I.95")
    sum[order(sum[,1], decreasing = TRUE),]
    sum$Tratamiento_2<-c("0%","25%","50%","75%","100%")
    sum
}
############################################################


############################################################ OK
## Loading data base
############################################################
Lechuga=read.delim("Datos/Lechuga.txt",header=T,sep="\t",dec=".")
str(Lechuga)
names(Lechuga)
Lechuga$Tratamiento_2<-as.character(Lechuga$Tratamiento)
Lechuga$Tratamiento_2[Lechuga$Tratamiento=="0%"]<-"1"
Lechuga$Tratamiento_2[Lechuga$Tratamiento=="25%"]<-"2"
Lechuga$Tratamiento_2[Lechuga$Tratamiento=="50%"]<-"3"
Lechuga$Tratamiento_2[Lechuga$Tratamiento=="75%"]<-"4"
Lechuga$Tratamiento_2[Lechuga$Tratamiento=="100%"]<-"5"
############################################################ clean database


############################################################
###########                c.hojas              ############
############################################################ OK
## ANOVA ó Kruskall-Wallis
############################################################
c.hojas <- aov(c.hojas ~ Tratamiento, data=Lechuga)
### NORMAL?
ifelse((shapiro.test(c.hojas$residuals))$p.value>0.05,"SI NORMALIDAD", "NO NORMALIDAD")
### BALANCIADOS?
balanciado<-tapply(Lechuga$c.hojas, Lechuga$Tratamiento, 
                   function(Lechuga){length(unique(Lechuga))})
balanciado
############################################################
## NORMAL y BALANCIADOS  -> ANOVA with a posteriori Tukey HSD
############################################################
c.hojas <- aov(c.hojas ~ Tratamiento, data=Lechuga)
summary(c.hojas)
Comparacion<-TukeyHSD(c.hojas)
plot(Comparacion)
############################################################
## NORMAL Y NO BALANCIADOS  -> ANOVA with a posteriori LSD
############################################################
c.hojas<-oneway.test (c.hojas ~ Tratamiento, data=Lechuga)
c.hojas
Comparacion<-pairwise.t.test (Lechuga$c.hojas, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################
## NO NORMAL -> Kruskall Wallis a posteriori
############################################################
kruskal.test (c.hojas ~ Tratamiento, data=Lechuga)
## BALANCIADOS o Muestras por tratamientos (n) > 6 o 20  -> Kruskall Wallis a posteriori Bonferroni
Comparacion<-pairwise.wilcox.test(Lechuga$c.hojas, Lechuga$Tratamiento, p.adj = "bonf", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
## NO BALANCIADOS o Muestras por tratamientos (n) < 6   -> Kruskall Wallis a posteriori LSD
Comparacion<-pairwise.wilcox.test (Lechuga$c.hojas, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################


############################################################
###########                AFS.cm2.mg           ############
############################################################ OK
## ANOVA ó Kruskall-Wallis
############################################################
AFS.cm2.mg <- aov(AFS.cm2.mg ~ Tratamiento, data=Lechuga)
### NORMAL?
ifelse((shapiro.test(AFS.cm2.mg$residuals))$p.value>0.05,"SI NORMALIDAD", "NO NORMALIDAD")
### BALANCIADOS?
balanciado<-tapply(Lechuga$AFS.cm2.mg, Lechuga$Tratamiento, length); ifelse(balanciado[1]-balanciado[2]==0,ifelse(balanciado[2]-balanciado[3]==0,ifelse(balanciado[3]-balanciado[4]==0,"Si balanciados", "No balanciados")))
############################################################
## NORMAL y BALANCIADOS  -> ANOVA with a posteriori Tukey HSD
############################################################
AFS.cm2.mg <- aov(AFS.cm2.mg ~ Tratamiento, data=Lechuga)
summary(AFS.cm2.mg)
Comparacion<-TukeyHSD(AFS.cm2.mg)
plot(Comparacion)
############################################################
## NORMAL Y NO BALANCIADOS  -> ANOVA with a posteriori LSD
############################################################
AFS.cm2.mg<-oneway.test (AFS.cm2.mg ~ Tratamiento, data=Lechuga)
AFS.cm2.mg
Comparacion<-pairwise.t.test (Lechuga$AFS.cm2.mg, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################
## NO NORMAL -> Kruskall Wallis a posteriori ¿?
############################################################
kruskal.test (AFS.cm2.mg ~ Tratamiento, data=Lechuga)
## BALANCIADOS o Muestras por tratamientos (n) > 6 o 20  -> Kruskall Wallis a posteriori Bonferroni
Comparacion<-pairwise.wilcox.test(Lechuga$AFS.cm2.mg, Lechuga$Tratamiento, p.adj = "bonferroni", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
## NO BALANCIADOS o Muestras por tratamientos (n) < 6   -> Kruskall Wallis a posteriori LSD
Comparacion<-pairwise.wilcox.test (Lechuga$AFS.cm2.mg, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################


############################################################
###########                raiz.total           ############
############################################################ OK
## ANOVA ó Kruskall-Wallis
############################################################
raiz.total <- aov(raiz.total ~ Tratamiento, data=Lechuga)
### NORMAL?
ifelse((shapiro.test(raiz.total$residuals))$p.value>0.05,"SI NORMALIDAD", "NO NORMALIDAD")
### BALANCIADOS?
balanciado<-tapply(Lechuga$raiz.total, Lechuga$Tratamiento, length); ifelse(balanciado[1]-balanciado[2]==0,ifelse(balanciado[2]-balanciado[3]==0,ifelse(balanciado[3]-balanciado[4]==0,"Si balanciados", "No balanciados")))
############################################################
## NORMAL y BALANCIADOS  -> ANOVA with a posteriori Tukey HSD
############################################################
raiz.total <- aov(raiz.total ~ Tratamiento, data=Lechuga)
summary(raiz.total)
Comparacion<-TukeyHSD(raiz.total)
plot(Comparacion)
############################################################
## NORMAL Y NO BALANCIADOS  -> ANOVA with a posteriori LSD
############################################################
raiz.total<-oneway.test (raiz.total ~ Tratamiento, data=Lechuga)
raiz.total
Comparacion<-pairwise.t.test (Lechuga$raiz.total, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################
## NO NORMAL -> Kruskall Wallis a posteriori ¿?
############################################################
kruskal.test (raiz.total ~ Tratamiento, data=Lechuga)
## BALANCIADOS o Muestras por tratamientos (n) > 6 o 20  -> Kruskall Wallis a posteriori Bonferroni
Comparacion<-pairwise.wilcox.test(Lechuga$raiz.total, Lechuga$Tratamiento, p.adj = "bonf", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
## NO BALANCIADOS o Muestras por tratamientos (n) < 6   -> Kruskall Wallis a posteriori LSD
Comparacion<-pairwise.wilcox.test (Lechuga$raiz.total, Lechuga$Tratamiento, p.adj = "none", exact= F)
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion
############################################################


############################################################
###########                Fig. c.hojas         ############
############################################################ OK
data.c.hojas<-function.mean(data=Lechuga, variable="c.hojas")
############################################################
jpeg(file = "Results/Fig. data.c.hojas.jpeg",
     width = 600, height = 500, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1.5,3,1,1))
############################################################
barplot(data.c.hojas$Mean, beside=T, col = c("gray"),border=c("black"),names.arg = ((as.character(data.c.hojas$Tratamiento_2))),legend.text=FALSE,ylim=c(0,7),ylab="Crecimiento número hojas (± E.E.)",main="")
error.bar.vertical(c(0.7, 1.9, 3.15, 4.3, 5.5),data.c.hojas$Mean,data.c.hojas$S.E., col="red")
box()
#######
dev.off()
############################################################


############################################################
###########                Fig. raiz.total         ############
############################################################ OK
data.raiz.total<-function.mean(data=Lechuga, variable="raiz.total")
############################################################
jpeg(file = "Results/Fig. data.raiz.total.jpeg",
     width = 600, height = 500, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1.5,3,1,1))
############################################################
barplot(data.raiz.total$Mean, beside=T, col = c("gray"),border=c("black"),names.arg = ((as.character(data.raiz.total$Tratamiento_2))),legend.text=FALSE,ylim=c(0,0.4),ylab="Partición BS raíz (± E.E.)",main="")
error.bar.vertical(c(0.7, 1.9, 3.15, 4.3, 5.5),data.raiz.total$Mean,data.raiz.total$S.E., col="red")
box()
#######
dev.off()
############################################################


############################################################
###########                Fig. tallo.total         ############
############################################################ OK
data.tallo.total<-function.mean(data=Lechuga, variable="tallo.total")
############################################################
jpeg(file = "Results/Fig. data.tallo.total.jpeg",
     width = 600, height = 500, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1.5,3,1,1))
############################################################
barplot(data.tallo.total$Mean, beside=T, col = c("gray"),border=c("black"),names.arg = ((as.character(data.tallo.total$Tratamiento_2))),legend.text=FALSE,ylim=c(0,0.2),ylab="Partición BS tallo (± E.E.)",main="")
error.bar.vertical(c(0.7, 1.9, 3.15, 4.3, 5.5),data.tallo.total$Mean,data.tallo.total$S.E., col="red")
box()
#######
dev.off()
############################################################


par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(1.5,3,1,1))
barplot(data.c.hojas$Mean, beside=T, col = c("gray"),border=c("black"),names.arg = ((as.character(data.c.hojas$Tratamiento_2))),legend.text=FALSE,ylim=c(0,7),ylab="Crecimiento número hojas (± E.E.)",main="")
error.bar.vertical(c(0.7, 1.9, 3.15, 4.3, 5.5),data.c.hojas$Mean,data.c.hojas$S.E., col="red")

box()



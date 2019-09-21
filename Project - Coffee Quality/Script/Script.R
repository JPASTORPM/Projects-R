############################################################
## Database by Prof. Edgard                               ##
## Date: 2017-07-29                                       ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Scripts by Junior Pastor Pérez-Molina                  ##
## Date: 2018-07-23                                       ##
############################################################



############################################################ OK
## Loading data base
############################################################
rm(list = ls()) #Remove all objects
graphics.off()  #Remove all graphics
cat("\014")     #Remove script in windows console
if(!grepl("Project_R_Edgar", getwd())){x= cat(prompt = "Please set the working directory to the project folder")}
############################################################
Database=read.delim("Data/Database_1.txt",header=T,sep="\t",dec=".")
Database$Period<-as.factor(Database$Period)
Database$Group<-paste(Database$Genotype,Database$Pollination, Database$Season,
                      Database$Period, sep="*")
str(Database)
############################################################
## Function
############################################################
fun.anova<-function(data, variable, var, name1){
    data.anova<-data.frame()
    library(Rmisc)
    library(car)
    library(multcompView)
    library(multcomp)
    library(lsmeans)   #Source: http://rcompanion.org/rcompanion/d_08.html
    sum = summarySE(data, measurevar= var, groupvars=c("Group"), na.rm=TRUE)
    sum<-sum[c(1,3,5,6)]
    sum<-data.frame(sum)
    names(sum)<-c("Genotype_Pollination_Season_Period","Mean","S.E.","C.I.95")
    data$Period<-as.factor(data$Period)
    model = lm(variable ~ Genotype + Pollination + Season + Period + Genotype*Pollination + Genotype*Pollination*Season +Genotype*Pollination*Season*Period, data= data)
    summary.model<-summary(model)
    P.value<-(pf(summary.model[[10]][1],summary.model[[10]][2],summary.model[[10]][3],lower.tail=F))
    R2<-round(summary.model$r.squared,2)
    F<-round(summary.model$fstatistic[1],2)
    coef<-Anova(model, type="II")
    P<-data.frame(round(coef$`Pr(>F)`,3))
    names(P)<-c("P")
    P<-data.frame(P)
    P$Factor<-c("Genotype","Pollination","Season","Period","Genotype x Pollination","Genotype x Pollination x Season","Genotype x Pollination x Season x Period","NA")
    P<-data.frame(P)
    lsm = lsmeans(model, pairwise ~ Genotype*Pollination*Season*Period, adjust="tukey")
    t1<-data.frame(cld(lsm, alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(4)])
    t2$"Genotype_Pollination_Season_Period"<-paste(t2$Genotype,t2$Pollination,t2$Season, t2$Period, sep="*")
    t2<-merge(t2, sum, by="Genotype_Pollination_Season_Period")
    t2$Genotype.<-P[1,1]
    t2$Pollination.<-P[2,1]
    t2$Season.<-P[3,1]
    t2$Period.<-P[4,1]
    t2$"Genotype x Pollination"<-P[5,1]
    t2$"Genotype x Pollination x Season"<-P[6,1]
    t2$"Genotype x Pollination x Season x Period"<-P[7,1]
    t2$F<-F
    t2$R2<-R2
    t2$P<-"<0.001"
    Sample<-data.frame(name1)
    names(Sample)<-c("Variable")
    data.anova <- rbind(data.anova,data.frame(Sample,t2))
    return(data.anova)
}
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.1, col=col)}
error.bar.horizontal<-function(x, y, se.x, col){arrows(x-se.x, y, x+se.x, y, code=3, angle=90, length=0.1, col=col)}
fun.boxplot<-function(data1, variable, data2,ylim, d, ylab){
    boxplot(variable ~ Genotype:Pollination:Season:Period, data = data1,
            boxwex = 0.5, col = c("blue", "white", "red", "white"), border=c("darkblue","blue","darkred", "red"),
            main = "",
            xlab = "IRR*Fruit", ylab = ylab,
            sep = "*", lex.order = TRUE, ylim = ylim, yaxs = "i")
    text(x=c(1:4), y=data2$Mean*d, data2$.group)
}
############################################################
Esp_Total<-fun.anova(data=Database, variable=Database$Esp_Total, var="Esp_Total", name1="Esp_Total")
Esp_Epi_Par_Col<-fun.anova(data=Database, variable=Database$Esp_Epi_Par_Col, var="Esp_Epi_Par_Col", name1="Esp_Epi_Par_Col")
Esp_Par<-fun.anova(data=Database, variable=Database$Esp_Par, var="Esp_Par", name1="Esp_Par")
Esp_Par_Col_End<-fun.anova(data=Database, variable=Database$Esp_Par_Col_End, var="Esp_Par_Col_End", name1="Esp_Par_Col_End")
Area_celulas_1<-fun.anova(data=Database, variable=Database$Area_celulas_1, var="Area_celulas_1", name1="Area_celulas_1")
Area_celulas_2<-fun.anova(data=Database, variable=Database$Area_celulas_2, var="Area_celulas_2", name1="Area_celulas_2")
Area_celulas_3<-fun.anova(data=Database, variable=Database$Area_celulas_3, var="Area_celulas_3", name1="Area_celulas_3")
############################################################
str(Esp_Total)
par(mfrow=c(1,1),mgp = c(1.75,0.5,0), mar = c(5,3.25,1,1))
fun.boxplot(data1=Database, variable=Database$Esp_Total, data2= Esp_Total, 
            ylim = c(7, 10000), d=1,
            ylab = expression(paste("Esp_Total (",mu,"m)")))
############################################################
Esp_Total$Period<-as.numeric(as.character(Esp_Total$Period))
Esp_Epi_Par_Col$Period<-as.numeric(as.character(Esp_Epi_Par_Col$Period))
Esp_Par$Period<-as.numeric(as.character(Esp_Par$Period))
Esp_Par_Col_End$Period<-as.numeric(as.character(Esp_Par_Col_End$Period))
Area_celulas_1$Period<-as.numeric(as.character(Area_celulas_1$Period))
Area_celulas_2$Period<-as.numeric(as.character(Area_celulas_2$Period))
Area_celulas_3$Period<-as.numeric(as.character(Area_celulas_3$Period))
############################################################
jpeg(file = "Results/Fig. Esp_Total.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Esp_Total
ylim=c(0,10000)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Total", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Total", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Total", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Total", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Esp_Epi_Par_Col.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Esp_Epi_Par_Col
ylim=c(0,300)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Epi_Par_Col", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Epi_Par_Col", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Epi_Par_Col", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Epi_Par_Col", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Esp_Par.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Esp_Par
ylim=c(0,10000)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Par", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Par", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Par", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Par", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Esp_Par_Col_End.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Esp_Par_Col_End
ylim=c(0,300)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Par_Col_End", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Esp_Par_Col_End", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Par_Col_End", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Esp_Par_Col_End", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Area_celulas_1.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Area_celulas_1
ylim=c(10000,40000)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_1", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_1", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_1", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_1", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Area_celulas_2.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Area_celulas_2
ylim=c(10000,1000000)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_2", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_2", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_2", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_2", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################
jpeg(file = "Results/Fig. Area_celulas_3.jpeg",
     width = 1200, height = 1000, units = "px", pointsize = 24,quality = 100,bg = "white", res = NA, family = "", restoreConsole = TRUE,type = c("windows", "cairo"))
par(mfrow=c(2,2),mgp = c(1.75,0.5,0), mar = c(3,3,1,1))
############################################################
dataplot=Area_celulas_3
ylim=c(0,20000)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
     pch=19, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_3", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=17, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Summer"],
       pch=15, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
     pch=1, cex=1.25, col="red",main="Summer",col.lab="red",col.main="red",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_3", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=2, cex=1.25, col="red")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Summer"],
       pch=0, cex=1.25, col="red")
grid(col="salmon")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='red',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("red"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Summer"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("red"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
     pch=19, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_3", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=17, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Natural" & dataplot$Season=="Winter"],
       pch=15, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Natural" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Natural" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Natural pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(19,17,15),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
plot(dataplot$Period[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
     dataplot$Mean[dataplot$Genotype=="Italiano" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
     pch=1, cex=1.25, col="blue",main="Winter",col.lab="blue",col.main="blue",
     xlab="Period (day)", ylab=expression(paste("Area_celulas_3", " (",mu, "m, ±SE)")), xlim = c(2,68), ylim = ylim)
points(dataplot$Period[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Salada" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=2, cex=1.25, col="blue")
points(dataplot$Period[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"], 
       dataplot$Mean[dataplot$Genotype=="Santa Cruz" & dataplot$Pollination=="Mechanics" & dataplot$Season=="Winter"],
       pch=0, cex=1.25, col="blue")
grid(col="skyblue")
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    reg<- lm(y ~ poly(x, 2))
    summary(reg)
    cor(y,predict(reg))
    newx <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out=100)
    predicted.intervals <- predict(reg,data.frame(x=newx),interval='confidence',level=0.95)
    lines(newx,predicted.intervals[,1],col='blue',lwd=2)
    #lines(newx,predicted.intervals[,2],col='black',lwd=1)
    #lines(newx,predicted.intervals[,3],col='black',lwd=1)
    legend("topleft",expression(paste("Fit")), col = c("blue"),text.col = "black",lty =1, lwd=3, merge = F, bg = NULL,bty='n', cex=1.25)
}
for (i in unique(dataplot$Genotype)){
    datasub1<-dataplot[dataplot$Genotype==i,]
    datasub1<-datasub1[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter",]
    x<-datasub1$Period[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    y<-datasub1$Mean[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    SE_1<-datasub1$S.E.[datasub1$Pollination=="Mechanics" & datasub1$Season=="Winter"]
    error.bar.vertical(x, y, SE_1, col="gray")
}
legend("bottomright", title="Mechanics pollination",c("Italiano","Salada","Santa Cruz"), col = c("blue"), text.col = "black",
       pch = c(1,2,0),lty=1,merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
############################################################
dev.off()
############################################################





#legend("left",c(expression(paste("R"^"2","= 0.78")),"p<0.05"), col = c("green"),text.col = "black",merge = F, bg = NULL,bty='n', cex=1.25)
#legend("bottom",expression(paste(italic("g"["s"]), " = 1.138 ABA"^"-0.889", "    R"^"2","= 0.92","    p<0.001")), text.col = "black",merge = F, bg = NULL,bty='n', cex=1.25)
#error.bar.vertical(x, y, SE_1, col="blue")
#error.bar.horizontal(x, y, SE_2, col="blue")
#datasub<-Curva_ABA[Curva_ABA$IRR_Fruit==IRRxFruit,]
#points(datasub$Abisic_acid, datasub$gs_est.el_after, pch=19, col="blue", cex=1.25)
#points(x, y, pch=1, col="gray", cex=2, lwd=1.75)
#text(x*1.25, y, letras, col="darkblue")
####%%%%%%%%%%%%%%%%%%%%%%%%%%%







AFR<-seq()

























































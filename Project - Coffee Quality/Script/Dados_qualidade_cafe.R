#------------------------------------------------
# Script: Qualidade de Café
# Autor: Junior Pastor Pérez-Molina, Ph.D.
# Date: 07-17-2019
# Database from: Dr. Edgard Picole, Ph.D.
#------------------------------------------------



#------------------------------------------------
# Initial steps: Packages & Functions
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
if(!grepl("Proyecto - Edgar", getwd())){
    x= cat(prompt = "Please set the working directory to the project folder")}
#------------------------------------------------



#------------------------------------------------
# Loading database
#------------------------------------------------
qualidade<-read.delim("Data/Dados_qualidade_cafe.txt",header=T,sep="\t",dec=".")
str(qualidade)
#------------------------------------------------



#------------------------------------------------
# Fig. Spearman - Correlations
#------------------------------------------------
library(corrplot)
library(Hmisc)
library(ggcorrplot)
df<-data.frame(qualidade[,c(-1,-2,-4:-6,-8:-10,-20:-24)])
str(df)
mydata <- df[,c(-1)] #, -3:-10)
str(mydata)
mydata<-mydata[, c("DQC","DQS","DQA","DQB","DQT","DQR","DQE","DQO","DQQ",
           "Elevation",
           "SLA","AC","SD","VS",
           "PTA","PVA","PPA","PXA","PFA","ML","MUE","MPP","MLP","MLE")]
pdf(file = "Results/Qualidade cafe/Fig. Qualidade - Spearman correlations.pdf", width = 4.5*2.5, height = 4.5*2.5)
p.mat <- cor_pmat(mydata) #Compute a matrix of correlation p-values
corr <- rcorr(as.matrix(mydata), type = "spearman")
corr <-round(corr$r, 2)
ggcorrplot(corr, legend.title = "Sparman´s coefficient",
           method = "square",  #"square" (default), "circle" 
           type = "lower", # "full" (default), "lower" or "upper" display.
           hc.order = FALSE, #logical value. If TRUE, correlation matrix will be hc.ordered using hclust function.
           outline.col = "gray", #the outline color of square or circle. Default "gray"
           ggtheme = ggplot2::theme_classic, #Change theme
           colors = c("black", "white", "black"), #Change colors
           lab = TRUE, #Add correlation coefficients
           sig.level=0.05, #set significant level
           p.mat = p.mat, #Add correlation significance level
           insig = "blank", pch.col = "white",lab_col = "white" #Leave blank on no significant coefficient
)
dev.off()
#------------------------------------------------
pdf(file = "Results/Qualidade cafe/Fig. Qualidade - Spearman correlations-color.pdf", width = 4.5*2.5, height = 4.5*2.5)
p.mat <- cor_pmat(mydata) #Compute a matrix of correlation p-values
corr <- rcorr(as.matrix(mydata), type = "spearman")
corr <-round(corr$r, 2)
ggcorrplot(corr, legend.title = "Sparman´s coefficient",
           method = "square",  #"square" (default), "circle" 
           type = "lower", # "full" (default), "lower" or "upper" display.
           hc.order = FALSE, #logical value. If TRUE, correlation matrix will be hc.ordered using hclust function.
           outline.col = "gray", #the outline color of square or circle. Default "gray"
           ggtheme = ggplot2::theme_classic, #Change theme
           colors = c("blue", "white", "red"), #Change colors
           lab = TRUE, #Add correlation coefficients
           sig.level=0.05, #set significant level
           p.mat = p.mat, #Add correlation significance level
           insig = "blank", pch.col = "white",lab_col = "white" #Leave blank on no significant coefficient
)
dev.off()
#------------------------------------------------



#------------------------------------------------
# Table Qualidade - Kruskal Wallis & LSD fisher´s
#------------------------------------------------
mydata <- df[,c(-1)]
str(mydata)
mydata<-mydata[, c("DQC","DQS","DQA","DQB","DQT","DQR","DQE","DQO","DQQ",
                   "Elevation",
                   "SLA","SD","VS","AC",
                   "PTA","PVA","PPA","PXA","PFA","ML","MUE","MPP","MLP","MLE")]
mydata$Hillside_position<- as.factor(df$Hillside_position)
mydata2<-mydata[mydata$Hillside_position==c("Noruega fria", "Soalheira quente"),]

data.kw<-data.frame()
fun.kw<-function(name,variable, factor, data){
    model<-kruskal.test(as.numeric(variable) ~ as.factor(factor))
    KW <- round(model$statistic[[1]], 1)
    df1<-model$parameter[[1]]
    df2<-length(variable)-model$parameter[[1]]
    P<-ifelse(round(model$p.value, 3)<0.001, "***", ifelse(round(model$p.value, 3)<0.01, "**", ifelse(round(model$p.value, 3)<0.05, "*", "n.s.")))
    df3<-data.frame(as.character(paste(df1, "/",df2)))
    names(df3)<-c("df")
    rm.whitespace <- function (x) gsub(" ", "", x)
    df3$df<-rm.whitespace(df3$df)
    library(Rmisc)
    sum = summarySE(data, measurevar= name, groupvars=c("Hillside_position"), na.rm=TRUE)
    sum<-sum[c(1,3,5)]
    sum<-data.frame(sum)
    names(sum)<-c("Hillside_position","Mean","S.E.")
    sum$Mean<-round(sum$Mean, 3)
    sum$S.E.<-round(sum$S.E., 3)
    sum2<-data.frame(t(sum$Mean))
    names(sum2)<-c(as.character(sum$Hillside_position))
    sum3<-data.frame(t(sum$S.E.))
    names(sum3)<-c(as.character(sum$Hillside_position))
    library(multcompView)
    a<-pairwise.wilcox.test(as.numeric(variable), as.factor(factor), p.adjust.method = "none")
    tri.to.squ<-function(x){
        rn<-row.names(x)
        cn<-colnames(x)
        an<-unique(c(cn,rn))
        myval<-x[!is.na(x)]
        mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
        for(ext in 1:length(cn))
        {
            for(int in 1:length(rn))
            {
                if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
                mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
                mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
            }
            
        }
        return(mymat)
    }
    mymat<-tri.to.squ(a$p.value)
    myletters<-multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
    myletters
    sum4<-t(data.frame(myletters$Letters))
    sum5<-data.frame("Noruega fria"=paste(sum2[1]," ± ", sum3[1]," ", sum4[1]),
                     "Noruega quente"=paste(sum2[2]," ± ", sum3[2]," ", sum4[2]),
                     "Soalheira fria"=paste(sum2[3]," ± ", sum3[3]," ", sum4[3]),
                     "Soalheira quente"=paste(sum2[4]," ± ", sum3[4]," ", sum4[4]))
    #sum5$Noruega.fria<-rm.whitespace(sum5$Noruega.fria)
    #sum5$Noruega.quente<-rm.whitespace(sum5$Noruega.quente)
    #sum5$Soalheira.fria<-rm.whitespace(sum5$Soalheira.fria)
    #sum5$Soalheira.quente<-rm.whitespace(sum5$Soalheira.quente)
    data.kw <- rbind(data.kw,data.frame(name,sum5,KW,df3,P))
    data.kw
    return(data.kw)
}

# Drink quality
DQ.C<-fun.kw(name="DQC",
             variable=mydata$DQC,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.S<-fun.kw(name="DQS",
            variable=mydata$DQS,
            factor=mydata$Hillside_position,
             data=mydata)
DQ.A<-fun.kw(name="DQA",
             variable=mydata$DQA,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.B<-fun.kw(name="DQB",
             variable=mydata$DQB,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.T<-fun.kw(name="DQT",
             variable=mydata$DQT,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.R<-fun.kw(name="DQR",
             variable=mydata$DQR,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.E<-fun.kw(name="DQE",
             variable=mydata$DQE,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.O<-fun.kw(name="DQO",
             variable=mydata$DQO,
             factor=mydata$Hillside_position,
             data=mydata)
DQ.Q<-fun.kw(name="DQQ",
             variable=mydata$DQQ,
             factor=mydata$Hillside_position,
             data=mydata)

# Leaf
SLA<-fun.kw(name="SLA",
             variable=mydata$SLA,
             factor=mydata$Hillside_position,
             data=mydata)
SD<-fun.kw(name="SD",
           variable=mydata$SD,
           factor=mydata$Hillside_position,
           data=mydata)
#VS<-fun.kw(name="VS",
#            variable=mydata$VS,
#            factor=mydata$Hillside_position,
#            data=mydata)
W<-wilcox.test(VS ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
VS<-data.frame(Variable="VS",W= wt, P= pt, row.names = NULL)
sum = summarySE(mydata2, measurevar= "VS", groupvars=c("Hillside_position"), na.rm=TRUE)
sum<-sum[c(1,3,5)]
sum<-data.frame(sum)
names(sum)<-c("Hillside_position","Mean","S.E.")
sum$Mean<-round(sum$Mean, 3)
sum$S.E.<-round(sum$S.E., 3)
sum2<-data.frame(t(sum$Mean))
names(sum2)<-c(as.character(sum$Hillside_position))
sum3<-data.frame(t(sum$S.E.))
names(sum3)<-c(as.character(sum$Hillside_position))
sum5<-data.frame("Noruega fria"=paste(sum2[1]," ± ", sum3[1]," "),
                 "Noruega quente"=paste(sum2[2]," ± ", sum3[2]," "))
sum5

AC<-fun.kw(name="AC",
           variable=mydata$AC,
           factor=mydata$Hillside_position,
           data=mydata)

# Petiole
P.TA<-fun.kw(name="PTA",
           variable=mydata$PTA,
           factor=mydata$Hillside_position,
           data=mydata)
W<-wilcox.test(PTA ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
PTA<-data.frame(Variable="PTA",W= wt, P= pt, row.names = NULL)

P.VA<-fun.kw(name="PVA",
             variable=mydata$PVA,
             factor=mydata$Hillside_position,
             data=mydata)
W<-wilcox.test(PVA ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
PVA<-data.frame(Variable="PVA",W= wt, P= pt, row.names = NULL)

P.PA<-fun.kw(name="PPA",
             variable=mydata$PPA,
             factor=mydata$Hillside_position,
             data=mydata)
W<-wilcox.test(PPA ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
PPA<-data.frame(Variable="PPA",W= wt, P= pt, row.names = NULL)

P.XA<-fun.kw(name="PXA",
             variable=mydata$PXA,
             factor=mydata$Hillside_position,
             data=mydata)
W<-wilcox.test(PXA ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
PXA<-data.frame(Variable="PXA",W= wt, P= pt, row.names = NULL)

P.FA<-fun.kw(name="PFA",
             variable=mydata$PFA,
             factor=mydata$Hillside_position,
             data=mydata)
W<-wilcox.test(PFA ~ Hillside_position, data=mydata2)
wt<- W$statistic[1]
pt<- ifelse(W$p.value[1]<0.001, "***", ifelse(W$p.value[1]<0.01, "**", ifelse(W$p.value[1]<0.05, "*", "n.s.")))  
PFA<-data.frame(Variable="PFA",W= wt, P= pt, row.names = NULL)


w.data <- merge(VS,PTA, all=TRUE)
w.data <- merge(w.data,PVA, all=TRUE)
w.data <- merge(w.data,PPA, all=TRUE)
w.data <- merge(w.data,PXA, all=TRUE)
w.data <- merge(w.data,PFA, all=TRUE)


# Mesophyll
M.L<-fun.kw(name="ML",
             variable=mydata$ML,
             factor=mydata$Hillside_position,
             data=mydata)
M.UE<-fun.kw(name="MUE",
             variable=mydata$MUE,
             factor=mydata$Hillside_position,
             data=mydata)
M.PP<-fun.kw(name="MPP",
             variable=mydata$MPP,
             factor=mydata$Hillside_position,
             data=mydata)
M.LP<-fun.kw(name="MLP",
             variable=mydata$MLP,
             factor=mydata$Hillside_position,
             data=mydata)
M.LE<-fun.kw(name="MLE",
             variable=mydata$MLE,
             factor=mydata$Hillside_position,
             data=mydata)

# Elevation
Elevation<-fun.kw(name="Elevation",
             variable=mydata$Elevation,
             factor=mydata$Hillside_position,
             data=mydata)

kw<-data.frame()
kw <- merge(DQ.C,DQ.S, all=TRUE)
kw <- merge(kw,DQ.A, all=TRUE)
kw <- merge(kw,DQ.B, all=TRUE)
kw <- merge(kw,DQ.T, all=TRUE)
kw <- merge(kw,DQ.R, all=TRUE)
kw <- merge(kw,DQ.E, all=TRUE)
kw <- merge(kw,DQ.O, all=TRUE)
kw <- merge(kw,DQ.Q, all=TRUE)
kw <- merge(kw,SLA, all=TRUE)
kw <- merge(kw,SD, all=TRUE)
kw <- merge(kw,AC, all=TRUE)
kw <- merge(kw,P.TA, all=TRUE)
kw <- merge(kw,P.VA, all=TRUE)
kw <- merge(kw,P.PA, all=TRUE)
kw <- merge(kw,P.XA, all=TRUE)
kw <- merge(kw,P.FA, all=TRUE)
kw <- merge(kw,M.L, all=TRUE)
kw <- merge(kw,M.UE, all=TRUE)
kw <- merge(kw,M.PP, all=TRUE)
kw <- merge(kw,M.LP, all=TRUE)
kw <- merge(kw,M.LE, all=TRUE)

require(openxlsx)
write.xlsx(kw, "Results/Table/Table 1. Kruskal-Wallis.xlsx",sheetName="Table 1",col.names=TRUE,
           row.names=FALSE, append=FALSE,showNA=TRUE, password=NULL)
write.xlsx(w.data, "Results/Table/Table 1. U-Mann-Whitney.xlsx",sheetName="Table 1",col.names=TRUE,
           row.names=FALSE, append=TRUE,showNA=TRUE, password=NULL)
#------------------------------------------------



#------------------------------------------------
# Fig. Mantel test 
#------------------------------------------------
mydata$Latitude  <- qualidade$Latitude
mydata$Longitude <- qualidade$Longitude
head(mydata)

fun.mantel.test<-function(name1, name2, variable1, variable2, data){
    df.mantel<-data.frame(data$Latitude, data$Longitude, variable1, variable2)
    df.mantel<-na.omit(df.mantel)
    names(df.mantel)<-c("Latitude","Longitude",name1,name2)
    dist.1 <- dist(df.mantel[,3])
    dist.2 <- dist(df.mantel[,4])
    coord.dists <- dist(cbind(df.mantel$Longitude, df.mantel$Latitude))
    
    library(ade4)
    a1<-mantel.rtest(coord.dists, dist.2, nrepet = 2000)
    a1
    p1<-ifelse(round(a1$pvalue, 3)<0.001, "<0.001 ",ifelse(round(a1$pvalue, 3)<0.01, "<0.01 ",ifelse(round(a1$pvalue, 3)<0.05, "<0.05 ", paste("=",round(a1$pvalue, 3), " "))))
    m1<-round(abs(a1$obs), 2)
    
    a<-mantel.rtest(dist.1, dist.2, nrepet = 2000)
    p<-ifelse(round(a$pvalue, 3)<0.001, "<0.001 ",ifelse(round(a$pvalue, 3)<0.01, "<0.01 ",ifelse(round(a$pvalue, 3)<0.05, "<0.05 ", paste("=",round(a$pvalue, 3), " "))))
    m<-round(abs(a$obs), 2)
    #------------------------------------------------
    library(plotly)
    library(MASS)  # in case it is not already loaded 
    set.seed(101)
    n <- 1000
    ## some pretty colors
    library(RColorBrewer)
    k <- 11
    my.cols <- rev(brewer.pal(k, "RdYlBu"))
    ## compute 2D kernel density, see MASS book, pp. 130-131
    z <- kde2d(dist.1, dist.2, n=50)
    #------------------------------------------------
    pdf(file = paste("Results/Qualidade cafe/Fig. Mantel-",name1,"vs",name2,".pdf"), width = 4.5*2.4, height = 4.5*2.25)
    library("TeachingDemos")
    par(xpd = FALSE,mfrow=c(1,1),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
    smoothScatter(dist.1, dist.2, nrpoints=.3*n, colramp=colorRampPalette(my.cols), pch=19, cex=.1, 
                  xlab=paste(name1," distance"), ylab=paste(name2," distance"), cex.axis=1.5, cex.lab=2)
    
    points(dist.1, dist.2, pch=3, cex=0.1)
    contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=1)
    #abline(h=mean(dist.1), v=mean(dist.2), lwd=2)
    #reg<-lm(dist.2 ~ dist.1)
    #abline(reg, col="red", lwd=1.5)
    legend("topleft", paste("R=", m, ", p", p), bty="n", text.col ="White", cex=2)
    #------------------------------------------------
    u <- par("usr")
    v <- c(
        grconvertX(u[1:2], "user", "ndc"),
        grconvertY(u[3:4], "user", "ndc")
    )
    v <- c( (v[1]+v[2])/2, v[2], (v[3]+v[4])/2, v[4] )
    par( fig=v, new=TRUE, mgp = c(2,0.5,0), mar = c(3.5,3.5,1,1) )
    plot(x=coord.dists, y=dist.2, pch='.',
         xlab="Spatial distance (Euclidean)", ylab=paste(name2, " distance"), cex.axis=1.25, cex.lab=1.75,col="red")
    #reg<-lm(dist.2 ~ coord.dists)
    #abline(reg, col="red", lwd=1.5)
    legend("topright", paste("R=", m1, ", p", p1), bty="n", text.col="white", cex=1.5)
    par(xpd = FALSE,mfrow=c(1,1),mgp = c(2.5,0.5,0), mar = c(6.5,4,1,1))
    dev.off()
}

#Elevation
fun.mantel.test(name1="DQQ", name2="Elevation", variable1=mydata$DQQ, variable2=mydata$Elevation, data=mydata)

#Leaf
fun.mantel.test(name1="DQQ", name2="SLA", variable1=mydata$DQQ, variable2=mydata$SLA, data=mydata)
fun.mantel.test(name1="DQQ", name2="SD",  variable1=mydata$DQQ, variable2=mydata$SD,  data=mydata)
fun.mantel.test(name1="DQQ", name2="AC",  variable1=mydata$DQQ, variable2=mydata$AC,  data=mydata)
fun.mantel.test(name1="DQQ", name2="VS",  variable1=mydata$DQQ, variable2=mydata$VS,  data=mydata)

#Petiole
fun.mantel.test(name1="DQQ", name2="PTA", variable1=mydata$DQQ, variable2=mydata$P.TA, data=mydata)
fun.mantel.test(name1="DQQ", name2="PVA", variable1=mydata$DQQ, variable2=mydata$P.VA, data=mydata)
fun.mantel.test(name1="DQQ", name2="PPA", variable1=mydata$DQQ, variable2=mydata$P.PA, data=mydata)
fun.mantel.test(name1="DQQ", name2="PXA", variable1=mydata$DQQ, variable2=mydata$P.XA, data=mydata)
fun.mantel.test(name1="DQQ", name2="PFA", variable1=mydata$DQQ, variable2=mydata$P.FA, data=mydata)

#Mesophyll
fun.mantel.test(name1="DQQ", name2="ML",  variable1=mydata$DQQ, variable2=mydata$M.L,  data=mydata)
fun.mantel.test(name1="DQQ", name2="MUE", variable1=mydata$DQQ, variable2=mydata$M.UE, data=mydata)
fun.mantel.test(name1="DQQ", name2="MPP", variable1=mydata$DQQ, variable2=mydata$M.PP, data=mydata)
fun.mantel.test(name1="DQQ", name2="MLP", variable1=mydata$DQQ, variable2=mydata$M.LP, data=mydata)
fun.mantel.test(name1="DQQ", name2="MLE", variable1=mydata$DQQ, variable2=mydata$M.LE, data=mydata)
#------------------------------------------------



#------------------------------------------------
# Fig. PCA plot.
#------------------------------------------------
if(!require(factoextra)){githubinstall("factoextra")}
if(!require(cowplot)){githubinstall("cowplot")}

str(mydata)
df.PCA<-mydata[,-c(1:8,25,26,27)]
str(df.PCA)

df.PCA = df.PCA[with(df.PCA, order(-DQQ)), ]
df.PCA[, c(-1, -7:-11)]
df.PCA<-na.omit(df.PCA)

res.pca1 <- prcomp(df.PCA, scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.numeric(round(df.PCA[,1], 0)) # Only treatment
quali.sup <- as.factor(ifelse(quali.sup>=90,"[90%-95%]",ifelse(quali.sup>=85,"[85%-89%]","[79%-84%]")))


a1<-fviz_pca_ind(res.pca1, geom = c("point"), title="",
                 habillage = quali.sup, addEllipses = FALSE, ellipse.level = 0.60) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(19,17,15))

b1<-fviz_pca_var(res.pca1, col.var="coord", title="") 
b1<- b1 +  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.50)
b1<- b1 + theme_minimal()

#pdf(file = "Results/Qualidade cafe/Fig. PCA color.pdf", width = 9, height = 3.75)
plot_grid(b1,a1,
          labels=c(""),
          ncol = 2, nrow = 1)
#dev.off();dev.off()
#------------------------------------------------




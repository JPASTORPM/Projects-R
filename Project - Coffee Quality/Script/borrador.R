#------------------------------------------------
# Fig. Spearman - Correlations.
#------------------------------------------------
df<-data.frame(qualidade[,c(-1,-2,-4:-6,-8:-10,-20:-24)])
str(df)
mydata <- df[,c(-1)] #, -3:-10)
#pdf(file = "Results/Qualidade cafe/Fig. Qualidade - Spearman correlations.pdf", width = 4.5*2, height = 4.5*2)
#dev.off()
#------------------------------------------------



#------------------------------------------------
library(corrplot)
library(Hmisc)
library(ggcorrplot)
p.mat <- cor_pmat(mydata) #Compute a matrix of correlation p-values
corr <- rcorr(as.matrix(mydata), type = "spearman")
corr <-round(corr$r, 2)
#------------------------------------------------
ggcorrplot(corr, 
           method = "circle",  #"square" (default), "circle" 
           type = "lower", # "full" (default), "lower" or "upper" display.
           hc.order = FALSE, #logical value. If TRUE, correlation matrix will be hc.ordered using hclust function.
           outline.col = "white", #the outline color of square or circle. Default "gray"
           ggtheme = ggplot2::theme_classic, #Change theme
           colors = c("red", "white", "green"), #Change colors
           lab = TRUE, #Add correlation coefficients
           sig.level=0.05, #set significant level
           p.mat = p.mat, #Add correlation significance level
           insig = "blank" #Leave blank on no significant coefficient
)

col <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = corr, col = col, symm = TRUE)
#------------------------------------------------
library(RColorBrewer)
col<- colorRampPalette(c("red", "white", "blue"))(20)

corrplot(corr, 
         method="circle", #visualization method
         type="upper", #types of layout
         order="hclust", #order: reorder the correlation matrix
         col=col, #col: Using different color spectrum
         #bg="lightblue", #bg: Change background color to lightblue
         #col=brewer.pal(n=8, name="RdBu") #use RcolorBrewer palette of colors
         #col=brewer.pal(n=8, name="RdYlBu") #use RcolorBrewer palette of colors
         #col=brewer.pal(n=8, name="PuOr") #use RcolorBrewer palette of colors
         tl.col="blue", #change text colors
         tl.srt=45, #change label rotations
         
         #Combine with significance
         p.mat = p.mat, #add the matrix of p-value
         sig.level = 0.01, ## Specialized the insignificant value according to the significant level
         insig = "blank", #Leave blank on no significant coefficient
         
         diag=FALSE #hide correlation coefficient on the principal diagonal
)





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
if(!grepl("Project_Solanum_lycopersicum", getwd())){
    x= cat(prompt = "Please set the working directory to the project folder")}
#------------------------------------------------



#------------------------------------------------
# Loading database
#------------------------------------------------
qualidade<-read.delim("Data/Dados_qualidade_cafe.txt",header=T,sep="\t",dec=".")
str(qualidade)
#------------------------------------------------



#------------------------------------------------
# Fig. Spearman - Correlations.
#------------------------------------------------
df<-data.frame(qualidade[,c(-1,-2,-4:-6,-8:-10,-20:-24)])
str(df)
mydataX <- df[,c(-1)]
dat<- mydataX

#pdf(file = "Results/Qualidade cafe/Fig. Qualidade - Spearman correlations.pdf", width = 4.5*4, height = 4.5*4)
# How to plot correlations of rating items with R
# sOURCE: https://statistics.ohlsen-web.de/correlationmatrix/
library("corrplot")
library("devtools")
library("sjPlot")
library("ggplot2")
library("plyr")
library("dplyr")
library("gtable")
library("sjPlot")
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
# Number of items, generate labels, and set size of text for correlations and item labels
n <- dim(dat)[2]
labels <- paste0("Item ", 1:n)
sizeItem = 16
sizeCor = 4
## List of scatterplots
scatter <- list()
for (i in 2:n) {
    for (j in 1:(i-1)) {
        
        # Data frame 
        df.point <- na.omit(data.frame(cbind(x = dat[ , j], y = dat[ , i])))
        
        # Plot
        p <- ggplot(df.point, aes(x, y)) +
            geom_jitter(size = .7, position = position_jitter(width = .2, height= .2)) +
            stat_smooth(method="lm", colour="black") +
            theme_bw() + theme(panel.grid = element_blank())
        
        name <- paste0("Item", j, i)
        scatter[[name]] <- p
    } }
## List of bar plots
bar <- list()
for(i in 1:n) {
    
    # Data frame
    bar.df <- as.data.frame(table(dat[ , i], useNA = "no"))
    names(bar.df) <- c("x", "y")
    
    # Plot
    p <- ggplot(bar.df) + 
        geom_bar(aes(x = x, y = y), stat = "identity", width = 0.6) +
        theme_bw() +  theme(panel.grid = element_blank()) +
        ylim(0, max(bar.df$y*1.05)) 
    
    name <- paste0("Item", i)
    bar[[name]] <- p
}
## List of tiles
tile <- list()
for (i in 1:(n-1)) {
    for (j in (i+1):n) {
        
        # Data frame 
        df.point <- na.omit(data.frame(cbind(x = dat[ , j], y = dat[ , i]))) # na.omit returns the object with incomplete cases removed
        
        x = df.point[, 1]
        y = df.point[, 2]
        correlation = cor.test(x, y,method="spearman",use="pairwise") #unter getOption("na.action") nachsehen ==> na.action = na.omit
        cor <- data.frame(estimate = correlation$estimate,
                          statistic = correlation$statistic,
                          p.value = correlation$p.value)
        cor$cor = paste0("r = ", sprintf("%.2f", cor$estimate), "\n", 
                         # "t = ", sprintf("%.2f", cor$statistic), "\n",
                         "p = ", sprintf("%.3f", cor$p.value))
        
        
        # Plot
        p <- ggplot(cor, aes(x = 1, y = 1)) +
            geom_tile(aes(fill = estimate)) +
            geom_text(aes(x = 1, y = 1, label = cor),
                      colour = "White", size = sizeCor, show_guide = FALSE) +
            theme_bw() + theme(panel.grid = element_blank()) +
            scale_fill_gradient2(limits = c(-1,1), midpoint=0,low=("black"),mid="grey",high=("black")) #Neu hinzugefügt
        
        name <- paste0("Item", j, i) ### CHANGE
        tile[[name]] <- p
    } }
# Convert the ggplots to grobs, 
# and select only the plot panels
barGrob <- llply(bar, ggplotGrob)
barGrob <- llply(barGrob, gtable_filter, "panel")
scatterGrob <- llply(scatter, ggplotGrob)
scatterGrob <- llply(scatterGrob, gtable_filter, "panel")
tileGrob <- llply(tile, ggplotGrob)
tileGrob <- llply(tileGrob, gtable_filter, "panel")
## Set up the gtable layout
gt <- gtable(unit(rep(1, n), "null"), unit(rep(1, n), "null"))

## Add the plots to the layout
# Bar plots along the diagonal
for(i in 1:n) {
    gt <- gtable_add_grob(gt, barGrob[[i]], t=i, l=i)
}
# Scatterplots in the lower half
k <- 1
for (i in 2:n) {
    for (j in 1:(i-1)) {
        gt <- gtable_add_grob(gt, scatterGrob[[k]], t=i, l=j)
        k <- k+1
    } }
# Tiles in the upper half
k <- 1
for (i in 1:(n-1)) {
    for (j in (i+1):n) {
        gt <- gtable_add_grob(gt, tileGrob[[k]], t=i, l=j)
        k <- k+1
    } }
# Add item labels
gt <- gtable_add_cols(gt, unit(1.5, "lines"), 0)
gt <- gtable_add_rows(gt, unit(1.5, "lines"), 2*n)
for(i in 1:n) {
    textGrob <- textGrob(labels[i], gp = gpar(fontsize = sizeItem)) 
    gt <- gtable_add_grob(gt, textGrob, t=n+1, l=i+1)
}
for(i in 1:n) {
    textGrob <- textGrob(labels[i], rot = 90, gp = gpar(fontsize = sizeItem)) 
    gt <- gtable_add_grob(gt, textGrob, t=i, l=1)
}
# Add small gap between the panels
for(i in n:1) gt <- gtable_add_cols(gt, unit(0.2, "lines"), i)
for(i in (n-1):1) gt <- gtable_add_rows(gt, unit(0.2, "lines"), i)
# Add chart title
gt <- gtable_add_rows(gt, unit(1.5, "lines"), 0)
textGrob <- textGrob("", gp = gpar(fontface = "bold", fontsize = 16)) 
gt <- gtable_add_grob(gt, textGrob, t=1, l=3, r=2*n+1)
# Add margins to the whole plot
for(i in c(2*n+1, 0)) {
    gt <- gtable_add_cols(gt, unit(.75, "lines"), i)
    gt <- gtable_add_rows(gt, unit(.75, "lines"), i)
}
# Draw it
grid.newpage()
grid.draw(gt)
#dev.off()
#------------------------------------------------




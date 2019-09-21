###################################################
rm(list = ls()) #Remove all objects
graphics.off()  #Remove all graphics
cat("\014")     #Remove script in windows console
set.seed(2018)
Lechuga_hojas=read.delim("https://raw.githubusercontent.com/JPASTORPM/Database/master/Lechuga_hojas..csv",header=T,sep=";",dec=".")
###################################################
c.hojas <- aov(c.hojas ~ Tratamiento, data=Lechuga_hojas)
shapiro.test(c.hojas$residuals) #Opción 1, ver p-value
ifelse((shapiro.test(c.hojas$residuals))$p.value>0.05,"SE ACEPTA EL SUPUESTO DE NORMALIDAD", "SE RECHAZA EL SUPUESTO DE NORMALIDAD, DATOS ASIMÉTRICOS") #Opción 2, la función te dice
###################################################
#¿Cuando están balanceados? Cuando tiene el mismo número de repeticiónes por tratamiento

balanceado<-tapply(Lechuga_hojas$c.hojas, Lechuga_hojas$Tratamiento, length)
Lechuga_hojas<-na.omit(Lechuga_hojas)

colSums(!is.na(Lechuga_hojas[Lechuga_hojas$Tratamiento=="0%",]))
colSums(!is.na(Lechuga_hojas[Lechuga_hojas$Tratamiento=="25%",]))
colSums(!is.na(Lechuga_hojas[Lechuga_hojas$Tratamiento=="50%",]))
colSums(!is.na(Lechuga_hojas[Lechuga_hojas$Tratamiento=="75%",]))
colSums(!is.na(Lechuga_hojas[Lechuga_hojas$Tratamiento=="100%",]))
##################################################
c.hojas<-aov (c.hojas ~ Tratamiento, data=Lechuga_hojas)
c.hojas
###################################################
Comparacion<-pairwise.wilcox.test (Lechuga_hojas$c.hojas, Lechuga_hojas$Tratamiento, p.adj = "none", exact= F) #Notar que usamos en p.adj= "none" para indicar que es LSD
Comparacion<-ifelse(Comparacion$p.value<0.05, "Diferente", "Igual")
Comparacion


TukeyHSD(c.hojas)
plot(TukeyHSD(c.hojas))
###################################################











data(ToothGrowth)
str(ToothGrowth) #Tenemos dos variables numericas y un factor

head(ToothGrowth)

ToothGrowth$dose<-as.factor(ToothGrowth$dose)

andeva3<-aov(len ~ supp + dose, data=ToothGrowth)
summary(andeva3)

andeva3<-aov(len ~ supp + dose + supp:dose, data=ToothGrowth)
summary(andeva3)

andeva3<-aov(len ~ supp * dose, data=ToothGrowth)
summary(andeva3)
plot(andeva3)

andeva4<-aov(len ~ supp * dose, data=ToothGrowth) #Observar que se utiliza el "*", para encontrar la interaccion entre "supp"" y "dose".
summary(andeva4)



andeva4<-aov(len ~ supp * dose, data=ToothGrowth) #Observar que se utiliza el "*", para encontrar la interaccion entre "supp"" y "dose".
summary(andeva4)

interaction.plot(ToothGrowth$dose,ToothGrowth$supp,  ToothGrowth$len, main="Interaction Plot")





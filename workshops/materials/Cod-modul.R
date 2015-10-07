# Cod pregatit pentru scoala de vara "Metode Aplicate de Cercetare Sociala"
# Iulie 2014, Manuel Bosancianu, Bosancianu_Constantin@ceu-budapest.edu

# Baza de date folosita este Valul 6 al Sondajului Social European (European Social Survey), versiunea 1.2 (http://www.europeansocialsurvey.org/download.html?file=ESS6e01_2&y=2012), la care au fost adaugate variabile contextuale:
# - inegalitatea de venit in 2011: "Standardized World Income Inequality Database", versiunea 4.0 (Septembrie 2013) construita de Frederick Solt (http://thedata.harvard.edu/dvn/dv/fsolt/faces/study/StudyPage.xhtml?studyId=36908&tab=files) (gini_net)
# - perceptia coruptiei: datele "Calitatea Guvernului", versiunea 20 Decembrie 2013 (http://www.qogdata.pol.gu.se/data/qog_std_ts_20dec13.sav) (ti_cpi)
# - indexul de inegalitate intre sexe: datele "Calitatea Guvernului", versiunea 20 Decembrie 2013 (undp_gii)
# - produsul intern brut per capita: datele "Calitatea Guvernului", versiunea 20 Decembrie 2013 (wdi_gdpc)
# - tara postcomunista: construita manual (postcom)

# Versiuni folosite:
# - R 3.1.0
# - lme4 1.1.6
# - plyr 1.8.1
# - ggplot2 0.9.3.1
# - gdata 2.13.3
# - scales 0.2.4
# - grid 3.0.3
# - gridExtra 0.9.1

# Sfatul initial este de a descarca de pe website baza de date folosita in capitol, si de a plasa atat baza de date cat si acest fisier in acelasi directoriu, e.g. "C:/Users/Manuel/Desktop/MLM". Cea mai buna metoda este de a plasa fiecare fisier intr-un sub-directoriu propriu:
# date: C:/Users/Manuel/Desktop/MLM/Date
# cod: C:/Users/Manuel/Desktop/MLM/Cod
# grafice: C:/Users/Manuel/Desktop/MLM/Grafice   (acest fisier trebuie creat manual de utilizator; aici for fi salvate graficele generate)

# Eliminam totul din memoria R
rm(list=ls())

# Instalarea pachetelor statistice
install.packages(c("lme4","ggplot2","grid","scales","gdata","gridExtra","sandwich","lmtest","plyr"),dependencies=TRUE, repos="http://cran.at.r-project.org")

setwd("C:/Users/Manuel/Desktop/MLM") # Inlocuieste aceasta adresa cu cea folosita de tine
load(file="./Date/ess6_reduced_CJ.Rdata") # Incarca baza de date in memorie, cautand-o in sub-directoriul "Date"

summary(ess6.merged) # Statistici de baza pentru variabile

# Variabilele sunt definite si in documentatia datelor. Am indicat in paranteze unde am recodificat variabile

# cntry - Numele tarii
# agea - Varsta respondentului in ani impliniti (de la 18 la 100)
# male - Sexul respondentului (recodificat, 0=femeie, 1=barbat)
# eduyrs - Ani de educatie (valori de peste 25 de ani au fost recodificate drept NA)
# mbtru - Respondentul este sau a fost membru intr-un sindicat (recodificat, 1=Da)
# hinctnta - Venitul din gospodarie (in decile) (recodificat, valori mai inalte denota un venit mai mare)
# uemp5yr - Respondentul a fost somer in cel putin o data in ultimii 5 ani (recodificat, 1=Da)
# lrscale - Auto-plasarea pe o scala Stanga-Dreapta a respondentului (valori mai inalte denota o plasare mai spre Dreapta)
# stflife - Satisfactia privind viata a respondentului (valori mai inalte denota un grad mai inalt de satisfactie)
# stfdem - Satisfactia privind democratia a respondentului (valori mai inalte denota un grad mai inalt de satisfactie)
# rlgdgr - Gradul de religiozitate a respondentului (valori mai inalte denota un grad mai inalt de religiozitate)
# ppltrst - "Poti avea incredere in majoritatea oamenilor vs. Nu poti fi suficient de atent cu oamenii" (valori mai mari denota un grad mai inalt de incredere)
# pplfair - "Majoritatea oamenilor incearca sa profite de tine vs. Majoritatea oamenilor incearca sa fie corecti" (valori mai mari denota un grad mai inalt de incredere)
# pplhlp - "Majoritatea timpului oamenii incearca sa fie de ajutor vs. Majoritatea timpului oamenii sunt atenti doar la propriile nevoi" (valori mai mari denota un grad mai inalt de incredere)
# polintr - Gradul de interes politic al respondentului (recodificat, valori mai inalte sa denota un grad mai inalt de interes politic)
# vote - Respondentul a votat in ultimele alegeri nationale (recodificat, 1=Da)
# clsprty - Respondentul se simte mai apropiat de un partid anume (recodificat, 1=Da)
# tvpol - Masura in care respondentul urmareste programe politice la televizor, intr-o saptamana obisnuita (valori mai mari denote un grad mai mare de informare politica)
# dweight - Ponderea de design
# pweight - Ponderea pentru corectia dimensiunii populatiei
# gini_net - inegalitatea de venit net (valori mai mari denota un grad mai inalt de inegalitate)
# ti_cpi - perceptia coruptiei, Transparency International
# undp_gii - index UNDP de inegalitate intre sexe
# wdi_gdpc - PIB per capita, Banca Mondiala, 

###################################################################################

ess6.merged$cntry <- as.factor(ess6.merged$cntry)
ess6.merged$stfdem <- as.numeric(ess6.merged$stfdem)
library(ggplot2)
library(grid)
new_theme <- theme_update(plot.title=element_text(size=14, face="bold", colour="black", family="Courier"),
                          axis.title.x=element_text(size=10, face="bold", colour="black", angle=0, vjust=-0.5, family="Courier"),
                          axis.title.y=element_text(size=10, face="bold", colour="black", angle=90, hjust=0.5, family="Courier"),
                          axis.text.x=element_text(size=8, colour="black", angle=0, vjust=0, family="Courier"),
                          axis.text.y=element_text(size=8, colour="black", angle=0, vjust=0, family="Courier"),
                          panel.grid.major.x=element_line(colour="grey90"),
                          panel.grid.minor.x=element_line(colour="grey80"),
                          panel.grid.major.y=element_line(colour="grey90"),
                          panel.background=element_rect(fill="white", colour=NA),
                          plot.background=element_rect(fill="white", colour=NA),
                          legend.title=element_text(colour="black", face="bold", size=8, family="Courier"),
                          legend.key=element_rect(colour="white"),
                          legend.text=element_text(colour="black", size=8, family="Courier"),
                          axis.ticks=element_blank())
library(scales)
library(plyr)
temp.mat <- ddply(ess6.merged, .(cntry), summarise, mean(stfdem, na.rm=TRUE))
temp.vec <- order(temp.mat[ ,2])
temp.char <- temp.mat[temp.vec, 1]
# Ordonarea tarilor dupa media satisfactiei cu democratia
ess6.merged <- within(ess6.merged, cntry <- factor(cntry, levels = temp.char))
rm(temp.mat, temp.vec, temp.char)
ess6.merged$postcom <- as.factor(ess6.merged$postcom)

# Figura cu distributia satisfactiei privind democratia in tarile din esantion
ggplot(ess6.merged, aes(x=cntry, y=stfdem)) + geom_boxplot(aes(fill=postcom)) + coord_flip() + xlab("") + ylab("") + scale_fill_manual(limits=c(0,1), breaks=c(0,1), values=c("grey80","grey50"), name="Satisfactie \nprivind \ndemocratia", labels=c("E. Vest", "E. Centru-Est")) + scale_y_continuous(breaks=c(0,2,4,6,8,10))

# Modele OLS pentru cateva tari din esantion: Danemarca, Belgia, Elvetia, Estonia, Slovacia, si Polonia
ess6.merged$agea <- ess6.merged$agea/10
model.1 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Danemarca"))
summary(model.1)
model.2 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Belgia"))
summary(model.2)
model.3 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Elvetia"))
summary(model.3)
model.4 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Estonia"))
summary(model.4)
model.5 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Slovacia"))
summary(model.5)
model.6 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=subset(ess6.merged, ess6.merged$cntry=="Polonia"))
summary(model.6)

# Eliberam memoria
rm(model.1, model.2, model.3, model.4, model.5, model.6)

# Model cu variabile dummy pentru tara
model.7 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+as.factor(cntry), na.action=na.omit, data=ess6.merged)
summary(model.7)

# Model OLS cu estimator sandwich
model.8 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=ess6.merged)
library(sandwich)
library(lmtest) # necesara pentru functia "coeftest()"
coeftest(model.8, vcov=sandwich)

# Eliberam memoria
rm(model.7, model.8)

# Model ierarhic liniar nul (fara predictori)
library(lme4)
model.0 <- lmer(stfdem ~ 1+(1|cntry), data=ess6.merged, na.action=na.omit)
summary(model.0)

# Grafic variabilitatea constantei intre tari
library(arm) # Contine functiile ranef() si se.ranef(), de care ne folosim aici
x <- as.matrix(as.numeric(se.ranef(model.0)[[1]]))
y <- as.matrix(as.numeric(ranef(model.0)[[1]][,1]))
mat.eff <- cbind(x,y)
rm(x,y)
colnames(mat.eff) <- c("se","inter") # Denumeste coloanele
mat.eff <- as.data.frame(mat.eff)
mat.eff$country <- c("Bulgaria", "Kosovo", "Slovenia", "Rusia", "Portugalia", "Spania", "Slovacia", "Cipru", "Estonia", "Polonia", "Republica Ceha", "Irlanda", "Marea Britanie", "Islanda", "Belgia", "Israel", "Germania", "Olanda", "Finlanda", "Suedia", "Norvegia", "Danemarca", "Elvetia")
mat.eff$se <- as.numeric(mat.eff$se)
mat.eff$inter <- as.numeric(mat.eff$inter)
mat.eff$inter <- mat.eff$inter + as.numeric(fixef(model.0)[1])
limits=aes(ymax=mat.eff$inter+1.96*mat.eff$se, ymin=mat.eff$inter-1.96*mat.eff$se)
# Imaginea
ggplot(data=mat.eff, aes(x=country,y=inter)) + geom_pointrange(limits, size=0.5) + geom_hline(yintercept=fixef(model.0)[1], linetype="dashed", colour="red", size=1) + coord_flip() + xlab("") + ylab("")

# Comparatie intre un model OLS si unul MLM
model.9 <- lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, na.action=na.omit, data=ess6.merged)
model.10 <- lmer(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+(1|cntry), na.action=na.omit, data=ess6.merged)
summary(model.9)
summary(model.10)

# Modele MLM suplimentare: adaugam la nivelul 2 inegalitatea si statutul postcomunist. In ultimul model specificam panta venitului aleatorie.
model.11 <- lmer(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+(1|cntry), na.action=na.omit, data=ess6.merged)
model.12 <- lmer(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1|cntry), na.action=na.omit, data=ess6.merged)
model.13 <- lmer(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1+hinctnta|cntry), na.action=na.omit, data=ess6.merged)
summary(model.11)
summary(model.12)
summary(model.13)

# Curatam putin memoria
rm(model.0, model.9, model.10)

# Ultimul model este folosit pentru a prezenta grafic variabilitatea pantei venitului
x <- as.matrix(as.numeric(se.ranef(model.13)[[1]][,2]))
y <- as.matrix(as.numeric(ranef(model.13)[[1]][,2]))
mat.eff <- cbind(x,y)
rm(x,y)
colnames(mat.eff) <- c("se","inter") # Numele coloanelor
mat.eff <- as.data.frame(mat.eff)
mat.eff$country <- c("Bulgaria", "Slovenia", "Rusia", "Portugalia", "Spania", "Slovacia", "Cipru", "Estonia", "Polonia", "Republica Ceha", "Irlanda", "Marea Britanie", "Islanda", "Belgia", "Germania", "Olanda", "Finlanda", "Suedia", "Norvegia", "Danemarca", "Elvetia")
mat.eff$se <- as.numeric(mat.eff$se)
mat.eff$inter <- as.numeric(mat.eff$inter)
mat.eff$inter <- mat.eff$inter + as.numeric(fixef(model.13)[5])
limits=aes(ymax=mat.eff$inter+1.96*mat.eff$se, ymin=mat.eff$inter-1.96*mat.eff$se)
# Cum arata pantele venitului din fiecare tara, comparat cu panta generala
ggplot(data=mat.eff, aes(x=country,y=inter)) + geom_pointrange(limits, size=0.5) + geom_hline(yintercept=fixef(model.13)[5], linetype="dashed", colour="red", size=1) + coord_flip() + xlab("") + ylab("")

# Gini este centrat pentru o mai buna interpretare. Desi nu am inclus informatie despre aceasta procedura in slide-uri, participantii interesati pot consulta Enders si Tofighi (2007) (link-ul poate fi gasit in syllabus)
ess6.merged$Gini.std <- ess6.merged$gini_net - mean(unique(ess6.merged$gini_net), na.rm=TRUE)
# Centrarea venitului pentru o mai buna interpretare. Vezi Enders si Tofighi (2007).
library(plyr)
ess6.merged <- ddply(ess6.merged,.(cntry),transform,income=hinctnta-mean(hinctnta,na.rm=TRUE))
# Modelul cu interactiunea inter-nivel
model.14 <- lmer(stfdem~agea+male+eduyrs+income+rlgdgr+ppltrst+Gini.std+postcom+Gini.std*income+(1+income|cntry), na.action=na.omit, data=ess6.merged)

# Grafic cu efectele marginale ale venitului pentru diferite niveluri de inegalitate de venit
betas <- fixef(model.14)  # Coeficientii modelului
varcovmat <- vcov(model.14)   # Matricea de variante-covariante
# Obtinem o secventa pentru variabila moderatoare
moderator.seq<-seq(min(ess6.merged$Gini.std,na.rm=TRUE),max(ess6.merged$Gini.std,na.rm=TRUE),by=0.1)  
# betas[5] - efectul variabilei focale independente (Venit)
# betas[10] - efectul interactiunii dintre variabila focala independenta (Venit) si variabila moderatoare (Gini)
# varcovmat[5,5] - varianta variabilei focale independente (Venit)   
# varcovmat[10,10] - varianta interactiunii  
# varcovmat[10,5] - covarianta dintre interactiune si variabila focala independenta (Venit)
eff<-betas[5]+betas[10]*moderator.seq # efect  
v.eff<-varcovmat[5,5]+(moderator.seq^2)*varcovmat[10,10]+2*moderator.seq*varcovmat[10,5] # varianta efectului  
se.eff <- sqrt(v.eff) # eroarea standard a efectului
lower <-   eff-1.96*se.eff # limita inferioara a intervalului de incredere
upper <-  eff+1.96*se.eff # limita superioara a intervalului de incredere 
ggplot(data=NULL, aes(x=moderator.seq, y=eff)) + geom_line(size=1.25,colour="red") + scale_x_continuous(breaks=c(min(ess6.merged$Gini.std, na.rm=TRUE), mean(ess6.merged$Gini.std, na.rm=TRUE), max(ess6.merged$Gini.std, na.rm=TRUE)), labels=c("Mica","Medie","Mare")) + ggtitle("") + geom_line(aes(x=moderator.seq, y=lower), linetype="dashed", size=1.25, colour="blue") + geom_line(aes(x=moderator.seq, y=upper), linetype="dashed", size=1.25, colour="blue") + geom_ribbon(data=NULL,aes(ymin=lower,ymax=upper),alpha=0.3) + geom_hline(yintercept=0, linetype="dotted", size=1.5, colour="black") + xlab("Inegalitatea de venit") + ylab("Efectul venitului")

# Curatam memoria
rm(model.11, model.12, model.13, model.14)

# Model ierarhic logistic
model.15 <- glmer(vote~agea+eduyrs+hinctnta+rlgdgr+Gini.std+postcom+(1|cntry), na.action=na.omit, data=ess6.merged, family=binomial)
summary(model.15)

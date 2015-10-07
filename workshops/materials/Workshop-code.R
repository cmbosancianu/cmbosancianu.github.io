# PolBeRG workshop code
# May 2014, Manuel Bosancianu, Bosancianu_Constantin@ceu-budapest.edu

# The dataset used is Wave 6 of the European Social Survey, version 2.0 (http://www.europeansocialsurvey.org/download.html?file=ESS6e02&c=&y=2012&loggedin), to which contextual variables have been added:
# - income inequality in 2011: "Standardized World Income Inequality Database", version 4.0 (September 2013), assembled by Frederick Solt (http://thedata.harvard.edu/dvn/dv/fsolt/faces/study/StudyPage.xhtml?studyId=36908&tab=files) (gini_net)
# - perception of corruption: "Quality of Government" data, December 20, 2013 (http://www.qogdata.pol.gu.se/data/qog_std_ts_20dec13.sav) (ti_cpi)
# - UNDP's index of gender inequality: "Quality of Government" data, December 20, 2013 (undp_gii)
# - World Bank's measure of GDP per capita: "Quality of Government" data, December 20, 2013 (wdi_gdpc)
# - postcommunist country: constructed by myself (postcom)

# Software version:
# - R 3.1.0
# - lme4 1.1.6
# - arm 1.7.3
# - plyr 1.8.1
# - ggplot2 0.9.3.1
# - gdata 2.13.3
# - scales 0.2.4
# - grid 3.0.3
# - gridExtra 0.9.1

# Download the dataset and the syntax file from the website, and place both in the same directory, e.g. "C:/Users/Manuel/Desktop/MLM". I would recommend placing each file in its own sub-directory:
# data: C:/Users/Manuel/Desktop/MLM/Data
# code: C:/Users/Manuel/Desktop/MLM/Code
# graphics: C:/Users/Manuel/Desktop/MLM/Graphics   (all these sub-directories should be manually created by the user; this last
# one is where the graphs created will be saved)

# Clear R's memory
rm(list=ls())

# Install some needed packages, if you don't have them already
install.packages(c("lme4","ggplot2","grid","scales","gdata","gridExtra","arm"),dependencies=TRUE, repos="http://cran.at.r-project.org")

setwd("C:/Users/Manuel/Documents/PhD/Polberg materials/Workshop May 2014/") # Replace this with address on your machine
load(file="./Data/ess6_reduced.Rdata") # Load data in memory

summary(ess6.merged) # Basic descriptives for the variables

# I've indicated in brackets where recoding has been performed

# cntry - Name of the country
# agea - Age of respondent (in years)
# male - Gender of respondent (recoded, 0=woman, 1=man)
# eduyrs - Years of education (values over 25 were recoded as NA)
# mbtru - Respondent is or was a member of a union (recoded, 1=yes)
# hinctnta - Household income (in deciles) (recoded, larger values denote a higher income)
# uemp5yr - Respondentul a fost somer in cel putin o data in ultimii 5 ani (recodificat, 1=Da)
# lrscale - Self-placement on a Left-Right scale (larger values denote more rightward placements)
# stflife - Satisfaction with life (higher values denote more satisfaction)
# stfdem - Satisfaction with democracy (higher values denote more satisfaction)
# rlgdgr - Degree of religiosity (higher values denote a more religious respondent)
# ppltrst - Most people can be trusted (higher values denote a higher degree of trust)
# pplfair - Most people try to be fair (higher values denote a higher degree of trust)
# pplhlp - Most people try to help (higher values denote a higher degree of trust)
# polintr - Political interest (recoded, higher values denote a higher degree of trust)
# vote - Respondent voted in the last national elections (recoded, 1=yes)
# clsprty - Respondent feels close to a particular party (recoded, 1=yes)
# tvpol - TV use for politics in a usual week (higher values denote a
# higher level of political information)
# dweight - Design weight
# pweight - Population weight
# gini_net - Gini index of net income (higher values denote more inequality)
# ti_cpi - Transparency International's Corruption Perception Index
#         (higher values denote less corruption)
# undp_gii - UNDP index of gender inequality
# wdi_gdpc - GDO per capita, World Bank Development Indicators

###################################################################################3
# Taking a look at how the dependent variable looks like

library(ggplot2)
library(grid)
library(scales)
gr.1 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Czech Republic"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Czech Republic") + scale_x_continuous(breaks=c(0:10))
gr.2 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Germany"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Germany") + scale_x_continuous(breaks=c(0:10))
gr.3 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Denmark"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Denmark") + scale_x_continuous(breaks=c(0:10))
gr.4 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Spain"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Spain") + scale_x_continuous(breaks=c(0:10))
gr.5 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Finland"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Finland") + scale_x_continuous(breaks=c(0:10))
gr.6 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Great Britain"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Great Britain") + scale_x_continuous(breaks=c(0:10))
gr.7 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Netherlands"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Netherlands") + scale_x_continuous(breaks=c(0:10))
gr.8 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Poland"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Poland") + scale_x_continuous(breaks=c(0:10))
gr.9 <- ggplot(subset(ess6.merged, ess6.merged$cntry=="Slovakia"), aes(x=stfdem)) + geom_histogram(binwidth=0.5) + xlab("") + ylab("") + ggtitle("Slovakia") + scale_x_continuous(breaks=c(0:10))
library(gridExtra)
pdf("./Graphics/histogram-1.pdf") # Save in PDF format in the "Graphics" subdirectory
grid.arrange(gr.1, gr.2, gr.3, gr.4, gr.5, gr.6, gr.7, gr.8, gr.9, ncol=3)
dev.off()

###################################################################################
# DISTRIBUTION OF THE DEPENDENT VARIABLE FOR ALL COUNTRIES

ess6.merged$cntry <- as.factor(ess6.merged$cntry)
ess6.merged$stfdem <- as.numeric(ess6.merged$stfdem)
require(ggplot2)
require(grid)
require(scales)
require(plyr)
temp.mat <- ddply(ess6.merged, .(cntry), summarise, mean(stfdem, na.rm=TRUE))
temp.vec <- order(temp.mat[ ,2])
temp.char <- temp.mat[temp.vec, 1]
ess6.merged <- within(ess6.merged, cntry <- factor(cntry, levels = temp.char)) # Re-order the countries
rm(temp.mat, temp.vec, temp.char)
ess6.merged$postcom <- as.factor(ess6.merged$postcom)
graph1 <- ggplot(ess6.merged, aes(x=cntry, y=stfdem)) + geom_boxplot(aes(fill=postcom)) + coord_flip() + xlab("Country") + ylab("Satisfaction with democracy") + scale_fill_manual(limits=c(0,1), breaks=c(0,1), values=c("grey80","grey50"), name="Location in \nEurope", labels=c("West", "East-Central")) + scale_y_continuous(breaks=c(0,2,4,6,8,10))
ggsave(graph1, file="./Graphics/boxplot-1.pdf", height=8, width=8, dpi=300)
rm(graph1)

###################################################################################
# HIERARCHICAL LINEAR MODELS

# Recode age, measured now in decades
ess6.merged$agea <- ess6.merged$agea/10
# Center Gini before including in a cross-level interaction
ess6.merged$Gini.std <- ess6.merged$gini_net - mean(unique(ess6.merged$gini_net), na.rm=TRUE)
# Center income before including in a cross-level interaction
library(plyr)
ess6.merged <- ddply(ess6.merged,.(cntry),transform,income=hinctnta-mean(hinctnta,na.rm=TRUE))
# Keep just the variables used in the models tested here
# To try out different variables in the dataset, simply exclude their
# names from the character vector below.
library(gdata)
ess6.merged<-remove.vars(ess6.merged,c("mbtru","uemp5yr","lrscale","stflife","pplfair","pplhlp","polintr","vote","clsprty","tvpol","dweight","pweight","undp_gii","wdi_gdpc"),info=FALSE)
# Remove incomplete cases
ess6.merged <- ess6.merged[complete.cases(ess6.merged), ]
# Load the package that runs HLMs.
# Alternatives in R are: "nlme" (more complex syntax)
#                        "multilevel" (similar abilities)
library(lme4)
# Added on the proverbial night before
# This shows you how running a model for each country would look like
lmList(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst|cntry, data=ess6.merged, na.action=na.omit)
# Yet another way of doing it, which is a bit more powerful
# Apply a function do each country, and return a list as output
models <- dlply(ess6.merged, "cntry", function(df) lm(stfdem~agea+male+eduyrs+hinctnta+rlgdgr+ppltrst, data=df, na.action=na.omit))
df1 <- ldply(models, coef) # Return coefficients
df2 <- ldply(models, function(df) sqrt(diag(vcov(df)))) # Return standard errors
df2 <- rename.vars(df2, c("cntry","(Intercept)","agea","male","eduyrs","hinctnta","rlgdgr","ppltrst"), c("cntry2","se.(Intercept)","se.agea","se.male","se.eduyrs","se.hinctnta","se.rlgdgr","se.ppltrst"), info=FALSE)
df.final <- cbind(df1,df2) # merge the two data frames
rm(df1,df2) # remove the old data frames
df.final <- remove.vars(df.final,c("cntry2"),info=FALSE)
library(ggplot2)
# The actual plotting
limits=aes(ymax=df.final$hinctnta+1.96*df.final$se.hinctnta, ymin=df.final$hinctnta-1.96*df.final$se.hinctnta)
# This is how the effects of income look like
ggplot(data=df.final, aes(x=cntry,y=hinctnta)) + geom_pointrange(limits, size=1) + geom_hline(yintercept=0, linetype="dashed", colour="red", size=2) + coord_flip()
# Informative, but not quick enough
df.final$cntry <- factor(df.final$cntry, levels = df.final$cntry[order(df.final$hinctnta)])
ggplot(data=df.final, aes(x=cntry,y=hinctnta)) + geom_pointrange(limits, size=1) + geom_hline(yintercept=0, linetype="dashed", colour="red", size=2) + coord_flip()
# Why the big differences in uncertainty for the estimate?
ddply(ess6.merged,~cntry, nrow)

# Null model
model.1<-lmer(stfdem~1+(1|cntry),na.action=na.omit,data=ess6.merged,REML=FALSE)
summary(model.1)
# The ICC can be computed by hand: 1.419/(1.419+4.795)=0.2283553
# There is also package "ICC", but that gives slightly different
# results (at the second decimal), and I can't figure out why.
# So here's a hand-built function, just for two-level models.
ICC.2 <- function(mer){
    clusvar <- as.character(attr(attr(mer@frame,"terms"),"term.labels"))
    tempvar <- paste0("VarCorr(mer)$",clusvar)
    icc.corr <- (as.numeric(attr(eval(parse(text=tempvar)),"stddev"))^2)/
        ((as.numeric(attr(eval(parse(text=tempvar)),"stddev"))^2) +
        (as.numeric((attr(VarCorr(mer),"sc")))^2))
    rm(clusvar,tempvar)
    print(icc.corr)
}
ICC.2(model.1)
# Level 1 predictors
model.2<-lmer(stfdem~1+agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+(1|cntry), na.action=na.omit, data=ess6.merged, REML=FALSE)
summary(model.2)
# The model tested above actually includes a random intercept.
# How can we see how this varies between countries?
# "lme4" offers two functions: ranef() and fixef()
fixef(model.2) # The coefficients
ranef(model.2) # The deviations from the intercept
x <- as.matrix(ranef(model.2)[[1]])
x[,1] <- x[,1]+as.numeric(fixef(model.2)[1]) # The country intercepts
x
require(ggplot2)
ggplot(data=NULL, aes(x=x[,1])) + geom_histogram(binwidth=0.5) +xlab("Distribution of intercepts") + ylab("Frequency") # Normally distributed coefficients
# Here would be another way to plot the variability of intercepts
library(arm) # Has some functions we need for this task
x <- as.matrix(as.numeric(se.ranef(model.2)[[1]]))
y <- as.matrix(as.numeric(ranef(model.2)[[1]][,1]))
mat.eff <- cbind(x,y)
rm(x,y)
colnames(mat.eff) <- c("se","inter") # Name columns
mat.eff <- as.data.frame(mat.eff)
mat.eff$country <- c("Belgium","Bulgaria","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Great Britain","Hungary","Iceland","Ireland","Italy","Lithuania","Netherlands","Norway","Poland","Portugal","Russia","Slovakia","Slovenia","Spain","Sweden","Switzerland")
mat.eff$se <- as.numeric(mat.eff$se)
mat.eff$inter <- as.numeric(mat.eff$inter)
mat.eff$inter <- mat.eff$inter + as.numeric(fixef(model.2)[1])
limits=aes(ymax=mat.eff$inter+1.96*mat.eff$se, ymin=mat.eff$inter-1.96*mat.eff$se)
# This is how the group intercepts look with respect to the general intercept
ggplot(data=mat.eff, aes(x=country,y=inter)) + geom_pointrange(limits, size=1) + geom_hline(yintercept=fixef(model.2)[1], linetype="dashed", colour="red", size=2) + coord_flip()

# 2 Level 2 predictors, as fixed effects
model.3<-lmer(stfdem~1+agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1|cntry),na.action=na.omit,data=ess6.merged,REML=FALSE)

# Slope of income allowed to vary
model.4<-lmer(stfdem~1+agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1+income|cntry),na.action=na.omit,data=ess6.merged,REML=FALSE)
# Let's play around a bit with this
# Is everyone clear as to why some coefficients vary between countries, whereas others don't?
coef(model.4)
x <- as.matrix(as.numeric(se.ranef(model.4)[[1]][,2]))
y <- as.matrix(as.numeric(ranef(model.4)[[1]][,2]))
mat.eff <- cbind(x,y)
rm(x,y)
colnames(mat.eff) <- c("se","income") # Name columns
mat.eff <- as.data.frame(mat.eff)
mat.eff$country <- c("Belgium","Bulgaria","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Great Britain","Hungary","Iceland","Ireland","Italy","Lithuania","Netherlands","Norway","Poland","Portugal","Russia","Slovakia","Slovenia","Spain","Sweden","Switzerland")
mat.eff$se <- as.numeric(mat.eff$se)
mat.eff$income <- as.numeric(mat.eff$income)
mat.eff$income <- mat.eff$income + as.numeric(fixef(model.4)[5])
limits=aes(ymax=mat.eff$income+1.96*mat.eff$se, ymin=mat.eff$income-1.96*mat.eff$se)
# This is how the group intercepts look with respect to the general intercept
ggplot(data=mat.eff, aes(x=country,y=income)) + geom_pointrange(limits, size=1) + geom_hline(yintercept=fixef(model.4)[5], linetype="dashed", colour="red", size=2) + xlab("Country") + ylab("Slope of income") + coord_flip()

# Inequality becomes a predictor for the slope of income
model.5<-lmer(stfdem~1+agea+male+eduyrs+income+rlgdgr+ppltrst+Gini.std+Gini.std*income+postcom+(1+income|cntry),na.action=na.omit,data=ess6.merged,REML=FALSE)

# Comparing model fit
anova(model.2, model.3)

# New tests of model fit with REML estimation
model.3.new<-lmer(stfdem~1+agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1|cntry),na.action=na.omit,data=ess6.merged,REML=TRUE)
model.4.new<-lmer(stfdem~1+agea+male+eduyrs+hinctnta+rlgdgr+ppltrst+gini_net+postcom+(1+income|cntry),na.action=na.omit,data=ess6.merged,REML=TRUE)
anova(model.3.new,model.4.new)

# Comparing the last two models
anova(model.4, model.5)

# Remove unnecessary models from memory
rm(model.1,model.2,model.3,model.4,model.3.new,model.4.new)

#################################################################################
# Marginal effects of income for different levels of inequality

betas <- fixef(model.5) # Fixed effects in the model
varcovmat <- vcov(model.5) # Variance-Covariance matrix for the model
# Obtain sequence for inequality
moderator.seq<-seq(min(ess6.merged$Gini.std,na.rm=TRUE),max(ess6.merged$Gini.std,na.rm=TRUE),by=0.1)
# betas[5] - main effect of income
# betas[10] - interaction effect (income by inequality)
# varcovmat[5,5] - variance of income
# varcovmat[10,10] - variance of interaction effect
# varcovmat[10,5] - covariance between interaction and income
eff<-betas[5]+betas[10]*moderator.seq # effect
v.eff<-varcovmat[5,5]+(moderator.seq^2)*varcovmat[10,10]+2*moderator.seq*varcovmat[10,5] # variance of the effect
se.eff <- sqrt(v.eff) # Standard Error of the effect
lower <-   eff-1.96*se.eff # upper Confidence Interval
upper <-  eff+1.96*se.eff # lower Confidence Interval
graph2 <- ggplot(data=NULL, aes(x=moderator.seq, y=eff)) + geom_line(size=1.25) + scale_x_continuous(breaks=c(min(ess6.merged$Gini.std, na.rm=TRUE), mean(ess6.merged$Gini.std, na.rm=TRUE), max(ess6.merged$Gini.std, na.rm=TRUE)), labels=c("Small","Average","Large")) + ggtitle("") + geom_line(aes(x=moderator.seq, y=lower), linetype="dashed", size=1.25, colour="grey50") + geom_line(aes(x=moderator.seq, y=upper), linetype="dashed", size=1.25, colour="grey50") + geom_hline(yintercept=0, linetype="dotted", size=1.5, colour="black") + xlab("Income inequality") + ylab("Effect of income")
ggsave(graph2, file="./Graphics/marginal-effects.pdf",height=8,width=8,dpi=300)
rm(graph2)

rm(list=ls())

#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######            STAT0023 ICA2: Script for report output        ######
######                                                           ######
###### This script is designed to generate any numerical and     ######
###### graphical output which is gonna be used for our report    ######
###### on the UK Covid dataset.                                  ######
######                                                           ######
###### This script will be divided into four parts. The first    ######
###### part is the import and preprocessing of the dataset. The  ######
###### second part shows our findings of the exploratory data    ######
###### analysis. Then, the model building process will form most ######
###### of our third part and finally we will do the predictions  ######
###### for the 1800 observations.                                ######
#######################################################################
#######################################################################
#######################################################################

#######################################################################
# First Step: Data import and preprocessing
#######################################################################

library(ggplot2)

UKCovid<-read.csv("UKCovidWave1.csv")

##
##Seperate the first 5401 observations for model building and the rest
##1800 observations for prediction.
##

UKCovid.model<-UKCovid[1:5401,]#Model-fitting dataset

UKCovid.pred<-UKCovid[5402:7201,]#Prediction dataset

########################################################################
# Second Step: Exploratory Data Analysis
########################################################################

##
## From our context information, we will start by exploring age related
## coefficients. We first created a new covariate "old.prop" which is the 
## proportion of people over age 60 in each MSOA and also "young.prop". 
## We generated a scatter plot of old, log(old.prop), young.prop and   
## log(young.prop) with Deaths, which has shown some correlations. 

## Create "old.prop" covariate
UKCovid.model$old.prop<-rowSums(cbind(UKCovid.model[,36:40]),
                                na.rm=FALSE)/UKCovid.model$PopTot

## Create "young.prop" covariate
UKCovid.model$young.prop<-rowSums(cbind(UKCovid.model[,25:27]),
                                na.rm=FALSE)/UKCovid.model$PopTot

## Before plotting, first specify the margin and number of plots in the page.
par(mar=c(2.5,2.5,2.5,2.5))
par(mfrow=c(1,1))

## Plot "Deaths" against "old.prop"
plot(UKCovid.model$old.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of people over 60",
     xlab = "old.prop",ylab="Deaths")

## Plot "Deaths" against log of "old.prop"
plot(UKCovid.model$old.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of people over 
     60",xlab = "log(old.prop)",ylab="Deaths",log="x")
     
## Plot "Deaths" against "young.prop"
plot(UKCovid.model$young.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of people under 9",
     xlab = "young.prop",ylab="Deaths")

## Plot "Deaths" against log of "young.prop"
plot(UKCovid.model$young.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of people 
           under 9",
     xlab = "log(young.prop)",ylab="Deaths",log="x")

##
## The next bits to look at are gender covariates (i.e "PopM" and "PopF").
## Again, we use scatter plots to try to capture potential relationship
## between them and death counts. Similarly, we use proportion of Male/
## Female in each MSOA(e.g "PopM/PopTot") for plotting to account for the  
## effects of different population sizes in different MSOA. Moreover, ggplot
## has been used to see the trends in rural/urban areas.
##

##Plot of "PopM" against "Deaths"
plot(UKCovid.model$PopM,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of male",
     xlab = "PopM",ylab="Deaths")

##Plot of log of "PopM" against "Deaths"
plot(UKCovid.model$PopM,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of number of male",
     xlab = "log(PopM)",ylab="Deaths",log="x")

##Plot of "PopF" against "Deaths"
plot(UKCovid.model$PopF,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of female",
     xlab = "PopF",ylab="Deaths")

##Plot of log of "PopF" against "Deaths"
plot(UKCovid.model$PopF,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of number of female",
     xlab = "log(PopF)",ylab="Deaths",log="x")

##
## Next, we will focus on "HH_HealthPrb" covariate suggested by context 
## information. We define a new covariate "HH_HealthPrb.prop" as the 
## proportion of households where at least one person has long term health
## problem and disability in each MSOA. Then we use scatter plot to try to
## see its relationship with deathc counts.
##

##Define "HH_HealthPrb.prop"
UKCovid.model$HH_HealthPrb.prop<-UKCovid.model$HH_HealthPrb/UKCovid.model$HH

##Plot of "HH_HealthPrb.prop" with "Deaths"
plot(UKCovid.model$HH_HealthPrb.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of households 
     where at least one person has a long-term health problem or disability", 
     xlab = "HH_HealthPrb.prop",ylab="Deaths")

##Plot of log of "HH_HealthPrb.prop" with "Deaths"
plot(UKCovid.model$HH_HealthPrb.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of households 
     where at least one person has a long-term health problem or disability", 
     xlab = "log(HH_HealthPrb.prop)",ylab="Deaths",log="x")


##
## Now, it's time to move to the four "HHDepriv" covariates. Also, we will include
## plots for ethnicity ("ETHBlack" etc.). 
## The reason for talking about these two groups of covariates is that there aren't
## any obvious patterns between these three groups and "Deaths" from the scatter plots
## generated (even under log transformations). 
## 
## Here we have used the proportion of deprived households in all four dimensions and
## the proportion of Black and Asian people in each MSOA as the "independent" covariate.
## We will define a new covariate "HHDepriv.prop" as the proportion of deprived 
## households (in all four dimensions) in each MSOA.
## 
##

##Definition of "HHDepriv.prop"
UKCovid.model$HHDepriv.prop<-rowSums(cbind(UKCovid.model[,18:21]),
                                     na.rm=FALSE)/UKCovid.model$HH

##Plot of "HHDepriv.prop" and "Deaths"
plot(UKCovid.model$HHDepriv.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of households 
           deprived in all four dimensions", 
     xlab = "HHDepriv.prop",ylab="Deaths")

##Plot of log of "HHDepriv.prop" and "Deaths"
plot(UKCovid.model$HHDepriv.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of households 
           deprived in all four dimensions", 
     xlab = "log(HHDepriv.prop)",ylab="Deaths",log="x")

##Plot of "EthAsian+EthBlack and "Deaths"
plot((UKCovid.model$EthAsian+UKCovid.model$EthBlack)/UKCovid.model$PopTot,
      UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of Black and Asian people.", 
     xlab = "(EthAsian+EthBlack)/PopTot",ylab="Deaths")

##Plot of log of "EthAsian+EthBlack" and "Deaths"
plot(UKCovid.model$EthAsian+UKCovid.model$EthBlack,
     UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of Black 
           and Asian people.", 
     xlab = "log(EthAsian+EthBlack)",ylab="Deaths",log="x")

##
## Then, the impacts of covariates about people living in communal establishments
## on death counts will be investigated as suggested by our eda. We will now use 
## "PopComm" which defines the number of people living in communal establishments 
## in each MSOA, the relationship is not so good.
##

##Plot "PopComm" with "Deaths"
plot(UKCovid.model$PopComm,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of people living 
          in communal establishments.", 
     xlab = "PopComm",ylab="Deaths")

##Plot log of "PopComm" with "Deaths"
plot(UKCovid.model$PopComm,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of log of people  
          living in communal establishments.", 
     xlab = "log(PopComm)",ylab="Deaths",log="x")


##
## This time we will investigate covariates about public transport uses (i.e 
## "MetroUsers", "TrainUsers" and "BusUsers") as suggested by our eda. 
## Again, we will create a new covariate "Pubtrans" for the total number of public
## transport users in each MSOA. The relationship is not bad for "Pubtrans".
##

## Create "Pubtrans"
UKCovid.model$Pubtrans<-(UKCovid.model$MetroUsers+UKCovid.model$BusUsers+
                           UKCovid.model$TrainUsers)

##Plot of "Pubtrans" against "Deaths"
plot(UKCovid.model$Pubtrans,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and the number of people using public 
           transports.", 
     xlab = "Pubtrans",ylab="Deaths")

##Plot of log of "Pubtrans" against "Deaths"
plot(UKCovid.model$Pubtrans,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of the number of people using  
           public transports.", 
     xlab = "log(Pubtrans)",ylab="Deaths",log="x")

##
## The next part of our eda deals with covariates related to unpaid carers (i.e
## "CarersLo, CarersMid and CarersHi"). 
## Again, we investigate the effect of the proportion of people doing at least one 
## hour of unpaid work each week in each MSOA on death counts in these areas.
## 
## We will define a new covariate "Care.prop" denoting this proportion of people 
## delivering at least one hour of unpaid work each week.
## 
## Although this is not suggested by context information, more people doing unpaid
## caring might cause the spread of Covid-19 and hence causes more deaths. 
## The plots do show some relationship though.
##

##Define "Care.prop"
UKCovid.model$Care.prop<-(UKCovid.model$CarersLo+UKCovid.model$CarersMid+
                          UKCovid.model$CarersHi)/UKCovid.model$PopTot

##Plot "Care.prop" against "Deaths"
plot(UKCovid.model$Care.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and proportion of people delivering
           at least one hour of unpaid work per week.", 
     xlab = "Care.prop",ylab="Deaths")

##Plot log of "Care.prop" against "Deaths"
plot(UKCovid.model$Care.prop,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of proportion of people 
           delivering at least one hour of unpaid work per week.", 
     xlab = "log(Care.prop)",ylab="Deaths",log="x")

##
## The last bit to investigate via scatter plots is about Qualifications. People
## with different level of qualifications might have different attitudes towards
## Covid-19 and people with lower qualifications may tend to ignore the severity
## of COvid-19, which causes more spread of it and hence more death.
##
## We have defined a new covariate "Qual1_3" denoting the total number of people
## with highest level qualifications at 1 or 2 or 3. We've found some correlation
## between "Deaths" and log of "Qual1_3", log of "NoQual" and log of "Stud18."
##

##Define "Qual1_3"
UKCovid.model$Qual1_3<-UKCovid.model$Qual1+UKCovid.model$Qual2+UKCovid.model$Qual3

##Plot of "Qual1_3" against "Deaths"
plot(UKCovid.model$Qual1_3,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of people with
           highest level of qualifications at 1 or 2 or 3.", 
     xlab = "Qual1_3",ylab="Deaths")

##Plot of log of "Qual1_3" against "Deaths"
plot(UKCovid.model$Qual1_3,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of number of people with
           highest level of qualifications at 1 or 2 or 3.", 
     xlab = "log(Qual1_3)",ylab="Deaths",log="x")

##Plot of "NoQual" with "Deaths"
plot(UKCovid.model$NoQual,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of people with
           no academic or professional qualifications.", 
     xlab = "NoQual",ylab="Deaths")

##Plot of log of "NoQual" with "Deaths"
plot(UKCovid.model$NoQual,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of number of people with
           no academic or professional qualifications.", 
     xlab = "log(NoQual)",ylab="Deaths",log="x")

##Plot of "Stud18." with "Deaths"
plot(UKCovid.model$Stud18.,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and number of full-time student or
           schoolchild aged 18 or over.", 
     xlab = "Stud18.",ylab="Deaths")

##Plot of log of "Stud18." with "Deaths"
plot(UKCovid.model$Stud18.,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and log of number of full-time 
           student or schoolchild aged 18 or over.", 
     xlab = "log(Stud18.)",ylab="Deaths",log="x")

## Finally, produce a scatter plot of "Deaths" against "PopTot".
plot(UKCovid.model$PopTot,UKCovid.model$Deaths,
     main="Scatter Plot of number of deaths and population in each MSOA.", 
     xlab = "PopTot",ylab="Deaths")

##
## There may be some important information undiscovered among the other numerical variables,
## and we note that the occupation variables and social grade variables may be highly correlated,
## which is verifyed by the correlation table
##

##Correlation table of occupation and social grade variables
cat("correlation table:\n")
cat("===================\n")
print(cor(UKCovid.model[,61:73]))

##
## Hence we perform Principal component analysis on those covariates and we indeed find some 
## relationships, we introduce the first 3 PCs which explain 84.5% of total variance into our 
## dataset and rename them to "upper class", "lower class" and "middle class" respectively.
##

##Performing PCA and rotation matrix
workgradepc<-prcomp(UKCovid.model[,61:73],scale. = TRUE)
workgrademat<-workgradepc$rotation

## Summary information of PCA
cat("Summary of PCA:\n")
cat("====================\n")
print(workgradepc)
print(summary(workgradepc))


##Introducing new covariates
workgradex<-workgradepc$x
UKCovid.model$lower_class<-workgradex[,1]
UKCovid.model$upper_class<-workgradex[,2]
UKCovid.model$middle_class<-workgradex[,3]

##
## In addition to numerical variables, there are two categorical covariates "Region" and "RUCode"
## in our dataset. We plot box plots of death counts by them to see if there exists some relationships
## between them and the number of deaths.From the Deaths-Region graph, we can see that Northwest and 
## London obtain slightly higher death counts than other regions, and the death counts in Southeastern 
## areas are gently lower than others.
##

##Plot of "Region" with "Deaths" and save it.
plot(ggplot(UKCovid.model, aes(x=Region, y=Deaths, fill=Region)) +
       geom_boxplot() +
       ggtitle("Box plot of number of deaths and region") +
       xlab("")+theme(axis.text.x=element_text(angle=55,size=18),
                      axis.title.y=element_text(size=20),
                      plot.title = element_text(hjust=0.5,size=18),
                      legend.position = "none"))
dev.copy(pdf,"region.pdf",17,11)
dev.off()


##
## The Deaths-RUCode plot shows that MSOAs with different codes have similar deaths distributions, but the 
## numbers of deaths in MSOAs coded A1, which means Urban major conurbation, are a bit higher than in other 
## MSOAs. As a result, we decided to include them in our model.
##

##Plot of "RUCode" with "Deaths" and save it.
plot(ggplot(UKCovid.model, aes(x=RUCode, y=Deaths, fill=RUCode)) +
   geom_boxplot() +
   ggtitle("Box plot of number of deaths and RUCode") +
   xlab("")+theme(axis.text.x=element_text(size=20),
                  axis.title.y=element_text(size=20),
                  plot.title = element_text(hjust = 0.5,size=20),
                  legend.position = "none"))
dev.copy(pdf,"RUcode.pdf",17,11)
dev.off()

##
## From those plots, there is a MSOA containing extremely high death counts, which is
## more than 60 and could possibly be an outlier. The code below will catch out that
## observation.
##

cat("A possible outlier:\n")
cat("===================\n")
print(UKCovid.model[UKCovid.model$Deaths>=60,1:4])

########################################################################
# Third Step: Model Building and Analysis
########################################################################

##
## As demonstrated in our report, we will first try to fit a general lineat
## model as a start with "Deaths" as response variable and all the covariates
## suggested by our eda.
##

## Build the first general linear model
UKCovid.lm1<-lm(Deaths~PopTot+RUCode+Region+log(old.prop)+log(young.prop)+log(PopF)
                +log(Pubtrans)+log(HH_HealthPrb.prop)+log(Care.prop)+log(NoQual)
                +log(Qual1_3)+log(Stud18.)+middle_class+lower_class+upper_class,
                data=UKCovid.model)

## Summary of UKCovid.lm1
cat("Summary of UKCovid.lm1:\n")
cat("===================\n")
print(summary(UKCovid.lm1))

cat("AIC of UKCovid.lm1:\n")
cat("==================\n")
print(AIC(UKCovid.lm1))

## Let's specify our margin before actually doing the plots
par(mar=c(2.5,2.5,2.5,2.5))

## Produce diagnostic plot of UKCovid.lm1 and save it.
par(mfrow=c(2,2))
plot(UKCovid.lm1,which=1:4,cex.caption = 1.5)
dev.copy(pdf,"lm1plot.pdf",17,11)
dev.off()

## Build our second general linear model using a log transformation of
## "PopTot".
UKCovid.lm2<-lm(Deaths~log(PopTot)+RUCode+Region+log(old.prop)+log(young.prop)+log(PopF)
                +log(Pubtrans)+log(HH_HealthPrb.prop)+log(Care.prop)+log(NoQual)
                +log(Qual1_3)+log(Stud18.)+middle_class+lower_class+upper_class,
                data=UKCovid.model)

## Summary of UKCovid.lm2
cat("Summary of UKCovid.lm2:\n")
cat("===================\n")
print(summary(UKCovid.lm2))

cat("AIC of UKCovid.lm2:\n")
cat("==================\n")
print(AIC(UKCovid.lm2))

## Diagnostic plots for UKCovid.lm2
par(mfrow=c(2,2))
plot(UKCovid.lm2,which=1:4)

## Build our third general linear model using a square transformation 
## for "middle_class", "upper_class" and "lower_class"
UKCovid.lm3<-lm(Deaths~PopTot+RUCode+Region+log(old.prop)+log(young.prop)+log(PopF)
                +log(Pubtrans)+log(HH_HealthPrb.prop)+log(Care.prop)+log(NoQual)
                +log(Qual1_3)+log(Stud18.)+middle_class^2+lower_class^2+upper_class^2,
                data=UKCovid.model)

## Summary and ALC of UKCovid.lm3
cat("Summary of UKCovid.lm3:\n")
cat("===================\n")
print(summary(UKCovid.lm3))

cat("AIC of UKCovid.lm3:\n")
cat("===================\n")
print(AIC(UKCovid.lm3))

## Diagnostic plots for UKCovid.lm3
par(mfrow=c(2,2))
plot(UKCovid.lm3,which=1:4)

## Build our fourth general linear model using a square root transformation 
## for "PopTot".
UKCovid.lm4<-lm(Deaths~sqrt(PopTot)+RUCode+Region+log(old.prop)+log(young.prop)+log(PopF)
                +log(Pubtrans)+log(HH_HealthPrb.prop)+log(Care.prop)+log(NoQual)
                +log(Qual1_3)+log(Stud18.)+middle_class+lower_class+upper_class,
                data=UKCovid.model)

## Summary and ALC of UKCovid.lm4
cat("Summary of UKCovid.lm4:\n")
cat("===================\n")
print(summary(UKCovid.lm4))

cat("AIC of UKCovid.lm4:\n")
cat("===================\n")
print(AIC(UKCovid.lm4))

## Diagnostic plots for UKCovid.lm4
par(mfrow=c(2,2))
plot(UKCovid.lm4,which=1:4)

##
## Now we will try to fit generalised linear models to see whether a GLM can
## account for more variability than the general linear model above. We choose
## a Poisson GLM with a log link to ensure that our predicted expected values of 
## "Deaths" are positive. 
##

## Fit our first Poissson GLM with log link and name it as "UKCovid.glm1"
UKCovid.glm1<-glm(Deaths~PopTot+RUCode+Region+log(old.prop)+log(young.prop)+log(PopF)
                 +log(Pubtrans)+log(HH_HealthPrb.prop)+log(Care.prop)+log(NoQual)
                 +log(Qual1_3)+log(Stud18.)+middle_class+lower_class+upper_class,
                 family=poisson(link="log"), data=UKCovid.model)

## Generate summary information of UKCovid.glm1
cat("Summary of UKCovid.glm1:\n")
cat("===================\n")
print(summary(UKCovid.glm1))

## Generate diagnostic plots for UKCovid.glm1
par(mfrow=c(2,2))
plot(UKCovid.glm1,which = 1:4)

## Check for overdispersion
cat("Estimated variance of Pearson residuals for UKCovid.glm1:\n")
cat("================================================\n")
print(sum( resid(UKCovid.glm1,type="pearson")^2 )/UKCovid.glm1$df.residual)

##
## Now, as we have decided to stick to generalised linear model, we will start 
## with our UKCovid.glm1. We will firstly consider interactions between numerical 
## covariates and categorical covariates. We found from Chi-Squared tests that 
## there do exists interaction between "Region" and "log(Pubtrans)"; "Region" and 
## "log(HH_HealthPrb.prop)". We name the resultant model as UKCovid.glm2.
##

## Define UKCovid.glm2
UKCovid.glm2<-update(UKCovid.glm1, . ~ . +log(Pubtrans):Region+
                    log(HH_HealthPrb.prop):Region)

## ANOVA for UKCovid.glm2 and UKCovid.glm1
cat("ANOVA table for UKCovid.glm2 and UKCovid.glm1:\n")
cat("============================================\n")
print(anova(UKCovid.glm1,UKCovid.glm2,test="Chi"))

## Summary information of UKCovid.glm2 and its AIC
cat("Summary of UKCovid.glm2:\n")
cat("========================\n")
print(summary(UKCovid.glm2))

## Diagnostic plots for UKCovid.lm4
par(mfrow=c(2,2))
plot(UKCovid.glm2,which = 1:4)

##
## We then tried to find possible interactions between numerical variables.
## However, after trying multiple combinations, there isn't any significant
## improvement, so we decided not to add any other interactions between 
## numerical covariates.
##

## add interactions between "log(HH_HealthPrb.prop)" and "log(old.prop)"
cat("Summary information after adding 'log(HH_HealthPrb.prop):log(old.prop)':\n")
cat("==========================================================================\n")
print(summary(update(UKCovid.glm2,. ~ .+log(HH_HealthPrb.prop):log(old.prop) )))

## add interactions between "log(Pubtrans)" and log(old.prop)
cat("Summary information after adding 'log(Pubtrans):log(old.prop)':\n")
cat("==================================================================\n")
print(summary(update(UKCovid.glm2,. ~ .+log(Pubtrans):log(old.prop) )))

## add interactions between "log(NoQual)" and "log(HH_HealthPrb.prop)"
cat("Summary information after adding 'log(NoQual):log(HH_HealthPrb.prop)':\n")
cat("=======================================================================\n")
print(summary(update(UKCovid.glm2,. ~ .+log(NoQual):log(HH_HealthPrb.prop) )))

##
## Still, we aim to explain more variablity of the death data using our model.
## After adding interactions, another thing to do is to see whether the four
## groups of covariates ("HH_Depriv"s, ethnicity, "PopComm" and "PopM"). It 
## truns out that adding "EthBlack", "EthAsian" and "HHDepriv1" do improves our
## model. Then, we name the resultant models as UKCovid.glm3 (add "EthBlack" 
## and "EthAsian" to UKCovid.glm2 ) and UKCovid.glm4 (add "HHDepriv1" to 
## UKCovid.glm3).
##

## Create UKCovid.glm3, adding "EthBlack" and "EthAsian"
UKCovid.glm3<-update(UKCovid.glm2,. ~ .+EthBlack+EthAsian)

## Analysis of Variance Result
cat("ANOVA table for UKCovid.glm3 and UKCovid.glm2:\n")
cat("============================================\n")
print(anova(UKCovid.glm2,UKCovid.glm3,test="Chi"))

## Summary information and AIC of UKCovid.glm3
cat("Summary output of UKCovid.glm3:\n")
cat("==============================\n")
print(summary(UKCovid.glm3))

## Diagnostic plots of UKCovid.glm3
par(mfrow=c(2,2))
plot(UKCovid.glm3,which=1:4)

## add "HHDepriv1" to UKCovid.glm3
cat("Summary information after adding 'HHDepriv1':\n")
cat("=============================================\n")
print(summary(update(UKCovid.glm3,. ~ .+HHDepriv1 )))

## add "HHDepriv2" to UKCOvid.glm3
cat("Summary information after adding 'HHDepriv2':\n")
cat("=============================================\n")
print(summary(update(UKCovid.glm3,. ~ .+HHDepriv2 )))

## add "HHDepriv3" to UKCOvid.glm3
cat("Summary information after adding 'HHDepriv3':\n")
cat("=============================================\n")
print(summary(update(UKCovid.glm3,. ~ .+HHDepriv3 )))

## add "HHDepriv4" to UKCOvid.glm3
cat("Summary information after adding 'HHDepriv4':\n")
cat("=============================================\n")
print(summary(update(UKCovid.glm3,. ~ .+HHDepriv4 )))

## Create UKCovid.glm4, adding "HHDepriv1" to UKCovid.glm3
UKCovid.glm4<-update(UKCovid.glm3,. ~ .+HHDepriv1)

## ANOVA of UKCovid.glm3 and UKCovid.glm4
cat("ANOVA table for UKCovid.glm4 and UKCovid.glm3:\n")
cat("============================================\n")
print(anova(UKCovid.glm3,UKCovid.glm4,test="Chi"))

## Summary information and AIC of UKCovid.glm4
cat("Summary output of UKCovid.glm4:\n")
cat("==============================\n")
print(summary(UKCovid.glm4))

## Diagnostic plots of UKCovid.glm4
par(mfrow=c(2,2))
plot(UKCovid.glm4,which=1:4)

## Check for overdispersion
cat("Estimated variance of Pearson residuals for UKCovid.glm4:\n")
cat("=========================================================\n")
print(sum( resid(UKCovid.glm4,type="pearson")^2 )/UKCovid.glm4$df.residual)

##
## As overdispersion is observed, we will use a quasipoisson family as our final
## model we name it as "UKCovid.glm5"
##

UKCovid.glm5<-update(UKCovid.glm4,family=quasipoisson(link="log"))

## Diagnostic plots of UKCovid.glm5 and save it.
par(mfrow=c(2,2))
plot(UKCovid.glm5,which=1:4,cex.caption = 1.5)
dev.copy(pdf,"glm5plot.pdf",17,11)
dev.off()

## Calculate the proportion of deviance explained by our final model.
cat("The proportion of deviance explained by UKCovid.glm5:\n")
cat("=====================================================\n")
print((UKCovid.glm5$null.deviance-UKCovid.glm5$deviance)/UKCovid.glm5$null.deviance)

## Summary information of UKCovid.glm5
cat("Summary output of UKCovid.glm5:\n")
cat("==============================\n")
print(summary(UKCovid.glm5))

##
## Now we tried to still fit a Quasi-Poisson GLM with log link with all the 82 
## covariates(exclude "Deaths" and "ID") that are originally provided to
## us to see how good are these covariate in terms of explaining the 
## variability of the deaths counts and we name this model as UKCovid.test.
##

## Create UKCovid.test
UKCovid.test<-glm(Deaths~.,family = quasipoisson(link="log"),
                  data=UKCovid.model[2:84])

## Null deviance, residual deviance and proportion of deviance explained
## by UKCovid.test
cat("The null deviance of UKCovid.glm5:\n")
cat("==================================\n")
print(UKCovid.test$null.deviance)

cat("The residual deviance of UKCovid.glm5:\n")
cat("======================================\n")
print(UKCovid.test$deviance)

cat("The proportion of deviance explained by UKCovid.test:\n")
cat("=====================================================\n")
print((UKCovid.test$null.deviance-UKCovid.test$deviance)/UKCovid.test$null.deviance)

################################################################################
## Final step: Prediction
################################################################################

##
## We first need to create all the covariates that are used in our UKCovid.glm5
## model in the UKCovid.pred dateset.
##

UKCovid.pred$old.prop<-rowSums(cbind(UKCovid.pred[,36:40]),
                                na.rm=FALSE)/UKCovid.pred$PopTot

UKCovid.pred$young.prop<-rowSums(cbind(UKCovid.pred[,25:27]),
                                  na.rm=FALSE)/UKCovid.pred$PopTot

UKCovid.pred$HH_HealthPrb.prop<-UKCovid.pred$HH_HealthPrb/UKCovid.pred$HH

UKCovid.pred$Pubtrans<-(UKCovid.pred$MetroUsers+UKCovid.pred$BusUsers+
                            UKCovid.pred$TrainUsers)

UKCovid.pred$Care.prop<-(UKCovid.pred$CarersLo+UKCovid.pred$CarersMid+
                             UKCovid.pred$CarersHi)/UKCovid.pred$PopTot

UKCovid.pred$Qual1_3<-UKCovid.pred$Qual1+UKCovid.pred$Qual2+UKCovid.pred$Qual3

workgradepc.pred<-prcomp(UKCovid.pred[,61:73],scale. = TRUE)
workgradex.pred<-workgradepc.pred$x
UKCovid.pred$lower_class<-workgradex.pred[,1]
UKCovid.pred$upper_class<-workgradex.pred[,2]
UKCovid.pred$middle_class<-workgradex.pred[,3]

## Now create a new dataset with only covarites mentioned in oyr UKCovid.glm5
pred.fit<-UKCovid.pred[,c(2:3,5,7,18,45:46,77,84:93)]


## Do our prediction and find the standard error of our prediction.
glm5_pred<-predict(UKCovid.glm5,newdata = pred.fit,type = "response",
                           se.fit = TRUE)

## Estimated the variance of our prediction and name it as var.fit
var_fit<-glm5_pred$se.fit^2

##
## As we are using a Quasi-Poisson GLM, the variance of Y_i (observations of
## the deaths counts in UKCovid.pred) is simply our predictions. Hence, the 
## standard deviation of our prediction errors are simply the square root of
## the corresponding predictions plus the corresponding var.fit.
##

pred_error<-sqrt(var_fit+glm5_pred$fit)

##
## Create the final prediction dataframe and export it to .dat format as 
## stated in the instructions.
##

ICA2Group_D_pred<-data.frame(ID=UKCovid.pred$ID,Prediction=glm5_pred$fit,
                             Prediction_error=pred_error)

write.table(ICA2Group_D_pred, file="ICA2Group_D_pred.dat", col.names =FALSE, 
            row.names = FALSE, sep="\t", quote=FALSE)


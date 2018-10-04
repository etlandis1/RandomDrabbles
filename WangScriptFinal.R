#to set up the 3 Excel data files I did the following:
#I copied and pasted some sample data according to the template we last discussed
#and created 3 separate files
#SAMDATI2s.csv which has the data for the intervention groups
#SAMDATC2s.csv which has the data for the control groups
#SAMDATVAR.csv which has the moderator variables
#all 3 data files have the column "studyID" in common so that we can match them later  


#First, we need to generate some reaosnable estimates for the correlations ("r") 
#between pre- and posttest means for the intervention groups and control groups
#This is step 1

#The reason why we need to do this is because most of the studies do not report this correltation

#We need values for these correlations in order for "metafor" to calculate proper effect size
#estimates (in our case "SCMR" because we will standardize the raw scores since we have them)
#so that we can use "SCMR" as the effect size estimates in the meta-analysis 
#This is step 2

#Step 3 will be the actual mea-analysis

#So, to compute a reasonable estimate for "r", we will use the raw pre- and posttest means, 
#pre- and posttest standard deviations and pre- and posttest ns to calculate:

# (1) first the standardized mean differences betwen pre- and post-test raw means
#We need to standardize because different studies used different outcome measures
#Morris & DeShon (2002) justifies the use of Cohen's d as a reasonable metric to use

# (2) second convert Cohen's d to "rs" to get the correlations that we need for the 
#meta-analysis

#I modified the script to do these steps for you once you import the data files


#So let's start on Step 1! 


#install the package "compute.es" - we will use it to calculate the standardized mean
#differences and convert to "rs"
#install the package "data.table" so that we can more easily tidy up the data that we
#import from our Excel files

#Load the library "compute.es" and library "data.table"  

library(compute.es)
library(data.table)

#specify workspace where the data file is located (just specify the path on your harddrive
#after the "paste0(

workplace <- paste0("C:\\Users/Yuchen/Desktop/Data_Analysis_PSY_400\\")
setwd(workplace)

#Now, we are going to import our data files so that they can be read as data frames
#We are first going to create the data frames that we need to for step (1) above
#for the intervention group, then create an INVTERVENTION data frame "datI"
#Then afterward, we will repeat the steps to create a CONTROL data "datC"
#datI and datC will then be used for step 2.

#First we will specify the name of the data file and tell r to read the data from that file
#Once we have specified the file name
#Then we specify the name for the data frame obtained from the file
#Then we just want to inspect the data frame so that everything we want is there

file_name <- "SAMDATI2sp1.csv"
rsI <- read.csv(file_name,header=TRUE,sep=",")
rsI

#We just created the data frame "rsI" which has the Intervention group data

#Now we want to calculate estimates of d and convert those into correlations
#so we will use "compute.es" for that

rsI <- mes(m.1=PrM, m.2=PoM, sd.1=PrSD, sd.2=PoSD, n.1=N, 
n.2=N, level=95, cer=.02, dig=2, verbose=TRUE, id=studyID, data=rsI)
rsI

#Now we have estimates for d as well as the corresponding correlations
#As you can see from the output table, the "mes" command gives us a lot of effect size estimates
#We only want "r"

#Now we need to delete the columns we don't want
#Note here with "rsI[1:11]" we have to specify the rows that we want to extract values from
#so in your script you will just use "rsi[1:whatever is the number of the last row in the data file]

rsI <- rsI[1:19, c("id", "r"), drop=FALSE]
rsI

#Now let us rename the column "id" as "Studyid" so that we have a column that will match the original 
#"SAMDATI2s.csv file which has the raw means, sds and ns for the Intervention group
#that we can use to merge the two files (i.e., add the "r" column to that data frame)

names(rsI) <- c("studyID", "r")
rsI

#Now we will merge the two data frames to create a single data frame "SAMDATI2r" with all of the data
#we need for the intervention group

file_name <- "SAMDATI2sp2.csv"
dat_I <- read.csv(file_name,header=TRUE,sep=",")
dat_I

datI <- merge(dat_I, rsI, by=0, all=TRUE)
datI

#Now we have our "datI" data frame that we will need for step 2. We now need the data frame for the 
#group


#Now let us run through the above steps and create the same type of data frame but for the Control group


file_name <- "SAMDATC2sp2.csv"
rsC <- read.csv(file_name,header=TRUE,sep=",")
rsC

#Now we want to calculate estimates of d and convert those into correlations
#so we will use "compute.es" for that

rsC <- mes(m.1=PrM, m.2=PoM, sd.1=PrSD, sd.2=PoSD, n.1=N, 
n.2=N, level=95, cer=.02, dig=2, verbose=TRUE, id=studyID, data=rsC)
rsI

#Note here with "rsI[1:11] we have to specify the rows that we want so in your script you will just
#use "rsC[1:whatever is the number of the last row in the data file]

rsC <- rsC[1:9, c("id", "r"), drop=FALSE]
rsC

names(rsC) <- c("studyID", "r")
rsC

file_name <- "SAMDATC2sp2.csv"
dat_C <- read.csv(file_name,header=TRUE,sep=",")
dat_C

datC <- merge(dat_C, rsC, by=0, all=TRUE)
datC

#Now we have the 2 data frames that we will use for Step 2
#"SAMDATI2r" and "SAMDATC2r" that contain the pre- and posttest means,
#pre- and posttest sds, ns, and rs for the intervention and control groups
#respectively

#Step 2 (according to Morris (2008)
#(Morris, S. B. (2008). Estimating effect sizes from pretest-posttest-control 
#group designs. Organizational Research Methods, 11(2), 364–386.)
# We need to calculate the meta-analysis effect size parameter ("SMCR") and sample
#variances for the intervention group and control group separately because we have
#pre- and posttest data from the studies

#load "metafor" library

library(metafor)

#specify workspace where data file is located (just specify the path on your harddrive)

#workplace <- paste0("C:\\Users\\shont\\Desktop\\WorkspaceR\\")
#setwd(workplace)

#Now, we are going to import our data files so that they can be read as data frames

#specify name of data file and to read the data from that file
#Once we have specified the file name
#Then we specify the name for the data frame obtained from the file
#Then we just want to inspect the data frame so that everything we want is there

#file_name <- "SAMDATI2.csv"
#datI <- read.csv(file_name,header=TRUE,sep=",")
#datI

#file_name <- "SAMDATC2.csv"
#datC <- read.csv(file_name,header=TRUE,sep=",")
#datC

#So we now have 2 separate data frames
#One data frame with the intervention data and one with the control data

#Now we are going to calculate the standarized mean change estimates for each group, basically Posttest-Pretest

#this is the command for the intervention group
datI <- escalc(measure="SMCR", m1i=PoM, m2i=PrM, sd1i=PoSD, sd2i=PrSD, ni=N, 
ri=r, data=datI)
datI

#this is the command for the control group
datC <- escalc(measure="SMCR", m1i=PoM, m2i=PrM, sd1i=PoSD, sd2i=PrSD, ni=N, 
ri=r, data=datC)
datC

#Notice you will have 2 new columns in each data frame yi (the standardized mean change) and vi (the sample variance)

#Now we are going to calculate the standardized mean change estimates and corresponding sample variances (yi and vi) that reflect the differences between groups
#that we will use in the meta-analysis
#Then we will save those estimates to a new data frame "dat_est" 

dat_est <- data.frame(yi = datI$yi - datC$yi, vi = datI$vi + datC$vi)
round(dat_est, 2)
dat_est

#Now we will add a "studyID" column to the dat_est data frame so we can merge it with the .csv file with out moderator variables

dat_est$studyID <- c(1, 2, 2, 2, 3, 3, 4, 5, 5, 6, 6)
dat_est

#Now we will read out file with the moderator variables as a separate data frame
 
file_name <- "SAMDATVARp2.csv"
datVAR <- read.csv(file_name,header=TRUE,sep=",")
datVAR

#Now we will merge the data frame with our estimates with our data frame with the corresponding moderator variables

dat_all <- merge(dat_est, datVAR, by=0, all=TRUE)
dat_all

#Now we run the meta-analysis and specify our moderators and that we want for Hedges g to be used as our method

ma_model1 <- rma(yi, vi, mods=cbind(Year, Recr, Gender, Age, Country, Outcome, Cond, CTRL, INT), method="HE", data=dat_all)
summary(ma_model1)

#Here is an alternative to Hedge's, the default and somewhat better "REML"

ma_model1 <- rma(yi, vi, mods=cbind(Year, Recr, Gender, Age, Country, Outcome, Cond, CTRL, INT), method="REML", data=dat_all)
summary(ma_model1)

#Then we ask for our plots

forest(ma_model1, slab = paste(dat_all$studyID.x, as.character(dat_all$Year), sep = ", "))

funnel(ma_model1)

#We also need some fit statistics


#stuff Yuchen did
mamodelind <- rma(yi, vi, method="HE", data=dat_all)
summary(mamodelind)

#outlier, cookdist
cookdist <- cooks.distance(mamodelind)
cookdist
plot(cookdist,type="o", pch=19)
hatvalues(mamodelind)

#funnel
funnel(mamodelind)
taf <- trimfill(mamodelind)
funnel(taf)
summary(taf)
regtest.rma(mamodelind,  model="rma" , predictor="sei")
help(plot)

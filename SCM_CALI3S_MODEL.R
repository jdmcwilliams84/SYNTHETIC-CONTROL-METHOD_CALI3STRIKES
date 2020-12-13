# INDENTATION LEGEND BELOW
######## --> MACRO-LEVEL ACTIVITY  
####     --> SUBCATEGORY OF MACRO-LEVEL ACTIVITY
##       --> INDIVIDUAL ACTIVITY INSIDE SUBCATEGORY


######## DATA UPLOAD, PACKAGE DOWNLOAD, DATA PREP, NEW VARIABLE INTRODUCTION & SCRUBBING ########

#### DATA UPLOAD | PACKAGE DOWNLOAD ####
## SETTING WORKING DRIVE TO LOCAL AREA ##
## setwd()
## READING IN DATA ##
data <- read.csv("Dataupload.csv", header = TRUE, sep = ",")
## DOWNLOADING PACKAGES ## 
library(dplyr) # for 'Select' funtion later
library(kableExtra)
library(Synth)



#### DATA PREP ####
## PUTTING NA FOR BLANK AREAS ##
data[data==""] <- NA
## MAKING VARIABLE NUMERIC FOR EASE OF USE LATER ON ##
data$Med_Fam_Inc <- as.character(data$Med_Fam_Inc)
data$Med_Fam_Inc <- as.numeric(data$Med_Fam_Inc)



#### NEW VARIABLE INTRODUCTION ####
## CREATING NEW VARIABLE | THE PERCENTAGE OF THE BEGINNING BALANCE OF THE INCARCERATED POPULATION IS RELEASED EACH YEAR (AKA CHURN) ##
Jail_Pop_Churn <- data$Releases/(data$Releases-data$Addmissions+data$YE_Population)
## CREATING NEW VARIABLE | PROPORTION OF INCARCERATED PER 100k PERSONS ##
incarcerated_p100k <- data$YE_Population/data$Population*100000
## ADDING THE NEW VARIABLE TO DATASET ##
data <- data.frame(cbind(data, Jail_Pop_Churn, incarcerated_p100k))



#### DATA SCRUBBING & SUBSETTING ####
## ISOLATING DATA WITHIN 1978 AND 2014 AND CUTTING OUT STATES THAT ENACTED A THREE STRIKES LAW ##
## WE CAN NOT INCLUDE STATES WITH A THREE STRIKES LAW AS IT WOULD BE A FALSE COMPARISON FOR SYNTHETIC CONTROL ##
## CUTTING OUT DATA PRIOR TO 1978 AND POST 2014 TO MAXIMIZE DATA COVERAGE ## 
data78plus <- data[data$Years>=1978,]
data78_14 <- data78plus[data78plus$Years<=2014,]
data78_14[data78_14==""]<-NA




######## CREATING FINAL DATASET FOR ONLY NON-THREE STRIKE STATES ########
######## STATES WITH THREE STRIKES CAN NOT BE USED AS THEY ARE NOT EQUIVALENT COMPARISON ######## 

## SELECTING SUBSETS OF DATA BASED ON STATE ABBREVIATION ##
AL.data78_14 <- data78_14[data78_14$Shortname=="AL",]
AZ.data78_14 <- data78_14[data78_14$Shortname=="AZ",]
CA.data78_14 <- data78_14[data78_14$Shortname=="CA",]
DE.data78_14 <- data78_14[data78_14$Shortname=="DE",]
DC.data78_14 <- data78_14[data78_14$Shortname=="DC",]
HI.data78_14 <- data78_14[data78_14$Shortname=="HI",]
ID.data78_14 <- data78_14[data78_14$Shortname=="ID",]
IL.data78_14 <- data78_14[data78_14$Shortname=="IL",]
IA.data78_14 <- data78_14[data78_14$Shortname=="IA",]
KS.data78_14 <- data78_14[data78_14$Shortname=="KS",]
KY.data78_14 <- data78_14[data78_14$Shortname=="KY",]
LA.data78_14 <- data78_14[data78_14$Shortname=="LA",]
ME.data78_14 <- data78_14[data78_14$Shortname=="ME",]
MA.data78_14 <- data78_14[data78_14$Shortname=="MA",]
MI.data78_14 <- data78_14[data78_14$Shortname=="MI",]
MN.data78_14 <- data78_14[data78_14$Shortname=="MN",]
MS.data78_14 <- data78_14[data78_14$Shortname=="MS",]
MO.data78_14 <- data78_14[data78_14$Shortname=="MO",]
NE.data78_14 <- data78_14[data78_14$Shortname=="NE",]
NH.data78_14 <- data78_14[data78_14$Shortname=="NH",]
NY.data78_14 <- data78_14[data78_14$Shortname=="NY",]
ND.data78_14 <- data78_14[data78_14$Shortname=="ND",]
OH.data78_14 <- data78_14[data78_14$Shortname=="OH",]
OK.data78_14 <- data78_14[data78_14$Shortname=="OK",]
OR.data78_14 <- data78_14[data78_14$Shortname=="OR",]
RI.data78_14 <- data78_14[data78_14$Shortname=="RI",]
SD.data78_14 <- data78_14[data78_14$Shortname=="SD",]
TX.data78_14 <- data78_14[data78_14$Shortname=="TX",]
WV.data78_14 <- data78_14[data78_14$Shortname=="WV",]
WY.data78_14 <- data78_14[data78_14$Shortname=="WY",]
US.data78_14 <- data78_14[data78_14$Shortname=="USA",]

## CREATING LIST OF STATE ABBREVIATIONS TO LABEL FINAL DATAFRAME ##
no3s.list <- c("AL","AZ","CA","DE","DC","HI","ID","IL","IA","KS","KY","LA","ME","MA","MI","MN","MS","MO",
                "NE","NH","NY","ND","OH","OK","OR","RI","SD","TX","WV","WY")

## CONDENSING ALL NON-THREE STRIKE STATES INTO ONE FINAL DATASET ##
data.new <- data.frame(rbind(AL.data78_14, AZ.data78_14, CA.data78_14, DE.data78_14, DC.data78_14, HI.data78_14, ID.data78_14, IL.data78_14, 
IA.data78_14, KS.data78_14, KY.data78_14, LA.data78_14, ME.data78_14, MA.data78_14, MI.data78_14, MN.data78_14, MS.data78_14, MO.data78_14, 
NE.data78_14, NH.data78_14, NY.data78_14, ND.data78_14, OH.data78_14, OK.data78_14, OR.data78_14, RI.data78_14, SD.data78_14, TX.data78_14, 
WV.data78_14, WY.data78_14))




######## STARTING EXPLORATORY DATA ANALYSIS ########

#### TO UNDERSTAND GENERAL CRIME LEVEL WE WILL RANK AND GROUP VIOLENT #### 
#### CRIME BY STATES USING THE AVERAGE VIOLENT OCCURENCES PER 100k ####
## ANNUAL RANKING OF PRETREATMENT YEARS ##
years <- seq(from = 1978, to = 1993, by = 1)
Violent_rank <- 0
for (i in years){
  annual_data <- subset(data.new, Years == i)
  rank <- rank(annual_data$Violent_rate)
  Violent_rank <- cbind(Violent_rank, rank)
}
Violent_rank <- Violent_rank[,2:17] # CUTTING OUT FIRST COLUMN OF ZEROES

## AVERAGE OF ALL PRETREATMENT YEARS ##
states <- seq(from = 1, to = 30, by = 1)
avg_rank <- 0
for (i in states){
  avg <- round(mean(Violent_rank[i,]),1)
  avg_rank <- rbind(avg_rank, avg)
}
avg_rank <- avg_rank[2:31,] # CUTTING OUT FIRST ROW OF ZEROES

## MAKING FINAL DATA FRAME ##
shortname.list <- unique(data.new$Shortname)
shortname.list <- as.character(shortname.list)
Violent_rank <- data.frame(cbind(shortname.list, avg_rank))
colnames(Violent_rank) <- c("Shortname", "AVG_VIOLENT_RATE")

## LIST OF MOST VIOLENT STATES ##
worst10 <- subset(Violent_rank, avg_rank > 20)
worst10 <- worst10[order(worst10[,"AVG_VIOLENT_RATE"], worst10[,"Shortname"]),]

## LIST OF LEAST VIOLENT STATES ##
best10 <- subset(Violent_rank, avg_rank < 10)
best10 <- best10[order(best10[,"AVG_VIOLENT_RATE"], best10[,"Shortname"]),]

## PRINTING OUTCOME OF HIGHEST AND LOWEST CRIME RATES PER STATE ##
print("The states with highest violent occurences per 100K are:")
print(worst10)
print("The states with lowest violent occurences per 100K are:")
print(best10)
## It appears that states with lower population and/or states in the midwest have the lowest ##
## violent rate (of Three Strike states) while states with large populations tend to have ##
## higher crime rates (possibly tied to their large mega-cities like Chicago, NYC, 
## New Orleans, Houston, Washington DC).




#### TESTING CORRELATION OF PREDICTIVE VARIABLES  TO VIOLENT CRIME #### 
## TESTING CORRELATION OF VIOLENT OCCURENCES BETWEEN CALIFORNIA AND NON-THREE STRIKE STATES ##
## ONCE WE FIND STATES THAT ARE A GOOD REPRESENTATION OF CALIFORNIA (I.E. HIGHER CORRELATION ##
## WE WILL THEN TEST THEIR PREDICTOR VARIABLES TO SEE WHICH HAVE PREDICTIVE ABILITIES IN A MODEL ##
## WE CAN ONLY USE DATA BEFORE THREE STRIKES WAS INSTITUTED IN 1994 TO MEASURE CORRELATION ## 
## SINCE POST-TREATMENT CORRELATION IS NOT RELEVANT ## 
cor_data <- subset(data.new, Years < 1994) # cutting out data from after post treatment

## FINDING CORRELATION OF VIOLENT RATE ACROSS ACROSS ALL DONOR STATES (PREDICTED VARIABLE TO PREDICTED VARIABLE CORRELATION) ## 
state.range <- seq(1:30) # MAKING RANGE OF ALL NON THREE-STRIKES STATES
CA.cor_data <- subset(cor_data, Shortname == "CA") # ISOLATING CALIFORNIA SPECIFIC DATA  
Violent2Violent_cor <- 0 
for (i in state.range){
  correlate_me <- subset(cor_data, Shortname == no3s.list[i])
  correlationVio <- cor(CA.cor_data$Violent_rate, correlate_me$Violent_rate) 
  Violent2Violent_cor <- c(Violent2Violent_cor, correlationVio)
}
Violent2Violent_cor <- Violent2Violent_cor[2:31]
V2V_cor_table <- data.frame(cbind(no3s.list, Violent2Violent_cor))
V2V_cor_table$Violent2Violent_cor <- as.numeric(Violent2Violent_cor)
# USING AN ARBITRARY CUTOFF OF 0.83 AS A THRESHHOLD FOR GOOD CORRELATION
Donor_Goodcorrelation <- subset(V2V_cor_table, V2V_cor_table$Violent2Violent_cor > .8) # HIGH CORRELATION TO CALIFORNIA
Donor_Badcorrelation <- subset(V2V_cor_table, V2V_cor_table$Violent2Violent_cor < .8) # LOW CORRELATION TO CALIFORNIA
# EXTRACTING STATE NAMES WITH HIGH CORRELATION TO CALIFORNIA FOR FURTHER TESTING BELOW ##
DonorNames <- as.character(unique(Donor_Goodcorrelation$no3s.list))
name.range <- length(DonorNames) # 16 states of high correlation will be used to find effective predictive variables below



## USING THE LIST OF HIGHLY-CORRELATED STATES FOUND ABOVE (CORRELATION MEASURING PREDICTED VARIABLE TO PREDICTED VARIABLE) ##
## WE NOW MEASURE PREDICTED VARIABLE (VIOLENT RATE) TO PREDICTOR VARIABLES TO ASSESS WHICH WOULD BE MOST EFFECTIVE IN A PREDICTIVE MODEL ##
## EACH CORRELATION COEFFICIENT CALCULATED MEASURES EACH STATE'S VIOLENT RATE BY YEAR MEASURED AGAINST THE RESPECTIVE PREDICTOR VARIABLE ##
## WE STILL WILL ONLY USE PRE-TREATMENT YEARS (PRE 1994) TO ASSES THIS CORRELATION ##

## RUNNING EACH PREDICTIVE VARIABLE OF EACH STATE THROUGH A "FOR-LOOP" TO AGGREGATE CORRELATION
GDP_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationGDP <- cor(correlate_me$Violent_rate, correlate_me$GDP)
  GDP_Violent_cor <- rbind(GDP_Violent_cor, correlationGDP)
}
GDP_Violent_cor <- GDP_Violent_cor[2:length(GDP_Violent_cor),]

FamInc_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationFamInc <- cor(correlate_me$Violent_rate, correlate_me$Med_Fam_Inc)
  FamInc_Violent_cor <- rbind(FamInc_Violent_cor, correlationFamInc)
}
FamInc_Violent_cor <- FamInc_Violent_cor[2:length(FamInc_Violent_cor),]

Unemployment_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationUnEmploy <- cor(correlate_me$Violent_rate, correlate_me$Unemployment)
  Unemployment_Violent_cor <- rbind(Unemployment_Violent_cor, correlationUnEmploy)
}
Unemployment_Violent_cor <- Unemployment_Violent_cor[2:length(Unemployment_Violent_cor),]

EDU_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationEDU <- cor(correlate_me$Violent_rate, correlate_me$Education)
  EDU_Violent_cor <- rbind(EDU_Violent_cor, correlationEDU)
}
EDU_Violent_cor <- EDU_Violent_cor[2:length(EDU_Violent_cor),]

Urban_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationUrban <- cor(correlate_me$Violent_rate, correlate_me$Urban)
  Urban_Violent_cor <- rbind(Urban_Violent_cor, correlationUrban)
}
Urban_Violent_cor <- Urban_Violent_cor[2:length(Urban_Violent_cor),]

Churn_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationChurn <- cor(correlate_me$Violent_rate, correlate_me$Jail_Pop_Churn)
  Churn_Violent_cor <- rbind(Churn_Violent_cor, correlationChurn)
}
Churn_Violent_cor <- Churn_Violent_cor[2:length(Churn_Violent_cor),]

IncarceratedPct_Violent_cor <- 0
for (i in seq(1:name.range)){
  correlate_me <- subset(cor_data, Shortname == DonorNames[i])
  correlationIncPctPop <- cor(correlate_me$Violent_rate, correlate_me$incarcerated_p100k)
  IncarceratedPct_Violent_cor <- rbind(IncarceratedPct_Violent_cor, correlationIncPctPop)
}
IncarceratedPct_Violent_cor <- IncarceratedPct_Violent_cor[2:length(IncarceratedPct_Violent_cor),]


## CREATING MATRIX OF CORRELATIONS FOUND IN PREVIOUS STEP ##
cor.matrix <- matrix(cbind(GDP_Violent_cor, FamInc_Violent_cor, Unemployment_Violent_cor, EDU_Violent_cor, Urban_Violent_cor, Churn_Violent_cor, IncarceratedPct_Violent_cor), ncol = 7)
cor.matrix <- cor.matrix[1:15,] # taking away first row that defaults to zeros
rownames(cor.matrix) <- DonorNames # inputting only state names with high correlation to California
colnames(cor.matrix) <- c("GDP", "MedianIncome", "Unemployment", "Education", "Urban", "Jail_Churn", "Incarcerated%") # inputting variable (column) names


## AGGREGATING AND FINDING THE MEDIAN OF THE PREDICTOR VARIABLE CORRELATIONS FROM THE PREVIOUS STEP ##
## TO ASSESS THE UTILITY OF THESE VARIABLES AS PREDICTORS (FOR THE FINAL MODEL) ##
GDP_cor <- median(cor.matrix[,"GDP"]) # GDP
Med_Inc_cor <- median(cor.matrix[,"MedianIncome"]) # HH Median Income
Unemployment_cor <- median(cor.matrix[,"Unemployment"]) # Unemployment %
EDU_cor_all <- cor.matrix[,"Education"]  
Edu_cor <- median(EDU_cor_all[complete.cases(EDU_cor_all[])]) # EDU...using complete cases since some years were missing data
Urban_cor_all <- cor.matrix[,"Urban"]
Urban_cor <- median(Urban_cor_all[complete.cases(Urban_cor_all[])]) # Urban population...using complete cases since some years were missing data
Jail_Churn_cor <- median(cor.matrix[,"Jail_Churn"]) # Jail_churn
IncarceratedPop_cor <- median(cor.matrix[,"Incarcerated%"]) # % of population that is incarcerated
## CREATING CORRELATION TABLE ##
correlation.table <- data.frame(cbind(GDP_cor, Med_Inc_cor, Unemployment_cor, Edu_cor, Urban_cor, Jail_Churn_cor, IncarceratedPop_cor))
correlation.table <- t(correlation.table)
print(correlation.table)
## Using average correlation for all states over the pre-three strikes period yields the following (in ranked order): 
#1 Incarcerated % of Population ==> this intuitively makes sense as more violence leads to more arrests, more arrests leads to a higher % of the population in jail  
#2 % of Population that is Urban ==> this is positivly correlated to the violent rate  
#3 GDP per Capita ==> suprisingly this is positively correlated. Perhaps this is because states with higher populations tend to have higher GDPs but also higher violent crime levels 
#4 Education ==> Suprisingly this is positively correlated to the violence rate. This is opposite 
#5 Jail Churn ==> not very correlated to the violent rate (+0.45)  
#6 Median Income per HH ==> not very correlated to the violent rate (+0.31) 
#7 Unemployment Rate ==> this is the only variable that has negative correlation (which is a surprise)



#### VISUAL EDA ####

## CRIME CHARTS ##

dev.off()
par(mfrow=c(2,2))
## USA ALL VIOLENT CRIME ##
US.data <- data[data$Shortname=="USA",]
plot(US.data$Years, US.data$Violent_rate, las = TRUE, main = "US Violent Crimes", ylab = "Violent Crimes per 100K", 
xlab = "Year", cex.lab = 1.25, type = "l", col = "Red", lwd = 3.5, ylim = c(0, 800))
lines(US.data$Years, US.data$Murder_rate, type = "l", col = "Black", pch = 3)
lines(US.data$Years, US.data$Rape1_rate, type = "l", col = "Orange", pch = 3)
lines(US.data$Years, US.data$Robbery_rate, type = "l", col = "Forest Green", pch = 3)
lines(US.data$Years, US.data$AggAssault_rate, type = "l", col = "Grey", pch = 3)
legend("topright", inset=.025, cex = .85, c("All Violent Crime", "Aggravated Assault", "Robbery", "Rape", "Murder"), horiz=FALSE, lty=c(1,1), lwd=c(2,2), 
col=c("Red", "Grey", "Forest Green", "Orange", "Black"), bg="grey96")
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 100, "3 Strikes", pos=2)
arrows(1990, 100, 1993.5, 100)
## CA ALL VIOLENT CRIME ##
CA.data <- data[data$Shortname=="CA",]
plot(CA.data$Years, CA.data$Violent_rate, las = TRUE, main = "California Violent Crimes", ylab = "Violent Crimes per 100K", 
xlab = "Year", cex.lab = 1.25, type = "l", col = "Red", lwd = 3.5, ylim = c(0, 1250))
lines(CA.data$Years, CA.data$Murder_rate, type = "l", col = "Black", pch = 3)
lines(CA.data$Years, CA.data$Rape1_rate, type = "l", col = "Orange", pch = 3)
lines(CA.data$Years, CA.data$Robbery_rate, type = "l", col = "Forest Green", pch = 3)
lines(CA.data$Years, CA.data$AggAssault_rate, type = "l", col = "Grey", pch = 3)
legend("topright", inset=.025, cex = .85, c("All Violent Crime", "Aggravated Assault", "Robbery", "Rape", "Murder"), horiz=FALSE, lty=c(1,1), lwd=c(2,2), 
col=c("Red", "Grey", "Forest Green", "Orange", "Black"), bg="grey96")
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 100, "3 Strikes", pos=2)
arrows(1990, 100, 1993.5, 100)
## US AND CA CRIME ##
plot(US.data$Years, US.data$Violent_rate, las = TRUE, main = "Violent Crime: US Avg and CA", ylab = "Violent Crimes per 100K", cex.axis = 1,
cex.lab = 1.25, xlab = "Year", type = "l", col = "Grey", lwd = 3.5, ylim = c(0, 1250)) 
lines(CA.data$Years, CA.data$Violent_rate, type = "l", col = "Red3", lwd = 3.5)
legend("topright", inset=.025, cex = 1, c("CAL", "US Avg"), horiz=FALSE, 
lty=c(1, 1), lwd=c(2,2), col=c("Red3", "Grey"), bg="grey96")
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 100, "3 Strikes", pos=2)
arrows(1990, 100, 1993.5, 100)
## VIOLENT CRIME BY ALL STATE ##
AL.data <- data[data$Shortname=="AL",]
AK.data <- data[data$Shortname=="AK",]
AZ.data <- data[data$Shortname=="AZ",]
AR.data <- data[data$Shortname=="AR",]
CO.data <- data[data$Shortname=="CO",]
CT.data <- data[data$Shortname=="CT",]
DE.data <- data[data$Shortname=="DE",]
DC.data <- data[data$Shortname=="DC",]
FL.data <- data[data$Shortname=="FL",]
GA.data <- data[data$Shortname=="GA",]
HI.data <- data[data$Shortname=="HI",]
ID.data <- data[data$Shortname=="ID",]
IL.data <- data[data$Shortname=="IL",]
IN.data <- data[data$Shortname=="IN",]
IA.data <- data[data$Shortname=="IA",]
KS.data <- data[data$Shortname=="KS",]
KY.data <- data[data$Shortname=="KY",]
LA.data <- data[data$Shortname=="LA",]
ME.data <- data[data$Shortname=="ME",]
MD.data <- data[data$Shortname=="MD",]
MA.data <- data[data$Shortname=="MA",]
MI.data <- data[data$Shortname=="MI",]
MN.data <- data[data$Shortname=="MN",]
MS.data <- data[data$Shortname=="MS",]
MO.data <- data[data$Shortname=="MO",]
MT.data <- data[data$Shortname=="MT",]
NE.data <- data[data$Shortname=="NE",]
NV.data <- data[data$Shortname=="NV",]
NH.data <- data[data$Shortname=="NH",]
NJ.data <- data[data$Shortname=="NJ",]
NM.data <- data[data$Shortname=="NM",]
NY.data <- data[data$Shortname=="NY",]
NC.data <- data[data$Shortname=="NC",]
ND.data <- data[data$Shortname=="ND",]
OH.data <- data[data$Shortname=="OH",]
OK.data <- data[data$Shortname=="OK",]
OR.data <- data[data$Shortname=="OR",]
PA.data <- data[data$Shortname=="PA",]
RI.data <- data[data$Shortname=="RI",]
SC.data <- data[data$Shortname=="SC",]
SD.data <- data[data$Shortname=="SD",]
TN.data <- data[data$Shortname=="TN",]
TX.data <- data[data$Shortname=="TX",]
UT.data <- data[data$Shortname=="UT",]
VT.data <- data[data$Shortname=="VT",]
VA.data <- data[data$Shortname=="VA",]
WA.data <- data[data$Shortname=="WA",]
WV.data <- data[data$Shortname=="WV",]
WI.data <- data[data$Shortname=="WI",]
WY.data <- data[data$Shortname=="WY",]
plot(CA.data$Years, CA.data$Violent_rate, las = TRUE, main = "Violent Crime: All States", ylab = "Violent Crimes per 100K", cex.axis = 1,
cex.lab = 1.25, xlab = "Year", type = "l", col = "Red3", lwd = 3.5, ylim = c(0, 1250)) 
lines(AL.data$Years, AL.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(AK.data$Years, AK.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(AZ.data$Years, AZ.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(AR.data$Years, AR.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(CO.data$Years, CO.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(CT.data$Years, CT.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(DE.data$Years, DE.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(DC.data$Years, DC.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(FL.data$Years, FL.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(GA.data$Years, GA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(HI.data$Years, HI.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(ID.data$Years, ID.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(IL.data$Years, IL.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(IN.data$Years, IN.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(IA.data$Years, IA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(KS.data$Years, KS.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(KY.data$Years, KY.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(LA.data$Years, LA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(ME.data$Years, ME.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MD.data$Years, MD.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MA.data$Years, MA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MI.data$Years, MI.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MN.data$Years, MN.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MS.data$Years, MS.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MO.data$Years, MO.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(MT.data$Years, MT.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NE.data$Years, NE.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NV.data$Years, NV.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NH.data$Years, NH.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NJ.data$Years, NJ.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NM.data$Years, NM.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NY.data$Years, NY.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(NC.data$Years, NC.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(ND.data$Years, ND.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(OH.data$Years, OH.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(OK.data$Years, OK.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(OR.data$Years, OR.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(PA.data$Years, PA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(RI.data$Years, RI.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(SC.data$Years, SC.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(SD.data$Years, SD.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(TN.data$Years, TN.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(TX.data$Years, TX.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(UT.data$Years, UT.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(VT.data$Years, VT.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(VA.data$Years, VA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(WA.data$Years, WA.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(WV.data$Years, WV.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(WI.data$Years, WI.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
lines(WY.data$Years, WY.data$Violent_rate, lty = 1, type = "l", col = "grey", lwd = 1)
legend("topright", inset=.025, cex = 1, c("CA", "All Other States"), horiz=FALSE, 
lty=c(1, 3, 3, 3, 3), lwd=c(2, 2, 2, 2, 2), col=c("Red3", "Grey"), bg="grey96")
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 35, "3 Strikes", pos=2)
arrows(1990, 35, 1993.5, 35)




## VIOLENT CRIME AND GDP
dev.off()
par(mfrow=c(1,3))
par(mar=c(5.1, 4.1, 4.1, 7.1), mgp=c(3, 1, 0))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and GDP
US Avg and California
Pre-treatment 1978-1993", ylab = "Violent Crimes per 100K", 
xlab = "Year", cex.lab = 1.25, type = "l", col = "Grey", lwd = 3.5, ylim = c(200, 1250), beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 225, "3 Strikes", pos=2)
arrows(1990, 225, 1993.5, 225)
##
par(new = T)
plot(US.data78_14$GDP, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Grey", lty = 3)
par(new = T)
plot(CA.data78_14$GDP, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, at=c(20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000), 
labels=c("20K", "25K", "30K", "35K", "40K", "45K", "50K", "55K", "60K", "65K"))
mtext("GDP p/Cap", side=4, col="black")
legend("topleft", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA GDP", "USA GDP"), horiz=FALSE, 
lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3", "Grey"), bg="grey96")
## CORRELATION BETWEEN VIOLENT RATE & GDP
plot(cor_data$GDP, cor_data$Violent_rate,las = TRUE, col = "Navy Blue", main = "Violent Crime vs. GDP
All Donor States
Pre-treatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "GDP", ylab = "Violent Crimes per 100K")
## CORRELATION OF GDP TO VIOLENT RATE ##
hist(GDP_Violent_cor, breaks = 10, cex.lab = 1.25, col = "dark grey", border = FALSE, las = TRUE, main = "Violent Crime vs. GDP Correlations
All Donor States
Pre-treatment 1978-1993", xlab = "Correlation")
abline(v = GDP_cor, col = "Dark Red", lty=3, lwd=2)
text(.73, 2.5, "Median", pos=2)
arrows(.73, 2.5, .77, 2.5)



## VIOLENT CRIME AND MEDIAN FAMILY INCOME
dev.off()
par(mfrow=c(1,3))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Median Family Income
US Avg and California
Pre-treatment 1978-1993", ylab = "Violent Crimes per 100K", 
xlab = "Year", cex.lab = 1.25, type = "l", col = "Grey", lwd = 3.5, ylim = c(200, 1250), beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(1990, 225, "3 Strikes", pos=2)
arrows(1990, 225, 1993.5, 225)
##
par(new = T)
plot(CA.data78_14$Median_HH, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, at=c(50000, 55000, 60000, 65000, 70000, 75000, 85000, 90000), labels=c("50K", "55K", "60K", "65K", "70K", "75K", "80K", "85K", "90K"))
legend("topleft", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA EDU Index"), horiz=FALSE, 
lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3"), bg="grey96")
## CORRELATION OF HH MEDIAN INCOME TO VIOLENT RATE: SCATTER PLOT ##
plot(cor_data$Med_Fam_Inc, cor_data$Violent_rate, las = TRUE, col = "Navy Blue", main = "Violent Crime vs. Median Family Income 
All Donor States
Pretreatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "Median Income", ylab = "Violent Crimes per 100K")
## CORRELATION OF HH MEDIAN INCOME TO VIOLENT RATE: HISTOGRAM ##
hist(FamInc_Violent_cor, breaks = 10, col = "dark grey", cex.lab = 1.25, border = FALSE, las = TRUE, main = "Violent Crime vs. Family Income Correlations 
All Donor States
Pretreatment 1978-1993", xlab = "Correlation")
abline(v = Med_Inc_cor, col = "Dark Red", lty=3, lwd=2)
text(0.2, 4, "Median", cex = 1.5, pos=2)
arrows(.17, 4, .3, 4)




## VIOLENT CRIME AND UNEMPLOYMENT
dev.off()
par(mfrow=c(1,3))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Unemployment
US Avg and California
Pre-treatment 1978-1993", cex.lab = 1.25, 
ylab = "Violent Crimes per 100K", xlab = "Year", type = "l", col = "Grey", lwd = 3.5, ylim = c(200, 1250), beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(2005, 1200, "3 Strikes", pos=2)
arrows(2000, 1200, 1995, 1200)
##
par(new = T)
plot(US.data78_14$Unemployment, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Grey", lty = 3)
par(new = T)
plot(CA.data78_14$Unemployment, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, ylab = "Unemployment Rate")
legend("topleft", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA Unemployment", "USA Unemployment"), horiz=FALSE, 
lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3", "Grey"), bg="grey96")
## CORRELATION OF UNEMPLOYMENT TO VIOLENT RATE: SCATTER PLOT ##
plot(cor_data$Unemployment, cor_data$Violent_rate, las = TRUE, col = "Navy Blue", main = "Violent Crime vs. Unemployment
All Donor States
Pretreatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "Unemployment Rate", ylab = "Violent Crimes per 100K")
## CORRELATION OF UNEMPLOYMENT TO VIOLENT RATE: HISTOGRAM ##
hist(Unemployment_Violent_cor, breaks = 10, col = "dark grey", cex.lab = 1.25, border = FALSE, las = TRUE, main = "Violent Crime vs. Unemployment Correlations
All Donor States
Pretreatment 1978-1993",  xlab = "Correlation")
abline(v = Unemployment_cor, col = "Dark Red", lty=3, lwd=2)
text(-0.05, 2.5, "Median", pos=2)
arrows(-0.21, 2.5, -0.28, 2.5)




## VIOLENT CRIME AND EDUCATION
dev.off()
par(mfrow=c(1,3))
plot(CA.data78_14$Years, CA.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Education
(Education = Pct of Population 25+ YoA with HS Degree)
Pre-treatment 1978-1993", cex.lab = 1.25, 
ylab = "Violent Crimes per 100K", xlab = "Year", type = "l", col = "Red3", lwd = 3.5, ylim = c(200, 1250), beside = FALSE)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(2005, 1200, "3 Strikes", pos=2)
arrows(2000, 1200, 1995, 1200)
##
par(new = T)
plot(CA.data78_14$Education, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE)
legend("topleft", inset=.025, cex = .75, c("CA Violent Rate", "CA Education"), horiz=FALSE, 
lty=c(1, 3), lwd=c(2, 2), col=c("Red3", "Red3"), bg="grey96")
## VIOLENT RATE TO EDUCATION CORRELATION: SCATTER PLOT ## 
plot(cor_data$Education, cor_data$Violent_rate, las = TRUE, col = "Navy Blue", main = "Violent Rate vs. Education 
All Donor States
Pre-treatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "Pct of Population 25+ YoA with HS Degree", ylab = "Violent Crimes per 100K")
## VIOLENT RATE TO EDUCATION CORRELATION: HISTOGRAM ##
hist(EDU_Violent_cor, breaks = 10, cex.lab = 1.25, col = "dark grey", border = FALSE, las = TRUE, main = "Violent Crime vs. Education Correlations
All Donor States
Pre-treatment 1978-1993",  xlab = "Correlation")
EDU_cor_all <- cor.matrix[,"EDU_cor"]
abline(v = Edu_cor, col = "Red", lty=3, lwd=2)
text(.55, 1.5, "Median", pos=2)
arrows(.55, 1.5, .65, 1.5)




## VIOLENT CRIME AND URBAN LIVING
dev.off()
par(mfrow=c(1,3))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Urban Population
US Avg and California
Pre-treatment 1978-1993", cex.lab = 1.25, 
ylab = "Violent Crimes per 100K", xlab = "Year", type = "l", col = "Grey", ylim = c(200, 1250), lwd = 3.5, beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(2006, 1200, "3 Strikes", pos=2)
arrows(2000, 1200, 1995, 1200)
##
par(new = T)
plot(US.data78_14$Urban, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Grey", lty = 3)
par(new = T)
plot(CA.data78_14$Urban, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, ylab = "Unemployment Rate")
legend("topleft", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA Urban %", "USA Urban %"), horiz=FALSE, 
lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3", "Grey"), bg="grey96")
## VIOLENT RATE TO URBAN CORRELATION: SCATTER PLOT ##
plot(cor_data$Urban, cor_data$Violent_rate, las = TRUE, col = "Navy Blue", main = "Violent Rate vs. Urban % of Pop.
All Donor States
Pre-treatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "Urban % of Population", ylab = "Violent Crimes per 100K")
## VIOLENT RATE TO URBAN CORRELATION: HISTOGRAM ##
hist(Urban_Violent_cor, breaks = 10, col = "dark grey", cex.lab = 1.25, border = FALSE, las = TRUE, main = "Violent Crime vs. Urban Pop. Correlations
All Donor States
Pre-treatment 1978-1993",  xlab = "Correlation")
Urban_cor_all <- cor.matrix[,"Urban_cor"]
abline(v = Urban_cor, col = "Red", lty=3, lwd=2)
text(0.69, 5, "Median", pos=2)
arrows(.7, 5, .79, 5)




## VIOLENT CRIME AND JAIL CHURN ##
dev.off()
par(mfrow=c(1,3))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Jail Population Churn
US Avg and California
Pre-treatment 1978-1993", cex.lab = 1.25, ylab = "Violent Crimes per 100K", xlab = "Year", type = "l", col = "Grey", lwd = 3.5, ylim = c(0, 1250), beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(2005, 1200, "3 Strikes", pos=2)
arrows(2000, 1200, 1995, 1200)
##
par(new = T)
plot(US.data78_14$Jail_Pop_Churn, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Grey", lty = 3)
par(new = T)
plot(CA.data78_14$Jail_Pop_Churn, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, ylab = "Unemployment Rate")
legend("bottomleft", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA Jail Churn", "USA Jail Churn"), horiz=FALSE, 
lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3", "Grey"), bg="grey96")
## VIOLENT RATE TO JAIL POPULATION CHURN: SCATTER PLOT ##
plot(cor_data$Urban, cor_data$Jail_Pop_Churn, las = TRUE, col = "Navy Blue", main = "Violent Crime vs. Jail Pop. Churn 
All Donor States
Pre-treatment 1978-1993", cex.lab = 1.25, pch = 19, xlab = "Jail Population Churn", ylab = "Violent Crimes per 100K")
## VIOLENT RATE TO JAIL POPULATION CHURN: HISTOGRAM ##
hist(Churn_Violent_cor, breaks = 10, col = "dark grey", cex.lab = 1.25, border = FALSE, las = TRUE, main = "Violent Crime vs. Jail Pop. Churn Correlations 
All Donor States
Pre-treatment 1978-1993",  xlab = "Correlation")
abline(v = Jail_Churn_cor, col = "Red", lty=3, lwd=2)
text(.34, 2.5, "Median", pos=2)
arrows(0.34, 2.5, .42, 2.5)




## VIOLENT CRIME AND INCARCERATED POPULATION % ##
dev.off()
par(mfrow=c(1,3))
plot(US.data78_14$Years, US.data78_14$Violent_rate, las = TRUE, main = "Violent Crime and Incarcerated Population
US Avg and California
Pre-treatment 1978-1993", cex.lab = 1.25, ylab = "Violent Crimes per 100K", xlab = "Year", type = "l", 
col = "Grey", lwd = 3.5, ylim = c(0, 1250), beside = FALSE)
lines(CA.data78_14$Years, CA.data78_14$Violent_rate, lty = 1, type = "l", col = "Red3", lwd = 3.5)
abline(v = 1994, col="Navy Blue", lwd=2, lty=1)
text(2005, 400, "3 Strikes", pos=2)
arrows(2000, 400, 1995, 400)
##
par(new = T)
plot(US.data78_14$incarcerated_p100k, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Grey", lty = 3)
par(new = T)
plot(CA.data78_14$incarcerated_p100k, axes = F, xlab = NA, ylab = NA, type = "l", lwd = 1, col = "Red3", lty = 3)
axis(side = 4, las = TRUE, ylab = "Unemployment Rate")
legend("bottomright", inset=.025, cex = .75, c("CA Violent Rate", "US Violent Rate", "CA Incarceration %", "USA Incarceration %"), horiz=FALSE, 
       lty=c(1, 1, 3, 3), lwd=c(2, 2, 2, 2), col=c("Red3", "Grey", "Red3", "Grey"), bg="grey96")
# VIOLENT RATE AND POPULATION PROPORTION INCARCERATED
plot(cor_data$incarcerated_p100k, cor_data$Violent_rate, las = TRUE, col = "Navy Blue", main = "Violent Crime and Incarcerated Population
All Donor States
Pre-treatment 1978-1993", cex.lab = 1.25, pch = 19, ylab = "Violent Crimes per 100K", xlab = "Incarcerated p/100K")
## CORRELATION OF INCACERATED POPULATION TO VIOLENT RATE ##
hist(IncarceratedPct_Violent_cor, breaks = 10, col = "dark grey", cex.lab = 1.25, border = FALSE, las = TRUE, main = "Violent Crime vs. Incarcerated Pop. Correlations
All Donor States
Pre-treatment 1978-1993",  xlab = "Correlation")
abline(v = IncarceratedPop_cor, col = "Red", lty=3, lwd=2)
text(.83, 4, "Median", pos=2)
arrows(.83, 4, .88, 4)




dev.off()
## NON THREE-STRIKE STATES ##
# ERROR = CA, MA, DC, and HI are not colored in for some reason # 
require(maps)
c=c("Alabama", "Arizona", "Delaware", "District of Columbia", "Hawaii", "Idaho", "Illinois", "Iowa", "Kansas", "Kentucky", 
    "Louisiana", "Maine", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Nebraska", "New Hampshire", 
    "New York", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Rhode Island", "South Dakota", "Texas", "West Virginia", 
    "Wyoming")
namevec <- map(database = "state", col = "Red", fill=T, namesonly=TRUE)
map(database = "state", col = c("light grey", "Red")[1+(namevec %in% tolower(c))],fill=T)




###### MODEL CREATION ######

## CREATING NUMERICAL VALUE FOR "SYNTH" FUNCTION: FOR MODELING PURPOSES LATER ##
state.num <- rep(0, times = nrow(data78_14))
state.num[data78_14$Shortname == "AL"] <- 1
state.num[data78_14$Shortname == "AZ"] <- 2
state.num[data78_14$Shortname == "CA"] <- 3
state.num[data78_14$Shortname == "DE"] <- 4
state.num[data78_14$Shortname == "DC"] <- 5
state.num[data78_14$Shortname == "HI"] <- 6
state.num[data78_14$Shortname == "ID"] <- 7
state.num[data78_14$Shortname == "IL"] <- 8
state.num[data78_14$Shortname == "IA"] <- 9
state.num[data78_14$Shortname == "KS"] <- 10
state.num[data78_14$Shortname == "KY"] <- 11
state.num[data78_14$Shortname == "LA"] <- 12
state.num[data78_14$Shortname == "ME"] <- 13
state.num[data78_14$Shortname == "MA"] <- 14
state.num[data78_14$Shortname == "MI"] <- 15
state.num[data78_14$Shortname == "MN"] <- 16
state.num[data78_14$Shortname == "MS"] <- 17
state.num[data78_14$Shortname == "MO"] <- 18
state.num[data78_14$Shortname == "NE"] <- 19
state.num[data78_14$Shortname == "NH"] <- 20
state.num[data78_14$Shortname == "NY"] <- 21
state.num[data78_14$Shortname == "ND"] <- 22
state.num[data78_14$Shortname == "OH"] <- 23
state.num[data78_14$Shortname == "OK"] <- 24
state.num[data78_14$Shortname == "OR"] <- 25
state.num[data78_14$Shortname == "RI"] <- 26
state.num[data78_14$Shortname == "SD"] <- 27
state.num[data78_14$Shortname == "TX"] <- 28
state.num[data78_14$Shortname == "WV"] <- 29
state.num[data78_14$Shortname == "WY"] <- 30


## INPUTTING STATE NUMBER LIST INTO DATASET ##
data78_14 <- cbind(data78_14, state.num)
## CUTTING OUT ALL UNNECESSARY VARIABLES ##
data78_14$Shortname <- as.character(data78_14$Shortname)
## CUTTING OUT ALL UNNECESSARY VARIABLES##
data.new.violent <- select(data78_14, Shortname, Years, Violent_rate, GDP, Median_HH, Unemployment, Urban, Education, Jail_Pop_Churn, incarcerated_p100k, state.num)
### WHY IS SELECT NOT WORKING NOW?? ###


## 1. dataprep() for matrix-extraction
## 2. synth() for the construction of the synthetic control group
## 3. synth.tab(), gaps.plot(), and path.plot() to summarize the results
## Below we provide two examples.
## First Example: Toy panel dataset
# load data

#### MODEL 1 ####
dataprep.out.1var <-
  dataprep(
    foo = data.new.violent,
    predictors = c("incarcerated_p100k", "GDP", "Urban"),
    predictors.op = "mean",
    dependent = "Violent_rate",
    unit.variable = "state.num",
    unit.names.variable = "Shortname",
    time.variable = "Years",
    special.predictors = list(
    list("Median_HH", 1978:1993, "mean"),
    list("Jail_Pop_Churn", 1978:1993, "mean"),
    list("Unemployment", 1978:1993, "mean")
   ),
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4:30),
    time.predictors.prior = 1978:1993,
    time.optimize.ssr = 1978:1993,
    time.plot = 1978:2014
  )

## run the synth command to identify the weights that create the best possible synthetic
## control unit for the treated.
synth.out.1var <- synth(dataprep.out.1var)

## there are two ways to summarize the results we can either access the output from synth.out directly
## contains the unit weights or
round(synth.out.1var$solution.w,2)
## contains the predictor weights.
synth.out.1var$solution.v

## the output from synth opt can be flexibly combined with the output from dataprep to compute other quantities of interest
## for example, the period by period discrepancies between the treated unit and its synthetic control unit can be computed by typing
gaps.1var <- dataprep.out.1var$Y1plot-(dataprep.out.1var$Y0plot%*%synth.out.1var$solution.w)
model1var_error_pre <- mean(abs(gaps.1var[1:16]))
model1var_error_post <- mean(abs(gaps.1var[17:37]))
model1var_error_post/model1var_error_pre # Cali post treatment effect

## There are three convenience functions to summarize results:
## to get summary tables for all information (V and W weights plus balance btw. treated and synthetic control) use the synth.tab() command
synth.tables.1var <- synth.tab(dataprep.res = dataprep.out.1var, synth.res = synth.out.1var)
print(synth.tables.1var)

## to get summary plots for outcome trajectories of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands plot in levels (treated and synthetic)
dev.off()
par(mfrow=c(1,2))
path.plot(dataprep.res = dataprep.out.1var, synth.res = synth.out.1var, Xlab = c("Year"), Ylab = c("Violent Occurences p/100K People"))
abline(v = 1994, col="Blue", lwd=2, lty=2)
text(1990, 400, "3 Strikes", cex = 1.5, pos=2)
arrows(1990, 400, 1993.5, 400)

## same plot as above except the synthetic control is used as the x-axis
gaps.plot(dataprep.res = dataprep.out.1var, synth.res = synth.out.1var, Xlab = c("Year"), Ylab = c("Variance vs. Control: Violent Occurrences p/100K People"))
abline(v = 1994, col="Blue", lwd=2, lty=2)
text(1990, -90, "3 Strikes", pos=2)
arrows(1990, -90, 1993.5, -90)




## MODEL CHECK 1: USING PERMUTATION TEST FOR ALL OTHER STATES TO BE THE FAKE TREATMENT INSTEAD OF CALIFORNIA ##
prevpost_effect <- 0
ctl <- c(1:30)
for (i in ctl){
dataprep.out.check1 <-
  dataprep(
    foo = data.new.violent,
    predictors = c("incarcerated_p100k", "GDP", "Urban"),
    predictors.op = "mean",
    dependent = "Violent_rate",
    unit.variable = "state.num",
    unit.names.variable = "Shortname",
    time.variable = "Years",
    special.predictors = list(
      list("Median_HH", 1978:1993, "mean"),
      list("Jail_Pop_Churn", 1978:1993, "mean"),
      list("Unemployment", 1978:1993, "mean")
    ),
    treatment.identifier = ctl[i],
    controls.identifier = ctl[-which(ctl == ctl[i])],
    time.predictors.prior = 1978:1993,
    time.optimize.ssr = 1978:1993,
    time.plot = 1978:2014
  )

## run the synth command to identify the weights that create the best possible synthetic
## control unit for the treated.
synth.out.check1 <- synth(dataprep.out.check1)

## the output from synth opt can be flexibly combined with the output from dataprep to compute other quantities of interest
## for example, the period by period discrepancies between the treated unit and its synthetic control unit can be computed by typing
gaps.check1 <- dataprep.out.check1$Y1plot-(dataprep.out.check1$Y0plot%*%synth.out.check1$solution.w)
modelcheck1_error_pre <- mean(abs(gaps.check1[1:16])) # this is pre-treatment mean absolute error
modelcheck1_error_post <- mean(abs(gaps.check1[17:37])) # this is post-treatment mean absolute error
check_result <- modelcheck1_error_post/modelcheck1_error_pre
prevpost_effect <- c(prevpost_effect, check_result)
}


## VISUALIZING PERMUTATION MODEL CHECK ##
dev.off()
hist((prevpost_effect[2:length(prevpost_effect)]), breaks = 20, las = TRUE, border = FALSE, col = "Grey", main = "Measuring Treatment Effect
Results of the Treatment Permutation Test", 
xlab = "Distribution of Ratio
Post-treatment / Pre-treatment")
abline(v = prevpost_effect[4], col = "Red", lty=2, lwd=2)
text(3.5, 4.6, "California", cex = 1.5, pos=2, col = "Red")
arrows(3, 4.4, 2.73, 4.1, col = "red")




## MODEL CHECK 2: CHANGE TREATMENT YEAR TO BE 2000 ##
dataprep.out.check2 <-
  dataprep(
    foo = data.new.violent,
    predictors = c("incarcerated_p100k", "GDP", "Urban"),
    predictors.op = "mean",
    dependent = "Violent_rate",
    unit.variable = "state.num",
    unit.names.variable = "Shortname",
    time.variable = "Years",
    special.predictors = list(
      list("Median_HH", 1978:1999, "mean"),
      list("Jail_Pop_Churn", 1978:1999, "mean"),
      list("Unemployment", 1978:1999, "mean")
    ),
    treatment.identifier = 3,
    controls.identifier = c(1:2, 4:30),
    time.predictors.prior = 1978:1999,
    time.optimize.ssr = 1978:1999,
    time.plot = 1978:2014
)
## run the synth command to identify the weights that create the best possible synthetic
## control unit for the treated.
synth.out.check2 <- synth(dataprep.out.check2)

## there are two ways to summarize the results we can either access the output from synth.out directly
## contains the unit weights or
round(synth.out.check2$solution.w,2)
## contains the predictor weights.
synth.out.check2$solution.v

## the output from synth opt can be flexibly combined with the output from dataprep to compute other quantities of interest
## for example, the period by period discrepancies between the treated unit and its synthetic control unit can be computed by typing
gaps.check2 <- dataprep.out.check2$Y1plot-(dataprep.out.check2$Y0plot%*%synth.out.check2$solution.w)
modelcheck2_error_pre <- mean(abs(gaps.check2[1:22]))
modelcheck2_error_post <- mean(abs(gaps.check2[23:37]))
modelcheck2_error_post/modelcheck2_error_pre

## There are three convenience functions to summarize results:
## to get summary tables for all information (V and W weights plus balance btw. treated and synthetic control) use the synth.tab() command
synth.tables.check2 <- synth.tab(dataprep.res = dataprep.out.check2, synth.res = synth.out.check2)
print(synth.tables.check2)

## to get summary plots for outcome trajectories of the treated and the synthetic control unit use the
## path.plot() and the gaps.plot() commands plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out.check2, synth.res = synth.out.check2, Xlab = c("Year"), Ylab = c("Violent Occurences p/100K People"))
abline(v = 2000, col="Blue", lwd=2, lty=2)
text(1997, 400, "3 Strikes fake date", pos=2)
arrows(1997, 400, 1999, 400)

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out.check2, synth.res = synth.out.check2, Xlab = c("Year"), Ylab = c("Variance vs. Control: Violent Occurrences p/100K People"))
abline(v = 2000, col="Blue", lwd=2, lty=2)
text(1998, 100, "3 Strikes fake date", pos=2)
arrows(1998, 100, 1999.5, 100)

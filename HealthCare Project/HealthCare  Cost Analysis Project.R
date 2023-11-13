HospitalCosts <- read.csv("C:/Users/Deviare_User/Desktop/Project/Project/Projects for Submission/Healthcare/Healthcare/HospitalCosts.csv")
head(HospitalCosts)
summary(HospitalCosts)
summary(as.factor(HospitalCosts$AGE))
hist(HospitalCosts$AGE, main="Histogram of Age Group and their hospital visits",
     xlab="Age group", border="grey", col=c("light blue", "dark blue"), xlim=c(0,20), ylim=c(0,350))
ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data=HospitalCosts)
which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))
barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))
summary(as.factor(HospitalCosts$APRDRG))
DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = HospitalCosts)
DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]
summary(as.factor(HospitalCosts$RACE))
HospitalCosts = na.omit(HospitalCosts)
summary(as.factor(HospitalCosts$RACE))
raceInfluence=lm(TOTCHG~ RACE, data=HospitalCosts)
summary(raceInfluence)
raceInfluenceAOV <- aov(TOTCHG ~ RACE, data=HospitalCosts)
raceInfluenceAOV
summary(raceInfluenceAOV)

summary(HospitalCosts$FEMALE)
ageGenderInflModel = lm(TOTCHG ~ AGE + FEMALE + RACE, data = HospitalCosts)
summary(ageGenderInflModel)

ageGenderRaceInflModel = lm(LOS ~ AGE + FEMALE + RACE, data = HospitalCosts)
summary(ageGenderRaceInflModel)

HospitalCostModel = lm(TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = HospitalCosts)
summary(HospitalCostModel)

hcm1 = lm(TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = HospitalCosts)
summary(hcm1)

hcm2 = lm(TOTCHG ~ AGE + LOS + APRDRG, data = HospitalCosts)
summary(hcm2)

hcm3 = lm(TOTCHG ~ AGE + LOS, data = HospitalCosts)
summary(hcm3)

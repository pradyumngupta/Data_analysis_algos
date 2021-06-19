
df <- read.csv("D:/sem 6/Data Analytics/heart_failure_clinical_records_dataset.csv")
print(df)
View(df)


# sub_df1 <- subset(df1, select = c("gender","test.preparation.course", "math.score", "reading.score", "writing.score"))
dfSubset <- df[c(1:150),c(1,2, 4, 6, 7, 9, 10, 11, 13)]
dfSubset


sum(is.na(dfSubset))


MeanAge = mean(dfSubset$age)
MeanAge

MedianAge = median(dfSubset$age)
MedianAge

StdAge = sd(dfSubset$age)
StdAge

summary(dfSubset$age)


plot(dfSubset$age, dfSubset$DEATH_EVENT)
plot(dfSubset$anaemia, dfSubset$DEATH_EVENT)
plot(dfSubset$diabetes, dfSubset$DEATH_EVENT)
plot(dfSubset$high_blood_pressure, dfSubset$DEATH_EVENT)
plot(dfSubset$platelets, dfSubset$DEATH_EVENT)
plot(dfSubset$serum_sodium, dfSubset$DEATH_EVENT)
plot(dfSubset$sex, dfSubset$DEATH_EVENT)
plot(dfSubset$smoking, dfSubset$DEATH_EVENT)


#correlation  coeeficients

cor(dfSubset$age, dfSubset$anaemia)
cor(dfSubset$age, dfSubset$diabetes)
cor(dfSubset$age, dfSubset$high_blood_pressure)
cor(dfSubset$age, dfSubset$platelets)
cor(dfSubset$age, dfSubset$serum_sodium)
cor(dfSubset$age, dfSubset$sex)
cor(dfSubset$age, dfSubset$smoking)
cor(dfSubset$age, dfSubset$DEATH_EVENT)

cor(dfSubset$anaemia, dfSubset$diabetes)
cor(dfSubset$anaemia, dfSubset$high_blood_pressure)
cor(dfSubset$anaemia, dfSubset$platelets)
cor(dfSubset$anaemia, dfSubset$serum_sodium)
cor(dfSubset$anaemia, dfSubset$sex)
cor(dfSubset$anaemia, dfSubset$smoking)
cor(dfSubset$anaemia, dfSubset$DEATH_EVENT)


cor(dfSubset$diabetes, dfSubset$high_blood_pressure)
cor(dfSubset$diabetes, dfSubset$platelets)
cor(dfSubset$diabetes, dfSubset$serum_sodium)
cor(dfSubset$diabetes, dfSubset$sex)
cor(dfSubset$diabetes, dfSubset$smoking)
cor(dfSubset$diabetes, dfSubset$DEATH_EVENT)


cor(dfSubset$high_blood_pressure, dfSubset$platelets)
cor(dfSubset$high_blood_pressure, dfSubset$serum_sodium)
cor(dfSubset$high_blood_pressure, dfSubset$sex)
cor(dfSubset$high_blood_pressure, dfSubset$smoking)
cor(dfSubset$high_blood_pressure, dfSubset$DEATH_EVENT)


cor(dfSubset$platelets, dfSubset$serum_sodium)
cor(dfSubset$platelets, dfSubset$sex)
cor(dfSubset$platelets, dfSubset$smoking)
cor(dfSubset$platelets, dfSubset$DEATH_EVENT)


cor(dfSubset$serum_sodium, dfSubset$sex)
cor(dfSubset$serum_sodium, dfSubset$smoking)
cor(dfSubset$serum_sodium, dfSubset$DEATH_EVENT)


cor(dfSubset$sex, dfSubset$smoking)
cor(dfSubset$sex, dfSubset$DEATH_EVENT)


cor(dfSubset$smoking, dfSubset$DEATH_EVENT)

#Creating Correlation Matrix
matr_Cor <- cor(dfSubset, dfSubset)
matr_Cor


#creating relation

rel_age_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$age)
rel_age_death
png(file = "D:/sem 6/Data Analytics/age_death.png")
plot( dfSubset$age, dfSubset$DEATH_EVENT, main = "Age vs DEATH_EVENT", col = "red", abline(rel_age_death), cex = 1.2, pch = 16)
dev.off()

rel_ana_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$anaemia)
rel_ana_death
png(file = "C:\\Users\\hp india\\Desktop\\ana_death.png")
plot( dfSubset$anaemia, dfSubset$DEATH_EVENT, main = "Anaemia vs DEATH_EVENT", col = "red", abline(rel_ana_death), cex = 1.2, pch = 16)
dev.off()

rel_dia_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$diabetes)
rel_dia_death
png(file = "C:\\Users\\hp india\\Desktop\\dia_death.png")
plot( dfSubset$diabetes, dfSubset$DEATH_EVENT, main = "Diabetes vs DEATH_EVENT", col = "red", abline(rel_dia_death), cex = 1.2, pch = 16)
dev.off()

rel_hbp_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$high_blood_pressure)
rel_hbp_death
png(file = "C:\\Users\\hp india\\Desktop\\hbp_death.png")
plot( dfSubset$high_blood_pressure, dfSubset$DEATH_EVENT, main = "HighBloodPressure vs DEATH_EVENT", col = "red", abline(rel_hbp_death), cex = 1.2, pch = 16)
dev.off()

rel_plat_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$platelets)
rel_plat_death
png(file = "C:\\Users\\hp india\\Desktop\\plat_death.png")
plot( dfSubset$platelets, dfSubset$DEATH_EVENT, main = "Platlets vs DEATH_EVENT", col = "red", abline(rel_plat_death), cex = 1.2, pch = 16)
dev.off()

rel_sod_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$serum_sodium)
rel_sod_death
png(file = "C:\\Users\\hp india\\Desktop\\sod_death.png")
plot( dfSubset$serum_sodium, dfSubset$DEATH_EVENT, main = "Serum_Sodium vs DEATH_EVENT", col = "red", abline(rel_sod_death), cex = 1.2, pch = 16)
dev.off()

rel_sex_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$sex)
rel_sex_death
png(file = "C:\\Users\\hp india\\Desktop\\sex_death.png")
plot( dfSubset$sex, dfSubset$DEATH_EVENT, main = "Sex vs DEATH_EVENT", col = "red", abline(rel_sex_death), cex = 1.2, pch = 16)
dev.off()

rel_smok_death <- lm(dfSubset$DEATH_EVENT~ dfSubset$smoking)
rel_smok_death
png(file = "C:\\Users\\hp india\\Desktop\\smok_death.png")
plot( dfSubset$smoking, dfSubset$DEATH_EVENT, main = "Smoking vs DEATH_EVENT", col = "red", abline(rel_smok_death), cex = 1.2, pch = 16)
dev.off()


#outlier

out_age <- boxplot.stats(dfSubset$age)$out
out_age

out_anaemia <- boxplot.stats(dfSubset$anaemia)$out
out_anaemia

out_diabetes <- boxplot.stats(dfSubset$diabetes)$out
out_diabetes

out_hbp <- boxplot.stats(dfSubset$high_blood_pressure)$out
out_hbp

out_plats <- boxplot.stats(dfSubset$platelets)$out
out_plats

out_serum <- boxplot.stats(dfSubset$serum_sodium)$out
out_serum

out_sex <- boxplot.stats(dfSubset$sex)$out
out_sex

out_smok <- boxplot.stats(dfSubset$smoking)$out
out_smok

out_death <- boxplot.stats(dfSubset$DEATH_EVENT)$out
out_death





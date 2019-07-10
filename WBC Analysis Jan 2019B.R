healthData <- gs_url("https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/edit?usp=sharing")

forecastData <- gs_read(ss=healthData, ws = "Scenarios", skip = 0)


save(forecastData, file="forecastData.RData")


DatesOfNoExperimentalMeds <- filter(HospitalDates, `Cyclosporine (mg/day)` < 250)

PostThreeMonthCheckup <- filter(HospitalDates, Date >= "2018-08-01")

#Interpolate Missing Values
PostThreeMonthCheckup$`WBC (K/uL) Interpolated` <- na.interpolation(PostThreeMonthCheckup$`WBC (K/uL)`)
HospitalDates$`WBC (K/uL) Interpolated` <- na.interpolation(HospitalDates$`WBC (K/uL)`)

plot(HospitalDates$`WBC (K/uL) Interpolated`, type = "line")

#All Dates Linear Model
WBCPredEltromCyclofit <- lm(`WBC (K/uL) Interpolated` ~ `Prednisone (mg/day)` + `Eltrombopag (mg/day)` + `Cyclosporine (mg/day)`, data = HospitalDates, lwd=3)
summary(WBCPredEltromCyclofit)

#Post Three Month Checkup Dates Linear Mode
WBCPredEltromCyclofit_PostThreeMonthCheckup <- lm(PostThreeMonthCheckup$`WBC (K/uL) Interpolated` ~ `Prednisone (mg/day)` + `Eltrombopag (mg/day)` + `Cyclosporine (mg/day)`, data = PostThreeMonthCheckup)
PostThreeMonthCheckup$`WBC (K/uL) Interpolated` <- na.interpolation(PostThreeMonthCheckup$`WBC (K/uL)`)

summary(WBCPredEltromCyclofit_PostThreeMonthCheckup)

#Post Eltrombopag
PostEltrombopag <- filter(HospitalDates, Date >= "2018-09-07")
PostEltrombopag$`WBC (K/uL) Interpolated` <- na.interpolation(PostEltrombopag$`WBC (K/uL)`)
WBCPredEltromCyclofit_PostEltrombopag <- lm(PostEltrombopag$`WBC (K/uL) Interpolated` ~ `Prednisone (mg/day)` + `Eltrombopag (mg/day)` + `Cyclosporine (mg/day)`, data = PostEltrombopag)
summary(WBCPredEltromCyclofit_PostEltrombopag)





Prediction <- predict(WBCPredEltromCyclofit_PostThreeMonthCheckup, newdata = forecastData)
plot(Prediction)


#Look at Predisone outside of the experimental treatment drug period
WBCPredfit_LowOrNoExperimentalMeds <- lm(`WBC (K/uL)` ~ `Prednisone (mg/day)`, data = DatesOfNoExperimentalMeds)


##Clean R Markdown Files before upload
rmdfiles <- c("diseaseProbabilities.Rmd")
sapply(rmdfiles, knit, quiet = T)



firstDay <- as.Date(c("2018-01-22"))
lastDay <- Sys.Date()
lastDay <- format(lastDay, format="%Y-%m-%d")

#Get Data from Google Sheets
healthData <- gs_url("https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/edit?usp=sharing")
gs_ws_ls(healthData)
#Download Master Spreadsheet from GoogleSheets
AllHealthData <- gs_read(ss=healthData, ws = "AllDataNew", skip = 0)

#Convert Character to Numeric
AllHealthData[2:25] <- sapply(AllHealthData[2:25str()],as.numeric)
#Convert Data Character to Date
AllHealthData$Date <- as.Date(AllHealthData$Date, "%m/%d/%Y")
#Create Medications Table (This is probably Unnecessary)
AllMedicationsData <- subset(AllHealthData, Date >= "2017-10-01" & Date <= lastDay)
#Create Hospital Dates Table (This is probably Unnecessary)
HospitalDates <- subset(AllHealthData, Date >= firstDay & Date <= lastDay)
#Download Key Events Table
keyEvents <- gs_read(ss=healthData, ws = "KeyEvents", skip = 0)
#convert datable URLs to live links
keyEvents$`Associated Report` <- ifelse(grepl("http", keyEvents$`Associated Report`, fixed=TRUE), paste0("<a href='",keyEvents$`Associated Report`,"'target=”_blank”>", paste("Associated Report"),"</a>"), paste(" "))
keyEvents$Photo <- ifelse(grepl("http", keyEvents$Photo, fixed=TRUE), paste0("<a href='",keyEvents$Photo,"'target=”_blank”>", paste("Photo"),"</a>"), paste(" "))

#Save final tables locally
save(HospitalDates, file="HospitalDates.RData")
save(AllHealthData, file="AllHealthData.RData")
save(keyEvents, file="keyEvents.RData")

library(markdown)
library(plotly)
library(dplyr)
library(shiny)
library(googlesheets)
library(dplyr)
library(imputeTS)
library(sparkline)
library(rhandsontable)

#convert datable URLs to live links
function(input, output, session) {
  
  #Set First and Last Day
  firstDay <- as.Date(c("2018-01-22"))
  firstNIHDay <- as.Date(c("2018-5-3"))
  lastDay <- Sys.Date()
  lastDay <- format(lastDay, format="%Y-%m-%d")
  SevenDayBack <- Sys.Date()-7
  
  #Get Data from Google Sheets
  healthData <- gs_url("https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/edit?usp=sharing")
  gs_ws_ls(healthData)
  #Download Master Spreadsheet from GoogleSheets
  AllHealthData <- gs_read(ss=healthData, ws = "AllDataNew", skip = 0)
  
  #AllHealthData$`Neutrophils Count` <- ifelse(AllHealthData$`Neutrophils Count` == 0, "", AllHealthData$`Neutrophils Count`)
  
  
  #Convert Character to Numeric
  AllHealthData[2:51] <- sapply(AllHealthData[2:51],as.numeric)
  #Convert Data Character to Date
  AllHealthData$Date <- as.Date(AllHealthData$Date, "%m/%d/%Y")
  #Create Medications Table (This is probably Unnecessary)
  AllMedicationsData <- subset(AllHealthData, Date >= "2017-10-01" & Date <= lastDay)
  
  #Remove Zeros from Neutrophils column
  
  #Create Hospital Dates Table (This is probably Unnecessary)
  HospitalDates <- subset(AllHealthData, Date >= firstDay & Date <= lastDay)
  
  #Create Post NIH Dates Table
  NIHTrialDates <- subset(AllHealthData, Date >= firstNIHDay & Date <= lastDay)
  
  #Download Key Events Table
  keyEvents <- gs_read(ss=healthData, ws = "KeyEvents", skip = 0)
  #convert datable URLs to live links
  keyEvents$`Associated Report` <- ifelse(grepl("http", keyEvents$`Associated Report`, fixed=TRUE), paste0("<a href='",keyEvents$`Associated Report`,"'target=”_blank”>", paste("Associated Report"),"</a>"), paste(" "))
  keyEvents$Photo <- ifelse(grepl("http", keyEvents$Photo, fixed=TRUE), paste0("<a href='",keyEvents$Photo,"'target=”_blank”>", paste("Photo"),"</a>"), paste(" "))

    #Save final tables locally
  save(HospitalDates, file="HospitalDates.RData")
  save(AllHealthData, file="AllHealthData.RData")
  save(keyEvents, file="keyEvents.RData")
  save(NIHTrialDates, file="NIHTrialDates.RData")
  save(AllMedicationsData, file="AllMedicationsData.RData")
  
  

  ############### Change Viewed Panel with UpdateTabsetPanel #################
  
  
  observeEvent(input$link_to_tabpanel_b, {
    updateNavbarPage(session, "HealthSummaryTabs", "blood_test_results")
  })
  
  observeEvent(input$link_to_tabpanel_c, {
    updateNavbarPage(session, "HealthSummaryTabs", "blood_test_results")
  })
  observeEvent(input$link_to_tabpanel_d, {
    updateNavbarPage(session, "HealthSummaryTabs", "blood_test_results")
  })
  
  observeEvent(input$link_to_tabpanel_home, {
    updateNavbarPage(session, "HealthSummaryTabs", "home")
  })
  
  
  observeEvent(input$link_to_tabpanel_keyevents, {
    updateNavbarPage(session, "HealthSummaryTabs", "key_events")
  })
  
  ################ ABOUT PAGE STATUS QUO TEXT ##############################
  
  #WBC 7 Day Average
  WBCstatus <- mean(tail(HospitalDates$`WBC (K/uL)`, n = 7), na.rm = TRUE)
  output$WBCstatusText <- renderText({
      print(WBCstatus)
  })
  
  #RBC 7 Day Average
  RBCstatus <- mean(tail(HospitalDates$`RBCs (M/uL)`, n = 7), na.rm = TRUE)
  output$RBCstatusText <- renderText({
    print(RBCstatus)
  })
  
  #Last Update
  
  WeekDF <- AllHealthData %>% select(Date, `Platelets (K/uL)`, `WBC (K/uL)`, `RBCs (M/uL)`) %>% filter(Date > SevenDayBack & Date <=Sys.Date())

  
  
  ########################GRAPH BUILDING STARTS HERE#######################
  
  #KEY EVENTS TABLE  
  EventsLength <- length(keyEvents[[1]])
  output$keyEventsTable <- DT::renderDataTable({
    DT::datatable(keyEvents[c("Date", "EventDetails", paste("Associated Report"), "Photo")], escape = FALSE, options = list(pageLength = EventsLength, autoWidth = TRUE))
  })
  

  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  
  date <- list(
    title = "Date",
    titlefont = f
  )

  #Write function to plot single variable
  
  #Write function to plot single variable
  bulletOneVar  <- function(Data, Variable, color, dataName, graphTitle, start, end, tick) {
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Variable`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = dataName,
        line = list(color = color),
        hoverinfo = "text",
        text = ~ paste(dataName,"-", Variable, '<br>',
                       "Date - ", `Date`)
      ) %>%
      layout(
        margin = list(r=50),
        title = graphTitle,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = dataName,
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = tick,
          range = c(start, end),
          fixedrange = TRUE
        )
      )
    }

  #WBC Test Results
  
      #As function call
  
      #As fully written
  output$wbcResults <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `WBC (K/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = 'WBC (K/uL)',
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(' WBC (K/uL): ', `WBC (K/uL)`, '<br>',
                       "Date: ", `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        margin = list(r=50, b=200, t=50, pad=40),
        plot_bgcolor='rgb(217, 217, 217)',
        title = 'WBC Counts',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'WBC (K/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 1,
          range = c(0, 6),
          fixedrange = TRUE
        )
      )
  })
  
  
    
  wbc <- list(
    title = "WBC (K/uL)",
    titlefont = f)
  
  output$wbcResults <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `WBC (K/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = 'WBC (K/uL)',
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(' WBC (K/uL): ', `WBC (K/uL)`, '<br>',
                       "Date: ", `Date`)
      ) %>%  
      add_segments(x = firstDay, xend = Sys.Date(), y = 3.5, yend = 3.5) %>%
      config(displayModeBar = F) %>%
      layout(
        margin = list(r=50),
        title = 'WBC Counts',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'WBC (K/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 1,
          range = c(0, 6),
          fixedrange = TRUE
        )
      )
  })

  
  
#Plot RBC Results
  
  rbc <- list(
    title = "RBC (K/uL)",
    titlefont = f)

  output$rbcResults <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `RBCs (M/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = 'RBCs (M/uL)',
        yaxis = 'y2',
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(' RBCs (M/uL):', `RBCs (M/uL)`, '<br>',
                      'Transfusions (Units): ',`RBC Infusion (unit)`, '<br>',
                      'Date: ', `Date`)
                       
      ) %>%
      add_trace(
        x = ~ Date,
        y = ~ `RBC Infusion (unit)`,
        type = 'bar',
        name = 'Transfusions',
        marker = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(' RBCs (M/uL):', `RBCs (M/uL)`, '<br>',
                       'Transfusions (Units): ',`RBC Infusion (unit)`, '<br>',
                       'Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        margin = list(r=50),
        title = 'RBC Counts and Transfusions',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'right',
          title = 'Transfusions (Units)',
          showgrid = FALSE,
          zeroline = TRUE,
          dtick = 1,
          fixedrange = TRUE
        ),
        yaxis2 = list(
          side = 'left',
          overlaying = "y",
          title = 'RBCs (M/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 1,
          range = c(0, 5),
          fixedrange = TRUE
        )
      )
  })
  
#Plot Platelet Results
  
  plt <- list(
    title = "Platelets (K/uL)",
    titlefont = f)
  
  t <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("grey50"))
  
  output$pltResults <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Platelets (K/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = 'Platelets (K/uL)',
        yaxis = 'y2',
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(' Platelets (K/uL): ',`Platelets (K/uL)`, '<br>',
                       'Transfusion (Units): ',`Platelets Infusion (unit)`, '<br>',
                       'Date: ', `Date`)
      ) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Platelets Infusion (unit)`,
        type = 'bar',
        name = 'Transfusions',
        marker = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(' Platelets (K/uL): ',`Platelets (K/uL)`, '<br>',
                       'Transfusion (Units): ',`Platelets Infusion (unit)`, '<br>',
                       'Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        margin = list(r=50),
        title = 'Platelet Counts and Transfusions',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'right',
          title = 'Transfusions (Units)',
          showgrid = TRUE,
          zeroline = TRUE,
          dtick = 1,
          range = c(0, 2),
          fixedrange = TRUE
        ),
        yaxis2 = list(
          side = 'left',
          overlaying = "y",
          title = 'Platelets (K/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 10,
          range = c(0, 100),
          fixedrange = TRUE
        )
      )
  })

  #HGB Output  
  
  output$hgbResults <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Hgb (g/dL)`,
        type = 'scatter',
        mode = 'lines+markers',
        connectgaps = TRUE,
        name = 'Hgb (g/dL)',
        yaxis = 'y2',
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(' Hgb (g/dL):', `Hgb (g/dL)`, '<br>',
                       'Transfusions (Units): ',`RBC Infusion (unit)`, '<br>',
                       'Date: ', `Date`)
        
      ) %>%
      add_trace(
        x = ~ Date,
        y = ~ `RBC Infusion (unit)`,
        type = 'bar',
        name = 'Transfusions',
        marker = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(' RBCs (M/uL):', `RBCs (M/uL)`, '<br>',
                       'Transfusions (Units): ',`RBC Infusion (unit)`, '<br>',
                       'Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        margin = list(r=50),
        title = 'HGB Counts and Transfusions',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'right',
          title = 'Transfusions (Units)',
          showgrid = FALSE,
          zeroline = TRUE,
          dtick = 1,
          fixedrange = TRUE
        ),
        yaxis2 = list(
          side = 'left',
          overlaying = "y",
          title = 'Hgb (g/dL)',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 5,
          range = c(0, 20),
          fixedrange = TRUE
        )
      )
  })
  
  #output$hgbResults <- renderPlotly({
   # bulletOneVar(HospitalDates, HospitalDates$`Hgb (g/dL)`, "Green", "Hgb (g/dL)", "HGB Counts", 0, 20, 5)
    
  #})
  
  output$mcvResults <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`MCV (fL)`, "Blue", "MCV (f/L)", "MCV (f/L) Counts", 0, 150, 25)
    
  })
  
  output$nrbcResults <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`NRBC (X/100WC)`, "Orange", "NRBC (X/100wc)", "NRBC Results", 0, 10, 2)
    
  })

  
  output$rdwResults <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`RDW, RBC (%)`, "Aqua", "RDW, RBC (%)", "RDW, RBC (%)", 0, 100, 25)
  })

  output$hctResults <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`HCT (%)` , "Olive", "HCT (%)", "HCT Percent", 0, 100, 25)
    
  })
  
  output$ReticulocytesAbsoluteBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Reticulocytes, Absolute (k/uL)` , "Blue", "k/uL", "Reticulocytes, Absolute (K/uL)", 0, 100, 25)
  })
  
  output$ReticulocytesPercentBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Reticulocyte, %` , "Blue", "% of RBCs", "Reticulocytes (% of RBCs)", 0, 5, 1)
  })
  
  output$ReticulocytesHemoglobinBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Reticulocyte Hemoglobin (pg)` , "Blue", "pg", "Reticulocyte Hemoglobin (pg)", 0, 50, 10)
  })
  
  ################ WBC RESULTS #####################
  output$neutrophilsResults <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`NEUTROPHILS, CALCULATED (K/uL)`, "Purple", "Neutrophils, Calc (K/uL)", "Neutrophils Calculated", 0, 5, 1)
  })
  
  ############### OTHER TEST RESULTS FROM NIH TIME #############
  
  output$CyclosporineBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Cyclosporine (mcg/L)` , "Blue", "mcg/L", "Cyclosporine (mcg/L)", 0, 600, 200)
  })
  
  output$AlanineBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Alanine Transaminase (U/L)` , "Blue", "U/L", "Alanine Transaminase (U/L)", 0, 300, 100)
  })
  
  output$AspartateBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Aspartate Aminotransferase (U/L)` , "Blue", "U/L", "Aspartate Aminotransferase (U/L)", 0, 100, 25)
  })
  
  output$BilirubinTotalBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Bilirubin, Total. (mg/dL)` , "Blue", "mg/dL", "Bilirubin, Total. (mg/dL)", 0, 4, 1)
  })
  
  output$BilirubinDirectBloodTest <- renderPlotly({
    bulletOneVar(HospitalDates, HospitalDates$`Bilirubin, Direct.(mg/dL)` , "Blue", "mg/dL", "Bilirubin, Direct. (mg/dL)", 0, 4, 1)
  })
  
  
  ###############PLOT MEDICATION INTAKE#############

  Y <- list(
    title = "mg/day",
    titlefont = f)
  
  #Prednisone  
  
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  
  output$prednisoneIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Prednisone (mg/day)`,
        mode = 'lines+markers',
        name = 'Prednisone (mg/day))',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Prednisone (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Prednisone Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 40,
          range = c(0, 80),
          fixedrange = TRUE
        )
      )
  })
  
  
#Triamterene

  output$triamtereneIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Triamterene 37.5 mg-hydrochlorothiazide 25 mg tablet (Maxzide) (mg Triam/day)`,
        mode = 'lines+markers',
        name = 'Triamterene (Maxzide) (mg/day))',
        line = list(color = 'Orange'),
        marker = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(`Triamterene 37.5 mg-hydrochlorothiazide 25 mg tablet (Maxzide) (mg Triam/day)`, 'mg/day',  ", Date: ", `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Triamterene (Maxzide) Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 30,
          range = c(0, 60),
          fixedrange = TRUE
        )
      )
  })

#Bisoprolol
  
  output$bisoprololIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Bisoprolol fumarate (mg/day)`,
        mode = 'lines+markers',
        name = 'Bisoprolol fumarate (mg/day)',
        marker = list(color = 'Purple'),
        line = list(color = 'Purple'),
        hoverinfo = "text",
        text = ~ paste(`Bisoprolol fumarate (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Bisoprolol fumarate Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 5,
          range = c(0, 10),
          fixedrange = TRUE
        )
      )
  })
  
#Aspirin 
  
  output$aspirinIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Low-dose aspirin (mg/day)`,
        mode = 'lines+markers',
        name = 'Low-dose aspirin (mg/day)',
        marker = list(color = 'Pink'),
        line = list(color = 'Pink'),
        hoverinfo = "text",
        text = ~ paste(`Low-dose aspirin (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Low-dose Aspirin Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 50,
          range = c(0, 100),
          fixedrange = TRUE
        )
      )
  })
  
#Alendronate

  output$alendronateIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Alendronate (Fosamax) (mg/day)`,
        mode = 'lines+markers',
        name = 'Alendronate (Fosamax) (mg/day)',
        marker = list(color = 'light-blue'),
        line = list(color = 'light-blue'),
        hoverinfo = "text",
        text = ~ paste(`Alendronate (Fosamax) (mg/day)`,'(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Alendronate (Fosamax) Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 50,
          range = c(0, 100),
          fixedrange = TRUE
        )
      )
  })
  
  #Vitamin D-3
  
  output$d3Intake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Vitamin D-3 (unit/day)`,
        mode = 'lines+markers',
        name = 'Vitamin D-3',
        marker = list(color = 'olive'),
        line = list(color = 'olive'),
        hoverinfo = "text",
        text = ~ paste(`Vitamin D-3 (unit/day)`, '(unit/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Vitamin D-3 (unit/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'unit/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 2500,
          range = c(0, 5000),
          fixedrange = TRUE
        )
      )
  })
  
  #Acetazolamide Intake
  
  output$acetazolamideIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Acetazolamide (Diamox) (mg/day)`,
        mode = 'lines+markers',
        name = 'Acetazolamide (Diamox) (mg/day)',
        marker = list(color = 'maroon'),
        line = list(color = 'maroon'),
        hoverinfo = "text",
        text = ~ paste(`Vitamin D-3 (unit/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Acetazolamide (Diamox) Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 500,
          range = c(0, 1000),
          fixedrange = TRUE
        )
      )
  })
  
  #Methazolamide
  
  output$methazolamideIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Methazolamide (Neptazane) (mg/day)`,
        mode = 'lines+markers',
        name = 'Methazolamide (Neptazane) (mg/day)',
        marker = list(color = 'navy'),
        line = list(color = 'navy'),
        hoverinfo = "text",
        text = ~ paste(`Methazolamide (Neptazane) (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Methazolamide (Neptazane) Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 100,
          range = c(0, 200),
          fixedrange = TRUE
        )
      )
  })
  
  #Cefadroxil
  output$cefadroxilIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Cefadroxil (mg/day)`,
        mode = 'lines+markers',
        name = 'Cefadroxil (mg/day)',
        marker = list(color = 'red'),
        line = list(color = 'red'),
        hoverinfo = "text",
        text = ~ paste(`Cefadroxil (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Cefadroxil Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 500,
          range = c(0, 1000),
          fixedrange = TRUE
        )
      )
  })
  
  #Sulfamethoxazole 
  output$sulfamethoxazoleIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Sulfamethoxazole 800 mg-trimethoprim 160 mg tablet (mg sulfa/day)`,
        mode = 'lines+markers',
        name = 'Sulfamethoxazole 800 mg-trimethoprim 160 mg tablet (mg sulfa/day)',
        marker = list(color = 'black'),
        line = list(color = 'black'),
        hoverinfo = "text",
        text = ~ paste(`Sulfamethoxazole 800 mg-trimethoprim 160 mg tablet (mg sulfa/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Sulfamethoxazole Levels',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg sulfa/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 500,
          range = c(0, 1000),
          fixedrange = TRUE
        )
      )
  })
  
  #Multivitamin 
  output$multivitaminIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Multivitamin (tablet)`,
        mode = 'lines+markers',
        name = 'Multivitamin (tablet)',
        marker = list(color = 'aqua'),
        line = list(color = 'aqua'),
        hoverinfo = "text",
        text = ~ paste(`Multivitamin (tablet)`, '(tablet)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Multivitamin (tablet)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'Tablets',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 1,
          range = c(0, 2),
          fixedrange = TRUE
        )
      )
  })
  
  output$lisonoprilIntake <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Lisinopril (mg/day)`,
        mode = 'lines+markers',
        name = 'Lisinopril (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Lisinopril (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Lisinopril (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 25,
          range = c(0, 100),
          fixedrange = TRUE
        )
      )
  })
  
  output$Horse_ATG <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Horse ATG (mg/day)`,
        mode = 'lines+markers',
        name = 'Horse ATG (mg/day)',
        marker = list(color = 'Brown'),
        line = list(color = 'Brown'),
        hoverinfo = "text",
        text = ~ paste(`Horse ATG (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Horse ATG (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 1000,
          range = c(0, 4000),
          fixedrange = TRUE
        )
      )
  })
  
  output$Cyclosporine <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Cyclosporine (mg/day)`,
        mode = 'lines+markers',
        name = 'Horse ATG (mg/day)',
        marker = list(color = 'Brown'),
        line = list(color = 'Brown'),
        hoverinfo = "text",
        text = ~ paste(`Cyclosporine (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Cyclosporine (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 250,
          range = c(0, 1000),
          fixedrange = TRUE
        )
      )
  })
  
  
  
  output$Eltrombopag <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Eltrombopag (mg/day)`,
        mode = 'lines+markers',
        name = 'Horse ATG (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Eltrombopag (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Eltrombopag (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 100,
          range = c(0, 300),
          fixedrange = TRUE
        )
      )
  })
  
  output$Methylprednisolone <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Methylprednisolone (mg/day)`,
        mode = 'lines+markers',
        name = 'Methylprednisolone (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Methylprednisolone (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Methylprednisolone (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 100,
          range = c(0, 300),
          fixedrange = TRUE
        )
      )
  })
  
  output$Diphenhydramine <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Diphenhydramine (mg/day)`,
        mode = 'lines+markers',
        name = 'Diphenhydramine (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Diphenhydramine (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Diphenhydramine (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 100,
          range = c(0, 300),
          fixedrange = TRUE
        )
      )
  })
  
  output$Pantoprazole <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Pantoprazole (mg/day)`,
        mode = 'lines+markers',
        name = 'Pantoprazole (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Pantoprazole (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Pantoprazole (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 100,
          range = c(0, 300),
          fixedrange = TRUE
        )
      )
  })
  
  output$Valacyclovir <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Valacyclovir (mg/day)`,
        mode = 'lines+markers',
        name = 'Valacyclovir (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Valacyclovir (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Valacyclovir (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 250,
          range = c(0, 1000),
          fixedrange = TRUE
        )
      )
  })
  
  output$Ceftazidime <- renderPlotly({
    plot_ly(AllMedicationsData) %>%
      add_trace(
        x = ~ Date,
        y = ~ `Ceftazidime (mg/day)`,
        mode = 'lines+markers',
        name = 'Ceftazidime (mg/day)',
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        hoverinfo = "text",
        text = ~ paste(`Ceftazidime (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      config(displayModeBar = F) %>%
      layout(
        title = 'Ceftazidime (mg/day)',
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        xaxis = list(title = "", fixedrange = TRUE),
        yaxis = list(
          side = 'left',
          title = 'mg/day',
          showgrid = TRUE,
          zeroline = TRUE,
          autotick = F,
          dtick = 2500,
          range = c(0, 10000),
          fixedrange = TRUE
        )
      )
  })
  
  ####################ANALYSIS PAGES###############################

  
  #Run Linear Regression
  WBCPredfit <- lm(`WBC (K/uL)` ~ `Prednisone (mg/day)`, data = HospitalDates)
  
  NeutroPredfit <- lm(`NEUTROPHILS, CALCULATED (K/uL)` ~ `Prednisone (mg/day)`, data = HospitalDates)
  
  RBCPredfit <- lm(`RBCs (M/uL)` ~ `Prednisone (mg/day)`, data = HospitalDates)
  
  
  #Print Regression Results
  output$WBCprednisoneRegression <- renderPrint(summary(WBCPredfit)) 
  output$NeutroPrednisoneRegression <- renderPrint(Summary(NeutroPredfit))
  
  #Create Scatterplot
  output$WBCandPrednisone <- renderPlotly({
    plot_ly(
      data = HospitalDates,
      x = HospitalDates$`WBC (K/uL)`,
      y = HospitalDates$`Prednisone (mg/day)`,
      marker = list(size = 10, color = 'Purple')
    ) %>%
      #config(displayModeBar = F) %>%
      layout(
        title = 'Prednisone vs WBC Counts',
        yaxis = list(
          title = 'Prednisone (mg/day)',
          zeroline = FALSE,
          range = c(0, 80)
        ),
        xaxis = list(
          title = 'WBC (K/uL)',
          zeroline = FALSE,
          range = c(0, 5)
        )
      )
  })
  
  #Create Dual Axis Line Graph of WBCs and Prednisone
  output$PredWBCtime <- renderPlotly({
    plot_ly(HospitalDates) %>%
      add_trace(
        x = ~ Date,
        y = HospitalDates$`WBC (K/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        name = "WBC Count",
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        connectgaps = TRUE,
        hoverinfo = "text",
        text = ~ paste(HospitalDates$`WBC (K/uL)`, '(K/uL)',', Date: ', `Date`)
      ) %>%
      add_trace(
        x = ~ Date,
        y = HospitalDates$`Prednisone (mg/day)`,
        type = 'scatter',
        mode = 'lines+markers',
        name = "Prednisone (mg/day)",
        marker = list(color = 'Orange'),
        yaxis = 'y2',
        line = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(HospitalDates$`Prednisone (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      add_trace(
        x = ~ Date,
        y = HospitalDates$`Methylprednisolone (mg/day)`,
        type = 'scatter',
        mode = 'lines+markers',
        name = "Methylprednisolone (mg/day)",
        marker = list(color = 'Red'),
        yaxis = 'y2',
        line = list(color = 'Red'),
        hoverinfo = "text",
        text = ~ paste(HospitalDates$`Methylprednisolone (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      #config(displayModeBar = F) %>%
      layout(
        title = 'WBC Counts and Prednisone Intake Over Time',
        xaxis = list(title = ""),
        yaxis = list(
          side = 'left',
          title = 'WBC (K/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(0, 5)
        ),
        yaxis2 = list(
          side = 'right',
          overlaying = "y",
          title = 'Prednisone Type (mg/day)',
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(0, 80)
        ),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        margin = list(r=50)
      
        )
  })
  
  ####################Eltrombopag ANALYSIS PAGES###############################
  PlateletsEltromfit <- lm(NIHTrialDates$`Platelets (K/uL)` ~ NIHTrialDates$`Eltrombopag (mg/day)`)
  
  
  #Print Regression Results
  output$PlaleletsRegression <- renderPrint(summary(PlateletsEltromfit)) 
  
  
  #Create Scatterplot
  output$PlateletsandEltrombopag <- renderPlotly({
    plot_ly(
      data = HospitalDates,
      x = NIHTrialDates$`Platelets (K/uL)`,
      y = NIHTrialDates$`Eltrombopag (mg/day)`,
      marker = list(size = 8, color = 'Purple')
    ) %>%
      #config(displayModeBar = F) %>%
      layout(
        title = 'Eltrombopag vs Platelet Counts',
        yaxis = list(
          title = 'Etrombopag (mg/day)',
          zeroline = FALSE,
          range = c(0, 200)
        ),
        xaxis = list(
          title = 'Platelets (K/uL)',
          zeroline = FALSE,
          range = c(0, 100)
        )
      )
  })
  
  #Create Dual Axis Line Graph of Platelets and Eltrompobag
  output$PlateteletsEltromTime <- renderPlotly({
    plot_ly(NIHTrialDates) %>%
      add_trace(
        x = ~ Date,
        y = NIHTrialDates$`Platelets (K/uL)`,
        type = 'scatter',
        mode = 'lines+markers',
        name = "Platelet Count",
        marker = list(color = 'Blue'),
        line = list(color = 'Blue'),
        connectgaps = TRUE,
        hoverinfo = "text",
        text = ~ paste(NIHTrialDates$`Platelets (K/uL)`, '(K/uL)',', Date: ', `Date`)
      ) %>%
      add_trace(
        x = ~ Date,
        y = NIHTrialDates$`Eltrombopag (mg/day)`,
        type = 'scatter',
        mode = 'lines+markers',
        name = "Eltrombopag (mg/day)",
        marker = list(color = 'Orange'),
        yaxis = 'y2',
        line = list(color = 'Orange'),
        hoverinfo = "text",
        text = ~ paste(NIHTrialDates$`Eltrombopag (mg/day)`, '(mg/day)',', Date: ', `Date`)
      ) %>%
      #config(displayModeBar = F) %>%
      layout(
        title = 'Platelet Counts and Eltrombopag Intake Over Time',
        xaxis = list(title = ""),
        yaxis = list(
          side = 'left',
          title = 'Platelets (K/uL)',
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(0, 60)
        ),
        yaxis2 = list(
          side = 'right',
          overlaying = "y",
          title = 'Eltrompobag (mg/day)',
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(0, 200)
        ),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5),
        margin = list(r=50)
        
      )
  })
  
  
  }
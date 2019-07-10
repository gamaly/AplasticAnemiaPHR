####################Eltrombopag ANALYSIS PAGES###############################
NIHdates <- filter(All)

#Run Linear Regression

#LaggedPlatelets <- lag(NIHTrialDates$`Platelets (K/uL)`)

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

#Create Dual Axis Line Graph of WBCs and Prednisone
output$PlateletEltromOvertime <- renderPlotly({
  plot_ly(NIHTrialDates) %>%
    add_trace(
      x = ~ Date,
      y = NIHTrialDates$`Platelets (K/uL)`,
      type = 'scatter',
      mode = 'lines+markers',
      name = "WBC Count",
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
        range = c(0, 100)
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
library(markdown)
library(plotly)
library(dplyr)
library(shiny)
library(shinythemes)
library(knitr)
library(shinycssloaders)
library(data.table)

options(shiny.sanitize.errors = TRUE)

navbarPage("Health Summary",
           id = "HealthSummaryTabs",
           theme = shinytheme("cerulean"),
           tabPanel("About",
                    value = "home",
                    fluidPage(
                      tags$head(
                        tags$style(HTML("hr {border-top: 2px solid #000000;}"))
                      ),
                      fluidRow(
                        column(6,
                    h1("Unresolved Pancytopenia"),
                    h4("This site includes a chronology of events related to, or possibly related to, the patient’s ongoing pancytopenia, as well as blood-test results, bone marrow biopsy results, medication-use data, statistical analysis of medication effects on blood health, and disease diagnosis probabilities based on historical data."),
                    h3("Download Data"),
                    a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=CBC", "Click Here to Download CBC Test Results"),
                    br(),
                    a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=WBCDifferential", "Click Here to Download WBC Differentials"),
                    br(),
                    a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=Medications", "Click Here to Download Recent Medication History")
                        ),
                    column(6,
                    
                           h3("Background"),       
                                p("Patient, a 73-year-old white male, had normal or near-normal blood counts in all periodic tests through early December 2017, but has had pancytopenia since at least late January 2018. Patient experienced painful swelling in right foot starting mid-January. On January 18, primary care doctor diagnosed an infection and prescribed Cefadroxil antibiotic. Over next few days, foot became more swollen with a large deep-red/purple patch on top of foot. On January 22, doctor increased dosage of Cefadroxil, added a second antibiotic (Sulfamethoxazole-trimethoprim), and ordered blood tests. Because blood tests revealed very low platelet and white blood cell (WBC) counts, patient went to emergency room and was admitted to hospital."), 

                                  p("The initial diagnosis was that the low counts resulted from one or more medications patient had been taking. Cefadroxil and Methazolamide were identified as the primary suspects. Aplastic anemia was also suspected, but was ruled out by pathologist during hospital stay. Tests also ruled out cancer. Patient stayed in hospital for 30 days, had periodic transfusions of platelets and red blood cells (RBCs), but platelet, RBC, and WBC counts never increased to normal range. Hospital doctors continued to believe that one or more medications caused the problem. The patient was discharged from hospital on February 21 and had thrice-weekly blood draws and additional transfusions as required. However, there was no improvement."),
                                  
                                  p("Because blood counts continued to remain at unsafe levels without intervention (transfusions and increased doses of prednisone), doctors finally abandoned the hypothesis that a halt in the use of suspect medications would lead to a gradual return to normalcy. On April 2, they settled on a diagnosis of aplastic anemia--a rare autoimmune disorder in which the body's bone marrow fails to produce enough new RBCs, WBCs, and platelets."),
                           
                                  p("Based on recommendation of outside expert, the next step was to go to the National Institutes of Health (NIH) on May 1 to confirm the diagnosis, be evaluated for participation in a clinical trial, and begin treatment. The treatment began on May 3 and focused on the administration of three drugs: horse antithymocyte globulin (h-ATG), cyclosporine, and eltrombopag. The first two are meant to suppress the immune system; the third is intended to stimulate the bone marrow.")

                           )))
                    #h2("Average WBC test results for the past 7 days:"),
                    #h4(textOutput("WBCstatusText"),
                    #h2("Average RBC test results for the past 7 days:"),
                    #h4(textOutput("RBCstatusText")),
                    
                    ),
           tabPanel("Key Events",
                    value = "key_events",
                    withSpinner(DT::dataTableOutput("keyEventsTable"))
           ),
           navbarMenu("Test Results",
           tabPanel("CBC Blood Test Results",
                    value = "blood_test_results",
                    h2("CBC Blood Test Results"),
                    p("Graphs below show CBC and other blood test results since January 22, 2018 (the day before hospital admission at Kaiser San Rafael Medical Center). From that date until February 22, blood tests were performed daily. Between February 23 and April 30, tests were conducted thrice a week. Starting May 1, blood tests were conducted at NIH every day."),
                    withSpinner(plotlyOutput("pltResults", width = "100%", height = "400px")),
                    tags$hr(),
                    plotlyOutput("wbcResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("neutrophilsResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("rbcResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("hgbResults", width="100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("nrbcResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("mcvResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("rdwResults", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("hctResults", width = "100%", height = "400px")
           ),
           
           tabPanel("Other Blood Test Results",
                    value = "other_blood_test_results",
                    h2("Other Blood Test Results"),
                    withSpinner(plotlyOutput("CyclosporineBloodTest", width = "100%", height = "400px")),
                    tags$hr(),
                    plotlyOutput("AlanineBloodTest", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("AspartateBloodTest", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("BilirubinTotalBloodTest", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("BilirubinDirectBloodTest", width = "100%", height = "400px"),
                    tags$hr()

                    ),
           tabPanel("Reticulocytes Blood Test Results",
                    value = "reticulocytes_blood_test_results",
                    h2("Reticulocyte Blood Test Results"),
                    tags$hr(),
                    withSpinner(plotlyOutput("ReticulocytesAbsoluteBloodTest", width = "100%", height = "400px")),
                    tags$hr(),
                    plotlyOutput("ReticulocytesPercentBloodTest", width = "100%", height = "400px"),
                    tags$hr(),
                    plotlyOutput("ReticulocytesHemoglobinBloodTest", width = "100%", height = "400px"),
                    tags$hr()
                    
           ),
           

           tabPanel("Bone Marrow Biopsy Results",
                    h2("Bone Marrow Biopsy Results"),
                    a(href="https://drive.google.com/open?id=0B_gYIvY1ZovJa1RBbVpUMEE1S3dtWWpacWRsMEhjUXpmTjZJ", "Click Here to Download 2-8-2018 Bone Marrow Biopsy Results.", target = "_blank"),
                    br(),
                    a(href="https://drive.google.com/open?id=1gZYnWLhWnGV_ezGpRsKgusF6dJU2qSxJ", "Click Here to Download 3-15-2018 Bone Marrow Biopsy Results.", target = "_blank"),
                    br(),
                    a(href="https://drive.google.com/open?id=0B_gYIvY1ZovJMXNmQUNsVU9pV3lXXzdkcUswY0VMTWpBVTJN", "Click Here to Download 8-7-2018 Bone Marrow Biopsy Results.", target = "_blank"),
                    br(),
                    a(href="https://drive.google.com/open?id=0B_gYIvY1ZovJX0ZWYlRSMndGcVg4VlZvUFZzVmEzVlhWNmVn", "Click Here to Download 11-6-2018 Bone Marrow Biopsy Results.", target = "_blank")
           )),
            tabPanel("Medications",
                     h1("Medications"),
                     h4("The following graphs show all medications that the patient took orally since October 1, 2017. This page does not show eye drops that the patient used during this period."),
                     br(),
                     withSpinner(plotlyOutput("prednisoneIntake", width = "100%", height = "200px")),
                     plotlyOutput("Cyclosporine", width = "100%", height = "200px"),
                     plotlyOutput("Eltrombopag", width = "100%", height = "200px"),
                     plotlyOutput("Methylprednisolone", width = "100%", height = "200px"),
                     plotlyOutput("Diphenhydramine", width = "100%", height = "200px"),
                     plotlyOutput("Pantoprazole", width = "100%", height = "200px"),
                     plotlyOutput("Valacyclovir", width = "100%", height = "200px"),
                     plotlyOutput("Ceftazidime", width = "100%", height = "200px"),
                     plotlyOutput("Horse_ATG", width = "100%", height = "200px"),
                     plotlyOutput("multivitaminIntake", width = "100%", height = "200px"),
                     plotlyOutput("lisonoprilIntake", width = "100%", height = "200px"),
                     plotlyOutput("d3Intake", width = "100%", height = "200px"),
                     plotlyOutput("bisoprololIntake", width = "100%", height = "200px"),
                     plotlyOutput("alendronateIntake", width = "100%", height = "200px"),
                     plotlyOutput("triamtereneIntake", width = "100%", height = "200px"),
                     plotlyOutput("cefadroxilIntake", width = "100%", height = "200px"),
                     plotlyOutput("sulfamethoxazoleIntake", width = "100%", height = "200px"),
                     plotlyOutput("methazolamideIntake", width = "100%", height = "200px"),
                     plotlyOutput("aspirinIntake", width = "100%", height = "200px"),
                     plotlyOutput("acetazolamideIntake", width = "100%", height = "200px")
                     
            ),
           tabPanel(
             "Documentation",
             fluidPage(
               id="documentation",
               h2("Documentation"),
               h4("1. Recent medical summary within the last 1-2 months"),
               tags$ul(tags$li(a(href="https://drive.google.com/open?id=105x2mWrRagw5QWj4mkxD0QAf_-Zvj2L9", "Health Summary", target = "_blank"))),
               h4("2. Recent history and physical report within the last 1-2 months"),
               tags$ul(
                 tags$li(a(href="https://drive.google.com/open?id=14wXoXeyK8ZFf4JUWsS8icMsDG8tShm9e", "Emergency Room & Hospital Admission Report", target = "_blank")),
                 tags$li(a(href="https://drive.google.com/file/d/1VwKrLCxdAuAJ2UAZ2HT6-mbqYHCS64KI/view?usp=sharing", "Hospital Discharge Summary", target = "_blank")),
                 tags$li(a(href="https://drive.google.com/file/d/1p9sHRlsN16XZwy4-sAxFomcP36OpmukJ/view?usp=sharing", "Aplastic Anemia Diagnosis", target = "_blank")),
                p(
                  tags$b("Note:"),
                  "Recent history is also summarized on the ", actionLink("link_to_tabpanel_home", "Home Page"), "narrative and the list of", actionLink("link_to_tabpanel_keyevents", "Key Events"), "on this website")
                
               ),
               
               h4(
                 "3. Copies of the reports of the most recent complete blood counts with differential, reticulocyte count, liver and kidney function test results."
               ),
               tags$ul(
                 tags$li(a(href="https://drive.google.com/open?id=105x2mWrRagw5QWj4mkxD0QAf_-Zvj2L9", "Health Summary with CBCs", target = "_blank")),
                 tags$li(a(href="https://drive.google.com/file/d/1yozUV_qdl8jAhSSxDVeFcGtNHQ-skTUQ/view?usp=sharing", "Reticulocyte Count", target="_blank")),
                 tags$li(a(href="https://drive.google.com/open?id=1my6jAprLj0k5X5OG4OwVHrP_tt2XEiBa", "Liver and Kidney Function Test Results", target="_blank")),
                 p(
                   tags$b("Note:"),
                   "CBCs are also shown on the", actionLink("link_to_tabpanel_b", "Blood Test Results"), "graphs on this website. You can also", a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=CBC", "download the CBC data.", target = "_blank")
                 )
               ),
               h4(
                 "4. Copies of the reports of the complete blood counts for the last 1-2 months."
               ),
               tags$ul(
                 tags$li(a(href="https://drive.google.com/open?id=105x2mWrRagw5QWj4mkxD0QAf_-Zvj2L9", "Health Summary with CBCs", target = "_blank")),
                 p(
                   tags$b("Note:"),
                   "CBCs are also shown on the", actionLink("link_to_tabpanel_c", "Blood Test Results"), "graphs on this website. You can also", a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=CBC", "download the CBC data.", target = "_blank")
                 )
               ),
               h4(
                 "5. Copy of the report of the bone marrow aspirate and biopsy report with cytogenetics, flow and FISH."
               ),
               tags$ul(
                 tags$li(a(href="https://drive.google.com/file/d/1K0KL2SJ5tUzlRRyTui9RUyKDSVYt630m/view?usp=sharing", "Results of First Bone Marrow Biopsy—2-8-2018", target = "_blank")),
                 tags$li(a(href="https://drive.google.com/file/d/1gZYnWLhWnGV_ezGpRsKgusF6dJU2qSxJ/view?usp=sharing", "Initial Results of Second Bone Marrow Biopsy—3-15-2018", target = "_blank"))
               ),
               
               h4(
                 "6. Copy of the report of fanconi anemia test results if 40 yrs or younger."
               ),
                tags$ul(
                p("Not Applicable")),
               
               
               h4("7. Transfusion summary for red blood cells and platelets."),
               tags$ul(
                 p(
                   tags$b("Note:"),
                   "Transfusions are shown on the Platelet and RBC graphs on the", actionLink("link_to_tabpanel_d", "Blood Test Results"), "page of this website. You can also download the", a(href="https://docs.google.com/spreadsheets/d/1O_a3mpZsCKOEQz5MXOzzYU0X1rEbCg6_jhkLVWUOnc8/gviz/tq?tqx=out:csv&sheet=INfusions", "transfusion data.", target = "_blank")
                 )
               ),
               h4("8. Patients demographics"),
               tags$ul(
                 tags$li(a(href="https://drive.google.com/file/d/1RMaPV52M8N6fgj5CcVKTDE5LVNs3Shzm/view?usp=sharing", "Patient demographics from Hospitalization Medical Record", target="_blank"))
               ),
               h4("9. Patient and Physician contact information"),
               tags$ul(p(
                 tags$b("Note:"), "Available upon request"
               )),
               br()
               
             )
                    ),
           navbarMenu("Analysis",
                      tabPanel("Prednisone and WBC Count",
                               (fluidPage(fluidRow(
                                 column(
                                   12,
                                   h1("Analysis: Correlation of Prednisone Intake and WBC Count"),
                                   p(
                                     "White Blood Cell (WBC) counts trended upwards for the months of January, February, and March, diverging from Red Blood Cell and  Platelet counts. This upward trend resulted in an assessment that Aplastic Anemia was unlikely. However, bivariate regression analysis of WBCs and Prednisone intake shows a statistically significant correlation (P < .001) and an R Squared of .38. Accounting for 38% of the variance in WBC counts per day, the model predicts an increase of .032 (K/uL) WBCs for every additional 1mg of Prednisone. The linear model does not currently account for infusions."), 
                                   p("It is worth noting that the model changed dramatically after the start of H-ATG at NIH in May. From January - April, Prednisone accounted for greater than 60% of the variance. However, the increased number of medications while inpatient at NIH appear to have reduced the direct effectiveness of Prednisone on WBCs."),
                                    p("(Note: Current analysis based on data from 1/22/2018 - Present. Statistical analysis was run in base R. Computer code for the linear model is presented at the top of the linear model output.)"
                                   ),
                                   fluidRow(column(
                                     6,
                                     withSpinner(plotlyOutput("WBCandPrednisone", width =
                                                    "100%", height = "400px"))
                                   ),
                                   column(6,
                                          verbatimTextOutput("WBCprednisoneRegression")),
                                   fluidRow(column(
                                     12,
                                     plotlyOutput("PredWBCtime", width =
                                                    "100%", height = "400px")
                                   )))
                                   )
                               )))),
                      tabPanel("Eltrompobag and Platelet Count",
                               (fluidPage(fluidRow(
                                 column(
                                   12,
                                   h1("Analysis: Correlation of Eltrompobag Intake and Platelet Count"),
                                  
                                   fluidRow(column(
                                     6,
                                     withSpinner(plotlyOutput("PlateletsandEltrombopag", width =
                                                                "100%", height = "400px"))
                                   ),
                                   column(6,
                                          verbatimTextOutput("PlaleletsRegression")),
                                   fluidRow(column(
                                     12,
                                     plotlyOutput("PlateteletsEltromTime", width =
                                                    "100%", height = "400px")
                                   )))
                                 )
                               )))),
                      
                      tabPanel("Diagnosis Probabilities",
                              fluidPage(
                                withMathJax(includeMarkdown("diseaseProbabilities.md"))
                              )
                                
           ),
           tabPanel("WBC Analysis January 2019",
                    fluidPage(
                      tags$iframe(src = 'WBCanalysis_jan2019.html', # put testdoc.html to /www
                                  width = '100%', height = '800px', 
                                  frameborder = 0, scrolling = 'auto')
                    
          )
                    
           )
)
)
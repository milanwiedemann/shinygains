# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# library(shiny)
# # library(shinydashboard)
# library(tidyverse)
# library(patchwork)
# library(suddengains)
# # devtools::install_github("milanwiedemann/suddengains", ref = "plos-one-revisions")
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
# 
# # Define UI for application that draws a histogram
# ui <- tagList(tags$head(
#   HTML(
#     "<script>
#       (function(i,s,o,g,r,a,m){
#         i['GoogleAnalyticsObject']=r;i[r]=i[r]||
#         function(){
#           (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
#           a=s.createElement(o), m=s.getElementsByTagName(o)[0];
#           a.async=1;
#           a.src=g;m.parentNode.insertBefore(a,m)
#         })
#       (window, document, 'script',
#         '//www.google-analytics.com/analytics.js','ga');
# 
#         ga('create', 'UA-113145816-3', 'auto');
#         ga('send', 'pageview');
# 
#       </script>"
#   )
# ),
# navbarPage(
#   "shinygains",
#   tabPanel(
#     "Sudden Gains",
#     column(
#       width = 3,
#       helpText(),
#       # just a placeholder for a little bit top margin
#       h4("Select Criteria:"),
#       wellPanel(
#         selectInput(
#           "sgsl",
#           label = "Identify:",
#           choices = list("Sudden Gains" = "sg",
#                          "Sudden Losses" = "sl"),
#           selected = "sg"
#         ),
#         helpText(
#           "Note: The cut-off value for Crit 1 needs to be positive to identify sudden gains and negative to identify sudden losses."
#         )
#       ),
#       tabsetPanel(
#         tabPanel(
#           "Crit 1",
#           helpText(),
#           wellPanel(
#             checkboxInput("sg_crit1", label = "Apply Criterion 1", value = TRUE),
#             numericInput(
#               "sg_crit1_cutoff",
#               "Cut-off value:",
#               value = 7,
#               step = .1,
#             ),
#             helpText("Note: xxx.")
#           )
#         ),
#         tabPanel(
#           "Crit 2",
#           helpText(),
#           wellPanel(
#             checkboxInput("sg_crit2", label = "Apply Criterion 2", value = TRUE),
#             sliderInput(
#               "sg_crit2_pct",
#               "Percentage CHange Threshhold:",
#               min = 0,
#               max = 100,
#               value = 25,
#               step = 1
#             ),
#             helpText("Note: xxx.")
#           )
#         ),
#         tabPanel(
#           "Crit 3",
#           helpText(),
#           wellPanel(
#             checkboxInput("sg_crit3",
#                           label = "Apply criterion 3",
#                           value = TRUE),
#             checkboxInput("sg_crit3_adjust",
#                           label = "Adjust critical value",
#                           value = TRUE),
#             conditionalPanel(
#               "input['sg_crit3_adjust'] == FALSE",
#               selectInput(
#                 "sg_crit3_alpha",
#                 label = "Select alpha:",
#                 choices = list(
#                   "0.5" = 1,
#                   ".01" = 2,
#                   ".001" = 3
#                 ),
#                 selected = 1
#               )
#             ),
#             conditionalPanel(
#               "input['sg_crit3_adjust'] == TRUE",
#               numericInput(
#                 "sg_crit3_critical_value",
#                 "Critical value:",
#                 value = NA,
#                 step = .1
#               )
#             ),
#             helpText("Note: xxx.")
#           )
#         )
#       ),
#       h4("Data Characteristics:"),
#       helpText(),
#       wellPanel(
#         helpText(),
#         selectInput(
#           "data",
#           label = "Select Data Set:",
#           choices = list("Example Data Set 1" = "sgdata",
#                          "Example Data Set 2" = "sgdata_xx"),
#           selected = "sgdata"
#         ),
#         selectInput(
#           "sg_var_list",
#           "Select Repeated Measures:",
#           choices = c(
#             "bdi_s1",
#             "bdi_s2",
#             "bdi_s3",
#             "bdi_s4",
#             "bdi_s5",
#             "bdi_s6",
#             "bdi_s7",
#             "bdi_s8",
#             "bdi_s9",
#             "bdi_s10",
#             "bdi_s11",
#             "bdi_s12"
#           ),
#           selected = c(
#             "bdi_s1",
#             "bdi_s2",
#             "bdi_s3",
#             "bdi_s4",
#             "bdi_s5",
#             "bdi_s6",
#             "bdi_s7",
#             "bdi_s8",
#             "bdi_s9",
#             "bdi_s10",
#             "bdi_s11",
#             "bdi_s12"
#           ),
#           multiple = TRUE
#         ),
#         helpText(
#           "Select variables in the order that reflects the time points they were measured."
#         ),
#         sliderInput(
#           "na_pct",
#           "Missingness in % in Repeated Measures:",
#           min = 0,
#           max = 100,
#           value = 0,
#           step = 1
#         ),
#         helpText(
#           "Note: Some values are already missing in the original data set, so this is not super super accurate."
#         )
#       )
#     ),
#     column(
#       9,
#       h4("Results:"),
#       tabsetPanel(
#         tabPanel("Summary",
#                  helpText(),
#                  tabsetPanel(
#                    tabPanel(
#                      "Descriptives",
#                      helpText(),
#                      # fixedPage(descriptives_magnitude_summary descriptives_n_summary
#                      fixedRow(
#                        column(
#                          4,
#                          h5("Criterion 1"),
#                          verbatimTextOutput("descriptives_sg_crit1")
#                        ),
#                        column(
#                          4,
#                          h5("Criterion 2"),
#                          verbatimTextOutput("descriptives_sg_crit2")
#                        )
#                        ,
#                        column(
#                          4,
#                          h5("Criterion 3"),
#                          verbatimTextOutput("descriptives_sg_crit3")
#                        )
#                      ),
#                      # responsive = T),
#                      hr(),
#                      fixedRow(
#                        column(
#                          4,
#                          h5("Sudden Gains"),
#                          verbatimTextOutput("descriptives_n_summary")
#                        ),
#                        column(
#                          4,
#                          h5("Multiple Gains"),
#                          verbatimTextOutput("descriptives_sg_summary")
#                        )
#                        ,
#                        column(
#                          4,
#                          h5("Magnitude"),
#                          verbatimTextOutput("descriptives_magnitude_summary")
#                        )
#                      ),
#                      # responsive = T),
#                      hr(),
#                      fixedRow(
#                        column(
#                          12,
#                          helpText(),
#                          selectInput(
#                            "describe_bysg_or_byperson",
#                            label = "Specify data set for plots:",
#                            choices = list("bysg" = "bysg",
#                                           "byperson" = "byperson"),
#                            selected = "bysg"
#                          ),
#                          helpText(),
#                          plotOutput("plot_sg_desc"),
#                          helpText(
#                            "Note: To change the selected gain for the byperson data set, go to the 'Create byperson Data Set' panel at the top."
#                          )
#                        )
#                      ),
#                      # plotOutput("plot_sg_session_n", height = 850),
#                    ),
#                    tabPanel(
#                      "Longitudinal Plot",
#                      helpText(),
#                      selectInput(
#                        "select_ids_list",
#                        "Select IDs:",
#                        choices = c(
#                          "1",
#                          "2",
#                          "3",
#                          "4",
#                          "5",
#                          "6",
#                          "7",
#                          "8",
#                          "9",
#                          "10",
#                          "11",
#                          "12",
#                          "13",
#                          "14",
#                          "15",
#                          "16",
#                          "17",
#                          "18",
#                          "19",
#                          "20",
#                          "21",
#                          "22",
#                          "23",
#                          "24",
#                          "25",
#                          "26",
#                          "27",
#                          "28",
#                          "29",
#                          "30",
#                          "31",
#                          "32",
#                          "33",
#                          "34",
#                          "35",
#                          "36",
#                          "37",
#                          "38",
#                          "39",
#                          "40",
#                          "41",
#                          "42",
#                          "43"
#                        ),
#                        selected = c("2", "4", "5", "9"),
#                        multiple = TRUE
#                      ),
#                      hr(),
#                      plotOutput("plot_sg_longitudinal")
#                    ) #,
#                    #     tabPanel("Example Code Here",
#                    #
#                    #   fixedRow(column(6,
#                    #                   "Level 1a",
#                    #                   ),
#                    #            column(3,
#                    #                   "Level 1b")),
#                    #   fixedRow(column(6,
#                    #                   "Level 2a"),
#                    #            column(3,
#                    #                   "Level 2b")),
#                    #   hr("Reference: xxx.")
#                    # )
#                  )),
#         tabPanel(
#           "Selected Data Set",
#           helpText(),
#           DT::dataTableOutput("sgdata_table"),
#           helpText("Note: xxx.")
#         ),
#         tabPanel(
#           HTML("Create <I>bysg</I> Data Set"),
#           helpText(),
#           DT::dataTableOutput("bysg_table"),
#           helpText("Note: xxx.")
#         ),
#         tabPanel(
#           HTML("Create <I>byperson</I> Data Set"),
#           helpText(),
#           selectInput(
#             "multiple_sg_select",
#             label = "Specify which sudden gain/loss to select:",
#             choices = list(
#               "first" = "first",
#               "last" = "last",
#               # THIS IS A BUG IN THE R PACKAGE
#               # NEEDS TO GET UPDATED SPELLING MISTAKE!!!!
#               "smallest" = "smallest",
#               "largest" = "largest"
#             ),
#             selected = "first"
#           ),
#           
#           
#           
#           helpText(),
#           DT::dataTableOutput("byperson_table"),
#           helpText("Note: xxx.")
#         )
#       )
#     )
#   ),
#   tabPanel("Check Interval",),
#   tabPanel("References",)
# ))
# 
# # Define server logic required to draw a histogram
# server <- function(input, output, session) {
#   # Select variables -----
#   
#   # Create dataset with option to add missing values
#   sgdata_reactive <- reactive({
#     sgdata_temp <- suddengains::sgdata
#     
#     sgdata_weekly_temp <- sgdata_temp %>%
#       select("id", input$sg_var_list)
#     
#     sgdata_weekly_temp_na <- sgdata_weekly_temp %>%
#       tidyr::gather(vars, value,-id) %>%
#       dplyr::mutate(vars = base::factor(vars, levels =  input$sg_var_list)) %>%
#       dplyr::mutate(
#         random_num = runif(nrow(.)),
#         value = base::ifelse(
#           vars %in% input$sg_var_list &
#             random_num <= (input$na_pct / 100),
#           NA,
#           value
#         )
#       ) %>%
#       dplyr::select(-random_num) %>%
#       tidyr::spread(vars, value)
#     # sgdata_weekly_temp_na %>%
#     #   left_join(sgdata_fu_temp)
#     sgdata_weekly_temp_na
#   })
#   # Create table of raw data set
#   output$sgdata_table <-
#     DT::renderDataTable(DT::datatable(
#       sgdata_reactive(),
#       caption = paste0(
#         "Table: Example data set with n = ",
#         nrow(sgdata_reactive()),
#         " participants and ",
#         length(input$sg_var_list),
#         " weekly measures."
#       ),
#       options = list(
#         pageLength = 10,
#         scrollX = TRUE,
#         fixedColumns = TRUE,
#         searching = FALSE
#       ),
#       rownames = FALSE
#     ))
#   # sort out options for 3rd crit
#   # depending on whether adjusted is ticked, other input options change
#   observe({
#     if (input$sg_crit3_adjust == TRUE) {
#       updateNumericInput(session,
#                          "sg_crit3_critical_value",
#                          value = NA)
#       updateSelectInput(
#         session,
#         "sg_crit3_alpha",
#         choices = list(
#           "0.5" = "0.5",
#           ".01" = ".01",
#           ".001" = ".001"
#         ),
#         selected = "0.5"
#       )
#     } else if (input$sg_crit3_adjust == FALSE) {
#       updateNumericInput(session,
#                          "sg_crit3_critical_value",
#                          value = 2.777)
#       
#       updateSelectInput(session,
#                         "sg_crit3_alpha",
#                         choices = list("NA" = NA),
#                         selected = "NA")
#     }
#   })
#   
#   # Create bysg data
#   bysg_reactive <- reactive({
#     sg_crit2_input <- input$sg_crit2
#     
#     create_bysg(
#       data = sgdata_reactive(),
#       identify = input$sgsl,
#       sg_crit1_cutoff = input$sg_crit1_cutoff,
#       sg_crit2_pct = (input$sg_crit2_pct / 100),
#       sg_crit3 = input$sg_crit3,
#       # sg_crit3_alpha = input$sg_crit3_alpha,
#       sg_crit3_adjust = input$sg_crit3_adjust,
#       # sg_crit3_critical_value = input$sg_crit3_critical_value,
#       id_var_name = "id",
#       tx_start_var_name = input$sg_var_list[1],
#       tx_end_var_name = last(input$sg_var_list),
#       sg_var_list = input$sg_var_list,
#       sg_measure_name = "bdi"
#     )
#     
#   })
#   
#   
#   
#   # Create bysg data
#   byperson_reactive <- reactive({
#     create_byperson(
#       data = sgdata_reactive(),
#       identify = input$sgsl,
#       multiple_sg_select = input$multiple_sg_select,
#       sg_crit1_cutoff = input$sg_crit1_cutoff,
#       sg_crit2_pct = (input$sg_crit2_pct / 100),
#       sg_crit3 = input$sg_crit3,
#       # sg_crit3_alpha = input$sg_crit3_alpha,
#       sg_crit3_adjust = input$sg_crit3_adjust,
#       # sg_crit3_critical_value = input$sg_crit3_critical_value,
#       id_var_name = "id",
#       tx_start_var_name = input$sg_var_list[1],
#       tx_end_var_name = last(input$sg_var_list),
#       sg_var_list = input$sg_var_list,
#       sg_measure_name = "bdi"
#     )
#   })
#   #     # Create bysg data table
#   output$bysg_table <-
#     DT::renderDataTable(DT::datatable(
#       bysg_reactive(),
#       caption = paste0(
#         "Table: Data set with one row for each sudden gain (n = ",
#         nrow(bysg_reactive()),
#         ") experienced by a total number of n = ",
#         length(unique(bysg_reactive()$id)),
#         " participants."
#       ),
#       options = list(
#         pageLength = 10,
#         scrollX = TRUE,
#         fixedColumns = TRUE,
#         searching = FALSE
#       ),
#       rownames = FALSE
#     ))
#   
#   # Create bysg data table
#   output$byperson_table <-
#     DT::renderDataTable(
#       DT::datatable(
#         byperson_reactive(),
#         caption = paste0(
#           "Table: Data set with one selected sudden gain (",
#           input$multiple_sg_select,
#           ") for each participant who experienced a sudden gain (n = ",
#           summarise(byperson_reactive(), sum(sg_crit123 == 1, na.rm = TRUE))[[1]],
#           ") and all participants without sudden gains (n = ",
#           summarise(byperson_reactive(), sum(sg_crit123 == 0, na.rm = TRUE))[[1]],
#           ")."
#         ),
#         options = list(
#           pageLength = 10,
#           scrollX = TRUE,
#           fixedColumns = TRUE,
#           searching = FALSE
#         ),
#         rownames = FALSE
#       )
#     )
#   
#   #  plot ----
#   output$plot_sg_desc <- renderPlot({
#     y_limit <- bysg_reactive() %>%
#       select(sg_session_n) %>%
#       group_by(sg_session_n) %>%
#       count() %>%
#       drop_na() %>%
#       max(.$n)
#     
#     if (input$describe_bysg_or_byperson == "bysg") {
#       plot_bysg_sg_session_n <- bysg_reactive() %>%
#         select(sg_session_n) %>%
#         ggplot(aes(sg_session_n)) +
#         geom_bar() +
#         labs(x = "Pregain Time Point", y = "Count", fill = "") +
#         scale_x_continuous(breaks = seq(2, max(
#           bysg_reactive()$sg_session_n, na.rm = T
#         ), by = 1)) +
#         scale_y_continuous(breaks = seq(0, 35, by = 2)) +
#         coord_cartesian(ylim = c(0, y_limit)) +
#         ggtitle('Fig. A.1: bysg (all gains)') +
#         theme_gray()
#       
#       plot_bysg_average_sg <- plot_sg(
#         data = bysg_reactive(),
#         id_var_name = "id",
#         tx_start_var_name = input$sg_var_list[1],
#         tx_end_var_name = last(input$sg_var_list),
#         sg_pre_post_var_list = c(
#           "sg_bdi_2n",
#           "sg_bdi_1n",
#           "sg_bdi_n",
#           "sg_bdi_n1",
#           "sg_bdi_n2",
#           "sg_bdi_n3"
#         ),
#         ylab = "BDI",
#         xlab = "Session"
#       ) +
#         ggtitle('Fig. B.1: bysg (all gains)') +
#         theme_gray()
#       
#       plot_bysg_sg_session_n + plot_bysg_average_sg &
#         theme(text = element_text(size = 18))
#       
#     } else if (input$describe_bysg_or_byperson == "byperson") {
#       plot_byperson_sg_session_n <- byperson_reactive() %>%
#         select(sg_session_n) %>%
#         ggplot(aes(sg_session_n)) +
#         geom_bar() +
#         labs(x = "Pregain Time Point", y = "Count", fill = "") +
#         scale_x_continuous(breaks = seq(2, max(
#           byperson_reactive()$sg_session_n, na.rm = T
#         ), by = 1)) +
#         scale_y_continuous(breaks = seq(0, 35, by = 2)) +
#         coord_cartesian(ylim = c(0, y_limit)) +
#         ggtitle(paste0('Fig. A.2: byperson (', input$multiple_sg_select  , " gain)")) +
#         theme_gray()
#       
#       plot_byperson_average_sg <-
#         plot_sg(
#           data = byperson_reactive(),
#           id_var_name = "id",
#           tx_start_var_name = input$sg_var_list[1],
#           tx_end_var_name = last(input$sg_var_list),
#           sg_pre_post_var_list = c(
#             "sg_bdi_2n",
#             "sg_bdi_1n",
#             "sg_bdi_n",
#             "sg_bdi_n1",
#             "sg_bdi_n2",
#             "sg_bdi_n3"
#           ),
#           ylab = "BDI",
#           xlab = "Session"
#         ) +
#         ggtitle(paste0('Fig. B.2: byperson (', input$multiple_sg_select  , " gain)")) +
#         theme_gray()
#       
#       plot_byperson_sg_session_n + plot_byperson_average_sg  &
#         theme(text = element_text(size = 18))
#       
#     }
#     
#     
#   })
#   
#   
#   
#   output$plot_sg_longitudinal <-   renderPlot({
#     plot_sg_trajectories(
#       data = suddengains::sgdata,
#       id_var = "id",
#       select_id_list = input$select_ids_list,
#       var_list = input$sg_var_list,
#       show_id = TRUE,
#       id_label_size = 5.5,
#       label.padding = .2,
#       show_legend = TRUE,
#       colour = "viridis",
#       viridis_option = "D",
#       viridis_begin = 0,
#       viridis_end = 1,
#       connect_missing = TRUE,
#       scale_x_num = TRUE,
#       scale_x_num_start = 1,
#       apaish = FALSE,
#       xlab = "Session",
#       ylab = "BDI"
#     ) +
#       ggtitle(paste0(
#         "Fig. C: Trajectories of BDI scores for a selection of participants"
#       )) +
#       theme(text = element_text(size = 18))
#     
#   })
#   
#   
#   # descriptives
#   output$descriptives_sg_crit123 <- renderText({
#     paste0(
#       "Crit 1: ",
#       input$sg_crit1,
#       "\n",
#       "Crit 1, Cut-off: ",
#       input$sg_crit1_cutoff,
#       "\n\n",
#       "Crit 2: ",
#       input$sg_crit2,
#       "\n",
#       "Crit 2, Pct: ",
#       input$sg_crit2_pct,
#       "\n\n",
#       "Crit 3: ",
#       input$sg_crit3,
#       "\n",
#       "Crit 3, Adjusted: ",
#       input$sg_crit3_adjust,
#       "\n",
#       "Crit 3, Alpha: ",
#       input$sg_crit3_alpha,
#       "\n",
#       "Crit 3, Critical Value: ",
#       input$sg_crit3_critical_value,
#       "\n"
#     )
#   })
#   
#   output$descriptives_sg_crit1 <- renderText({
#     paste0("Crit 1: ",
#            input$sg_crit1,
#            "\n",
#            "Crit 1, Cut-off: ",
#            input$sg_crit1_cutoff,
#            " \n \n ")
#   })
#   
#   output$descriptives_sg_crit2 <- renderText({
#     paste0("Crit 2: ",
#            input$sg_crit2,
#            "\n",
#            "Crit 2, Pct: ",
#            input$sg_crit2_pct,
#            " \n \n ")
#   })
#   
#   output$descriptives_sg_crit3 <- renderText({
#     paste0(
#       "Crit 3: ",
#       input$sg_crit3,
#       "\n",
#       "Crit 3, Adjusted: ",
#       input$sg_crit3_adjust,
#       "\n",
#       "Crit 3, Alpha: ",
#       input$sg_crit3_alpha,
#       "\n",
#       "Crit 3, Critical Value: ",
#       input$sg_crit3_critical_value
#     )
#   })
#   
#   output$descriptives_n_summary <- renderText({
#     descriptives_bysg <-
#       describe_sg(bysg_reactive(), sg_data_structure = "bysg")
#     descriptives_byperson <-
#       describe_sg(byperson_reactive(), sg_data_structure = "byperson")
#     
#     paste0(
#       "total_n: ",
#       descriptives_byperson$total_n,
#       "\n",
#       "sg_total_n: ",
#       descriptives_byperson$sg_total_n,
#       "\n",
#       "sg_n: ",
#       descriptives_byperson$sg_n,
#       "\n",
#       "sg_pct: ",
#       descriptives_byperson$sg_pct
#     )
#   })
#   
#   output$descriptives_sg_summary <- renderText({
#     descriptives_bysg <-
#       describe_sg(bysg_reactive(), sg_data_structure = "bysg")
#     descriptives_byperson <-
#       describe_sg(byperson_reactive(), sg_data_structure = "byperson")
#     
#     paste0(
#       "sg_multiple_n: ",
#       descriptives_byperson$sg_multiple_n,
#       "\n",
#       "sg_multiple_pct: ",
#       descriptives_byperson$sg_multiple_pct
#     )
#   })
#   
#   output$descriptives_magnitude_summary <- renderText({
#     descriptives_bysg <-
#       describe_sg(bysg_reactive(), sg_data_structure = "bysg")
#     descriptives_byperson <-
#       describe_sg(byperson_reactive(), sg_data_structure = "byperson")
#     
#     paste0(
#       "sg_magnitude_m: ",
#       descriptives_byperson$sg_magnitude_m,
#       "\n",
#       "sg_magnitude_sd: ",
#       descriptives_byperson$sg_magnitude_sd
#     )
#   })
#   
#   
#   
# }
# # Run the application
# shinyApp(ui = ui, server = server)

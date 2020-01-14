#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(suddengains)
library(ggplot2)
library(dplyr)
options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

# Define UI for application that draws a histogram
ui <- navbarPage(
  "shinygains",
  tabPanel(
    "Sudden Gains",
    column(
      width = 3,
      helpText(),
      # just a placeholder for a little bit top margin
      h4("Select Criteria:"),
      tabsetPanel(
        tabPanel(
          "Crit 1",
          helpText(),
          wellPanel(
            checkboxInput("sg_crit1", label = "Apply Criterion 1", value = TRUE),
            numericInput(
              "sg_crit1_cutoff",
              "Cut-off value:",
              value = 7,
              step = .1,
            ),
            helpText("Note: xxx.")
          )
        ),
        tabPanel(
          "Crit 2",
          helpText(),
          wellPanel(
            checkboxInput("sg_crit2", label = "Apply Criterion 2", value = TRUE),
            sliderInput(
              "sg_crit2_pct",
              "Percentage:",
              min = 0,
              max = 100,
              value = 25,
              step = 1
            ),
            helpText("Note: xxx.")
          )
        ),
        tabPanel(
          "Crit 3",
          helpText(),
          wellPanel(
            checkboxInput("sg_crit3",
                          label = "Apply criterion 3",
                          value = TRUE),
            checkboxInput("sg_crit3_adjust",
                          label = "Adjust critical value",
                          value = TRUE),
            selectInput(
              "sg_crit3_alpha",
              label = "Select alpha:",
              choices = list(
                "0.5" = 1,
                ".01" = 2,
                ".001" = 3
              ),
              selected = 1
            ),
            numericInput(
              "sg_crit3_critical_value",
              "Critical value:",
              value = NA,
              step = .1
            ),
            helpText("Note: xxx.")
          )
        )
      ),
      h4("Data Characteristics:"),
      helpText(),
      wellPanel(
        helpText(),
        selectInput(
          "data",
          label = "Select Data Set:",
          choices = list("Example Data Set 1" = "sgdata",
                         "Example Data Set 2" = "sgdata_xx"),
          selected = "sgdata"
        ),
        selectInput(
          "sg_var_list",
          "Select Repeated Measures:",
          choices = c(
            "bdi_s1",
            "bdi_s2",
            "bdi_s3",
            "bdi_s4",
            "bdi_s5",
            "bdi_s6",
            "bdi_s7",
            "bdi_s8",
            "bdi_s9",
            "bdi_s10",
            "bdi_s11",
            "bdi_s12"
          ),
          selected = c(
            "bdi_s1",
            "bdi_s2",
            "bdi_s3",
            "bdi_s4",
            "bdi_s5",
            "bdi_s6",
            "bdi_s7",
            "bdi_s8",
            "bdi_s9",
            "bdi_s10",
            "bdi_s11",
            "bdi_s12"
          ),
          multiple = TRUE
        ),
        helpText(
          "Select variables in the order that reflects the time points they were measured."
        ),
        sliderInput(
          "na_pct",
          "Missingness in % in Repeated Measures:",
          min = 0,
          max = 100,
          value = 0,
          step = 1
        ),
        helpText(
          "Note: Some values are already missing in the original data set, so this is not super super accurate."
        )
      )
    ),
    column(
      9,
      h4("Results:"),
      tabsetPanel(
        tabPanel("Summary",
                 hr("Reference: xxx.")),
        tabPanel(
          "Selected Data Set",
          helpText(),
          DT::dataTableOutput("sgdata_table"),
          helpText("Note: xxx.")
        ),
        tabPanel(
          "'bysg' Data Set",
          helpText(),
          DT::dataTableOutput("bysg_table"),
          helpText("Note: xxx.")
        ),
        tabPanel(
          "'byperson' Data Set",
          helpText(),
          selectInput(
            "multiple_sg_select",
            label = "Specify which sudden gain/loss to select:",
            choices = list(
              "first" = "first",
              "last" = "last",
              # THIS IS A BUG IN THE R PACKAGE
              # NEEDS TO GET UPDATED SPELLING MISTAKE!!!!
              # "smallest" = "smallest",
              "largest" = "largest"
            ),
            selected = "first"
          ),
          helpText(),
          DT::dataTableOutput("byperson_table"),
          helpText("Note: xxx.")
        )
      )
    )
  ),
  tabPanel("Check Interval",),
  tabPanel("References",)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Select variables -----
  
  # Create dataset with option to add missing values
  sgdata_reactive <- reactive({
    
    sgdata_temp <- suddengains::sgdata
    
    sgdata_weekly_temp <- sgdata_temp %>%
      select("id", input$sg_var_list)

    sgdata_weekly_temp_na <- sgdata_weekly_temp %>%
      tidyr::gather(vars, value,-id) %>%
      dplyr::mutate(vars = base::factor(vars, levels =  input$sg_var_list)) %>%
      dplyr::mutate(
        random_num = runif(nrow(.)),
        value = base::ifelse(
          vars %in% input$sg_var_list &
            random_num <= (input$na_pct / 100),
          NA,
          value
        )
      ) %>%
      dplyr::select(-random_num) %>%
      tidyr::spread(vars, value)
    # sgdata_weekly_temp_na %>%
    #   left_join(sgdata_fu_temp)
    sgdata_weekly_temp_na
  })
  # Create table of raw data set
  output$sgdata_table <-
    DT::renderDataTable(DT::datatable(
      sgdata_reactive(),
      caption = paste0(
        "Table: Example data set with n = ",
        nrow(sgdata_reactive()),
        " participants and ",
        length(input$sg_var_list),
        " weekly measures."
      ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        fixedColumns = TRUE,
        searching = FALSE
      ),
      rownames = FALSE
    ))
  # sort out options for 3rd crit
  # depending on whether adjusted is ticked, other input options change
  observe({
    if (input$sg_crit3_adjust == TRUE) {
      updateNumericInput(session,
                         "sg_crit3_critical_value",
                         value = NA)
      updateSelectInput(
        session,
        "sg_crit3_alpha",
        choices = list(
          "0.5" = 1,
          ".01" = 2,
          ".001" = 3
        ),
        selected = 1
      )
    } else if (input$sg_crit3_adjust == FALSE) {
      updateNumericInput(session,
                         "sg_crit3_critical_value",
                         value = 2.777)
      
      updateSelectInput(session,
                        "sg_crit3_alpha",
                        choices = list("NA" = 1),
                        selected = 1)
    }
    # Create bysg data
    bysg_reactive <- reactive({
      create_bysg(
        data = sgdata_reactive(),
        sg_crit1_cutoff = input$sg_crit1_cutoff,
        sg_crit2_pct = (input$sg_crit2_pct / 100),
        sg_crit3 = input$sg_crit3,
        # sg_crit3_alpha = input$sg_crit3_alpha,
        # sg_crit3_adjust = input$sg_crit3_adjust,
        # sg_crit3_critical_value = input$sg_crit3_critical_value,
        id_var_name = "id",
        tx_start_var_name = input$sg_var_list[1],
        tx_end_var_name = last(input$sg_var_list),
        sg_var_list = input$sg_var_list,
        sg_measure_name = "bdi"
      )
    })
    # Create bysg data
    byperson_reactive <- reactive({
      create_byperson(
        data = sgdata_reactive(),
        multiple_sg_select = input$multiple_sg_select,
        sg_crit1_cutoff = input$sg_crit1_cutoff,
        sg_crit2_pct = (input$sg_crit2_pct / 100),
        sg_crit3 = input$sg_crit3,
        # sg_crit3_alpha = input$sg_crit3_alpha,
        # sg_crit3_adjust = input$sg_crit3_adjust,
        # sg_crit3_critical_value = input$sg_crit3_critical_value,
        id_var_name = "id",
        tx_start_var_name = input$sg_var_list[1],
        tx_end_var_name = last(input$sg_var_list),
        sg_var_list = input$sg_var_list,
        sg_measure_name = "bdi"
      )
    })
    #     # Create bysg data table
    output$bysg_table <-
      DT::renderDataTable(DT::datatable(
        bysg_reactive(),
        caption = paste0(
          "Table: Data set with one row for each sudden gain (n = ",
          nrow(bysg_reactive()),
          ") experienced by a total number of n = ",
          length(unique(bysg_reactive()$id)),
          " participants."
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          fixedColumns = TRUE,
          searching = FALSE
        ),
        rownames = FALSE
      ))
    # Create bysg data table
    output$byperson_table <-
      DT::renderDataTable(
        DT::datatable(
          byperson_reactive(),
          caption = paste0(
            "Table: Data set with one selected sudden gain (",
            input$multiple_sg_select,
            ") for each participant who experienced a sudden gain (n = ",
            summarise(byperson_reactive(), sum(sg_crit123 == 1, na.rm = TRUE))[[1]],
            ") and all participants without sudden gains (n = ",
            summarise(byperson_reactive(), sum(sg_crit123 == 0, na.rm = TRUE))[[1]],
            ")."
          ),
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            fixedColumns = TRUE,
            searching = FALSE
          ),
          rownames = FALSE
        )
      )
  })
}
# Run the application
shinyApp(ui = ui, server = server)

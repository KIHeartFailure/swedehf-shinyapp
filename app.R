library(shiny)
library(shinythemes)
library(bslib)
library(shinyWidgets)
library(dplyr)

source("setup.R")

# Define UI for random distribution app ----
ui <- fluidPage(
  # theme = bs_theme(preset = "bootstrap"),
  theme = shinytheme("spacelab"),
  # App title ----
  titlePanel("Eligibility and event rates - SwedeHF"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4(strong("Select patients:")),
      selectInput(
        inputId = "ef",
        label = "Ejection Fraction (%)",
        choices = appvar_values$shf_ef,
        selected = appvar_values$shf_ef,
        multiple = TRUE
      ),
      br(),
      h4(strong("Eligibility criteria:")),
      selectInput(
        inputId = "prevhfh6mo",
        label = "HF hospitalization < 6 months",
        choices = appvar_values$shf_sos_prevhfh6mo,
        selected = appvar_values$shf_sos_prevhfh6mo,
        multiple = TRUE
      ),
      selectInput(
        inputId = "nyha",
        label = "NYHA class",
        choices = appvar_values$shf_nyha,
        selected = appvar_values$shf_nyha,
        multiple = TRUE
      ),
      selectInput(
        inputId = "af",
        label = "Atrial Fibrillation",
        choices = appvar_values$shf_sos_com_af,
        selected = appvar_values$shf_sos_com_af,
        multiple = TRUE
      ),
      selectInput(
        inputId = "gfrckdepi",
        label = "eGFR (mL/min/1.73 m²)",
        choices = appvar_values$shf_gfrckdepi,
        selected = appvar_values$shf_gfrckdepi,
        multiple = TRUE
      ),
      selectInput(
        inputId = "ntprobnp",
        label = "NT-proBNP (pg/mL)",
        choices = appvar_values$shf_ntprobnp,
        selected = appvar_values$shf_ntprobnp,
        multiple = TRUE
      ),
      selectInput(
        inputId = "bpsys",
        label = "Systolic blood pressure (mmHg)",
        choices = appvar_values$shf_bpsys,
        selected = appvar_values$shf_bpsys,
        multiple = TRUE
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Eligibility and rates",
          page_fillable(
            # br(),
            h4(uiOutput("error")),
            br(),
            layout_column_wrap(
              # width = "250px",
              fill = FALSE,
              value_box(
                title = NULL,
                uiOutput("eligibilty"),
                showcase = bsicons::bs_icon("people-fill"),
                theme = "bg-success"
              )
            ),
            # card(
            DT::dataTableOutput(outputId = "base")
            # )
            ,
            br(),
            # card(
            DT::dataTableOutput(outputId = "event")
            # )
          )
        ),
        tabPanel(
          "Information",
          value = "Information",
          icon = icon("info"),
          br(),
          p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 with NYHA class II, III or IV, non-missing Ejection fraction and discharged alive from hospital (N = 46,636)."),
          p("The outcomes were derived from the National Patient register (hospitalizations) and The Cause of Death Register (deaths). HF was defined as ICD-10 I110, I130, I132, I255, I420, I423, I425-9, I43, I50, J81, K761, R570 and CV as I, J81, K761, R570, G45."),
          p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 with NYHA class II, III or IV and discharged alive from hospital."),
          p("Missing values are imputed with a multivariate imputation algorithm based on random forests (Mayer M (2024). _missRanger: Fast Imputation of Missing Values_. doi:10.32614/CRAN.package.missRanger")
        )
      )
    )
  )
)

server <- function(input, output) {
  select_func2 <- reactive({
    select_func(
      ef = input$ef,
      prevhfh6mo = input$prevhfh6mo,
      nyha = input$nyha,
      af = input$af,
      gfrckdepi = input$gfrckdepi,
      ntprobnp = input$ntprobnp,
      bpsys = input$bpsys
    )
  })

  output$error <- renderText({
    if (select_func2() == 0) {
      checkempty <- c(is.null(input$ef), is.null(input$prevhfh6mo), is.null(input$nyha), is.null(input$af), is.null(input$gfrckdepi), is.null(input$ntprobnp), is.null(input$bpsys))
      labs <- c("Ejection Fraction", "HF hospitalization", "NYHA class", "Atrial Fibrillation", "eGFR", "NT-proBNP", "Systolic blood pressure")
      paste0("<br /><font color=\"#FF0000\"><b>Please select at least one value for ", paste0(labs[checkempty], collapse = " and "), "</b></font>")
    } else if (select_func2() == -1) {
      checkad <- c(
        "Ejection Fraction"[any(diff(sort(as.numeric(input$ef))) > 1)],
        "HF hospitalization"[any(diff(sort(as.numeric(input$prevhfh6mo))) > 1)],
        "NYHA class"[any(diff(sort(as.numeric(input$nyha))) > 1)],
        "Atrial Fibrillation"[any(diff(sort(as.numeric(input$af))) > 1)],
        "eGFR"[any(diff(sort(as.numeric(input$gfrckdepi))) > 1)],
        "NT-proBNP"[any(diff(sort(as.numeric(input$ntprobnp))) > 1)],
        "Systolic blood pressure"[any(diff(sort(as.numeric(input$bpsys))) > 1)]
      )
      paste0("<br /><font color=\"#FF0000\"><b>All selected categories need to be numerically ajoining for ", paste0(checkad, collapse = " and "), "</b></font>")
    } else if (!select_func2() %in% c(0, -1)) {
      NULL
    }
  })

  output$eligibilty <- renderText({
    if (select_func2() %in% c(-1, 0)) {
      return(NULL)
    } else {
      paste0(el[[select_func2()]], "<br> of patients with EF ", paste0(names(appvar_values$shf_ef)[appvar_values$shf_ef %in% input$ef], collapse = ", "), "% are eligible")
    }
  })

  output$base <- DT::renderDataTable(
    if (select_func2() %in% c(-1, 0)) {
      return(NULL)
    } else {
      DT::datatable(
        base[[select_func2()]],
        options = list(
          paging = FALSE, searching = F, info = F,
          dom = "Bfrtip",
          buttons =
            list(list(
              extend = "collection",
              buttons = list(
                list(extend = "csv", filename = "characteristics"),
                list(extend = "excel", filename = "characteristics"),
                list(extend = "pdf", filename = "characteristics")
              ),
              text = "Download"
            ))
        ),
        rownames = F,
        colnames = c("", "Not eligible", "Eligible"),
        escape = F
      )
    }
  )

  output$event <- DT::renderDataTable(
    if (select_func2() %in% c(-1, 0)) {
      return(NULL)
    } else {
      DT::datatable(
        rate[[select_func2()]],
        extensions = "Buttons",
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = "Bfrtip",
          buttons =
            list(list(
              extend = "collection",
              buttons = list(
                list(extend = "csv", filename = "event rates"),
                list(extend = "excel", filename = "event rates"),
                list(extend = "pdf", filename = "event rates")
              ),
              text = "Download"
            ))
        ),
        rownames = F,
        colnames = c("", "Time frame", "Not eligible, events/100 person-years (95% CI)", "Eligible, events/100 person-years (95% CI)"),
        escape = F
      )
    }
  )
}


# Create Shiny app ----
shinyApp(ui, server)

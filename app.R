source("setup.R")

ui <- page_sidebar(
  title = h4("Eligibility and event rates - SwedeHF"),
  theme = mytheme,
  sidebar = sidebar(
    width = "30%",
    fillable = FALSE,
    card(
      card_header(h5("Ejection fraction (%)"), class = "bg-primary"),
      selectInput(
        inputId = "ef",
        label = NULL,
        choices = appvar_values$shf_ef,
        selected = appvar_values$shf_ef,
        multiple = TRUE
      )
    ),
    card(
      card_header(h5("Eligibility criteria"), class = "bg-primary"),
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
        inputId = "gfrckdepi",
        label = "eGFR (mL/min/1.73 m²)",
        choices = appvar_values$shf_gfrckdepi,
        selected = appvar_values$shf_gfrckdepi,
        multiple = TRUE
      ),
      checkboxInput("ntprobnp_af", "Separate NT-proBNP by atrial fibrillation"),
      conditionalPanel(
        condition = "input.ntprobnp_af == true",
        p(HTML("NT-proBNP (pg/mL) <b>with</b> atrial fibrillation > 900<br>NT-proBNP (pg/mL) <b>without</b> atrial fibrillation > 300")),
      ),
      conditionalPanel(
        condition = "input.ntprobnp_af == false",
        selectInput(
          inputId = "ntprobnp",
          label = "NT-proBNP (pg/mL)",
          choices = appvar_values$shf_ntprobnp,
          selected = appvar_values$shf_ntprobnp,
          multiple = TRUE
        )
      ),
      selectInput(
        inputId = "bpsys",
        label = "Systolic blood pressure (mmHg)",
        choices = appvar_values$shf_bpsys,
        selected = appvar_values$shf_bpsys,
        multiple = TRUE
      )
    ), card(
      card_header(h5("Event type"), class = "bg-primary"),
      selectInput(
        inputId = "out",
        label = NULL,
        choices = outvars$name,
        selected = outvars$name,
        multiple = TRUE
      )
    )
  ),
  navset_card_underline(
    nav_panel(
      title = "Table",
      icon = icon("table"),
      #             page_fillable(
      # br(),
      uiOutput("error"),
      # width = "250px",
      # fill = FALSE,
      value_box(
        title = NULL,
        uiOutput("eligibilty"),
        showcase = bsicons::bs_icon("people-fill"),
        theme = "bg-primary"
      ),
      card(
        DT::dataTableOutput(outputId = "base"),
        padding = 0
      ),
      card(
        DT::dataTableOutput(outputId = "event")
      )
      # )
    ),
    nav_panel(
      title = "Graph",
      icon = icon("chart-simple"),
      page_fillable(
        uiOutput("error2"),
        plotOutput("plot")
      )
    ),
    nav_panel("Information",
      value = "Information",
      icon = icon("info"),
      p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 with recorded Ejection fraction and discharged alive from hospital (N = 46,636)."),
      p("The outcomes were derived from the National Patient register (hospitalizations) and The Cause of Death Register (deaths). HF was defined as ICD-10 I110, I130, I132, I255, I420, I423, I425-9, I43, I50, J81, K761, R570 and CV as I, J81, K761, R570, G45."),
      p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 and discharged alive from hospital."),
      p("Missing values are imputed with a multivariate imputation algorithm based on random forests (Mayer M (2024). _missRanger: Fast Imputation of Missing Values_. doi:10.32614/CRAN.package.missRanger")
    )
  )
)

server <- function(input, output) {
  show_modal_spinner()
  Sys.sleep(3)
  remove_modal_spinner()
  select_func2 <- reactive({
    select_func(
      ef = input$ef,
      prevhfh6mo = input$prevhfh6mo,
      nyha = input$nyha,
      gfrckdepi = input$gfrckdepi,
      ntprobnp = input$ntprobnp,
      ntprobnp_af = input$ntprobnp_af,
      bpsys = input$bpsys
    )
  })

  output$error <- renderText({
    if (select_func2() == 0) {
      checkempty <- c(is.null(input$ef), is.null(input$prevhfh6mo), is.null(input$nyha), is.null(input$gfrckdepi), is.null(input$ntprobnp), is.null(input$bpsys))
      labs <- c("Ejection Fraction", "HF hospitalization", "NYHA class", "eGFR", "NT-proBNP", "Systolic blood pressure")
      paste0("<font color=\"#FF0000\"><b>Select at least one value for ", paste0(labs[checkempty], collapse = " and "), "</b></font>")
    } else if (select_func2() == -1) {
      checkad <- c(
        "Ejection Fraction"[any(diff(sort(as.numeric(input$ef))) > 1)],
        "NYHA class"[any(diff(sort(as.numeric(input$nyha))) > 1)],
        "NT-proBNP"[any(diff(sort(as.numeric(input$ntprobnp))) > 1)],
        "Systolic blood pressure"[any(diff(sort(as.numeric(input$bpsys))) > 1)]
      )
      paste0("<font color=\"#FF0000\"><b>All selected categories must be numerically ajoining for ", paste0(checkad, collapse = " and "), "</b></font>")
    } else if (!select_func2() %in% c(0, -1)) {
      NULL
    }
  })

  output$error2 <- renderText({
    if (select_func2() == 0 | is.null(input$out)) {
      checkempty <- c(is.null(input$ef), is.null(input$prevhfh6mo), is.null(input$nyha), is.null(input$gfrckdepi), is.null(input$ntprobnp), is.null(input$bpsys), is.null(input$out))
      labs <- c("Ejection Fraction", "HF hospitalization", "NYHA class", "eGFR", "NT-proBNP", "Systolic blood pressure", "Event type")
      paste0("<font color=\"#FF0000\"><b>Select at least one value for ", paste0(labs[checkempty], collapse = " and "), "</b></font>")
    } else if (select_func2() == -1) {
      checkad <- c(
        "Ejection Fraction"[any(diff(sort(as.numeric(input$ef))) > 1)],
        "NYHA class"[any(diff(sort(as.numeric(input$nyha))) > 1)],
        "NT-proBNP"[any(diff(sort(as.numeric(input$ntprobnp))) > 1)],
        "Systolic blood pressure"[any(diff(sort(as.numeric(input$bpsys))) > 1)]
      )
      paste0("<font color=\"#FF0000\"><b>All selected categories must be numerically ajoining for ", paste0(checkad, collapse = " and "), "</b></font>")
    } else if (!select_func2() %in% c(0, -1)) {
      NULL
    }
  })

  output$eligibilty <- renderText({
    if (select_func2() %in% c(-1, 0)) {
      return(NULL)
    } else {
      paste0(el[[select_func2()]], "% eligibility for patients with EF ", paste0(names(appvar_values$shf_ef)[appvar_values$shf_ef %in% input$ef], collapse = ", "), "%")
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
        rate[[select_func2()]] %>%
          filter(outname %in% c(input$out)) %>%
          arrange(outname, time) %>%
          select(-rate_0, -rate_1),
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
  output$plot <- renderPlot(
    if (select_func2() %in% c(-1, 0) | is.null(input$out)) {
      return(NULL)
    } else {
      figfunc(rate[[select_func2()]] %>% filter(outname %in% c(input$out)))
    }
  )
}


# Create Shiny app ----
shinyApp(ui, server)

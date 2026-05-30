library(shiny)
library(bslib)
library(HowDirty)

ui <- page_sidebar(
  title = "HowDirty — Contamination Report Generator",
  sidebar = sidebar(
    width = 380,

    fileInput("peaks", "Step 1 — Skyline peak areas CSV",
              accept      = ".csv",
              buttonLabel = "Browse…",
              placeholder = "No file selected"),

    uiOutput("dl_template_ui"),

    fileInput("annot", "Step 2 — Sample annotation CSV",
              accept      = ".csv",
              buttonLabel = "Browse…",
              placeholder = "No file selected"),

    fileInput("ref", "Step 3 — Reference thresholds Excel (optional)",
              accept      = c(".xlsx", ".xls"),
              buttonLabel = "Browse…",
              placeholder = "No file selected"),

    textInput("dataset", "Experiment name", placeholder = "e.g. MyExperiment"),

    accordion(
      open = NULL,
      accordion_panel(
        "Advanced options",
        numericInput("n_top", "Top contaminant groups in plots",
                     value = 10, min = 1, step = 1),
        checkboxInput("remove_missing",
                      "Remove undetected contaminants", TRUE),
        checkboxInput("multiply_dilution",
                      "Multiply by dilution factor", FALSE),
        checkboxInput("interactive_plots",
                      "Interactive plots (slower)", FALSE),
        textInput("user_names", "Analyst name(s)"),
        textAreaInput("notes", "Notes", rows = 2)
      )
    ),

    hr(),
    actionButton("run", "Generate Report",
                 class = "btn-primary btn-lg w-100"),
    uiOutput("missing_msg")
  ),

  uiOutput("main_panel")
)

server <- function(input, output, session) {

  # ---- Annotation template ------------------------------------------------
  annot_template_rv <- reactive({
    req(input$peaks)
    get_annotation_template(
      file_report_skyline = input$peaks$datapath,
      save = FALSE
    )
  })

  output$dl_template_ui <- renderUI({
    req(input$peaks)
    downloadButton("dl_template", "Download annotation template",
                   class = "btn-outline-primary w-100 mb-2")
  })

  output$dl_template <- downloadHandler(
    filename = "samples_annotation_template.csv",
    content  = function(f) write.csv(annot_template_rv(), f, row.names = FALSE)
  )

  # ---- Missing inputs hint ------------------------------------------------
  output$missing_msg <- renderUI({
    missing <- character(0)
    if (is.null(input$peaks))   missing <- c(missing, "peak areas CSV")
    if (is.null(input$annot))   missing <- c(missing, "annotation CSV")
    if (!nzchar(input$dataset)) missing <- c(missing, "experiment name")
    if (length(missing) == 0)   return(NULL)
    tags$small(class = "text-muted d-block mt-2",
      paste("Still needed:", paste(missing, collapse = ", ")))
  })

  # ---- Results state ------------------------------------------------------
  rv <- reactiveValues(html_path = NULL, xlsx_path = NULL, error = NULL)

  observeEvent(list(input$peaks, input$annot, input$ref), {
    rv$html_path <- NULL
    rv$xlsx_path <- NULL
    rv$error     <- NULL
  }, ignoreInit = TRUE)

  # ---- Run report ---------------------------------------------------------
  observeEvent(input$run, {
    req(input$peaks, input$annot, nzchar(input$dataset))

    rv$html_path <- NULL
    rv$xlsx_path <- NULL
    rv$error     <- NULL

    tmpdir <- tempfile()
    dir.create(tmpdir)

    withProgress(message = "Generating report, please wait…", value = 0.5, {
      tryCatch({
        generate_howdirty_report(
          dataset                     = input$dataset,
          file_peak_areas             = input$peaks$datapath,
          file_annotation             = input$annot$datapath,
          file_ref_thresholds         = if (!is.null(input$ref)) input$ref$datapath else FALSE,
          output_directory            = tmpdir,
          output_dir                  = tmpdir,
          remove_missing_contaminants = input$remove_missing,
          n_top_contaminant_groups    = input$n_top,
          multiply_dilution_factor    = input$multiply_dilution,
          plots_interactive           = input$interactive_plots,
          user_names                  = input$user_names,
          notes                       = input$notes
        )
        setProgress(1)
        rv$html_path <- list.files(tmpdir, pattern = "\\.html$", full.names = TRUE)[1]
        rv$xlsx_path <- list.files(tmpdir, pattern = "\\.xlsx$", full.names = TRUE)[1]
      }, error = function(e) {
        rv$error <- conditionMessage(e)
      })
    })
  })

  # ---- Downloads ----------------------------------------------------------
  output$dl_html <- downloadHandler(
    filename = function() paste0(input$dataset, "_HowDirtyReport.html"),
    content  = function(f) file.copy(rv$html_path, f)
  )
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(input$dataset, "_HowDirtyReport.xlsx"),
    content  = function(f) file.copy(rv$xlsx_path, f)
  )

  # ---- Table previews -----------------------------------------------------
  output$template_dt <- DT::renderDT(
    annot_template_rv(),
    options  = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )

  output$annot_dt <- DT::renderDT({
    req(input$annot)
    read.csv(input$annot$datapath)
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)

  # ---- Main panel ---------------------------------------------------------
  output$main_panel <- renderUI({
    if (!is.null(rv$error)) {
      card(card_body(
        div(class = "alert alert-danger mb-0",
          tags$strong("Error: "), rv$error)
      ))

    } else if (!is.null(rv$html_path)) {
      card(card_body(
        div(class = "alert alert-success mb-3",
          tags$strong("Report generated successfully!")),
        tags$p("Download your results:"),
        tags$div(
          class = "d-flex gap-2",
          downloadButton("dl_html", "HTML report",    class = "btn-primary"),
          downloadButton("dl_xlsx", "Excel workbook", class = "btn-success")
        )
      ))

    } else if (!is.null(input$annot)) {
      card(
        card_header("Annotation file preview"),
        card_body(DT::DTOutput("annot_dt"))
      )

    } else if (!is.null(input$peaks)) {
      card(
        card_header(
          "Annotation template — download, fill in, then upload in Step 2"),
        card_body(DT::DTOutput("template_dt"))
      )

    } else {
      card(card_body(
        class = "text-center py-5",
        tags$h4("Welcome to HowDirty"),
        tags$p(class = "text-muted mb-4",
          "Follow the steps on the left to generate your contamination report."),
        tags$ol(
          class = "text-start d-inline-block",
          tags$li("Upload your Skyline peak areas CSV"),
          tags$li("Download the annotation template, fill it in, then upload it"),
          tags$li("Optionally upload a reference thresholds file"),
          tags$li("Enter an experiment name and click Generate Report"),
          tags$li("Download the HTML report and Excel workbook")
        )
      ))
    }
  })
}

shinyApp(ui, server)

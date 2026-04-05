# =============================================================================
# Computo Metrico — Automated Cost Estimation (Shiny)
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(readxl)
library(DT)
library(ggplot2)

# --- Helpers -----------------------------------------------------------------

read_table_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext %in% c("csv", "txt")) {
    readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  } else {
    stop("Formato non supportato. Usa CSV o Excel (.csv, .xlsx, .xls).")
  }
}

REQ_QUANT <- c("id_materiale", "descrizione", "quantita")
REQ_PRICE <- c("id_materiale", "prezzo_unitario")

validate_columns <- function(df, required, label) {
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    return(paste0(
      "File ", label, ": colonne mancanti: ",
      paste(miss, collapse = ", "),
      ". Attese: ", paste(required, collapse = ", "), "."
    ))
  }
  NULL
}

compute_cost_table <- function(quant_path, price_path) {
  if (is.null(quant_path) || is.null(price_path)) {
    return("Carica entrambi i file (quantità e listino prezzi).")
  }
  
  q <- tryCatch(
    read_table_file(quant_path),
    error = function(e) paste0("Errore lettura quantità: ", conditionMessage(e))
  )
  if (is.character(q)) return(q)
  
  p <- tryCatch(
    read_table_file(price_path),
    error = function(e) paste0("Errore lettura listino: ", conditionMessage(e))
  )
  if (is.character(p)) return(p)
  
  e1 <- validate_columns(q, REQ_QUANT, "quantità")
  if (!is.null(e1)) return(e1)
  e2 <- validate_columns(p, REQ_PRICE, "prezzi")
  if (!is.null(e2)) return(e2)
  
  q <- q %>%
    mutate(
      id_materiale = as.character(id_materiale),
      quantita = suppressWarnings(as.numeric(quantita))
    )
  
  p <- p %>%
    mutate(
      id_materiale = as.character(id_materiale),
      prezzo_unitario = suppressWarnings(as.numeric(prezzo_unitario))
    )
  
  out <- q %>%
    left_join(p, by = "id_materiale") %>%
    mutate(costo_totale = quantita * prezzo_unitario)
  
  out
}

trunc_label <- function(x, max_chars = 42) {
  x <- as.character(x)
  ifelse(
    nchar(x) <= max_chars,
    x,
    paste0(substr(x, 1, max_chars - 3), "...")
  )
}

# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Computo metrico — Stima automatica dei costi",
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#0d6efd"
  ),
  sidebar = sidebar(
    title = "Dati in ingresso",
    width = 360,
    fileInput(
      "file_quant",
      "File quantità (CSV / Excel)",
      accept = c(".csv", ".txt", ".xlsx", ".xls"),
      buttonLabel = "Sfoglia…",
      placeholder = "Colonne: id_materiale, descrizione, quantita"
    ),
    fileInput(
      "file_price",
      "Listino prezzi (CSV / Excel)",
      accept = c(".csv", ".txt", ".xlsx", ".xls"),
      buttonLabel = "Sfoglia…",
      placeholder = "Colonne: id_materiale, prezzo_unitario"
    ),
    hr(),
    downloadButton("download_csv", "Scarica tabella (CSV)", class = "btn-primary w-100"),
    helpText(
      style = "font-size: 0.85rem;",
      "Unisci quantità e prezzi su ", tags$code("id_materiale"),
      ". Il costo totale è ", tags$code("quantita × prezzo_unitario"), "."
    )
  ),
  layout_columns(
    col_widths = c(12),
    uiOutput("total_cost_box")
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header("Dettaglio voci calcolate"),
      uiOutput("table_or_message")
    ),
    card(
      full_screen = TRUE,
      card_header("Top 5 materiali per impatto sul costo"),
      plotOutput("plot_top5", height = "380px")
    )
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  cost_data <- reactive({
    if (is.null(input$file_quant) || is.null(input$file_price)) {
      return(NULL)
    }
    compute_cost_table(
      input$file_quant$datapath,
      input$file_price$datapath
    )
  })
  
  has_table <- reactive({
    d <- cost_data()
    is.data.frame(d) && nrow(d) > 0
  })
  
  output$total_cost_box <- renderUI({
    d <- cost_data()
    if (is.null(d)) {
      return(
        layout_columns(
          value_box(
            title = "Benvenuto",
            value = "Carica i due file per iniziare",
            showcase = icon("file-upload", lib = "font-awesome"),
            theme = "secondary"
          )
        )
      )
    }
    if (is.character(d)) {
      return(
        layout_columns(
          value_box(
            title = "Stato",
            value = "Dati non validi — vedi messaggio sotto",
            showcase = icon("exclamation-triangle", lib = "font-awesome"),
            theme = "danger"
          )
        )
      )
    }
    if (!is.data.frame(d) || nrow(d) == 0) {
      return(
        layout_columns(
          value_box(
            title = "Costo totale progetto",
            value = "—",
            showcase = icon("euro-sign", lib = "font-awesome"),
            theme = "secondary"
          )
        )
      )
    }
    tot <- sum(d$costo_totale, na.rm = TRUE)
    layout_columns(
      value_box(
        title = "Costo totale progetto",
        value = sprintf("€ %s", format(round(tot, 2), big.mark = " ", decimal.mark = ",", scientific = FALSE)),
        showcase = icon("euro-sign", lib = "font-awesome"),
        theme = "success"
      )
    )
  })
  
  output$table_or_message <- renderUI({
    d <- cost_data()
    if (is.null(d)) {
      return(tags$p(class = "text-muted", "Carica il file quantità e il listino prezzi nella barra laterale."))
    }
    if (is.character(d)) {
      return(tags$div(class = "alert alert-warning", role = "alert", d))
    }
    if (!is.data.frame(d) || nrow(d) == 0) {
      return(tags$p(class = "text-muted", "Nessuna riga da mostrare."))
    }
    DTOutput("tbl_costs")
  })
  
  output$tbl_costs <- renderDT({
    req(has_table())
    d <- cost_data()
    datatable(
      d,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/it-IT.json")
      ),
      class = "display compact stripe hover"
    ) %>%
      formatCurrency(
        columns = intersect(names(d), c("prezzo_unitario", "costo_totale")),
        currency = "€",
        digits = 2,
        dec.mark = ",",
        before = TRUE
      )
  })
  
  output$plot_top5 <- renderPlot({
    d <- cost_data()
    if (is.null(d)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Carica i file per il grafico.", size = 4) +
          theme_void()
      )
    }
    if (!is.data.frame(d) || nrow(d) == 0 || is.character(d)) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = if (is.character(d)) d else "Nessun dato da visualizzare.",
            size = 4
          ) +
          theme_void()
      )
    }
    
    top <- d %>%
      mutate(
        costo_totale = ifelse(is.na(costo_totale), 0, costo_totale),
        lbl = ifelse(
          is.na(descrizione) | descrizione == "",
          as.character(id_materiale),
          paste0(as.character(id_materiale), " — ", trunc_label(descrizione, 40))
        )
      ) %>%
      slice_max(order_by = costo_totale, n = 5, with_ties = FALSE)
    
    if (nrow(top) == 0) {
      return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Nessun dato.", size = 4))
    }
    
    euro_fmt <- function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)
    
    ggplot(top, aes(x = reorder(lbl, costo_totale), y = costo_totale)) +
      geom_col(fill = "#0d6efd", width = 0.75, alpha = 0.9) +
      coord_flip() +
      labs(x = NULL, y = "Costo totale (€)", title = NULL) +
      scale_y_continuous(labels = euro_fmt) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        plot.margin = margin(8, 8, 8, 8)
      )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("computo_metrico_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      d <- cost_data()
      if (!is.data.frame(d)) {
        readr::write_csv(
          data.frame(messaggio = "Nessun dato da esportare. Carica file validi."),
          file,
          na = ""
        )
      } else {
        readr::write_excel_csv(d, file, delim = ";", na = "")
      }
    }
  )
}

# --- Run ---------------------------------------------------------------------

shinyApp(ui, server)

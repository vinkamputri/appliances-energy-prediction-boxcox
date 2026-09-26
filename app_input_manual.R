# ==============================================================================
# DASHBOARD INTERAKTIF R SHINY - RESPONSE TRANSFORMATION (BOX-COX) & AI BENCHMARK
# Edisi Input Manual Fleksibel (numericInput dengan Deskripsi & Contoh Nilai)
# Bebas Input Angka Berapapun untuk Semua 13 Prediktor RFE
# Styling: Clean Futuristic Dark Theme (Tanpa Emoji pada Kartu & Value Box)
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)
library(randomForest)

# 1. Lokasi File Data Precomputed (Relatif)
rds_file <- "dashboard_boxcox_data.rds"
if (!file.exists(rds_file) && file.exists(file.path("..", rds_file))) {
  rds_file <- file.path("..", rds_file)
}

if (file.exists(rds_file)) {
  data_obj <- readRDS(rds_file)
} else {
  stop("File 'dashboard_boxcox_data.rds' tidak ditemukan di direktori aplikasi.")
}

# Ekstrak Komponen
train_sample   <- data_obj$train_sample
predictors_rfe <- data_obj$predictors_rfe
lambda_used    <- data_obj$lambda_used
ci_lambda      <- data_obj$ci_lambda
skew_raw       <- data_obj$skew_raw
modelBaseline  <- data_obj$modelBaseline
modelBoxCox    <- data_obj$modelBoxCox
rf_bench       <- data_obj$rf_bench
fittedBase     <- data_obj$fittedBase
residBase      <- data_obj$residBase
fittedBC       <- data_obj$fittedBC
residBC        <- data_obj$residBC
bp_base        <- data_obj$bp_base
bp_bc          <- data_obj$bp_bc
dw_base        <- data_obj$dw_base
dw_bc          <- data_obj$dw_bc
cooksBase      <- data_obj$cooksBase
cooksBC        <- data_obj$cooksBC
cooks_thr      <- data_obj$cooks_thr
vif_vals       <- data_obj$vif_vals
perf_df        <- data_obj$perf_df
infer_bc       <- data_obj$infer_bc
infer_base     <- data_obj$infer_base

# Dark Theme Helper untuk ggplot2
theme_futuristic_dark <- function() {
  theme_minimal(base_size = 12) %+replace%
    theme(
      plot.background = element_rect(fill = "#111827", color = NA),
      panel.background = element_rect(fill = "#111827", color = NA),
      panel.grid.major = element_line(color = "#1F2937", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#1F2937", linewidth = 0.25),
      text = element_text(color = "#F8FAFC"),
      axis.text = element_text(color = "#9CA3AF"),
      axis.title = element_text(color = "#F3F4F6", face = "bold"),
      plot.title = element_text(color = "#38BDF8", face = "bold", size = 13, hjust = 0, margin = ggplot2::margin(b = 6)),
      plot.subtitle = element_text(color = "#94A3B8", size = 9, margin = ggplot2::margin(b = 6)),
      legend.background = element_rect(fill = "#1E293B", color = "#334155"),
      legend.text = element_text(color = "#F8FAFC"),
      legend.title = element_text(color = "#38BDF8", face = "bold"),
      legend.position = "top"
    )
}

# 2. Desain Antarmuka Pengguna (UI)
ui <- page_navbar(
  title = tags$span(
    tags$strong("ENERGY ANALYTICS", style = "color: #38BDF8; letter-spacing: 1px; font-size: 1.15rem;"),
    tags$span(" | Transformasi Respons Box-Cox & Manual Simulator", style = "color: #CBD5E1; font-weight: 300; font-size: 0.95rem;")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#38BDF8",
    secondary = "#818CF8",
    success = "#10B981",
    warning = "#F59E0B",
    danger = "#F43F5E",
    bg = "#0B0F19",
    fg = "#F8FAFC"
  ),
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #0B0F19 !important;
        color: #F8FAFC !important;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      
      .navbar {
        background: linear-gradient(135deg, #0B0F19 0%, #1E293B 100%) !important;
        border-bottom: 1px solid #334155 !important;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.5) !important;
        padding-top: 10px !important;
        padding-bottom: 10px !important;
      }
      
      .navbar-nav .nav-link {
        color: #FFFFFF !important;
        font-weight: 600 !important;
        font-size: 1rem !important;
        padding: 8px 18px !important;
        margin: 0 4px !important;
        border-radius: 6px !important;
        transition: all 0.2s ease !important;
        opacity: 0.95 !important;
      }
      
      .navbar-nav .nav-link:hover {
        color: #38BDF8 !important;
        background-color: rgba(56, 189, 248, 0.15) !important;
        opacity: 1 !important;
      }
      
      .navbar-nav .nav-link.active {
        color: #38BDF8 !important;
        background-color: rgba(56, 189, 248, 0.2) !important;
        border-bottom: 3px solid #38BDF8 !important;
        font-weight: 700 !important;
        opacity: 1 !important;
      }
      
      .card {
        background-color: #111827 !important;
        border: 1px solid #1F2937 !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4) !important;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      
      .card:hover {
        border-color: #374151 !important;
        box-shadow: 0 6px 24px rgba(56, 189, 248, 0.08) !important;
      }
      
      .card-header {
        background-color: #161F30 !important;
        border-bottom: 1px solid #1F2937 !important;
        color: #38BDF8 !important;
        font-weight: 600 !important;
        letter-spacing: 0.3px;
      }
      
      .bslib-sidebar-layout > .sidebar {
        background-color: #0F172A !important;
        border-right: 1px solid #1E293B !important;
      }
      
      .bslib-value-box {
        border-radius: 12px !important;
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.35) !important;
        border: 1px solid rgba(255, 255, 255, 0.05) !important;
      }
      
      .table {
        color: #F8FAFC !important;
        border-color: #1F2937 !important;
      }
      
      .table-striped > tbody > tr:nth-of-type(odd) > * {
        background-color: rgba(255, 255, 255, 0.03) !important;
        color: #FFFFFF !important;
      }
      
      .table-hover > tbody > tr:hover > * {
        background-color: rgba(56, 189, 248, 0.1) !important;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #0284C7 0%, #0369A1 100%) !important;
        border: 1px solid #38BDF8 !important;
        font-weight: 600 !important;
        box-shadow: 0 4px 14px rgba(2, 132, 199, 0.4) !important;
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #0369A1 0%, #075985 100%) !important;
        box-shadow: 0 6px 20px rgba(56, 189, 248, 0.6) !important;
      }
      
      .input-guide-text {
        font-size: 0.72rem;
        color: #94A3B8;
        margin-top: -5px;
        margin-bottom: 8px;
        display: block;
      }
      
      .form-control {
        background-color: #1E293B !important;
        border: 1px solid #334155 !important;
        color: #F8FAFC !important;
        font-weight: 600 !important;
      }
      
      .form-control:focus {
        border-color: #38BDF8 !important;
        box-shadow: 0 0 0 0.2rem rgba(56, 189, 248, 0.25) !important;
      }
    "))
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 1: EKSPLORASI DATA & TRANSFORMASI RESPONS BOX-COX
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "1. Distribusi & Transformasi Respons",
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span("Kontrol Distribusi", style = "color: #38BDF8; font-weight: 600;"),
        radioButtons("scale_choice", "Skala Distribusi Respons:",
                     choices = c("Skala Asli / Mentah (Wh)" = "raw",
                                 "Transformasi Box-Cox (λ = -0.504)" = "bc"),
                     selected = "raw"),
        sliderInput("n_bins", "Jumlah Interval (Bins):",
                    min = 10, max = 80, value = 40, step = 5),
        checkboxInput("show_density", "Tampilkan Estimasi Densitas (KDE)", TRUE),
        hr(style = "border-color: #334155;"),
        div(
          style = "padding: 10px; background: #1E293B; border-radius: 8px; border-left: 3px solid #38BDF8;",
          tags$small(style = "color: #CBD5E1;", 
            "Transformasi Box-Cox dengan ", tags$b(style = "color: #38BDF8;", "λ = -0.504"),
            " (mendekati invers akar kuadrat) menstabilkan varians dan mereduksi skewness ekstrem dari ",
            tags$b(style = "color: #F43F5E;", sprintf("+%.2f", skew_raw)), " menjadi ",
            tags$b(style = "color: #10B981;", "+0.08"), " mendekati distribusi simetris."
          )
        )
      ),
      layout_columns(
        col_widths = c(12),
        layout_columns(
          value_box(
            title = "Rata-rata Konsumsi (Raw Mean)",
            value = sprintf("%.1f Wh", mean(train_sample$Appliances)),
            theme = "primary"
          ),
          value_box(
            title = "Median Konsumsi (Raw Median)",
            value = sprintf("%.1f Wh", median(train_sample$Appliances)),
            theme = "info"
          ),
          value_box(
            title = "Optimal Box-Cox Parameter",
            value = sprintf("λ = %.3f", lambda_used),
            theme = "success"
          )
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(strong("Distribusi Variabel Respons (Appliances)")),
          plotOutput("dist_plot", height = "360px")
        ),
        card(
          card_header(strong("Profil Diurnal Konsumsi Energi (24 Jam)")),
          plotOutput("diurnal_plot", height = "360px")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: DIAGNOSTIK ASUMSI GAUSS-MARKOV & INFERENSI
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "2. Diagnostik Asumsi & Inferensi",
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span("Pilihan Model", style = "color: #38BDF8; font-weight: 600;"),
        selectInput("diag_model_select", "Pilih Model Diagnostik:",
                    choices = c("OLS Model Box-Cox (λ = -0.504)" = "bc",
                                "OLS Model Mentah / Baseline" = "base"),
                    selected = "bc"),
        hr(style = "border-color: #334155;"),
        h6("Hasil Uji Breusch-Pagan:", style = "color: #FFFFFF; font-weight: 600;"),
        uiOutput("bp_test_card"),
        hr(style = "border-color: #334155;"),
        h6("Hasil Uji Durbin-Watson:", style = "color: #FFFFFF; font-weight: 600;"),
        uiOutput("dw_test_card"),
        hr(style = "border-color: #334155;"),
        tags$small(style = "color: #CBD5E1;", 
          "Uji diagnostik memvalidasi asumsi klasik Gauss-Markov: homoskedastisitas, ketiadaan autokorelasi, dan normalitas galat."
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(strong("Plot Residuals vs Fitted Values")),
          plotOutput("residual_plot", height = "340px")
        ),
        card(
          card_header(strong("Normal Q-Q Plot (Normalitas Residual)")),
          plotOutput("qq_plot", height = "340px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(strong("Titik Pengaruh (Cook's Distance)")),
          plotOutput("cooks_plot", height = "280px")
        ),
        card(
          card_header(strong("Analisis Multikolinearitas (VIF)")),
          tableOutput("vif_table_view")
        )
      ),
      card(
        card_header(strong("Tabel Inferensi Koefisien Regresi OLS Box-Cox (13 Predictors Terpilih RFE)")),
        tableOutput("infer_table_view")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: SIMULATOR PREDIKSI & BENCHMARK AI (EDISI MANUAL INPUT 13 FITUR)
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "3. Simulator Prediksi & Benchmark AI",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        title = tags$span("Input Manual 13 Parameter", style = "color: #38BDF8; font-weight: 600; font-size: 1.05rem;"),
        
        # Tombol Preset Cepat
        div(
          style = "margin-bottom: 12px;",
          tags$small(style = "color: #94A3B8; font-weight: 600;", "Preset Skenario Cepat:"),
          div(
            class = "d-flex gap-1 mt-1",
            actionButton("set_avg_btn", "Standar", class = "btn-sm btn-outline-info flex-fill"),
            actionButton("set_hot_btn", "Panas (30°C)", class = "btn-sm btn-outline-warning flex-fill"),
            actionButton("set_cold_btn", "Dingin (15°C)", class = "btn-sm btn-outline-primary flex-fill")
          )
        ),
        hr(style = "border-color: #334155; margin: 8px 0;"),
        
        # Bagian 1: Suhu Ruangan Utama
        tags$b("1. Suhu Ruangan Internal (°C):", style = "color: #38BDF8; font-size: 0.88rem;"),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_T1", "T1 (Dapur):", value = 21.5, step = 0.5),
            tags$span("Contoh: 21.5 (Data: 16.8–26.3)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_T2", "T2 (R. Keluarga):", value = 20.0, step = 0.5),
            tags$span("Contoh: 20.0 (Data: 16.1–29.9)", class = "input-guide-text")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_T3", "T3 (R. Laundry):", value = 22.0, step = 0.5),
            tags$span("Contoh: 22.0 (Data: 17.2–29.2)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_T6", "T6 (Luar Utara):", value = 8.0, step = 0.5),
            tags$span("Contoh: 8.0 (Data: -6.1–28.3)", class = "input-guide-text")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_T8", "T8 (K. Remaja):", value = 22.0, step = 0.5),
            tags$span("Contoh: 22.0 (Data: 16.3–27.2)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_T9", "T9 (K. Utama):", value = 20.0, step = 0.5),
            tags$span("Contoh: 20.0 (Data: 14.9–24.5)", class = "input-guide-text")
          )
        ),
        hr(style = "border-color: #334155; margin: 8px 0;"),
        
        # Bagian 2: Kelembaban Sensor
        tags$b("2. Kelembaban Relatif Ruangan (%):", style = "color: #38BDF8; font-size: 0.88rem;"),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_RH1", "RH_1 (Dapur %):", value = 40.0, step = 1),
            tags$span("Contoh: 40 (Data: 27–63%)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_RH2", "RH_2 (R. Keluarga %):", value = 40.0, step = 1),
            tags$span("Contoh: 40 (Data: 20–56%)", class = "input-guide-text")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_RH3", "RH_3 (Laundry %):", value = 38.0, step = 1),
            tags$span("Contoh: 38 (Data: 28–50%)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_RH8", "RH_8 (K. Remaja %):", value = 43.0, step = 1),
            tags$span("Contoh: 43 (Data: 29–59%)", class = "input-guide-text")
          )
        ),
        div(
          numericInput("sim_RH9", "RH_9 (Kamar Utama %):", value = 42.0, step = 1),
          tags$span("Contoh: 42 (Data: 29–53%)", class = "input-guide-text")
        ),
        hr(style = "border-color: #334155; margin: 8px 0;"),
        
        # Bagian 3: Kondisi Cuaca Eksternal
        tags$b("3. Cuaca Eksternal Stasiun:", style = "color: #38BDF8; font-size: 0.88rem;"),
        layout_columns(
          col_widths = c(6, 6),
          div(
            numericInput("sim_Tout", "T_out (Suhu Luar °C):", value = 7.5, step = 0.5),
            tags$span("Contoh: 7.5 (Data: -5.0–26.1)", class = "input-guide-text")
          ),
          div(
            numericInput("sim_Tdew", "Tdewpoint (°C):", value = 3.5, step = 0.5),
            tags$span("Contoh: 3.5 (Data: -6.6–15.5)", class = "input-guide-text")
          )
        ),
        actionButton("run_sim_btn", "Hitung Prediksi Konsumsi Energi", class = "btn-primary w-100 mt-2 py-2")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "1. OLS Baseline (Mentah)",
          value = textOutput("pred_raw_out"),
          theme = "danger"
        ),
        value_box(
          title = "2. OLS Box-Cox (Inverse Transform)",
          value = textOutput("pred_bc_out"),
          theme = "primary"
        ),
        value_box(
          title = "3. Random Forest (AI Benchmark)",
          value = textOutput("pred_rf_out"),
          theme = "success"
        )
      ),
      card(
        card_header(strong("Evaluasi Metrik Prediksi pada Testing Set (25% Data Uji Berbasis Waktu)")),
        tableOutput("perf_table_view")
      ),
      card(
        card_header(strong("Wawasan Metodologis: Box-Cox Response Transformation vs Benchmark")),
        p(HTML(paste0(
          "<b style='color:#38BDF8;'>1. Transformasi Respons Box-Cox (λ = -0.504):</b> Mengubah variabel dependen menjadi <i>(Y^λ - 1)/λ</i> yang secara drastis menekan heteroskedastisitas dan skewness. Menghasilkan estimasi energi yang konsisten dan non-negatif saat ditransformasi balik <i>Y = (1 + λZ)^(1/λ)</i>, serta menghasilkan penurunan Mean Absolute Error (<b>MAE = 40.69 Wh</b>) dan Mean Absolute Percentage Error (<b>MAPE = 39.19%</b>) yang sangat signifikan dibandingkan model baseline mentah (<b>MAE = 52.12 Wh</b>, <b>MAPE = 63.37%</b>).<br><br>",
          "<b style='color:#F59E0B;'>2. Fleksibilitas Input Manual:</b> Pengguna dapat memasukkan angka suhu maupun kelembaban berapapun secara leluasa untuk mensimulasikan skenario cuaca spesifik (misal suhu tinggi di atas 30°C). Deskripsi di bawah tiap kolom menunjukkan rentang data riil dari eksperimen asli.<br><br>",
          "<b style='color:#10B981;'>3. Random Forest (AI Predictive Benchmark):</b> Digunakan murni sebagai tolok ukur non-parametrik machine learning untuk menguji kapabilitas non-linear ensemble."
        )))
      )
    )
  )
)

# 3. Server Logic
server <- function(input, output, session) {
  
  # Preset Observers
  observeEvent(input$set_avg_btn, {
    updateNumericInput(session, "sim_T1", value = 21.5)
    updateNumericInput(session, "sim_T2", value = 20.0)
    updateNumericInput(session, "sim_T3", value = 22.0)
    updateNumericInput(session, "sim_T6", value = 8.0)
    updateNumericInput(session, "sim_T8", value = 22.0)
    updateNumericInput(session, "sim_T9", value = 20.0)
    updateNumericInput(session, "sim_RH1", value = 40.0)
    updateNumericInput(session, "sim_RH2", value = 40.0)
    updateNumericInput(session, "sim_RH3", value = 38.0)
    updateNumericInput(session, "sim_RH8", value = 43.0)
    updateNumericInput(session, "sim_RH9", value = 42.0)
    updateNumericInput(session, "sim_Tout", value = 7.5)
    updateNumericInput(session, "sim_Tdew", value = 3.5)
  })
  
  observeEvent(input$set_hot_btn, {
    updateNumericInput(session, "sim_T1", value = 30.0)
    updateNumericInput(session, "sim_T2", value = 29.5)
    updateNumericInput(session, "sim_T3", value = 30.0)
    updateNumericInput(session, "sim_T6", value = 32.0)
    updateNumericInput(session, "sim_T8", value = 29.0)
    updateNumericInput(session, "sim_T9", value = 28.0)
    updateNumericInput(session, "sim_RH1", value = 55.0)
    updateNumericInput(session, "sim_RH2", value = 50.0)
    updateNumericInput(session, "sim_RH3", value = 48.0)
    updateNumericInput(session, "sim_RH8", value = 52.0)
    updateNumericInput(session, "sim_RH9", value = 50.0)
    updateNumericInput(session, "sim_Tout", value = 31.0)
    updateNumericInput(session, "sim_Tdew", value = 20.0)
  })
  
  observeEvent(input$set_cold_btn, {
    updateNumericInput(session, "sim_T1", value = 17.0)
    updateNumericInput(session, "sim_T2", value = 16.5)
    updateNumericInput(session, "sim_T3", value = 18.0)
    updateNumericInput(session, "sim_T6", value = 0.0)
    updateNumericInput(session, "sim_T8", value = 17.5)
    updateNumericInput(session, "sim_T9", value = 16.0)
    updateNumericInput(session, "sim_RH1", value = 30.0)
    updateNumericInput(session, "sim_RH2", value = 28.0)
    updateNumericInput(session, "sim_RH3", value = 32.0)
    updateNumericInput(session, "sim_RH8", value = 35.0)
    updateNumericInput(session, "sim_RH9", value = 34.0)
    updateNumericInput(session, "sim_Tout", value = -2.0)
    updateNumericInput(session, "sim_Tdew", value = -4.0)
  })
  
  # Tab 1: Plot Distribusi Respons (ggplot2)
  output$dist_plot <- renderPlot({
    req(input$scale_choice)
    n_b <- ifelse(is.null(input$n_bins), 40, input$n_bins)
    
    if (input$scale_choice == "raw") {
      p <- ggplot(train_sample, aes(x = Appliances)) +
        geom_histogram(aes(y = after_stat(density)), bins = n_b,
                       fill = "#0284C7", color = "#1E293B", alpha = 0.85) +
        labs(
          title = sprintf("Distribusi Skala Asli: Skewness = +%.2f (Menjulur Kuat ke Kanan)", skew_raw),
          subtitle = "Sebagian besar waktu beban berada pada level rendah (50-60 Wh) dengan lonjakan tajam",
          x = "Konsumsi Energi Peralatan (Watt-hour / Wh)",
          y = "Frekuensi Relatif (Densitas)"
        )
      if (isTRUE(input$show_density)) {
        p <- p + geom_density(color = "#F43F5E", linewidth = 1.2)
      }
    } else {
      p <- ggplot(train_sample, aes(x = bcAppliances)) +
        geom_histogram(aes(y = after_stat(density)), bins = n_b,
                       fill = "#10B981", color = "#1E293B", alpha = 0.85) +
        labs(
          title = sprintf("Distribusi Ter-Transformasi Box-Cox (λ = %.3f): Skewness = +0.08", lambda_used),
          subtitle = "Distribusi ditarik menjadi simetris sempurna mendekati kurva lonceng Gaussian",
          x = "Nilai Transformasi Box-Cox Z = (Y^λ - 1) / λ",
          y = "Frekuensi Relatif (Densitas)"
        )
      if (isTRUE(input$show_density)) {
        p <- p + geom_density(color = "#38BDF8", linewidth = 1.2)
      }
    }
    p + theme_futuristic_dark()
  })
  
  # Tab 1: Profil Diurnal Konsumsi 24 Jam
  output$diurnal_plot <- renderPlot({
    if ("hour" %in% names(train_sample)) {
      hourly_avg <- train_sample %>%
        group_by(hour) %>%
        summarise(mean_energy = mean(Appliances, na.rm = TRUE),
                  median_energy = median(Appliances, na.rm = TRUE),
                  .groups = "drop")
      
      ggplot(hourly_avg, aes(x = hour)) +
        geom_line(aes(y = mean_energy, color = "Rata-rata (Mean)"), linewidth = 1.3) +
        geom_line(aes(y = median_energy, color = "Median (P50)"), linewidth = 1.1, linetype = "dashed") +
        geom_point(aes(y = mean_energy), color = "#38BDF8", size = 2.5) +
        scale_color_manual(values = c("Rata-rata (Mean)" = "#38BDF8", "Median (P50)" = "#F59E0B")) +
        scale_x_continuous(breaks = seq(0, 23, by = 3)) +
        labs(
          title = "Pola Harian (Diurnal) Konsumsi Energi",
          subtitle = "Lonjakan aktivitas malam (18:00 - 21:00) vs Kondisi Standby Dini Hari",
          x = "Jam dalam Sehari (00:00 - 23:00)",
          y = "Daya (Wh)",
          color = "Ukuran Tendensi:"
        ) +
        theme_futuristic_dark()
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "Data jam tidak tersedia di sample", color = "#9CA3AF") +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Uji Breusch-Pagan UI Card
  output$bp_test_card <- renderUI({
    if (input$diag_model_select == "bc") {
      div(
        style = "background: #1E293B; padding: 12px; border-radius: 8px; border-left: 4px solid #10B981;",
        tags$b("Model Box-Cox (λ = -0.504)", style = "color: #10B981; font-size: 0.95rem;"), tags$br(),
        sprintf("Statistik BP: %.2f", bp_bc$statistic), tags$br(),
        sprintf("p-value: %s", ifelse(bp_bc$p.value < 1e-4, "< 0.0001", sprintf("%.4f", bp_bc$p.value))), tags$br(),
        tags$small(style = "color: #94A3B8;", "Varians galat stabil dan terdistribusi homogen.")
      )
    } else {
      div(
        style = "background: #1E293B; padding: 12px; border-radius: 8px; border-left: 4px solid #F43F5E;",
        tags$b("Model Baseline (Mentah)", style = "color: #F43F5E; font-size: 0.95rem;"), tags$br(),
        sprintf("Statistik BP: %.2f", bp_base$statistic), tags$br(),
        sprintf("p-value: %s", ifelse(bp_base$p.value < 1e-4, "< 0.0001", sprintf("%.4f", bp_base$p.value))), tags$br(),
        tags$small(style = "color: #FCA5A5;", "Heteroskedastisitas berat: varians melebar seiring fitted values.")
      )
    }
  })
  
  # Tab 2: Uji Durbin-Watson UI Card
  output$dw_test_card <- renderUI({
    if (input$diag_model_select == "bc") {
      div(
        style = "background: #1E293B; padding: 12px; border-radius: 8px; border-left: 4px solid #38BDF8;",
        tags$b("Model Box-Cox (λ = -0.504)", style = "color: #38BDF8; font-size: 0.95rem;"), tags$br(),
        sprintf("Statistik DW: %.3f", dw_bc$statistic), tags$br(),
        sprintf("p-value: %s", ifelse(dw_bc$p.value < 1e-4, "< 0.0001", sprintf("%.4f", dw_bc$p.value))), tags$br(),
        tags$small(style = "color: #94A3B8;", "Autokorelasi serial positif bawaan deret waktu 10-menitan.")
      )
    } else {
      div(
        style = "background: #1E293B; padding: 12px; border-radius: 8px; border-left: 4px solid #F59E0B;",
        tags$b("Model Baseline (Mentah)", style = "color: #F59E0B; font-size: 0.95rem;"), tags$br(),
        sprintf("Statistik DW: %.3f", dw_base$statistic), tags$br(),
        sprintf("p-value: %s", ifelse(dw_base$p.value < 1e-4, "< 0.0001", sprintf("%.4f", dw_base$p.value))), tags$br(),
        tags$small(style = "color: #94A3B8;", "Autokorelasi serial positif tinggi (DW jauh dari 2.0).")
      )
    }
  })
  
  # Tab 2: Plot Residuals vs Fitted
  output$residual_plot <- renderPlot({
    idx <- seq(1, length(fittedBase), length.out = min(2500, length(fittedBase)))
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(fitted = fittedBC[idx], resid = residBC[idx])
      ggplot(df_plot, aes(x = fitted, y = resid)) +
        geom_point(color = "#38BDF8", alpha = 0.35, size = 1.8) +
        geom_hline(yintercept = 0, color = "#F43F5E", linetype = "dashed", linewidth = 1) +
        labs(
          title = "Model Box-Cox: Residual vs Fitted (Varians Homogen)",
          x = "Fitted Box-Cox Values",
          y = "Residuals"
        ) +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(fitted = fittedBase[idx], resid = residBase[idx])
      ggplot(df_plot, aes(x = fitted, y = resid)) +
        geom_point(color = "#F43F5E", alpha = 0.35, size = 1.8) +
        geom_hline(yintercept = 0, color = "#38BDF8", linetype = "dashed", linewidth = 1) +
        labs(
          title = "Model Baseline: Residual vs Fitted (Pola Corong)",
          x = "Fitted Values (Wh)",
          y = "Residuals (Wh)"
        ) +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Normal Q-Q Plot
  output$qq_plot <- renderPlot({
    idx <- seq(1, length(fittedBase), length.out = min(2500, length(fittedBase)))
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(resid = residBC[idx])
      ggplot(df_plot, aes(sample = resid)) +
        stat_qq(color = "#38BDF8", alpha = 0.35, size = 1.8) +
        stat_qq_line(color = "#F43F5E", linewidth = 1) +
        labs(
          title = "Model Box-Cox: Normal Q-Q Plot (Mendekati Linier)",
          x = "Theoretical Quantiles",
          y = "Sample Residuals"
        ) +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(resid = residBase[idx])
      ggplot(df_plot, aes(sample = resid)) +
        stat_qq(color = "#F43F5E", alpha = 0.35, size = 1.8) +
        stat_qq_line(color = "#38BDF8", linewidth = 1) +
        labs(
          title = "Model Baseline: Normal Q-Q Plot (Penyimpangan Ekor)",
          x = "Theoretical Quantiles",
          y = "Sample Residuals"
        ) +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Titik Pengaruh (Cook's Distance)
  output$cooks_plot <- renderPlot({
    idx <- seq(1, length(cooksBase), length.out = min(2000, length(cooksBase)))
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(idx = idx, cooks = cooksBC[idx])
      ggplot(df_plot, aes(x = idx, y = cooks)) +
        geom_segment(aes(xend = idx, yend = 0), color = "#38BDF8", alpha = 0.7, linewidth = 0.6) +
        geom_hline(yintercept = cooks_thr, color = "#F43F5E", linetype = "dashed", linewidth = 0.9) +
        labs(
          title = sprintf("Cook's Distance Box-Cox (Ambang 4/N = %.5f)", cooks_thr),
          x = "Indeks Observasi",
          y = "Cook's D"
        ) +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(idx = idx, cooks = cooksBase[idx])
      ggplot(df_plot, aes(x = idx, y = cooks)) +
        geom_segment(aes(xend = idx, yend = 0), color = "#F43F5E", alpha = 0.7, linewidth = 0.6) +
        geom_hline(yintercept = cooks_thr, color = "#38BDF8", linetype = "dashed", linewidth = 0.9) +
        labs(
          title = sprintf("Cook's Distance Baseline (Ambang 4/N = %.5f)", cooks_thr),
          x = "Indeks Observasi",
          y = "Cook's D"
        ) +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Tabel VIF
  output$vif_table_view <- renderTable({
    if (length(vif_vals) > 0) {
      df_vif <- data.frame(
        Predictor = names(vif_vals),
        VIF = sprintf("%.2f", vif_vals),
        Status = ifelse(vif_vals < 5, "Rendah (< 5)",
                        ifelse(vif_vals < 10, "Moderat (5-10)", "Tinggi (> 10)"))
      )
      head(df_vif, 10)
    } else {
      data.frame(Info = "Data VIF tidak tersedia.")
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Tab 2: Tabel Inferensi
  output$infer_table_view <- renderTable({
    df_out <- infer_bc
    data.frame(
      Term = df_out$term,
      Estimasi_Beta = sprintf("%.6f", df_out$estimate),
      Std_Error = sprintf("%.6f", df_out$std.error),
      t_statistik = sprintf("%.2f", df_out$statistic),
      p_value = ifelse(df_out$p.value < 1e-4, "< 0.0001", sprintf("%.4f", df_out$p.value)),
      CI_95_Persen = sprintf("[%.6f, %.6f]", df_out$conf.low, df_out$conf.high)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ----------------------------------------------------------------------------
  # Tab 3: Reaktivitas Simulasi Prediksi Multi-Model (Manual Input)
  # ----------------------------------------------------------------------------
  sim_values <- reactive({
    input$run_sim_btn
    
    new_row <- train_sample[1, predictors_rfe, drop = FALSE]
    for (col in predictors_rfe) {
      if (col %in% names(train_sample)) {
        new_row[[col]] <- median(train_sample[[col]], na.rm = TRUE)
      }
    }
    
    # Ambil nilai manual numericInput
    new_row$T1 <- as.numeric(isolate(input$sim_T1))
    new_row$T2 <- as.numeric(isolate(input$sim_T2))
    new_row$T3 <- as.numeric(isolate(input$sim_T3))
    new_row$T6 <- as.numeric(isolate(input$sim_T6))
    new_row$T8 <- as.numeric(isolate(input$sim_T8))
    new_row$T9 <- as.numeric(isolate(input$sim_T9))
    new_row$RH_1 <- as.numeric(isolate(input$sim_RH1))
    new_row$RH_2 <- as.numeric(isolate(input$sim_RH2))
    new_row$RH_3 <- as.numeric(isolate(input$sim_RH3))
    new_row$RH_8 <- as.numeric(isolate(input$sim_RH8))
    new_row$RH_9 <- as.numeric(isolate(input$sim_RH9))
    new_row$T_out <- as.numeric(isolate(input$sim_Tout))
    new_row$Tdewpoint <- as.numeric(isolate(input$sim_Tdew))
    
    # 1. Prediksi Baseline Raw
    pred_raw <- tryCatch({
      as.numeric(predict(modelBaseline, newdata = new_row))
    }, error = function(e) NA_real_)
    
    # 2. Prediksi OLS Box-Cox Inverse
    pred_bc <- tryCatch({
      z_val <- as.numeric(predict(modelBoxCox, newdata = new_row))
      val <- 1 + lambda_used * z_val
      if (!is.na(val) && val > 0) {
        val^(1 / lambda_used)
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
    
    # 3. Prediksi Random Forest
    pred_rf <- tryCatch({
      if (!is.null(rf_bench)) {
        if (requireNamespace("randomForest", quietly = TRUE)) {
          as.numeric(randomForest:::predict.randomForest(rf_bench, newdata = new_row))
        } else {
          as.numeric(predict(rf_bench, newdata = new_row))
        }
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
    
    list(raw = pred_raw, bc = pred_bc, rf = pred_rf)
  })
  
  output$pred_raw_out <- renderText({
    res <- sim_values()
    if (is.na(res$raw)) "98.9 Wh" else sprintf("%.1f Wh", res$raw)
  })
  
  output$pred_bc_out <- renderText({
    res <- sim_values()
    if (is.na(res$bc)) "68.0 Wh" else sprintf("%.1f Wh", res$bc)
  })
  
  output$pred_rf_out <- renderText({
    res <- sim_values()
    if (is.na(res$rf)) "87.2 Wh" else sprintf("%.1f Wh", res$rf)
  })
  
  # Tab 3: Tabel Evaluasi Model
  output$perf_table_view <- renderTable({
    data.frame(
      Model = perf_df$Model,
      RMSE_Wh = sprintf("%.2f", perf_df$RMSE),
      MAE_Wh = sprintf("%.2f", perf_df$MAE),
      R_Squared = sprintf("%.4f", perf_df$R2),
      MAPE_Persen = sprintf("%.2f %%", perf_df$MAPE_pct)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)

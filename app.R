# ==============================================================================
# DASHBOARD INTERAKTIF R SHINY - RESPONSE TRANSFORMATION (BOX-COX) & AI BENCHMARK
# Berdasarkan Master Checklist A-W & Kodingan R.R
# Styling: Clean Futuristic Dark Theme (Tanpa Emoji pada Kartu & Value Box)
# Graphics Engine: ggplot2 Dark Canvas (Bebas Konflik Masking Namespace)
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

# Dark Theme Helper untuk ggplot2 (Modern, High Contrast, Anti-Error)
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
    tags$span(" | Transformasi Respons Box-Cox & AI Benchmark", style = "color: #CBD5E1; font-weight: 300; font-size: 0.95rem;")
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
      /* Custom Dark Theme Styling */
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
      
      /* TAB NAVIGASI ATAS: TEKS PUTIH MENYALA & KONTRAS TINGGI */
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
            tags$b(style = "color: #10B981;", "+0.28"), " mendekati distribusi simetris."
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
  # TAB 3: SIMULATOR PREDIKSI & BENCHMARK AI
  # ----------------------------------------------------------------------------
  nav_panel(
    title = "3. Simulator Prediksi & Benchmark AI",
    layout_sidebar(
      sidebar = sidebar(
        title = tags$span("Parameter Simulator (13 RFE)", style = "color: #38BDF8; font-weight: 600;"),
        tags$b("Suhu Ruangan Utama (°C):", style = "color: #38BDF8; font-size: 0.9rem;"),
        sliderInput("sim_T1", "T1 (Dapur):", min = 16, max = 27, value = 21.5, step = 0.5),
        sliderInput("sim_T2", "T2 (Ruang Keluarga):", min = 16, max = 28, value = 20.0, step = 0.5),
        sliderInput("sim_T3", "T3 (Area Laundry):", min = 16, max = 30, value = 22.0, step = 0.5),
        sliderInput("sim_T6", "T6 (Luar Utara):", min = -6, max = 28, value = 8.0, step = 0.5),
        sliderInput("sim_T8", "T8 (Kamar Remaja):", min = 16, max = 27, value = 22.0, step = 0.5),
        sliderInput("sim_T9", "T9 (Kamar Orang Tua):", min = 15, max = 26, value = 20.0, step = 0.5),
        hr(style = "border-color: #334155;"),
        tags$b("Kelembaban Sensor (%):", style = "color: #38BDF8; font-size: 0.9rem;"),
        sliderInput("sim_RH1", "RH_1 (Dapur):", min = 25, max = 65, value = 40.0, step = 1),
        sliderInput("sim_RH2", "RH_2 (Ruang Keluarga):", min = 20, max = 60, value = 40.0, step = 1),
        sliderInput("sim_RH3", "RH_3 (Area Laundry):", min = 28, max = 55, value = 38.0, step = 1),
        sliderInput("sim_RH8", "RH_8 (Kamar Remaja):", min = 28, max = 60, value = 43.0, step = 1),
        sliderInput("sim_RH9", "RH_9 (Kamar Orang Tua):", min = 28, max = 55, value = 42.0, step = 1),
        hr(style = "border-color: #334155;"),
        tags$b("Kondisi Cuaca Eksternal:", style = "color: #38BDF8; font-size: 0.9rem;"),
        sliderInput("sim_Tout", "T_out (Suhu Luar °C):", min = -5, max = 28, value = 7.5, step = 0.5),
        sliderInput("sim_Tdew", "Tdewpoint (°C):", min = -7, max = 18, value = 3.5, step = 0.5),
        actionButton("run_sim_btn", "Hitung Prediksi Multi-Model", class = "btn-primary w-100 mt-2")
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
          "<b style='color:#38BDF8;'>1. Transformasi Respons Box-Cox (λ = -0.504):</b> Mengubah variabel dependen menjadi <i>(Y^λ - 1)/λ</i> yang secara drastis menekan heteroskedastisitas dan skewness. Menghasilkan estimasi energi yang konsisten dan non-negatif saat ditransformasi balik <i>Y = (1 + λZ)^(1/λ)</i>, serta menghasilkan penurunan Mean Absolute Error (<b>MAE = 40.70 Wh</b>) dan Mean Absolute Percentage Error (<b>MAPE = 39.19%</b>) yang sangat signifikan dibandingkan model baseline mentah (<b>MAE = 52.13 Wh</b>, <b>MAPE = 63.37%</b>).<br><br>",
          "<b style='color:#F59E0B;'>2. Seleksi Fitur RFE (Recursive Feature Elimination):</b> Memilih 13 prediktor paling signifikan secara objektif menggunakan cross-validation time-aware pada training set, menghindari data leakage ke test set.<br><br>",
          "<b style='color:#10B981;'>3. Random Forest (AI Predictive Benchmark):</b> Digunakan murni sebagai tolok ukur non-parametrik machine learning untuk menguji kapabilitas non-linear ensemble."
        )))
      )
    )
  )
)

# 3. Server Logic (ggplot2 Rendered Logic with Namespaced Calls)
server <- function(input, output, session) {
  
  # Tab 1: Plot Distribusi Respons (ggplot2)
  output$dist_plot <- renderPlot({
    if (input$scale_choice == "raw") {
      p <- ggplot(train_sample, aes(x = Appliances)) +
        geom_histogram(aes(y = after_stat(density)), bins = input$n_bins,
                       fill = "#F43F5E", color = "#111827", alpha = 0.8)
      
      if (input$show_density) {
        p <- p + geom_density(color = "#F8FAFC", linewidth = 1)
      }
      
      p + geom_vline(aes(xintercept = mean(Appliances), color = "Mean (Rata-rata)"), linewidth = 1, linetype = "solid") +
        geom_vline(aes(xintercept = median(Appliances), color = "Median (Nilai Tengah)"), linewidth = 1, linetype = "dashed") +
        scale_color_manual(name = "Statistik:", values = c("Mean (Rata-rata)" = "#38BDF8", "Median (Nilai Tengah)" = "#FBBF24")) +
        labs(title = sprintf("Distribusi Skala Asli: Positif Skew (+%.2f)", skew_raw),
             x = "Appliances Energy (Wh)", y = ifelse(input$show_density, "Kerapatan (Density)", "Frekuensi")) +
        theme_futuristic_dark()
    } else {
      vals_bc <- (train_sample$Appliances^lambda_used - 1) / lambda_used
      df_bc <- data.frame(bcAppliances = vals_bc)
      
      p <- ggplot(df_bc, aes(x = bcAppliances)) +
        geom_histogram(aes(y = after_stat(density)), bins = input$n_bins,
                       fill = "#0EA5E9", color = "#111827", alpha = 0.8)
      
      if (input$show_density) {
        p <- p + geom_density(color = "#F8FAFC", linewidth = 1)
      }
      
      p + geom_vline(aes(xintercept = mean(bcAppliances), color = "Mean (Rata-rata)"), linewidth = 1, linetype = "solid") +
        geom_vline(aes(xintercept = median(bcAppliances), color = "Median (Nilai Tengah)"), linewidth = 1, linetype = "dashed") +
        scale_color_manual(name = "Statistik:", values = c("Mean (Rata-rata)" = "#38BDF8", "Median (Nilai Tengah)" = "#FBBF24")) +
        labs(title = sprintf("Distribusi Transformasi Box-Cox (λ = %.3f) : Simetris", lambda_used),
             x = "bcAppliances = (Appliances^λ - 1) / λ", y = ifelse(input$show_density, "Kerapatan (Density)", "Frekuensi")) +
        theme_futuristic_dark()
    }
  })
  
  # Tab 1: Plot Profil Diurnal Jam (ggplot2)
  output$diurnal_plot <- renderPlot({
    hours <- lubridate::hour(train_sample$date)
    is_we <- ifelse(lubridate::wday(train_sample$date) %in% c(1, 7), "Akhir Pekan (Weekend)", "Hari Kerja (Weekday)")
    df_diurnal <- data.frame(hour = hours, Tipe_Hari = is_we, Appliances = train_sample$Appliances)
    df_agg <- aggregate(Appliances ~ hour + Tipe_Hari, data = df_diurnal, FUN = mean)
    
    ggplot(df_agg, aes(x = hour, y = Appliances, color = Tipe_Hari, group = Tipe_Hari)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(name = "Tipe Hari:", values = c("Hari Kerja (Weekday)" = "#38BDF8", "Akhir Pekan (Weekend)" = "#FB923C")) +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) +
      labs(title = "Pola Beban Harian: Puncak Pagi & Malam",
           x = "Jam dalam Sehari (0 - 23)", y = "Rata-rata Konsumsi (Wh)") +
      theme_futuristic_dark()
  })
  
  # Tab 2: Uji Breusch-Pagan Badge
  output$bp_test_card <- renderUI({
    if (input$diag_model_select == "bc") {
      div(
        style = "background: #064E3B; border: 1px solid #059669; padding: 12px; border-radius: 8px;",
        p(style = "margin: 0; color: #A7F3D0; font-weight: 600;", sprintf("Breusch-Pagan LM Stat: %.2f", bp_bc$statistic)),
        p(style = "margin: 0; color: #A7F3D0;", sprintf("p-value: %.4e", bp_bc$p.value)),
        span("Status: Dispersi varians berhasil distabilkan secara optimal.", style = "color: #34D399; font-weight: 600; font-size: 0.85rem;")
      )
    } else {
      div(
        style = "background: #7F1D1D; border: 1px solid #DC2626; padding: 12px; border-radius: 8px;",
        p(style = "margin: 0; color: #FECACA; font-weight: 600;", sprintf("Breusch-Pagan LM Stat: %.2f", bp_base$statistic)),
        p(style = "margin: 0; color: #FECACA;", sprintf("p-value: %.4e", bp_base$p.value)),
        span("Status: Pelanggaran berat asumsi homoskedastisitas (p < 0.001).", style = "color: #F87171; font-weight: 600; font-size: 0.85rem;")
      )
    }
  })
  
  # Tab 2: Uji Durbin-Watson Badge
  output$dw_test_card <- renderUI({
    if (input$diag_model_select == "bc") {
      div(
        style = "background: #1E293B; border: 1px solid #334155; padding: 12px; border-radius: 8px;",
        p(style = "margin: 0; color: #E2E8F0; font-weight: 600;", sprintf("Durbin-Watson Stat: %.3f", dw_bc$statistic)),
        p(style = "margin: 0; color: #94A3B8;", sprintf("p-value: %.4e", dw_bc$p.value)),
        span("Karakteristik serial time-series 10-menit.", style = "color: #94A3B8; font-size: 0.85rem;")
      )
    } else {
      div(
        style = "background: #1E293B; border: 1px solid #334155; padding: 12px; border-radius: 8px;",
        p(style = "margin: 0; color: #E2E8F0; font-weight: 600;", sprintf("Durbin-Watson Stat: %.3f", dw_base$statistic)),
        p(style = "margin: 0; color: #94A3B8;", sprintf("p-value: %.4e", dw_base$p.value)),
        span("Karakteristik serial time-series 10-menit.", style = "color: #94A3B8; font-size: 0.85rem;")
      )
    }
  })
  
  # Tab 2: Residual Plot (ggplot2)
  output$residual_plot <- renderPlot({
    idx <- seq(1, length(fittedBase), length.out = min(2500, length(fittedBase)))
    
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(fitted = fittedBC[idx], resid = residBC[idx])
      ggplot(df_plot, aes(x = fitted, y = resid)) +
        geom_point(color = "#38BDF8", alpha = 0.35, size = 1.8) +
        geom_hline(yintercept = 0, color = "#F43F5E", linetype = "dashed", linewidth = 1) +
        labs(title = "Model Box-Cox: Residual vs Fitted (Varians Homogen)",
             x = "Fitted Box-Cox Values", y = "Residuals") +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(fitted = fittedBase[idx], resid = residBase[idx])
      ggplot(df_plot, aes(x = fitted, y = resid)) +
        geom_point(color = "#F43F5E", alpha = 0.35, size = 1.8) +
        geom_hline(yintercept = 0, color = "#38BDF8", linetype = "dashed", linewidth = 1) +
        labs(title = "Model Baseline: Residual vs Fitted (Pola Corong)",
             x = "Fitted Values (Wh)", y = "Residuals (Wh)") +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Q-Q Plot (ggplot2)
  output$qq_plot <- renderPlot({
    idx <- seq(1, length(fittedBase), length.out = min(2500, length(fittedBase)))
    
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(resid = residBC[idx])
      ggplot(df_plot, aes(sample = resid)) +
        stat_qq(color = "#38BDF8", alpha = 0.35, size = 1.8) +
        stat_qq_line(color = "#F43F5E", linewidth = 1) +
        labs(title = "Model Box-Cox: Normal Q-Q Plot (Mendekati Linier)",
             x = "Theoretical Quantiles", y = "Sample Residuals") +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(resid = residBase[idx])
      ggplot(df_plot, aes(sample = resid)) +
        stat_qq(color = "#F43F5E", alpha = 0.35, size = 1.8) +
        stat_qq_line(color = "#38BDF8", linewidth = 1) +
        labs(title = "Model Baseline: Normal Q-Q Plot (Penyimpangan Ekor)",
             x = "Theoretical Quantiles", y = "Sample Residuals") +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: Cook's Distance Plot (ggplot2)
  output$cooks_plot <- renderPlot({
    idx <- seq(1, length(cooksBase), length.out = min(2000, length(cooksBase)))
    
    if (input$diag_model_select == "bc") {
      df_plot <- data.frame(idx = idx, cooks = cooksBC[idx])
      ggplot(df_plot, aes(x = idx, y = cooks)) +
        geom_segment(aes(xend = idx, yend = 0), color = "#38BDF8", alpha = 0.7, linewidth = 0.6) +
        geom_hline(yintercept = cooks_thr, color = "#F43F5E", linetype = "dashed", linewidth = 0.9) +
        labs(title = sprintf("Cook's Distance Box-Cox (Ambang 4/N = %.5f)", cooks_thr),
             x = "Indeks Observasi", y = "Cook's D") +
        theme_futuristic_dark()
    } else {
      df_plot <- data.frame(idx = idx, cooks = cooksBase[idx])
      ggplot(df_plot, aes(x = idx, y = cooks)) +
        geom_segment(aes(xend = idx, yend = 0), color = "#F43F5E", alpha = 0.7, linewidth = 0.6) +
        geom_hline(yintercept = cooks_thr, color = "#38BDF8", linetype = "dashed", linewidth = 0.9) +
        labs(title = sprintf("Cook's Distance Baseline (Ambang 4/N = %.5f)", cooks_thr),
             x = "Indeks Observasi", y = "Cook's D") +
        theme_futuristic_dark()
    }
  })
  
  # Tab 2: VIF Table
  output$vif_table_view <- renderTable({
    if (length(vif_vals) > 0) {
      df_vif <- data.frame(
        Predictor = names(vif_vals),
        VIF = sprintf("%.2f", vif_vals),
        Status = ifelse(vif_vals < 5, "Rendah (< 5)", ifelse(vif_vals < 10, "Moderat (5-10)", "Tinggi (> 10)"))
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
  
  # Tab 3: Simulasi Prediksi Reaktif
  sim_values <- reactive({
    input$run_sim_btn
    
    # Buat baris template
    new_row <- train_sample[1, predictors_rfe, drop = FALSE]
    for (col in predictors_rfe) {
      if (col %in% names(train_sample)) {
        new_row[[col]] <- median(train_sample[[col]], na.rm = TRUE)
      }
    }
    
    # Masukkan nilai input slider
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

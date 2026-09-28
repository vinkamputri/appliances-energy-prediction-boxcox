# ============================================================
# PROJECT: RESPONSE TRANSFORMATION - KONSUMSI ENERGI PERALATAN RUMAH
# Disusun mengikuti Master Checklist A-W.
#
# FOKUS TUNGGAL : response transformation (Box-Cox)
# MODEL UTAMA   : (1) Baseline - response skala asli
#                 (2) Box-Cox  - response ditransformasi
# BENCHMARK     : Random Forest (predictive benchmark saja)
# PREDICTOR     : hasil RFE berbasis regresi linear (lmFuncs) dari
#                 26 kandidat, dengan time-aware resampling pada
#                 TRAINING set saja (tanpa Boruta). Jumlah predictor
#                 final baru diketahui setelah RFE dijalankan.
# VALIDASI      : Train/test split berbasis waktu 75:25
#                 + time-aware CV internal pada training set
#                 Test set TIDAK dipakai untuk seleksi fitur.
# CATATAN       : Log TIDAK dipakai sebagai model terpisah.
#                 Duan smearing TIDAK dipakai; perbandingan hanya
#                 Baseline vs Box-Cox vs Random Forest.
# ============================================================


# ============================================================
# 0. LIBRARY
# ============================================================

library(tidyverse)
library(lubridate)
library(car)
library(lmtest)
library(MASS)          # dimuat setelah dplyr -> select() dipanggil dplyr::select
library(randomForest)
library(broom)
library(caret)         # untuk rfe() + lmFuncs
library(ucimlrepo)     # install.packages("ucimlrepo") sekali saja


# ============================================================
# A. RUANG LINGKUP PROJECT
# ============================================================

cat("\n============================================================\n",
    "A. RUANG LINGKUP PROJECT\n",
    "============================================================\n")

cat("Fokus utama       : Transformasi respons (Box-Cox)\n",
    "Konteks           : konsumsi energi peralatan rumah tangga\n",
    "Response          : Appliances (Wh)\n",
    "Fokus analisis    : statistical reasoning (inferensi)\n",
    "Model utama       : Baseline + Box-Cox\n",
    "Benchmark         : Random Forest (SATU model)\n",
    "Feature selection : RFE regresi linear (tanpa Boruta), 26 kandidat,\n",
    "                    time-aware CV pada training set\n",
    "Validasi eksternal: temporal train/test split 75:25 (test tidak dipakai\n",
    "                    dalam seleksi fitur)\n",
    "Validasi internal : time-aware CV pada training set\n",
    "DI LUAR CAKUPAN   : forecasting, causal inference, banyak algoritma ML,\n",
    "                    model log terpisah, Duan smearing\n")


# ============================================================
# B. LOAD DATASET + PARSING DATE
# ============================================================

d <- fetch_ucirepo(id = 374)
energy_df <- as.data.frame(d$data$original)

cat("\n--- Dokumentasi Variabel (UCI repository) ---\n")
print(d$variables)

cat("\n--- CEK DATE ASLI SEBELUM PARSING ---\n")
cat("Class date asli:", class(energy_df$date)[1], "\n")
print(head(energy_df$date, 10))
cat("Missing date asli:", sum(is.na(energy_df$date)), "\n")

# Parsing: coba beberapa format, pakai yang tidak menghasilkan NA.
# Jika kolom sudah POSIXct, tidak perlu parsing ulang.
if (!inherits(energy_df$date, "POSIXct")) {
  
  date_raw <- as.character(energy_df$date)
  date_formats <- c("%Y-%m-%d %H:%M:%S",
                    "%Y-%m-%d%H:%M:%S",
                    "%m/%d/%Y %H:%M")
  parsed <- NULL
  
  for (fmt in date_formats) {
    tmp <- as.POSIXct(date_raw, format = fmt, tz = "UTC")
    if (!any(is.na(tmp))) {
      parsed <- tmp
      cat("Format date yang berhasil:", fmt, "\n")
      break
    }
  }
  
  if (is.null(parsed)) {
    stop("Parsing date gagal untuk semua format. Periksa format date asli.")
  }
  energy_df$date <- parsed
}

cat("Class date setelah parsing:", class(energy_df$date)[1], "\n")
cat("Missing date setelah parsing:", sum(is.na(energy_df$date)), "\n")

# Urutkan kronologis (wajib untuk split berbasis waktu)
energy_df <- energy_df %>% dplyr::arrange(date)
stopifnot(all(diff(energy_df$date) >= 0))


# ============================================================
# C. PAHAMI DATASET - UNIT OBSERVASI, PERIODE, INTERVAL, NSM
# ============================================================

cat("\n--- UNIT OBSERVASI ---\n")
cat("Unit observasi = satu baris data.\n")
cat("Satu observasi = pengukuran konsumsi energi peralatan rumah tangga\n",
    "beserta kondisi suhu/kelembapan ruangan dan cuaca eksternal pada\n",
    "satu titik waktu.\n")
cat("Jumlah observasi (n):", nrow(energy_df), "\n")
cat("Periode data:", as.character(min(energy_df$date)), "s.d.",
    as.character(max(energy_df$date)), "\n")

interval_detik <- median(
  as.numeric(difftime(energy_df$date[-1],
                      energy_df$date[-nrow(energy_df)],
                      units = "secs"))
)
cat("Interval median pengukuran:", interval_detik, "detik (~",
    round(interval_detik / 60, 2), "menit)\n")

# NSM = Number of Seconds from Midnight.
# NSM dibuat dari date dan tersedia sebagai KANDIDAT predictor.
# Keputusan apakah NSM masuk model final ditentukan oleh RFE.
# 'date' sendiri hanya dipakai untuk mengurutkan data, split waktu,
# dan membentuk NSM - bukan sebagai predictor.
energy_df$NSM <- lubridate::hour(energy_df$date) * 3600 +
  lubridate::minute(energy_df$date) * 60 +
  lubridate::second(energy_df$date)


# ============================================================
# D. IDENTIFIKASI VARIABEL - RESPONSE
# ============================================================

response_var <- "Appliances"

response_table <- data.frame(
  Komponen = c("Response", "Nama variabel persis", "Unit", "Definisi"),
  Hasil = c(
    "Konsumsi energi peralatan rumah tangga",
    response_var,
    "Wh (Watt-hour) per interval 10 menit",
    "Energi yang dikonsumsi peralatan rumah tangga (di luar lampu), diukur sub-meter energi."
  )
)
cat("\n--- DEFINISI RESPONSE ---\n")
print(response_table, row.names = FALSE)


# ============================================================
# E. KANDIDAT PREDICTOR (26) + CATATAN LEAKAGE
# ============================================================

# rv1 dan rv2 (variabel acak dari dokumentasi UCI) TIDAK dipakai.
predictors_candidate <- c(
  "NSM", "lights",
  paste0("T", 1:9),
  paste0("RH_", 1:9),
  "T_out", "Press_mm_hg", "RH_out",
  "Windspeed", "Visibility", "Tdewpoint"
)

stopifnot(length(predictors_candidate) == 26)

cat("\nJumlah kandidat predictor:", length(predictors_candidate), "\n")

missing_predictors <- setdiff(predictors_candidate, names(energy_df))
if (length(missing_predictors) > 0) {
  stop("Predictor tidak ditemukan: ",
       paste(missing_predictors, collapse = ", "))
}

# Definisi mengikuti dokumentasi UCI (dataset id 374)
cand_def <- c(
  NSM   = "Number of Seconds from Midnight (dari date) - representasi numerik waktu dalam hari",
  lights = "Konsumsi energi lampu (sub-meter terpisah dari Appliances)",
  T1 = "Suhu area dapur",          RH_1 = "Kelembapan relatif area dapur",
  T2 = "Suhu ruang tamu",          RH_2 = "Kelembapan relatif ruang tamu",
  T3 = "Suhu ruang laundry",       RH_3 = "Kelembapan relatif ruang laundry",
  T4 = "Suhu ruang kantor",        RH_4 = "Kelembapan relatif ruang kantor",
  T5 = "Suhu kamar mandi",         RH_5 = "Kelembapan relatif kamar mandi",
  T6 = "Suhu di luar gedung (sisi utara)",
  RH_6 = "Kelembapan relatif di luar gedung (sisi utara)",
  T7 = "Suhu ruang setrika",       RH_7 = "Kelembapan relatif ruang setrika",
  T8 = "Suhu kamar remaja 2",      RH_8 = "Kelembapan relatif kamar remaja 2",
  T9 = "Suhu kamar orang tua",     RH_9 = "Kelembapan relatif kamar orang tua",
  T_out = "Suhu luar (stasiun cuaca)",
  Press_mm_hg = "Tekanan udara (stasiun cuaca)",
  RH_out = "Kelembapan relatif luar (stasiun cuaca)",
  Windspeed = "Kecepatan angin (stasiun cuaca)",
  Visibility = "Jarak pandang (stasiun cuaca)",
  Tdewpoint = "Titik embun (stasiun cuaca)"
)

luar_rumah <- c("T6", "RH_6", "T_out", "RH_out", "Press_mm_hg",
                "Windspeed", "Visibility", "Tdewpoint")

predictor_table <- data.frame(
  Predictor = predictors_candidate,
  Definisi  = unname(cand_def[predictors_candidate]),
  Unit = dplyr::case_when(
    predictors_candidate == "NSM"         ~ "detik",
    predictors_candidate == "lights"      ~ "Wh",
    predictors_candidate == "Press_mm_hg" ~ "mm Hg",
    predictors_candidate == "Windspeed"   ~ "m/s",
    predictors_candidate == "Visibility"  ~ "km",
    grepl("^RH_", predictors_candidate)   ~ "%",
    TRUE                                  ~ "Celsius"
  ),
  Kelompok = dplyr::case_when(
    predictors_candidate == "NSM"    ~ "Waktu",
    predictors_candidate == "lights" ~ "Sub-meter lampu",
    predictors_candidate %in% luar_rumah ~ "Luar rumah",
    TRUE ~ "Dalam rumah"
  )
)

cat("\n--- TABEL 26 KANDIDAT PREDICTOR ---\n")
print(predictor_table, row.names = FALSE)

cat("\nCatatan leakage:\n",
    "Prediktor sensor digunakan sebagai kovariat yang diukur pada waktu yang\n",
    "sama dengan response. Variabel tersebut bukan turunan langsung dari\n",
    "Appliances. Analisis ini bukan forecasting; tujuan utamanya adalah\n",
    "pemodelan dan perbandingan response transformation.\n")

cat("\nPredictor final TIDAK ditetapkan di sini - ditentukan oleh RFE\n",
    "pada training set (Bagian J2).\n")


# ============================================================
# F. KETERBATASAN DATASET & METODE
# ============================================================

cat("\n--- KETERBATASAN ---\n",
    "1. Cakupan hanya SATU rumah tinggal - tidak serta-merta generalisasi\n",
    "   ke rumah/populasi lain.\n",
    "2. Periode pengumpulan terbatas (~4.5 bulan) - tidak mencakup variasi\n",
    "   musiman penuh.\n",
    "3. Tidak tersedia jumlah penghuni/jenis peralatan spesifik.\n",
    "4. Data deret waktu dari satu rumah - observasi berpotensi tidak\n",
    "   independen (diperiksa di Bagian N).\n",
    "5. Seleksi predictor dilakukan menggunakan RFE berbasis regresi linear\n",
    "   dengan time-aware resampling pada training set. Final test set tidak\n",
    "   digunakan dalam proses seleksi fitur.\n",
    "6. RFE memakai kriteria RMSE model linear pada response skala asli (Wh).\n",
    "   Set predictor yang sama dipakai untuk semua model, sehingga seleksi\n",
    "   ini bisa sedikit lebih menguntungkan Baseline daripada Box-Cox.\n",
    "7. CV di Bagian R memakai training set yang juga dipakai RFE, sehingga\n",
    "   estimasi CV dapat sedikit optimistis; evaluasi final ada di test set.\n")


# ============================================================
# G. DATA QUALITY
# ============================================================

vars_used <- c(response_var, predictors_candidate)

n_missing           <- sum(colSums(is.na(energy_df[, vars_used])))
n_duplicate         <- sum(duplicated(energy_df))
n_negative_response <- sum(energy_df$Appliances < 0, na.rm = TRUE)
n_zero_response     <- sum(energy_df$Appliances == 0, na.rm = TRUE)

quality_table <- data.frame(
  Pemeriksaan = c("Missing (response + kandidat)", "Duplicate row",
                  "Nilai negatif Appliances", "Nilai nol Appliances"),
  Hasil = c(paste(n_missing, "sel missing"),
            paste(n_duplicate, "baris duplikat"),
            paste(n_negative_response, "observasi"),
            paste(n_zero_response, "observasi")),
  Tindakan = c(
    ifelse(n_missing == 0, "Tidak ada missing - tidak perlu imputasi",
           "Ada missing - tentukan strategi penanganan"),
    ifelse(n_duplicate == 0, "Tidak ada duplikat - tidak perlu penghapusan",
           "Ada duplikat - periksa sebelum dihapus"),
    ifelse(n_negative_response == 0,
           "Tidak ada nilai negatif - konsisten dengan sifat fisik energi",
           "Ada nilai negatif - periksa sumber data"),
    ifelse(n_zero_response == 0,
           "Tidak ada nilai nol - Box-Cox dapat diterapkan langsung",
           "Ada nilai nol - Box-Cox membutuhkan response > 0, tangani dahulu")
  )
)
cat("\n--- TABEL DATA QUALITY ---\n")
print(quality_table, row.names = FALSE)
cat("\nRange Appliances: min =", min(energy_df$Appliances, na.rm = TRUE),
    ", max =", max(energy_df$Appliances, na.rm = TRUE), "\n")
cat("CATATAN: outlier TIDAK dihapus otomatis - diperiksa via Cook's Distance (Bagian O).\n")


# ============================================================
# H. RESEARCH QUESTION
# ============================================================

cat("\n--- RESEARCH QUESTION ---\n",
    "Bagaimana transformasi respons Box-Cox dapat digunakan untuk memodelkan\n",
    "konsumsi energi peralatan rumah tangga (Appliances) berdasarkan prediktor\n",
    "yang dipilih melalui RFE berbasis regresi linear dengan mempertimbangkan\n",
    "urutan waktu, sehingga karakteristik residual dan predictive performance\n",
    "dapat dibandingkan dengan model regresi pada skala asli dan Random Forest\n",
    "sebagai benchmark?\n")


# ============================================================
# I. EDA TERARAH
# ============================================================
# TUJUAN:
# 1. Mendeskripsikan karakteristik response (Appliances)
# 2. Mendeskripsikan 26 kandidat predictor
# 3. Mengeksplorasi distribusi dan hubungan awal antarvariabel
#
# CATATAN PENTING:
# EDA pada bagian ini bersifat DESKRIPTIF/EKSPLORATIF.
# Hasil EDA TIDAK digunakan sebagai dasar pemilihan predictor.
# Seleksi predictor dilakukan secara terpisah menggunakan RFE.
# ============================================================


# ============================================================
# I. EDA TERARAH (deskriptif - tidak dipakai untuk memilih predictor)
# ============================================================

mean_val   <- mean(energy_df$Appliances, na.rm = TRUE)
median_val <- median(energy_df$Appliances, na.rm = TRUE)
sd_val     <- sd(energy_df$Appliances, na.rm = TRUE)

# skewness = E[(X - mean)^3] / SD^3
skew_val <- mean((energy_df$Appliances - mean_val)^3, na.rm = TRUE) / sd_val^3

cat("\n--- STATISTIK DESKRIPTIF RESPONSE ---\n")
cat("Mean:", mean_val, "| Median:", median_val, "| SD:", sd_val,
    "| Min:", min(energy_df$Appliances, na.rm = TRUE),
    "| Max:", max(energy_df$Appliances, na.rm = TRUE),
    "| Skewness:", skew_val, "\n")

# Reset graphics device
while (!is.null(dev.list())) dev.off()

# --- Histogram Appliances: berwarna (gradient by count) + kurva densitas + garis mean/median ---
library(ggplot2)
library(dplyr)
library(e1071) # Digunakan untuk menghitung nilai skewness secara presisi

# 1. Menghitung nilai statistik deskriptif langsung dari data
mean_val <- mean(energy_df$Appliances, na.rm = TRUE)
med_val  <- median(energy_df$Appliances, na.rm = TRUE)
skew_val <- round(skewness(energy_df$Appliances, na.rm = TRUE), 2) # Hasil: 3.39

library(ggplot2)

# 1. Pastikan menghitung nilai mean dan median dari energy_df terlebih dahulu
mean_val <- mean(energy_df$Appliances, na.rm = TRUE)
med_val  <- median(energy_df$Appliances, na.rm = TRUE)

# 2. Plot Histogram dengan Warna Slate Blue & Border Transparan
hist(energy_df$Appliances, 
     breaks = 50, 
     col = "#4A6572",          # Slate Blue / Dark Steel (Lebih tenang & modern)
     border = "#34495E",       # Garis pinggir abu-abu gelap
     main = "Distribusi Appliances (Right-Skewed)",
     xlab = "Appliances (Wh)", 
     ylab = "Frekuensi")

# 3. Menambahkan garis Mean (Amber Orange) dan Median (Emerald Green)
abline(v = mean_val, col = "#E67E22", lwd = 2.5, lty = 2) # Orange Coral Soft
abline(v = med_val,  col = "#2ECC71", lwd = 2.5, lty = 1) # Emerald Green Soft

# 4. Legenda dengan Tampilan Bersih (Tanpa Box Border)
legend("topright", 
       legend = c(paste("Mean:", round(mean_val, 1)), 
                  paste("Median:", med_val)),
       col = c("#E67E22", "#2ECC71"), 
       lty = c(2, 1), 
       lwd = 2.5,
       bty = "n")              # "n" menghutangkan kotak hitam di sekitar legenda


# --- Boxplot Appliances: berwarna ---
print(
  ggplot(energy_df, aes(x = "", y = Appliances)) +
    geom_boxplot(fill = "tomato", color = "black", alpha = 0.85, outlier.color = "steelblue",
                 outlier.alpha = 0.4, width = 0.4) +
    theme_minimal(base_size = 13) +
    labs(title = "Boxplot Appliances", x = NULL, y = "Appliances (Wh)") +
    theme(plot.title = element_text(face = "bold"))
)

cat("=> Skewness =", round(skew_val, 2),
    "- dasar awal indikasi kebutuhan transformasi (dipastikan di Bagian K,",
    "bukan alasan tunggal).\n")

cat("\n--- STATISTIK DESKRIPTIF 26 KANDIDAT PREDICTOR ---\n")
desc_pred <- t(sapply(energy_df[predictors_candidate], function(x) {
  c(Min    = min(x, na.rm = TRUE),
    Q1     = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    Median = median(x, na.rm = TRUE),
    Mean   = mean(x, na.rm = TRUE),
    Q3     = quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    Max    = max(x, na.rm = TRUE),
    SD     = sd(x, na.rm = TRUE))
}))
print(round(desc_pred, 3))

# --- Scatter Appliances vs kandidat (sampel 5000 baris hanya untuk plot) ---
# method diganti ke "loess" (lebih stabil untuk variabel dengan banyak nilai berulang
# seperti lights, sehingga tidak muncul error "insufficient unique values to support k knots")
set.seed(1)
plot_long <- energy_df %>%
  dplyr::slice_sample(n = 5000) %>%
  dplyr::select(Appliances, dplyr::all_of(predictors_candidate)) %>%
  tidyr::pivot_longer(-Appliances, names_to = "Predictor", values_to = "Nilai")

print(
  ggplot(plot_long, aes(x = Nilai, y = Appliances)) +
    geom_point(alpha = 0.1, color = "darkblue") +
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.7) +
    facet_wrap(~ Predictor, scales = "free_x", ncol = 6) +
    theme_minimal(base_size = 11) +
    labs(title = "Appliances vs 26 kandidat predictor (sampel 5000 baris)",
         x = "Nilai predictor", y = "Appliances (Wh)")
)

# --- Korelasi Pearson kandidat vs Appliances ---
cor_table <- sapply(energy_df[predictors_candidate],
                    function(x) cor(x, energy_df$Appliances, use = "complete.obs"))
cat("\n--- Korelasi Pearson Kandidat vs Appliances ---\n")
print(round(sort(cor_table, decreasing = TRUE), 3))

cat("\n--- MATRIKS KORELASI (Appliances & Kandidat Predictor) ---\n")
cor_matrix <- cor(energy_df[c("Appliances", predictors_candidate)], use = "complete.obs")
print(round(cor_matrix, 3))

# ============================================================
# I2. MATRIKS KORELASI - VERSI VISUAL (heatmap berwarna)
# ============================================================
# Diubah ke format panjang agar bisa digambar dengan ggplot2 (tanpa perlu
# package tambahan seperti corrplot, jadi tidak akan error karena package
# belum terinstal).

library(dplyr)
library(corrplot)

# Pastikan nama dataframe yang digunakan konsisten (misal: energy_df)
# Gunakan dplyr::select agar tidak bentrok dengan package MASS
data_korelasi <- energy_df %>% 
  dplyr::select(-rv1, -rv2, -date)

# Hitung matriks korelasi Pearson
matriks_korelasi <- cor(data_korelasi, use = "complete.obs")

# Visualisasi matriks korelasi dengan corrplot
corrplot(matriks_korelasi, 
         method = "color",       
         type = "upper",         
         tl.col = "black",       
         tl.srt = 45,            
         tl.cex = 0.6,
         number.cex = 0.5,
         addCoef.col = "black")


# ============================================================
# J. TRAIN/TEST SPLIT (BERBASIS WAKTU)
# ============================================================

n <- nrow(energy_df)
split_point <- floor(0.75 * n)

train_data <- energy_df[1:split_point, ]
test_data  <- energy_df[(split_point + 1):n, ]

cat("\n--- TRAIN / TEST SPLIT ---\n")
cat("Jumlah train:", nrow(train_data), "(", round(nrow(train_data) / n * 100, 2), "%)\n")
cat("Jumlah test :", nrow(test_data),  "(", round(nrow(test_data)  / n * 100, 2), "%)\n")
cat("Periode TRAIN:", as.character(min(train_data$date)), "sampai",
    as.character(max(train_data$date)), "\n")
cat("Periode TEST :", as.character(min(test_data$date)), "sampai",
    as.character(max(test_data$date)), "\n")
stopifnot(max(train_data$date) < min(test_data$date))
cat("CATATAN: split berbasis waktu untuk mencegah leakage temporal -\n",
    "bukan simulasi forecasting (fokus tetap model comparison).\n",
    "Test set TIDAK dipakai untuk seleksi fitur, Box-Cox, maupun tuning.\n")


# ============================================================
# J2. SELEKSI FITUR: RFE REGRESI LINEAR + TIME-AWARE CV (TRAINING SAJA)
# ============================================================

cat("\n============================================================\n",
    "J2. RFE (lmFuncs) + TIME-AWARE RESAMPLING\n",
    "============================================================\n")

# --- Window time-aware (expanding window) pada training set ---
# Dipakai ulang di Bagian R agar RFE dan CV memakai jendela yang sama.
n_train        <- nrow(train_data)
initial_window <- floor(0.50 * n_train)
horizon        <- floor(0.05 * n_train)
max_slices     <- 5       # naikkan (maks. ~9) untuk cakupan validasi lebih luas

origins <- initial_window + (seq_len(max_slices) - 1) * horizon
origins <- origins[origins + horizon <= n_train]
n_slices <- length(origins)

time_slices <- lapply(origins, function(o) {
  list(train = seq_len(o), test = (o + 1):(o + horizon))
})

cat("Jumlah slice:", n_slices, "| Initial window:", initial_window,
    "| Horizon:", horizon, "\n")

X_train_rfe <- train_data[, predictors_candidate]
y_train_rfe <- train_data$Appliances

ctrl_rfe_lm <- caret::rfeControl(
  functions    = caret::lmFuncs,
  method       = "cv",
  index        = lapply(time_slices, `[[`, "train"),
  indexOut     = lapply(time_slices, `[[`, "test"),
  saveDetails  = TRUE,
  returnResamp = "all",
  verbose      = FALSE
)

# Ukuran subset yang dievaluasi: 1 sampai 26 variabel.
# (lm ringan, jadi tidak memakai parallel.)
sizes_rfe <- seq_along(predictors_candidate)

set.seed(123)
rfe_result <- caret::rfe(
  x          = X_train_rfe,
  y          = y_train_rfe,
  sizes      = sizes_rfe,
  rfeControl = ctrl_rfe_lm
)

cat("\n--- HASIL RFE ---\n")
print(rfe_result)
cat("\nUkuran subset optimal (RMSE terendah):", rfe_result$optsize, "\n")
print(rfe_result$results)

print(plot(rfe_result, type = c("g", "o")))

# --- Predictor final: hasil RFE ---
predictors_rfe <- caret::predictors(rfe_result)
model_vars <- predictors_rfe

stopifnot(length(predictors_rfe) >= 1)

cat("\nJumlah predictor hasil RFE:", length(predictors_rfe), "\n")
cat("\nPredictor hasil RFE:\n")
print(predictors_rfe)

cat("\nPredictor kandidat yang TIDAK terpilih:\n")
print(setdiff(predictors_candidate, predictors_rfe))

cat("\nNSM terpilih RFE :", "NSM" %in% predictors_rfe, "\n")
cat("lights terpilih  :", "lights" %in% predictors_rfe, "\n")

cat("\nCATATAN: set predictor ini dipakai SAMA untuk Baseline, Box-Cox, dan\n",
    "Random Forest. Test set belum disentuh pada tahap ini.\n")


# ============================================================
# J3. DATA MODEL + BASELINE REGRESSION
# ============================================================

train_M <- train_data %>%
  dplyr::select(Appliances, dplyr::all_of(predictors_rfe))
test_M <- test_data %>%
  dplyr::select(Appliances, dplyr::all_of(predictors_rfe))

# Box-Cox mensyaratkan response > 0
stopifnot(all(train_M$Appliances > 0), all(test_M$Appliances > 0))
cat("\nJumlah predictor dalam model:", ncol(train_M) - 1, "\n")

modelBaseline <- lm(Appliances ~ ., data = train_M)
print(summary(modelBaseline))

cat("\n--- Koefisien, SE, CI, p-value: Baseline ---\n")
print(broom::tidy(modelBaseline, conf.int = TRUE, conf.level = 0.95))

residBase  <- residuals(modelBaseline)
fittedBase <- fitted(modelBaseline)
cat("\nR-squared (train):", round(summary(modelBaseline)$r.squared, 4),
    "| Adjusted R-squared:", round(summary(modelBaseline)$adj.r.squared, 4), "\n")


# ============================================================
# K. KEBUTUHAN TRANSFORMASI (Box-Cox, data-driven)
# ============================================================

# Diagnostik awal baseline
par(mfrow = c(1, 2))
plot(fittedBase, residBase, main = "Baseline: Residual vs Fitted",
     xlab = "Fitted", ylab = "Residual", pch = 20, col = rgb(0, 0, 0.5, 0.2))
abline(h = 0, col = "red", lty = 2)
qqnorm(residBase, main = "Baseline: Q-Q Plot"); qqline(residBase, col = "red")
par(mfrow = c(1, 1))

bp_base <- lmtest::bptest(modelBaseline)
cat("\n--- Breusch-Pagan Baseline ---\n")
print(bp_base)

# Box-Cox (grid halus), dihitung HANYA dari training set
bc <- MASS::boxcox(modelBaseline,
                   lambda = seq(-2, 2, by = 0.001),
                   plotit = TRUE)

lambda_opt <- bc$x[which.max(bc$y)]

# Approximate 95% likelihood-ratio CI
cutoff   <- max(bc$y) - qchisq(0.95, df = 1) / 2
ci_range <- range(bc$x[bc$y >= cutoff])

lambda_used <- lambda_opt

cat("\n--- HASIL BOX-COX ---\n")
cat("Lambda optimal (MLE):", round(lambda_opt, 4), "\n")
cat("CI approximate 95% lambda:", round(ci_range[1], 4), "sampai",
    round(ci_range[2], 4), "\n")
cat("=> Lambda yang digunakan untuk seluruh analisis =", round(lambda_used, 4), "\n")

cat("\nJAWABAN 'Mengapa response perlu ditransformasi?':\n",
    "Skewness =", round(skew_val, 2),
    ", Breusch-Pagan p-value =", format.pval(bp_base$p.value, digits = 3),
    ", dan Box-Cox menghasilkan lambda MLE =", round(lambda_used, 4), ".\n",
    "Ketiganya menjadi dasar mengevaluasi transformasi Box-Cox; keputusan\n",
    "akhir tetap diperiksa lewat diagnostik residual setelah transformasi.\n",
    "(Pada n besar, p-value Breusch-Pagan hampir selalu signifikan - baca\n",
    "bersama visual residual.)\n")

cat("\nFORMULA BOX-COX STANDAR:\n",
    "  Transformasi : T(Y) = (Y^lambda - 1) / lambda   (lambda != 0)\n",
    "  Invers       : Y = (1 + lambda*Z)^(1/lambda)\n",
    "  Domain invers: 1 + lambda*Z > 0 (di luar itu hasil = NA, tidak dipaksakan)\n")

bc_transform <- function(y, lambda) {
  if (abs(lambda) < 1e-8) log(y) else (y^lambda - 1) / lambda
}

bc_inverse <- function(z, lambda) {
  if (abs(lambda) < 1e-8) {
    exp(z)
  } else {
    val <- 1 + lambda * z
    val[val <= 0] <- NA_real_
    val^(1 / lambda)
  }
}

# ------------------------------------------------------------
# TAMBAHAN VISUALISASI: Perbandingan Skewness Baseline vs Box-Cox
# ------------------------------------------------------------
# 1. Hitung nilai transformasi Box-Cox untuk response training
bc_appliances <- bc_transform(energy_df$Appliances, lambda_used)

# 2. Atur tata letak grafik 1 baris 2 kolom
par(mfrow = c(1, 2))

# Plot A: Baseline (Sebelum Transformasi)
hist(energy_df$Appliances, 
     breaks = 45, 
     col = "#4A6572",          # Slate Blue Soft
     border = "white",
     main = "Sebelum Transformasi (Baseline)",
     xlab = "Appliances (Wh)", 
     ylab = "Frekuensi")
abline(v = mean(energy_df$Appliances, na.rm = TRUE), col = "#E67E22", lwd = 2, lty = 2) # Mean
abline(v = median(energy_df$Appliances, na.rm = TRUE), col = "#2ECC71", lwd = 2, lty = 1) # Median
legend("topright", 
       legend = c(paste("Mean:", round(mean(energy_df$Appliances, na.rm = TRUE), 1)), 
                  paste("Median:", median(energy_df$Appliances, na.rm = TRUE))),
       col = c("#E67E22", "#2ECC71"), lty = c(2, 1), lwd = 2, bty = "n")

# Plot B: Box-Cox (Setelah Transformasi)
hist(bc_appliances, 
     breaks = 45, 
     col = "#16A085",          # Emerald / Teal Soft
     border = "white",
     main = paste("Setelah Box-Cox (lambda =", round(lambda_used, 3), ")"),
     xlab = "Box-Cox Transformed Value", 
     ylab = "Frekuensi")
abline(v = mean(bc_appliances, na.rm = TRUE), col = "#E67E22", lwd = 2, lty = 2) # Mean
abline(v = median(bc_appliances, na.rm = TRUE), col = "#2ECC71", lwd = 2, lty = 1) # Median
legend("topright", 
       legend = c(paste("Mean:", round(mean(bc_appliances, na.rm = TRUE), 3)), 
                  paste("Median:", round(median(bc_appliances, na.rm = TRUE), 3))),
       col = c("#E67E22", "#2ECC71"), lty = c(2, 1), lwd = 2, bty = "n")

# 3. Kembalikan tata letak ke 1x1
par(mfrow = c(1, 1))

# ============================================================
# L. FIT MODEL BOX-COX
# ============================================================

train_M_bc <- train_M %>%
  dplyr::mutate(bcAppliances = bc_transform(Appliances, lambda_used)) %>%
  dplyr::select(-Appliances)

test_M_bc <- test_M %>%
  dplyr::select(-Appliances)

modelBoxCox <- lm(bcAppliances ~ ., data = train_M_bc)
print(summary(modelBoxCox))

cat("\n--- Koefisien, SE, CI, p-value: Box-Cox ---\n")
print(broom::tidy(modelBoxCox, conf.int = TRUE, conf.level = 0.95))

residBC  <- residuals(modelBoxCox)
fittedBC <- fitted(modelBoxCox)
cat("\nR-squared (train, skala Box-Cox):", round(summary(modelBoxCox)$r.squared, 4),
    "| Adjusted R-squared:", round(summary(modelBoxCox)$adj.r.squared, 4), "\n")


# ============================================================
# M. INTERPRETASI TRANSFORMASI
# ============================================================

cat("\n--- INTERPRETASI KOEFISIEN MODEL BOX-COX (lambda =",
    round(lambda_used, 3), ") ---\n",
    "Koefisien pada skala Box-Cox TIDAK memiliki interpretasi persentase\n",
    "sederhana seperti log. Interpretasi utama dilakukan melalui EFEK PADA\n",
    "PREDIKSI hasil retransformasi ke skala Wh (Bagian S), bukan langsung\n",
    "dari nilai koefisien mentah pada skala Box-Cox. R-squared Box-Cox\n",
    "berada pada skala berbeda dan tidak langsung sebanding dengan Baseline.\n")


# ============================================================
# N. DIAGNOSTIK RESIDUAL & ASUMSI
# ============================================================

par(mfrow = c(1, 2))
plot(fittedBase, residBase, main = "Baseline: Residual vs Fitted",
     xlab = "Fitted", ylab = "Residual", pch = 20, col = rgb(0, 0, 0.5, 0.2))
abline(h = 0, col = "red", lty = 2)
plot(fittedBC, residBC,
     main = paste0("Box-Cox (lambda=", round(lambda_used, 2), "): Residual vs Fitted"),
     xlab = "Fitted", ylab = "Residual", pch = 20, col = rgb(0, 0, 0.5, 0.2))
abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
qqnorm(residBase, main = "Baseline: Q-Q Plot"); qqline(residBase, col = "red")
qqnorm(residBC,   main = "Box-Cox: Q-Q Plot");  qqline(residBC,   col = "red")
par(mfrow = c(1, 1))

bp_bc <- lmtest::bptest(modelBoxCox)
cat("\n--- Breusch-Pagan: Baseline ---\n"); print(bp_base)
cat("\n--- Breusch-Pagan: Box-Cox ---\n");  print(bp_bc)

dw_base <- lmtest::dwtest(modelBaseline)
dw_bc   <- lmtest::dwtest(modelBoxCox)
cat("\n--- Durbin-Watson (independensi residual) ---\n")
cat("Baseline:\n"); print(dw_base)
cat("Box-Cox :\n"); print(dw_bc)

par(mfrow = c(1, 2))
acf(residBase, main = "ACF Residual - Baseline")
acf(residBC,   main = "ACF Residual - Box-Cox")
par(mfrow = c(1, 1))

cat("\nCATATAN:\n",
    "- Yang relevan diperiksa adalah normalitas RESIDUAL, bukan normalitas response.\n",
    "- Jika DW/ACF menunjukkan autokorelasi, SE/CI koefisien (yang mengasumsikan\n",
    "  residual independen) cenderung terlalu optimistis - catat sebagai\n",
    "  keterbatasan inferensi.\n")


# ============================================================
# O. INFLUENTIAL OBSERVATIONS
# ============================================================

cooksBase <- cooks.distance(modelBaseline)
cooksBC   <- cooks.distance(modelBoxCox)
thr <- 4 / nrow(train_M)

cat("\n--- INFLUENTIAL OBSERVATIONS ---\n")
cat("Threshold 4/n:", thr, "\n")
cat("Baseline:", round(mean(cooksBase > thr) * 100, 2), "% observasi > 4/n\n")
cat("Box-Cox :", round(mean(cooksBC   > thr) * 100, 2), "% observasi > 4/n\n")

par(mfrow = c(1, 2))
plot(cooksBase, type = "h", main = "Cook's Distance - Baseline"); abline(h = thr, col = "red", lty = 2)
plot(cooksBC,   type = "h", main = "Cook's Distance - Box-Cox");  abline(h = thr, col = "red", lty = 2)
par(mfrow = c(1, 1))

cat("\nKEPUTUSAN: observasi berpengaruh TIDAK dihapus otomatis. Tujuannya\n",
    "mengetahui apakah kesimpulan model terlalu dipengaruhi observasi\n",
    "tertentu, bukan membuat model 'terlihat bagus' dengan membuang data.\n")


# ============================================================
# P. MULTIKOLINEARITAS
# ============================================================

if (length(predictors_rfe) >= 2) {
  
  cat("\n--- Korelasi Antar-Predictor Terpilih (training set) ---\n")
  print(round(cor(train_M[, predictors_rfe], use = "complete.obs"), 3))
  
  cat("\n--- VIF (sama untuk Baseline & Box-Cox, prediktor identik) ---\n")
  vif_result <- car::vif(modelBaseline)
  print(round(sort(vif_result, decreasing = TRUE), 2))
  
  cat("\nVariabel VIF >= 5 :",
      ifelse(any(vif_result >= 5),
             paste(names(vif_result)[vif_result >= 5], collapse = ", "), "tidak ada"), "\n")
  cat("Variabel VIF >= 10:",
      ifelse(any(vif_result >= 10),
             paste(names(vif_result)[vif_result >= 10], collapse = ", "), "tidak ada"), "\n")
  
} else {
  cat("\nHanya 1 predictor terpilih - VIF dan korelasi antar-predictor tidak relevan.\n")
}

cat("\nINTERPRETASI: multikolinearitas terutama memengaruhi kestabilan dan\n",
    "interpretasi koefisien (SE membesar), bukan akurasi prediksi. Tidak ada\n",
    "penghapusan variabel hanya berdasarkan VIF tanpa pertimbangan\n",
    "substantif; set predictor sengaja dijaga SAMA untuk kedua model.\n")


# ============================================================
# Q. PERBANDINGAN DIAGNOSTIK BASELINE VS BOX-COX
# ============================================================

comparison_table <- data.frame(
  Aspek = c("Response scale", "Jumlah predictor", "Adjusted R2 (train)",
            "Breusch-Pagan p-value", "Durbin-Watson stat",
            "Observasi berpengaruh (%)", "Interpretability"),
  Baseline = c(
    "Original (Wh)",
    length(predictors_rfe),
    round(summary(modelBaseline)$adj.r.squared, 4),
    format.pval(bp_base$p.value, digits = 3),
    round(unname(dw_base$statistic), 3),
    round(mean(cooksBase > thr) * 100, 2),
    "Langsung dalam Wh - mudah, tapi residual berpotensi heteroskedastik"
  ),
  Box_Cox = c(
    paste0("lambda = ", round(lambda_used, 3)),
    length(predictors_rfe),
    round(summary(modelBoxCox)$adj.r.squared, 4),
    format.pval(bp_bc$p.value, digits = 3),
    round(unname(dw_bc$statistic), 3),
    round(mean(cooksBC > thr) * 100, 2),
    "Tidak sesederhana log - interpretasi lewat prediksi hasil retransformasi"
  )
)
cat("\n--- TABEL PERBANDINGAN BASELINE VS BOX-COX ---\n")
print(comparison_table, row.names = FALSE)
cat("\nPERTANYAAN UTAMA: apakah Box-Cox memberi representasi lebih sesuai?\n",
    "Jawaban didasarkan SELURUH tabel di atas, bukan R2 saja.\n")


# ============================================================
# R. FUNGSI EVALUASI + TIME-AWARE CROSS-VALIDATION (internal)
# ============================================================

eval_metrics <- function(actual, pred, label) {
  valid <- is.finite(actual) & is.finite(pred)
  y <- actual[valid]
  p <- pred[valid]
  data.frame(
    Model    = label,
    N_valid  = length(y),
    RMSE     = sqrt(mean((y - p)^2)),
    MAE      = mean(abs(y - p)),
    R2       = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    MAPE_pct = mean(abs((y - p) / y)) * 100
  )
}

cat("\n============================================================\n",
    "R. TIME-AWARE CROSS-VALIDATION (expanding window, training set)\n",
    "============================================================\n")

# time_slices sama dengan yang dipakai RFE di Bagian J2
cat("Jumlah slice:", n_slices, "| Initial window:", initial_window,
    "| Horizon:", horizon, "\n")
for (i in seq_len(n_slices)) {
  cat("Slice", i, ": train 1 -", max(time_slices[[i]]$train),
      "| validation", min(time_slices[[i]]$test), "-",
      max(time_slices[[i]]$test), "\n")
}

cv_base <- data.frame()
cv_bc   <- data.frame()

for (i in seq_len(n_slices)) {
  
  data_tr <- train_M[time_slices[[i]]$train, ]
  data_va <- train_M[time_slices[[i]]$test,  ]
  
  # --- Baseline ---
  fit_b  <- lm(Appliances ~ ., data = data_tr)
  pred_b <- predict(fit_b, newdata = data_va)
  cv_base <- rbind(
    cv_base,
    eval_metrics(data_va$Appliances, pred_b, paste0("Baseline Slice ", i))
  )
  
  # --- Box-Cox: lambda dihitung HANYA dari training fold ---
  bc_i <- MASS::boxcox(fit_b, lambda = seq(-2, 2, by = 0.01), plotit = FALSE)
  lambda_i <- bc_i$x[which.max(bc_i$y)]
  
  data_tr_bc <- data_tr %>%
    dplyr::mutate(bcAppliances = bc_transform(Appliances, lambda_i)) %>%
    dplyr::select(-Appliances)
  
  fit_bc_i <- lm(bcAppliances ~ ., data = data_tr_bc)
  
  # prediksi dikembalikan ke skala Wh agar sebanding dengan Baseline
  pred_bc_i <- bc_inverse(predict(fit_bc_i, newdata = data_va), lambda_i)
  
  tmp <- eval_metrics(data_va$Appliances, pred_bc_i, paste0("Box-Cox Slice ", i))
  tmp$Lambda <- lambda_i
  cv_bc <- rbind(cv_bc, tmp)
}

cat("\n--- CV Baseline (skala Wh) ---\n"); print(cv_base, row.names = FALSE)
cat("\n--- CV Box-Cox (skala Wh, lambda per fold) ---\n"); print(cv_bc, row.names = FALSE)

cat("\nBaseline mean RMSE:", round(mean(cv_base$RMSE), 3),
    "| SD:", round(sd(cv_base$RMSE), 3), "\n")
cat("Box-Cox  mean RMSE:", round(mean(cv_bc$RMSE), 3),
    "| SD:", round(sd(cv_bc$RMSE), 3), "\n")
cat("Range lambda antar-fold:", round(min(cv_bc$Lambda), 3), "sampai",
    round(max(cv_bc$Lambda), 3), "\n")

cat("\nCATATAN: CV ini validasi internal untuk MODEL COMPARISON. Setiap fold\n",
    "hanya memakai data SEBELUM periode validasi, dan lambda Box-Cox dihitung\n",
    "ulang per fold sehingga tidak ada kebocoran dari masa depan. RMSE kedua\n",
    "model berada pada skala Wh yang sama. Karena RFE memakai training set\n",
    "dan window yang sama, estimasi ini bisa sedikit optimistis.\n")


# ============================================================
# S. EVALUASI TESTING SET (SEMUA MODEL PADA SKALA ASLI Wh)
# ============================================================

actual_test <- test_M$Appliances

# 1. Baseline
pred_base_test <- predict(modelBaseline, newdata = test_M)

# 2. Box-Cox pada skala transformasi
pred_bc_scale_test <- predict(modelBoxCox, newdata = test_M_bc)

# 3. Pemeriksaan domain inverse: 1 + lambda * Z > 0
domain_condition <- 1 + lambda_used * pred_bc_scale_test
n_invalid <- sum(domain_condition <= 0, na.rm = TRUE)

cat("\n--- DOMAIN CHECK INVERSE BOX-COX ---\n")
cat("Lambda:", round(lambda_used, 4), "\n")
cat("Jumlah prediksi test:", length(pred_bc_scale_test), "\n")
cat("Prediksi di luar domain:", n_invalid, "\n")
cat("Proporsi di luar domain:",
    round(mean(domain_condition <= 0, na.rm = TRUE) * 100, 4), "%\n")

# 4. Inverse Box-Cox (lambda SAMA dengan yang dipakai saat fit)
pred_bc_test <- bc_inverse(pred_bc_scale_test, lambda_used)

cat("\n--- Ringkasan prediksi Baseline ---\n"); print(summary(pred_base_test))
cat("\n--- Ringkasan prediksi Box-Cox pada skala transformasi ---\n"); print(summary(pred_bc_scale_test))
cat("\n--- Ringkasan prediksi Box-Cox setelah inverse ---\n"); print(summary(pred_bc_test))
cat("\nPrediksi tidak finite - Baseline:", sum(!is.finite(pred_base_test)),
    "| Box-Cox:", sum(!is.finite(pred_bc_test)), "\n")

# 5. Metrik
hasil_base <- eval_metrics(actual_test, pred_base_test, "Baseline (Original scale)")
hasil_bc   <- eval_metrics(actual_test, pred_bc_test,
                           paste0("Box-Cox (lambda = ", round(lambda_used, 3), ")"))

hasil_evaluasi <- rbind(hasil_base, hasil_bc)

cat("\n--- TEST SET: MODEL UTAMA (skala Wh) ---\n")
print(hasil_evaluasi, row.names = FALSE)

if (hasil_base$N_valid != hasil_bc$N_valid) {
  cat("PERINGATAN: N_valid berbeda antar model (prediksi NA dikeluarkan dari\n",
      "metrik). Bandingkan RMSE/MAE dengan hati-hati.\n")
}

cat("\nCATATAN: tanpa koreksi retransformasi, inverse Box-Cox cenderung\n",
    "menghasilkan prediksi sekitar median kondisional, bukan mean. Ini bisa\n",
    "sedikit merugikan RMSE Box-Cox pada skala Wh - sebutkan di pembahasan.\n")


# ============================================================
# T. UNCERTAINTY
# ============================================================

cat("\n--- RINGKASAN UNCERTAINTY ---\n")
cat("Baseline - CI koefisien (95%):\n"); print(confint(modelBaseline))
cat("\nBox-Cox - CI koefisien (95%, skala Box-Cox):\n"); print(confint(modelBoxCox))

cat("\nVariasi RMSE antar-slice CV (skala Wh):\n")
cat("Baseline:", round(sd(cv_base$RMSE), 3), "\n")
cat("Box-Cox :", round(sd(cv_bc$RMSE), 3), "\n")

cat("\nINTERPRETASI: CI koefisien menjawab 'seberapa yakin kita terhadap arah\n",
    "& besar pengaruh tiap prediktor'; SD RMSE antar-slice menjawab 'seberapa\n",
    "stabil performa prediksi model' - keduanya saling melengkapi.\n",
    "Catatan: CI koefisien tidak memperhitungkan ketidakpastian dari proses\n",
    "seleksi variabel (RFE) dan mengasumsikan residual independen.\n")


# ============================================================
# U. RANDOM FOREST (benchmark predictive accuracy - SATU model)
# ============================================================

set.seed(123)
rf_bench <- randomForest::randomForest(
  Appliances ~ .,
  data = train_M,
  ntree = 500,
  importance = TRUE
)

pred_rf_test <- predict(rf_bench, newdata = test_M)
hasil_rf <- eval_metrics(actual_test, pred_rf_test, "Random Forest (benchmark)")

hasil_final <- rbind(hasil_base, hasil_bc, hasil_rf)

cat("\n--- PERBANDINGAN PREDICTIVE ACCURACY (skala Wh, testing set) ---\n")
print(hasil_final, row.names = FALSE)

cat("\n--- Variable Importance Random Forest ---\n")
imp_rf <- randomForest::importance(rf_bench)
print(imp_rf[order(-imp_rf[, "%IncMSE"]), , drop = FALSE])

cat("\n--- Pemeriksaan prediksi Random Forest ---\n")
print(summary(pred_rf_test))
cat("Prediksi RF negatif:", sum(pred_rf_test < 0, na.rm = TRUE), "\n")
cat("Prediksi RF > 500  :", sum(pred_rf_test > 500, na.rm = TRUE), "\n")
cat("Prediksi RF > 1000 :", sum(pred_rf_test > 1000, na.rm = TRUE), "\n")
cat("Prediksi RF tidak finite:", sum(!is.finite(pred_rf_test)), "\n")


# ============================================================
# V. BEDAKAN INFERENSI DAN PREDICTION
# ============================================================

cat("\n--- INFERENSI vs PREDICTION ---\n",
    "REGRESI (Baseline & Box-Cox) - fokus: statistical inference\n",
    "  (koefisien, SE, CI, asosiasi, uncertainty - Bagian L, M, T).\n",
    "RANDOM FOREST - fokus: predictive accuracy semata (RMSE, MAE, R2, MAPE\n",
    "  pada data yang belum dipakai fitting - Bagian S dan U).\n")

best_reg_rmse <- min(hasil_base$RMSE, hasil_bc$RMSE)

if (hasil_rf$RMSE < best_reg_rmse) {
  cat("Pada testing set, Random Forest memiliki RMSE lebih rendah\n",
      "daripada kedua model regresi pada skema evaluasi ini.\n")
} else {
  cat("Pada testing set, salah satu model regresi memiliki RMSE\n",
      "setara atau lebih rendah daripada Random Forest pada skema evaluasi ini.\n")
}
cat("TIDAK BOLEH disimpulkan: 'Random Forest membuktikan hubungan\n",
    "antarvariabel lebih baik' - kedua pendekatan punya tujuan analitik berbeda.\n")


# ============================================================
# W. TEMUAN UTAMA
# ============================================================

cat("\n============================================================\n",
    "TEMUAN UTAMA\n",
    "============================================================\n",
    "1. Response (Appliances) memiliki skewness =", round(skew_val, 2),
    "dan Breusch-Pagan baseline p =", format.pval(bp_base$p.value, digits = 3),
    "(", ifelse(bp_base$p.value < 0.05,
                "indikasi heteroskedastisitas", "tidak ada indikasi heteroskedastisitas"),
    ").\n",
    "2. RFE regresi linear (time-aware, training set) memilih",
    length(predictors_rfe), "dari", length(predictors_candidate),
    "kandidat predictor.\n",
    "3. Box-Cox dipilih data-driven dengan lambda =", round(lambda_used, 3),
    "(CI 95%:", round(ci_range[1], 3), "sampai", round(ci_range[2], 3), ").\n",
    "4. Breusch-Pagan p-value berubah dari", format.pval(bp_base$p.value, digits = 3),
    "menjadi", format.pval(bp_bc$p.value, digits = 3),
    "setelah transformasi - lihat tabel Bagian Q (bukan hanya R2).\n",
    "5. Baseline, Box-Cox, dan Random Forest memakai predictor hasil RFE yang SAMA.\n",
    "6. Ketidakpastian koefisien (CI 95%) dan variasi RMSE CV tersedia\n",
    "   untuk kedua model (Bagian R dan T).\n",
    "7. Predictive accuracy pada testing set (skala Wh):\n")
print(hasil_final, row.names = FALSE)
cat("8. Random Forest hanya benchmark predictive accuracy, bukan dasar\n",
    "   inferensi koefisien maupun klaim kausal.\n",
    "\nPREDICTOR FINAL HASIL RFE:\n")
print(predictors_rfe)
cat("============================================================\n")
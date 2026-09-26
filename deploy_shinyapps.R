# ==============================================================================
# SCRIPT DEPLOY / UPDATE APLIKASI KE SHINYAPPS.IO
# Target Aplikasi: appliances-energy-prediction16Predictor
# ==============================================================================

# 1. Muat library rsconnect
library(rsconnect)

# 2. Atur batas timeout koneksi
options(timeout = 600)
options(rsconnect.http.timeout = 300)

# 3. Kredensial akun shinyapps.io
# Masukkan token & secret akun Anda dari https://www.shinyapps.io/admin/#/tokens
# rsconnect::setAccountInfo(
#   name   = '<YOUR_ACCOUNT_NAME>',
#   token  = '<YOUR_TOKEN>',
#   secret = '<YOUR_SECRET>'
# )

# 4. Eksekusi Deploy & Update Aplikasi
# Catatan: Pilih salah satu (app_input_manual.R atau app_boxcox.R) pada appPrimaryDoc
rsconnect::deployApp(
  appDir        = getwd(),
  appPrimaryDoc = "app_input_manual.R",                      # Ganti ke "app_boxcox.R" jika ingin versi slider
  appName       = "appliances-energy-prediction16Predictor", # Nama aplikasi di shinyapps.io
  appFiles      = c("app_input_manual.R", "dashboard_boxcox_data.rds"),
  lint          = FALSE,                                     # Mencegah dialog prompt interaktif
  forceUpdate   = TRUE                                       # Otomatis menimpa versi lama
)

cat("\n============================================================\n")
cat("Aplikasi berhasil diperbarui di URL:\n")
cat("https://vnka.shinyapps.io/appliances-energy-prediction16Predictor/\n")
cat("============================================================\n")

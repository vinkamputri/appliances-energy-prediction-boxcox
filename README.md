# ⚡ Pemodelan Konsumsi Energi Peralatan Rumah Tangga Menggunakan Transformasi Respons Box-Cox dan Recursive Feature Elimination (RFE)

> **Mata Kuliah:** Analisis Regresi Tingkat Lanjut  
> **Program Studi:** Magister Statistika Terapan, Departemen Statistika  
> **Fakultas:** Matematika dan Ilmu Pengetahuan Alam (FMIPA), Universitas Padjadjaran  
> **Dosen Pengampu:** Dr. I Gede Nyoman Mindra Jaya  
> **Penyusun:**  
> 1. **Intania Bungan Apui** (NPM: `140720260013`)  
> 2. **Vinka Marisa Putri** (NPM: `140720260012`)  

---

## 📌 1. Deskripsi Proyek

Proyek ini bertujuan untuk memodelkan dan memprediksi konsumsi energi peralatan rumah tangga (*Appliances*) berbasis data sensor nirkabel *Internet of Things* (IoT) dari dataset **UCI Appliances Energy Prediction** (19.735 baris data, logging 10 menit selama 4,5 bulan di Belgia).

Tantangan utama data ini adalah:
1. **Distribusi Menjulur ke Kanan (*Heavy Right-Skewed*, Skewness = +3.39):** Mayoritas waktu beban berada pada level *standby* (50–60 Wh) dengan lonjakan pemakaian sesaat hingga 1.080 Wh.
2. **Pelanggaran Asumsi Gauss-Markov pada OLS Mentah:** Mengakibatkan heteroskedastisitas berat ($p < 2.2 \times 10^{-16}$) dan deviasi normalitas ekstrem.
3. **Multikolinearitas Tinggi:** Korelasi kuat antar-sensor suhu ($T_1 \dots T_9$) dan kelembaban ($RH_1 \dots RH_9$).

Untuk mengatasi tantangan tersebut, diterapkan:
* **Seleksi Fitur RFE (*Recursive Feature Elimination*):** Menyaring 13 prediktor paling optimal berbasis *time-aware cross-validation* pada *training set*.
* **Transformasi Respons Box-Cox ($\lambda = -0.504$):** Menormalkan galat residual mendekati simetri (skewness residual = +0.08) dan menstabilkan varians.
* **Inversi Transformasi Non-Negatif:** $Y_{\text{pred}} = (1 + \lambda Z_{\text{pred}})^{1/\lambda}$ untuk menghasilkan estimasi energi riil (Wh) yang valid.
* **Benchmark AI Non-Parametrik (*Random Forest Regression*):** Menguji daya generalisasi *machine learning* terhadap pergeseran musiman (*seasonal distribution shift*).

---

## 📊 2. Ringkasan Hasil Evaluasi Model

Evaluasi model dilakukan secara independen pada **Testing Set Temporal (25% data uji / 4.934 observasi)** pada skala fisik energi asli (**Watt-hour / Wh**):

| Model Evaluasi | RMSE (Wh) | MAE (Wh) | $R^2$ Score | MAPE (%) | Keterangan |
| :--- | :---: | :---: | :---: | :---: | :--- |
| **1. Baseline OLS (Skala Asli Wh)** | 84.87 | 52.12 | 0.0757 | 63.37% | Terdistorsi skewness & varians heterogen |
| **2. Box-Cox OLS ($\lambda = -0.504$)** | **84.04** | **40.69** | **0.0935** | **39.19%** | **Terbaik (Akurasi MAE naik 21%, MAPE turun ke 39.19%)** |
| **3. Random Forest (AI Benchmark)** | 189.97 | 162.86 | -3.6300 | 240.47% | Gagal ekstrapolasi temporal pada pergeseran musim |

---

## 🗂️ 3. Struktur File dan Direktori

```text
📁 Project Upload/
├── 📄 README.md                                            # Dokumentasi utama proyek
├── 📄 Kodingan R.R                                         # Skrip komputasi R lengkap (Master Checklist A-W)
├── 📄 app_input_manual.R                                   # Dashboard R Shiny interaktif (Edisi Manual Numeric Input)
├── 📄 dashboard_boxcox_data.rds                            # Data pra-hitung ringan (738 KB) untuk dashboard
├── 📄 deploy_shinyapps.R                                   # Skrip otomatisasi deploy ke ShinyApps.io
├── 📄 Paper_Project_Transformasi_Respons_Vinka_Intania.pdf # Paper ilmiah 5 halaman (Format Jurnal)
├── 📄 Paper_Project_Transformasi_Respons_Vinka_Intania.docx# Source file dokumen paper ilmiah
└── 📄 Laporan project transformasi respon Vinka dan Intania.pdf # Laporan lengkap proyek
```

---

## 🚀 4. Panduan Menjalankan Proyek

### A. Prasyarat Paket R (*Prerequisites*)
Pastikan paket-paket R berikut sudah terpasang:
```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "lubridate", "MASS", "randomForest", "car", "lmtest", "caret", "rsconnect"))
```

### B. Menjalankan Dashboard Shiny Secara Lokal
Buka R / RStudio, arahkan ke direktori proyek, lalu jalankan:

```r
# Opsi 1: Menjalankan Dashboard Versi Input Manual (Rekomendasi)
shiny::runApp("app_input_manual.R")



### C. Deploy ke ShinyApps.io
Jalankan skrip `deploy_shinyapps.R` atau eksekusi perintah berikut di konsol R:

```r
library(rsconnect)
options(timeout = 600, rsconnect.http.timeout = 300)

rsconnect::deployApp(
  appDir        = getwd(),
  appPrimaryDoc = "app_input_manual.R",
  appName       = "appliances-energy-prediction16Predictor",
  appFiles      = c("app_input_manual.R", "dashboard_boxcox_data.rds"),
  lint          = FALSE,
  forceUpdate   = TRUE
)
```

🌐 **Live Demo Dashboard:**  
[https://vnka.shinyapps.io/appliances-energy-prediction16Predictor/](https://vnka.shinyapps.io/appliances-energy-prediction16Predictor/)

---

## 🔬 5. Fitur Interaktif Dashboard

1. **Tab 1: Distribusi & Transformasi Respons:**
   * Visualisasi histogram skala mentah vs skala Box-Cox ($Z$).
   * Estimasi densitas KDE dan profil diurnal konsumsi 24 jam.
2. **Tab 2: Diagnostik Asumsi Gauss-Markov & Inferensi:**
   * Uji Breusch-Pagan (Homoskedastisitas) dan Uji Durbin-Watson (Autokorelasi).
   * Plot Residual vs Fitted, Normal Q-Q Plot, Cook's Distance, dan Tabel VIF.
   * Tabel inferensi koefisien regresi OLS Box-Cox.
3. **Tab 3: Simulator Prediksi & Benchmark AI:**
   * Kolom input manual 13 prediktor lingkungan ($T_1 \dots T_9$, $RH_1 \dots RH_9$, $T_{\text{out}}$, $T_{\text{dewpoint}}$) dengan panduan rentang data riil.
   * Tombol preset cepat (*Standar, Panas 30°C, Dingin 15°C*).
   * Perbandingan estimasi *real-time* multi-model: Baseline OLS vs Box-Cox OLS vs Random Forest.

---

## 📚 6. Referensi Ilmiah

1. **Box, G. E., & Cox, D. R. (1964).** *An analysis of transformations.* Journal of the Royal Statistical Society: Series B (Methodological), 26(2), 211-243.
2. **Breiman, L. (2001).** *Random forests.* Machine Learning, 45(1), 5-32.
3. **Candanedo, L. M., Feldheim, V., & Deramaix, D. (2017).** *Data driven prediction models of energy use of appliances in a low-energy house.* Energy and Buildings, 140, 81-97.
4. **Hastie, T., Tibshirani, R., & Friedman, J. (2009).** *The Elements of Statistical Learning: Data Mining, Inference, and Prediction (2nd ed.).* Springer.
5. **Montgomery, D. C., Peck, E. A., & Vining, G. G. (2021).** *Introduction to Linear Regression Analysis (6th ed.).* John Wiley & Sons.

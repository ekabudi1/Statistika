# I Wayan Eka Budi Permana/2415091027/1 Dps

install.packages("ggplot2")
install.packages("car")


# Creating the dataset in R
data <- data.frame(
  ID = 1:10,
  Jam_Belajar = c(2.5, 3.0, 1.5, 4.0, 3.5, 2.0, 5.0, 4.5, 3.2, 2.8),
  Nilai_Ujian = c(75, 78, 70, 85, 80, 73, 90, 88, 79, 76)
)

# Categorizing "Jam Belajar" into groups: Low and Medium-High
data$Kategori_Jam_Belajar <- cut(
  data$Jam_Belajar,
  breaks = c(0, 2.5, 5.0),
  labels = c("Low", "Medium-High")
)

# Load necessary libraries
library(ggplot2)
library(car)

# Uji Asumsi 1: Normalitas (Shapiro-Wilk test)
anova_groups <- split(data$Nilai_Ujian, data$Kategori_Jam_Belajar)
normality_results <- lapply(anova_groups, shapiro.test)

# Uji Asumsi 2: Homogenitas Varians (Levene's Test)
levene_test <- leveneTest(Nilai_Ujian ~ Kategori_Jam_Belajar, data = data)

# Uji Asumsi 3: Kesamaan rata-rata (One-Way ANOVA)
anova_result <- aov(Nilai_Ujian ~ Kategori_Jam_Belajar, data = data)
anova_summary <- summary(anova_result)

# Visualisasi Boxplot
ggplot(data, aes(x = Kategori_Jam_Belajar, y = Nilai_Ujian)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("Distribusi Nilai Ujian Berdasarkan Kategori Jam Belajar") +
  xlab("Kategori Jam Belajar") +
  ylab("Nilai Ujian") +
  theme_minimal()

# Output summary
cat("Hasil Uji Asumsi dan Analisis ANOVA:\n\n")

cat("Normalitas:\n")
for (group in names(normality_results)) {
  cat(sprintf("  - %s: Statistic = %.3f, p-value = %.3f\n", 
              group, normality_results[[group]]$statistic, normality_results[[group]]$p.value))
}

cat("\nHomogenitas Varians:\n")
print(levene_test)

cat("\nHasil ANOVA:\n")
print(anova_summary)




# -----------------interpretasi---------------------------
cat("\nInterpretasi Deskriptif:\n")

# Normalitas
cat("Hasil uji normalitas menunjukkan:\n")
for (group in names(normality_results)) {
  cat(sprintf("  - Grup %s memiliki nilai p sebesar %.3f\n", 
              group, normality_results[[group]]$p.value))
}

# Homogenitas Varians
cat("\nHasil uji homogenitas varians:\n")
cat(sprintf("  - Nilai p Levene's Test adalah %.3f\n", levene_test$`Pr(>F)`[1]))

# Kesamaan Rata-rata (ANOVA)
cat("\nHasil uji kesamaan rata-rata (ANOVA):\n")
anova_pvalue <- summary(anova_result)[[1]][["Pr(>F)"]][1]
cat(sprintf("  - Nilai p ANOVA adalah %.3f\n", anova_pvalue))

cat("\nCatatan:\n")
cat("  - Jika nilai p < 0.05, hasil uji menunjukkan perbedaan signifikan atau pelanggaran asumsi.\n")
cat("  - Jika nilai p > 0.05, tidak ada perbedaan signifikan atau asumsi terpenuhi.\n")

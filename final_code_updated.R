

library(ggplot2)
library(dplyr)

# ---- Path to dataset ----
DATA_FILE <- "C:/Users/MrLaptop/Desktop/webs/diet/Fat_Supply_Quantity_Data.csv"

cat("Loading dataset from:\n", DATA_FILE, "\n\n")

# ==============================
# 1. LOAD DATA
# ==============================

fat_raw <- read.csv(DATA_FILE,
                    stringsAsFactors = FALSE,
                    check.names = TRUE)

cat("Columns detected in dataset:\n")
print(names(fat_raw))
cat("\nFirst rows:\n")
print(head(fat_raw))
cat("\n")

# ==============================
# 2. IDENTIFY COLUMNS
# ==============================

animal_col <- grep("Animal", names(fat_raw), value = TRUE)[1]
plant_col  <- grep("Vegetal|Plant", names(fat_raw), value = TRUE)[1]

cat("Animal fat column detected: ", animal_col, "\n", sep = "")
cat("Plant fat column detected : ", plant_col,  "\n\n", sep = "")

# ==============================
# 3. CLEAN DATA
# ==============================

fat <- fat_raw %>%
  dplyr::select(Country, all_of(animal_col), all_of(plant_col))

names(fat)[2:3] <- c("Animal_Fat", "Plant_Fat")

fat$Animal_Fat <- as.numeric(fat$Animal_Fat)
fat$Plant_Fat  <- as.numeric(fat$Plant_Fat)

fat_clean <- fat %>%
  filter(!is.na(Animal_Fat), !is.na(Plant_Fat))

cat("Countries included in analysis:", nrow(fat_clean), "\n\n")

# ==============================
# 4. SUMMARY STATISTICS
# ==============================

cat("===== SUMMARY STATISTICS =====\n")
print(summary(fat_clean))
cat("\n")

# ==============================
# 5. VISUALISATIONS (SHOW PLOTS ONLY)
# ==============================

# ----- 5.1 Scatter Plot -----
scatter_plot <- ggplot(fat_clean,
                       aes(x = Animal_Fat, y = Plant_Fat)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  labs(
    title = "Correlation between Animal Fat Supply and Plant Fat Supply",
    x     = "Animal Fat Supply (g/capita/day)",
    y     = "Plant Fat Supply (g/capita/day)"
  ) +
  theme_minimal()

print(scatter_plot)  # SHOW IN RSTUDIO


# ----- 5.2 Histogram -----
hist_plot <- ggplot(fat_clean, aes(x = Animal_Fat)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Animal Fat Supply Across Countries",
    x     = "Animal Fat Supply (g/capita/day)",
    y     = "Number of Countries"
  ) +
  theme_minimal()

print(hist_plot)  # SHOW IN RSTUDIO

# ==============================
# 6. STATISTICAL ANALYSIS
# ==============================

cat("===== PEARSON CORRELATION =====\n")
cor_result <- cor.test(fat_clean$Animal_Fat,
                       fat_clean$Plant_Fat,
                       method = "pearson")

print(cor_result)

r_value   <- as.numeric(cor_result$estimate)
p_value   <- cor_result$p.value
df_value  <- cor_result$parameter
n_value   <- df_value + 2
r_squared <- r_value^2

cat("\nCorrelation r: ", round(r_value, 3), "\n", sep = "")
cat("p-value: ", signif(p_value, 3), "\n", sep = "")
cat("R-squared: ", round(r_squared, 3), "\n\n", sep = "")

# ----- Linear Regression -----
cat("===== LINEAR REGRESSION MODEL =====\n")
lm_model <- lm(Plant_Fat ~ Animal_Fat, data = fat_clean)
print(summary(lm_model))
cat("\n")

# ==============================
# 7. TEXT SNIPPETS FOR WORD REPORT
# ==============================

cat("----- Research Question (Copy into Report) -----\n")
cat(
  "This study investigates whether there is a statistically significant ",
  "correlation between per-capita animal fat supply and plant fat supply ",
  "across countries. The research explores whether countries with greater ",
  "availability of animal-based fats also tend to have higher availability ",
  "of plant (vegetal) fats.\n\n", sep = ""
)

cat("----- Hypotheses (Copy into Report) -----\n")
cat(
  "H0: There is no linear correlation between per-capita animal fat supply ",
  "and plant fat supply (ρ = 0).\n",
  "H1: There is a positive linear correlation between per-capita animal fat ",
  "supply and plant fat supply (ρ > 0).\n\n", sep = ""
)

cat("===== END OF SCRIPT =====\n")
fat_clean$Animal_Category <- ifelse(fat_clean$Animal_Fat >= median(fat_clean$Animal_Fat),
                                    "High Animal Fat", "Low Animal Fat")

fat_clean$Plant_Category <- ifelse(fat_clean$Plant_Fat >= median(fat_clean$Plant_Fat),
                                   "High Plant Fat", "Low Plant Fat")
table(fat_clean$Animal_Category, fat_clean$Plant_Category)

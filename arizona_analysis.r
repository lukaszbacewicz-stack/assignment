# --- ARIZONA: OPTIMIZED FULL ANALYSIS ---
library(readr)
library(dplyr)
library(Synth)
library(ggplot2)

# 1. DATA LOADING & PREPARATION
sheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6AOT-QzZ-G7Q9mxcFmo0ZQXWr7YxbzPPBrTp_1JbdlONkUTGEAo_Q-gvMmcf7-hq9_e00nmWfr3r6/pub?gid=304023993&output=csv"
df <- read_csv(sheet_url) %>% mutate(State = trimws(State))

never_treated <- c(
  "Alabama", "Arkansas", "Florida", "Georgia", "Hawaii", "Idaho",
  "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Mississippi",
  "Nebraska", "New Hampshire", "North Carolina", "North Dakota",
  "Oklahoma", "Pennsylvania", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "West Virginia", "Wisconsin", "Wyoming"
)

df_az <- df %>%
  filter(State %in% c("Arizona", never_treated)) %>%
  mutate(state_id = as.numeric(as.factor(State)))

id_map <- df_az %>% distinct(State, state_id)
all_units <- c("Arizona", never_treated)

# 2. RUN ALL MODELS (SINGLE LOOP FOR EFFICIENCY)
gaps_list <- list()

cat("Processing all 27 units... please wait.\n")

for (s_name in all_units) {
  t_id <- id_map$state_id[id_map$State == s_name]
  c_ids <- id_map$state_id[id_map$State != s_name]
  
  try({
    dp_loop <- dataprep(
      foo = as.data.frame(df_az),
      predictors = c("Politics", "Alcohol_PC", "PR", "Edu"),
      predictors.op = "mean",
      time.predictors.prior = 2014:2020,
      special.predictors = list(list("AADR", 2014:2020, "mean")),
      dependent = "AADR",
      unit.variable = "state_id",
      unit.names.variable = "State",
      time.variable = "Year",
      treatment.identifier = t_id,
      controls.identifier = c_ids,
      time.optimize.ssr = 2014:2020,
      time.plot = 2014:2023
    )
    
    # Robust settings for convergence across all states
    syn_loop <- synth(dp_loop, verbose = FALSE, Margin.ipop = 0.02, Sigf.ipop = 7)
    
    if(s_name == "Arizona") {
      synth.az <- syn_loop
      dataprep.az <- dp_loop
    }
    
    gap_vals <- dp_loop$Y1plot - (dp_loop$Y0plot %*% syn_loop$solution.w)
    
    # Calculate RMSPE Components
    pre_rmspe  <- sqrt(mean(gap_vals[as.numeric(rownames(gap_vals)) <= 2020]^2))
    post_rmspe <- sqrt(mean(gap_vals[as.numeric(rownames(gap_vals)) > 2020]^2))
    
    gaps_list[[s_name]] <- data.frame(
      Year = as.numeric(rownames(gap_vals)),
      Gap = as.numeric(gap_vals),
      State = s_name,
      Pre_RMSPE = pre_rmspe,
      Post_RMSPE = post_rmspe,
      Ratio = post_rmspe / pre_rmspe,
      Type = ifelse(s_name == "Arizona", "Treated", "Placebo")
    )
  }, silent = TRUE)
}

# 3. ARIZONA'S PRE-TREATMENT FIT (RMSPE)
# Confirms how well the synthetic version matches the real Arizona before policy change
cat("\n--- ARIZONA'S PRE-TREATMENT FIT (RMSPE) ---\n")
az_pre_rmspe <- gaps_list[["Arizona"]]$Pre_RMSPE[1]
cat("Formula-based RMSPE (2014-2020):", round(az_pre_rmspe, 5), "\n")

# 4. ARIZONA YEAR-BY-YEAR ANALYSIS
actual    <- dataprep.az$Y1plot
synthetic <- dataprep.az$Y0plot %*% synth.az$solution.w
full_results <- data.frame(
  Year = as.numeric(rownames(actual)),
  Actual_AADR = round(actual[,1], 2),
  Synth_AADR = round(synthetic[,1], 2),
  Gap = round(actual[,1] - synthetic[,1], 2),
  Actual_Change = round(c(NA, diff(actual[,1])), 2),
  Synth_Change = round(c(NA, diff(synthetic[,1])), 2)
)

cat("\n--- ARIZONA YEAR-BY-YEAR ANALYSIS (With Differences) ---\n")
print(full_results, row.names = FALSE)

# 5. UNIT WEIGHTS
synth.tables.az <- synth.tab(dataprep.res = dataprep.az, synth.res = synth.az)
weights_table <- data.frame(
  State = synth.tables.az$tab.w$unit.names,
  Weight = round(synth.tables.az$tab.w$w.weights, 3)
) %>% arrange(desc(Weight))

cat("\n--- ARIZONA UNIT WEIGHTS (Top Contributors) ---\n")
print(subset(weights_table, Weight > 0), row.names = FALSE)

# 6. PLOTS: PATH, GAP, AND PLACEBOS
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
path.plot(synth.az, dataprep.az, Main = "AZ vs. Synthetic AZ (AADR Path)", Ylab = "AADR", Xlab = "Year")
abline(v = 2020, col = "red", lty = "dashed")

gaps.plot(synth.az, dataprep.az, Main = "Treatment Effect (Actual - Synthetic Gap)", Ylab = "Gap in AADR", Xlab = "Year")
abline(v = 2020, col = "red", lty = "dashed")
abline(h = 0, lty = "dotted")
par(mfrow = c(1, 1))

all_gaps_df <- do.call(rbind, gaps_list)
ggplot(all_gaps_df, aes(x = Year, y = Gap, group = State, color = Type, size = Type, alpha = Type)) +
  geom_line() +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_manual(values = c("Placebo" = "gray80", "Treated" = "red")) +
  scale_size_manual(values = c("Placebo" = 0.5, "Treated" = 1.2)) +
  scale_alpha_manual(values = c("Placebo" = 0.5, "Treated" = 1)) +
  theme_minimal() + labs(title = "In-Space Placebo Gaps", y = "Gap (Actual - Synthetic)")

# 7. RMSPE RATIO ANALYSIS
ratio_results <- all_gaps_df %>% 
  distinct(State, .keep_all = TRUE) %>% 
  select(State, Ratio, Type) %>% 
  arrange(desc(Ratio))

ratio_results$State <- factor(ratio_results$State, levels = ratio_results$State)

cat("\n--- FINAL RMSPE RATIO TABLE (N =", nrow(ratio_results), ") ---\n")
print(ratio_results, row.names = FALSE)

ggplot(ratio_results, aes(x = State, y = Ratio, fill = Type)) +
  geom_bar(stat = "identity", width = 0.75, color = "white") +
  scale_fill_manual(values = c("Placebo" = "#A6CEE3", "Treated" = "red")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
        panel.grid.major.x = element_blank(), legend.position = "none",
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Figure 5. Distribution of Post-/Pre-treatment RMSPE Ratios",
       subtitle = paste0("Total States Optimized: ", nrow(ratio_results)),
       x = "State", y = "Post-period RMSPE / Pre-period RMSPE",
       caption = "Red bar represents the treated unit (Arizona).")

# 8. IN-SPACE PLACEBO INFERENCE
R_Arizona <- ratio_results$Ratio[ratio_results$State == "Arizona"]
N <- nrow(ratio_results)
count_greater_equal <- sum(ratio_results$Ratio >= R_Arizona)
p_value <- count_greater_equal / N

cat("\n--- IN-SPACE PLACEBO INFERENCE ---")
cat("\nArizona RMSPE Ratio (R_Arizona): ", round(R_Arizona, 4))
cat("\nNumber of units with Ratio >= Arizona: ", count_greater_equal)
cat("\nTotal units in distribution (N): ", N)
cat("\n----------------------------------")
cat("\nPermutation p-value: ", round(p_value, 4))
cat("\n----------------------------------\n")
cat("Result:", ifelse(p_value <= 0.05, "Statistically Significant (p <= 0.05)", "Not Statistically Significant (p > 0.05)"), "\n")

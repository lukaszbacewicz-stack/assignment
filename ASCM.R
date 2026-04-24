install.libraries("readr")
install.libraries("dplyr")
install.libraries("augsynth")
install.libraries("ggplot2")
library(readr)
library(dplyr)
library(augsynth)
library(ggplot2)

#data import
sheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6AOT-QzZ-G7Q9mxcFmo0ZQXWr7YxbzPPBrTp_1JbdlONkUTGEAo_Q-gvMmcf7-hq9_e00nmWfr3r6/pub?gid=304023993&output=csv"
#pointing out never treated states
never_treated <- c(
  "Alabama", "Arkansas", "Florida", "Georgia", "Hawaii", "Idaho",
  "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Mississippi",
  "Nebraska", "New Hampshire", "North Carolina", "North Dakota",
  "Oklahoma", "Pennsylvania", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "West Virginia", "Wisconsin", "Wyoming"
)

df_ascm <- read_csv(sheet_url) %>% 
  mutate(State = trimws(State)) %>%
  filter(State %in% c("Arizona", never_treated)) %>%
  mutate(treated_dummy = ifelse(State == "Arizona" & Year >= 2021, 1, 0))

# running augmented synthetic control using the Ben Mitchell package
ascm_mod <- augsynth(AADR ~ treated_dummy, 
                     unit = State, 
                     time = Year, 
                     data = df_ascm,
                     progfunc = "ridge", 
                     scm = TRUE)

#printing out the results
cat("\n--- AUGMENTED SCM SUMMARY ---\n")
print(summary(ascm_mod))
#since the graph automatically has a line when the treatment starts, I am printing a graph with red line for treatment 
plot(ascm_mod) + 
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Augmented Synthetic Control: Arizona (AADR)",
       subtitle = "Vertical line marks 2020; Treatment begins 2021",
       y = "Difference (Actual - Synthetic)")

# Printing full weigths here
# Extracting once and formatting to 3 significant figures
all_weights <- data.frame(
  State = rownames(ascm_mod$weights),
  Weight = signif(as.numeric(ascm_mod$weights), 3)
) %>% 
  arrange(desc(abs(Weight))) # Sorted by absolute influence

cat("\n--- COMPLETE DONOR POOL WEIGHTS (3 SIGNIFICANT FIGURES) ---\n")
print(all_weights, row.names = FALSE)

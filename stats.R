# Installing and loading necessary packages
if (!require("psych")) install.packages("psych")
library(psych)

# Importing the data from your Google Sheets CSV link
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6AOT-QzZ-G7Q9mxcFmo0ZQXWr7YxbzPPBrTp_1JbdlONkUTGEAo_Q-gvMmcf7-hq9_e00nmWfr3r6/pub?gid=304023993&single=true&output=csv"
data <- read.csv(url)

# Selecting variables included in the analysis
selected_vars <- data[, c("AADR", "Politics", "Alcohol_PC", "PR", "Edu")]

# Calculating the descriptive stats
stats_table <- describe(selected_vars)

# Printing the results
print(stats_table)

library(dplyr)

# calculating the mean for every state
state_means <- data %>%
  group_by(State) %>%
  summarise(across(c(AADR, Politics, Alcohol_PC, PR, Edu), 
                   mean, na.rm = TRUE))

# printing the results
print(state_means, n = Inf)

# Calculating the median for every state
state_medians <- data %>%
  group_by(State) %>%
  summarise(across(c(AADR, Politics, Alcohol_PC, PR, Edu), 
                   median, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2))) # Optional: rounds to 2 decimal places

# Printing the results
print(state_medians, n = Inf)

# Calculate Min and Max for each variable by State
state_ranges <- data %>%
  group_by(State) %>%
  summarise(across(c(AADR, Politics, Alcohol_PC, PR, Edu), 
                   list(min = ~min(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# Print the results
print(state_ranges, n = Inf)

# Load  libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(dplyr)
#grouping the data by states (summary)
state_summary <- data %>%
  group_by(State) %>%
  summarise(Politics = mean(Politics, na.rm = TRUE),
            PR = mean(PR, na.rm = TRUE))

firearm_data <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  Firearm_Ownership = c(53, 57, 36, 52, 16, 38, 19, 39, 29, 38, 9, 58, 23, 42, 39, 42, 53, 52, 48, 17, 9, 39, 39, 54, 53, 65, 39, 33, 46, 9, 36, 15, 37, 53, 42, 55, 41, 40, 14, 45, 55, 47, 36, 40, 50, 35, 32, 60, 47, 61)
)

final_df <- inner_join(state_summary, firearm_data, by = "State")

# Drawing plot: Firearm ownership vs politics, estimating the r value
p1 <- ggplot(final_df, aes(x = Politics, y = Firearm_Ownership)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", fill = "pink") + # Adds regression line
  labs(title = "Firearm Ownership vs. Politics",
       subtitle = paste("Pearson r =", round(cor(final_df$Firearm_Ownership, final_df$Politics), 2)),
       x = "Political Ideology (Mean)",
       y = "% Household Firearm Ownership") +
  theme_minimal()
grid.arrange(p1, ncol = 2)


# Define the donor pool states
donor_pool_states <- c(
  "Alabama", "Arkansas", "Florida", "Georgia", "Hawaii", "Idaho", "Indiana",
  "Iowa", "Kansas", "Kentucky", "Louisiana", "Mississippi", "Nebraska", 
  "New Hampshire", "North Carolina", "North Dakota", "Oklahoma", "Pennsylvania", 
  "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
  "West Virginia", "Wisconsin", "Wyoming"
)

#create the comparison table for donor pool and Arizona
comparison_table <- data %>%
  filter(State %in% c(donor_pool_states, "Arizona")) %>%
  mutate(Group = ifelse(State == "Arizona", "Arizona (Treated)", "Donor Pool (Average)")) %>%
  group_by(Group) %>%
  summarise(across(c(AADR, Politics, Alcohol_PC, PR, Edu), 
                   mean, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

#  Print the results
print(comparison_table)

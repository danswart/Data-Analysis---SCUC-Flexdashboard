# Weekly Attendance Data Processing Workflow
# Author: [Your Name]
# Date: [Current Date]

# Load required libraries
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(tidyr)
library(stringr)

# Step 1: Import the wide CSV file
df_wide <- read_csv("data/2024-25 Weekly Atten Percentage-WIDE.csv")



# Step 2: Clean column names with janitor
df_clean <- df_wide %>%
  clean_names()



# Step 3: Convert week column to dates with lubridate (using week ENDING date)
df_dates <- df_clean %>%
  mutate(
    # Extract end date from week range - handle various spacing around dashes
    end_date_text = str_extract(week, "\\s*-\\s*([A-Za-z]+\\s?\\d+)$"),
    end_date_text = str_remove(end_date_text, "^\\s*-\\s*"),  # Remove dash and spaces
    end_date_text = str_replace_all(end_date_text, "\\s", ""),  # Remove remaining spaces

    # Convert to actual date (assuming 2024-2025 school year)
    week_date = mdy(paste0(end_date_text, ", 2024")),

    # Handle year transition (Jan-May are 2025)
    week_date = case_when(
      month(week_date) %in% 1:7 ~ week_date + years(1),
      TRUE ~ week_date
    )
  ) %>%
  select(-end_date_text) %>%  # Remove helper column
  select(week = week_date, everything(), -week)  # Replace original week column



# Step 4: Convert all percentage values from 99.50 to 0.9950
df_converted <- df_dates %>%
  mutate(
    # Convert all numeric columns (except week_date) from percentage to decimal
    across(where(is.numeric) & !matches("week"), ~ .x / 100)
  )

# Step 5: Pivot longer with values going into 'pct' column
df_long <- df_converted %>%
  pivot_longer(
    cols = -week,                    # Keep week column, pivot everything else
    names_to = "campus",             # Campus names go to 'campus' column
    values_to = "pct"                # Values go to 'pct' column
  ) %>%
  mutate(
    metric = "attend"                # Add metric column (all attendance)
  ) %>%
  select(week, campus, metric, pct) %>%  # Reorder columns as requested
  arrange(week, campus)                  # Sort by week, then campus

# Step 6: View the final result
glimpse(df_long)

# Optional: Save the processed data
write_csv(df_long, "data/2024-25_weekly_attendance_tidy.csv")

# Optional: Preview first few rows
head(df_long, 20)

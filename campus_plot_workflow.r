# Campus A Attendance Line Plot Workflow
library(dplyr)
library(ggplot2)

# Step 1: Filter for just Campus A (assuming your tidy data is in df_long)
campus_a_data <- df_long %>%
  filter(campus == "campus_a")  # Use exact name from your data

# Step 2: Create line plot
campus_a_plot <- campus_a_data %>%
  ggplot(aes(x = week, y = pct)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +  # Add points for clarity
  scale_y_continuous(
    labels = scales::percent_format(),  # Show as percentages
    limits = c(0.85, 1.0)  # Adjust range as needed
  ) +
  scale_x_date(
    date_breaks = "2 weeks",  # Tick marks every 2 weeks
    date_labels = "%b %d"     # Format: Aug 16, Sep 02, etc.
  ) +
  labs(
    title = "Campus A Weekly Attendance",
    subtitle = "2024-2025 School Year",
    x = "Week Ending Date",
    y = "Attendance Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate date labels
    plot.title = element_text(size = 14, face = "bold")
  )

# Step 3: Display the plot
print(campus_a_plot)



# Optional: Save the plot
ggsave("plots/campus_a_attendance.png",
       plot = campus_a_plot,
       width = 10, height = 6, dpi = 300)



# Alternative: One-liner approach (no intermediate objects)
df_long %>%
  filter(campus == "campus_a") %>%
  ggplot(aes(x = week, y = pct)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Campus A Weekly Attendance",
       x = "Week Ending Date",
       y = "Attendance Rate") +
  theme_minimal()

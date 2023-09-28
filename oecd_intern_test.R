
library(ggplot2)
library(dplyr)

''
# Group the data by 'Sub-sector' and perform various analyses
data_grouped <- df %>%
  group_by(`Sub-sector`) %>%
  summarize(
    mean_jobs_created = mean(`Jobs created`),
    total_jobs_created = sum(`Jobs created`),
    median_jobs_created = median(`Jobs created`),
    mean_capital_investment = mean(`Capital investment`)
  ) %>%
  arrange(desc(mean_jobs_created)) %>%
  head(10)  # Keep only the Top 10

# Create various visualizations for the analyses
# Bar chart for Average Jobs Created
plot1 <- ggplot(data_grouped, aes(x = reorder(`Sub-sector`, -mean_jobs_created), y = mean_jobs_created)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Jobs Created by Sub-sector (Top 10)",
    x = "Sub-sector",
    y = "Average Jobs Created"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text by 90 degrees
  )

# Bar chart for Total Jobs Created
plot2 <- ggplot(data_grouped, aes(x = reorder(`Sub-sector`, -total_jobs_created), y = total_jobs_created)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Total Jobs Created by Sub-sector (Top 10)",
    x = "Sub-sector",
    y = "Total Jobs Created"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text by 90 degrees
  )

# Bar chart for Median Jobs Created
plot3 <- ggplot(data_grouped, aes(x = reorder(`Sub-sector`, -median_jobs_created), y = median_jobs_created)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Median Jobs Created by Sub-sector (Top 10)",
    x = "Sub-sector",
    y = "Median Jobs Created"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text by 90 degrees
  )

# Bar chart for Average Capital Investment
plot4 <- ggplot(data_grouped, aes(x = reorder(`Sub-sector`, -mean_capital_investment), y = mean_capital_investment)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Capital Investment by Sub-sector (Top 10)",
    x = "Sub-sector",
    y = "Average Capital Investment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate text by 90 degrees
  )


# Compare the number of investments in different countries
country_investments <- df %>%
  group_by(`Source country investor`, `Destination country`) %>%
  summarize(Investment_count = n()) %>%
  arrange(desc(Investment_count))


print(country_investments)

# Count the number of investments from different origin and destination regions
investor_counts <- df %>%
  group_by(`Source region investor`, `Destination region`) %>%
  summarize(Investment_count = n()) %>%
  arrange(desc(Investment_count))

print(investor_counts)


# Count the number of projects for different activities
activity_counts <- df %>%
  group_by(Activity) %>%
  summarize(Project_count = n()) %>%
  arrange(desc(Project_count))

print(activity_counts)


# Calculate the correlation between jobs created and capital investment


correlation <- cor(df$`Jobs created`, df$`Capital investment`, method = "pearson")

print(paste("Correlation between Jobs created and capital investment:", correlation))


# time series of created jobs

df$`Project date` <- as.Date(df$`Project date`) 
plot5 <- ggplot(df, aes(x = `Project date`, y = `Jobs created`)) +
  geom_line() +
  labs(
    title = "Development of jobs created ",
    x = "Date",
    y = "Jobs"
  ) +
  theme_minimal()


# calculate relation between jobs created and capital investment
df$ROI <- (df$`Jobs created` / df$`Capital investment`) * 100  # %
print(df$ROI)



# save plots
ggsave("Average Jobs Created.png", plot1, width = 8, height = 6)
ggsave("Total Jobs Created.png", plot2, width = 8, height = 6)
ggsave("Median Jobs Created.png", plot3, width = 8, height = 6)
ggsave("Average Capital Investment.png", plot4, width = 8, height = 6)
ggsave("time series.png", plot5, width = 8, height = 6)




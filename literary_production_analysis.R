# This project reads a JSON file and turns it into two comparison charts.
library(jsonlite)
library(ggplot2)
library(dplyr)

# Use fixed file paths so R reads the correct data file and saves the charts in the same output folder.
data_path <- file.path(getwd(), "data", "packhum_authors_data.json")
output_dir <- file.path(getwd(), "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read data from the JSON file and refine it to be usable.
authors_raw <- fromJSON(data_path, simplifyDataFrame = FALSE)

authors_df <- do.call(rbind, lapply(authors_raw, function(author) {
  birth_year <- author$birth_year
  works <- author$works

  data.frame(
    author = if (is.null(author$author)) NA_character_ else author$author,
    birth_year = if (is.null(birth_year) || is.na(birth_year)) NA_real_ else as.numeric(birth_year),
    works_count = if (is.null(works)) 0L else length(works),
    stringsAsFactors = FALSE
  )
}))

# Some literature does not have an exact date, as the scholars were unnable to locate them.
# As such, we want to remove the rows with no date available.
authors_df <- subset(authors_df, !is.na(birth_year))
authors_df$period <- case_when(
  authors_df$birth_year < -44 ~ "Before fall of the Republic (44 BC)",
  TRUE ~ "After fall of the Republic (44 BC)"
)

# First chart prep!!! Sum of number of works before and after the fall of the Republic
works_analysis_and_discoveries <- aggregate(works_count ~ period, data = authors_df, FUN = sum)
works_analysis_and_discoveries$period <- factor(
  works_analysis_and_discoveries$period,
  levels = c("Before fall of the Republic (44 BC)", "After fall of the Republic (44 BC)")
)

# Second chart prep!!! Count quantity of authors before and after the fall of the Republic
authors_analysis_and_discoveries <- aggregate(author ~ period, data = authors_df, FUN = length)
names(authors_analysis_and_discoveries)[2] <- "total_authors"
authors_analysis_and_discoveries$period <- factor(
  authors_analysis_and_discoveries$period,
  levels = c("Before fall of the Republic (44 BC)", "After fall of the Republic (44 BC)")
)

# FIRST CHART CREATION!! Send the data prepared before to a png file
works_plot <- ggplot(works_analysis_and_discoveries, aes(x = period, y = works_count, fill = period)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = format(works_count, big.mark = ",")), vjust = -0.4, size = 5, color = "black") +
  scale_fill_manual(values = c("#4C78A8", "#F58518")) +
  labs(
    title = "Total literary production before and after the fall of the Republic",
    subtitle = "Split at 44 BC",
    x = "Period",
    y = "Total works catalogued",
    fill = "Period"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(works_analysis_and_discoveries$works_count) * 1.2)

ggsave(file.path(output_dir, "literature_total_before_after_fall.png"), works_plot, width = 8, height = 6, dpi = 300)

# SECOND CHART CREATION!! Send the data prepared before to a png file
authors_plot <- ggplot(authors_analysis_and_discoveries, aes(x = period, y = total_authors, fill = period)) +
  geom_col(width = 0.7, color = "black") +
  geom_text(aes(label = format(total_authors, big.mark = ",")), vjust = -0.4, size = 5, color = "black") +
  scale_fill_manual(values = c("#4C78A8", "#F58518")) +
  labs(
    title = "Total authors before and after the fall of the Republic",
    subtitle = "Split at 44 BC",
    x = "Period",
    y = "Total authors",
    fill = "Period"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(authors_analysis_and_discoveries$total_authors) * 1.2)

ggsave(file.path(output_dir, "authors_total_before_after_fall.png"), authors_plot, width = 8, height = 6, dpi = 300)

# Check if the run was successful in the terminal
print(works_analysis_and_discoveries)
print(authors_analysis_and_discoveries)

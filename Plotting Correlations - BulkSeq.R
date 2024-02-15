# Load required libraries
library(ggplot2)
library(readr)
library(ggplot2)
library(dplyr)
library(forcats)

# Load the data from the CSV file
filtered_df <- read_csv('C:/Users/crtuser/OneDrive - TCDUD.onmicrosoft.com/Documents/PhD/Project/Writing/Figures/correlation_data.csv')

# Assuming 'filtered_df' is your dataframe with 'Column' for gene names and 'Correlations' for their correlation values
# Order genes by correlation for plotting
filtered_df <- filtered_df %>%
  arrange(Correlation) %>%
  mutate(Column = factor(Column, levels = Column))

# Create the plot using ggplot2
ggplot_object <- ggplot(filtered_df, aes(x = Column, y = Correlation)) +
  geom_point(size = 3) +  # Adjust point size
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", size = 0.5) +  # Add horizontal line at y=0
  theme_minimal() +  # Use a minimalistic theme
  labs(title = "NFKB Gene Target Correlations with TNFAIP3 Across Diets", 
       x = "Gene", 
       y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),  # Adjust X axis labels
        axis.text.y = element_text(size = 10, color = "black"),  # Adjust Y axis labels
        plot.title = element_text(face = "bold", size = 14),  # Bold and larger plot title
        plot.subtitle = element_text(size = 12),  # Subtitle
        axis.title = element_text(size = 12),  # Axis titles
        legend.title = element_text(size = 12),  # Legend title
        legend.text = element_text(size = 10),  # Legend text
        panel.grid.major.x = element_blank(),  # Remove vertical gridlines
        panel.grid.minor.x = element_blank(),  # Remove vertical minor gridlines
        panel.grid.major.y = element_line(color = "lightgray", size = 0.5),  # Lighter horizontal gridlines
        panel.border = element_rect(color = "black", size = 0.5, fill = NA))  # Border around the plot

# Show the plot
print(ggplot_object)

# Save the plot to a file
ggsave("gene_correlation_plot.png", plot = ggplot_object, width = 8, height = 6, dpi = 300)

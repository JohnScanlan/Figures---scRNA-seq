library(ggplot2)

df <- read.csv('C:\\Users\\crtuser\\OneDrive - TCDUD.onmicrosoft.com\\Documents\\PhD\\Project\\Writing\\Figures\\TNFAIP3_week6.csv')

df$Cell.Type <- df$Cluster
df$Log.Fold.Change <- df$avg_log2FC
df$p.value.adj <- df$p_val_adj

# Find entries that contain 'Tnfaip3' in the X column
tnfaip3_rows <- grepl("Tnfaip3", df$X, ignore.case = TRUE)

# Create a new dataframe with these entries
tnfaip3_df <- df[tnfaip3_rows, ]


# Add a new column for significance level based on p.value.adj
df$Significance <- ifelse(df$p.value.adj < 0.001, '***',
                          ifelse(df$p.value.adj < 0.01, '**',
                                 ifelse(df$p.value.adj < 0.05, '*', '')))


library(ggplot2)

# Reorder Cell.Type based on Log.Fold.Change
df$Cell.Type <- reorder(df$Cell.Type, df$Log.Fold.Change)

# Create the bar plot with dynamic fill color based on Log.Fold.Change
p <- ggplot(df, aes(x = Cell.Type, y = Log.Fold.Change, fill = ifelse(Log.Fold.Change < 0, "Negative", "Positive"))) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = Significance), vjust = -0.3, color = "black", size = 5) +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Positive" = "steelblue")) +  # Red for negative, steelblue for positive
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "none",  # This line removes the legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TNFAIP3 Expression - After WL",
       x = "Cell Type",
       y = "Log Fold Change",
       caption = "Data Source: In-House Dataset")

print(p)

#---- In house epi
df  <- read.csv('C:\\Users\\crtuser\\OneDrive - TCDUD.onmicrosoft.com\\Documents\\PhD\\Project\\Writing\\Figures\\WLvsHFD_bycell_inhousepi.csv')

# Filter the dataframe to keep only rows where 'Tnfaip3' is found in the X column
df <- df[grepl("Tnfaip3", df$X, ignore.case = TRUE), ]

# The rest of your transformations
df$Cell.Type <- df$Cluster
df$Log.Fold.Change <- df$avg_log2FC
df$p.value.adj <- df$p_val_adj
df$Significance <- ifelse(df$p.value.adj < 0.001, '***',
                          ifelse(df$p.value.adj < 0.01, '**',
                                 ifelse(df$p.value.adj < 0.05, '*', '')))

# Reorder Cell.Type based on Log.Fold.Change
df$Cell.Type <- reorder(df$Cell.Type, df$Log.Fold.Change)

# Create the bar plot with dynamic fill color based on Log.Fold.Change
p1 <- ggplot(df, aes(x = Cell.Type, y = Log.Fold.Change, fill = ifelse(Log.Fold.Change < 0, "Negative", "Positive"))) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = Significance), vjust = -0.3, color = "black", size = 5) +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Positive" = "steelblue")) +  # Red for negative, steelblue for positive
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "none",  # This line removes the legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TNFAIP3 Expression - After WL",
       x = "Cell Type",
       y = "Log Fold Change",
       caption = "Data Source: In-House Dataset")

print(p1)

#---- Epi Dataset
library(ggplot2)

# Assuming df is your original dataframe

# Filter the dataframe for rows where 'Tnfaip3' appears in the X column
tnfaip3_df <- df[grepl("Tnfaip3", df$X, ignore.case = TRUE), ]

# Reorder Cell.Type in the filtered dataframe based on Log.Fold.Change
tnfaip3_df$Cell.Type <- reorder(tnfaip3_df$Cell.Type, tnfaip3_df$Log.Fold.Change)

# Create the bar plot with the filtered dataframe
p <- ggplot(tnfaip3_df, aes(x = Cell.Type, y = Log.Fold.Change, fill = ifelse(Log.Fold.Change < 0, "Negative", "Positive"))) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = Significance), vjust = -0.3, color = "black", size = 5) +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Positive" = "steelblue")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TNFAIP3 Expression - After WL",
       x = "Cell Type",
       y = "Log Fold Change",
       caption = "Data Source: In-House Dataset")

print(p)

# ---- Caloric Restriction Dataset
df <- read.csv('C:\\Users\\crtuser\\OneDrive - TCDUD.onmicrosoft.com\\Documents\\PhD\\Project\\Writing\\Figures\\cr_epi.csv')

df$Cell.Type <- df$Cluster
df$Log.Fold.Change <- df$avg_log2FC
df$p.value.adj <- df$p_val

# Add a new column for significance level based on p.value.adj
df$Significance <- ifelse(df$p.value.adj < 0.001, '***',
                          ifelse(df$p.value.adj < 0.01, '**',
                                 ifelse(df$p.value.adj < 0.05, '*', '')))


# Filter the dataframe for rows where 'Tnfaip3' appears in the X column
tnfaip3_df <- df[grepl("Tnfaip3", df$X, ignore.case = TRUE), ]

# Reorder Cell.Type in the filtered dataframe based on Log.Fold.Change
tnfaip3_df$Cell.Type <- reorder(tnfaip3_df$Cell.Type, tnfaip3_df$Log.Fold.Change)

# Create the bar plot with the filtered dataframe
p1 <- ggplot(tnfaip3_df, aes(x = Cell.Type, y = Log.Fold.Change, fill = ifelse(Log.Fold.Change < 0, "Negative", "Positive"))) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = Significance), vjust = -0.3, color = "black", size = 5) +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Positive" = "steelblue")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TNFAIP3 Expression - After WL",
       x = "Cell Type",
       y = "Log Fold Change",
       caption = "Data Source: In-House Dataset")

print(p1)


#---- Hasty
df <- read.csv('C:\\Users\\crtuser\\OneDrive - TCDUD.onmicrosoft.com\\Documents\\PhD\\Project\\Writing\\Figures\\hasty_WLvsHFD_TNFAIP3.csv')


df$Cell.Type <- df$Cluster
df$Log.Fold.Change <- df$avg_log2FC
df$p.value.adj <- df$p_val_adj

# Add a new column for significance level based on p.value.adj
df$Significance <- ifelse(df$p.value.adj < 0.001, '***',
                          ifelse(df$p.value.adj < 0.01, '**',
                                 ifelse(df$p.value.adj < 0.05, '*', '')))


# Filter the dataframe for rows where 'Tnfaip3' appears in the X column
tnfaip3_df <- df[grepl("Tnfaip3", df$X, ignore.case = TRUE), ]

# Reorder Cell.Type in the filtered dataframe based on Log.Fold.Change
tnfaip3_df$Cell.Type <- reorder(tnfaip3_df$Cell.Type, tnfaip3_df$Log.Fold.Change)

# Create the bar plot with the filtered dataframe
p2 <- ggplot(tnfaip3_df, aes(x = Cell.Type, y = Log.Fold.Change, fill = ifelse(Log.Fold.Change < 0, "Negative", "Positive"))) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = Significance), vjust = -0.3, color = "black", size = 5) +
  scale_fill_manual(values = c("Negative" = "#FF0000", "Positive" = "steelblue")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TNFAIP3 Expression - After WL",
       x = "Cell Type",
       y = "Log Fold Change",
       caption = "Data Source: In-House Dataset")

print(p2)

#---- Patchwork
# Install and load the patchwork package
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(patchwork)

# Assuming p, p1, and p2 are your ggplot objects
combined_plot <- p / p1 / p2 

# To ensure that all plots are of the same size
combined_plot <- combined_plot & 
  plot_layout(heights = c(1, 1, 1))  # Adjust the ratio if you want different relative heights

# Print the combined plot
print(combined_plot)

combined_plot <- p / p1 / p2  # Your combined plot

# Save the plot with custom dimensions
ggsave("C:\\Users\\crtuser\\OneDrive - TCDUD.onmicrosoft.com\\Documents\\PhD\\Project\\Writing\\Figures\\combined_plot.png", plot = combined_plot, width = 8, height = 10, dpi = 300)


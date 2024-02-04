# Load the magick library
library(magick)

# Specify the directory containing the images
image_directory <- "C://Users//crtuser//OneDrive - TCDUD.onmicrosoft.com//Documents//PhD//Project//Writing//Figures//Flow Raw Plots"

# Define the names of the specific images to combine
image_names <- c('histo_cd4.png', 'histo_cd8s.png', 'histo_gds.png', 'histo_nks.png')

# Construct the full paths to the images
image_paths <- file.path(image_directory, image_names)

# Read the images into a list
images <- lapply(image_paths, image_read)

# Combine the images into a single image, arranging them in 4 rows and 1 column
combined_image <- image_montage(do.call(c, images), tile = "1x4", geometry = "x400") # Adjust '4x1' and 'x400' as needed

# Specify the output file path
output_file_path <- file.path(image_directory, "combined_histo_plot.png")

# Save the combined image
image_write(combined_image, path = output_file_path, quality = 100, format = 'png')

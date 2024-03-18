library(tidyverse)
library(ggrepel)

subset_paths <- function(file_path) {
  # Normalize the file path to handle special characters and ensure correct directory extraction
  normalized_path <- normalizePath(file_path)
  
  # Extract the directory path and file name without extension
  dir_path <- dirname(normalized_path)  # Extract one level up from the normalized file path
  file_name <- basename(normalized_path)
  plot_title <- substr(file_name, 1, nchar(file_name) - nchar(file_ext(file_name)))
  
  # Read data from the specified file
  data <- read.delim(file_path, header = TRUE)
  
  # Filter rows and mutate columns
  data <- data %>%
    filter(str_detect(ID, "HALLMARK.*$")) %>%
    mutate(log_pval = -log10(p.adjust),
           change = case_when(NES < 0 ~ "downregulated",
                              NES > 0 ~ "upregulated",
                              TRUE ~ "stable"))

  # Create the plot using ggplot2
  plot <- ggplot(
    data = data,
    mapping = aes(x = NES, y = log_pval, label = ID, color = NES)
  ) +
    geom_point(size = 0.5) +
    geom_text_repel(
      max.overlaps = 100,
      size = 1.75,
      direction = "y",
      nudge_x = -1,
      nudge_y = 1
    ) +
    labs(
      x = "NES",
      y = "-log10(p.adjust)",
      title = paste(plot_title)
    ) +
    scale_color_gradient2(
      name = "Regulation",
      high = "red",
      mid = "darkgray",
      low = "blue",
      limits = c(-3,3),
      breaks = c(3,1.5,0,-1.5,-3),
      labels = c(">3 (Highly Upregulated)", "1.5", "0", "-1.5", "<-3 (Highly Downregulated)")
    ) +
    theme(
      #plot.background = element_rect(fill = "darkgray"),  
      panel.background = element_rect(fill = "white"), 
      panel.grid = element_line(color = "black")
    )
  
  # Construct the file path for saving the plot inside the directory of the table
  plot_file_path <- file.path(dir_path, paste0(plot_title, "_plot.pdf"))
  
  # Save the plot
  ggsave(plot_file_path, plot, width = 10, height = 8)
  
  return(plot)  # Return the plot object
}


#
#
#TET2 Mutated ATF4, EIF2AL3, IL1A, IL6
#
#
#

#TET2 Mutated + ATF4
TET2_file1 <- file.choose()
subset_paths(TET2_file1)

#TET2 Mutated + EIF2AK3
TET2_file2a <- file.choose()
subset_paths(TET2_file2a)
#No Tet2 Mutated + EIF2AK3
TET2_file2b <- file.choose()
subset_paths(TET2_file2b)

#TET2 Mutated + IL6
TET2_file3a <- file.choose()
subset_paths(TET2_file3a)
#No Tet2 Mutated + IL6
TET2_file3b <- file.choose()
subset_paths(TET2_file3b)

#TET2 Mutated + IL1A
TET2_file4 <- file.choose()
subset_paths(TET2_file4)


#
#
#
#ASXL1 Mutated ATF4, EIF2AL3, IL1A, IL6
#
#
#

#ASXL1 Mutated + ATF4
ASXL1_file1 <- file.choose()
subset_paths(ASXL1_file1)

#ASXL1 Mutated + EIF2AK3
ASXL1_file2a <- file.choose()
subset_paths(ASXL1_file2a)
#No ASXL1 Mutated + EIF2AK3
ASXL1_file2b <- file.choose()
  subset_paths(ASXL1_file2b)

#ASXL1 Mutated + IL6
ASXL1_file3a <- file.choose()
  subset_paths(ASXL1_file3a)
#No ASXL1 Mutated + I:6
ASXL1_file3b <- file.choose()
  subset_paths(ASXL1_file3b)

#ASXL1 Mutated + IL1A
ASXL1_file4a <- file.choose()
  subset_paths(ASXL1_file4a)
#No ASXL1 Mutated + IL1A
ASXL1_file4b <- file.choose()
  subset_paths(ASXL1_file4b)


#
#
#
#ASXL1 + TET2 Mutated ATF4, EIF2AL3, IL1A, IL6
#
#
#

#ASXL1 + TET2 Mutated ATF4
ASXL1_TET2_file1 <- file.choose()
  subset_paths(ASXL1_TET2_file1)

#No ASXL1 + TET2 Mutated EIF2AK3
ASXL1_TET2_file2 <- file.choose()
  subset_paths(ASXL1_TET2_file2)

#No ASXL1 + TET2 Mutated IL6
ASXL1_TET2_file2 <- file.choose()
  subset_paths(ASXL1_TET2_file2)

#No ASXL1 + TET2 Mutated IL1A
ASXL1_TET2_file2 <- file.choose()
  subset_paths(ASXL1_TET2_file2)

#
#
# Positive Controls
#TET2 Mutated TET2
#ASXL1 Mutated TET2
#ASXL1 + TET2 Mutated TET2
#
#
#

#TET2 Mutated TET2
TET2_TET2 <- file.choose()
  subset_paths(TET2_TET2)
#ASXL1 Mutated TET2
ASXL1_TET2 <- file.choose()
  subset_paths(ASXL1_TET2)

#ASXL1 + TET2 Mutated TET2
ASXL1_TET2_TET2 <- file.choose()
  subset_paths(ASXL1_TET2_TET2)









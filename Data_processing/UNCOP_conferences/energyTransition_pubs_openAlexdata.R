######### open alex data 
########################

library(tidyverse)

file.choose()
opena <- read_csv("/Users/camilalivio/Desktop/energy_transition_OA.csv")
colnames(opena)


opena <- opena[-1, ]
opena <- opena[, colnames(opena) != "NA_NA"]
colnames(opena)

new_column_names <- c("publication_year", 
                      "publication_year_count", 
                      "NA", 
                      "open_access", 
                      "open_access_count", 
                      "NA", 
                      "primary_topic", 
                      "primary_topic_count", 
                      "NA", 
                      "institutions", 
                      "institutions_count", 
                      "NA", 
                      "publication_type", 
                      "publication_type_count", 
                      "NA", 
                      "Authorships_Countries", 
                      "authorship_countries_count", 
                      "NA")
colnames(opena) <- new_column_names
colnames(opena)
opena <- opena[, colnames(opena) != "NA"]


file.choose()
countPubs <- read_csv("/Users/camilalivio/Desktop/countPubs_energyTransition.csv")
countPubs <- countPubs %>% select(-...3)

#### 2022 is missing
new_row <- data.frame(Year = 2022, count_of_pubs = 22370)
countPubs <- rbind(countPubs, new_row)
countPubs <- countPubs[order(countPubs$Year), ]
countPubs <- countPubs[!(countPubs$Year == 2025 & countPubs$count_of_pubs == 1946), ]
print(countPubs)

# Load required library
library(ggplot2)
library(dplyr)

# Load ggplot2 package if you haven't already
library(ggplot2)

# Filter the data for years from 1950 onwards
countPubs_filtered <- countPubs[countPubs$Year >= 1950, ]

# Create the line plot
ggplot(countPubs_filtered, aes(x = Year, y = count_of_pubs)) +
  geom_line(color = "purple", size = 1) +  # Line plot with color and thickness
  labs(title = "Count of Publications Containing the Term 'Energy Transition' in the Title and Abstract",
       subtitle = "Data source: OpenAlex.org",
       x = "Year", 
       y = "Count of Publications") +
  theme_get()  

library(ggplot2)

ggplot(countPubs_filtered, aes(x = Year, y = count_of_pubs)) +
  geom_line(color = "#6A0DAD", size = 1.5) +  # Use a vibrant purple shade and thicker line
  geom_point(color = "#6A0DAD", size = 3) +  # Add points to highlight data
  labs(
    title = "Count of Publications Containing the Term 'Energy Transition' in the Title and Abstract",
    subtitle = "Data source: OpenAlex.org",
    x = "Year", 
    y = "Count of Publications"
  ) +
  theme_minimal(base_size = 14) +  # Cleaner theme and slightly larger base font size
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#3B3B3B"),  # Bold, larger title
    plot.subtitle = element_text(size = 12, color = "#6A6A6A"),  # Smaller subtitle, muted color
    axis.title = element_text(size = 14, face = "bold", color = "#3B3B3B"),  # Bold axis titles
    axis.text = element_text(size = 12, color = "#3B3B3B"),  # Axis text with a clean, readable font
    panel.grid.major = element_line(color = "#E3E3E3"),  # Lighter major gridlines
    panel.grid.minor = element_line(color = "#F5F5F5"),  # Even lighter minor gridlines
    panel.background = element_rect(fill = "#FAFAFA"),  # Light background for the panel
    plot.background = element_rect(fill = "#FFFFFF")  # White background for the plot area
  )

library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggrepel)

# Ensure the Year and count_of_pubs are numeric
countPubs_filtered$Year <- as.numeric(countPubs_filtered$Year)
countPubs_filtered$count_of_pubs <- as.numeric(countPubs_filtered$count_of_pubs)

# Define the years where you want the labels
label_years <- c(1950, 1975, 2000, 2024)

# Filter the data to only include these years
label_data <- countPubs_filtered[countPubs_filtered$Year %in% label_years, ]

# Create the plot with geom_label_repel() to avoid label overlap
##### this one works, but the one below it looks better
ggplot(countPubs_filtered, aes(x = Year, y = count_of_pubs)) +
  geom_line(color = "#6A0DAD", size = 1.3) +  # Line plot with thicker line
  geom_label_repel(data = label_data, 
                   aes(x = Year, y = count_of_pubs, label = count_of_pubs), 
                   box.padding = 0.35, 
                   point.padding = 0.5,
                   max.overlaps = Inf,  # To show all labels without cutting off
                   fill = "white", 
                   color = "#6A0DAD", 
                   size = 4, 
                   fontface = "bold", 
                   nudge_y = 50) +  # Nudging the labels a bit for clarity
  labs(
    title = "Count of Publications Containing 'Energy Transition' in Their Title and Abstract",
    subtitle = "Data source: OpenAlex.org",
    x = "Year", 
    y = "Count of Publications"
  ) +
  theme_get()

library(ggplot2)
library(ggrepel)

# Define the years where you want the labels (including the new years)
years <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)

# Ensure the Year and count_of_pubs are numeric
countPubs_filtered$Year <- as.numeric(countPubs_filtered$Year)
countPubs_filtered$count_of_pubs <- as.numeric(countPubs_filtered$count_of_pubs)

# Define the years where you want the labels
label_years <- c(1950, 1975, 2000, 2025, 2024)

# Filter the data to only include these years
label_data <- countPubs_filtered[countPubs_filtered$Year %in% label_years, ]

# Create the plot with geom_label_repel() to avoid label overlap
ggplot(countPubs_filtered, aes(x = Year, y = count_of_pubs)) +
  geom_line(color = "#01796F", size = 1.5) +  # Line plot with thicker line
  geom_label_repel(data = label_data, 
                   aes(x = Year, y = count_of_pubs, label = count_of_pubs), 
                   box.padding = 0.35, 
                   point.padding = 0.5,
                   max.overlaps = Inf,  # To show all labels without cutting off
                   fill = "white", 
                   color = "#01796F", 
                   size = 4, 
                   fontface = "bold", 
                   nudge_y = 50) +  # Nudging the labels a bit for clarity
  scale_x_continuous(breaks = years) +  # Set the x-axis breaks
  labs(
    title = "Count of Publications Containing 'Energy Transition' in Their Title and Abstract",
    subtitle = "Data source: OpenAlex.org",
    x = "Year", 
    y = "Count of Publications"
  ) +
  theme_get()


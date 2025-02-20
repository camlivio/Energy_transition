#########
#### Plotting milestones
#########

library(ggplot2)
library(dplyr)
library(stringr)

# Adjust position dynamically based on text length
energy_transition_timeline <- energy_transition_timeline %>%
  mutate(milestone_wrapped = str_wrap(milestone, width = 40),  # Wrap text to 40 characters per line
         text_length = str_length(milestone),  
         pos = ifelse(row_number() %% 2 == 0, text_length * 0.02, -text_length * 0.02))

# Create the improved timeline plot
ggplot(energy_transition_timeline, aes(x = time, y = 0)) +
  geom_hline(yintercept = 0, color = "#01796F", linewidth = 1.5) +  # Timeline base
  geom_point(size = 5, color = "#01796F") +  # Timeline event points
  geom_label(aes(y = pos, label = milestone_wrapped),  # Use wrapped text
             hjust = ifelse(energy_transition_timeline$pos > 0, 0, 1), 
             vjust = 0.5, 
             size = 5, 
             fill = "lightgrey", 
             label.size = 0.5, 
             color = "black") +  
  geom_segment(aes(y = 0, yend = pos * 0.9, xend = time), color = "#01796F") +  # Connecting lines
  theme_minimal() +
  labs(title = "Energy Transitions Timeline", x = "", y = "", subtitle = "Timeline based on Araújo 2023:3-5") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text()) +
  ylim(-max(abs(energy_transition_timeline$pos)) * 1.2, max(abs(energy_transition_timeline$pos)) * 1.2)  # Dynamically adjust ylim

########################
##### vertical graph?
########################

library(ggplot2)
library(dplyr)
library(stringr)

# Adjust position dynamically based on text length
energy_transition_timeline <- energy_transition_timeline %>%
  mutate(milestone_wrapped = str_wrap(milestone, width = 40),  # Wrap text to 40 characters per line
         text_length = str_length(milestone),  
         pos = ifelse(row_number() %% 2 == 0, text_length * 0.02, -text_length * 0.02))

# Create the vertical timeline plot
ggplot(energy_transition_timeline, aes(y = time, x = 0)) +  # Swap x and y
  geom_vline(xintercept = 0, color = "#01796F", linewidth = 1.5) +  # Vertical timeline
  geom_point(size = 4, color = "#01796F") +  # Timeline event points
  geom_label(aes(x = pos, label = milestone_wrapped),  # Use wrapped text
             hjust = 0.5, 
             vjust = ifelse(energy_transition_timeline$pos > 0, 0, 1),  # Adjust vertical positioning
             size = 4, 
             fill = "lightgrey", 
             label.size = 0.4, 
             color = "black") +  
  geom_segment(aes(x = 0, xend = pos * 0.9, yend = time), color = "#01796F") +  # Connecting lines
  theme_minimal() +
  labs(title = "Energy Transitions Timeline", x = "", y = "", subtitle = "Timeline based on Araújo 2023:3-5") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 10),  # Rotate timeline labels for readability
        plot.title = element_text()) +
  xlim(-max(abs(energy_transition_timeline$pos)) * 1.2, max(abs(energy_transition_timeline$pos)) * 1.2)  # Dynamically adjust xlim

############
ggplot(data = energy_transition_timeline, aes(x = time, y = milestone)) +
  geom_point(size = 4, color = '#01796F') +
  geom_text(aes(label = milestone), nudge_y = 0.2, size = 5, color = 'black') +
  theme_minimal() +
  labs(title = "Milestones Over Time", x = "Year", y = "Milestone") +
  theme(axis.text.y = element_blank())  # Removes the y-axis text

library(stringr)

ggplot(data = energy_transition_timeline, aes(x = time, y = milestone)) +
  geom_point(size = 4, color = 'grey') +
  geom_text(aes(label = str_wrap(milestone, width = 20)), nudge_y = 0.2, size = 5, color = 'black') +  # Wrap text
  theme_get() +
  labs(title = "Milestones Over Time", x = "Year", y = "Milestone") +
  theme(axis.text.y = element_blank())  # Removes the y-axis text

library(ggplot2)
library(stringr)

ggplot(data = energy_transition_timeline, aes(x = time, y = milestone)) +
  geom_point(size = 5, color = '#FF5733', alpha = 0.2) +  # Fading the points with alpha
  geom_text(aes(label = str_wrap(milestone, width = 20)), 
            nudge_y = 0.2, size = 5, color = '#01796F') +  # Text with more emphasis
  geom_line(aes(group = 1), color = '#3498db', size = 1.2, linetype = 'solid', alpha = 0.2) +  # Fading the line
  theme_minimal() +
  labs(title = "Energy Transition Milestones Over Time",
       subtitle = "Timeline based on Araújo 2023: 3-5",
       x = "", 
       y = "") +
  theme(axis.text.y = element_blank(),  # Removes the y-axis text
        axis.title.x = element_text(size = 14, face = 'bold', color = '#333333'),
        axis.title.y = element_text(size = 14, face = 'bold', color = '#333333'),
        plot.title = element_text(size = 16, face = 'bold', color = 'black'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'lightgray', size = 0.5),
        panel.grid.minor = element_blank())  # Subtle grid lines for clarity

#####
library(ggplot2)
library(stringr)

ggplot(data = energy_transition_timeline, aes(x = time, y = milestone)) +
  geom_point(size = 5, color = '#FF5733', alpha = 0.6) +  # Points with transparency
  geom_text(aes(label = str_wrap(milestone, width = 20)), 
            nudge_y = 0.2, size = 5, color = '#01796F') +  # Labels near points
  theme_minimal() +
  labs(title = "Energy Transition Milestones Over Time",
       subtitle = "Timeline based on Araújo 2023: 3-5",
       x = "", 
       y = "") +
  theme(axis.text.y = element_blank(),  # Removes y-axis text
        axis.title.x = element_text(size = 15, face = 'bold', color = '#333333'),
        axis.title.y = element_text(size = 14, face = 'bold', color = '#333333'),
        plot.title = element_text(size = 16, face = 'bold', color = 'black'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'lightgray', size = 0.5),
        panel.grid.minor = element_blank())  # Subtle grid lines for clarity

ggplot(data = energy_transition_timeline, aes(x = time, y = milestone)) +
  geom_point(size = 5, color = '#FF5733', alpha = 0.2) +  # Points with transparency
  geom_text(aes(label = str_wrap(milestone, width = 20)), 
            nudge_y = 0.2, size = 5, color = '#01796F') +  # Labels near points
  theme_minimal() +
  labs(title = "Energy Transition Milestones Over Time",
       subtitle = "Timeline based on Araújo 2023: 3-5",
       x = "", 
       y = "") +
  theme(axis.text.y = element_blank(),  # Removes y-axis text
        axis.text.x = element_text(size = 15, color = '#333333'),  # Larger x-axis text
        axis.title.x = element_text(size = 14, face = 'bold', color = '#333333'),
        axis.title.y = element_text(size = 14, face = 'bold', color = '#333333'),
        plot.title = element_text(size = 16, face = 'bold', color = 'black'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'lightgray', size = 0.1),  # Faded grid lines
        panel.grid.minor = element_blank())  # Subtle grid lines for clarity

##########
# Load necessary libraries
library(ggplot2)

# Create the data
data <- data.frame(
  year = c(2005, 2016),
  countries_with_targets = c(43, 176)
)

# Create the plot
ggplot(data, aes(x = factor(year), y = countries_with_targets, fill = factor(year))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Countries with Renewable Energy Targets",
    subtitle = "Comparison between 2005 and 2016",
    x = "Year",
    y = "Number of Countries",
    fill = "Year"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#3498db", "#3498db"))

# Create the plot with geom_label to add numbers on top of the bars
ggplot(data, aes(x = factor(year), y = countries_with_targets)) +
  geom_bar(stat = "identity", fill = "#01796F" ) +
  geom_label(aes(label = countries_with_targets), vjust = 0.1, size = 4) +  # Add numbers above bars
  labs(
    title = "Number of Countries with Renewable Energy Targets",
    subtitle = "Data source: Araújo 2017: 2",
    x = "Year",
    y = "Number of Countries",
    fill = "Year" 
  ) +
  theme_get() +
  theme(plot.title = element_text(face = "bold")) 

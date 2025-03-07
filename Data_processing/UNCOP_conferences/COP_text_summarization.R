library(mall)
library(dplyr)

llm_use("ollama", "llama3.2", seed = 100, temperature = 0)

#### I will refer to the data that I previously extracted from the PDFs and cleaned as pdf_data 
file.choose()

pdf_data <- read_csv("/Users/yourpathtofile/cop_summaries.csv")

##### Here's our main problem: the 'Text' column is confusing and lengthy. One way to make sense of the content is to use the summarization function from the {mall} package to handle it for us. I'm adding a new column for the summarized text and have set it to return 200 words.

###### Let's pass the 'CleanedText' data instead of 'Text' and add a prompt
cop_summaries <- llm_summarize( 
  pdf_data, 
  CleanedText, 
  max_words = 200, 
  pred_name = "cop_summary",
  additional_prompt = "Summarize the key decisions on energy transition from the Conference of the Parties reports, focusing on commitments, policies, and recommendations." 
) 

###### Let's check some outputs
print(cop_summaries$cop_summary[13])
print(cop_summaries$cop_summary[10])

install.packages("openxlsx")
library(openxlsx)

###### Save the data to disk
write.csv(cop_summaries, "path/cop_summaries.csv", row.names = FALSE)

####### now that we have the summaries, let's try mall's text extraction feature

cop_summaries <- data

cop_summaries <- llm_extract(cop_summaries, cop_summary, c("energy transition"), pred_name = "energy_transition", expand_cols = TRUE) 

cop_summaries <- llm_extract(cop_summaries, cop_summary, c("policy"), pred_name = "policy", expand_cols = TRUE) 

cop_summaries <- llm_extract(cop_summaries, cop_summary, c("energy security"), pred_name = "energy_security", expand_cols = TRUE) 

cop_summaries <- llm_extract(cop_summaries, cop_summary, c("renewable energy"), pred_name = "renewable_energy", expand_cols = TRUE) 

cop_summaries <- llm_extract(cop_summaries, CleanedText, c("renewable energy"), pred_name = "renewable_energy2", expand_cols = TRUE) 

write.csv(cop_summaries, "/Users/camilalivio/Desktop/cop_summaries.csv", row.names = FALSE)

#### Inspecting the data, I noticed that there are many references to 'solar energy'. Because the dataset in cop_summaries underwent some cleaning before being fed into llm_extract(), the model picks up subtle variations or word forms that you might not have anticipated. For instance, a phrase like "solar technology" could be implicitly linked to "solar energy," even though it's not an exact match.

###### Now let's do some transformations to plot it 
library(dplyr)

cop_summaries <- cop_summaries %>%
  mutate(COP_City = paste0(sub("_.*", "", Document), "_COP", sub(".*_(\\d+).pdf", "\\1", Document)))

####### now let's plot
library(dplyr)
library(ggplot2)
library(tidyr)


### Create a binary matrix: 1 if energy_transition is present, 0 if not
heatmap_data <- cop_summaries %>%
  distinct(COP_City, energy_transition) %>%
  mutate(Present = 1) %>%
  spread(energy_transition, Present, fill = 0)

### Step 1: Reshape the data for ggplot
heatmap_data_long <- heatmap_data %>%
  gather(key = "energy_transition", value = "Present", -COP_City)

### Ordering COP by number
heatmap_data_long <- heatmap_data_long %>%
  mutate(
    COP_number = as.numeric(gsub(".*_COP([0-9]+)", "\\1", COP_City)),  # Extract COP number
    City = gsub("_COP[0-9]+", "", COP_City)  # Extract city name
  )

### Step 2: Create a new factor for COP_City with levels ordered by COP_number
heatmap_data_long$COP_City <- with(heatmap_data_long, 
                                   factor(COP_City, 
                                          levels = unique(COP_City[order(COP_number)])))  
### Step 3: Create the heatmap plot
energy_transition <- ggplot(heatmap_data_long, aes(x = COP_City, y = energy_transition, fill = factor(Present))) +
  geom_tile(color = "black", size = 0.2) +  # White borders for the tiles to make them stand out
  scale_fill_manual(
    values = c("0" = "lightgray", "1" = "#09716F")  # Gray for absence, Salmon for presence
  ) +
  labs(
    title = "Automatic Extraction of Terms Related to *Energy Transition*",
    subtitle="by COP event",
    x = "COP Event (City)",
    y = "Term = Energy Transition",
    fill = "Presence"
  ) +
  theme_minimal(base_size = 14) +  # Larger base size for easy reading
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),  # Rotate and size the x-axis labels
    axis.text.y = element_text(size = 15),  # Size the y-axis labels
    plot.title = element_text(size = 16, face = "bold"),  # Title customization
    panel.grid = element_blank(),  # Remove gridlines for a cleaner look
    legend.position = "top",  # Position the legend at the top
    legend.key.size = unit(1, "cm")  # Adjust the size of the legend keys
  )
energy_transition

### Modify the above code to plot other variables 

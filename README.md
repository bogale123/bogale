packages ----------------------------------------------------------------
  
library(tidyverse)
library(openxlsx)
library(inti) 
library(shiny)
library(tibble)
library(strex)
library(summarytools) 
library(readxl)
library(officer)
library(flextable)
library(cowplot)

# Remove the XML package
remove.packages("XML")

# Install the XML package again
install.packages("XML")

data1 <- read_excel("16.06.2023_maindata_20.05.22_WMEg.xlsx", sheet = "WithMEg")
data2 <- data1 %>% mutate(Breedname = case_when(Breed == "Arsi" ~ "ar",
                             Breed == "Holestien" ~ "HF",
                             Breed == "1/2Fresian x 1/2Boran" ~ "HFbor",
                             Breed == "½ Fresin x ½ Arsi"~ "HFar",
                             Breed == "Boran" ~  "bor",
                             Breed == "Horo" ~ "hor",
                             Breed == "1/2Fresian x 1/2Horo" ~ "HFhor",
                             Breed == "Jersey" ~ "jer",
                             Breed == "1/2Jersey x 1/2Boran" ~ "jerbor"))
# data importing ----------------------------------------------------------
# create columns
data2 <- data1  %>%  
  select(No,ANIMAL.ID , Breed, Breedtyp, Sex, Farms,
         MEI, MF, MP, Age, LW, MYO, GS,DP, LL, ADG, VDMI, Mediet) %>%  
  mutate(Breedname = case_when(Breed == "Arsi" ~ "ar",
                               Breed == "Holestien" ~ "HF",
                               Breed == "1/2Fresian x 1/2Boran" ~ "HFbor",
                               Breed == "½ Fresin x ½ Arsi"~ "HFar",
                               Breed == "Boran" ~  "bor",
                               Breed == "Horo" ~ "hor",
                               Breed == "1/2Fresian x 1/2Horo" ~ "HFhor",
                               Breed == "Jersey" ~ "jer",
                               Breed == "1/2Jersey x 1/2Boran" ~ "jerbor")) %>%
  mutate(MEI = Mediet * VDMI)%>%
  mutate(MEa1 = 0.54 * 0.000002259 * LW * 1500) %>% #INRA (Tedeschi et al 2010),MEactivity = 0.54cal/kgBW/horizontal m and 1500m horizontal distance as suggested by Prof Uta
  mutate(MEa2 = 0) %>% # with activity allowance 
  mutate(MEa3 = 1) %>% # Bateki and Dickhoefer, 2019
  mutate(MEc = case_when(DP > 0 ~ 0.044*exp(0.0165 * DP) / 0.2, 
                         DP == 0 ~ 0)) %>% 
  mutate(MEinmilk =((0.95 + (0.38*MF) +(0.21 * MP))/0.6)) %>% 
  mutate(MEg = case_when(Breedtyp == "L"~ ADG * 32.5, 
                         Breedtyp == "Ex"~ ADG * 24.3, 
                         Breedtyp== "Cr"~ ADG * 24.3))%>% 
  # for all breeds
  mutate(MEm1 = 0.529 * LW^0.75) %>% # salah as refered by prof uta  calorimetric measurements
  mutate(MEm2 = 0.631 * LW^0.75)%>% # Salah 
  mutate(MEm3 = 0.43 * LW^0.75)%>%
  mutate(MEm4 = case_when(Breedtyp == "L"~ 0.631 * LW^0.75, # Salah, 
                          Breedtyp == "Ex" ~ 0.631 * LW^0.75, # Salah  
                          Breedtyp == "Cr"~ 0.583 * LW^0.75)) %>%  # Kelly
  mutate(MEm5 = case_when(Breedtyp == "L"~ 0.43 * LW^0.75,# Susenbeth
                          Breedtyp == "Ex" ~ 0.631 * LW^0.75, # Salah  
                          Breedtyp == "Cr"~ 0.43 * LW^0.75)) %>%  # Susenbeth
  # With activity allowance (MEa) added
  mutate(MEm6 = 0.631 * LW^0.75)%>%  # for all breeds Salah et al 2014
  mutate(MEm7 = case_when(Breedtyp =="L" ~ 0.43 * LW^0.75, # Susenbeth
                          Breedtyp == "Ex" ~ 0.631 * LW^0.75, # Salah  
                          Breedtyp == "Cr" ~ 0.43 * LW^0.75)) %>%  # Susenbeth
  mutate(MEm8 = 0.631 * LW^0.75) %>%  # for all breeds Salah et al 2014 )
  mutate(MEm9 =case_when(Breedtyp =="L" ~ 0.43 * LW^0.75, # Susenbeth
                           Breedtyp == "Ex" ~ 0.631 * LW^0.75, # Salah  
                           Breedtyp == "Cr" ~ 0.43 * LW^0.75)) %>%  # Susenbeth   
  mutate(MEtot1 = MEm1 + MEg + MEc + MEa2)%>% # for all breeds without activity MEa1 = 0
  mutate(MEtot2 = MEm2 + MEg + MEc + MEa2)%>% # for all breeds without activity MEa1 = 0
  mutate(MEtot3 = MEm3 + MEg + MEc + MEa2)%>% # for all breeds without activity MEa1 = 0
  mutate(MEtot4 = MEm4 + MEg + MEc + MEa2)%>% # Kelly for cross & Salah for L and Ex
  mutate(MEtot5 = MEm5 + MEg + MEc + MEa2)%>% # Susenbeth for Cr & L and Salah for Ex
  mutate(MEtot6 = MEm6 + MEg + MEc + MEa3)%>% # for all breeds Salah et al 2014 with activity
  mutate(MEtot7 = MEm7 + MEg + MEc + MEa3) %>%   # Susenbeth for Cr & L and Salah for Ex with activity  
  # with activity allowance  
  mutate(MEtot8 = MEm6 + MEg + MEc + MEa1)%>% # for all breeds Salah et al 2014 with activity
  mutate(MEtot9 = MEm7 + MEg + MEc + MEa1) %>%   # Susenbeth for Cr & L and Salah for Ex with activity
  
  mutate(PMY1=(MEI - MEtot1)/MEinmilk) %>% # to calculate the PMY of each combination
  mutate(PMY2=(MEI - MEtot2)/MEinmilk) %>%
  mutate(PMY3=(MEI - MEtot3)/MEinmilk) %>%
  mutate(PMY4=(MEI - MEtot4)/MEinmilk) %>%
  mutate(PMY5=(MEI - MEtot5)/MEinmilk) %>%
  mutate(PMY6=(MEI - MEtot6)/MEinmilk) %>%
  mutate(PMY7=(MEI - MEtot7)/MEinmilk) %>%
  mutate(PMY8=(MEI - MEtot8)/MEinmilk) %>%
  mutate(PMY9=(MEI - MEtot9)/MEinmilk) %>%
  mutate (MB1 =  MYO - PMY1)%>% 
  mutate (MB2 =  MYO - PMY2)%>% 
  mutate (MB3 =  MYO - PMY3)%>% 
  mutate (MB4 =  MYO - PMY4)%>% 
  mutate (MB5 =  MYO - PMY5)%>% 
  mutate (MB6 =  MYO - PMY6)%>% 
  mutate (MB7 =  MYO - PMY7) %>% 
  mutate (MB8 =  MYO - PMY8) %>% 
  mutate (MB9 =  MYO - PMY9) %>%
  mutate(FLuta = VDMI*1000/LW ^0.75) # to create for column for Prof Uta suggestion, FLuat)
data3e <- subset(data2,!(FLuta > 199.999)) 
data3f1 <- subset(data3e, !(Breedname %in% c("HF", "HFbor")))
data3e %>% web_table()
data3f1 %>% web_table()
# Mean Bias of <200gm VDMI ------------------------------------------------
data3g <- subset(data3f1,!(FLuta > 199.999)) %>% # based on Uta recommendation and with HF and HFbor excluded
  select(MB1,MB2,MB3,MB4,MB5,MB6,MB7,MB8, MB9, Breedname) %>% 
  group_by(Breedname) %>% 
  summarise_all(
    list(~ mean(., na.rm = TRUE)))
data3g %>% web_table()
# RMSE, root mean square error ---------------------------------------------
rmse_data <- data3f1 %>%
  select(Breedname, MYO, PMY1, PMY2, PMY3,PMY4,PMY5,PMY6,PMY7,PMY8,PMY9) %>% 
group_by(Breedname) %>% 
  summarise_all(
    list(~ mean(., na.rm = TRUE)))
rmse_data %>% web_table()
# Calculate the RMSE for each breed
rmse_data <- data3f1 %>%
  select(Breedname, MYO, PMY1, PMY2, PMY3, PMY4, PMY5, PMY6, PMY7, PMY8, PMY9) %>%
  group_by(Breedname) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

rmse1 <- rmse_data %>%
  group_by(Breedname) %>%
  summarise(
    RMSE_PMY1 = calculate_rmse(MYO, PMY1),
    RMSE_PMY2 = calculate_rmse(MYO, PMY2),
    RMSE_PMY3 = calculate_rmse(MYO, PMY3),
    RMSE_PMY4 = calculate_rmse(MYO, PMY4),
    RMSE_PMY5 = calculate_rmse(MYO, PMY5),
    RMSE_PMY6 = calculate_rmse(MYO, PMY6),
    RMSE_PMY7 = calculate_rmse(MYO, PMY7),
    RMSE_PMY8 = calculate_rmse(MYO, PMY8),
    RMSE_PMY9 = calculate_rmse(MYO, PMY9)
  ) 
rmse_data %>% web_table()
rmse1 %>% web_table()


# To draw a table for mean, sd 
library(openxlsx)

data4 <- subset(data3f1, !(FLuta > 199.999))

variables <- c("MYO", "PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9", "MEm1", "MEm2", "MEm3", "MEm4", "MEm5", "MEm6", "MEm7", "MEm8", "MEm9", "MEc", "MEa1", "MEa2","MEa3","MEg")

summary_list <- list()
for (var in variables) {
  summary_result <- summary(data4[[var]])
  sd_result <- sd(data4[[var]])
  
  summary_list[[var]] <- list(summary_result = as.vector(summary_result), sd_result = as.vector(sd_result))
}

# Create the summary table
summary_table <- data.frame()
for (var in variables) {
  row <- c(var, unlist(summary_list[[var]]$summary_result), unlist(summary_list[[var]]$sd_result))
  summary_table <- rbind(summary_table, row)
}

colnames(summary_table) <- c("Variable", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Std.Dev.")

# Save the summary table to an Excel file
write.xlsx(summary_table, "summary_table.xlsx", rowNames = FALSE)

# Display the summary table
web_table(summary_table)


library(openxlsx)
library(agricolae)

data4 <- subset(data3f1, !(FLuta > 199.999))

variables <- c("MYO", "PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9", "MEm1", "MEm2", "MEm3", "MEm4", "MEm5", "MEm6", "MEm7", "MEm8", "MEm9", "MEc", "MEa1", "MEa2", "MEa3", "MEg")

summary_list_actual <- list()
summary_list_predicted <- list()
p_value_list_actual <- list()
p_value_list_predicted <- list()
letter_rep_list_actual <- list()
letter_rep_list_predicted <- list()

for (var in variables) {
  summary_result_actual <- summary(data4[[var]])
  summary_result_predicted <- summary(data4[[paste0("Predicted_", var)]])
  sd_result_actual <- sd(data4[[var]])
  sd_result_predicted <- sd(data4[[paste0("Predicted_", var)]])
  
  summary_list_actual[[var]] <- list(summary_result = as.vector(summary_result_actual), sd_result = as.vector(sd_result_actual))
  summary_list_predicted[[var]] <- list(summary_result = as.vector(summary_result_predicted), sd_result = as.vector(sd_result_predicted))
  # Perform mean separation for actual values
  mean_sep_result_actual <- HSD.test(data4[[var]], trt = data4$Group)
  p_value_list_actual[[var]] <- mean_sep_result_actual$pvalue
  letter_rep_list_actual[[var]] <- mean_sep_result_actual$groups$letter
  
  # Perform mean separation for predicted values
  mean_sep_result_predicted <- HSD.test(data4[[paste0("Predicted_", var)]], trt = data4$Group)
  p_value_list_predicted[[var]] <- mean_sep_result_predicted$pvalue
  letter_rep_list_predicted[[var]] <- mean_sep_result_predicted$groups$letter
}

# Create the summary table for actual values
summary_table_actual <- data.frame()
for (var in variables) {
  row <- c(var, unlist(summary_list_actual[[var]]$summary_result), unlist(summary_list_actual[[var]]$sd_result))
  summary_table_actual <- rbind(summary_table_actual, row)
}

colnames(summary_table_actual) <- c("Variable", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Std.Dev.")

# Add p-values and letter representation for actual values to the summary table
for (i in seq_along(variables)) {
  p_value_row <- rep("", 8)
  p_value_row[5] <- p_value_list_actual[[i]]
  summary_table_actual <- rbind(summary_table_actual, p_value_row)
  
  letter_rep_row <- rep("", 8)
  letter_rep_row[5] <- paste0("(", letter_rep_list_actual[[i]], ")")
  summary_table_actual <- rbind(summary_table_actual, letter_rep_row)
}

# Create the summary table for predicted values
summary_table_predicted <- data.frame()
for (var in variables) {
  row <- c(paste0("Predicted_", var), unlist(summary_list_predicted[[var]]$summary_result), unlist(summary_list_predicted[[var]]$sd_result))
  summary_table_predicted <- rbind(summary_table_predicted, row)
}

colnames(summary_table_predicted) <- c("Variable", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Std.Dev.")


# Visualization for the data  ---------------------------------------------

# To draw a box plot  -----------------------------------------------------
# Box plot for observed milk yield between breeds
# Set font-*
font <- "Times New Roman"

# Create the plot for MYO
# Exclude the breeds "HF" and "HFbor" from the dataset

library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(officer)

# Subset data for breeds without non-finite values
data_subset <- subset(data3f1, !(FLuta > 199.999))
# Subset data for breeds without non-finite values
data_subset <- subset(data3f1, !(FLuta > 199.999))

# Define custom breed labels for the legend
breed_labels <- c("Arsi",
                  "½ Fresin x ½ Arsi" ,
                  "Boran " ,
                  "hor = horo " ,
                  "1/2Fresian x 1/2Horo",
                  "Jersey",
                  "1/2Jersey x 1/2Boran")

# Define specific breed names for x-axis labels
specific_breed_names <- c("Arsi",
                          "½ Fresin x ½ Arsi",
                          "Boran",
                          "horo",
                          "1/2Fresian x 1/2Horo",
                          "Jersey",
                          "1/2Jersey x 1/2Boran")

# Draw the box plot with legend
boxplot_plot <- ggplot(data_subset, aes(x = fct_reorder(Breedname, MYO, median), y = MYO, fill = Breedname)) +
  geom_boxplot() +
  labs(x = "Breeds", y = "Observed Milk Yield (kg/day, n = 126)") + scale_y_continuous(limits = c(0, 10.5)) + 
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 13),  # Adjust the legend text size here
        axis.text = element_text(size = 13),  # Adjust the axis text size here
        axis.title = element_text(size = 13),  # Adjust the axis title size here
        plot.title = element_text(size = 13))  # Adjust the plot title size here +
guides(fill = guide_legend(title = "Breed"))

# Modify the legend labels
boxplot_plot <- boxplot_plot +
  scale_fill_manual(values = c("Arsi" = "gray", 
                               "½ Fresin x ½ Arsi" = "gray",
                               "Boran" = "gray",
                               "horo" = "gray",
                               "1/2Fresian x 1/2Horo" = "gray",
                               "Jersey" = "gray",
                               "1/2Jersey x 1/2Boran"),
                    labels = breed_labels) +
  scale_x_discrete(labels = specific_breed_names)  # Map specific breed names to x-axis labels

# Save the plot as an image file
ggsave(filename = "figures/boxplots/figboxplt_allbreeds2.jpg",
       plot = boxplot_plot,
       width = 10,
       height = 8)

# Create a Word document
doc <- read_docx()

# Add the plot to the Word document
doc <- doc %>%
  body_add_par(value = "") %>%
  body_add_img(src = "figures/boxplots/figboxplt_allbreeds2.jpg", width = 10, height = 8) %>%
  body_add_par(value = "")

# Save the document as a Word file
print(doc, target = "figboxplt_allbreeds4.docx")


# Box plot for Predicted  milk yield between breeds

  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(cowplot)
  library(officer)
  
  # Subset data for breeds without non-finite values
  data_subset <- subset(data3f1, !(FLuta > 199.999))
  
  # Define specific breed names for y-axis labels
  specific_breed_names <- c("Arsi",
                            "½ Fresin x ½ Arsi",
                            "Boran",
                            "hor = horo",
                            "1/2Fresian x 1/2Horo",
                            "Jersey")
  
  # Draw the box plot
  boxplot_plot <- ggplot(data_subset, aes(x = PMY, y = Breedname, fill = Breedname)) +
    geom_boxplot() +
    labs(x = "Predicted Milk Yield Scenarios", y = "Breeds") +
    scale_x_continuous(breaks = 1:9, labels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9")) +
    scale_fill_manual(values = c("Arsi" = "gray", 
                                 "½ Fresin x ½ Arsi" = "gray",
                                 "Boran" = "gray",
                                 "horo" = "gray",
                                 "1/2Fresian x 1/2Horo" = "gray",
                                 "Jersey" = "gray"),
                      labels = specific_breed_names) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.text = element_text(size = 13),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 13)) +
    guides(fill = guide_legend(title = "Legend Title"))  # Change the legend title here
  
  # Save the plot as an image file (consider using PNG format for better quality)
  ggsave(filename = "figures/boxplots/figboxplt_PMY_breeds11.png",
         plot = boxplot_plot,
         width = 8,
         height = 10) %>% 
  
  # Create a Word document
  doc <- read_docx()
  
  # Add the plot to the Word document
  doc <- body_add_par(doc, value = "")
  doc <- body_add_img(doc, src = "figures/boxplots/figboxplt_PMY_breeds11.docx", width = 8, height = 10)
  doc <- body_add_par(doc, value = "")  # Add a paragraph between images
  
  # Save the document as a Word file
  print(doc, target = "figures/boxplots/figboxplt_PMY_breeds11.docx")
  
  
  
  
  # To draw regression graph for observed and predicted MY ------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

group_labels <- c("Line 1:1", "PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9")
font <- "Times New Roman"

# Calculate R-squared values
lm_eqn <- function(df) {
  m <- lm(MYO ~ "PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9", data = df)
  r2 <- summary(m)$r.squared
  eq <- substitute(italic(R)^2 == r2, list(r2 = format(r2, digits = 2)))
  as.character(as.expression(eq))
}

# Assuming data4 contains your dataset

# Melt the data for PMY scenarios
data4_long <- data4 %>%
  select(MYO, PMY1, PMY2, PMY3, PMY4, PMY5, PMY6, PMY7, PMY8, PMY9) %>%
  pivot_longer(-MYO, names_to = "Scenario", values_to = "PPMY")

# Create the regression plot
myfigPMYMYOregressn <- ggplot(data4_long, aes(y = MYO, x = PPMY, color = Scenario)) +
  geom_point(size = 2, shape = 20, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, linetype = "solid", na.rm = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "longdash", size = 0.5) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1.5), expand = c(0, 0)) +
  scale_color_discrete(labels = group_labels) +
  labs(
    color = "",
    y = "Observed milk yield (kg/day), n=180",
    x = "Predicted milk yield (kg/day),n=180"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = font, size = 10),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(margin = margin(t = 10, b = 0, r = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 10, l = 0)),
    legend.position = "bottom",
    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
    legend.text.align = 0,
    legend.background = element_rect(colour = "transparent", fill = "transparent")
  ) +
  geom_text(
    x = 8,
    y = 14,
    label = lm_eqn(data4_long),
    parse = TRUE,
    color = "black",
    hjust = 1,
    vjust = 1
  )

# Save the plot
ggsave(filename = "figures/regressionplots/figregressionOMYPMY.png", plot = myfigPMYMYOregressn, height = 4, width = 4, units = "in")


# Regression of MYO with VDMI, ADG-------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

group_labels <- c("ADG")
font <- "Times New Roman"

myfigADGMYO3 <- data4 %>%
  select(MYO, ADG) %>%
  pivot_longer(!MYO, names_to = "model", values_to = "values") %>%
  ggplot(aes(y = MYO, x = values, color = model)) +
  geom_point(size = 2, shape = 20, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, linetype = "solid", na.rm = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "longdash", size = 0.5) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0,12, by = 3), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 19, by = 3), expand = c(0, 0)) +
  scale_color_discrete(labels = group_labels) +
  labs(
    color = "",
    y = "Observed milk yield (kg/day, n =126)",
    x = "Average daily gain (kg/day, n =126)",
  ) +
  theme_classic() +
  theme(
    text = element_text(family = font, size = 12),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(margin = margin(t = 10, b = 0, r = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 10, l = 0)),
    legend.position = "bottom",
    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
    legend.text.align = 0,
    legend.background = element_rect(colour = "transparent", fill = "transparent")
  )

# Calculate R-squared values
lm_eqn <- function(df) {
  m <- lm(MYO ~ ADG, df)
  r2 <- summary(m)$r.squared
  eq <- substitute(italic(R)^2 == r2, list(r2 = format(r2, digits = 2)))
  as.character(as.expression(eq))
}

# Add R-squared labels to the plot
myfigADGMYO3 <- myfigADGMYO3 +
  geom_text(
    x = 8,
    y = 14,
    label = lm_eqn(data3c),
    parse = TRUE,
    color = "black",
    hjust = 1,
    vjust = 1
  )

# Save the plot
myfigADGMYO3 %>%
  ggsave(filename = "figures/regressionplots/myfigADGMYO3.png", height = 4, width = 4, units = "in")


library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

group_labels <- c("ADG(Average daily gain, kg/day)", "VDMI (Voluntary dry matter intake,gm/kgWt^0.75")
font <- "Times New Roman"

myfigADGVDMIMYO2 <- data4 %>%
  select(MYO, ADG, VDMI) %>%
  pivot_longer(cols = c(ADG, VDMI), names_to = "model", values_to = "values") %>%
  ggplot(aes(y = MYO, x = values, color = model)) +
  geom_point(size = 2, shape = 20, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, linetype = "solid", na.rm = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "longdash", size = 0.5) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 3), expand = c(0, 0)) +
  scale_color_discrete(labels = group_labels) +
  labs(
    color = "",
    y = "Observed milk yield (kg/day, n = 126)",
    x = "ADG (kg/day, n = 126)"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = font, size = 14),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(margin = margin(t = 10, b = 0, r = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 10, l = 0)),
    legend.position = "bottom",
    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
    legend.text.align = 0,
    legend.background = element_rect(colour = "transparent", fill = "transparent")
  )

# Calculate R-squared values
lm_eqn <- function(df) {
  m <- lm(MYO ~ values, data = df)
  r2 <- summary(m)$r.squared
  eq <- substitute(italic(R)^2 == r2, list(r2 = format(r2, digits = 2)))
  as.character(as.expression(eq))
}

# Add R-squared labels to the plot
myfigADGVDMIMYO2 <- myfigADGVDMIMYO +
  geom_text(
    x = 8,
    y = 14,
    label = lm_eqn(data4),
    parse = TRUE,
    color = "black",
    hjust = 1,

  ) +
  guides(color = guide_legend(
    title = "Variables",
    override.aes = list(
      label = c("ADG (Average daily gain, kg/day)", "VDMI (Voluntary dry matter intake,gm/kgWt^0.75)")
    )
  ))

# Save the plot
ggsave(filename = "figures/regressionplots/myfigADGVDMIMYO2.png", plot = myfigADGVDMIMYO2)

# Regression of Energy requirement (ME) and OMY-------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

group_labels <- c("MEm 1 to 9 (Energy requirment for maintenance,MJ/day for 9 scenarios)",
                  "MEc (Energy requirment for conception,MJ/day for 9 scenarios",
                  "MEg (Energy requirment for body gain,MJ/day for 9 scenarios", 
                  "MEa (Energy requirment for activity,MJ/day for 9 scenarios")
font <- "Times New Roman"


myfigADGVDMIMYO4 <- data4 %>%
  select(MYO, MEm1,MEm2,MEm3,MEm4,MEm5,MEm6,MEm7,MEm8,MEm9,MEc,MEg,MEa1,MEa2,MEa3) %>%
  pivot_longer(cols = c(MEm1,MEm2,MEm3,MEm4,MEm5,MEm6,MEm7,MEm8,MEm9,MEc,MEg,MEa1,MEa2,MEa3), names_to = "model", values_to = "MEm1","MEm2","MEm3","MEm4","MEm5","MEm6","MEm7","MEm8","MEm9","MEc","MEg","MEa1","MEa2","MEa3") %>%
  ggplot(aes(y = MYO, x = ADG, color = model)) +
  geom_point(size = 2, shape = 20, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, linetype = "solid", na.rm = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "longdash", size = 0.5) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 3), expand = c(0, 0)) +
  scale_color_discrete(labels = group_labels) +
  labs(
    color = "",
    y = "MYO, Observed milk yield (kg/day, n = 126)",
    x = "MEm1","MEm2","MEm3","MEm4","MEm5","MEm6","MEm7","MEm8","MEm9","MEc","MEg","MEa1","MEa2","MEa3"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = font, size = 14),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title.x = element_text(margin = margin(t = 10, b = 0, r = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 10, l = 0)),
    legend.position = "bottom",
    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
    legend.text.align = 0,
    legend.background = element_rect(colour = "transparent", fill = "transparent")
  )

# Calculate R-squared values
lm_eqn <- function(df) {
  m <- lm(MYO ~ MEm1,MEm2,MEm3,MEm4,MEm5,MEm6,MEm7,MEm8,MEm9,MEc,MEg,MEa1,MEa2,MEa3, data = df)
  r2 <- summary(m)$r.squared
  eq <- substitute(italic(R)^2 == r2, list(r2 = format(r2, digits = 2)))
  as.character(as.expression(eq))
}

# Add R-squared labels to the plot
myfigADGVDMIMYO4 <- myfigADGVDMIMYO4 +
  geom_text(
    x = 8,
    y = 14,
    label = lm_eqn(data4),
    parse = TRUE,
    color = "black",
    hjust = 1,
    vjust = 1
  ) +
  guides(color = guide_legend(
    title = "Variables",
    override.aes = list(
      label = c("MYO (Observed milk yield (kg/day)",
                "MEm 1 to 9 (Energy requirment for maintenance,MJ/day for 9 scenarios)",
                "MEc (Energy requirment for conception,MJ/day for 9 scenarios",
                "MEg (Energy requirment for body gain,MJ/day for 9 scenarios",
                "MEa (Energy requirment for activity,MJ/day for 9 scenarios")
    )
  ))
# Save the plot with dimensions 9.53 x 3.81 inches
ggsave(filename = "figures/regressionplots/myfigADGVDMIMYO4.png", plot = myfigADGVDMIMYO4, width = 9.53, height = 3.81, units = "in")

# To draw correlarion graph for each senarios  ----------------------------
# correlation graph for MYO and MYP in one graph  -------------------------

library(ggplot2)
library(gridExtra)
library(cowplot)
library(officer)

# Create a list to store correlation plots
correlation_plots <- list()
# Define custom legend labels
legend_labels <- c("scenario (the combination of equations to estimate predicted milk yield for the nine combinations)",
                   "MYO (Observed milk yield, kg/day)")

# Loop through each scenario
for (i in 1:9) {
  # Subset the data for the current scenario
  scenario_data <- data4[, c("MYO", paste0("PMY", i))]
  
  # Calculate R-squared value
  r_squared <- cor(scenario_data$MYO, scenario_data[[paste0("PMY", i)]])^2
  
  # Create a correlation plot for the current scenario
  correlation_plot <- ggplot(scenario_data, aes(x = MYO, y = .data[[paste0("PMY", i)]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste("Scenario", i),
         subtitle = paste("R2 =", round(r_squared, 2)),
         x = "MYO, kg/day",
         y = paste("Predicted Milk Yield, kg/day", i)) +
    theme_minimal() +
    theme(legend.title = element_text(size = 10, face = "normal"),
          legend.text = element_text(size = 10),
          legend.position = "bottom",  # Customize the position of the legend (options: "none", "left", "right", "bottom", "top", or specify coordinates using the "legend.position" argument)
          legend.box = "horizontal",  # Customize the legend box style (options: "horizontal", "vertical", or "none")
          legend.margin = margin(t = 10, unit = "cm"),  # Adjust the margin around the legend
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"))
  
  # Add the correlation plot to the list
  correlation_plots[[i]] <- correlation_plot
}

# Arrange the correlation plots in a grid
grid <- cowplot::plot_grid(plotlist = correlation_plots, ncol = 9)

# Create a caption or legend for the graph
caption <- ggplot2::labs(fill = "Legend") +
  ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 8))

# Combine the graph and caption
combined_layout <- cowplot::plot_grid(grid, caption, ncol = 1, rel_heights = c(0.9, 0.1))

# Save the combined layout as an image file
ggsave("figures/correlationplots/correlanMYOPMY2.png", combined_layout, width = 10, height = 8, units = "in", dpi = 300)

# Create a new Word document
doc <- officer::read_docx()

# Add the image to the Word document
doc <- officer::body_add_img(doc, src = "figures/correlationplots/correlanMYOPMY2.png", width = 10, height = 8)

# Save the Word document
print(doc, target = ("figures/correlationplots/correlanMYOPMY2.docx"))






# Significance difference between R2 values of scenarios ANOVA

# Define the R-squared values for each scenario
r_squared_values <- c(0.75, 0.75, 0.74, 0.77, 0.69, 0.75, 0.69, 0.75, 0.69)

# Define the null hypothesis mean R-squared value
desired_mu <- 0.7

# Perform paired t-test to compare R-squared values
t_test_result <- t.test(r_squared_values, mu = desired_mu, alternative = "two.sided")

# Extract the p-value
p_value <- t_test_result$p.value

# Print the results
cat("Paired t-test result:\n")
cat("Null hypothesis: The mean R-squared value is equal to", desired_mu, "\n")
cat("Alternative hypothesis: The mean R-squared value is different from", desired_mu, "\n")
cat("p-value:", p_value, "\n")


# Define the R-squared values for each scenario
r_squared_values <- c(0.75, 0.75, 0.74, 0.77, 0.69, 0.75, 0.69, 0.75, 0.69)

# Define the null hypothesis mean R-squared value
desired_mu <- 0.7

# Create a vector to store the p-values
p_values <- numeric(length(r_squared_values))

# Perform paired t-test for each scenario
for (i in 1:length(r_squared_values)) {
  t_test_result <- t.test(r_squared_values[i], mu = desired_mu, alternative = "two.sided")
  p_values[i] <- t_test_result$p.value
}

# Create a table of significance
significance_table <- data.frame(
  Scenario = 1:length(r_squared_values),
  R_squared = r_squared_values,
  p_value = p_values,
  Significance = ifelse(p_values < 0.05, "Yes", "No")
)

# Print the table
print(significance_table)
# For cross validarion reggression whole data -------------------------------------
library(caret)
library(ggplot2)
library(officer)
library(magrittr)

# Create a linear regression model
model <- train(MYO ~ ., data = data4[, c("MYO", paste0("PMY", 1:9))], method = "lm")

# Perform cross-validation predictions
predicted <- predict(model, data4[, c("MYO", paste0("PMY", 1:9))])

scatter_plot <- ggplot(data4, aes(x = predicted, y = MYO)) +
  geom_point(aes("Predicted PMY (Scenarios 1-9)" = "PMY", "Observed milk yield, MYO, kg/day" = "MYO"), show.legend = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Predicted PMY (Scenarios 1-9), kg/day", y = "Observed milk yield, MYO, kg/day") +
  theme_minimal() +
  scale_color_manual(values = "black", labels = c("PMY: Predicted milk yield")) +
  scale_shape_manual(values = 16, labels = c("MYO: Observed milk yield")) +
  labs(PMY = "Predicted PMY (Scenarios 1-9), kg/day", MYO = "Observed milk yield, MYO, kg/day")

# Export the scatter plot to Word document
doc <- read_docx()
doc <- doc %>% body_add_gg(scatter_plot)
print(doc, target = "figures/regressionplots/crossvalidation.docx")

# Save the scatter plot as an image file
ggsave("figures/regressionplots/crossvalidation.png", plot = scatter_plot, width = 15, height = 12, units = "in", dpi = 300)


# Regression between MYO and ADG for combined Breedname for 200gm --------
library(ggplot2)
library(officer)
library(magrittr)
library(flextable)
library(gridExtra)

# List of breeds
breeds <- c("ar", "HF", "HFbor", "HFar", "bor", "hor", "HFhor", "jer", "jerbor")

# Create an empty data frame to store regression coefficients
coefficients_df <- data.frame(Breedname = character(),
                              Intercept = numeric(),
                              ADG_Coefficient = numeric(),
                              R_squared = numeric(),
                              stringsAsFactors = FALSE)

# Create a Word document
doc <- read_docx()

# Create a grid to store the scatter plots
grid <- list()

# Perform linear regression and create scatter plots for each breed
for (breed in breeds) {
  # Subset data for the current breed
  breed_data <- data4[data4$Breedname == breed, ]
  
  # Perform linear regression
  lm_model <- lm(MYO ~ ADG, data = breed_data)
  
  # Create a scatter plot with regression equation label and R-squared value
  scatter_plot <- ggplot(breed_data, aes(x = ADG, y = MYO, color = Breedname)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    geom_label(
      aes(label = paste0("y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), " * x")),
      x = Inf, y = -Inf, hjust = 1, vjust = 0, size = 3, color = "black"
    ) +
    geom_text(
      aes(label = paste0("R-squared = ", round(summary(lm_model)$r.squared, 2))),
      x = Inf, y = -Inf, hjust = 1, vjust = 1, size = 3, color = "black"
    ) +
    labs(
      x = "ADG", y = "MYO",
      title = paste("Regression: MYO ~ ADG (", breed, ")")
    ) +
    theme_minimal()
  
  # Store the scatter plot in the grid
  grid[[breed]] <- scatter_plot
  
  # Store the regression coefficients in the data frame
  coefficients_df <- rbind(coefficients_df, data.frame(Breedname = breed,
                                                       Intercept = coef(lm_model)[1],
                                                       ADG_Coefficient = coef(lm_model)[2],
                                                       R_squared = summary(lm_model)$r.squared,
                                                       stringsAsFactors = FALSE))
}

# Remove the empty row from coefficients_df
coefficients_df <- coefficients_df[-1, ]

# Combine the scatter plots into one large graph
combined_plot <- do.call(grid.arrange, c(grid, nrow = 3, ncol = 3))

# Save the combined plot as a PNG image
combined_plot_path <- "figures/regressionplots/regMYOADG_combined11.png"
ggsave(combined_plot_path, plot = combined_plot, width = 10, height = 10, units = "in", dpi = 300)

# Add the combined plot image to the Word document
doc <- doc %>%
  body_add_par("Combined Scatter Plots", style = "Table Caption") %>%
  body_add_img(src = combined_plot_path, width = 10, height = 10)

# Save the Word document
doc_path <- "figures/regressionplots/regMYOADG_combined11.doc"
print

# RMSE, root mean square error ---------------------------------------------
# Select relevant columns for RMSE calculation
rmse_data <- data4 %>%
  select(Breedname, MYO, PMY1, PMY2, PMY3, PMY4, PMY5, PMY6, PMY7, PMY8, PMY9)

# Function to calculate RMSE
calculate_rmse <- function(MYO, predicted) {
  sqrt(((sum(MYO)-sum(predicted))^2/126))
}
# Calculate RMSE for each breed and each variable
rmse <- rmse_data %>%
  group_by(Breedname) %>%
  summarise(
    RMSE_PMY1 = calculate_rmse(MYO, PMY1),
    RMSE_PMY2 = calculate_rmse(MYO, PMY2),
    RMSE_PMY3 = calculate_rmse(MYO, PMY3),
    RMSE_PMY4 = calculate_rmse(MYO, PMY4),
    RMSE_PMY5 = calculate_rmse(MYO, PMY5),
    RMSE_PMY6 = calculate_rmse(MYO, PMY6),
    RMSE_PMY7 = calculate_rmse(MYO, PMY7),
    RMSE_PMY8 = calculate_rmse(MYO, PMY8),
    RMSE_PMY9 = calculate_rmse(MYO, PMY9)
  )

# Display the RMSE table
rmse %>% web_table()

# Calculate the overall RMSE
overall_rmse <- mean(unlist(rmse[-1]), na.rm = TRUE)








# Function to calculate RMSE
mutate(PMY = PMY1, PMY2, PMY3, PMY4, PMY5, PMY6, PMY7, PMY8, PMY9)
calculate_rmse <- function(MYO, predicted) {
  sqrt(sum((MYO-PMY)^2/126))
}
# Calculate RMSE for each breed and each variable
rmse <- rmse_data %>%
  group_by(Breedname) %>%
  summarise(
    RMSE_PMY1 = calculate_rmse(MYO-PMY1)^2,
    RMSE_PMY2 = calculate_rmse(MYO-PMY2)^2,
    RMSE_PMY3 = calculate_rmse(MYO-PMY3)^2,
    RMSE_PMY4 = calculate_rmse(MYO-PMY4)^2,
    RMSE_PMY5 = calculate_rmse(MYO-PMY5)^2,
    RMSE_PMY6 = calculate_rmse(MYO-PMY6)^2,
    RMSE_PMY7 = calculate_rmse(MYO-PMY7)^2,
    RMSE_PMY8 = calculate_rmse(MYO-PMY8)^2,
    RMSE_PMY9 = calculate_rmse(MYO-PMY9)^2
  )

# Display the RMSE table
rmse %>% web_table()

# Calculate the overall RMSE
overall_rmse <- mean(unlist(rmse[-1]), na.rm = TRUE)




# Function to calculate RMSE
calculate_rmse <- function(MYO, predicted) {
  sqrt((((sum(MYO)- sum(predicted))^2/126)))
}
# Calculate RMSE for each breed and each variable
rmse_results <- data.frame(Breedname = character(), stringsAsFactors = FALSE)

for (variable in c("PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9")) {
  rmse <- rmse_data %>%
    group_by(Breedname) %>%
    summarise(RMSE = calculate_rmse(MYO, !!sym(variable)))
  
  rmse_results <- bind_rows(rmse_results, rmse)
}

# Display the RMSE table
rmse_results %>% web_table()





# Define the data
data <- list(
  'Observed MY' = list(
    c(4.66, 2.75), c(4.66, 2.75), c(4.66, 2.75), c(4.66, 2.75), c(4.66, 2.75),
    c(4.66, 2.75), c(4.66, 2.75), c(4.66, 2.75), c(4.66, 2.75)
  ),
  'Predicted YM' = list(
    c(6.49, 3.81), c(5.16, 3.94), c(7.79, 3.70), c(5.39, 4.14), c(7.54, 3.54),
    c(4.99, 3.95), c(7.37, 3.55), c(5.05, 3.95), c(7.43, 3.55)
  ),
  'ME maintenance (MEm)' = list(
    c(41.52, 3.78), c(49.52, 4.51), c(33.75, 3.07), c(47.97, 4.83), c(35.61, 6.76),
    c(49.52, 4.51), c(35.61, 6.76), c(49.52, 4.51), c(35.61, 6.76)
  ),
  'ME activit (MEa)' = list(
    0, 0, 0, 0, 0, 1, 1, c(0.62, 0.07), c(0.62, 0.07)
  ),
  'ME conception (MEc)' = list(
    c(14.48, 2.60), c(14.48, 2.61), c(14.48, 2.62), c(14.48, 2.63), c(14.48, 2.64),
    c(14.48, 2.65), c(14.48, 2.66), c(14.48, 2.67), c(14.48, 2.68)
  ),
  'ME gain (Meg)' = list(
    c(9.25, 3.09), c(9.25, 3.09), c(9.25, 3.09), c(9.25, 3.09), c(9.25, 3.09),
    c(9.25, 3.09), c(9.25, 3.09), c(9.25, 3.09), c(9.25, 3.09)
  )
)

# Perform pairwise t-tests for each variable
variables <- names(data)
scenarios <- 1:9

for (variable in variables) {
  cat(paste0("Variable: ", variable, "\n"))
  cat("Scenario\tMean Difference\tp-value\n")
  
  for (i in scenarios) {
    for (j in scenarios) {
      if (i != j) {
        scenario_i <- unlist(data[[variable]][[i]])
        scenario_j <- unlist(data[[variable]][[j]])
        result <- t.test(scenario_i, scenario_j)
        
        cat(paste0(i, " vs ", j, "\t", mean(scenario_i) - mean(scenario_j), "\t", result$p.value, "\n"))
      }
    }
  }
}
# Load the necessary library for Tukey's HSD test
library("agricolae")

# Perform ANOVA and Tukey's HSD test for each variable
variables <- names(data)
scenarios <- 1:9

for (variable in variables) {
  cat(paste0("Variable: ", variable, "\n"))
  cat("Scenario\tMean Difference\tp-value\tLetters\n")
  
  # Perform ANOVA
  anova_result <- aov(data[[variable]] ~ factor(scenarios))
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, "scenarios")
  
  for (i in 1:length(tukey_result$groups$trt)) {
    scenario_i <- tukey_result$groups$trt[i]
    mean_diff <- tukey_result$groups$means[i]
    p_value <- tukey_result$groups$pvals[i]
    letters <- tukey_result$groups$letters[i]
    
    cat(paste0(scenario_i, "\t", mean_diff, "\t", p_value, "\t", letters, "\n"))
  }
}                         

# Load the necessary library for Tukey's HSD test
library("agricolae")

# Perform ANOVA and Tukey's HSD test for each variable
variables <- names(data)
scenarios <- 1:9

for (variable in variables) {
  cat(paste0("Variable: ", variable, "\n"))
  cat("Scenario\tMean Difference\tp-value\tLetters\n")
  
  # Prepare the data for ANOVA and Tukey's HSD test
  variable_data <- data[[variable]]
  scenario_data <- rep(scenarios, each = length(variable_data))
  
  # Perform ANOVA
  anova_result <- aov(variable_data ~ factor(scenario_data))
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, "scenario_data")
  
  for (i in 1:length(tukey_result$groups$trt)) {
    scenario_i <- tukey_result$groups$trt[i]
    mean_diff <- tukey_result$groups$means[i]
    p_value <- tukey_result$groups$pvals[i]
    letters <- tukey_result$groups$letters[i]
    
    cat(paste0(scenario_i, "\t", mean_diff, "\t", p_value, "\t", letters, "\n"))
  }
}
# Load the necessary library for Tukey's HSD test
library("agricolae")

# Perform ANOVA and Tukey's HSD test for each variable
variables <- names(data)
scenarios <- 1:9

for (variable in variables) {
  cat(paste0("Variable: ", variable, "\n"))
  cat("Scenario\tMean Difference\tp-value\tLetters\n")
  
  # Prepare the data for ANOVA and Tukey's HSD test
  variable_data <- unlist(data[[variable]])
  scenario_data <- rep(scenarios, each = length(variable_data))
  
  # Create a data frame for ANOVA
  anova_data <- data.frame(variable = variable_data, scenario = factor(scenario_data))
  
  # Perform ANOVA
  anova_result <- aov(variable ~ scenario, data = anova_data)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, "scenario")
  
  for (i in 1:length(tukey_result$groups$trt)) {
    scenario_i <- tukey_result$groups$trt[i]
    mean_diff <- tukey_result$groups$means[i]
    p_value <- tukey_result$groups$pvals[i]
    letters <- tukey_result$groups$letters[i]
    
    if (length(letters) == 0) {
      letters <- "-"
    } else if (length(letters) > 1) {
      letters <- paste(letters, collapse = "")
    }
    
    cat(paste0(scenario_i, "\t", mean_diff, "\t", p_value, "\t", letters, "\n"))
  }
}



# Install and load the 'irr' package
if (!requireNamespace("irr", quietly = TRUE))
  install.packages("irr")
library(irr)

# Define the data for PMY and MYo with sample size 126
PMY <- c(6.49, 5.16, 7.79, 5.39, 7.54, 4.99, 7.37, 5.05, 7.43)
MYo <- c(4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66)

# Calculate the Concordance Correlation Coefficient (CCC)
ccc <- ccc(PMY, MYO)

# Print the CCC
cat("Concordance Correlation Coefficient (CCC) =", ccc$rho.c, "\n")

# Mean comparison based on khatys codes 
# ME values among scenarios
data4 <- subset(data3f1, !(FLuta > 199.999))

me_m1 <- data4$MEm1
me_m2 <- data4$MEm2
me_m3 <- data4$MEm3
me_m4 <- data4$MEm4
me_m5 <- data4$MEm5
me_m6 <- data4$MEm6
me_m7 <- data4$MEm7


me <- data.frame(me_m1, me_m2, me_m3, me_m4, me_m5, me_m6, me_m7, me_m8,me_m9)

df_long <- me %>%
  pivot_longer(cols = starts_with("me"),
               names_to = "Column",
               values_to = "Value")

mod_me <- lm(Value ~ Column, data = df_long)

mod_means_contr_me <- emmeans::emmeans(object = mod_me,
                                       pairwise ~ "Column",
                                       adjust = "tukey")
mod_means_me <- multcomp::cld(object = mod_means_contr_me$emmeans,
                              Letters = letters)
mod_means_me

mod <- lm(MYO ~ Breedname, data = data4)
mod_means_contr <- emmeans::emmeans(object = mod,
                                    pairwise ~ "Breedname",
                                    adjust = "tukey")
mod_means <- multcomp::cld(object = mod_means_contr$emmeans,
                           Letters = letters)

mod_means
# PMY and MYO values among scenarios 
PMY1 <- data4$PMY1
PMY2 <- data4$PMY2
PMY3 <- data4$PMY3
PMY4 <- data4$PMY4
PMY5 <- data4$PMY5
PMY6 <- data4$PMY6
PMY7 <- data4$PMY7
PMY8 <- data4$PMY8
PMY9 <- data4$PMY9

pmy <- data.frame(PMY1, PMY2, PMY3, PMY4, PMY5, PMY6, PMY7, PMY8, PMY9)

pmy_long <- pmy %>%
  pivot_longer(cols = starts_with("PMY"),
               names_to = "Column",
               values_to = "Value")

mod_pmy <- lm(Value ~ Column, data = pmy_long)

mod_means_contr_pmy <- emmeans::emmeans(object = mod_pmy,
                                        pairwise ~ "Column",
                                        adjust = "tukey")
mod_means_pmy <- multcomp::cld(object = mod_means_contr_pmy$emmeans,
                               Letters = letters)
mod_means_pmy


# RMSE per scenario per breed
columns2 <- c("PMY1", "PMY2", "PMY3", "PMY4", "PMY5", "PMY6", "PMY7", "PMY8", "PMY9")
rmse_values <- list()

for (column in columns2) {
  rmse_list <- list()
  for (breed in unique(data4$Breedname)) {
    subset_data <- data4[data4$Breedname == breed, ]
    errors <- subset_data$MYO - subset_data[[column]]
    rmse <- sqrt(mean(errors^2, na.rm = TRUE))  # Calculate RMSE using squared errors
    rmse_list[[breed]] <- rmse
  }
  rmse_values[[column]] <- rmse_list
}
for (i in seq_along(columns2)) {
  cat("Column:", columns2[i], "\n")
  for (breed in names(rmse_values[[i]])) {
    cat("Breed:", breed, "  RMSE:", rmse_values[[i]][[breed]], "\n")
  }
  cat("\n")
}

  

# Create a data frame to store RMSE values
rmse_summary <- data.frame(Breed = character(), stringsAsFactors = FALSE)

# Loop through each breed
for (breed in unique(data4$Breedname)) {
  rmse_values <- c()
  # Loop through each scenario
  for (scenario in columns2) {
    subset_data <- data4[data4$Breedname == breed, ]
    errors <- subset_data$MYO - subset_data[[scenario]]
    rmse <- sqrt(mean(errors^2, na.rm = TRUE))  # Calculate RMSE using squared errors
    rmse_values <- c(rmse_values, rmse)
  }
  
  # Add breed and RMSE values to the summary data frame
  rmse_summary <- rbind(rmse_summary, data.frame(Breed = breed, rmse_values))
}

# Use pivot_wider to rearrange the data into a summary table
library(tidyr)
rmse_summary <- pivot_wider(rmse_summary, names_from = Breed, values_from = rmse_values)

# Print the RMSE summary table
print(rmse_summary)




#Duncan test for the milk quality and performance parameters (NEW CODE)

# Load required packages
# Load required packages
library(inti)
library(shiny)
library(tibble)
library(strex)
library(summarytools)
library(multcomp)
library(emmeans)
library(readxl)
library(DescTools)
library(multcomp)
library(tidyverse)
library(openxlsx)
library(agricolae) 
library(writexl) 
library(officer)
# For Duncan's test for milk quality and performance parameters

# Data importing ----------------------------------------------------------
# Read data from Excel sheet named "126data"
data_duncan <- read_excel("data_23.08.2023_VDMI_200.xlsx", sheet = "126data") %>% 
  mutate(breedname = case_when(
    breed == "Arsi" ~ "ar",
    breed == "Holestien" ~ "hf",
    breed == "1/2Fresian x 1/2Boran" ~ "hfbor",
    breed == "½ Fresin x ½ Arsi" ~ "hfar",
    breed == "Boran" ~  "bor",
    breed == "Horo" ~ "hor",
    breed == "1/2Fresian x 1/2Horo" ~ "hfhor",
    breed == "Jersey" ~ "jer",
    breed == "1/2Jersey x 1/2Boran" ~ "jerbor"
  )) 

# Convert breedname to a factor
data_duncan$breedname <- as.factor(data_duncan$breedname)

# Create a list to store ANOVA models for each variable
anova_models <- list()

# List of variables to test
variables_to_test <- c("LW", "LL", "MYO", "GS", "MF", "MP", "ADG", "VDMI", "DP", "MEinmilk", "Age", "Mediet", "MEI")

# Create ANOVA models for each variable
for (variable in variables_to_test) {
  anova_model <- aov(as.formula(paste(variable, "~ breedname")), data = data_duncan)
  anova_models[[variable]] <- anova_model
}

# Perform Duncan's test for each variable
duncan_results_list <- list()
for (variable in variables_to_test) {
  duncan_results <- agricolae::duncan.test(anova_models[[variable]], "breedname")
  duncan_results_list[[variable]] <- duncan_results
}

# Print Duncan's test results for each variable
for (variable in variables_to_test) {
  cat("Duncan's test results for", variable, ":\n")
  print(duncan_results_list[[variable]])
  cat("\n")
}
# Save the Excel file
saveWorkbook(wb, file = "Duncan_Test_Results.xlsx")

# For Duncan's test for milk quality and performance parameters

# Data importing ----------------------------------------------------------
# Read data from Excel sheet named "126data"
# Initialize a vector to store significance labels
data_duncanenergy <- read_excel("data_23.08.2023_VDMI_200.xlsx", sheet = "126data") %>% 
  mutate(breedname = case_when(
    breed == "Arsi" ~ "ar",
    breed == "Holestien" ~ "hf",
    breed == "1/2Fresian x 1/2Boran" ~ "hfbor",
    breed == "½ Fresin x ½ Arsi" ~ "hfar",
    breed == "Boran" ~  "bor",
    breed == "Horo" ~ "hor",
    breed == "1/2Fresian x 1/2Horo" ~ "hfhor",
    breed == "Jersey" ~ "jer",
    breed == "1/2Jersey x 1/2Boran" ~ "jerbor"
  )) 

# Convert breedname to a factor
data_duncanenergy$breedname <- as.factor(data_duncanenergy$breedname)
# Create a list to store ANOVA models for each variable
anova_models <- list()

# List of variables to test
variables_to_test <- c("MEm1","MEm2","MEm3","MEm4", "MEm5","MEm6","MEm7","MEc","MEa1", "MEa2", "MEa3", "MEg")
# Create ANOVA models for each variable
for (variable in variables_to_test) {
  anova_model <- aov(as.formula(paste(variable, "~ breedname")), data = data_duncanenergy)
  anova_models[[variable]] <- anova_model
 }

# Perform Duncan's test for each variable
duncan_results_list <- list()
for (variable in variables_to_test) {
  duncan_results <- agricolae::duncan.test(anova_models[[variable]], "breedname")
  duncan_results_list[[variable]] <- duncan_results
}


# Print Duncan's test results for each variable
for (variable in variables_to_test) {
  cat("Duncan's test results for", variable, ":\n")
  print(duncan_results_list[[variable]])
  cat("\n")
}
data_duncanenergy <- na.omit(data_duncanenergy)
# Assuming you have a list of Duncan's test results for each variable
# Create a list to store p-values for each variable
pvalues_list <- list()

# Extract p-values from Duncan's test results
for (variable in variables_to_test) {
  pvalues <- duncan_results_list[[variable]]$p.value
  pvalues_list[[variable]] <- pvalues
}

# Initialize a list to store significance labels
sig_list <- list()

# Loop through variables and calculate significance labels
for (variable in variables_to_test) {
  pvalues <- pvalues_list[[variable]]
  sig <- character(length(pvalues))
  
  for (k in seq_along(pvalues)) {
    if (is.na(pvalues[k])) {
      # If the p-value is NA, assign "NA" as the significance label
      sig[k] <- "NA"
    } else if (pvalues[k] <= 0.001) {
      sig[k] <- "***"
    } else if (pvalues[k] <= 0.01) {
      sig[k] <- "**"
    } else if (pvalues[k] <= 0.05) {
      sig[k] <- "*"
    } else {
      sig[k] <- ""
    }
  }
  
  # Store the significance labels in the list
  sig_list[[variable]] <- sig
}
# Save the Excel file
saveWorkbook(wb, file = "Duncan_Test_Results_energy1.xlsx")

































# Mean separation for performance parameters with Tukey's test 

library(tidyverse)
library(openxlsx)
library(multcomp)
library(emmeans)

# Data importing ----------------------------------------------------------
# Read data from Excel sheet named "unscreened"
data1 <- read_excel("data_23.08.2023_VDMI_200.xlsx", sheet = "126data")

# List of variables to analyze
variables <- c("LW", "LL", "GL", "MYO", "GS", "MF", "MP", "ADG", "VDMI", "DP", "ME_milk", "Age", "Mediet", "MEI")

# List of breeds to compare
breeds <- c("Arsi", "½ Fresin x ½ Arsi", "Boran", "horo", "1/2Fresian x 1/2Horo", "Jersey", "1/2Jersey x 1/2Boran")

# Create an empty list to store Tukey's test results
tukey_results <- list()

# Loop through each variable
for (var in variables) {
  # Subset data for the specified variables and breeds
  data_subset <- data1 %>%
    filter(Breed %in% breeds) %>%
    select(.data[[var]], Breed) 
  
  # Perform Tukey's test for the current variable
  tukey_result <- TukeyHSD(aov(as.formula(paste(var, ~ "Arsi", "½ Fresin x ½ Arsi", "Boran", "horo", "1/2Fresian x 1/2Horo", "Jersey", "1/2Jersey x 1/2Boran")), data = data_subset))
  
  # Store the Tukey's test result in the list
  tukey_results[[var]] <- tukey_result
}

# Display Tukey's test results for each variable
for (var in variables) {
  cat("Variable:", var, "\n")
  print(tukey_results[[var]])
  cat("\n")
}


# Khatys code concordance correlation coefficient 

  Khatty <- function(Obs,Pred){
    options("scipen"=100,"digits"=2)
    MSEP <- mean((Obs-Pred)^2) # Calculate mean square prediction error
    RMSEP<-sqrt(MSEP)
    RPE <- RMSEP*100/mean(Obs) # Calculate Root mean square prediction error
    MAE<- mean(abs(Obs-Pred))
    MAPE <- mean(abs((Obs-Pred)/Obs))*100
    
    ECT <- (mean(Obs) - mean(Pred))^2 # Calculate Error central tendency/Mean Bias
    
    s_O <- mean((Obs - mean(Obs))^2) # Calculate variance of observed values
    s_P <- mean((Pred - mean(Pred))^2) # Calculate variance of predictee values
    
    r_Pearson <- mean( (Pred - mean(Pred)) * (Obs - mean(Obs)) ) / sqrt(s_O*s_P) # Calculate correlation coefficient
    ER <- (sqrt(s_P)-r_Pearson*sqrt(s_O))^2 # Calculate Error due to regression/Slope bias
    ED <- (1-r_Pearson^2)*s_O # Calculate error due to disturbances/Random bias
    
    STDEV <- sd(Obs) # Calculate standard deviation of observation
    RSR <- RMSEP/STDEV # Calculate RMSEP-STDEV-ratio
    
    ECTp <- (ECT/MSEP)*100 # Express ECT as a percentage of MSEP
    ERp <- (ER/MSEP)*100 # Express ECT as a percentage of MSEP
    EDp <- (ED/MSEP)*100 # Express ECT as a percentage of MSEP
    
    
    nu <-  sd(Obs)/sd(Pred) # Calculate scale shift
    mu <-  (mean(Obs) - mean(Pred)) / (sd(Pred)*sd(Obs))**0.5 # Calculate location shift
    
    C_b <-  2 / (nu + 1/nu + mu**2) # calculate Bias correction factor
    
    M_obs <- mean(Obs)
    M_pred <- mean(Pred)
    std_obs <- sd(Obs)
    std_pred <- sd(Pred)
    
    return(round(c(M_obs=M_obs, M_pred=M_pred, std_obs=std_obs, std_pred=std_pred
                   , MSEP=MSEP,RMSEP=RMSEP, RPE=RPE,MAE=MAE, MAPE=MAPE
                   , ECTp=ECTp,ERp=ERp,EDp=EDp,RSR=RSR,CCC=r_Pearson*C_b,r_Pearson=r_Pearson
                   ,C_b=C_b,nu=nu,mu=mu),2)) # Return model evaluation statistics
  } # End of function 
  
  Khatty(data4$MYO, data4$PMY1)
  Khatty(data4$MYO, data4$PMY2)
  Khatty(data4$MYO, data4$PMY3)
  Khatty(data4$MYO, data4$PMY4)
  Khatty(data4$MYO, data4$PMY5)
  Khatty(data4$MYO, data4$PMY6)
  Khatty(data4$MYO, data4$PMY7)
  Khatty(data4$MYO, data4$PMY8)
  Khatty(data4$MYO, data4$PMY9)
  # Create a data frame to store the results
  results <- data.frame(
    Scenario = 1:9,
    M_obs = c(4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66, 4.66),
    M_pred = c(6.49, 5.16, 7.79, 5.39, 7.54, 4.99, 7.37, 5.05, 7.43),
    std_obs = c(2.75, 2.75, 2.75, 2.75, 2.75, 2.75, 2.75, 2.75, 2.75),
    std_pred = c(3.81, 3.94, 3.70, 4.14, 3.54, 3.95, 3.55, 3.95, 3.55),
    MSEP = c(7.27, 4.49, 13.49, 5.26, 12.15, 4.39, 11.21, 4.43, 11.58),
    RMSEP = c(2.70, 2.12, 3.67, 2.29, 3.49, 2.09, 3.35, 2.11, 3.40),
    RPE = c(57.87, 45.49, 78.84, 49.22, 74.83, 44.96, 71.86, 45.20, 73.05),
    MAE = c(2.18, 1.72, 3.21, 1.93, 2.97, 1.70, 2.82, 1.71, 2.88),
    MAPE = c(66.88, 60.87, 98.65, 63.86, 95.28, 61.79, 90.21, 61.52, 92.25),
    ECTp = c(46.22, 5.54, 72.52, 10.28, 68.11, 2.50, 65.50, 3.54, 66.46),
    ERp = c(27.83, 53.09, 13.01, 56.27, 12.78, 55.36, 14.00, 54.53, 13.55),
    EDp = c(25.95, 41.38, 14.47, 33.44, 19.11, 42.14, 20.49, 41.93, 19.99),
    RSR = c(0.98, 0.77, 1.33, 0.83, 1.27, 0.76, 1.22, 0.77, 1.24),
    CCC = c(0.71, 0.81, 0.56, 0.87, 0.57, 0.81, 0.59, 0.81, 0.58),
    r_Pearson = c(0.87, 0.87, 0.86, 0.88, 0.83, 0.87, 0.83, 0.87, 0.83),
    C_b = c(0.82, 0.93, 0.66, 0.90, 0.69, 0.93, 0.71, 0.93, 0.70),
    nu = c(0.72, 0.70, 0.74, 0.67, 0.78, 0.70, 0.78, 0.70, 0.78),
    mu = c(-0.57, -0.15, -0.98, -0.22, -0.92, -0.10, -0.87, -0.12, -0.89)
  )
 # Create a Word document
 doc <- read_docx()
 
 # Add a title
 doc <- doc %>%
   body_add_par("Summary Table", style = "centered")
 
 # Create the flextable
 ft <- flextable::qflextable(results)
 
 # Add the flextable to the document
 doc <- doc %>%
   body_add_flextable(ft)
 
 # Save the document
 output_path <- "summary_table.docx"
 print(doc, target = output_path)

 # For cross validation regression MYO and PMY -------------------------------------
 
 
 library(caret)
 library(ggplot2)
 library(officer)
 library(magrittr)
 
 # Set the font type
 font <- "Times New Roman"
 
 # Set the font size
 font_size <- 12
 
 # Create a linear regression model
 model <- train(MYO ~ ., data = data1[, c("MYO", paste0("PMY", 1:9))], method = "lm")
 
 # Perform cross-validation predictions
 predicted <- predict(model, data1[, c("MYO", paste0("PMY", 1:9))])
 
 # Create a scatter plot with adjusted axis labels, font type, and font size
 scatter_plot <- ggplot(data1, aes(x = predicted, y = MYO)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
   geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red") +  # Add 1:1 line
   labs(x = "Predicted milk yield (Scenarios 1-9), (kg/day)",
        y = "Observed milk yield (kg/day)") +
   theme_minimal() +
   theme(axis.title.x = element_text(margin = margin(t = 10, unit = "pt"), size = font_size, family = font),
         axis.title.y = element_text(margin = margin(r = 10, unit = "pt"), size = font_size, family = font),
         text = element_text(size = font_size, family = font),
         panel.grid.major = element_blank(),  # Remove major grid lines
         panel.grid.minor = element_blank(),  # Remove minor grid lines
         panel.border = element_blank())      # Remove plot border
 
 # Set the x-axis limit to 9, y-axis limit to 12, and have labels with gaps of one unit
 scatter_plot <- scatter_plot +
   scale_x_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1), expand = c(0, 0)) +
   scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 1), expand = c(0, 0))
 
 # Export the scatter plot to Word document
 doc <- read_docx()
 doc <- doc %>% body_add_gg(scatter_plot)
 print(doc, target = "regrassion/graphdoc/newdata30_13.docx")
 
 # Save the scatter plot as an image file
 ggsave("regrassion/graphdoc/newdata30_13.png", plot = scatter_plot, width = 8, height = 6, units = "in", dpi = 300)
 
 
 
# Regression/correlation graph for 3 --------------------------------------

font <- "Arial"
group_labels <- c("Line 1:1", "Scenario X")
 
 
fig_x <- data4 %>% 
   select(MYO, PMY1) %>% 
   ggplot(., aes(y = MYO, x = PMY1)) +
   geom_point(size = 2) +
   geom_smooth(method = "lm", se = F, size = 0.5) +
   geom_abline(aes(intercept =  0, slope = 1
                   , color = "1:1", shape = "1:1", linetype = "1:1")
               , show.legend = F) +
   scale_y_continuous(limits = c(0, 14)
                      , breaks = seq(from = 0, to = 14, by = 1)
                      , expand = c(0, 0)
   ) +
   scale_x_continuous(limits = c(0, 14)
                      , breaks = seq(from = 0, to = 14, by = 1)
                      , expand = c(0, 0)
   ) +
   scale_color_manual(values = c("gray", "blue")
                      , labels = group_labels) +
   scale_shape_manual(values = c(15, 16)
                      , labels = group_labels) +
   scale_linetype_manual(values= c("solid","dashed")
                         , labels = group_labels) +
   theme_classic() +
   theme(text = element_text(family = font, size = 12)
         , axis.line = element_line(colour="black") #color de las lineas del grafico
         , axis.text = element_text(colour="black") #color del texto en las lineas
         , axis.title.x = element_text(margin = margin(t = 9, b = 0, r = 0, l = 0)) #distancia del texto a los axis
         , axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 8, l = 0))  #distancia del texto a los axis
         , legend.position = "bottom" # legend position c(1, 1)
         , plot.margin=unit(c(.2,.5,.2,.2), "cm")
         , legend.text.align = 0
         , legend.background = element_rect(colour = "transparent", fill = "transparent")
   ) +
   labs(color = ""
        , linetype = ""
        , shape = ""
        , y = "Observed milk yield (kg/day)"
        , x = "Predicted milk yield - scenario 1 (kg/day)"
   ) 
 #+facet_grid(.~Type)
 fig_x
 
fig_y <- data4 %>% 
  select(MYO, PMY2) %>% 
  ggplot(., aes(y = MYO, x = PMY2)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F, size = 0.5) +
  geom_abline(aes(intercept =  0, slope = 1
                  , color = "1:1", shape = "1:1", linetype = "1:1")
              , show.legend = F) +
  scale_y_continuous(limits = c(0, 14)
                     , breaks = seq(from = 0, to = 14, by = 1)
                     , expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0, 14)
                     , breaks = seq(from = 0, to = 14, by = 1)
                     , expand = c(0, 0)
  ) +
  scale_color_manual(values = c("gray", "blue")
                     , labels = group_labels) +
  scale_shape_manual(values = c(15, 16)
                     , labels = group_labels) +
  scale_linetype_manual(values= c("solid","dashed")
                        , labels = group_labels) +
  theme_classic() +
  theme(text = element_text(family = font, size = 12)
        , axis.line = element_line(colour="black") #color de las lineas del grafico
        , axis.text = element_text(colour="black") #color del texto en las lineas
        , axis.title.x = element_text(margin = margin(t = 9, b = 0, r = 0, l = 0)) #distancia del texto a los axis
        , axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 8, l = 0))  #distancia del texto a los axis
        , legend.position = "bottom" # legend position c(1, 1)
        , plot.margin=unit(c(.2,.5,.2,.2), "cm")
        , legend.text.align = 0
        , legend.background = element_rect(colour = "transparent", fill = "transparent")
  ) +
  labs(color = ""
       , linetype = ""
       , shape = ""
       , y = "Observed milk yield (kg/day)"
       , x = "Predicted milk yield - scenario 2 (kg/day)"
  ) 

fig_y

fig_w <- data4 %>% 
  select(MYO, PMY3) %>% 
  ggplot(., aes(y = MYO, x = PMY3)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F, size = 0.5) +
  geom_abline(aes(intercept =  0, slope = 1
                  , color = "1:1", shape = "1:1", linetype = "1:1")
              , show.legend = F) +
  scale_y_continuous(limits = c(0, 14)
                     , breaks = seq(from = 0, to = 14, by = 1)
                     , expand = c(0, 0)
  ) +
  scale_x_continuous(limits = c(0, 14)
                     , breaks = seq(from = 0, to = 14, by = 1)
                     , expand = c(0, 0)
  ) +
  scale_color_manual(values = c("gray", "blue")
                     , labels = group_labels) +
  scale_shape_manual(values = c(15, 16)
                     , labels = group_labels) +
  scale_linetype_manual(values= c("solid","dashed")
                        , labels = group_labels) +
  theme_classic() +
  theme(text = element_text(family = font, size = 12)
        , axis.line = element_line(colour="black") #color de las lineas del grafico
        , axis.text = element_text(colour="black") #color del texto en las lineas
        , axis.title.x = element_text(margin = margin(t = 9, b = 0, r = 0, l = 0)) #distancia del texto a los axis
        , axis.title.y = element_text(margin = margin(t = 0, b = 0, r = 8, l = 0))  #distancia del texto a los axis
        , legend.position = "bottom" # legend position c(1, 1)
        , plot.margin=unit(c(.2,.5,.2,.2), "cm")
        , legend.text.align = 0
        , legend.background = element_rect(colour = "transparent", fill = "transparent")
  ) +
  labs(color = ""
       , linetype = ""
       , shape = ""
       , y = "Observed milk yield (kg/day)"
       , x = "Predicted milk yield - scenario 3 (kg/day)"
  ) 

fig_w

figure <- plot_grid(
   fig_x + theme(legend.position="none"),
   fig_y + theme(legend.position="none"),
   fig_w + theme(legend.position="none"),
   align = 'vh',
   labels = c("A", "B", "C"),
   hjust = -1,
   nrow = 1)
 figure
 
 legend_fig1 <- get_legend(fig_x + 
                             guides(color = guide_legend(nrow = 1)) +
                             theme(legend.position = "bottom"))
 
 plot_grid(figure, legend_fig1, ncol = 1, rel_heights = c(1, .1)) #%>% 
   #cowplot::save_plot(filename = "figures/fig1-29032023.jpg"
                     # , plot = .
                  #, base_height = 4
                     # , base_width = 10)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

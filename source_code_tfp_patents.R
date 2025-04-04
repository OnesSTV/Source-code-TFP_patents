install.packages("writexl")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("RColorBrewer")
install.packages("googledrive")
install.packages("tidyr")
install.packages("viridis")
install.packages("forcats")
install.packages("plm")
install.packages("rstudioapi")

library(purrr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library (writexl)
library(googledrive)
library(tidyr)
library(viridis)
library(forcats)
library(plm)
library(rstudioapi)

#to be able to run our code and download the files you will need, you have top 
#run the code line below and in the console type 1 and then authorise the access 
#of Tidyverse API 

drive_auth()

#below the line, there is a loop to be able to create a folder on your dekstop 
#and download the files you will need to run our code

#loop-------------------------------------------------------------------------------

if (Sys.info()["sysname"] == "Windows") {
  desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")  
  if (!dir.exists(desktop_path)) {
    desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Bureau")
  }
} else {
  desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
  if (!dir.exists(desktop_path)) {
    desktop_path <- file.path(Sys.getenv("HOME"), "Bureau") 
  }
}

data_dir <- file.path(desktop_path, "data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("folder data created on the dekstop")
}

folder_id <- "1wZ0HC5rOK6F9i-vEp_tLWmVjdwFJE7BL"
files_in_folder <- drive_ls(as_id(folder_id))

file_ids <- files_in_folder[["id"]]
file_names <- files_in_folder[["name"]]  

walk2(file_ids, file_names, ~ drive_download(as_id(.x), path = file.path(data_dir, .y)))

folder_id_tfp <- "1hV8KSR8ZdIjdSMQsPyEEFuROynv0DoY2"
files_in_folder_tfp <- drive_ls(as_id(folder_id_tfp))

file_ids_tfp <- files_in_folder_tfp[["id"]]
file_names_tfp <- files_in_folder_tfp[["name"]]  

walk2(file_ids_tfp, file_names_tfp, ~ drive_download(as_id(.x), path = file.path(data_dir, .y)))

rm(desktop_path, file_ids, file_names,files_in_folder,folder_id,file_ids_tfp,file_names_tfp,folder_id_tfp,files_in_folder_tfp)

setwd(data_dir)
rstudioapi::filesPaneUpdate()

#---code for patents excel files ----------------------------------------------------

check<- c("202401_PCT_Inv_reg.txt","202401_PCT_IPC.txt")

existing_files <- file.exists(file.path(data_dir,check))

if (all(existing_files)) {
  cat("All necessary files are present in the folder.\n")
} else {
  cat("Some files are missing. Downloading...\n")
}  

# code above checks if all the files needed are loaded and installed

fichier_1 <- file.path(data_dir, "202401_PCT_Inv_reg.txt")
fichier_2 <- file.path(data_dir, "202401_PCT_IPC.txt")

data_1<- fread(fichier_1, sep = "|" )
data_2<- fread(fichier_2,sep = "|")

rm(fichier_1,fichier_2,check,existing_files)

str(data_1)
str(data_2)

data_unique_1 <- unique(data_1, by = "pct_nbr")
data_unique_2 <- unique(data_2, by = "pct_nbr")

merged_data_unique <- merge(data_unique_1, data_unique_2, by = "pct_nbr", all = TRUE) #take only the first appearance of the "pct_nbr" 

summary_table <- merged_data_unique[prio_year >= 1990 & prio_year <= 2019, #keep only the observations from 1990 to 2019
                                    .(nb_pct_nbr = uniqueN(pct_nbr)),    #then merge the two data tables 
                                    by = .(ctry_code, prio_year)]


summary_table <- summary_table[order(ctry_code, prio_year)]

countries_to_keep_OCDE <- c("AU", "AT", "BE", "BG", "CA", "HR", "CY", "CZ", "DK", 
                            "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "JP", 
                            "KR", "LV", "LT", "LU", "MT", "NL", "NO", "PL", "PT", 
                            "SK", "SI", "ES", "SE", "CH", "TR", "GB", "US")

summary_table_OCDE <- summary_table[ctry_code %in% countries_to_keep_OCDE]

code_mapping <- c(
  AU = "AUS", AT = "AUT", BE = "BEL", BG = "BGR", CA = "CAN",
  HR = "HRV", CY = "CYP", CZ = "CZE", DK = "DNK", EE = "EST",
  FI = "FIN", FR = "FRA", DE = "DEU", GR = "GRC", HU = "HUN",
  IE = "IRL", IT = "ITA", JP = "JPN", KR = "KOR", LV = "LVA",
  LT = "LTU", LU = "LUX", MT = "MLT", NL = "NLD", NO = "NOR",
  PL = "POL", PT = "PRT", SK = "SVK", SI = "SVN", ES = "ESP",
  SE = "SWE", CH = "CHE", TR = "TUR", GB = "GBR", US = "USA"
)

# Code above is made to keep only the countries from the OECD 

summary_table_OCDE <- summary_table_OCDE %>%
  mutate(ctry_code = recode(ctry_code, !!!code_mapping))

output_file_OCDE <- file.path(data_dir, "data_patents_OCDE.xlsx")  #create the excel file for the merged data about patents in the OECD
write_xlsx(summary_table_OCDE, output_file_OCDE)

print(paste("excel file about patents and OECD saved here : ", output_file_OCDE))

countries_to_keep_G7 <- c("CA","FR", "DE","IT", "JP", "GB", "US")

summary_table_G7 <- summary_table[ctry_code %in% countries_to_keep_G7]

code_mapping_G7 <- c( CA = "CAN", FR = "FRA", DE = "DEU", IT = "ITA", JP = "JPN", GB = "GBR", US = "USA"
)

summary_table_G7 <- summary_table_G7 %>%
  mutate(ctry_code = recode(ctry_code, !!!code_mapping_G7))

output_file_G7 <- file.path(data_dir, "data_patents_G7.xlsx") #same but to create the merged data table for the G7 countries
write_xlsx(summary_table_G7, output_file_G7)

countries_to_keep_G7_CN <- c("CA","FR", "DE","IT", "JP", "GB", "US", "CN")

summary_table_G7_CN <- summary_table[ctry_code %in% countries_to_keep_G7_CN]

code_mapping_G7_CN <- c( CA = "CAN", FR = "FRA", DE = "DEU", IT = "ITA", JP = "JPN", GB = "GBR", US = "USA", CN="CHN"
)

summary_table_G7_CN <- summary_table_G7_CN %>%
  mutate(ctry_code = recode(ctry_code, !!!code_mapping_G7_CN))

output_file_G7_CN <- file.path(data_dir, "data_patents_G7_CN.xlsx") #same but including China
write_xlsx(summary_table_G7_CN, output_file_G7_CN)


countries_to_keep_OECD_CN <- c("AU", "AT", "BE", "BG", "CA", "HR", "CY", "CZ", "DK", 
                            "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "JP", 
                            "KR", "LV", "LT", "LU", "MT", "NL", "NO", "PL", "PT", 
                            "SK", "SI", "ES", "SE", "CH", "TR", "GB", "US","CN")

summary_table_OECD_CN <- summary_table[ctry_code %in% countries_to_keep_OECD_CN]

code_mapping_OECD_CN <- c(
  AU = "AUS", AT = "AUT", BE = "BEL", BG = "BGR", CA = "CAN",
  HR = "HRV", CY = "CYP", CZ = "CZE", DK = "DNK", EE = "EST",
  FI = "FIN", FR = "FRA", DE = "DEU", GR = "GRC", HU = "HUN",
  IE = "IRL", IT = "ITA", JP = "JPN", KR = "KOR", LV = "LVA",
  LT = "LTU", LU = "LUX", MT = "MLT", NL = "NLD", NO = "NOR",
  PL = "POL", PT = "PRT", SK = "SVK", SI = "SVN", ES = "ESP",
  SE = "SWE", CH = "CHE", TR = "TUR", GB = "GBR", US = "USA",CN="CHN"
)

summary_table_OECD_CN <- summary_table_OECD_CN %>%
  mutate(ctry_code = recode(ctry_code, !!!code_mapping_OECD_CN))

output_file_OECD_CN <- file.path(data_dir, "data_patents_OECD_CN.xlsx") #last are the OECD countries with China observations
write_xlsx(summary_table_OECD_CN, output_file_OECD_CN)

#create tfp excel files---------------------------------------------------------

existing_files_tfp <- file.exists(file.path(data_dir,"pwt1001.xlsx"))

if (all(existing_files_tfp)) {
  cat("All necessary files are present in the folder.\n")
} else {
  cat("Some files are missing. Downloading...\n")
} 

rm(existing_files_tfp)

# code above checks if all the files needed are loaded and installed

fichier_TFP<- file.path(data_dir, "pwt1001.xlsx")

data_TFP <- read_xlsx(fichier_TFP, sheet = 3)

countries_OECD <- c("AUS", "AUT", "BEL", "BGR", "CAN", "HRV", "CYP", "CZE", "DNK", 
                    "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "JPN", 
                    "KOR", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", 
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

data_filtered_OCDE <- data_TFP %>%
  select(country, countrycode, year, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna) %>%
  filter(countrycode %in% countries_OECD) %>%
  filter(year >= 1990 & year <= 2019)

data_filtered_OCDE<- data_filtered_OCDE %>%
  rename(ctry_code = countrycode, prio_year = year)

countries_OECD_CN <- c("AUS", "AUT", "BEL", "BGR", "CAN", "HRV", "CYP", "CZE", "DNK", 
                    "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "JPN", 
                    "KOR", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", 
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA","CHN")

data_filtered_OCDE_CN <- data_TFP %>%
  select(country, countrycode, year, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna) %>%
  filter(countrycode %in% countries_OECD_CN) %>%
  filter(year >= 1990 & year <= 2019)

data_filtered_OCDE_CN<- data_filtered_OCDE_CN %>%
  rename(ctry_code = countrycode, prio_year = year)

#merge tfp with patents---------------------------------------------------------

fichier_patents_OCDE<- file.path(data_dir, "data_patents_OCDE.xlsx")

data_patents_OCDE<- read_excel(fichier_patents_OCDE, sheet =1)

merged_data_OCDE <- data_patents_OCDE %>%
  inner_join(data_filtered_OCDE, by = c("ctry_code" = "ctry_code", "prio_year" = "prio_year"))

merged_data_OCDE <- merged_data_OCDE %>%
  rename(nbr_patents = nb_pct_nbr) %>%
  select(country, ctry_code, prio_year, nbr_patents, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna)

output_merged_OCDE <- file.path(data_dir, "data_merged_OCDE.xlsx")
write_xlsx(merged_data_OCDE, output_merged_OCDE)



fichier_patents_OCDE_CN<- file.path(data_dir, "data_patents_OECD_CN.xlsx")

data_patents_OCDE_CN<- read_excel(fichier_patents_OCDE_CN, sheet =1)

merged_data_OCDE_CN <- data_patents_OCDE_CN %>%
  inner_join(data_filtered_OCDE_CN, by = c("ctry_code" = "ctry_code", "prio_year" = "prio_year"))

merged_data_OCDE_CN <- merged_data_OCDE_CN %>%
  rename(nbr_patents = nb_pct_nbr) %>%
  select(country, ctry_code, prio_year, nbr_patents, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna)

output_merged_OCDE_CN <- file.path(data_dir, "data_merged_OCDE_CN.xlsx")
write_xlsx(merged_data_OCDE_CN, output_merged_OCDE_CN)



fichier_patents_G7_CN<- file.path(data_dir, "data_patents_G7_CN.xlsx")

data_patents_G7_CN<- read_excel(fichier_patents_G7_CN, sheet =1)

merged_data_G7_CN <- data_patents_G7_CN %>%
  inner_join(data_filtered_OCDE_CN, by = c("ctry_code" = "ctry_code", "prio_year" = "prio_year"))

merged_data_G7_CN <- merged_data_G7_CN %>%
  rename(nbr_patents = nb_pct_nbr) %>%
  select(country, ctry_code, prio_year, nbr_patents, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna)

output_merged_G7_CN <- file.path(data_dir, "data_merged_G7_CN.xlsx")
write_xlsx(merged_data_G7_CN, output_merged_G7_CN)



fichier_patents_G7<- file.path(data_dir, "data_patents_G7.xlsx")

data_patents_G7<- read_excel(fichier_patents_G7, sheet =1)

merged_data_G7 <- data_patents_G7 %>%
  inner_join(data_filtered_OCDE_CN, by = c("ctry_code" = "ctry_code", "prio_year" = "prio_year"))

merged_data_G7 <- merged_data_G7 %>%
  rename(nbr_patents = nb_pct_nbr) %>%
  select(country, ctry_code, prio_year, nbr_patents, ctfp, rtfpna, rgdpna, avh, pop, emp,rnna)

output_merged_G7 <- file.path(data_dir, "data_merged_G7.xlsx")
write_xlsx(merged_data_G7, output_merged_G7)


#graph for descriptive statistics(each part is a code for a graph that we used for the descriptive statistics---------------------------------------------------------------

#if you were not able to run the code before, here is a code to download the data needed for the following steps


folder_id <- "1EFy14_ZIF_Q1AlZXWtqiItlbT_2CUCyT"
files_in_folder <- drive_ls(as_id(folder_id))

file_ids <- files_in_folder[["id"]]
file_names <- files_in_folder[["name"]]  

walk2(file_ids, file_names, ~ drive_download(as_id(.x), path = file.path(data_dir, .y), overwrite = TRUE))

fichier_data_merged_OCDE_CN<- file.path(data_dir, "data_merged_OCDE_CN.xlsx")
fichier_data_merged_OCDE<- file.path(data_dir, "data_merged_OCDE.xlsx")
fichier_data_merged_G7_CN<- file.path(data_dir, "data_merged_G7_CN.xlsx")
fichier_data_merged_G7<- file.path(data_dir, "data_merged_G7.xlsx")


merged_data_OCDE_CN <- read_xlsx(fichier_data_merged_OCDE_CN, sheet = 1)
merged_data_OCDE <- read_xlsx(fichier_data_merged_OCDE, sheet = 1)
merged_data_G7_CN <- read_xlsx(fichier_data_merged_G7_CN, sheet = 1)
merged_data_G7 <- read_xlsx(fichier_data_merged_G7, sheet = 1)



#Evolution of the total number of patents per year(OECD & CHN)---------------------------------


data_summary_total <- merged_data_OCDE_CN %>%
  group_by(prio_year) %>%
  summarise(total_patents = sum(nbr_patents, na.rm = TRUE))%>%
  mutate(Group = "OECD + China")

ggplot(data_summary_total, aes(x = prio_year, y = total_patents)) +
  geom_line(color = "black", size = 1) +  
  geom_point(color = "black", size = 2) + 
  labs(
    title = "Evolution of the total number of patents per year",
    x = "Year",
    y = "Total number of patents"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



#Evolution of the total number of patents per year(OECD)-----------------------------------------------------------------------------------------------


data_summary_OCDE <- merged_data_OCDE %>%
  group_by(prio_year) %>%
  summarise(total_patents = sum(nbr_patents, na.rm = TRUE))%>%
  mutate(Group = "OECD")

ggplot(data_summary_OCDE, aes(x = prio_year, y = total_patents)) +
  geom_line(color = "black", size = 1) +  
  geom_point(color = "black", size = 2) + 
  labs(
    title = "Evolution of the total number of patents per year in the OECD",
    x = "Year",
    y = "Total number of patents"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) + 
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
)



#difference in number of patents between only OCDE and OECD + CHN-------------------------------------------------------------------------


data_combined <- bind_rows(data_summary_total, data_summary_OCDE)

ggplot(data_combined, aes(x = prio_year, y = total_patents, color = Group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("OECD" = "#1f77b4", "OECD + China" = "black")) +  
  labs(
    title = "Evolution of the total number of patents per year",
    x = "Year",
    y = "Total number of patents",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#difference in number of patents between G7 and G7 + CHN--------------------------------------------------------------------


data_summary_G7 <- merged_data_G7 %>%
  group_by(prio_year) %>%
  summarise(total_patents = sum(nbr_patents, na.rm = TRUE))%>%
  mutate(Group = "G7")

data_summary_G7_CN <- merged_data_G7_CN %>%
  group_by(prio_year) %>%
  summarise(total_patents = sum(nbr_patents, na.rm = TRUE))%>%
  mutate(Group = "G7 + China")

data_combined_G7 <- bind_rows(data_summary_G7_CN, data_summary_G7)

ggplot(data_combined_G7, aes(x = prio_year, y = total_patents, color = Group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("G7" = "#1f77b4", "G7 + China" = "black")) +  
  labs(
    title = "Evolution of the total number of patents per year",
    x = "Year",
    y = "Total number of patents",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#difference between each country of the OECD + CHN in number of patents(emphasize on CHN)----------------------------------------------------------------------


countries_G7 <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
merged_data_G7_CN <- merged_data_G7_CN %>%
  mutate(Group = ifelse(ctry_code %in% countries_G7, "G7", "China"))

color_palette_G7 <- colorRampPalette(c("#e0f3f8", "#9ecae1", "#3182bd", "#08519c"))(length(countries_G7))

country_colors <- setNames(color_palette_G7, countries_G7)
country_colors["CHN"] <- "black" 

ggplot(merged_data_G7_CN, aes(x = prio_year, y = nbr_patents, color = ctry_code, group = ctry_code)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = country_colors) +  
  labs(
    title = "Evolution of the number of patents per year by country (G7 & China)",
    x = "Year",
    y = "Total number of patents",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#difference between each country of the OECD + CHN in number of patents (emphasize on CHN and US)-----------------------------------------------------------------------------------------------


countries_G7 <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR")

merged_data_G7_CN <- merged_data_G7_CN %>%
  mutate(Group = ifelse(ctry_code %in% countries_G7, "G7", "China"))

color_palette_G7 <- colorRampPalette(c("#e0f3f8", "#9ecae1", "#3182bd", "#08519c"))(length(countries_G7))

country_colors_G7 <- setNames(color_palette_G7, countries_G7)

country_colors_G7["CHN"] <- "black"  
country_colors_G7["USA"] <- "#4d4d4d"  

ggplot(merged_data_G7_CN, aes(x = prio_year, y = nbr_patents, color = ctry_code, group = ctry_code)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = country_colors_G7) +  
  labs(
    title = "Evolution of the number of patents per year by country (G7 & China)",
    x = "Year",
    y = "Total number of patents",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#difference between each country of the OECD + CHN in number of patents (emphasize on JPN)----------------------------------------------------------------


countries_G7 <- c("CAN", "FRA", "DEU", "ITA","CHN", "GBR", "USA")
merged_data_G7_CN <- merged_data_G7_CN %>%
  mutate(Group = ifelse(ctry_code %in% countries_G7, "G7", "China"))

color_palette_G7 <- colorRampPalette(c("#e0f3f8", "#9ecae1", "#3182bd", "#08519c"))(length(countries_G7))

country_colors <- setNames(color_palette_G7, countries_G7)
country_colors["JPN"] <- "black" 

ggplot(merged_data_G7_CN, aes(x = prio_year, y = nbr_patents, color = ctry_code, group = ctry_code)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = country_colors) +  
  labs(
    title = "Evolution of the number of patents per year by country (G7 & China)",
    x = "Year",
    y = "Total number of patents",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Evolution of TFP(RTFPNA) by country (G7 & China) per year----------


ggplot(merged_data_G7_CN, aes(x = prio_year, y = rtfpna, color = ctry_code, group = ctry_code)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_color_manual(values = country_colors_G7) +  
  labs(
    title = "Evolution of TFP by country (G7 & China) per year",
    x = "Year",
    y = "TFP",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Evolution of TFP(RTFPNA) by country (G7 & China) per year (change in color and legend for better understanding)-----


color_palette <- brewer.pal(8, "Set2")

ggplot(merged_data_G7_CN, aes(x = prio_year, y = rtfpna, color = ctry_code, group = ctry_code)) +
  geom_line(aes(linetype = ctry_code), size = 1) +
  geom_point(size = 3) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = color_palette) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash", "dotdash", "longdash", "solid", "solid")) +  # Types de lignes différents
  labs(
    title = "Evolution of TFP by country (G7 & China) per year",
    x = "Year",
    y = "TFP (at constant national prices)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"  
  )


#Evolution of TFP(RTFPNA) by country (G7 & China) per year (focus on the period between 2008 and 2019) -------------------------


ggplot(merged_data_G7_CN, aes(x = prio_year, y = rtfpna, color = ctry_code, group = ctry_code)) +
  geom_line(aes(linetype = ctry_code), size = 1) +  
  geom_point(size = 3) +  
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = color_palette) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash", "dotdash", "longdash", "solid", "solid")) +  # Types de lignes différents
  labs(
    title = "Evolution of TFP by country (G7 & China) per year",
    x = "Year",
    y = "TFP (at constant national prices)",
    color = "Country"
  ) +
  coord_cartesian(xlim = c(2008, 2019)) + 
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"  
  )


#Evolution in the number of patents by country (G7 & China) per year (change in color for better understanding) ----------


ggplot(merged_data_G7_CN, aes(x = prio_year, y = nbr_patents, color = ctry_code, group = ctry_code)) +
  geom_line(aes(linetype = ctry_code), size = 1) +  
  geom_point(size = 3) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_color_manual(values = color_palette) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash", "dotdash", "longdash", "solid", "solid")) +  # Types de lignes différents
  labs(
    title = "Evolution of the number of patents per country (G7 & China) per year",
    x = "Year",
    y = "Number of Patents",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"  
  )


#Evolution in the number of patents by country (G7 & China) per year (focus on the period between 2008 and 2019)-------------- 


ggplot(merged_data_G7_CN, aes(x = prio_year, y = nbr_patents, color = ctry_code, group = ctry_code)) +
  geom_line(aes(linetype = ctry_code), size = 1) +  
  geom_point(size = 3) +  
  scale_y_continuous(labels = scales::comma) +  
  scale_color_manual(values = color_palette) +  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash", "dotdash", "longdash", "solid", "solid")) +  # Types de lignes différents
  labs(
    title = "Evolution of the number of patents per country (G7 & China) per year",
    x = "Year",
    y = "Number of Patents",
    color = "Country"
  ) +
  coord_cartesian(xlim = c(2008, 2019)) +  
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"  
  )


#different graph for each country of the G7 + CHN (TFP)-------------------------


ggplot(merged_data_G7_CN, aes(x = prio_year, y = rtfpna, color = ctry_code)) +
  geom_line(color = "black", size = 1) +  
  geom_point(color = "black", size = 2) +
  geom_smooth(color = "red",method = "lm", se = FALSE, aes(group = ctry_code))+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ ctry_code) + 
  labs(
    title = "Evolution of TFP by country (G7 & China) per year",
    x = "Year",
    y = "TFP (at constant national prices)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")  
  )



#different graph for each country of the G7 + CHN (relationship between TFP and number of patents)-----------


ggplot(merged_data_G7_CN, aes(x = prio_year)) +
  geom_line(aes(y = rtfpna, color = "TFP (RTFPNA)"), size = 1.2)+
  geom_line(aes(y = nbr_patents / max(nbr_patents, na.rm = TRUE), color = "Nombre de brevets"), size = 1.2) +
  scale_y_continuous(
    name = "TFP (RTFPNA)",
    sec.axis = sec_axis(~ . * max(merged_data_G7_CN$nbr_patents, na.rm = TRUE), name = "Nombre de brevets")
  ) +
  scale_color_manual(values = c("TFP (RTFPNA)" = "black", "Nombre de brevets" = "#1f77b4")) +
  labs(
    x = "Année", 
    y = "TFP (RTFPNA)",
    title = "Evolution du TFP et du nombre de brevets par pays",
    color = "Légende"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 11),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  facet_wrap(~ ctry_code, scales = "free_y", labeller = labeller(ctry_code = label_value))


# statistical analysis------------------------------------------------------------

#if you were not able to run the code before, here is a code to download the data needed for the following steps

drive_auth()

folder_id <- "1EFy14_ZIF_Q1AlZXWtqiItlbT_2CUCyT"
files_in_folder <- drive_ls(as_id(folder_id))

file_ids <- files_in_folder[["id"]]
file_names <- files_in_folder[["name"]]  

walk2(file_ids, file_names, ~ drive_download(as_id(.x), path = file.path(data_dir, .y), overwrite = TRUE))

fichier_data_merged_OCDE_CN<- file.path(data_dir, "data_merged_OCDE_CN.xlsx")
fichier_data_merged_OCDE<- file.path(data_dir, "data_merged_OCDE.xlsx")
fichier_data_merged_G7_CN<- file.path(data_dir, "data_merged_G7_CN.xlsx")
fichier_data_merged_G7<- file.path(data_dir, "data_merged_G7.xlsx")


merged_data_OCDE_CN <- read_xlsx(fichier_data_merged_OCDE_CN, sheet = 1)
merged_data_OCDE <- read_xlsx(fichier_data_merged_OCDE, sheet = 1)
merged_data_G7_CN <- read_xlsx(fichier_data_merged_G7_CN, sheet = 1)
merged_data_G7 <- read_xlsx(fichier_data_merged_G7, sheet = 1)

#correlation--------------------------------------------------------------

head(merged_data_G7_CN)

data_correlation <- merged_data_G7_CN %>%
  select(ctry_code,prio_year, nbr_patents, rtfpna)

ggplot(data_correlation, aes(x = nbr_patents, y = rtfpna)) +
  geom_point(aes(color = ctry_code), size = 2) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(
    title = "Relationship between the number of patents and the evolution of the RTFPNA",
    x = "Number of patents",
    y = "RTFPNA (Total Factor Productivity)"
  ) +
  theme_minimal()

ggplot(data_correlation, aes(x = nbr_patents, y = rtfpna)) +
  geom_point(aes(color = ctry_code), size = 2) +  
  geom_smooth(method = "lm", color = "black", se = FALSE) +  
  labs(
    title = "Relationship between the number of patents and the evolution of the RTFPNA by country",
    x = "Number of patents",
    y = "RTFPNA (Total Factor Productivity)"
  ) +
  facet_wrap(~ ctry_code) + 
  theme_minimal()



corr_by_country <- data_correlation %>%
  group_by(ctry_code) %>%
  summarise(correlation = cor(rtfpna, nbr_patents, use = "complete.obs"))

print(corr_by_country)


data_1990_2008 <- data_correlation %>%
  filter(prio_year >= 1990 & prio_year <= 2008)

corr_by_country_1990_2008 <- data_1990_2008 %>%
  group_by(ctry_code) %>%
  summarise(correlation = cor(rtfpna, nbr_patents, use = "complete.obs"))

print(corr_by_country_1990_2008)



data_2008_2019 <- data_correlation %>%
  filter(prio_year >= 2008 & prio_year <= 2019)

corr_by_country_2008_2019 <- data_2008_2019 %>%
  group_by(ctry_code) %>%
  summarise(correlation = cor(rtfpna, nbr_patents, use = "complete.obs"))

print(corr_by_country_2008_2019)



corr_by_country_1990_2008 <- corr_by_country_1990_2008 %>%
  mutate(Period = "1990-2008")

corr_by_country_2008_2019 <- corr_by_country_2008_2019 %>%
  mutate(Period = "2008-2019")

data_corr_all_2 <- bind_rows(corr_by_country_1990_2008, corr_by_country_2008_2019)

ggplot(data_corr_all_2, aes(x = ctry_code, y = correlation, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("1990-2008" = "#1f77b4", "2008-2019" = "#4d4d4d")) +
  theme_minimal() +
  labs(
    title = "Evolution of the correlation between patents and TFP by country",
    x = "Country",
    y = "Correlation",
    fill = "Period of time"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )























#test to make a regression and understand the correlation between IPC and tfp 






model<- lm(rtfpna ~ nbr_patents, data = merged_data_G7_CN)
summary(model)


merged_data_G7_CN_IPC$Year <- as.factor(merged_data_G7_CN_IPC$prio_year) 
model_time <- lm(rtfpna ~ nb_brevets * Year, data = merged_data_G7_CN_IPC)
summary(model_time)


merged_data_G7_CN_IPC_2 <- pdata.frame(merged_data_G7_CN_IPC, index = c("ctry_code", "Year")) 
model_panel <- plm(rtfpna ~ nb_brevets+emp+rnna, data = merged_data_G7_CN_IPC_2, model = "within") 
summary(model_panel)

model_random <- plm(rtfpna ~ nb_brevets, data = merged_data_G7_CN_IPC, model = "random")
hausman_test <- phtest(model_panel, model_random)
hausman_test

merged_data_G7_CN$Year <- as.numeric(as.character(merged_data_G7_CN$Year))

data_1 <- subset(merged_data_G7_CN, Year <= 2008)
data_2 <- subset(merged_data_G7_CN, Year > 2008)

model_1 <- lm(rtfpna ~ nbr_patents, merged_data_G7_CN = data_1)
model_2 <- lm(rtfpna ~ nbr_patents, merged_data_G7_CN = data_2)

summary(model_1)
summary(model_2)

#try to make relation between TFP and IPC (sector of impact of the patents)---------------------------------------------------------------------------


merged_data_IPC_class<- merge(data_1,data_2, by="pct_nbr", all= TRUE, allow.cartesian = TRUE)

setDT(merged_data_IPC_class)

sum(is.na(merged_data_IPC_class))

merged_data_IPC_class <- na.omit(merged_data_IPC_class)

#to keep only the class of the IPC for inventions
merged_data_IPC_class[, IPC_section_class := substr(IPC, 1, 1)] 

head(merged_data_IPC_class)

#sum of the number of IPC per year and countries 
IPC_summary <- merged_data_IPC_class[, .(nb_brevets = .N), by = .(ctry_code, prio_year, IPC_section_class)] 

setorder(IPC_summary, ctry_code, prio_year, IPC_section_class)

#to keep only the countries we need and be able to use this data table to merge it with the tfp data
IPC_summary <- IPC_summary %>%
  mutate(ctry_code = recode(ctry_code, !!!code_mapping_G7_CN))
IPC_summary_1 <- IPC_summary %>%
  filter(ctry_code %in% code_mapping_G7_CN)%>%
  filter(prio_year >= 1990 & prio_year <= 2019)

#to make it more readable
setorder(IPC_summary_1, ctry_code, prio_year, IPC_section_class)

#check and remove the NA observations because some of the patents were not linked to a class name 
#or at least a section named by a letter at the beginning of the IPC number
sum(IPC_summary_1$IPC_section_class == "")

IPC_summary_1$IPC_section_class <- na_if(IPC_summary_1$IPC_section_class, "")

sum(is.na(IPC_summary_1$IPC_section_class))

IPC_summary_1 <- na.omit(IPC_summary_1)

#merged the TFP data with the IPC data table
merged_data_G7_CN_IPC<- IPC_summary_1 %>%
  inner_join(data_filtered_OCDE_CN, by = c("ctry_code" = "ctry_code", "prio_year" = "prio_year"))

output_merged_G7_CN_IPC <- file.path(data_dir, "data_merged_G7_CN_IPC.xlsx")
write_xlsx(merged_data_G7_CN_IPC, output_merged_G7_CN_IPC)


#relation between IPC sector and number of patents per sector and TFP-------------------------

corr_by_country_ipc <- merged_data_G7_CN_IPC %>%
  group_by(ctry_code, IPC_section_class) %>%
  summarise(correlation = cor(nb_brevets, rtfpna, use = "complete.obs")) %>%
  arrange(ctry_code, desc(correlation))

output_merged_corr_IPC <- file.path(data_dir, "corr_by_country_ipc.xlsx")
write_xlsx(corr_by_country_ipc, output_merged_corr_IPC)


corr_by_country_ipc_1990_2008 <- merged_data_G7_CN_IPC %>%
  filter(prio_year >= 1990 & prio_year <= 2008) %>%
  group_by(ctry_code, IPC_section_class) %>%
  summarise(correlation = cor(rtfpna, nb_brevets, use = "complete.obs"), .groups = "drop")

corr_by_country_ipc_2008_2019 <- merged_data_G7_CN_IPC %>%
  filter(prio_year > 2008 & prio_year <= 2019) %>%
  group_by(ctry_code, IPC_section_class) %>%
  summarise(correlation = cor(rtfpna, nb_brevets, use = "complete.obs"), .groups = "drop")

print(corr_by_country_ipc_1990_2008)
print(corr_by_country_ipc_2008_2019)

corr_by_country_ipc_1990_2008$Period <- "1990-2008"
corr_by_country_ipc_2008_2019$Period <- "2008-2019"

data_corr_all <- bind_rows(corr_by_country_ipc_1990_2008, corr_by_country_ipc_2008_2019)

ggplot(data_corr_all, aes(x = IPC_section_class, y = ctry_code, fill = correlation)) +
  geom_tile() +
  scale_fill_viridis(option = "C", direction = 1) +
  facet_wrap(~Period) +
  theme_minimal() +
  labs(title = "Corrélation entre TFP et nombre de brevets par section IPC",
       fill = "Corrélation",
       x = "Section IPC",
       y = "Pays") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_corr_all, aes(x = IPC_section_class, y = correlation, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ctry_code) +
  theme_minimal() +
  labs(title = "Comparaison des corrélations par pays et section IPC",
       x = "Section IPC",
       y = "Corrélation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ipc_order <- data_corr_all %>%
  group_by(IPC_section_class) %>%
  summarise(mean_corr = mean(correlation, na.rm = TRUE)) %>%
  arrange(desc(mean_corr)) %>%
  pull(IPC_section_class)

data_corr_all <- data_corr_all %>%
  mutate(IPC_section_class = factor(IPC_section_class, levels = ipc_order))

ggplot(data_corr_all, aes(x = IPC_section_class, y = correlation, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(correlation, 2)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  facet_wrap(~ ctry_code, scales = "free_x") +
  scale_fill_manual(values = c("1990-2008" = "#F8766D", "2008-2019" = "#00BFC4")) +
  theme_minimal() +
  labs(
    title = "Évolution de la corrélation entre brevets (par section IPC) et TFP",
    x = "Section IPC",
    y = "Corrélation",
    fill = "Période"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Filtrer les données pour chaque période
data_1990_2008 <- merged_data_G7_CN_IPC %>%
  filter(prio_year >= 1990 & prio_year <= 2008) %>%
  group_by(ctry_code, prio_year, IPC_section_class) %>%
  summarise(total_brevets = sum(nb_brevets, na.rm = TRUE), .groups = 'drop')

data_2008_2019 <- merged_data_G7_CN_IPC %>%
  filter(prio_year >= 2008 & prio_year <= 2019) %>%
  group_by(ctry_code, prio_year, IPC_section_class) %>%
  summarise(total_brevets = sum(nb_brevets, na.rm = TRUE), .groups = 'drop')

# Ajouter une colonne pour différencier les périodes
data_1990_2008$Period <- "1990-2008"
data_2008_2019$Period <- "2008-2019"

# Fusionner les deux jeux de données
data_combined <- bind_rows(data_1990_2008, data_2008_2019)

# Créer le graphique
ggplot(data_combined, aes(x = prio_year, y = total_brevets, fill = IPC_section_class)) +
  geom_col(position = "stack") +
  facet_grid(rows = vars(ctry_code), cols = vars(Period), scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Évolution du nombre de brevets par classe IPC",
    subtitle = "Comparaison des périodes 1990-2008 et 2008-2019",
    x = "Année",
    y = "Nombre total de brevets",
    fill = "Classe IPC"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(data_combined, aes(x = prio_year, y = total_brevets, color = IPC_section_class)) +
  geom_line(size = 1) +
  facet_wrap(~ ctry_code) +
  theme_minimal() +
  labs(
    title = "Évolution du nombre de brevets par classe IPC",
    subtitle = "Comparaison des périodes 1990-2008 et 2008-2019",
    x = "Année",
    y = "Nombre total de brevets",
    color = "Classe IPC"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data_combined, aes(x = prio_year, y = IPC_section_class, fill = total_brevets)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(~ ctry_code) +
  theme_minimal() +
  labs(
    title = "Répartition des brevets par classe IPC",
    subtitle = "Comparaison des périodes 1990-2008 et 2008-2019",
    x = "Année",
    y = "Classe IPC",
    fill = "Nombre de brevets"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_combined, aes(x = IPC_section_class, y = total_brevets, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ctry_code) +
  theme_minimal() +
  labs(
    title = "Comparaison du nombre de brevets par classe IPC",
    subtitle = "Entre 1990-2008 et 2008-2019",
    x = "Classe IPC",
    y = "Nombre total de brevets",
    fill = "Période"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_combined %>% filter(ctry_code %in% c("CHN", "JPN", "USA", "ITA")), 
       aes(x = IPC_section_class, y = total_brevets, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ctry_code) +
  theme_minimal() +
  labs(
    title = "Comparison of the number of patents by IPC class",
    subtitle = "Between 1990-2008 and 2008-2019",
    x = "IPC class",
    y = "Total number of patents",
    fill = "Period of time"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("1990-2008" = "#1f77b4", "2008-2019" = "#4d4d4d"))

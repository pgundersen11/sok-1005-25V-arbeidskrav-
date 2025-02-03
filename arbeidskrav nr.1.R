
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)

df_1 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tlt/uahncdc_lt_6.1.txt")

df_2 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tmt/uahncdc_mt_6.1.txt")

df_3 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/ttp/uahncdc_tp_6.1.txt")

df_4 <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tls/uahncdc_ls_6.1.txt")

# Velg kun den tredje kolonnen ("Global") fra hvert datasett
df_1_global <- df_1 %>% select(3)
df_2_global <- df_2 %>% select(3)
df_3_global <- df_3 %>% select(3)
df_4_global <- df_4 %>% select(3)


# Funksjon for å rengjøre data og beregne 12-måneders glidende gjennomsnitt
clean_and_calculate <- function(df) {
  df <- df %>%
    filter(!grepl("[A-Za-z]", Globe)) %>%  # Fjern rader med ikke-numeriske verdier
    mutate(
      Globe = as.numeric(Globe),          # Konverter til numerisk
      Rolling_Avg = rollmean(Globe, 12, fill = NA, align = "right")  # Beregn glidende gjennomsnitt
    )
  return(df)
}

# Bruk funksjonen på alle datasettene
df_1_global <- clean_and_calculate(df_1_global)
df_2_global <- clean_and_calculate(df_2_global)
df_3_global <- clean_and_calculate(df_3_global)
df_4_global <- clean_and_calculate(df_4_global)


# Kombiner glidende gjennomsnitt fra alle datasettene
combined_data <- data.frame(
  Date = seq.Date(from = as.Date("1978-01-01"), by = "month", length.out = nrow(df_1_global)),
  Globe_1 = df_1_global$Rolling_Avg,
  Globe_2 = df_2_global$Rolling_Avg,
  Globe_3 = df_3_global$Rolling_Avg,
  Globe_4 = df_4_global$Rolling_Avg
)

# Beregn gjennomsnittet for alle datasettene
combined_data <- combined_data %>%
  mutate(Average = rowMeans(select(., Globe_1:Globe_4), na.rm = TRUE))

library(ggplot2)

# Filtrer data for perioden fra januar 1980
combined_data_filtered <- combined_data %>%
  filter(Date >= as.Date("1980-01-01"))

# Sørg for at alle kolonner er numeriske
combined_data_filtered <- combined_data_filtered %>%
  mutate(
    Globe_1 = as.numeric(Globe_1),
    Globe_2 = as.numeric(Globe_2),
    Globe_3 = as.numeric(Globe_3),
    Globe_4 = as.numeric(Globe_4),
    Average = as.numeric(Average)
  )

combined_data <- combined_data %>%
  filter(!is.na(Globe_1) & !is.na(Globe_2) & !is.na(Globe_3) & !is.na(Globe_4)) %>%
  mutate(Average = rowMeans(select(., Globe_1:Globe_4), na.rm = TRUE))


ggplot(combined_data_filtered, aes(x = Date)) +
  geom_line(aes(y = Globe_1, color = "Globe_1"), size = 1) +
  geom_line(aes(y = Globe_2, color = "Globe_2"), size = 1) +
  geom_line(aes(y = Globe_3, color = "Globe_3"), size = 1) +
  geom_line(aes(y = Globe_4, color = "Globe_4"), size = 1) +
  geom_line(aes(y = Average, color = "Average"), size = 1.2, linetype = "dashed") +
  labs(
    title = "Global temperaturendring med 12-måneders glidende gjennomsnitt",
    x = "Tid",
    y = "Temperaturavvik (°C)",
    color = "Datasett"
  ) +
  scale_color_manual(
    values = c(
      "Globe_1" = "blue",
      "Globe_2" = "green",
      "Globe_3" = "red",
      "Globe_4" = "purple",
      "Average" = "black"
    ),
    labels = c(
      "Globe_1" = "Nedre troposfære",
      "Globe_2" = "Midtre troposfære",
      "Globe_3" = "Tropopausen",
      "Globe_4" = "Nedre stratosfære",
      "Average" = "Gjennomsnitt"
    )
  ) +
  theme_minimal()

# Kort forklaring av grafen:
# Garfen viser hvordan den globale temperaturen endrer seg over tid, basert på en 12-måneders glidende
# gjenomsnitt. De ulike fargete linjene representerer temperaturavvik for forkjellige ag av atmosfæren.
# De svarte stripene viser gjennomsnittet. 


### Load required packages ###
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(writexl)


### Load and clean data ###
ds_bed <- read.csv("../data/Bettenstatistik_bereinigt_v2.csv", sep = ";", stringsAsFactors = FALSE, na.strings = "missing")

# Convert and clean selected columns
ds_bed <- ds_bed %>%
  mutate(
    Anteil.Kinder = as.numeric(gsub(",", ".", Anteil.Kinder)),
    Geburtenrate.im.Kanton = as.numeric(gsub(",", ".", Geburtenrate.im.Kanton)),
    across(c(Anzahl.der.Einwohner.im.Kanton, Anzahl.Kinder.in.der.Großregion), ~ as.numeric(gsub(" ", "", .)))
  )

# Convert relevant variables to numeric (excluding metadata columns)
cols_to_numeric <- setdiff(names(ds_bed), c("Großregion", "Bemerkungen", "Kanton", "Spital...Hôpital", "Datum", "Stadt"))
ds_bed[cols_to_numeric] <- lapply(ds_bed[cols_to_numeric], as.numeric)
ds_bed <- ds_bed[1:29, ]  # keep only hospital-level rows


### Helper function to summarise bed types ###
summarise_beds <- function(data, var_total, var_operated, type_label) {
  data %>%
    group_by(Hospital = `Spital...Hôpital`) %>%
    summarise(
      Total = sum(.data[[var_total]], na.rm = TRUE),
      Operated = sum(.data[[var_operated]], na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c(Total, Operated), names_to = "Status", values_to = "Beds") %>%
    mutate(Type = type_label)
}

# Create summary tables for each bed type
bed_categories <- list(
  ICU      = c("ICU.beds..Total.structural.", "ICU.beds..Total.operated."),
  Neonatal = c("Neonatal.beds..Total.structural.", "Neonatal.beds..Total.operated."),
  Paed     = c("Paediatric.beds..Total.structural.", "Paediatric.beds..Total.operated."),
  IMC      = c("Intermediate.Care.beds..Total.structural.", "Intermediate.Care.beds..Total.operated."),
  Surgery  = c("Surgical.beds..Total.structural.", "Surgical.beds..Total.operated."),
  ShortStay= c("ED.short.stay.unit.beds..Total.structural.", "ED.short.stay.unit.beds..Total.operated."),
  DayCare  = c("Day.care.beds..Total.structural.", "Day.care.beds..Total.operated.")
)

bed_summaries <- lapply(names(bed_categories), function(cat) {
  summarise_beds(ds_bed, bed_categories[[cat]][1], bed_categories[[cat]][2], cat)
})

bed_summary_df <- bind_rows(bed_summaries)


### Plotting ###
plot_beds <- function(df, title) {
  ggplot(df, aes(y = reorder(Hospital, Beds), x = Beds, fill = Status)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Type, scales = "free_x") +
    labs(title = title, y = "Hospital", x = "Number of Beds") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

print(plot_beds(bed_summary_df, "Overview of Bed Types by Hospital"))


### Save summary outputs ###
total_beds_kanton <- ds_bed %>%
  group_by(Kanton) %>%
  summarise(Total_Beds = sum(Total.all.paediatric.beds..Total.structural., na.rm = TRUE))

beds_per_100k_kanton <- ds_bed %>%
  mutate(Beds_per_100k = (Total.all.paediatric.beds..Total.operated. / Anzahl.der.Einwohner.im.Kanton) * 100000) %>%
  group_by(Kanton) %>%
  summarise(Beds_per_100k = sum(Beds_per_100k, na.rm = TRUE))

write.csv(total_beds_kanton, "../output/total_beds_per_kanton.csv", row.names = FALSE)
write.csv(beds_per_100k_kanton, "../output/beds_per_100k_per_kanton.csv", row.names = FALSE)


### Map Visualisation ###
swiss_map <- ne_countries(country = "Switzerland", returnclass = "sf")
ds_bed_sf <- st_as_sf(ds_bed, coords = c("longitude", "latitude"), crs = 4326)

map_plot <- ggplot() +
  geom_sf(data = swiss_map, fill = "grey90", color = "white") +
  geom_sf(data = ds_bed_sf, aes(size = Total.all.paediatric.beds..Total.structural.), color = "steelblue") +
  scale_size_continuous(range = c(3, 10)) +
  theme_void() +
  labs(title = "Distribution of Paediatric Beds in Switzerland")

print(map_plot)


library(qs)
library(here)   # For å lagre koden som en datasett som kan brukes i quarto
library(tidyverse)
library(readxl)
library(stringr)

# 1. Angi mappa der Excel-filene ligger
mappe <- "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/Sammenlignkommunedata/"

# 2. Finn alle .xlsx-filer i mappa
filer <- list.files(
  path = mappe,
  pattern = "\\.xlsx$",         # alle excel-filer
  full.names = TRUE
)

# (Valgfritt) skriv ut filene som ble funnet
print(filer)

# 3. Les inn alle filer og gjør om til long-format
alle_long <- map_df(filer, function(f) {
  
  # hent år fra filnavn, f.eks. "2009" fra "2009.xlsx"
  år <- str_extract(basename(f), "\\d{4}") %>% as.integer()
  
  read_excel(f) %>% 
    select(Utslippskilde, Avløp) %>%  # behold bare disse
    mutate(år = år)                   # årstall som egen variabel
})

# 4. Gjør datasettet bredere med Avløp 2009, Avløp 2011, ...
Utslipp_kom_avløp <- alle_long %>% 
  pivot_wider(
    names_from  = år,
    values_from = Avløp,
    names_glue  = "{år}"
  )

# 5. Sjekk resultatet
View(Utslipp_kom_avløp)


# Etter at Utslipp_kom_avløp er laget:
qs::qsave(Utslipp_kom_avløp, here("data", "Utslipp_kom_avløp.qs"))
qs::qsave(
  Utslipp_kom_avløp,
  "C:/Users/magnudam/OneDrive - Universitetet i Oslo/UIO Master/Data prosjekt/WebMap-main/data/Utslipp_kom_avløp.qs"
)


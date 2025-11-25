# Oslofjorden WebMap

Dette prosjektet er en Quarto-nettside som visualiserer den økologiske tilstanden i Oslofjorden.

## Installér nødvendige R-pakker

Kjør dette i R:

``` r
install.packages(c(
  "sf", "leaflet", "dplyr", "stringr",
  "qs", "here", "htmltools"
))
```

## Kjør nettsiden lokalt

``` r
quarto::quarto_render()
```

library(tidyverse)
library(jsonlite)

url <- "https://talnaefni.fasteignaskra.is/talnaefni/v1/eignarhaldibuda"

d_json <- fromJSON(url)


d <- d_json$gögn |> 
  as_tibble() |> 
  unnest_longer(everything()) |> 
  mutate(
    date = d_json$date
  ) |> 
  select(date, everything()) |> 
  mutate_all(
    parse_number
  )  |> 
  select(
    date,
    ein_einst_hlutf = hlutfall_ein_einstaklingur,
    ein_log_hlutf = hlutfall_ein_logadilar,
    fleir_einst_hlutf = hlutfall_fleiri_en_ein_einstaklingur,
    fleir_log_hlutf = hlutfall_fleiri_en_ein_logadilar,
    ein_einst_fjoldi = fjoldi_ein_einstaklingur,
    ein_log_fjoldi = fjoldi_ein_logadilar,
    fleir_einst_fjoldi = fjoldi_fleiri_en_ein_einstaklingur,
    fleir_log_fjoldi = fjoldi_fleiri_en_ein_logadilar
  ) |> 
  pivot_longer(
    c(-date), 
    names_to = c("fjoldi", "eigandi", "tegund"),
    names_sep = "_"
  ) |> 
  mutate(
    eigandi = fct_recode(
      eigandi,
      "Einstaklingar" = "einst",
      "Lögaðilar" = "log"
    ),
    fjoldi = fct_recode(
      fjoldi,
      "Sem eiga eina íbúð" = "ein",
      "Sem eiga fleiri en eina íbúð" = "fleir"
    )
  )

d |> 
  write_csv("data/data.csv")
library(tidyverse)

recife <- read_csv("basico-censo-recife-utf8.csv") %>%
  select(Cod_setor)

domicilios <- read_csv2("CSV/Domicilio01_PE.csv") %>%
  filter(Cod_setor %in% recife$Cod_setor) %>%
  subset(select = c(Cod_setor, V062:V099)) %>%
  mutate_if(is.character,as.numeric)

homens <- domicilios %>%
  subset(select = c(Cod_setor, V062:V080)) %>%
  mutate(sum_homens = rowSums(.[2:20])) %>%
  select(Cod_setor, sum_homens)

mulheres <- domicilios %>%
  subset(select = c(Cod_setor, V081:V099)) %>%
  mutate(sum_mulheres = rowSums(.[2:20])) %>%
  select(Cod_setor, sum_mulheres)

domis <- homens %>%
  inner_join(mulheres, by = "Cod_setor") %>% 
  mutate(maioria = case_when(
    sum_mulheres - sum_homens > 0 ~ "mulheres",
    sum_mulheres - sum_homens < 0 ~ "homens",
    sum_mulheres - sum_homens == 0 ~ "empate"
  )) %>%
  write_csv("domicilios.csv")


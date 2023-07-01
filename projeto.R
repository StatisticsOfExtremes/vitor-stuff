setwd("~/projects/base-victor/")

library(tidyverse)
library(dplyr)
library(lubridate)
library(basedosdados)
set_billing_id("testing-server-327310")

dados <- readr::read_delim("1o-ciclo-base-de-dados-2016-2-semestre.csv", delim = ";") %>% janitor::clean_names()


#FUNCTION TO READ ALL THE BASES
{
  things <- function(size) {
    list(
      i = c(1:{size}),
      ano = purrr::map_dbl(2016:(2016 + {size}), \(x) rep(x, 2)),
      semestre = rep(c(2, 1), {size}/2))
  }
  
  View(things(10))
  
  nomes <- function(i, ano, semestre) {
    paste0(i, "o-ciclo-base-de-dados-", ano, "-", semestre, "-semestre", ".csv")
  }
  
  paths <- purrr::pmap(things(10),\(i, ano, semestre) nomes(i, ano, semestre))
  
  map_df(paths, \(path) readr::read_delim(path, delim = ";"))
  
}

final_do_ano <- function(year) {
  dmy(paste0("31-12-", year))
}

dados2 <- dados %>% 
  select(nome_do_estabelecimento,
         situacao_do_estabelecimento,
         ambito, uf, municipio, codigo_ibge,
         x1_1_estabelecimento_originalmente_destinado_a_pessoa_privadas_de_liberdade_do_sexo,
         x1_6_data_de_inauguracao_do_estabelecimento,
         starts_with("x4_1") & contains("justica_estadual") & !ends_with("total"),
         starts_with("x4_1") & contains("justica_federal") & !ends_with("total"), 
         starts_with("x4_1") & !contains(c("justica_estadual", "justica_estadual")) & !ends_with("total")
         ) %>% filter(situacao_do_estabelecimento == "Ativo") %>% 
  rename(presidio_tipo = x1_1_estabelecimento_originalmente_destinado_a_pessoa_privadas_de_liberdade_do_sexo,
         data_inauguracao = x1_6_data_de_inauguracao_do_estabelecimento)


dados2<- dados2 %>% 
  mutate(
  ano_dado = 2016,
  tempo_presidio_2000 = round(lubridate::time_length(lubridate::interval(lubridate::my("01-2000"), lubridate::dmy(data_inauguracao)), "years")),
  tempo_presidio_2010 = round(lubridate::time_length(lubridate::interval(lubridate::my("01-2010"), lubridate::dmy(data_inauguracao)), "years")),
  tempo_presidio_1990 = round(lubridate::time_length(lubridate::interval(lubridate::my("01-1990"), lubridate::dmy(data_inauguracao)), "years")),
  presidio_2000 = if_else(dmy(data_inauguracao) >= my("01-2000"), 1, 0),
  presidio_2000 = if_else(dmy(data_inauguracao) >= my("01-1990"), 1, 0),
  presidio_2000 = if_else(dmy(data_inauguracao) >= my("01-1980"), 1, 0),
  presidio_federal = dplyr::if_else(ambito == "Federal", 1, 0),
  presidio_estadual = dplyr::if_else(ambito == "Estadual", 1, 0),
  presidio_masculino = dplyr::if_else(presidio_tipo == "Masculino", 1, 0),
  presidio_misto = dplyr::if_else(presidio_tipo == "Misto", 1, 0)
)

unidades_prisionais <- dados2 %>% 
  group_by(codigo_ibge) %>% 
  summarise(qnt_unidades_prisionais = n())





qnt_presidios <- dados2 %>%
  group_by(codigo_ibge) %>% 
  summarise(
    qnt_presidios_2010 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("2010"), 1, 0)),
    qnt_presidios_2000 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("2000"), 1, 0)),
    qnt_presidios_1990 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("1990"), 1, 0)),
    qnt_presidios_1980 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("1980"), 1, 0)),
    qnt_presidios_1970 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("1970"), 1, 0)),
    qnt_presidios_1960 = sum(if_else(dmy(data_inauguracao) <= final_do_ano("1960"), 1, 0)),
  )



dados2 %<>% 
  rowwise() %>% 
  mutate(
    pop_estadual = sum(c_across(starts_with("x4_1") & contains("justica_estadual") & !ends_with("total")), na.rm = TRUE),
    pop_federal = sum(c_across(starts_with("x4_1") & contains("justica_federal") & !ends_with("total")), na.rm = TRUE), 
    pop_outros = sum(c_across(starts_with("x4_1") & !contains(c("justica_estadual", "justica_estadual")) & !ends_with("total")), na.rm = TRUE)
    )

dados3 <- dados2 %>%
  left_join(unidades_prisionais, by = "codigo_ibge") %>% 
  left_join(qnt_presidios, by = "codigo_ibge") %>% 
  select(!any_of(starts_with("x4_1")))

View(dados3)


municipios %>% select(id_municipio, nome_regiao) %>%
  mutate(id_municipio = as.double(id_municipio)) %>% 
  left_join(dados3, by = c("id_municipio" = "codigo_ibge")) %>%
  mutate(
    norte = if_else(nome_regiao == "Norte", 1, 0),
    nordeste = if_else(nome_regiao == "Nordeste" ,1, 0),
    sudeste = if_else(nome_regiao == "Sudeste", 1, 0),
    sul = if_else(nome_regiao == "Sul" ,1, 0),
    centro_oeste = if_else(nome_regiao == "Centro-Oeste",1, 0))  %>% 
  View(.)

## DADOS DEMOGRÁFICOS
{
  series <- data.frame(
    codigo = c(
      "ADH_RDPC", "MRDPC", "ADH_PEA18M", "IDHM", "IDHMED", "IDHMLO", "IDHMRE"),
  descricao = c("Renda per Capita Média", "Rendimento médio dos ocupados Social", "População economicamente ativa de 18 anos ou mais de idade", "Índice de Desenvolvimento Humano (IDH)", "Índice de Desenvolvimento Humano (IDH) - educação", "Índice de Desenvolvimento Humano (IDH) - longevidade", "Índice de Desenvolvimento Humano (IDH) - renda"))
  
  
  # Defina o seu projeto no Google Cloud

  
  # Para carregar o dado direto no R
  query <- bdplyr("br_bd_diretorios_brasil.municipio")
  municipios <-  bd_collect(query)
  
  
  demograficos <- tibble::tibble(
    series,
    dados = list(ipeadatar::ipeadata(codigo))
  )
  
  
  
}

write.csv(municipios, "codigos_municipios.csv")

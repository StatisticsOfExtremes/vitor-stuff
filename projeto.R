#setwd("~/base-victor")

library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(basedosdados)
library(rlang)
library(xml2)
library(rvest)
library(httr)
library(readxl)
library(readr)


#FUNCTION TO READ ALL THE DBASES
{
  things <- function(size) {
    tibble(
      i = list(c(seq(1, {size * 2} , by = 2), {size * 2})),
      ano = list(c(2016:(2016 + {size}))),
      semestre = list(c(rep(2, {size}), 1))
    )
  }

  
caminhos <-  lapply(things(6), unlist) %>%
    as.data.frame() %>% 
    mutate(path = paste0(i, "o-ciclo-base-de-dados-", ano, "-", semestre, "-semestre", ".csv"))

dados_presidios <- tibble(
    caminhos,
    dados = sapply(path, read.csv, sep = ";")
  )
}


municipios <- read_csv("codigos_municipios.csv")

#DADOS PRESIDIOS
{

gera_dados_presidios <- function(dados, ano, municipios)  {

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
         starts_with("x4_1") & !contains(c("justica_estadual", "justica_estadual")) & !ends_with("total")) %>%
  filter(situacao_do_estabelecimento == "Ativo") %>% 
  rename(presidio_tipo = x1_1_estabelecimento_originalmente_destinado_a_pessoa_privadas_de_liberdade_do_sexo,
         data_inauguracao = x1_6_data_de_inauguracao_do_estabelecimento)


dados2 <- dados2 %>% 
  mutate(
  ano_dado = {ano},
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




presidios_final <- municipios %>% select(id_municipio, nome_regiao) %>%
  mutate(id_municipio = as.double(id_municipio)) %>% 
  left_join(dados3, by = c("id_municipio" = "codigo_ibge")) %>%
  mutate(
    norte = if_else(nome_regiao == "Norte", 1, 0),
    nordeste = if_else(nome_regiao == "Nordeste" ,1, 0),
    sudeste = if_else(nome_regiao == "Sudeste", 1, 0),
    sul = if_else(nome_regiao == "Sul" ,1, 0),
    centro_oeste = if_else(nome_regiao == "Centro-Oeste",1, 0))
}
  
  presidios_final <-  dados_presidios 
}



#DADOS DEMOGRAFICOS
{
## DADOS DEMOGRÁFICOS
{
  series <- data.frame(
    codigo = c(
      "ADH_RDPC",
      "MRDPC",
      "ADH_PEA18M",
      "ADH_T_DES18M",
      "ADH_P_FORMAL",
      "ADH_P_AGRO",
      "ADH_P_COM",
      "ADH_P_CONSTR",
      "ADH_P_EXTR",
      "ADH_P_TRANSF",
      "ADH_P_SERV",
      "ADH_P_FUND",
      "ADH_P_MED",
      "ADH_P_SUPER",
      "ADH_PIND",
      "ADH_PMPOB",
      "ADH_GINI",
      #TODO "log_ADH_PESOTOT",
      "ADH_PESOTOT",
      "ADH_PESORUR",
      "ADH_PESOURB",
      "ADH_T_DENS"
    ),
  descricao = c(
    "Renda per Capita Média",
    "Rendimento médio dos ocupados Social",
    "População economicamente ativa de 18 anos ou mais de idade",
    "Taxa de desocupação da população de 18 anos ou mais de idade",
    "Grau de formalização do trabalho das pessoas ocupadas",
    "Percentual dos ocupados no setor agropecuário",
    "Percentual dos ocupados no setor extrativo mineral",
    "Percentual dos ocupados na indústria de transformação",
    "Percentual dos ocupados no setor serviços",
    "Percentual dos ocupados com fundamental completo",
    "Percentual dos ocupados com médio completo",
    "Ocupados com superior completo",
    "Proporção de extremamente pobres",
    "Proporção de pobres",
    "Índice de Gini",
    "ADH_PESOTOT",
    "ADH_PESORUR",
    "ADH_PESOURB",
    "Percentual da população que vive em domicílios com densidade superior a 2 pessoas por dormitório",
    "a",
    "V"
    )
  )
  }


demograficos <- tibble::tibble(
  dados = list(ipeadatar::ipeadata(series$codigo, "br"))
)

clean_data <- function(data) {
  anos <- c("2010","2000", "1991")
  data %>%
    filter(uname == "Municípios",) %>% 
    mutate(date = substr(date, 1, 4)) %>% 
    filter(date %in% anos)
}


cleaned_demo <- demograficos %>%
  mutate(dados = lapply(dados, clean_data)) %>% 
  unnest() %>%
  select(-uname) %>%
  pivot_wider(names_from = code, values_from = value) 


### UMA HORA EU FIQUEI COM PREGUIÇA E PEGUEI DIRETO DO ATLAS
### FONTE: https://onedrive.live.com/?authkey=%21ABiV0mb1HeyuOxU&id=124653557C0404EC%2123017&cid=124653557C0404EC&parId=root&parQt=sharedby&parCid=90ED43367F5EC97A&o=OneUp


dados_atlas <- read_xlsx('atlas_2013.xlsx', sheet = 2)


colunas_atlas <- c(
  'ANO',
  'Codmun7',
  "IDHM",
  "IDHM_E",
  "IDHM_L",
  "IDHM_R",
  'T_ANALF18M',
  'T_ANALF25M',
  'T_MED18M',
  'T_FUND25M',
  'T_SUPER25M',
  'HOMEM15A19',
  'HOMEM20A24',
  'HOMEM25A29',
  'HOMEM30A34',
  'HOMEM35A39',
  'HOMEM40A44',
  'HOMEM45A49',
  'HOMEM50A54',
  'HOMEM55A59',
  'T_MULCHEFEFIF014',
  'T_AGUA',
  'T_BANAGUA',
  'T_LIXO',
  'T_LUZ'
)

dados_atlas %<>% select(all_of(colunas_atlas))

dados_atlas %<>% 
  mutate(ANO = as.character(ANO))

dados_finais <- left_join(cleaned_demo_wide, dados_atlas, by = c("tcode" = "Codmun7", "date" = "ANO")) 
  }


final <- left_join(presidios_final, dados_finais, by = c("id_municipio" = "tcode"))


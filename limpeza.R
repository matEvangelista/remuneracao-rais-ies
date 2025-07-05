library(tidyr)
library(openxlsx)
library(janitor)
library(dplyr)
library(RPostgres)
library(stringr)
library(geobr)

readRenviron(".env")
DB_NAME=Sys.getenv("DB_NAME")
DB_HOST=Sys.getenv("DB_HOST")
DB_PORT=Sys.getenv("DB_PORT")
DB_USER=Sys.getenv("DB_USER")
DB_PW=Sys.getenv("DB_PW")
DB_SCHEMA=Sys.getenv("DB_SCHEMA")

# driver e conexão
drv <- RPostgres::Postgres()

con <- dbConnect(drv, dbname = DB_NAME,
                 host = DB_HOST, port = DB_PORT,
                 user = DB_USER, password = DB_PW)


# lendo tabelas com empregos
cursos_empregos <- read.csv("dados_sensiveis/dados_manualmente.csv")

cbos_unicos <- cursos_empregos$cbo %>% unique()


# query para selecionar as colunas do CBO
nomes_colunas <- c("cbo_ocupacao_2002", "faixa_etaria", "faixa_hora_contrat",
                   "faixa_remun_media_sm", "faixa_tempo_emprego", "escolaridade_apos_2005",
                   "qtd_hora_contr", "idade", "municipio", "raca_cor", "vl_remun_media_nom",
                   "sexo_trabalhador", "tamanho_estabelecimento",
                   "tipo_admissao", "tipo_estab", "tipo_vinculo")

query_rais <- paste0("select ", paste(nomes_colunas, collapse = " ,"),
                     "\nfrom ae1.rais_microdados \nwhere cbo_ocupacao_2002 in (",
                     paste(cbos_unicos, collapse = ", "), ");")

df_inicial <- dbGetQuery(con, query_rais) %>% clean_names()

# 1o filtro: ensino médio completo
df_inicial_filtro_1 <- df_inicial %>% filter(escolaridade_apos_2005 > 6)
# 2o remuneração acima de 1/2SM de 2023
df_inicial_filtro_2 <- df_inicial_filtro_1 %>% filter(vl_remun_media_nom >= 1302/2)

q1 <- quantile(df_inicial_filtro_2$vl_remun_media_nom, 0.25, na.rm = TRUE)
q3 <- quantile(df_inicial_filtro_2$vl_remun_media_nom, 0.75, na.rm = TRUE)
iqr <-  q3 - q1
limite_outlier <- q3 + 4 * iqr
# 3o filtro: removendo salários muito altos
df_inicial_filtro_3 <- df_inicial_filtro_2 %>% filter(vl_remun_media_nom < limite_outlier)

# 4o filtro: válidos
df_inicial_filtro_4 <- df_inicial_filtro_3 %>% filter(raca_cor != 9,
                                                      municipio != 999999,
                                                      faixa_hora_contrat != 99)

## tabelas auxiliares
tabela_etnias <- data.frame(
  codigo = c(-1, 2, 8, 1, 4, 6, 9),
  etnia = c("Ignorado", "Branca", "Parda", "Indígena", "Preta", "Amarela", "Não identificado")
)

escolaridade_df <- data.frame(
  grau_de_instrucao = c(
    "Analfabeto", "Até 5ª Incompleto", "5ª Completo - Fundamental", 
    "6ª a 9ª - Fundamental", "Fundamental Completo", "Médio Incompleto", 
    "Médio Completo", "Superior Incompleto", "Superior Completo", 
    "Mestrado", "Doutorado", "Ignorado"
  ),
  codigo = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, -1)
)

dicionario_rais <- dbGetQuery(con, "select * from ae1.dicionario_rais;")

#' Criar tabela de equivalência entre dicionários
#'
#' @param dict_df O dicionário a ser consultados (em geral, o da RAIS)
#' @param column_name A coluna a ser filtrada (e.g., 'faixa_etaria')
#' @param value_col_suffix SUfixo para a coluna (default = "_valor")
#'
#' @return Dicionário processado com as colunas
create_dictionary <- function(dict_df, column_name, value_col_suffix = "_valor") {
  value_col_name <- paste0(column_name, value_col_suffix)
  
  dict_df %>%
    filter(nome_coluna == column_name) %>%
    select(chave, valor) %>%
    rename(!!value_col_name := valor) %>%
    mutate(chave = as.integer(chave))
}

#' Join com dicionário e renomear colunas
#'
#' @param data DF principal
#' @param dict_df O dicionário df a ser consultado (em geral, o da rais)
#' @param column_name O nome da coluna a ser processado (e.g., 'faixa_etaria')
#' @param value_col_suffix Sufixo para a coluna (padrão = "_valor")
#'
#' @return Um df com join e colunas renomeadas
join_with_dictionary <- function(data, dict_df, column_name, value_col_suffix = "_valor") {
  value_col_name <- paste0(column_name, value_col_suffix)
  
  dict <- create_dictionary(dict_df, column_name, value_col_suffix)
  
  data %>%
    left_join(dict, by = setNames("chave", column_name)) %>%
    select(-all_of(column_name)) %>%
    rename(!!column_name := all_of(value_col_name))
}

## homem e mulher
df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  mutate(sexo_trabalhador =  ifelse(sexo_trabalhador == 1, "Homem", "Mulher"))

df_inicial_filtro_4 <- df_inicial_filtro_4 %>% left_join(tabela_etnias,
                                                         by=c("raca_cor"="codigo")) %>%
  select(-raca_cor)


df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  join_with_dictionary(dicionario_rais, "faixa_etaria")

df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  join_with_dictionary(dicionario_rais, "tamanho_estabelecimento")

df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  join_with_dictionary(dicionario_rais, "tipo_admissao")

dicionario_rais_vinc <- data.frame(
  fonte = rep("microdados_vinculos", 18),
  nome_coluna = rep("tipo_vinculo", 18),
  chave = c(10, 15, 20, 25, 30, 31, 35, 50, 55, 60, 65, 70, 75, 80, 90, 95, 96, 97),
  periodo = c(
    "1994(1)", "1994(1)", "1994(1)", "1994(1)", "1994(1)", 
    "1994(1)", "1994(1)", "1994(1)2002, 2016(1)2018", "1994(1)", 
    "1994(1)", "1994(1)", "1994(1)", "1994(1)", "1994(1)", 
    "1994(1)", "1994(1)", "1994(1)", "1994(1)"
  ),
  valor = c(
    "CLT U/PJ IND", "CLT U/PF IND", "CLT R/PJ IND", "CLT R/PF IND", 
    "ESTATUTARIO", "ESTAT RGPS", "ESTAT N/EFET", "TEMPORARIO", 
    "APREND CONTR", "CLT U/PJ DET", "CLT U/PF DET", "CLT R/PJ DET", 
    "CLT R/PF DET", "DIRETOR", "CONT PRZ DET", "CONT TMP DET", 
    "CONT LEI EST", "CONT LEI MUN"
  ),
  stringsAsFactors = FALSE
)

df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  join_with_dictionary(dicionario_rais_vinc, "tipo_vinculo")

df_inicial_filtro_4 <- df_inicial_filtro_4 %>% left_join(escolaridade_df,
                                              by=c("escolaridade_apos_2005"="codigo")) %>%
  select(-escolaridade_apos_2005)

df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  mutate(tipo_estab = case_when(
    tipo_estab == 1 ~ "CNPJ",
    tipo_estab == 6 ~ "CNO",
    tipo_estab == 5 ~ "CAEPF",
    TRUE ~ as.character(tipo_estab)  # keeps original value if no match
  ))


municipios <- read_municipality(year=2023)
municipios$code_muni_6 <- (municipios$code_muni/10) %>% floor()


df_inicial_filtro_4 <- df_inicial_filtro_4 %>% 
  left_join(municipios %>% rename_with(~ paste0(., "_municipio"), -code_muni_6),
            by = c("municipio" = "code_muni_6"))


df_inicial_filtro_4 <- df_inicial_filtro_4 %>%
  join_with_dictionary(dicionario_rais, "faixa_tempo_emprego")

df_inicial_filtro_4 <- df_inicial_filtro_4 %>% rename(faixa_horas_contratadas = faixa_hora_contrat) %>%
  join_with_dictionary(dicionario_rais, "faixa_horas_contratadas")

df_final <- df_inicial_filtro_4 %>% rename(faixa_remuneracao_media_sm = faixa_remun_media_sm) %>%
  join_with_dictionary(dicionario_rais, "faixa_remuneracao_media_sm")

df_final <- df_final %>% select(-(matches("geom")))

df_final <- df_final %>% left_join(cursos_empregos %>% select(profissao, cbo) %>% distinct(),
                                   by=c("cbo_ocupacao_2002"="cbo"), relationship = 'many-to-many')

df_final <- df_final %>% rename(c("cbo"="cbo_ocupacao_2002"))

DBI::dbWriteTable(con, DBI::Id(schema=DB_SCHEMA, table="dados_rais_limpos"),
                  df_final, overwrite = TRUE, append = FALSE)

df_final %>% write.csv("dados_sensiveis/dados_rais_limpos.csv", row.names = F)

df_final %>% saveRDS("dados_sensiveis/dados_rais_limpos.rds")

variaveis_df <- data.frame(
  nome_coluna = c(
    "cbo", "cargo", "vl_remun_media_nom", "etnia", "sexo_trabalhador", "idade",
    "grau_de_instrucao", "faixa_etaria", "faixa_remuneracao_media_sm",
    "qtd_hora_contr", "faixa_horas_contratadas", "tipo_vinculo", "tipo_admissao", 
    "tipo_estab", "tamanho_estabelecimento", "faixa_tempo_emprego",
    "municipio", "code_muni_municipio", "name_muni_municipio",
    "code_state_municipio", "abbrev_state_municipio", "name_state_municipio",
    "code_region_municipio", "name_region_municipio", "curso_ies"
  ),
  tipo_coluna = c(
    "fator", "fator", "número", "fator", "fator", "número",
    "fator", "fator", "fator",
    "número", "fator", "fator", "fator", 
    "fator", "fator", "fator",
    "texto", "texto", "texto",
    "texto", "texto", "texto",
    "texto", "texto", "texto"
  ),
  nome_real = c(
    "Código CBO", "Cargo", "Remuneração média nominal", "Etnia", "Sexo", "Idade",
    "Grau de instrução", "Faixa etária", "Faixa de remuneração média (SM)",
    "Horas contratadas", "Faixa de horas contratadas", "Tipo de vínculo", "Tipo de admissão",
    "Tipo de estabelecimento", "Tamanho do estabelecimento", "Faixa de tempo de emprego",
    "Município", "Código do município", "Nome do município",
    "Código do estado", "Sigla do estado", "Nome do estado",
    "Código da região", "Nome da região", "Curso Superior"
  ),
  stringsAsFactors = FALSE
)

variaveis_df %>% write.csv("dados_sensiveis/traducao.csv")



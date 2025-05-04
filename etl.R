library(RPostgres)
library(dplyr)
library(DBI)
library(janitor)
library(stringr)
library(data.table)

# variáveis de ambiente
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

# dados de IES
microdados_curso <- read.csv("MICRODADOS_CADASTRO_CURSOS_2023.CSV",
                             encoding = 'latin1', sep=';')

dbWriteTable(con, DBI::Id(table="microdados_cadastro_curso", schema=DB_SCHEMA), microdados_curso)

microdados_ed_superior <- read.csv("MICRODADOS_ED_SUP_IES_2023.CSV",
                                   sep = ';', encoding = 'latin1')

dbWriteTable(con, DBI::Id(table="microdados_ies", schema=DB_SCHEMA), microdados_ed_superior)

rm(microdados_curso, microdados_ed_superior)

# RAIS
local <- "/rais 2023/"
arquivos <- list.files(local)
arquivos <- arquivos[2:length(arquivos)]

chunk_size <- 500000  # Linhas por lote

for (arquivo in arquivos) {
  file_path <- file.path(local, arquivo)
  file_path_sys <- paste0('"', file_path, '"')  # Para uso no system()
  
  # Leitura do cabeçalho
  header <- fread(file_path, sep = ";", encoding = "Latin-1", dec = ",", nrows = 1)
  header <- janitor::clean_names(header)
  
  # Determinar número total de linhas
  if (.Platform$OS.type == "unix") {
    total_rows <- as.integer(system(paste("wc -l <", file_path_sys), intern = TRUE)) - 1
  } else {
    total_rows <- as.integer(
      system(
        paste('powershell -command "(Get-Content', file_path_sys, ').Count"'),
        intern = TRUE
      )
    ) - 1
  }
  
  if (length(total_rows) == 0 || is.na(total_rows)) {
    message("Não foi possível contar as linhas - usando valor alto como fallback")
    total_rows <- 1e8
  }
  
  rows_processed <- 0
  
  while (rows_processed < total_rows) {
    chunk <- fread(
      file = file_path,
      sep = ";",
      encoding = "Latin-1",
      dec = ",",
      skip = rows_processed + 1,
      nrows = min(chunk_size, total_rows - rows_processed)
    )
    
    if (nrow(chunk) == 0) break
    
    if (ncol(chunk) == length(header)) {
      setnames(chunk, colnames(header))
    } else {
      warning(paste("Número de colunas incompatível no arquivo:", arquivo))
      break
    }
    
    chunk[, source_file := arquivo]
    
    # tratamento da coluna 'nacionalidade'
    if ("nacionalidade" %in% names(chunk)) {
      chunk[, row_id := .I + rows_processed]  # adiciona o número da linha real do arquivo
      
      invalid_nacionalidade <- chunk[
        is.na(suppressWarnings(as.integer(nacionalidade))) & !is.na(nacionalidade),
        .(source_file, row_id, valor = nacionalidade)
      ]
      
      if (nrow(invalid_nacionalidade) > 0) {
        fwrite(invalid_nacionalidade, "erros_nacionalidade.csv", append = file.exists("erros_nacionalidade.csv"))
      }
      
      # Substituir os valores inválidos por NA
      chunk[, nacionalidade := suppressWarnings(as.integer(nacionalidade))]
      
      chunk[, row_id := NULL]  # remove coluna temporária
    }
    
    # Inserção no banco com tratamento de erro
    tryCatch({
      dbWriteTable(
        con,
        name = DBI::Id(schema = DB_SCHEMA, table = "rais_microdados"),
        value = chunk,
        append = TRUE
      )
    }, error = function(e) {
      message("Erro ao gravar no banco: ", e$message)
      break
    })
    
    rows_processed <- rows_processed + nrow(chunk)
    print(paste("Processadas", rows_processed, "/", total_rows, "linhas do arquivo", arquivo))
  }
}

# CBO
local <- "ESTRUTURA CBO/"
arquivos <- list.files(local)

for (arquivo in arquivos) {
  nome_tabela <- str_to_lower(str_replace(arquivo, ".csv", ""))
  df <- read.csv(paste0(local, arquivo), sep=";", encoding = 'latin1')
  df <- janitor::clean_names(df)
  dbWriteTable(con, DBI::Id(table=nome_tabela, schema=DB_SCHEMA), df)
}

# dicionário RAIS
# peguei daqui: https://console.cloud.google.com/bigquery?p=basedosdados&d=br_me_rais&t=microdados_vinculos&page=table
dicionario_rais <- read.csv("bquxjob_47514fa5_1969bf4d684.csv")
dicionario_rais <- dicionario_rais %>% filter(id_tabela == "microdados_vinculos")

dbWriteTable(con, DBI::Id(table="dicionario_rais", schema=DB_SCHEMA), dicionario_rais)


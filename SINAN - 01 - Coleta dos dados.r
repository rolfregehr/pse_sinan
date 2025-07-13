pacman::p_load(tidyverse,  RCurl)


ftp_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
output_dir <- "./dados_sinan/"

# Crie o diretório de destino se ele não existir
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Liste os arquivos no FTP
# A função getURL retorna uma string com o conteúdo do diretório
ftp_files_string <- getURL(ftp_url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

# Divida a string em nomes de arquivos individuais
ftp_files <- unlist(str_split(ftp_files_string, "\r\n"))

# Filtre os arquivos que começam com "VIOL"
viol_files <- ftp_files[str_detect(ftp_files, "^VIOL")]

# Baixe cada arquivo filtrado
if (length(viol_files) > 0) {
  for (file_name in viol_files) {
    file_url <- paste0(ftp_url, file_name)
    destination_path <- paste0(output_dir, file_name)
    
    message(paste("Baixando:", file_name, "para", destination_path))
    
    # Use download.file para baixar o arquivo
    # mode = "wb" é importante para arquivos binários (como .dbc, .zip, etc.)
    tryCatch({
      download.file(file_url, destfile = destination_path, mode = "wb")
      message(paste(file_name, "baixado com sucesso!"))
    }, error = function(e) {
      message(paste("Erro ao baixar", file_name, ":", e$message))
    })
  }
} else {
  message("Nenhum arquivo que inicia com 'VIOL' encontrado no FTP.")
}

message("Processo concluído.")

# Info VIOL preliminar
url_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
info_prelim <-  getURL(url_prelim,
   ftp.use.epsv = FALSE, 
   dirlistonly = TRUE
  )

prelim_file <-  unlist(str_split(info_prelim, "\r\n"))
arq_prelim <- prelim_file[str_detect(prelim_file, "^VIOL")]
download.file(paste0(url_prelim, arq_prelim), destfile = paste0(output_dir, arq_prelim), mode = "wb")

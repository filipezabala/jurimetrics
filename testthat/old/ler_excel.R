# funcao ler_excel, sequencial
ler_excel <- function(data_ini, data_fim, diretorio = getwd()){
  
  # lendo arquivos
  arqCol <- dir(diretorio)
  dateCol <- as.Date(unclass(substr(arqCol, 6, 15)))
  dateColSize <- length(dateCol)
  df <- bind_cols(as_tibble(arqCol),as_tibble(dateCol)) %>%
    rename(arquivo=value, data=value1)
  
  # filtrando por periodo
  dfFilter <- df %>%
    filter(between(data, as.Date(data_ini), as.Date(data_fim)))
  dfFilterSize <- nrow(dfFilter)
  
  # lendo arquivos filtrados
  tibbleList <- vector(mode = 'list', length = dateColSize)
  j <- 0
  ini <- Sys.time()
  
  for(i in dfFilter$arquivo){
    j <- j + 1
    tibbleList[[j]] <- read_excel(i)
    perc <- noquote(paste0(round(j/dfFilterSize,3)*100, ' %'))
    print(perc)
  }
  
  # empilhando
  tjrs <- bind_rows(tibbleList)
  gc()
  print(Sys.time()-ini)
  
  return(tjrs)
}

library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(xml2)
library(stringr)

#06/03/2021

correct_Value = function(vector) {
  pattern_mil = str_locate(vector$VALOR, pattern = "mil")[,1]
  mil_index = which(pattern_mil != "NA")
  
  pattern = str_locate(vector$VALOR, pattern = "mi")[,1]
  value = substr(vector$VALOR, 1, pattern -2)
  value = gsub(",", ".", value) %>% as.numeric()
  
  if(length(mil_index) > 0) {
    value[mil_index] = value[mil_index]/1000
  }
  
  return(value)
}


Ler_URL = function(url) {


flag_index = str_locate(url, pattern =  "/BRA")[2] %>% as.numeric()

flag = substr(url, flag_index+1, flag_index+1) %>% as.numeric()
  
if (flag == 1) {
  TIMES = c("FLA", "PAL", "CAM", "GRE", "INT", "SAO", "COR", "SAN", "BGT", "CAP", "FLU", "VAS", "BOT", "CEA",
                  "FOR", "CFC", "BAH", "SPO", "GOI", "AGO")
}

if(flag == 2) {
  TIMES = c("CRU", "AVA", "CSA", "CHA", "CUI", "PON", "JUV", "CRB", "AME", "VIT", "GUA", "NAU", "FIG", "OPE", 
            "BSP", "SAM", "BRA", "PAR", "OES", "CONF")
}

YEAR_INDEX = str_locate(url, pattern = "=")[1] %>% as.numeric()
YEAR = substr(url, YEAR_INDEX+1, YEAR_INDEX+4) %>% as.numeric()
  
    
link =  url %>% 
  read_html() %>% 
  html_table(fill = TRUE)

link = link[[4]]
link = link[-1,]

Coluna = paste("Pertence à liga01/01/", YEAR, sep = "")

VALOR_INDEX = which(colnames(link) == Coluna) %>% as.numeric()

Market_value = data.frame(
  TIME = TIMES,
  VALOR = link[,VALOR_INDEX],
  YEAR = rep(YEAR, 20)
)

Market_value$VALOR = correct_Value(Market_value)



return(Market_value)

}


lista_ano = c(seq(2016, 2021, 1))
dia = "01"
mes = "01"

dia_mes = paste(mes, "-", dia, sep = "")
url = "https://www.transfermarkt.com.br/campeonato-brasileiro-serie-a/marktwerteverein/wettbewerb/BRA1/plus/1?stichtag="
Market_value = data.frame(
  TIME = 0,
  VALOR = 0, 
  YEAR = 0
)



for(i in lista_ano) {
  urlA = paste(url, i,"-", dia_mes, sep = "")
  urlB = gsub("BRA1", "BRA2", urlA)
  Market_value = rbind(Market_value, Ler_URL(urlA), Ler_URL(urlB)) 
    
    
}

Market_value = Market_value[-1,]


save(Market_value, file = "Market_value")

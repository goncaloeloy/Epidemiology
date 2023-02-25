###Definir diretório
setwd("C:/Users/gonca/OneDrive/Documentos/R projects/Epidemiology")

###Carregar algumas bibliotecas
pacman::p_load(
  devtools,
  RODBC,
  dplyr,
  mapboxer,
  plotly,
  readxl,
  lubridate
)

###Importar dados do mdb
con <- odbcConnectAccess(path.expand('Dados.mdb'))
localizacao_exp <- sqlFetch(con, "Bov_Exploracoes") 
movimentos <- sqlFetch(con, "Bov_Mov_Nac_Export")

###Importar dados do excel com DDOS
marca_auricular <- readxl::read_excel("dados_ddo.xlsx")

### Para fazer o flowmap precisamos de 2 df:
## locations : com id, name, latitude e longitude
## flow: origem, destino, numero de movimentos (count)

### Para o flow vamos usar a df movimentos e selecionar apenas os que dizem respeito aos animais positivos a TB:
##Retirar só os animais positivos a Tuberculose
ma_tuberculose <- marca_auricular %>% 
  filter(DOENÇA == "TUBERCULOSE")

##Manipular variáveis
unique(ma_tuberculose$`MARCA AURICULAR`) #existem espaços dentro das observações

##retirar espaços
ma_tuberculose$`MARCA AURICULAR` <- gsub(" ", "", ma_tuberculose$`MARCA AURICULAR`, fixed = TRUE) #retirar os espaços

##Extrair o check digit da posição 3, extrair a posição 1 e 2 e voltar a extrair apartir da posição 4
ma_tuberculose$`MARCA AURICULAR` <- paste(substring(ma_tuberculose$`MARCA AURICULAR`, 1, 2), substring(ma_tuberculose$`MARCA AURICULAR`, 4), sep = "")

# Cruzar informação para selecionar os animais positivos a TB
mov_TB <- movimentos %>% filter(movimentos$NUM_BRI %in% ma_tuberculose$`MARCA AURICULAR`)

#Converter para formato Data 
mov_TB <- mov_TB %>% mutate_at(vars(DATA,DAT_NAS), as.Date, format="%Y-%m-%d")

#Selecionar só as variáveis que nos interessam
localization_TB <- mov_TB%>%
  select(NUM_BRI, DAT_NAS, ME, ME_Dest, DATA, Motivo)

#Mudar o nome das colunas
colnames(localization_TB) <- c("MARCA AURICULAR", "DATA_NAS", "ORIGEM", "DESTINO", "DATA", "MOTIVO")

#Criar dataframe "flows" e adicionar coluna com contagem do número de vezes em que o mesmo movimento é feito
library(plyr)
flows <- ddply(localization_TB,.(ORIGEM,DESTINO),nrow)

#Mudar o nome das colunas
colnames(flows) <- c("origin", "dest", "count")

flows #Ver se está OK, parece que sim

### Para a locations vamos cruzar os dados das explorações presentes na data.frame flows e retirar as respetivas latitudes e longitudes
#Retirar o PT e espaços da variavel marca exploração (CEX_MAR_EXP)
localizacao_exp$CEX_MAR_EXP <- gsub(".*PT", "", localizacao_exp$CEX_MAR_EXP)

#Ir buscar as latitudes e longitudes das explorações presentes na dataframe "flows"
#Verificar quais são as explorações presentes na coluna origem e destino e atribui-las a um vector
x <- unique(flows$origin) #só nos interessam as únicas
y <- unique(flows$dest) #só nos interessam as únicas

#combinar num único vector
z <- c(x, y)

# Ficar só com os únicos de Z
z <- unique(z)

#Tranformar em data.frame para se poder fazer pesquisa e merge mais facilmente
z <- as.data.frame(z)
z #Ver se se está OK, parece que sim

###Cruzar os dados:
#Mudar os nome da variavél para coincidir com nome da variável onde está o nome das explorações na data.frame localizacao_Exp
names(z) <- c("CEX_MAR_EXP")

#Cruzar os dados e juntar segundo a data_frame que está à direita que só tem as explorações que aparecem no flow (z)
locations <- right_join(localizacao_exp, z, by = "CEX_MAR_EXP") #juntar as linhas 

#Selecionar apenas as variáveis de interesse
locations<- locations %>%
  select(CEX_MAR_EXP, LATITUDE, LONGITUDE)

#Ver se são todos unicos
unique(locations$CEX_MAR_EXP)

#Remover os que não são únicos
locations <- distinct(locations, CEX_MAR_EXP, .keep_all = TRUE)

##Tratar da dataframe locations
#Juntar uma coluna id para termos as 4 colunas id, name, lat e long. Neste caso para ficar mais fácil o id vai ser o nome da exploração 
locations$ID <- seq.int(nrow(locations))
locations <- locations %>%
  select(CEX_MAR_EXP, ID, LATITUDE, LONGITUDE)

#Mudar os nomes das colunas para coincidir com o pretendido para o flow map 
names(locations) <- c("id", "name", "lat", "lon")  

#Mudar o tipo de variável
locations$name <- as.character(locations$name)

locations # Para ver se está tudo OK, e não. Not OK! Muitos NA.

#tirar os Na
library(tidyr)
locations_1 <-locations %>% drop_na()

locations_1 #Ver se está OK, parece que sim

### Já temos as nossas df flows e locations (locations_1). Vamos dar uma vista de olhos
flows
locations_1

###Vamos fazer o nosso flowmpablue
library(flowmapblue)
#Definir o token, usei o meu público do mapbox
mapboxAccessToken <- "pk.eyJ1IjoiZ29uY2Fsb2Vsb3kiLCJhIjoiY2t3NTU3amRsZWt1cDJvczdwNmNwdHQ1dCJ9.jFWSjjtVBwpYweTjPjB-Mg"

#Fazer o flowmap
flowmapblue(locations_1, flows, mapboxAccessToken, clustering=, animation=FALSE) #Temos de exportar como html, a layer do Mapbox não está a fazer render no viewer do RSTUDIO

#Nos nodes aparece com o número de id em vez do name
locations_1$name <- locations_1$id

#fazer novo flowmap
flowmapblue(locations_1, flows, mapboxAccessToken, clustering=, animation=FALSE)
Footer
© 2023 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About

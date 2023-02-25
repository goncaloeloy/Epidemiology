## Definir onde estamos a trabalhar (não tem de ser onde este documento está guardado)
setwd("C:/Users/marga/Desktop/6.º Ano/Problemas/4. DDO/Drive")


## Bibliotecas
library(devtools)
library(RODBC)
library(dplyr)
library(mapboxer)
library(plotly)
library(RColorBrewer)
library(lubridate)


# Ficheiro Excel onde nos importam apenas quais são os indivíduos positivos 
marca_auricular <- readxl::read_excel("Dados DDO.xlsx")

# Vamos primeiramente restringir-nos a casos positivos a tuberculose
ma_tuberculose <- marca_auricular %>% filter(DOENÇA == "TUBERCULOSE")

# Tirar os espaços a mais
ma_tuberculose$`MARCA AURICULAR` <- gsub(" ", "", ma_tuberculose$`MARCA AURICULAR`, fixed = TRUE)

# Tirar o "check digit" (ou seja, o 3.º caracter nestes casos não faz parte da identificação em si)
# Dividi em 2 passos (remoção de PT mais o dígito em excesso e depois adição de PT)
ma_tuberculose$`MARCA AURICULAR` <- gsub("PT[0-9]", "", ma_tuberculose$`MARCA AURICULAR`) 

# NB: Só é preciso ".*" se o padrão existir na string mais que 1x (. = todo o caracter, exceto uma nova linha; * = 0 ou + vezes dentro da string)

ma_tuberculose$`MARCA AURICULAR` <- paste("PT", ma_tuberculose$`MARCA AURICULAR`, sep = "")

#Existem alternativas:

# Alternativa 1: ma_tuberculose$`MARCA AURICULAR` <- gsub(paste(str_sub(x, 1, 2)), str_sub(x, 4, nchar(x), sep = "")
# . str_sub(x, 1, 2) -> olha, da string x dá-me a string existente da posição 1 à 2 (inclusive)! -> PT
# . str_sub(x, 4, nchar(x)) -> olha, da string x dá-me a string existente da posição 4 até à última posição da string x (nchart(x) = número de caracteres de x) e, novamente, inclusive -> 64774520
# . Agora tinha 2 strings separadas com o que queríamos e pedi o paste delas duas para as juntar numa só, tipo "oi, paste, junta-me aqui o PT com o 64774520 com o separador "" (vazio) entre as duas"

# Alternativa 2 (para 2.º passo): ma_tuberculose$`MARCA AURICULAR` <- gsub("(.*)", "PT\\1", ma_tuberculose$`MARCA AURICULAR`)
# Acima segui um exemplo da net. Tive uma explicação amigável do porquê deste padrão no código, que fica num documento da pasta DDO no meu pc.


# Ficheiro mdb onde nos importam a localização das explorações e os movimentos efetuados
con <- odbcConnectAccess(path.expand('Dados.mdb'))

movimentos <- sqlFetch(con, "Bov_Mov_Nac_Export")

localizacao_exp <- sqlFetch(con, "Bov_Exploracoes")

# Cruzar info
mov_TB <- movimentos %>% filter(movimentos$NUM_BRI %in% ma_tuberculose$`MARCA AURICULAR`)

# Transformar dados em formato data
mov_TB$DATA <- as.Date(mov_TB$DATA, format = "%Y-%m-%d")

mov_TB$DAT_NAS <- as.Date(mov_TB$DAT_NAS, format = "%Y-%m-%d")


# Para ver só um exemplo, caso quisessemos: mov_TB <- filter(movimentos, NUM_BRI =="PT919301665")


# Agora temos uma lista das movimentações feitas pelos animais que foram detetados como positivos a tuberculose.


# Próximo passo: análise dos movimentos
# - perceber se os dados estão completos, ou seja, fazer um controlo de qualidade dos dados (total ou por amostragem)
#   . perceber se o 1.º movimento tem origem na marca de nascimento do animal (não obrigatório) e se o último movimento é um matadouro (obrigatório)
#   . dados podem estar em falta e podemos precisar de criar registos ou perceber porque é que não estão aqui (ex. movimento efetuado fora do intervalo de tempo em que temos registos disponíveis em mov_TB)
# - descrição geral de movimentos dos quais temos registos
#   . n.º de explorações, i.e., quantas vezes é que foi movimentado, na vida e, por exemplo, nos últimos 6 meses
#   . distribuição do tempo entre o último movimento e o abate
# - idade em que os animais foram abatidos

# Objetivo final: perceber como é que esses animais foram sujeitos a controlos anuais de tuberculose e controlos pré-movimentação de tuberculose e, mesmo assim, chegaram ao matadouro com uma DDO.
# Como é que estes animais são abatidos e têm lesões de tuberculose, qd temos um plano que controla todas as explorações e, cada vez que se movimenta um animal temos de o testar para saber se tem ou não TB.


# NB: 
# - Alguns bovinos que aparecem podem ser efetivamente mais velhos (ex. vaca reprodutora)
# - Legenda Dados.mdb, Folha "Bov_Nac_Mov_Export"
# • DATA = Data do MOVIMENTO
# • ME = Marca da exploração de ORIGEM
# • NUM_BRI = Número do brinco
# • DAT_NAS = Data de nascimento
# • ME_Dest = Marca da exploração de DESTINO (Outras explorações ou Matadouros)
# • Motivo = Motivo
# ME e ME_Dest pode ser igual (registo de movimentos dentro da mesma exploração são transferências de proprietários)
# Cada linha de mov_TB regista um movimento de um animal entre 2 pontos, num determinado intervalo temporal (2015-2021). 
# Assim, a origem do movimento não tem de ser a exploração onde nasceram, por ex. (que podemos consultar no Excel)
# Para o mesmo animal pode surgir uma série de movimentos na tabela mov_TB!


## Análise dos movimentos
# - dados em falta
# ver que animais estão em falta na tabela mov_TB 
unique(mov_TB$NUM_BRI)
unique(ma_tuberculose$`MARCA AURICULAR`)

# Comparando o n.º de resultados obtidos, verifica-se que falta o registo de alguns animais presentes da tabela "ma_tuberculose" na tabela "mov_TB"

# - movimentos por animal
falta <- setdiff(ma_tuberculose$`MARCA AURICULAR`, mov_TB$NUM_BRI)
falta

# Ficamos a saber que os registos de movimentos de 11 animais estão em falta na tabela mov_TB
# [1] "PT19720750" "PT17800652" "PT23998470" "PT24835721" "PT24868721" "PT24868564" "PT24868554" "PT22707652" "PT14795472" "PT14366704" "PT64876617"

# Para investigarmos acerca desses movimento, isolamos esses casos
falta_mov_TB <- ma_tuberculose %>% filter(ma_tuberculose$`MARCA AURICULAR` %in% falta)

# Para vermos qual o movimento mais antigo registado
min(mov_TB$DATA)

# É "2015-09-02". Movimentos que se deram neste ano estão presentes na tabela mov_TB, por isso, até esta data, devíamos ter registos dos movimentos.
# Os 11 animais em falta têm data de abate entre 2019 e 2021. 
# Têm todos apenas 1 movimento registado (MOE_NASCIMENTO -> MOE_SAÍDA), mais uma ida para o matadouro, exceto um animal que tem 3 movimentos + a ida para matadouro
# São maioritariamente movimentos dentro da mesma exploração.

# Retocar os espaços nos matadouros
falta_mov_TB$NCV <- gsub(" ", "", falta_mov_TB$NCV, fixed = TRUE)
falta_mov_TB$NCV <- ifelse(falta_mov_TB$NCV == "MATADOUROOUTROPAÍS", "MATADOURO OUTRO PAÍS", falta_mov_TB$NCV)

# Criar tabela para completar a mov_TB com movimentos dos quais temos data
falta2_mov_TB <- data.frame(falta_mov_TB$`DATA ABATE`, falta_mov_TB$MOE_SAÍDA, falta_mov_TB$`MARCA AURICULAR`, falta_mov_TB$`DATA NASCIMENTO DO ANIMAL`, falta_mov_TB$NCV)

# Mudar nomes da tabela anterior
colnames(falta2_mov_TB) <- c("DATA", "ME", "NUM_BRI", "DAT_NAS", "ME_Dest")

# Colocar em formato Date os dados referentes a datas
falta2_mov_TB$DATA <- as.Date(falta2_mov_TB$DATA, format = "%Y-%m-%d")

falta2_mov_TB$DAT_NAS <- as.Date(falta2_mov_TB$DAT_NAS, format = "%Y-%m-%d")

# Acrescentar coluna final, para ficar com o mesmo tipo de variáveis de a tabela "mov_TB"
falta2_mov_TB$Motivo <- "Saídas Abate"

# Não tendo disponíveis as datas dos outros movimentos, não conseguimos efetuar o seu registo. Vamos juntar tabelas para ficar com a informação reunida.
mov_TB <- rbind(mov_TB, falta2_mov_TB)


# Para verificarmos quantos movimentos temos corretamente registados a partir de cada uma das explorações identificadas como origem (já que há testes pré-movimentação):
n_exp <- mov_TB %>% group_by(ME) %>% tally()

# Procurar a exploração com mais movimentos registados
n_exp[which.max(n_exp$n),]

# Ficamos a saber que é a NU81L, com 6 movimentos registados.


# - Para verificarmos quantos movimentos temos corretamente registados por cada animal, durante toda a sua vida:
n_mov <- mov_TB %>% group_by(NUM_BRI) %>% tally()

# Procurar o animal com mais movimentos registados
n_mov[which.max(n_mov$n),]

# Ficamos a saber que é foi o PT19485008, com 8 movimentos registados.

# Ver distribuição do n.º de movimentos
p1 <- plot_ly(x = n_mov$n, 
              type = "histogram") %>% layout(title = 'Distribuição do n.º de movimentos de bovinos positivos a tuberculose', 
                                             font=list(family = "helvetica", size = 12, color = '#1a1a1a'),
                                             xaxis = list(nticks = 10, title = "N.º de movimentos", showgrid = FALSE), 
                                             yaxis = list(title = "% animais positivos movimentados", showgrid = FALSE, ticksuffix = '%'),
                                             paper_bgcolor = "#ffffff",
                                             plot_bgcolor = "#ffffff")
p1

# Verifica-se que a maior percentagem dos casos positivos (46%) corresponde a bovinos que têm apenas 1 movimento registado.

# Sabemos a este ponto que nem todos os movimentos de cada animal estão registados, mas pelo menos o seu último movimento, ou seja, o que foi feito até ao matadouro, está na tabela "mov_TB"


# Para verificarmos quantos movimentos temos corretamente registados por cada animal, durante o último ano, vem:
n_mov_1a <- mov_TB %>% filter(DATA >= (Sys.Date()-years(1)))

# Distribuição do n.º de movimentos nos últimos 6 meses (não existem dados, mas seria assim):
# n_mov_6m <- mov_TB %>% filter(DATA >= (Sys.Date()-months(6)))

n_mov_1a <- n_mov_1a %>% group_by(NUM_BRI) %>% tally()

n_mov_1a$percentagem <- (n_mov_1a$n/sum(n_mov_1a$n)) * 100


# Tentando fazer um histograma, não se obtém 100%... Não percebo a razão. Em baixo vê-se o resultado pretendido com gráfico de barras
#p2 <- plot_ly(x = n_mov_1a$n, 
#              type = "histogram") %>% layout(title = 'Distribuição do n.º de movimentos de bovinos positivos a tuberculose (último ano)', 
#                                             font=list(family = "helvetica", size = 12, color = '#1a1a1a'),
#                                             xaxis = list(nticks = 2, title = "N.º de movimentos", showgrid = FALSE), 
#                                             yaxis = list(title = "% animais positivos movimentados", showgrid = FALSE, ticksuffix = '%'),
#                                             paper_bgcolor = "#ffffff",
#                                             plot_bgcolor = "#ffffff")
#p2


p3 <- plot_ly(
  x = n_mov_1a$n,
  y = n_mov_1a$percentagem,
  type = "bar") %>% layout(title = 'Distribuição do n.º de movimentos de bovinos positivos a tuberculose (último ano)', 
                           font=list(family = "helvetica", size = 12, color = '#1a1a1a'),
                           xaxis = list(nticks = 2, title = "N.º de movimentos", showgrid = FALSE), 
                           yaxis = list(title = "% animais positivos movimentados", showgrid = FALSE, ticksuffix = '%'),
                           paper_bgcolor = "#ffffff",
                           plot_bgcolor = "#ffffff")

p3

###### Em TRABALHO! (POR DECIDIR)
# - Distribuição do tempo entre o último movimento e o abate de um animal

# Queremos animais que tenham mais do que o movimento para abate
n_mov_m2 <- n_mov %>% filter(n_mov$n > 1)

n_mov_m2 <- merge(mov_TB, n_mov_m2, by = "NUM_BRI")

# Retirar animais que não foram para abate, mas sim para exportação
num_brin_exportacao <- n_mov_m2[which(n_mov_m2$Motivo == 'Exportação'), ]

n_mov_m2 <- subset(n_mov_m2, !(NUM_BRI %in% num_brin_exportacao))


# ideia geral: 
# - dividir tabela em saídas Vida e saídas Abate
# - Ficar apenas com a data "máxima" de cada animal em cada uma das tabelas criadas anteriormente
# - subtração entre as datas obtidas (Abate - Máximo Vida) 

# Podia ser um raciocínio assim:
# if( n == 2 ) { fico com as duas linhas }
# if( n > 2 ) { mantém apenas a linha 'Saída Abate' e a max.data(Saída Vida) }


# É preciso a data de abate da cada animal e a data da "Saídas Vida" mais recente de cada um
n_mov_m2_abate <- n_mov_m2 %>% filter(Motivo == "Saídas Abate")

n_mov_m2_vida <- n_mov_m2 %>% filter(Motivo == "Saídas Vida")

saidas_vida_max <- n_mov_m2_vida %>% group_by(NUM_BRI) %>% summarise(DATA = max(DATA))

# Mas agora detetou-se uma diferença entre o n.º de observações da tabela saídas_vida_max (60) e da tabela n_mov_m2_abate (53)
# setdiff(saidas_vida_max$NUM_BRI, n_mov_m2_abate$NUM_BRI)
# "PT14580358" "PT14869111" "PT16078311" "PT18238223" "PT19544177" "PT22739948" "PT40675910"
# Com a linha de código acima, percebemos que existem 7 animais que não têm registado, pelo menos, a sua saída para abate.

# Perante isto, investigou-se se nos animais com 1 movimento registado, este era uma Saída para abate

n_mov_1 <- n_mov %>% filter(n_mov$n == 1)

n_mov_1 <- merge(mov_TB, n_mov_1, by = "NUM_BRI")

# Assim, vê-se que existem alguns animais com apenas 1 movimento registado que se trata de uma saída de vida. Falta, portanto, registar, pelo menos a sua saída para abate.

# DÚVIDA EXISTENCIAL: É razoável/faz sentido adicionar os movimentos cuja falta se detetou agora? 
# Ou tinhamos de ver mesmo animal a animal, quais os movimentos registados de forma a retificar tudo?
# Ou eliminamos os animais "problemáticos" porque não temos a certeza de ter datas disponíveis para todos os movimentos?
# Food for thought ^.^

######


# - idade em que foram abatidos

# Para ver que variáveis existem na coluna Motivo
unique(mov_TB$Motivo)

# Só temos registada a data de movimentos que se deram de uma exploração para um matadouro na tabela "ma_tuberculose".
# Criar tabela só com movimentos para matadouro 
mov_TB_abate <- mov_TB %>% filter(Motivo == "Saídas Abate")

# Criar coluna com idade de abate
mov_TB_abate$idade <- (mov_TB_abate$DATA - mov_TB_abate$DAT_NAS)/365.25

mov_TB_abate$idade <- as.numeric(mov_TB_abate$idade)

mov_TB_abate$idade_arredondada <- (mov_TB_abate$DATA - mov_TB_abate$DAT_NAS)/365.25

mov_TB_abate$idade_arredondada <- ceiling(as.numeric(mov_TB_abate$idade_arredondada))


# Nova tabela para poder mostrar a informação graficamente, se optarmos pela idade arredondada
anos_abate <- mov_TB_abate %>% group_by(idade_arredondada) %>% tally()

anos_abate$percentagem <- (anos_abate$n/sum(anos_abate$n)) * 100

# Histograma e Gráfico de barras (não sei qual será a melhor apresentação dos dados)
f1 <- plot_ly(x = mov_TB_abate$idade, 
              type = "histogram") %>% layout(title = 'Distribuição das idades de bovinos positivos a tuberculose no matadouro', 
                                             font=list(family = "helvetica", size = 12, color = '#1a1a1a'),
                                             xaxis = list(nticks = 20, title = "Idade (anos)", showgrid = FALSE), 
                                             yaxis = list(title = "% positivos a tuberculose", showgrid = FALSE, ticksuffix = '%'),
                                             paper_bgcolor = "#ffffff",
                                             plot_bgcolor = "#ffffff")
f1

# No matadouro, regista-se uma maior percentagem (> 30%) de bovinos até 2 anos positivos a tuberculose.

f2 <- plot_ly(
  x = anos_abate$idade_arredondada,
  y = anos_abate$percentagem,
  type = "bar") %>% layout(title = 'Distribuição das idades de bovinos positivos a tuberculose no matadouro', 
                                                        font=list(family = "helvetica", size = 12, color = '#1a1a1a'),
                                                        xaxis = list(nticks = 20, title = "Idade (anos)", showgrid = FALSE), 
                                                        yaxis = list(title = "% positivos a tuberculose", showgrid = FALSE, ticksuffix = '%'),
                                                        paper_bgcolor = "#ffffff",
                                                        plot_bgcolor = "#ffffff")

f2

# Com este gráfico, conseguimos discernir que a maior percentagem (23%) de bovinos infetados tem entre > 12 e 24 meses, dado o arredondamento feito. 


# A bactéria pode permanecer no animal infetado em estado latente  sem que a doença se desenvolva. 
# Um animal infetado  pode transmitir a doença a  muitos outros animais (domésticos ou selvagens), antes de morrer e até  mesmo antes que os 1.ºs sintomas se manifestem.
# A profilaxia da tuberculose bovina é obrigatória em todo o território nacional.

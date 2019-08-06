#========================================================#
# AUTOMACAO DE MODELOS DE SERIES TEMPORAIS INTERROMPIDAS
# Avaliacao do programa do governo de PE Mae Coruja
#========================================================#
# Claudio Alves Monteiro e Dalson Britto Figueiredo
# Agosto, 2019
#========================================================#

# importar dados
nasc = read.csv('dados/nascimento.csv', sep=';', stringsAsFactors=FALSE, fileEncoding="latin1")[,-24]
obito = read.csv('dados/obitos_infantis.csv', sep=';', stringsAsFactors=FALSE, fileEncoding="latin1")[,-24]

#==============================#
# TRATAMENTO DE DADOS
#=============================#

# funcao para tratamento de dados

tratamento <- function(data, name){
  
  # carregar pacotes
  library(reshape2); library(stringr)
  # Pivotar tabela para colunas ano e valor
  data = melt(data, measure.vars=2:23, variable.name="ano", value.name=name)
  # transformar em numerico
  data[,3] = as.numeric(data[,3])
  # substituit 'X' por ''
  data$ano = as.numeric(str_replace(data$ano, 'X', ''))
  # split codigos dos nomes
  data$code_muni = sapply(strsplit(data$Município," "), `[`, 1)
  # remover numeros
  data$Município = str_replace(gsub('[[:digit:]]+', '', data$Município), ' ', '')
  
  return(data)
  
}

# executar tratamento
obito = tratamento(obito, 'numero_obitos')
nasc = tratamento(nasc, 'numero_nascimentos')

#  combinar dados
dataset = merge(obito, nasc, by=c('code_muni', 'ano'))

# capturar codigo do estado
dataset$estado_code = substr(dataset$code_muni, start = 1, stop = 2)

# selecionar PE
pe = dataset[dataset$estado_code == '26',]

# selecionar casos completos [CALCULAR MEDIA]
pe = pe[complete.cases(pe),]

# calcular taxa por municipio
pe$taxa = (pe$numero_obitos / pe$numero_nascimentos)*1000

#================================#
# PRE-PROCESSAMENTO P/ MODELAGEM
#===============================#

# criar lista dos municipios
lista = levels(as.factor(datana$code_muni))

muni = pe[pe$Município.x == 'Recife',]

# automatizar geracao de base de dados
for (i in lista)  {
  
  # found year
  year = 2007
  
  # selec muni
  muni = pe[pe$code_muni == i,]
  # length of vector
  dime = dim(muni)[1]
  # find index of year
  yi = match(year, muni$ano) - 1
  
  # range tempo
  tempo = 1:dime
  # range nivel
  nivel = c(rep(0, each=yi), rep(1, each=(dime-yi) ))
  
  
}

muni = datana[datana$code_muni == '110006',]


 


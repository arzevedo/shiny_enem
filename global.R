library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(leaflet.minicharts)
library(DT)
library(shinythemes)
library(RColorBrewer)

Regiao<-c('S','SE','CO','NE','N','BR_T')
Lat<-c(-28,-20,-16,-8,-3.5,-23)
Long<-c(-51,-45,-54,-42,-60,-31)
categ <- c('Sexo', 'Tipo de Cor', 'Tipo de escola', 'Tipo de escola frequentada no EM', 'Turno do EM')
dados_g<-data.frame(Regiao,Lat,Long)

br.shp=readOGR(dsn="grandes_regioes_shp.shp")

colors<-brewer.pal(7,'Spectral')

dados_enem<-read.csv('data/enem_final.csv',header=T)
dados_enem[,22:34][is.na(dados_enem[,22:34])]<-0

# CRIAÃÂÃÂO DE NOVAS VARIÃÂVEIS E AGRUPAMENTO DE FATORES

# Criando variÃÂ¡vel de regiÃÂ£o do paÃï¿½s de acordo com cÃÂ³digo do estado
# Criando variÃÂ¡vel de faixa de idade com agrupamento da idade declarada
# Criando variÃÂ¡vel de faixa de renda de acordo com a renda declarada
# Renomeando fatores conforme legendas do banco

dados_enem <- dados_enem %>%
  mutate(REGIAO_BR=factor(case_when(.$CO_UF_RESIDENCIA %in% c(11:17) ~ 'N',
                                    .$CO_UF_RESIDENCIA %in% c(21:29) ~ 'NE',
                                    .$CO_UF_RESIDENCIA %in% c(50:53) ~ 'CO',
                                    .$CO_UF_RESIDENCIA %in% c(31:35) ~ 'SE',
                                    .$CO_UF_RESIDENCIA %in% c(41:43) ~ 'S'),
                          levels=c('S','SE','CO','NE','N'))) %>%
  mutate(FAIXA_IDADE=factor(case_when(.$NU_IDADE < 16 ~ 'Menos de 16 anos',
                                      .$NU_IDADE %in% c(16:19) ~ 'Entre 16 e 19 anos',
                                      .$NU_IDADE %in% c(20:25) ~ 'Entre 20 e 25 anos',
                                      .$NU_IDADE > 25 ~ 'Mais de 25 anos'),
                            levels=c('Menos de 16 anos','Entre 16 e 19 anos',
                                     'Entre 20 e 25 anos','Mais de 25 anos'))) %>%
  mutate(FAIXA_RENDA=factor(case_when(.$Q006 == 'A' ~ 'Nenhuma renda',
                                      .$Q006== 'B' ~'Apenas um Salário-Mínimo',
                                      .$Q006 %in% c('C','D') ~'De um a dois Salários-Mínimos',
                                      .$Q006 %in% c('E','F') ~ 'De dois a três Salários-Mínimos',
                                      .$Q006 %in% c('G','H') ~ 'De três a cinco Salários-Mínimos',
                                      .$Q006 %in% c('I','J','K','L','M','N','O','P','Q') ~ 'Acima de cinco Salários-Mínimos'),
                            levels=c('Nenhuma renda','Apenas um Salário-Mínimo','De um a dois Salários-Mínimos',
                                     'De dois a três Salário-Mínimo','De três a cinco Salário-Mínimo',
                                     'Acima de cinco Salários-Mínimos'))) %>%
  mutate(TP_SEXO=recode(TP_SEXO,
                        'F'='Feminino',
                        'M'='Masculino')) %>%
  mutate(TP_COR_RACA=recode_factor(TP_COR_RACA,'0'='Não declarado','1'='Branca','2'='Preta','3'='Parda',
                                   '4'='Amarela','5'='Indígena','6'='Não dispõe da informação')) %>%
  mutate(Q047=recode(Q047,
                     'A'='Somente em escola pública',
                     'B'='Parte em escola pública e parte em escola privada sem bolsa de estudo integral',
                     'C'='Parte em escola pública e parte em escola privada com bolsa de estudo integral',
                     'D'='Somente em escola privada sem bolsa de estudo integral',
                     'E'='Somente em escola privada com bolsa de estudo integral')) %>%
  mutate(Q049=recode(Q049,
                     'A'='Somente no diurno',
                     'B'='Parte no diurno e parte no noturno',
                     'C'='Somente no noturno'))




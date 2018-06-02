fluidPage(theme = shinytheme(theme = "journal"),

  titlePanel('ENEM 2016'),

  sidebarLayout(

    sidebarPanel(

      selectInput(inputId='banco',label='Selecione o banco de dados',
                  choices=c('Selecione uma opção'='default','Quem presta o ENEM'='geral',
                            'Quem são os melhores'='melhores','Quem são os piores'='piores')),

      conditionalPanel(

        condition=" input.banco!='default' & input.banco!='geral' ",

        radioButtons(inputId='hab',label='Selecione o tipo de habilidade',
                     choices=c('Performace na prova'='performance','Ciências da Natureza'='natureza',
                               'Ciências Humanas'='humanas','Linguagens e Códigos'='linguagem',
                               'Matemática'='matematica','Redação'='redacao'))

        ), # fecha primeiro conditional panel

      conditionalPanel(

        condition=" input.banco!='default' ",

        radioButtons(inputId='var',label='Selecione a variável',
                     choices=c('Sexo'='sexo',"Idade"='idade','Cor/Raça'='raca','Faixa de Renda'='faixa_renda',
                               'Tipo de escola frequentada no EM'='tipo_EM','Turno do EM'='turno'))

        # removi a categoria "tipo de escola"
        # se for a variÃ¡vel TP_ESCOLA nÃ£o vale a pena analisar pelo quantitativo de dados inconclusos

        ) # fecha segundo conditional panel

      ), # fecha sidebar panel

    mainPanel(#'Tabela separada por regiÃ£o',

              leafletOutput('mymap',height=700)

              ) # fecha main panel

    ) # fecha sidebar layout

  ) # fecha fluid page

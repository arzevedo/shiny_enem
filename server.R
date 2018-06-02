function(input, output,session) {

  output$mymap <- renderLeaflet({

    mapa<-leaflet(width='100%',height='700px',options=leafletOptions(minZoom=4,maxZoom=4,zoomControl=F,
                                                                     doubleClickZoom =FALSE, bounceAtZoomLimits = FALSE)) %>%
      addProviderTiles('Esri.WorldShadedRelief') %>%
      setView(lng=-52.561972,lat=-14.446204,zoom=5) %>%
      addPolygons(data=br.shp,weight=2,color='#78BD67',fillOpacity=0.4)

  })


  dados1<-reactive({

    if (input$banco=='geral') {

      dados_enem<-dados_enem[which(dados_enem$AMOSTRA_30MIL==1),]

      } else {

        if (input$banco=='melhores') {

          data1 <- switch(input$hab,
                          'performance' = dados_enem$MELHORES_1000,
                          'natureza' = dados_enem$MELHORES_1000_CN,
                          'humanas' = dados_enem$MELHORES_1000_CH,
                          'linguagem' = dados_enem$MELHORES_1000_LC,
                          'matematica' = dados_enem$MELHORES_1000_MT,
                          'redacao' = dados_enem$MELHORES_1000_REDACAO)

          dados_enem<-dados_enem[which(data1==1),]

        } else {

          if (input$banco=='piores') {

            data1 <- switch(input$hab,
                            'performance' = dados_enem$PIORES_1000,
                            'natureza' = dados_enem$PIORES_1000_CN,
                            'humanas' = dados_enem$PIORES_1000_CH,
                            'linguagem' = dados_enem$PIORES_1000_LC,
                            'matematica' = dados_enem$PIORES_1000_MT,
                            'redacao' = dados_enem$PIORES_1000_REDACAO)

            dados_enem<-dados_enem[which(data1==1),]

          }

        }

      }

    return(dados_enem)

    })


  observe({

    if(input$banco!='default') {

      dados_mapa<-dados1()

      data1 <- switch(input$var,
                      'sexo' = dados_mapa$TP_SEXO,
                      'idade' = dados_mapa$FAIXA_IDADE,
                      'raca' = dados_mapa$TP_COR_RACA,
                      'faixa_renda' = dados_mapa$FAIXA_RENDA,
                      'tipo_EM' = dados_mapa$Q047,
                      'turno' = dados_mapa$Q049)

      data2 <- switch(input$var,
                      'sexo' = 'pie',
                      'idade' = 'bar',
                      'raca' = 'pie',
                      'faixa_renda' = 'bar',
                      'tipo_EM' = 'pie',
                      'turno' = 'pie')


      dados_chart<-data.frame(table(dados_mapa$REGIAO_BR,data1))

      aux<-split(dados_chart,dados_chart$data1)


      for (i in 1:(nrow(dados_chart)/5)) {

        dados_g[,3+i]<-rbind(aux[[i]][3],sum(aux[[i]][3]))
        names(dados_g)[3+i]<-as.character(aux[[i]][1,2])


      }


      proxy<-leafletProxy('mymap',data=dados_g,session=session)

      proxy %>%
        addMinicharts(dados_g$Long,dados_g$Lat,type=data2,
                      width=55,colorPalette=if(input$var %in% c('idade','faixa_renda')){
                        brewer.pal(length(dados_g), "Oranges")
                      } else {
                        brewer.pal(length(dados_g), "Set1")
                      },transitionTime=0,
                      chartdata=dados_g[,c(4:ncol(dados_g))])

    } # fecha if input$banco


  }) # fecha observer


}


################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        dashboard Demogravis do InfoVis
#####   ESCRITO POR:      Cleiton Rocha, Jackson Conceicao, Jonatas Silva, Kellyene Coleho, Rodrigo Cerqueira
#####   SITE:             https://infovis.sei.ba.gov.br/demogravis/#
#####   LICENÇA:          GPLv3


library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(shinydashboardPlus)
library(sp)
library(ggforce)
library(tidyr)
library(httpuv)
library(rgdal)
library(tibble)
library(scales)
library(fresh)
#library(Cairo)
# Carregando os dataset necessarios

estimativa <- read.csv2("Estimativa_IBGE.csv",fileEncoding = "ISO-8859-1",dec=",")
proj18 <- read.csv2("proj18.csv",fileEncoding = "ISO-8859-1") 
popfaixaquinquenal <- read.csv2("Pop_Faixa_Quiquena_BA_Proj2018.csv", dec=",", header=TRUE,fileEncoding = "ISO-8859-1") 
populacao_censo <- read.csv2("populacao_censo.csv", dec=",",h=T,fileEncoding = "ISO-8859-1")
raca <- read.csv2("MUN_cor_raca_v2.csv",fileEncoding = "ISO-8859-1")
estado_civil_agreg <- read.csv2("estado_civil_agreg.csv",fileEncoding = "ISO-8859-1") 
naturalide_long <- read.csv2("naturalide_municipio.csv",fileEncoding = "ISO-8859-1") 
genero_mun <- read.csv2("genero_municipios.csv",fileEncoding = "ISO-8859-1") 
mortalidade <- read.csv2("natalidade_infantil.csv",fileEncoding = "ISO-8859-1") 
raca_long <- read.csv2("raca_long.csv",fileEncoding = "ISO-8859-1") 
fxaetaria_long <- read.csv2("faixa_etaria_tidyr.csv",fileEncoding = "ISO-8859-1") 
piramide_municipios <- read.csv2("piramide_etaria_municipal.csv",fileEncoding = "ISO-8859-1") 
urbanizacao_municipios <- read.csv2("urbanizacao_municipios.csv",fileEncoding = "ISO-8859-1")
info_mun <- read.csv2("info_pop_municipal.csv",fileEncoding = "ISO-8859-1") 
territorios_geo <- rgdal::readOGR("Terri_iden_ba_v2.json", use_iconv = T,encoding = "UTF-8") # mapa
territorios_pop <- read.csv2("territorios_pop_total.csv",fileEncoding = "ISO-8859-1") 
pop_total_mun <- read.csv2("pop_total_mun.csv",fileEncoding = "ISO-8859-1")
proj_2020_gen <- read.csv2("proj_2020_gen.csv",fileEncoding = "ISO-8859-1")
mapa_ba_mun <- rgdal::readOGR(dsn=getwd(), layer="DPA_A_GEN_2019_05_14_GCS_SIR_SEI", encoding = "UTF-8") # mapa


#transformando a coluna em caracter
estimativa$NM_MUNICIP <- as.character(estimativa$NM_MUNICIP)
estimativa$NM_TI <- as.character(estimativa$NM_TI)

# renomeando codigo do municipio
mapa_ba_mun@data <- mapa_ba_mun@data %>% rename(CD_GEOCMU=Codigo)

# transformando factor em num na shapefile
mapa_ba_mun@data[["CD_GEOCMU"]] <- as.numeric(as.character((mapa_ba_mun@data[["CD_GEOCMU"]])))


#server
function(input, output, session) {
  
  ######################################################################################
  #
  # PAGINA INFORMACOES GERAIS - ABA 1
  #
  ######################################################################################
  
  ############# value Box - Caixa com Populacao total da Bahia - ABA 1   ############# 
  
  output$poptotalgeral <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==2020),select = c(PopTotal)),"População da Bahia em 2020", icon = icon("users"),
      color = "blue"
    )
  })
  #############  Caixa com a posicao da Bahia no ranking do Brasil - ABA 1   ############# 
  
  output$pos_Bahia_Brasil <- renderValueBox({
    valueBox(
      paste("4º do Brasil"),"em termos populacionais", icon = icon("user-friends"),
      color = "green"
    )
  })
  
  #############  Caixa com a posicao da Bahia no ranking do Nordeste - ABA 1   ############# 
  
  output$pos_Bahia_NE <- renderValueBox({
    valueBox(
      paste("1º do NE"),"em termos populacionais", icon = icon("user-circle"),
      color = "yellow"
    )
  })
  

  ################## Mapa Bahia - 2020 - Mun - ABA 1 ############################
  output$mun_2020 <- renderLeaflet({
    

    # criando variável categórica
    estimativa$fxa_pop <- cut(estimativa$Ano2020, c(0,10000,50000,100000,200000,400000,650000,100000000), 
                              labels=c("Até 10 mil",
                                       "10 mil - 50 mil",
                                       "50 mil - 100 mil",
                                       "100 mil - 200 mil",
                                       "200 mil - 400 mil",
                                       "400 mil - 650 mil",
                                       "Mais de 650 mil"),
                              rigth=T,exclude=NULL, include.lowest=TRUE)
    
    
    # banco unindo mapa com os dados #
    pop_esti_ibge <- merge(mapa_ba_mun, estimativa, by = c("CD_GEOCMU"))
    
    # paleta
    wardpal_pop_esti <- colorFactor(brewer.pal(7, "YlOrBr"),
                                    domain=factor(pop_esti_ibge$fxa_pop, 
                                                  levels=c("Até 10 mil",
                                                           "10 mil - 50 mil",
                                                           "50 mil - 100 mil",
                                                           "100 mil - 200 mil",
                                                           "200 mil - 400 mil",
                                                           "400 mil - 650 mil",
                                                           "Mais de 650 mil")))
    
    
    leaflet(pop_esti_ibge, options = leafletOptions(zoomControl = FALSE,
                                                    minZoom = 5.5, maxZoom = 7.5, 
                                                    dragging = TRUE,
                                                    doubleClickZoom=FALSE)) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
      addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal_pop_esti(fxa_pop),
                  label = ~paste0(NM_MUNICIP, ": ", format(Ano2020, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomleft",pal = wardpal_pop_esti, values = ~fxa_pop, opacity = 1.0, title = "Número estimado de habitantes") 
    
  })
  
  ######################### Mapa Bahia - 2020 - Territorio de Identidade ###########################
  output$ter_2020 <- renderLeaflet({
    
    # ajuste no dataset
    territorio_iden_2020 <- estimativa %>% group_by(NM_TI) %>% summarise(pop_territorio=sum(Ano2020))
    
    
    # criando variável categórica
    territorio_iden_2020$fxa_pop_ter <- cut(territorio_iden_2020$pop_territorio, c(0,200000,400000,600000,
                                                                                   800000,1000000,1000000000), 
                                            labels=c("Até 200 mil",
                                                     "200 mil - 400 mil",
                                                     "400 mil - 600 mil",
                                                     "600 mil - 800 mil",
                                                     "800 mil - 1 milhão",
                                                     "Mais de 1 milhão"),
                                            rigth=T,exclude=NULL, include.lowest=TRUE)
    
    
    
    # banco unindo mapa com os dados #
    pop_territorios_identidade <- merge(territorios_geo, territorio_iden_2020, by = "NM_TI")
    
    # paleta
    wardpal_pop_ter <- colorFactor(c("#fee391", "#fec44f","#fe9929","#ec7014","#8c2d04","#461602"),
                                   domain=factor(pop_territorios_identidade$fxa_pop_ter, 
                                                 levels=c("Até 200 mil",
                                                          "200 mil - 400 mil",
                                                          "400 mil - 600 mil",
                                                          "600 mil - 800 mil",
                                                          "800 mil - 1 milhão",
                                                          "Mais de 1 milhão")))
    leaflet(pop_territorios_identidade, options = leafletOptions(zoomControl = FALSE,
                                                                 minZoom = 5.5, maxZoom = 7.5, 
                                                                 dragging = TRUE,
                                                                 doubleClickZoom=FALSE)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
      addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal_pop_ter(fxa_pop_ter),
                  label = ~paste0(NM_TI, ": ", format(pop_territorio, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomleft",pal = wardpal_pop_ter, values = ~fxa_pop_ter, opacity = 1.0, title = "Número estimado de habitantes") 
    
  })
  
  #######################################################################################
  #
  # PAGINA CENSO BAHIA - ABA 2
  #
  #######################################################################################
  
  ######################### Grafico de Barras - Populacao ao longo dos anos - ABA 2 ####################### 
  
  # Colocando separador de milhar no Grafico de Barras do CENSO - ABA 1 #012338
  
  output$censo_barras <- renderPlot({ 
    	
    ggplot(populacao_censo, aes(x=factor(Ano),y=Populacao)) + 
      geom_col(show.legend = FALSE, fill="#20B2AA",width = 0.7) +
      geom_text(aes(x=factor(Ano),y=Populacao,label=format(Populacao, big.mark = ".",decimal.mark=",")),
                position=position_dodge(width=0.9),
                vjust=-0.25, size=3, fontface='bold') +
      labs(x = "Ano", y="População") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=8),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5,face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10, vjust=0.5, angle = 45)) +
      scale_y_continuous(breaks=c(0,2000000,4000000,6000000,8000000,10000000,12000000,14000000,16000000),
                         labels =c("0","2.000.000","4.000.000","6.000.000","8.000.000","10.000.000",
                                   "12.000.000","14.000.000","16.000.000"))
      
  })
  
  ############################ Mapa - População Municipios - ABA 2 ############################
  
  
  ### Ajustes no dataset e JSON ###
  
  # mapa
  output$mapa_ba_mun <- renderLeaflet({
    
    # criando dataset com ano filtrado
    pop_total_mun1 <- pop_total_mun %>% filter(Ano==input$selectano_aba2)
    
    # ordenando variaveis
    pop_total_mun1$fxa_pop_mun <- factor(pop_total_mun1$fxa_pop_mun, levels = c("Até 10 mil",
                                                                                "10 mil - 50 mil",
                                                                                "50 mil - 100 mil",
                                                                                "100 mil - 200 mil",
                                                                                "200 mil - 400 mil",
                                                                                "400 mil - 600 mil",
                                                                                "Mais de 600 mil"))
    
    
    # banco unindo mapa com os dados #
    pop_total_mun2 <- merge(mapa_ba_mun, pop_total_mun1, by = "CD_GEOCMU")
    
    # paleta
    wardpal_pop_mun <- colorFactor(brewer.pal(7, "YlOrBr"),
                                   domain=factor(pop_total_mun2$fxa_pop_mun, 
                                                 levels=c("Até 10 mil",
                                                          "10 mil - 50 mil",
                                                          "50 mil - 100 mil",
                                                          "100 mil - 200 mil",
                                                          "200 mil - 400 mil",
                                                          "400 mil - 600 mil",
                                                          "Mais de 600 mil")))
    
    
    leaflet(pop_total_mun2, options = leafletOptions(zoomControl = FALSE,
                                                     minZoom = 5.5, maxZoom = 7.5, 
                                                     dragging = TRUE,
                                                     doubleClickZoom=FALSE)) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
      addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal_pop_mun(fxa_pop_mun),
                  label = ~paste0(Municipios, ": ", format(total_pop_mun, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomleft",pal = wardpal_pop_mun, values = ~fxa_pop_mun, opacity = 1.0, title = "Número estimado de habitantes") 
  })
  
  
  ############################# Mapa com Territorios de Identidade - ABA 2 ###########################
  
  # convertendo variavel de caracter em numerica - ABA 1
  territorios_geo$CD_TI <- as.numeric(territorios_geo$CD_TI)
  
  # mapa
  output$mapa_bahia <- renderLeaflet({
    
    # criando dataset com ano filtrado
    territorios_pop_v2 <- territorios_pop %>% filter(Ano==input$selectano_aba2)
    
    # ordenando variaveis
    territorios_pop_v2$faixas_pop <- factor(territorios_pop_v2$faixas_pop, levels = c("Até 200 mil",
                                                                                      "200 mil - 400 mil",
                                                                                      "400 mil - 600 mil",
                                                                                      "600 mil - 800 mil",
                                                                                      "800 mil - 1 milhão",
                                                                                      "Mais de 1 milhão"))
    
    
    # banco unindo mapa com os dados #
    territorios_pop_v3 <- merge(territorios_geo, territorios_pop_v2, by = "CD_TI")
    
    # paleta
    wardpal <- colorFactor(c("#fee391", "#fec44f","#fe9929","#ec7014","#8c2d04","#461602"),
                           domain=factor(territorios_pop_v3$faixas_pop, 
                                         levels=c("Até 200 mil",
                                                  "200 mil - 400 mil",
                                                  "400 mil - 600 mil",
                                                  "600 mil - 800 mil",
                                                  "800 mil - 1 milhão",
                                                  "Mais de 1 milhão")))
    
    # mapa final
    leaflet(territorios_pop_v3, options = leafletOptions(zoomControl = FALSE,
                                                         minZoom = 5.5, maxZoom = 7.5, 
                                                         dragging = TRUE,
                                                         doubleClickZoom=FALSE)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lat = -13.800000, lng = -41.559343, zoom = 5.5) %>% 
      addPolygons(stroke = T, opacity = 1, color = "grey",weight = 0.5, smoothFactor = 0.0, fillOpacity = 1,
                  fillColor = ~wardpal(faixas_pop),
                  label = ~paste0(NM_TI, ": ", format(total_ter, big.mark = ".",decimal.mark=",")," pessoas")) %>%
      addLegend("bottomleft",pal = wardpal, values = ~faixas_pop, opacity = 1.0, title = "Número estimado de habitantes") 
    
    
  })
  
  #######################################################################################
  #
  # PAGINA PROJECOES - ABA 3
  #
  #######################################################################################
  
  
  ######################## Value Box's - ABA 3 ############################
  
  ########## Box com populacao total - ABA 3  ########## 
  output$poptotalproj <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(PopTotal)),"População baiana", icon = icon("users"),
      color = "blue"
    )
  })
  
  ########## Box com tft projetada - ABA 3  ########## 
  output$tftproj <- renderValueBox({
    valueBox(
      format(subset(x=proj18,subset=(Ano==input$sliderano),select = c(TFT)), nsmall=0,  big.mark=".", decimal.mark=","),"filhos por mulher", icon = icon("child"),
      color = "yellow"
    )
  })

  ########## Box EVN - ABA 3  ########## 
  output$EVN <- renderValueBox({
    valueBox(
      subset(x=proj18,subset=(Ano==input$sliderano),select = c(EVNAmbos)),"Expectativa de vida do baiano", icon = icon("heart"),
      color = "green"
    )
  })
  
  ########## Box com Mortalidade Infantil - ABA 3  ########## 
  output$MortInf <- renderValueBox({
    valueBox(
      paste0(subset(x=proj18,subset=(Ano==input$sliderano),select = c(Pop60Mais)),"%"),"dos baianos tem 60+ anos", icon = icon("user",lib = "glyphicon"),
      color = "maroon"
    )
  })
  
  
################ Piramide etaria da pagina projecoes - ABA 3 #######################
  
   
  output$piramide <- renderPlot({  
    
    popfaixaquinquenal <- popfaixaquinquenal %>%
    mutate(Genero=recode(Genero, 
                         "Homem"="Masculino",
                         "Mulher"="Feminino"))
    
    # ordenando o eixo X
    popfaixaquinquenal$Genero <- factor(popfaixaquinquenal$Genero, levels = c("Masculino",	"Feminino"))
    
    # grafico
    ggplot(subset(popfaixaquinquenal, subset= (Ano==input$sliderano)), aes(x=FaixaEtaria, y=Populacao, fill=Genero)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c( "#1b6db5", "#b51b8f"))+
      labs(x="Faixa Etária", y="Total por sexo", fill="Sexo: ") +
      theme_classic() +
      theme(legend.position="bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"))+
      theme(plot.title = element_text(hjust = 0.5, face="bold", size = 17),
            axis.title.x = element_text(size=12, colour = "black"),
            axis.title.y = element_text(size=12,  colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),
            legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-700000,700000, 200000),
                         labels = paste0(as.character(c(seq(700, 0, -200),
                                                        seq(100, 700, 200))), "Mil"))

  }) 
  
  
################### Grafico de Pizza - Genero - ABA 3 ################################  
  
  output$genero_proj <- renderPlot({  
    
  
  # filtrando e criando a coluna porcentagem
  proj_2020_2060 <- proj_2020_gen %>%
    filter(Anos==input$sliderano) %>%
    mutate(sex_percent = total_gen/sum(total_gen)*100 )
  
  # ordenando o eixo X
  proj_2020_2060$genero <- factor(proj_2020_2060$genero, levels = c("Masculino",	"Feminino"))
  
  
  # criando coluna com a posicao da legenda
  
  proj_2020_2060 <- proj_2020_2060 %>% arrange(desc(genero)) %>%
    mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent)
  
  
  # grafico 
  ggplot(proj_2020_2060,aes(x="", y=sex_percent, fill=genero)) +
    geom_bar(width=1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
              color = 'black',fontface='bold') +
    labs(x="",y="", fill="Sexo: ") +
    theme_minimal() +
    theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
          legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
          legend.text = element_text(size=10, face="bold"),
          plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
    scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank())
  
  })
  
################### Expectativa de Vida ao Nascer - ABA 3 #######################

  output$EVNporSexo <- renderPlot({ 
    
    ggplot(proj18, aes(x=Ano)) +
      geom_area(aes(y=EVNFem),fill="#b51b8f",alpha=0.3) +
      geom_area(aes(y=EVNMasc),fill="#1b6db5",alpha=0.7) +
      geom_line(aes(y=EVNFem,color="#b51b8f"), size=1.2) +
      geom_line(aes(y=EVNMasc,color="#1b6db5"), size=1.2) +           
      labs(x="Ano", y="Anos de Vida") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),  legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold"), 
            legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank")) +
      scale_colour_manual(name = 'Sexo: ', 
                          values = c("#1b6db5","#b51b8f"), labels = c('Masculino','Feminino'))
    

  })
  
  
  #################### Grafico da populacao geral por ano - ABA 3 #######################
  
  # cores 
  
  # paleta2 <- c("#F4A989","#F0936D","#E47658","#D35749","#C2373A",
  #              "#B2182B", "#d63e00", "#f7723b", "#F8BFA5","#FCD6C1",
  #              "#F3DDD0","#E7E0DB","#DAE2E6","#CBE1EE","#ADD1E5","#90C0DB")
  # 
  output$GrafPopTotal <- renderPlot({ 
    ggplot(data=subset(x=proj18,subset=(Ano %in% seq(2010,2060,by=5)))) + 
      geom_col(aes(x=as.factor(Ano),y=PopTotal),fill= "#00bdaa",show.legend = FALSE) + 
      #scale_fill_manual(values = paleta2) +
      geom_text(aes(x=factor(Ano),y=PopTotal,label=format(PopTotal, big.mark = ".",decimal.mark=",")), position=position_dodge(width=0.9),
                vjust=-0.25, size=3, fontface='bold') +
      labs(x = "Ano", y = "População") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, face="bold",size = 17),
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12, colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10,vjust=0.5, angle = 45))
    
  })
  
################### Grafico de TBN e TBM - ABA 3 ###################

    output$TaxasBrutas <- renderPlot({      
    ggplot(proj18,aes(x=Ano)) +
      geom_area(aes(y=TBM), fill="brown4", alpha=0.6) +
      geom_area(aes(y=TBN), fill="orange1",alpha=0.3) +
      geom_line(aes(y=TBM,color="red"), size=1.2) +
      geom_line(aes(y=TBN,color="orange"), size=1.2) +           
      labs(y="Total Bruto, por mil pessoas") +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            plot.title = element_text(colour = "black", size = 15, hjust=0.5, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold"), 
            legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank")) +
      scale_colour_manual(name = 'Legenda', 
                          values =c('red'='red','orange'='orange'), labels = c('TBN','TBM')) 

  })
  
  
  ########################################################################################################
  #
  # ABA 4 - PAGINA SOBRE INFORMACOES SOBRE MUNICIPIOS 
  #
  ########################################################################################################
  
  
  ############################ Value Box's - ABA 4 ################################
  
  ############ Value Box - Pop total Mun - ABA 4 ################
  
  output$pop_tot_mun <- renderValueBox({
    info_mun %>% 
      filter(Ano==input$selectano & Codigo.do.Municipio ==input$selectmuni) %>% 
        group_by(Municipios) %>%
          summarise(cont_pop=sum(total_homens,total_mulheres)) %>%
            select(cont_pop) %>%
              as.numeric() %>%
                format(nsmall=0,  big.mark=".", decimal.mark=",") %>%
                  valueBox("População total no município", icon = icon("users"),color = "green")
  })
  

  output$nascidos_mortos <- renderValueBox({
    valueBox(
      sprintf("%1.2f%%",subset(x=mortalidade,subset=(Codigo.do.Municipio ==input$selectmuni),
                               select = c(percentual_nascidosvivos))),"Percentual de nascidos mortos (2010)",
      icon = icon("user-minus"),
      color = "blue"
    )
  })  
  
  ################ Value Box - Urbanizacao - ABA 4 ##################
  
  output$urbanizacao_mun <- renderValueBox({
    
    # filtro para SALVADOR e pegando apenas inf sobre urbanizacao
    
    info_urban <- urbanizacao_municipios %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(status) %>% 
            summarise(cont_total=sum(total)) 

    #PERCENTUAL
    
    info_urban = mutate(info_urban, total_percent = cont_total/sum(cont_total)*100)
    
    #filtrando apenas o valor "Urbano" 
    info_urban <- info_urban %>% filter(status=="Total_urb")
    

    valueBox(
      sprintf("%1.2f%%",subset(x=info_urban,select = c(total_percent))),
      "Taxa de urbanização", icon = icon("city"),
      color = "yellow"
    )
  })
  
  
  
############################ Grafico de barras - Raca\Cor - ABA 4 ########################

  output$raca <- renderPlot({
    
    raca_long_filtrado <- raca_long %>% 
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni)
    
    # grafico
    ggplot(raca_long_filtrado, aes(x=reorder(Cor,+PercentCor), y=PercentCor)) + 
      geom_bar(stat="identity", fill="#40739e") +
      geom_text(aes(label=sprintf("%1.2f%%",PercentCor)),size = 4.2, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            axis.text.x = element_text(face="bold", color="#000000", size=10),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5),
            legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Cor/Raça") + 
      guides(fill=FALSE) 

  })
  
################################ Grafico de Pizza - Fxa etaria - ABA 4 ###############################
  
  
  output$fxa_etaria <- renderPlot({
    
    # filtrando e criando a coluna porcentagem
    pizza_graf_fxa <- fxaetaria_long %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(status) %>%
            summarise(cont_fxa=sum(total))
    
    # criando coluna com a posicao da legenda
    
    pizza_graf_fxa <- pizza_graf_fxa %>% arrange(desc(status)) %>%
      mutate(yposicao_legenda = cumsum(cont_fxa)- 0.5*cont_fxa)
    
    # alterando o nome das variaveis 
    pizza_graf_fxa$status <- factor(pizza_graf_fxa$status, levels=c("fxa_0a14",
                                                                    "fxa_15a59",
                                                                    "fxa_60mais"),
                                    labels=c("0 a 14", "15 a 59", "+60"))
    
    # grafico
    ggplot(pizza_graf_fxa,aes(x="", y=cont_fxa, fill=status)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",cont_fxa)), size = 4,
                color = 'black',fontface='bold') +
      scale_fill_brewer(palette = "YlOrBr") +
      labs(x="",y="", fill="Faixa etária (em anos): ") +
      theme_minimal() +
      theme(legend.position="bottom", 
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            legend.text = element_text(size=12, face="bold"),legend.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())


  })
  
########################### Grafico de Barras - Estado Civil - ABA 4 ########################### 
  
  output$estado_civil <- renderPlot({
    
    # filtrando ano e municipio
    estado_civil_v2 <- estado_civil_agreg %>%
       filter(Ano==input$selectano) %>%
         filter(Codigo.do.Municipio ==input$selectmuni) %>%
           group_by(situacao_conjugal) %>% 
             summarise(cont_civil=sum(total_civil))
    
    # calculando a porcentagem
    estado_civil_v2 = mutate(estado_civil_v2,civil_percent = cont_civil/sum(cont_civil)*100 )
    
    # Ajustando o eixo x
    estado_civil_v2 <- estado_civil_v2 %>% mutate(situacao_conjugal=recode(situacao_conjugal, 
                         "separado_jud"="Separado Jud.","Divorciado"="Divorciado",
                         "Viuvo" = "Viúvo", "casado" = "Casado", "Solteiro" = "Solteiro"))
    
    # grafico
    ggplot(estado_civil_v2, aes(x=reorder(situacao_conjugal, +civil_percent), y=civil_percent)) + 
      geom_bar(stat="identity",fill="#DC143C") +
      geom_text(aes(label=sprintf("%1.2f%%",civil_percent)),size = 4, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            axis.text.x = element_text(face="bold",color="#000000", size=10),
            legend.text = element_text(size=9, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold"), 
            legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Estado Civil") +
      guides(fill=F)

  })
  
############################# Grafico de Pizza - Naturalidade - ABA 4 #############################
  
  
  paleta_nat <- c("#f8c43a", "#35d0ba")

  
  output$naturalidade <- renderPlot({
    
  
  # filtro 
  pizza_graf_nat <- naturalide_long %>%
                      filter(Ano==input$selectano) %>%
                       filter(Codigo.do.Municipio ==input$selectmuni) %>%
                        group_by(local_natural) %>%
                          summarise(cont_nat=sum(total_natural))
  
  # coluna com percentual
  pizza_graf_nat = mutate(pizza_graf_nat, nat_percent = cont_nat/sum(cont_nat)*100 )
  
  # posicao da legenda
  pizza_graf_nat <- pizza_graf_nat %>% arrange(desc(cont_nat)) %>%
    mutate(yposicao_legenda = cumsum(nat_percent)- 0.5*nat_percent)
  
  # alterando o nome das variaveis 
  pizza_graf_nat$local_natural <- factor(pizza_graf_nat$local_natural,levels = c("não_natural_mun","natural.do.mun"),
                                         labels = c("Não-Natural", "Natural"))
  
  # grafico
  ggplot(pizza_graf_nat,aes(x="", y=nat_percent, fill=local_natural)) +
    geom_bar(width=1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",nat_percent)), size = 4,
              color = 'black',fontface='bold') +
    labs(x="",y="", fill="Naturalidade: ") +
    theme_minimal() +
    theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
          legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
          legend.text = element_text(size=12, face="bold"), 
          plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
    scale_fill_manual(values = paleta_nat) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank())


  
  })
  
################################# Grafico de pizza - Genero - ABA 4 ##################################
  
  output$genero_mun <- renderPlot({
    
    # filtro para municipio e pegando apenas inf sobre SEXO
    
    genero_mun_v2 <- genero_mun %>%
      filter(Ano==input$selectano) %>%
        filter(Codigo.do.Municipio ==input$selectmuni) %>%
          group_by(genero) %>%
            summarise(cont_sex=sum(total_genero))
    
    #PERCENTUAL
    
    genero_mun_v2 = mutate(genero_mun_v2, sex_percent = cont_sex/sum(cont_sex)*100 )
    
    # posicao da legenda
    
    genero_mun_v2 <- genero_mun_v2 %>%
      arrange(desc(cont_sex)) %>%
        mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent )
    
    # alterando o nome das variaveis 
    genero_mun_v2$genero <- factor(genero_mun_v2$genero, levels=c("total_homens","total_mulheres"),
                                   labels=c("Masculino","Feminino"))
    
    
    # grafico
    ggplot(genero_mun_v2,aes(x="", y=sex_percent, fill=genero)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Sexo: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            legend.text = element_text(size=12, face="bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())

  })

################################ Piramide Etaria CENSO -  ABA 4 ###################################
  
  output$piramide_censo <- renderPlot({
    
    # fazendo filtro e selecionando municipio
    
    piramide_filter <- piramide_municipios %>%
      filter(Ano==input$selectano) %>%
        filter(codigo_municipio ==input$selectmuni) %>%
          group_by(faixa_etarias,sexo) %>%
            summarise(cont_total=sum(total_faixa))
    

    # Ajustando o eixo y
    piramide_filter$faixa_etarias <- factor(piramide_filter$faixa_etarias, levels = c("Entre.0.a.4",	"Entre.5.a.9",
                                                                                      "Entre.10.a.14",	"Entre.15.a.19",
                                                                                      "Entre.20.a.24",	"Entre.25.a.29",
                                                                                      "Entre.30.a.34",	"Entre.35.a.39",
                                                                                      "Entre.40.a.44",	"Entre.45.a.49",
                                                                                      "Entre.50.a.54",	"Entre.55.a.59",
                                                                                      "Entre.60.a.64",	"Entre.65.a.69",
                                                                                      "Entre.70.a.74",	"Entre.75.a.79",
                                                                                      "Entre.80.a.84",	"Entre.85.a.89",
                                                                                      "Entre.90.a.94",	"Entre.95.a.99",
                                                                                      "Mais.de.100"),
                                            labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                                                       "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                                                       "60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89",
                                                       "90 a 94","95 a 99","+100"))
    # ordenando o eixo X
    piramide_filter$sexo <- factor(piramide_filter$sexo, levels = c("Masculino",	"Feminino"))
                                                                                      
    # grafico
    ggplot(piramide_filter, aes(x=faixa_etarias, y=cont_total, fill=sexo)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      labs(x="Faixa Etaria", y="Total por sexo" ,fill="Sexo: ") +
      theme_classic() +
      theme(legend.position="bottom", 
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            plot.title = element_text(hjust = 0.5,face="bold", colour = "black",size = 17), 
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12,  colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-200000,200000, 50000),
                         labels = paste0(as.character(c(seq(200, 0, -50),
                                                        seq(50, 200, 50))), "Mil"))

    
  })
  
  #####################################################################################################
  #
  ######### ABA 5 - Pagina com informacoes sobre Territorio de Identidade
  #
  #####################################################################################################
  
  ##### Grafico de Barras - Cor/Raca - Territorio de Identidade - ABA 5 #####
  
  output$raca_ter <- renderPlot({
    
    raca_filtrado <- raca_long %>% 
      filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(Cor) %>% summarise(cont_civil=mean(PercentCor))
    
    # grafico 
    ggplot(raca_filtrado, aes(x=reorder(Cor, +cont_civil), y=cont_civil)) + 
      geom_bar(stat="identity",fill="#40739e") +
      geom_text(aes(label=sprintf("%1.2f%%",cont_civil)),size = 4.2, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=10, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5),
            legend.position = "bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Cor/Raça") + 
      guides(fill = F)

  })
  
################################ Grafico de Barras - Estado Civil - ABA 5 ##########################

  output$estado_civil_ter <- renderPlot({
    
    # Filtrando territorio de identidade e ano
    estado_civil_v2 <- estado_civil_agreg %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(situacao_conjugal) %>% summarise(cont_civil=sum(total_civil))
    
    # calculando a porcentagem
    estado_civil_v2 = mutate(estado_civil_v2,civil_percent = cont_civil/sum(cont_civil)*100 )
    
    # Ajustando o eixo x
    estado_civil_v2 <- estado_civil_v2 %>% mutate(situacao_conjugal=recode(situacao_conjugal, 
                                                                           "separado_jud"="Separado Jud.","Divorciado"="Divorciado",
                                                                           "Viuvo" = "Viúvo", "casado" = "Casado", "Solteiro" = "Solteiro"))
    
    # grafico
    ggplot(estado_civil_v2, aes(x=reorder(situacao_conjugal, +civil_percent), y=civil_percent)) + 
      geom_bar(stat="identity",fill="#DC143C") +
      geom_text(aes(label=sprintf("%1.2f%%",civil_percent)),size = 4, position =position_dodge(width=0.9),
                vjust=-0.25, color = 'black',fontface='bold') +
      theme_classic() +
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", size=10),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text=element_text(size=6, face="bold"),
            legend.text = element_text(size=10, face="bold"),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold"),
            legend.position = "bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
      labs(y="Percentual", x="Estado Civil") +
      guides(fill=F)

  })
  
################################### Grafico de Pizza - Genero - ABA 5 ################################
  
  output$genero_ter <- renderPlot({
    
    # filtro para Territorios e pegando apenas inf sobre SEXO
    genero_territorio <- genero_mun %>% filter(Ano==input$selectanov1) %>%
      filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(genero) %>% summarise(cont_sex=sum(total_genero))
    
    #PERCENTUAL
    genero_territorio = mutate(genero_territorio, sex_percent = cont_sex/sum(cont_sex)*100 )
    
    # posicao da legenda
    genero_territorio <- genero_territorio %>% arrange(desc(cont_sex)) %>%
      mutate(yposicao_legenda = cumsum(sex_percent)- 0.5*sex_percent )
    
    # alterando o nome das variaveis 
    genero_territorio$genero <- factor(genero_territorio$genero, levels=c("total_homens","total_mulheres"),
                                   labels=c("Masculino","Feminino"))
    
    
    # grafico 
    ggplot(genero_territorio,aes(x="", y=sex_percent, fill=genero)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",sex_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Sexo: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            legend.text = element_text(size=12, face="bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
      scale_fill_manual(values = c("#1b6db5","#b51b8f")) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())


  })
########################### Grafico de Pizza - Fxa Etaria - ABA 5 #################################
  
  output$fxa_et_terr <- renderPlot({
    
    # filtrando territorio de identidade e criando a coluna porcentagem
    
    fxa_etaria_terr <- fxaetaria_long %>% filter(Ano==input$selectanov1) %>%
      filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(status) %>% summarise(cont_fxa=mean(total))
    
    
    # criando coluna com a posicao da legenda
    
    fxa_etaria_terr <- fxa_etaria_terr %>% arrange(desc(status)) %>%
      mutate(yposicao_legenda = cumsum(cont_fxa)- 0.5*cont_fxa)
    
    # alterando o nome das variaveis 
    fxa_etaria_terr$status <- factor(fxa_etaria_terr$status, levels=c("fxa_0a14",
                                                                    "fxa_15a59",
                                                                    "fxa_60mais"),
                                    labels=c("0 a 14", "15 a 59", "+60"))
    
    # grafico
    ggplot(fxa_etaria_terr,aes(x="", y=cont_fxa, fill=status)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",cont_fxa)), size = 4,
                color = 'black',fontface='bold') +
      scale_fill_brewer(palette = "YlOrBr") +
      labs(x="",y="", fill="Faixa etária (em anos): ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"), 
            legend.text = element_text(size=12, face="bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())

  })
  
################################ Grafico de Pizza - Naturalidade - ABA 5 ###############################
  
  output$naturalidade_terr <- renderPlot({
    
    # filtro para territorio de identidade
    
    naturalidade_terr <- naturalide_long %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(local_natural) %>% summarise(cont_nat=sum(total_natural))
    
    # coluna com percentual
    
    naturalidade_terr = mutate(naturalidade_terr, nat_percent = cont_nat/sum(cont_nat)*100 )
    
    # posicao da legenda
    
    naturalidade_terr <- naturalidade_terr %>% arrange(desc(cont_nat)) %>%
      mutate(yposicao_legenda = cumsum(nat_percent)- 0.5*nat_percent )
    
    # alterando o nome das variaveis 
    naturalidade_terr$local_natural <- factor(naturalidade_terr$local_natural,levels = c("não_natural_mun","natural.do.mun"),
                                           labels = c("Não-Natural", "Natural"))
    
    # grafico
    ggplot(naturalidade_terr,aes(x="", y=nat_percent, fill=local_natural)) +
      geom_bar(width=1, stat = "identity") +
      coord_polar("y", start=0) +
      geom_text(aes(y=yposicao_legenda, label = sprintf("%1.2f%%",nat_percent)), size = 4,
                color = 'black',fontface='bold') +
      labs(x="",y="", fill="Naturalidade: ") +
      theme_minimal() +
      theme(legend.position="bottom", legend.title = element_text(colour="Black", size=12, face="bold"),
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            legend.text = element_text(size=12, face="bold"),
            plot.title = element_text(colour = "black", size = 17, hjust=0.5, face="bold")) +
      scale_fill_manual(values = paleta_nat) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())

  })
  
################### Piramide CENSO - Territorio de Identidade - ABA 5 #######################
  
  output$piramide_terr <- renderPlot({
    
    # fazendo filtro 

    piramide_filter_ter <- piramide_municipios %>% filter(Ano==input$selectanov1) %>% filter(Territorios.de.Identidade ==input$select_ter) %>%
      group_by(faixa_etarias,sexo) %>% summarise(cont_total=sum(total_faixa))
    
    
    # Ajustando o eixo y
    piramide_filter_ter$faixa_etarias <- factor(piramide_filter_ter$faixa_etarias, levels = c("Entre.0.a.4",	"Entre.5.a.9",
                                                                                      "Entre.10.a.14",	"Entre.15.a.19",
                                                                                      "Entre.20.a.24",	"Entre.25.a.29",
                                                                                      "Entre.30.a.34",	"Entre.35.a.39",
                                                                                      "Entre.40.a.44",	"Entre.45.a.49",
                                                                                      "Entre.50.a.54",	"Entre.55.a.59",
                                                                                      "Entre.60.a.64",	"Entre.65.a.69",
                                                                                      "Entre.70.a.74",	"Entre.75.a.79",
                                                                                      "Entre.80.a.84",	"Entre.85.a.89",
                                                                                      "Entre.90.a.94",	"Entre.95.a.99",
                                                                                      "Mais.de.100"),
                                            labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                                                       "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                                                       "60 a 64","65 a 69","70 a 74","75 a 79","80 a 84","85 a 89",
                                                       "90 a 94","95 a 99","+100"))
    # ordenando o eixo X
    piramide_filter_ter$sexo <- factor(piramide_filter_ter$sexo, levels = c("Masculino",	"Feminino"))
    
    
    # grafico
    ggplot(piramide_filter_ter, aes(x=faixa_etarias, y=cont_total, fill=sexo)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values = c("#1b6db5", "#b51b8f")) +
      labs(x="Faixa Etaria", y="Total por sexo") +
      theme_classic() +
      theme(legend.position="bottom",
            legend.background = element_rect(fill="ghostwhite", size=0.5, linetype="blank"),
            plot.title = element_text(hjust = 0.5,face="bold", colour = "black",size = 17),
            axis.title.x = element_text(size=12,  colour = "black"),
            axis.title.y = element_text(size=12, colour = "black"),
            axis.line = element_line(colour = "black", 
                                     size = 1, linetype = "solid"),
            axis.text.y = element_text(face="bold", color="#000000", 
                                       size=10),
            axis.text.x = element_text(face="bold", color="#000000", 
                                       size=10), legend.text = element_text(size=12, face="bold"),
            legend.title = element_text(size = 12, face = "bold")) +
      scale_y_continuous(breaks = seq(-200000,200000, 50000),
                         labels = paste0(as.character(c(seq(200, 0, -50),
                                                        seq(50, 200, 50))), "Mil")) +
      labs(fill= "Sexo: ")


  })
  
  ################ Titulo reativo - Projeção ###############
  output$proj_text_piramide <- renderText({
    paste0("Pirâmida etária, Bahia, ", input$sliderano,".")
  })
  
  output$proj_text_pizza <- renderText({
    paste0("Sexo da população, Bahia, ", input$sliderano,".")
  })

  ################ Titulo reativo - Municipios ###############
  output$mun_piramide <- renderText({
    paste("Pirâmide etária da população, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  output$mun_sexo <- renderText({
    paste("Sexo da população, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  output$mun_fxaetaria <- renderText({
    paste("Faixa etária da população, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  output$mun_raca <- renderText({
    paste("Cor/Raça da população, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  output$mun_civil <- renderText({
    paste("Estado civil da população, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  output$mun_nat <- renderText({
    paste("Naturalidade da população em relação ao município, ", estimativa[estimativa$CD_GEOCMU==input$selectmuni,"NM_MUNICIP"],".")
  })
  
  ################ Titulo reativo - Territorio ###############
  
  output$ter_piramide <- renderText({
    paste("Pirâmide etária da população, ", input$select_ter,".")
  })
  
  output$ter_sexo_t <- renderText({
    paste("Sexo da população, ", input$select_ter,".")
  })
  
  output$ter_fxaetaria <- renderText({
    paste("Faixa etária da população, ", input$select_ter,".")
  })
  
  output$ter_raca <- renderText({
    paste("Cor/Raça da população, ", input$select_ter,".")
  })
  
  output$ter_civil <- renderText({
    paste("Estado civil da população, ", input$select_ter,".")
  })
  
  output$ter_nat <- renderText({
    paste("Naturalidade da população em relação ao território de identidade, ", input$select_ter,".")
  })
  
  # teste
   # output$downloadPlot <- downloadHandler(
   #   filename = function(){paste(input$piramide,'.png',sep='')},
   #   content = function(piramide){
   #     ggsave(plot = piramide, "piramide.png",
   #            width = 10, height = 5, dpi = 120, units = "in")
   #    # ggsave(piramide,plotInput())
   #   }
   # )
   # 
   # output$downloadPlot <- downloadHandler(
   #  # filename = function(piramide){paste(input$piramide,'.png',sep='')},
   #   filename = "piramide.png",
   #   content = function(piramide) {
   #     png(piramide)
   #     #plotInput()
   #     dev.off()
   #   })    
   
   # output$downloadPlot <-downloadHandler(
   #   filename=function(){
   #     paste("piramide","png",sep=".")},
   #     content=function(file) {
   #     png(file)
   #     print(piramide())
   #     dev.off() 
   #   }
   # )
   
  # plotInput <- function(){output$piramide}
  # 
  #  output$downloadPlot <- downloadHandler(
  #    filename = "piramide.png",
  #    content = function(piramide) {
  #           png(output$piramide)
  #           plotInput()
  #           dev.off()
  #         })  

}





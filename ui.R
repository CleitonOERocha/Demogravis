

################################################################################################### 
# SEIDataLab - Laboratorio de Dados da Superintendencia de Estudos Economicos e Sociais da Bahia
################################################################################################### 
#####   DESCRIÇÃO:        dashboard Demogravis do InfoVis
#####   ESCRITO POR:      Cleiton Rocha, Jackson Conceicao, Jonatas Silva, Kellyene Coleho, Rodrigo Cerqueira
#####   SITE:             https://infovis.sei.ba.gov.br/demogravis/#
#####   LICENÇA:          GPLv3

#Carregando Pacote para fazer aplicativos shiny
library(shiny)
library(shinydashboard)
library(lubridate)
library(shinydashboardPlus)
library(rgdal)
library(leaflet)
library(png)
library(shinycssloaders)
library(fresh)

# lista com municipios

nomesmuni <- list("ABAÍRA" = 2900108,	"ABARÉ" = 2900207,	"ACAJUTIBA" = 2900306,	"ADUSTINA" = 2900355,	"ÁGUA FRIA" = 2900405,
                  "AIQUARA" = 2900603,	"ALAGOINHAS" = 2900702,	"ALCOBAÇA" = 2900801,	"ALMADINA" = 2900900,	"AMARGOSA" = 2901007,
                  "AMÉLIA RODRIGUES" = 2901106,	"AMÉRICA DOURADA" = 2901155,	"ANAGÉ" = 2901205,	"ANDARAÍ" = 2901304,
                  "ANDORINHA" = 2901353,	"ANGICAL" = 2901403,	"ANGUERA" = 2901502,	"ANTAS" = 2901601,	"ANTÔNIO CARDOSO" = 2901700,
                  "ANTÔNIO GONÇALVES" = 2901809,	"APORÁ" = 2901908,	"APUAREMA" = 2901957,	"ARAÇÁS" = 2902054,	"ARACATU" = 2902005,
                  "ARACI" = 2902104,	"ARAMARI" = 2902203,	"ARATACA" = 2902252,	"ARATUÍPE" = 2902302,	"AURELINO LEAL" = 2902401,
                  "BAIANÓPOLIS" = 2902500,	"BAIXA GRANDE" = 2902609,	"BANZAÊ" = 2902658,	"BARRA" = 2902708,
                  "BARRA DA ESTIVA" = 2902807,	"BARRA DO CHOÇA" = 2902906,	"BARRA DO MENDES" = 2903003,	"BARRA DO ROCHA" = 2903102,
                  "BARREIRAS" = 2903201,	"BARRO ALTO" = 2903235,	"BARRO PRETO/LOMANTO JUNIOR" = 2903300,	"BARROCAS" = 2903276,
                  "BELMONTE" = 2903409,	"BELO CAMPO" = 2903508,	"BIRITINGA" = 2903607,	"BOA NOVA" = 2903706,
                  "BOA VISTA DO TUPIM" = 2903805,	"BOM JESUS DA LAPA" = 2903904,	"BOM JESUS DA SERRA" = 2903953,	"BONINAL" = 2904001,
                  "BONITO" = 2904050,	"BOQUIRA" = 2904100,	"BOTUPORÃ" = 2904209,	"BREJÕES" = 2904308,	"BREJOLÂNDIA" = 2904407,
                  "BROTAS DE MACAÚBAS" = 2904506,	"BRUMADO" = 2904605,	"BUERAREMA" = 2904704,	"BURITIRAMA" = 2904753,
                  "CAATIBA" = 2904803,	"CABACEIRAS DO PARAGUAÇU" = 2904852,	"CACHOEIRA" = 2904902,	"CACULÉ" = 2905008,
                  "CAÉM" = 2905107,	"CAETANOS" = 2905156,	"CAETITÉ" = 2905206,	"CAFARNAUM" = 2905305,	"CAIRU" = 2905404,
                  "CALDEIRÃO GRANDE" = 2905503,	"CAMACAN" = 2905602,	"CAMAÇARI" = 2905701,	"CAMAMU" = 2905800,
                  "CAMPO ALEGRE DE LOURDES" = 2905909,	"CAMPO FORMOSO" = 2906006,	"CANÁPOLIS" = 2906105,	"CANARANA" = 2906204,
                  "CANAVIEIRAS" = 2906303,	"CANDEAL" = 2906402,	"CANDEIAS" = 2906501,	"CANDIBA" = 2906600,	"CÂNDIDO SALES" = 2906709,
                  "CANSANÇÃO" = 2906808,	"CANUDOS" = 2906824,	"CAPELA DO ALTO ALEGRE" = 2906857,	"CAPIM GROSSO" = 2906873,
                  "CARAÍBAS" = 2906899,	"CARAVELAS" = 2906907,	"CARDEAL DA SILVA" = 2907004,	"CARINHANHA" = 2907103,
                  "CASA NOVA" = 2907202,	"CASTRO ALVES" = 2907301,	"CATOLÂNDIA" = 2907400,	"CATU" = 2907509,	"CATURAMA" = 2907558,
                  "CENTRAL" = 2907608,	"CHORROCHÓ" = 2907707,	"CÍCERO DANTAS" = 2907806,	"CIPÓ" = 2907905,	"COARACI" = 2908002,
                  "CÔCOS" = 2908101,	"CONCEIÇÃO DA FEIRA" = 2908200,	"CONCEIÇÃO DO ALMEIDA" = 2908309,	"CONCEIÇÃO DO COITÉ" = 2908408,
                  "CONCEIÇÃO DO JACUÍPE" = 2908507,	"CONDE" = 2908606,	"CONDEÚBA" = 2908705,	"CONTENDAS DO SINCORÁ" = 2908804,
                  "CORAÇÃO DE MARIA" = 2908903,	"CORDEIROS" = 2909000,	"CORIBE" = 2909109,	"CORONEL JOÃO SÁ" = 2909208,
                  "CORRENTINA" = 2909307,	"COTEGIPE" = 2909406,	"CRAVOLÂNDIA" = 2909505,	"CRISÓPOLIS" = 2909604,
                  "CRISTÓPOLIS" = 2909703,	"CRUZ DAS ALMAS" = 2909802,	"CURAÇÁ" = 2909901,	"DÁRIO MEIRA" = 2910008,
                  "DIAS D'ÁVILA" = 2910057,	"DOM BASÍLIO" = 2910107,	"DOM MACEDO COSTA" = 2910206,	"ELÍSIO MEDRADO" = 2910305,
                  "ENCRUZILHADA" = 2910404,	"ENTRE RIOS" = 2910503,	"ÉRICO CARDOSO" = 2900504,	"ESPLANADA" = 2910602,
                  "EUCLIDES DA CUNHA" = 2910701,	"EUNÁPOLIS" = 2910727,	"FÁTIMA" = 2910750,	"FEIRA DA MATA" = 2910776,
                  "FEIRA DE SANTANA" = 2910800,	"FILADÉLFIA" = 2910859,	"FIRMINO ALVES" = 2910909,	"FLORESTA AZUL" = 2911006,
                  "FORMOSA DO RIO PRETO" = 2911105,	"GANDU" = 2911204,	"GAVIÃO" = 2911253,	"GENTIO DO OURO" = 2911303,
                  "GLÓRIA" = 2911402,	"GONGOGI" = 2911501,	"GOVERNADOR MANGABEIRA" = 2911600,	"GUAJERU" = 2911659,
                  "GUANAMBI" = 2911709,	"GUARATINGA" = 2911808,	"HELIÓPOLIS" = 2911857,	"IAÇU" = 2911907,	"IBIASSUCÊ" = 2912004,
                  "IBICARAÍ" = 2912103,	"IBICOARA" = 2912202,	"IBICUÍ" = 2912301,	"IBIPEBA" = 2912400,	"IBIPITANGA" = 2912509,
                  "IBIQUERA" = 2912608,	"IBIRAPITANGA" = 2912707,	"IBIRAPOÃ" = 2912806,	"IBIRATAIA" = 2912905,	"IBITIARA" = 2913002,
                  "IBITITÁ" = 2913101,	"IBOTIRAMA" = 2913200,	"ICHU" = 2913309,	"IGAPORÃ" = 2913408,	"IGRAPIÚNA" = 2913457,
                  "IGUAÍ" = 2913507,	"ILHÉUS" = 2913606,	"INHAMBUPE" = 2913705,	"IPECAETÁ" = 2913804,	"IPIAÚ" = 2913903,
                  "IPIRÁ" = 2914000,	"IPUPIARA" = 2914109,	"IRAJUBA" = 2914208,	"IRAMAIA" = 2914307,	"IRAQUARA" = 2914406,
                  "IRARÁ" = 2914505,	"IRECÊ" = 2914604,	"ITABELA" = 2914653,	"ITABERABA" = 2914703,	"ITABUNA" = 2914802,
                  "ITACARÉ" = 2914901,	"ITAETÉ" = 2915007,	"ITAGI" = 2915106,	"ITAGIBÁ" = 2915205,	"ITAGIMIRIM" = 2915304,
                  "ITAGUAÇU DA BAHIA" = 2915353,	"ITAJU DO COLÔNIA" = 2915403,	"ITAJUÍPE" = 2915502,	"ITAMARAJU" = 2915601,
                  "ITAMARI" = 2915700,	"ITAMBÉ" = 2915809,	"ITANAGRA" = 2915908,	"ITANHÉM" = 2916005,	"ITAPARICA" = 2916104,
                  "ITAPÉ" = 2916203,	"ITAPEBI" = 2916302,	"ITAPETINGA" = 2916401,	"ITAPICURU" = 2916500,	"ITAPITANGA" = 2916609,
                  "ITAQUARA" = 2916708,	"ITARANTIM" = 2916807,	"ITATIM" = 2916856,	"ITIRUÇU" = 2916906,	"ITIÚBA" = 2917003,
                  "ITORORÓ" = 2917102,	"ITUAÇU" = 2917201,	"ITUBERÁ" = 2917300,	"IUIU" = 2917334,	"JABORANDI" = 2917359,
                  "JACARACI" = 2917409,	"JACOBINA" = 2917508,	"JAGUAQUARA" = 2917607,	"JAGUARARI" = 2917706,	"JAGUARIPE" = 2917805,
                  "JANDAÍRA" = 2917904,	"JEQUIÉ" = 2918001,	"JEREMOABO" = 2918100,	"JIQUIRIÇÁ" = 2918209,	"JITAÚNA" = 2918308,
                  "JOÃO DOURADO" = 2918357,	"JUAZEIRO" = 2918407,	"JUCURUÇU" = 2918456,	"JUSSARA" = 2918506,	"JUSSARI" = 2918555,
                  "JUSSIAPE" = 2918605,	"LAFAYETTE COUTINHO" = 2918704,	"LAGEDO DO TABOCAL" = 2919058,	"LAGOA REAL" = 2918753,
                  "LAJE" = 2918803,	"LAJEDÃO" = 2918902,	"LAJEDINHO" = 2919009,	"LAMARÃO" = 2919108,	"LAPÃO" = 2919157,
                  "LAURO DE FREITAS" = 2919207,	"LENÇÓIS" = 2919306,	"LICÍNIO DE ALMEIDA" = 2919405,
                  "LIVRAMENTO DE N. SENHORA" = 2919504,	"LUÍS EDUARDO MAGALHÃES" = 2919553,	"MACAJUBA" = 2919603,	"MACARANI" = 2919702,
                  "MACAÚBAS" = 2919801,	"MACURURÉ" = 2919900,	"MADRE DE DEUS" = 2919926,	"MAETINGA" = 2919959,	"MAIQUINIQUE" = 2920007,
                  "MAIRI" = 2920106,	"MALHADA" = 2920205,	"MALHADA DE PEDRAS" = 2920304,	"MANOEL VITORINO" = 2920403,
                  "MANSIDÃO" = 2920452,	"MARACÁS" = 2920502,	"MARAGOGIPE" = 2920601,	"MARAÚ" = 2920700,	"MARCIONÍLIO SOUZA" = 2920809,
                  "MASCOTE" = 2920908,	"MATA DE SÃO JOÃO" = 2921005,	"MATINA" = 2921054,	"MEDEIROS NETO" = 2921104,
                  "MIGUEL CALMON" = 2921203,	"MILAGRES" = 2921302,	"MIRANGABA" = 2921401,	"MIRANTE" = 2921450,
                  "MONTE SANTO" = 2921500,	"MORPARÁ" = 2921609,	"MORRO DO CHAPÉU" = 2921708,	"MORTUGABA" = 2921807,
                  "MUCUGÊ" = 2921906,	"MUCURI" = 2922003,	"MULUNGU DO MORRO" = 2922052,	"MUNDO NOVO" = 2922102,
                  "MUNIZ FERREIRA" = 2922201,	"MUQUÉM DO SÃO FRANCISCO" = 2922250,	"MURITIBA" = 2922300,	"MUTUÍPE" = 2922409,
                  "NAZARÉ" = 2922508,	"NILO PEÇANHA" = 2922607,	"NORDESTINA" = 2922656,	"NOVA CANAÃ" = 2922706,
                  "NOVA FÁTIMA" = 2922730,	"NOVA IBIÁ" = 2922755,	"NOVA ITARANA" = 2922805,	"NOVA REDENÇÃO" = 2922854,
                  "NOVA SOURE" = 2922904,	"NOVA VIÇOSA" = 2923001,	"NOVO HORIZONTE" = 2923035,	"NOVO TRIUNFO" = 2923050,	
                  "OLINDINA" = 2923100,	"OLIVEIRA DOS BREJINHOS" = 2923209,	"OURIÇANGAS" = 2923308,	"OUROLÂNDIA" = 2923357,
                  "PALMAS DE MONTE ALTO" = 2923407,	"PALMEIRAS" = 2923506,	"PARAMIRIM" = 2923605,	"PARATINGA" = 2923704,
                  "PARIPIRANGA" = 2923803,	"PAU BRASIL" = 2923902,	"PAULO AFONSO" = 2924009,	"PÉ DE SERRA" = 2924058,
                  "PEDRÃO" = 2924108,	"PEDRO ALEXANDRE" = 2924207,	"PIATÃ" = 2924306,	"PILÃO ARCADO" = 2924405,	"PINDAÍ" = 2924504,
                  "PINDOBAÇU" = 2924603,	"PINTADAS" = 2924652,	"PIRAÍ DO NORTE" = 2924678,	"PIRIPÁ" = 2924702,	"PIRITIBA" = 2924801,
                  "PLANALTINO" = 2924900,	"PLANALTO" = 2925006,	"POÇÕES" = 2925105,	"POJUCA" = 2925204,	"PONTO NOVO" = 2925253,
                  "PORTO SEGURO" = 2925303,	"POTIRAGUÁ" = 2925402,	"PRADO" = 2925501,	"PRESIDENTE DUTRA" = 2925600,
                  "PRESIDENTE JÂNIO QUADROS" = 2925709,	"PRESIDENTE TANCREDO NEVES" = 2925758,	"QUEIMADAS" = 2925808,
                  "QUIJINGUE" = 2925907,	"QUIXABEIRA" = 2925931,	"RAFAEL JAMBEIRO" = 2925956,	"REMANSO" = 2926004,
                  "RETIROLÂNDIA" = 2926103,	"RIACHÃO DAS NEVES" = 2926202,	"RIACHÃO DO JACUÍPE" = 2926301,
                  "RIACHO DE SANTANA" = 2926400,	"RIBEIRA DO AMPARO" = 2926509,	"RIBEIRA DO POMBAL" = 2926608,
                  "RIBEIRÃO DO LARGO" = 2926657,	"RIO DE CONTAS" = 2926707,	"RIO DO ANTÔNIO" = 2926806,	"RIO DO PIRES" = 2926905,
                  "RIO REAL" = 2927002,	"RODELAS" = 2927101,	"RUY BARBOSA" = 2927200,	"SALINAS DA MARGARIDA" = 2927309,
                  "SALVADOR" = 2927408,	"SANTA BÁRBARA" = 2927507,	"SANTA BRÍGIDA" = 2927606,	"SANTA CRUZ CABRÁLIA" = 2927705,
                  "SANTA CRUZ DA VITÓRIA" = 2927804,	"SANTA INÊS" = 2927903,	"SANTA LUZIA" = 2928059,
                  "SANTA MARIA DA VITÓRIA" = 2928109,	"SANTA RITA DE CÁSSIA" = 2928406,	"SANTA TEREZINHA" = 2928505,
                  "SANTALUZ" = 2928000,	"SANTANA" = 2928208,	"SANTANÓPOLIS" = 2928307,	"SANTO AMARO" = 2928604,
                  "SANTO ANTÔNIO DE JESUS" = 2928703,	"SANTO ESTEVÃO" = 2928802,	"SÃO DESIDÉRIO" = 2928901,
                  "SÃO DOMINGOS" = 2928950,	"SÃO FELIPE" = 2929107,	"SÃO FÉLIX" = 2929008,	"SÃO FÉLIX DO CORIBE" = 2929057,
                  "SÃO FRANCISCO DO CONDE" = 2929206,	"SÃO GABRIEL" = 2929255,	"SÃO GONÇALO DOS CAMPOS" = 2929305,
                  "SÃO JOSÉ DA VITÓRIA" = 2929354,	"SÃO JOSÉ DO JACUÍPE" = 2929370,	"SÃO MIGUEL DAS MATAS" = 2929404,
                  "SÃO SEBASTIÃO DO PASSÉ" = 2929503,	"SAPEAÇU" = 2929602,	"SÁTIRO DIAS" = 2929701,	"SAUBARA" = 2929750,
                  "SAÚDE" = 2929800,	"SEABRA" = 2929909,	"SEBASTIÃO LARANJEIRAS" = 2930006,	"SENHOR DO BONFIM" = 2930105,
                  "SENTO SÉ" = 2930204,	"SERRA DO RAMALHO" = 2930154,	"SERRA DOURADA" = 2930303,	"SERRA PRETA" = 2930402,
                  "SERRINHA" = 2930501,	"SERROLÂNDIA" = 2930600,	"SIMÕES FILHO" = 2930709,	"SÍTIO DO MATO" = 2930758,
                  "SÍTIO DO QUINTO" = 2930766,	"SOBRADINHO" = 2930774,	"SOUTO SOARES" = 2930808,	"TABOCAS DO BREJO VELHO" = 2930907,
                  "TANHAÇU" = 2931004,	"TANQUE NOVO" = 2931053,	"TANQUINHO" = 2931103,	"TAPEROÁ" = 2931202,	"TAPIRAMUTÁ" = 2931301,
                  "TEIXEIRA DE FREITAS" = 2931350,	"TEODORO SAMPAIO" = 2931400,	"TEOFILÂNDIA" = 2931509,	"TEOLÂNDIA" = 2931608,
                  "TERRA NOVA" = 2931707,	"TREMEDAL" = 2931806,	"TUCANO" = 2931905,	"UAUÁ" = 2932002,	"UBAÍRA" = 2932101,
                  "UBAITABA" = 2932200,	"UBATÃ" = 2932309,	"UIBAÍ" = 2932408,	"UMBURANAS" = 2932457,	"UNA" = 2932507,
                  "URANDI" = 2932606,	"URUÇUCA" = 2932705,	"UTINGA" = 2932804,	"VALENÇA" = 2932903,	"VALENTE" = 2933000,
                  "VÁRZEA DA ROÇA" = 2933059,	"VÁRZEA DO POÇO" = 2933109,	"VÁRZEA NOVA" = 2933158,	"VARZEDO" = 2933174,
                  "VERA CRUZ" = 2933208,	"VEREDA" = 2933257,	"VITÓRIA DA CONQUISTA" = 2933307,	"WAGNER" = 2933406,
                  "WANDERLEY" = 2933455,	"WENCESLAU GUIMARÃES" = 2933505,	"XIQUE-XIQUE" = 2933604)




nomes_territorio <- list("Bacia do Jacuípe",	"Bacia do Paramirim",	"Bacia do Rio Corrente",
                         "Bacia do Rio Grande",	"Baixo Sul",	"Chapada Diamantina",
                         "Costa do Descobrimento",	"Extremo Sul",	"Irecê",	"Itaparica",
                         "Litoral Norte e Agreste Baiano",	"Litoral Sul",	"Médio Rio de Contas",
                         "Médio Sudoeste da Bahia",	"Metropolitano de Salvador",	"Piemonte da Diamantina",
                         "Piemonte do Paraguaçu",	"Piemonte Norte do Itapicuru",	"Portal do Sertão",
                         "Recôncavo",	"Semiárido Nordeste II",	"Sertão do São Francisco",
                         "Sertão Produtivo",	"Sisal",	"Sudoeste Baiano",	"Vale do Jiquiriçá",
                         "Velho Chico")

#Preparando algumas listas necessarias
seqano <- as.list(2010:2060) ; names(seqano) <- 2010:2060
Anoatual <- lubridate::year(today())

# tema do pacote fresh - para mudar as cores do dash
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#003f5c",
    blue = "#726a95",
    yellow = "#f57b51",
    green = "#28df99",
    maroon = "#ff427f"
  ),
  adminlte_sidebar(
    dark_bg = "#778798",
    dark_hover_bg = "#5d6c7b	",
    dark_color = "#000000",
    dark_submenu_color = "#000000",
    dark_submenu_hover_color = "#FFFFFF"
  ),
  adminlte_global(
    content_bg = "#FFFFFF",
    box_bg = "#FFFFFF",
    info_box_bg = "#D8DEE9"
  )
)

#ui
dashboardPagePlus(skin = "blue", title="SEI - Demografia",
                  header = dashboardHeaderPlus(
                    title = tagList(
                      span(class = "logo-lg",img(src = "LogoGovBaTransp.png", width = "147.46px", height = "40px")),
                      img(src = "SEI_transparente.png",
                          width = "30px", height = "30px")
                    )
                  ),
                  sidebar = dashboardSidebar(collapsed = TRUE,
                    
                    sidebarMenu(
                      menuItem("Atual", tabName = "aba1", icon = icon("desktop")),
                      menuItem("Projeção Bahia 2010-2060", tabName = "aba3", icon = icon("chart-line")),
                      menuItem("Censo Demográfico", icon = icon("search"),
                        menuSubItem("Bahia", tabName = "aba2", icon = icon("globe-americas")),
                        menuSubItem("Municípios", tabName = "aba4", icon = icon("map-marker")),
                        menuSubItem("Territórios de Identidade", tabName = "aba5", icon = icon("map-marked-alt"))           
                        ),
                      menuItem("Registro Civil", icon = icon("th"),
                               badgeLabel = "Em breve", badgeColor = "green"),
                      menuItem("Sobre", tabName = "aba6", icon = icon("file-alt"))
                    )
                  ),   
                  dashboardBody(
                    
                    tags$head(
                      tags$link(rel="sortcut icon", href="LogoGovBa.png", type="image/png"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), includeHTML("google-analytics.html")
                      ),
                    use_theme(mytheme),
                    
                  
                    tabItems(
                      
                      #################################################################################
                      #Pagina geral - ABA 1
                      #################################################################################
                      
                      tabItem(tabName = "aba1", 
                              fluidRow(valueBoxOutput("poptotalgeral", width=4), 
                                       valueBoxOutput("pos_Bahia_Brasil", width=4),
                                       valueBoxOutput("pos_Bahia_NE", width = 4)),br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", 
                                                    footer = "Fonte: IBGE. Estimativas da População, 2020.
                                                    Nota: Os limites municipais apresentados neste pictograma foram generalizados.
                                                    Confira os limites oficiais em: https://arcg.is/1O9bmT.",
                                                    title = paste("População estimada, por município, Bahia, 2020."),
                                                    shinycssloaders::withSpinner(leafletOutput("mun_2020", height = 500), color = "#838383"))), #),
                                column(width=6, box(width=NULL,status = "primary", 
                                                    footer = "Fonte: IBGE. Estimativas da População, 2020.
                                                    Nota: Os limites territóriais apresentados neste pictograma foram generalizados.
                                                    Confira os limites oficiais em: https://arcg.is/1O9bmT.",
                                                    title = paste("População estimada, por território de identidade, Bahia, 2020."),
                                                    shinycssloaders::withSpinner(leafletOutput("ter_2020", height = 500), color = "#838383")))#, br(),
                              ) 
                              
                      ),
                      
                      
                      #################################################################################
                      #Pagina Bahia CENSO  ABA - 2
                      #################################################################################
                      
                      tabItem(tabName = "aba2",
                              fluidRow(column(
                                width=4,
                                selectInput("selectano_aba2",
                                            h4("Selecione o ano"),
                                            choices=list("1991"= 1991,"2000"= 2000,"2010"=2010), 
                                            selected=2010)
                              )), br(),
                             fluidRow(
                                column(width=6, box(width=NULL, status = "primary", 
                                                     footer = "Fonte: IBGE. Censo Demográfico.
                                                   Nota: Os limites municipais apresentados neste pictograma foram generalizados.
                                                    Confira os limites oficiais em: https://arcg.is/1O9bmT.",
                                                     title = paste("População residente, por Município."),
                                                    shinycssloaders::withSpinner(leafletOutput("mapa_ba_mun", height = 500), color = "#838383"))),
                                column(width=6, box(width=NULL, status = "primary", 
                                                    footer = "Fonte: IBGE. Censo Demográfico.
                                                   Nota: Os limites territóriais apresentados neste pictograma foram generalizados.
                                                    Confira os limites oficiais em: https://arcg.is/1O9bmT.",
                                                    title = paste("População residente, por Território de Identidade."),
                                                    shinycssloaders::withSpinner(leafletOutput("mapa_bahia",height = 500), color = "#838383")))
                              ), br(),
                             fluidRow(column(width=12,
                               box(width=NULL,status = "primary", footer = "Fonte: IBGE. População nos Censos Demográficos,\n segundo as Grandes Regiões e as Unidades da Federação - 1872/2010.", title = "População baiana de acordo com o CENSO Demográfico (1872 - 2010).",
                                   plotOutput("censo_barras"))
                             ))
                      ),
                      

                      #################################################################################
                      #Pagina Projecao - ABA 3
                      #################################################################################
                      tabItem(tabName = "aba3", 
                              sliderInput(inputId="sliderano", label=h4("Selecione o ano"), min=2010, max=2060, value=Anoatual, step = NULL, round = TRUE,
                                          ticks = TRUE, animate = TRUE,
                                          width = 1620, sep = "", pre = NULL, post = NULL, timeFormat = NULL,
                                          timezone = NULL, dragRange = TRUE),
                              fluidRow(valueBoxOutput("poptotalproj", width=3), 
                                       valueBoxOutput("tftproj", width=3),
                                       valueBoxOutput("EVN", width=3), 
                                       valueBoxOutput("MortInf", width=3)
                              ),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Projeção (2010-2060).", title = textOutput("proj_text_piramide"), 
                                       plotOutput("piramide"),
                                      # actionButton("result1","Generate Result"),
                                       # downloadButton('downloadPlot','Baixar gráfico')
                                      )),
                                column(width=6, box(width = NULL,status = "primary", footer = "Fonte: IBGE. Projeção (2010-2060).", title = textOutput("proj_text_pizza"),
                                                    plotOutput("genero_proj")))
                              ), br(),
                              fluidRow(
                                column(width=6, box(width = NULL,status = "primary", footer = "Fonte: IBGE. Projeção (2010-2060).", title = "Esperança de vida, Bahia, 2010 - 2060.",
                                                    plotOutput("EVNporSexo"))),
                                column(width=6, box(width = NULL,status = "primary", footer = "Fonte: IBGE. Projeção (2010-2060).", title = "Taxa Bruta de Natalidade (TBN) e\n Taxa Bruta de Mortalidade (TBM), Bahia, 2010 - 2060.",
                                                    plotOutput("TaxasBrutas"))),
                                ), br(),
                                fluidRow(width=12, box(width = NULL,status = "primary", footer = "Fonte: IBGE. Projeção (2010-2060).", title ="População projetada pelo IBGE, Bahia, 2010 - 2060.",
                                                       plotOutput("GrafPopTotal"))

                          )), 
                      
                      #################################################################################
                      #Pagina por municipio - aba 4
                      #################################################################################
                      tabItem(tabName = "aba4", 
                              fluidRow(
                                column(width=4,
                                       selectInput("selectano",
                                                   h4("Selecione o ano"),
                                                   choices=list("1991"= 1991,"2000"= 2000,"2010"=2010), 
                                                   selected=2010)
                                ),
                                column(width=4,
                                       selectInput("selectmuni",
                                                   h4("Selecione o Município"),
                                                   choices=nomesmuni,
                                                   selected="2927408")
                                )

                                       ),
                              fluidRow(valueBoxOutput("pop_tot_mun", width=4), 
                                       valueBoxOutput("nascidos_mortos", width=4),
                                       valueBoxOutput("urbanizacao_mun", width = 4)), br(),
                               fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("mun_piramide"), 
                                       plotOutput("piramide_censo"))), #),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("mun_sexo"),
                                           plotOutput("genero_mun")))#, br(),
                                  ), br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("mun_fxaetaria"),
                                       plotOutput("fxa_etaria"))),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("mun_raca"),
                                                    plotOutput("raca"))),
                                ), br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.",title = textOutput("mun_civil"),
                                       plotOutput("estado_civil"))),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("mun_nat"),
                                                    plotOutput("naturalidade")))
                       
                              )),
                      
                      #################################################################################
                      #
                      # Pagina Territorio de Identidade - ABA 5
                      #
                      #################################################################################

                      tabItem(tabName = "aba5", 
                              fluidRow(
                                column(width=4,
                                       selectInput("selectanov1",
                                                   h4("Selecione o ano"),
                                                   choices=list("1991"= 1991,"2000"= 2000,"2010"=2010), 
                                                   selected=2010)
                                ),
                                column(width=4,
                                       selectInput("select_ter",
                                                   h4("Selecione o Território de Identidade"),
                                                   choices=nomes_territorio,
                                                   selected="Metropolitano de Salvador")
                                )
                                
                              ), br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("ter_piramide"),
                                       plotOutput("piramide_terr"))),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.",title = textOutput("ter_sexo_t"),
                                                    plotOutput("genero_ter"))),
                                ), br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("ter_fxaetaria"),
                                       plotOutput("fxa_et_terr"))),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("ter_raca"),
                                                    plotOutput("raca_ter"))),
                                ), br(),
                              fluidRow(
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("ter_civil"),
                                       plotOutput("estado_civil_ter"))),
                                column(width=6, box(width=NULL,status = "primary", footer = "Fonte: IBGE. Censo Demográfico.", title = textOutput("ter_nat"),
                                                    plotOutput("naturalidade_terr"))),
                                ), br()
                      ),
                      
                      
                      ################################################################################
                      #Pagina de metadados
                      #################################################################################
                      tabItem(tabName = "aba6" ,
                              flipBox(
                                id = 1,
                                main_img = "SeiDataLab3.png",
                                #header_img = "",
                                front_title = "Sobre o Dashboard",
                                back_title = "Mais Informações",
                                front_btn_text = "saiba mais", back_btn_text = "voltar",
                                p("Este Dashboard foi elaborado com o software R, através das bibliotecas Shiny."),
                                p("As informações detalhas de Demografia foram elaboradas com dados provenientes do IBGE, utilizando o Censo Demográfico e a Projeção 2010 - 2060."),
                                back_content = tagList(
                                  tags$div(class="header", checked=NA,
                                  tags$b("Para mais informações acesse o site da SEI:"), tags$a(href="http://www.sei.ba.gov.br/", "http://www.sei.ba.gov.br/"))
                                )))

                    ),
                    
                    ########### rodape - inicio
                    
                    hr(),
                    fluidRow(
                      column(width=5,
                             align="center",
                             a(href="http://seplan.ba.gov.br", target="_blank",
                               img(class="align-middle", src = "Seplancol.png",width = "351px", height = "100px")
                             )
                      ),
                      column(width=4,
                             align="center",
                             a(href="http://sei.ba.gov.br", target="_blank",
                               img(class="align-middle", src = "sei.png",width = "201.33px", height = "100px")
                             )
                      ),
                      column(width=3,
                             align="center",
                             a(href="http://sei.ba.gov.br", target="_blank",
                               img(class="align-middle", src = "SeiDataLab.png",width = "225.35px", height = "100px")
                             )
                      )
                    )
                    ############# rodape - final ##################################################
                    )

)

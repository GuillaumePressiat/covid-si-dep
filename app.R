


library(dplyr, warn.conflicts = FALSE)

# setwd('Z:/3_Outils/COVID')

liste_url <- list(
  # indicateurs = list(url_web = "https://www.data.gouv.fr/fr/datasets/indicateurs-de-suivi-de-lepidemie-de-covid-19/",
  #                    url_stable = "https://www.data.gouv.fr/fr/datasets/r/4acad602-d8b1-4516-bc71-7d5574d5f33e",
  #                    url_api = "https://www.data.gouv.fr/api/1/datasets/5ee9df5003284f565d561278/",
  #                    titre = "Indicateurs de suivi de l'épidémie de COVID-19",
  #                    file_pattern = "indicateurs-covid19-dep",
  #                    delim = ",",
  #                    include = TRUE),
  # tests_positivite = list(url_web = "https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/",
  #                         url_stable = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",
  #                         url_api = "https://www.data.gouv.fr/api/1/datasets/5ed117db6c161bd5baf070be",
  #                         titre = "Données relatives aux résultats des tests virologiques COVID-19 SI-DEP",
  #                         file_pattern = "sp-pos-quot-dep",
  #                         delim = ";",
  #                         include = FALSE),
  # tests_capacites = list(url_web = "https://www.data.gouv.fr/fr/datasets/capacite-analytique-de-tests-virologiques-dans-le-cadre-de-lepidemie-covid-19/",
  #                        url_stable = "https://www.data.gouv.fr/fr/datasets/r/0c230dc3-2d51-4f17-be97-aa9938564b39",
  #                        url_api = "https://www.data.gouv.fr/api/1/datasets/5ed11705afd28672e40fbc2f/",
  #                        titre = "Capacité analytique de tests virologiques dans le cadre de l'épidémie COVID-19 SI-DEP",
  #                        file_pattern = "sp-capa-quot-dep",
  #                        delim = ";",
  #                        include = TRUE),
  incidence = list(url_web = "https://www.data.gouv.fr/fr/datasets/taux-dincidence-de-lepidemie-de-covid-19/",
                   url_stable = "https://www.data.gouv.fr/fr/datasets/r/19a91d64-3cd3-42fc-9943-d635491a4d76",
                   url_api = "https://www.data.gouv.fr/api/1/datasets/5ed1175ca00bbe1e4941a46a",
                   titre = "Taux d'incidence de l'épidémie de COVID-19 SI-DEP",
                   file_pattern = "sp-pe-tb-quot-dep", 
                   delim = ";",
                   include = TRUE)#,
  # sursaud = list(url_web = "https://www.data.gouv.fr/fr/datasets/donnees-des-urgences-hospitalieres-et-de-sos-medecins-relatives-a-lepidemie-de-covid-19/",
  #                url_stable = "https://www.data.gouv.fr/fr/datasets/r/eceb9fb4-3ebc-4da3-828d-f5939712600a",
  #                url_api = "https://www.data.gouv.fr/api/1/datasets/5e74ecf52eb7514f2d3b8845",
  #                titre = "Données des urgences hospitalières et de SOS médecins relatives à l'épidémie de COVID-19",
  #                file_pattern = "sursaud-corona-quot-dep",
  #                delim = ";",
  #                include = TRUE)
)


type_fichier <- 'incidence'

get_data <- function(type_fichier, liste_url) {
  cat(type_fichier, "\n")
  url_api <- liste_url[type_fichier] %>% purrr::map('url_api') %>% .[[1]]
  url_stable <- liste_url[type_fichier] %>% purrr::map('url_stable') %>% .[[1]]
  file_pattern <- liste_url[type_fichier] %>% purrr::map('file_pattern') %>% .[[1]]
  
  file_delim <- liste_url[type_fichier] %>% purrr::map('delim') %>% .[[1]]
  u <- httr::GET(url_api, config = httr::config(verbose = FALSE))
  url_search <- httr::content(u)$resources
  
  df_date <- tibble(url = url_search %>% purrr::map_chr('url'),
                    timestamp = url_search %>% purrr::map_chr('last_modified')) %>% 
    filter(grepl(file_pattern, url)) %>% 
    arrange(desc(timestamp)) %>% 
    pull(timestamp) %>% 
    .[1] %>% 
    lubridate::as_datetime() %>% 
    format(., '%Y-%m-%d--%Hh%Mm')
  
  url_file <- tibble(url = url_search %>% purrr::map_chr('url'),
                     timestamp = url_search %>% purrr::map_chr('last_modified')) %>% 
    filter(grepl(file_pattern, url)) %>% 
    arrange(desc(timestamp)) %>% 
    pull(url) %>% 
    .[1]
  
  file_name <- 'data_temp.csv'
  
  # file_name <- basename(url_file) %>% 
  #   stringr::str_replace_all("\\.csv", paste0('__',df_date, ".csv"))
  # 
  f <- httr::GET(url_stable, httr::write_disk(paste0("files/", file_name), overwrite = TRUE))
  
  enc <- readr::guess_encoding(paste0("files/", file_name)) %>% pull(encoding) %>% .[1]
  
  donnee <- readr::read_delim(paste0("files/", file_name), delim = file_delim, 
                              locale = readr::locale(encoding = enc))
  
  names(donnee)[names(donnee) == "departement"] <- "dep"
  names(donnee)[names(donnee) == "extract_date"] <- "jour"
  names(donnee)[names(donnee) == "date_de_passage"] <- "jour"
  
  donnee <- donnee %>% 
    mutate(semaine = paste0(lubridate::year(lubridate::floor_date(jour, unit = 'week', week_start = 1)), ' S', stringr::str_pad(lubridate::isoweek(jour), 2, 'left', 0)),           jour_sem = lubridate::wday(jour, week_start = 1, abbr = FALSE, label = TRUE)) %>% 
    select(semaine, jour, jour_sem, everything())
  
  donnee <- donnee %>% arrange(desc(jour))
  
  # donnee <- bind_rows(donnee %>% 
  #                       filter(dep %in% c('29')),
  #                     donnee %>% 
  #                       filter(dep %in% c('22', '35', '56')))
  
  if (type_fichier == 'incidence'){
    donnee <- donnee %>% 
      mutate(incidence_1e5 = P * 1e5 / pop)
    donnee <- donnee %>% arrange(desc(jour), cl_age90)
  }
  
  if (type_fichier == 'sursaud'){
    donnee <- donnee %>% filter(sursaud_cl_age_corona == "0")
  }
  retour <- liste_url[type_fichier]
  retour <- modifyList(retour, setNames(list(list(df_date = df_date, 
                                                  file_link = paste0("SI-DEP/downloads/", file_name), 
                                                  file_name = file_name,
                                                  donnee = donnee)), type_fichier))
  
  
  return(retour)
}

data_and_meta <- unique(names(liste_url)) %>% 
  purrr::map(get_data, liste_url)

lib_dep <- readr::read_csv('cog/departement2019.csv')
lib_reg <- readr::read_csv('cog/region2019.csv')

to_plot_clage <- data_and_meta %>% purrr::map('incidence') %>% 
  purrr::compact() %>% 
  .[[1]] %>% 
  .$donnee %>% 
  filter(cl_age90 != '0') %>% 
  group_by(semaine, jour, dep, cl_age90, pop) %>% 
  summarise(P = sum(P)) %>% 
  mutate(inc = P * 1e5 / pop) %>% 
  ungroup() %>% 
  left_join(lib_dep %>% rename(libelle_dep = libelle)) %>% 
  left_join(lib_reg %>% select(reg, libelle_reg = libelle)) %>% 
  filter(!is.na(libelle_dep)) %>% 
  mutate(dep = paste0(dep, " - ", libelle_dep),
         reg = paste0(reg, " - ", libelle_reg))

to_plot <- data_and_meta %>% purrr::map('incidence') %>% 
  purrr::compact() %>% 
  .[[1]] %>% 
  .$donnee %>% 
  filter(cl_age90 == '0') %>% 
  group_by(semaine, dep, pop) %>% 
  summarise(P = sum(P),
            nb_jour = n()) %>% 
  mutate(inc = P * 1e5 / pop) %>% 
  ungroup() %>% 
  mutate(semaine = ifelse(nb_jour < 7, paste0(semaine, "*\n", nb_jour, "j"), semaine)) %>% 
  left_join(lib_dep %>% rename(libelle_dep = libelle)) %>% 
  left_join(lib_reg %>% select(reg, libelle_reg = libelle)) %>% 
  filter(!is.na(libelle_dep)) %>% 
  mutate(dep = paste0(dep, " - ", libelle_dep),
         reg = paste0(reg, " - ", libelle_reg))

to_plot_jour <- data_and_meta %>% purrr::map('incidence') %>% 
  purrr::compact() %>% 
  .[[1]] %>% 
  .$donnee %>% 
  filter(cl_age90 == '0') %>% 
  group_by(jour, dep, pop) %>% 
  summarise(P = sum(P),
            nb_jour = n()) %>% 
  mutate(inc = P * 1e5 / pop) %>% 
  ungroup() %>% 
  left_join(lib_dep %>% rename(libelle_dep = libelle)) %>% 
  left_join(lib_reg %>% select(reg, libelle_reg = libelle)) %>% 
  filter(!is.na(libelle_dep)) %>% 
  mutate(dep = paste0(dep, " - ", libelle_dep),
         reg = paste0(reg, " - ", libelle_reg))

dataa <- data_and_meta %>% 
  purrr::map('incidence') %>% 
  purrr::compact() %>% 
  .[[1]] %>% 
  .$donnee %>% as.data.frame

dataa1 <- dataa %>% 
  left_join(lib_dep %>% rename(libelle_dep = libelle)) %>% 
  left_join(lib_reg %>% select(reg, libelle_reg = libelle)) %>% 
  mutate(dep = paste0(dep, " - ", libelle_dep),
         reg = paste0(reg, " - ", libelle_reg)) %>% 
  select(-contains('ncc'), - contains('chefl'), - contains('libelle')) %>% 
  mutate(`Classe d'âge` = case_when(cl_age90 == "09" ~ "0 - 9 ans",
                                    cl_age90 == "19" ~ "10 - 19 ans",
                                    cl_age90 == "29" ~ "20 - 29 ans",
                                    cl_age90 == "39" ~ "30 - 39 ans",
                                    cl_age90 == "49" ~ "40 - 49 ans",
                                    cl_age90 == "59" ~ "50 - 59 ans",
                                    cl_age90 == "69" ~ "60 - 69 ans",
                                    cl_age90 == "79" ~ "70 - 79 ans",
                                    cl_age90 == "89" ~ "80 - 89 ans",
                                    cl_age90 == "90" ~ "90 ans et +",
                                    TRUE ~ "Toutes classes d'âge")) %>% 
  mutate(incidence_1e5 = round(incidence_1e5,3))

library(ggplot2)
to_plot_clage <- to_plot_clage %>% 
  mutate(dep = forcats::fct_rev(dep)) %>% 
  mutate(`Classe d'âge` = case_when(cl_age90 == "09" ~ "0 - 9 ans",
                              cl_age90 == "19" ~ "10 - 19 ans",
                              cl_age90 == "29" ~ "20 - 29 ans",
                              cl_age90 == "39" ~ "30 - 39 ans",
                              cl_age90 == "49" ~ "40 - 49 ans",
                              cl_age90 == "59" ~ "50 - 59 ans",
                              cl_age90 == "69" ~ "60 - 69 ans",
                              cl_age90 == "79" ~ "70 - 79 ans",
                              cl_age90 == "89" ~ "80 - 89 ans",
                              cl_age90 == "90" ~ "90 ans et +"))


library(echarts4r)
# to_plot %>% 
#   mutate(inc = round(inc,2)) %>% 
#   mutate(inc = ifelse(inc == 0, NA, inc)) %>% 
#   e_charts(semaine) %>% 
#   e_heatmap(dep, inc) %>% 
#   e_visual_map(inc, left = "90%", top = "8%") %>% 
#   e_toolbox_feature(feature = "dataZoom") %>% 
#   e_toolbox_feature(feature = "saveAsImage") %>% 
#   e_toolbox_feature(feature = "restore") %>% 
#   e_y_axis(inverse = TRUE) %>% 
#   e_tooltip(formatter = htmlwidgets::JS("
#                                     function(params){
#                                     return(params.value[1] + ' / ' + params.value[0] + ' : ' + 
#                                     params.value[2])
#                                     }
#                                     "))

library("shiny")
library(shinyWidgets)

library(shinydashboard)

liste_region <- unique(to_plot$reg)
liste_departement <- unique(to_plot$dep)

library(shinyjs)

ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "COVID via SI-DEP"),
                    dashboardSidebar(
                      shinyjs::useShinyjs(),
                      switchInput(label = "<i class='far fa-calendar'></i>", 'donnee', onLabel = 'Sem.', offLabel = 'Jour', TRUE, size = 'mini'),
                      switchInput(label = "<i class='far fa-info-circle'></i>", 'incide', onLabel = 'Effectifs', offLabel = 'Incidence', FALSE, size = 'mini'),
                      dateInput(
                        inputId = "date_1", 
                        value = 
                          lubridate::floor_date(
                            # min(to_plot_jour$jour), week_start = 1, unit = "weeks"),
                            as.Date('2020-06-01'), week_start = 1, unit = "weeks"),
                            daysofweekdisabled = c(0, 2:6),
                        weekstart = 1,
                        startview = "year",
                        label = "Semaine de début",
                        format = "D dd/mm/yyyy",
                        min = lubridate::floor_date(
                          min(to_plot_jour$jour), week_start = 1, unit = "weeks"),
                        language = "fr",
                        max = Sys.Date()
                      ),
                      pickerInput(
                        inputId = "lregs",
                        label = "Régions",
                        choices = sort(unique(to_plot$reg)),
                        options = list(
                          `actions-box` = TRUE), 
                        multiple = TRUE,
                        selected = sort(unique(to_plot$reg))
                      ),
                      pickerInput(
                        inputId = "ldeps",
                        label = "Départements",
                        choices = sort(unique(to_plot$dep)),
                        options = list(
                          `live-search` = TRUE,
                          # `mobile` = TRUE,
                          `actions-box` = TRUE), 
                        multiple = TRUE,
                        selected = unique(to_plot$dep)
                      ),
                      sidebarMenu(id = "tabsa", 
                                  
                        menuItem("Heatmap", tabName = "heatmap", icon = icon("location-arrow")),
                        menuItem("Classes d'âge", icon = icon("eye"), tabName = "clages"),
                        menuItem("Calendrier", icon = icon("calendar"), tabName = "calendar",
                                 badgeLabel = "new", badgeColor = "green"),
                        menuItem("Données brutes", icon = icon("th"), tabName = "databr")
                      ),
                      br(),
                      HTML('&nbsp;'), tags$em('   Dernière mise à jour le : ', br(),HTML('&nbsp;'), data_and_meta[[1]]$incidence$df_date),
                      br(),
                      tags$b(tags$a(href = 'https://www.data.gouv.fr/fr/datasets/taux-dincidence-de-lepidemie-de-covid-19/', 'Source : Données SI-DEP Santé publique France'))),
                    dashboardBody(
                      
                      tags$em(textOutput('value')),
                      tabItems(
                        tabItem(tabName = "heatmap",
                                
                                  h2(textOutput('title')), 
                                  echarts4r::echarts4rOutput("plot",  height = "650px")), #width = "100%",
                        
                        tabItem(tabName = "clages",
                                
                                  h2(textOutput("title_clage")), 
                                echarts4r::echarts4rOutput("plot_clage", width = "100%", height = "650px")),
                        tabItem(tabName = "calendar",
                                
                                h2(textOutput("title_cal")), 
                                echarts4r::echarts4rOutput("plot_calendar", height = "650px")),
                        
                        tabItem(tabName = "databr",
                                
                                  h2('Données sources brutes'), 
                                  DT::dataTableOutput('data'))))
                    
)

server <- function(input, output, session) {
  
  semaine_pivot <- reactive({
    paste0(
    lubridate::year(lubridate::floor_date(input$date_1, week_start = 1, unit = "weeks")),
    ' S', stringr::str_pad(lubridate::isoweek(lubridate::floor_date(input$date_1, week_start = 1, unit = "weeks")), 2,"left","0")
    )
  })
  output$value <- renderText({
    if (input$tabsa != "calendar"){
    if (input$donnee){
      "Les données présentées sont hebdomadaires (semaines iso), attention certaines semaines ne sont pas complètes voir abscisse"
    } else {
      "Les données présentées sont journalières"
    }
    } else {' '}
  })
  

  
  output$title <- renderText({
    if (input$incide){
      "Effectifs positifs"
    } else {
      "Incidence pour 100 000 habitants"
    }
  })
  output$title_cal <- renderText({
    if (input$incide){
      "Effectifs positifs"
    } else {
      "Incidence pour 100 000 habitants"
    }
  })
    
  output$title_clage <- renderText({
    if (input$incide){
      "Effectifs positifs par classe d'âge"
    } else {
      "Incidence pour 100 000 habitants par classe d'âge"
    }
  })
  
  plotting <- reactive({
    req(input$ldeps)
    
    if (input$donnee){
      temp <- to_plot %>% 
        filter(reg %in% input$lregs, dep %in% input$ldeps, semaine >= semaine_pivot()) %>% 
        rename(time_ = semaine)
    } else {
      temp <- to_plot_jour %>% 
        filter(reg %in% input$lregs, dep %in% input$ldeps, jour >= input$date_1) %>% 
        rename(time_ = jour)
    }
    
    if (input$incide){
      temp %>% rename(mesure = P)
    } else {
      temp %>% rename(mesure = inc)
    }
  })
  
  plotting_clage <- reactive({
    req(input$ldeps)
    
    
    if (input$donnee){
      temp <- to_plot_clage %>% 
        filter(reg %in% input$lregs, dep %in% input$ldeps, semaine >= semaine_pivot()) %>% 
        group_by(semaine, `Classe d'âge`, pop) %>% 
        summarise(nb_jour = n_distinct(jour),
                  P = sum(P)) %>% 
        ungroup() %>% 
        group_by(semaine, `Classe d'âge`, nb_jour) %>% 
        summarise(P = sum(P),
                  pop = sum(pop)) %>% 
        ungroup() %>% 
        mutate(inc = round(P * 1e5 / pop,3)) %>% 
        mutate(semaine = ifelse(nb_jour < 7, paste0(semaine, "*\n", nb_jour, "j"), semaine)) %>% 
        rename(time_ = semaine)
    } else {
      temp <- to_plot_clage %>% 
        filter(reg %in% input$lregs, dep %in% input$ldeps, jour >= input$date_1) %>% 
        group_by(jour, `Classe d'âge`) %>% 
        summarise(pop = sum(pop),
                  P = sum(P)) %>% 
        mutate(inc = round(P *1e5/pop,3)) %>% 
        ungroup() %>% 
        rename(time_ = jour)
    }
    if (input$incide){
      temp %>% rename(mesure = P)
    } else {
      temp %>% rename(mesure = inc)
    }
  })
  
    plotting_calendar <- reactive({
      req(input$ldeps)
      
        temp <- to_plot_jour %>% 
          filter(reg %in% input$lregs, dep %in% input$ldeps, jour >= input$date_1) %>% 
          group_by(jour) %>% 
          summarise(P = sum(P), 
                    pop = sum(pop)) %>% 
          mutate(inc = round(P *1e5/pop,2))
        
        pivot_time <- tibble(jour = seq.Date(from = as.Date('2020-01-01'), 
                                             to = as.Date('2022-12-31'), by = 1))
        
        temp <- temp %>% right_join(pivot_time, by = "jour") %>% 
          tidyr::replace_na(list(inc = 0, P = 0)) %>% 
          arrange(jour)
        
    if (input$incide){
      temp %>% rename(mesure = P)
    } else {
      temp %>% rename(mesure = inc)
    }
  })
  
  cdata <- session$clientData

  output$plot <- echarts4r::renderEcharts4r(
    plotting() %>% 
      mutate(mesure = round(mesure, 1)) %>% 
      # mutate(mesure = ifelse(mesure == 0, NA, mesure)) %>% 
      e_charts(time_) %>% 
      e_heatmap(dep, mesure) %>% 
      e_visual_map(mesure, left = "90%", top = "8%") %>% 
      e_toolbox_feature(feature = "dataZoom") %>% 
      e_toolbox_feature(feature = "saveAsImage") %>% 
      e_toolbox_feature(feature = "restore") %>% 
      e_y_axis(inverse = TRUE) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                    function(params){
                                    return(params.value[1] + ' / ' + params.value[0] + ' : ' + 
                                    Number(params.value[2]).toLocaleString('fr-FR'))
                                    }
                                    "))
  )
  
  output$plot_clage <- echarts4r::renderEcharts4r(
    plotting_clage() %>% 
      mutate(mesure = round(mesure, 1)) %>% 
      # mutate(mesure = ifelse(mesure == 0, NA, mesure)) %>% 
      e_charts(time_) %>% 
      e_heatmap(`Classe d'âge`, mesure) %>% 
      e_visual_map(mesure, left = "90%", top = "8%") %>% 
      e_toolbox_feature(feature = "dataZoom") %>% 
      e_toolbox_feature(feature = "saveAsImage") %>% 
      e_toolbox_feature(feature = "restore") %>% 
      # e_y_axis(inverse = TRUE) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                    function(params){
                                    return(params.value[1] + ' / ' + params.value[0] + ' : ' + 
                                    Number(params.value[2]).toLocaleString('fr-FR'))
                                    }
                                    "))
)
  
    output$plot_calendar <- echarts4r::renderEcharts4r(

      
    plotting_calendar() %>% 
      mutate(mesure = round(mesure, 1)) %>% 
      # mutate(mesure = ifelse(mesure == 0, NA, mesure)) %>% 
      mutate(mesure = ifelse(mesure == 0, NA, mesure)) %>% 
      mutate(year = format(jour, "%Y")) %>% 
      group_by(year) %>% 
      e_charts(jour) %>% 
      e_calendar(range = "2020",
                 top="20", 
                 left = 60, 
                 dayLabel = list(
                   firstDay=1, 
                   nameMap = c('Di', 'Lu', 'Ma', 'Me', 'Je', 'Ve', 'Sa')),
                 monthLabel = list(
                   nameMap = lubridate::month(1:12, label = TRUE, abbr = FALSE))) %>%
      e_calendar(range = "2021",
                 top="170", 
                 left = 60, 
                 dayLabel = list(
                   firstDay=1, 
                   nameMap = c('Di', 'Lu', 'Ma', 'Me', 'Je', 'Ve', 'Sa')),
                 monthLabel = "") %>% 
                 # list(
                 #   nameMap = lubridate::month(1:12, label = TRUE, abbr = FALSE))) %>%
      e_calendar(range = "2022",
                 top="320", 
                 left = 60, 
                 dayLabel = list(
                   firstDay=1, 
                   nameMap = c('Di', 'Lu', 'Ma', 'Me', 'Je', 'Ve', 'Sa')),
                 monthLabel = "") %>% 
      # list(
      #   nameMap = lubridate::month(1:12, label = TRUE, abbr = FALSE))) %>%
      e_heatmap(mesure, coord_system = "calendar") %>% 
      e_visual_map(min = 1L, max = max(plotting_calendar()$mesure), top = "500px", left = "5px") %>% 
      e_toolbox_feature(feature = "saveAsImage") %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                    function(params){
                                    return(params.value[0] + ': ' + Number(params.value[1]).toLocaleString('fr-FR'))
                                    }
                                    "))
    )
  
  output$data <- DT::renderDataTable(dataa1 %>% 
                                       filter(cl_age90 == '0', reg %in% input$lregs, dep %in% input$ldeps, jour >= input$date_1) %>% 
                                       select(-cl_age90, - `Classe d'âge`),
                                     filter = 'top',
                                     extensions = c('Scroller', 'Buttons'),
                                     options = list(
                                       deferRender = FALSE,
                                       scrollY = 550,
                                       scroller = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c('excel' , 'copy', 'csv', 'colvis')), server = FALSE, rownames = F, escape = FALSE)
  
  observeEvent(input$lregs,{
    choice_tab <-  reactive({to_plot %>% 
        filter(reg %in% input$lregs)})
    
    updatePickerInput(session, 'ldeps', 
                      choices = unique(choice_tab()$dep), 
                      selected = unique(choice_tab()$dep))})
  

  
  observeEvent(TRUE, {
    updateTabsetPanel(session, "tabsa",
                      selected = "calendar"
    )
  })

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['reg']])) {
      updatePickerInput(session, "lregs", selected = liste_region[grepl(query[['reg']], liste_region)])
    }
    if (!is.null(query[['dep']])) {
      updatePickerInput(session, "ldeps", selected = liste_departement[grepl(query[['dep']], liste_departement)])
    }
    if (input$tabsa == 'calendar'){
      hide('donnee')
    } else {
      show('donnee')
    }
  })
  
  
}

shinyApp(ui, server)


---
title: "Informe mensual de indicadores"
format: 
  html:
  fig-width: 20
fig-height: 25

editor: source
embed-resources: true
page-layout: full
date: "`r Sys.Date()`"
---


  
  
  
  ## PASO CERO ----

library(tidyverse, quietly = T)
library(janitor, quietly = T)
library(rio, quietly = T)
library(lubridate, quietly = T)
library(DT, quietly = T)
library(scales, quietly = T)
library(devtools, quietly = T)
library(patchwork, quietly = T)
library(devtools, quietly = T)
library(patchwork, quietly = T)
library(tidytext, quietly = T)
library(stringi, quietly = T)



irakurri <- function(x) {
    
  if(str_detect(x, "csv")){
    irakurri_doc <- read_csv(x, show_col_types = F)
  } else{
    irakurri_doc <- import(x) %>% 
      as_tibble()
  }
   
  irakurri_doc %>% 
    clean_names() %>% 
    mutate(
      across(starts_with("fecha"), as.Date),
      across(where(is.character), ~ stringi::stri_trans_general(., "Latin-ASCII") %>% 
               str_to_lower())
    )
}


# EDA ----

skimr::skim(mtcars)

summarytools::dfSummary(data_table_final) %>% 
  summarytools::stview()

gtExtras::gt_plt_summary(mtcars)

crudo %>% DataExplorer::create_report()





# -------------------------------------------

datatable(rownames = FALSE,
          filter = c("top"),
          class = 'cell-border stripe',
          extensions = c("Buttons"),
          options = list(
            #columnDefs = list(list(targets = 7, visible = FALSE)),
            pageLength = 10, 
            dom = 'Brtip',
            buttons = c('colvis', "csv", "print")
          )
) %>% 
  formatStyle(
    "por",           # OBJETIVOS
    "por2",          # CASILLA REFERENCIA
    backgroundColor = styleInterval(
      c(25,50),
      c('palegreen', "yellow", "orange"))) %>% 
  formatStyle(c("rp", "p_value"), #OBJETIVOS
              "p_value",          # CASILLA REFERENCIA
              backgroundColor = styleEqual("<0.05", "lightgreen"))







# Calculo AÑOMES ----
ano_mes_alta = substr(str_remove(fecha_alta, "-"), 1,6)


scale_y_continuous(lim = c(0,30),
                   labels = function(x) str_c(x, '%')) +
  theme(
    text = element_text(family = "TradeGothic")
  )

library(extrafont)
  
# para cambiar eñes
mutate(centro_episodio = str_replace_all(centro_episodio, "\uFFFD", "n")


## Servicios médicos 
::: {.panel-tabset}
:::

  

La primera columna de las gráficas corresponde a `r inicio2` del año pasado y la última a `r fin2` de este año. 
<br>

  
  
  
  
  
  
  
  
  
  
# tabla internet

datatable(iris) %>% 
  formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
    'Sepal.Width',
    color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
  ) %>%
  formatStyle(
    'Petal.Length',
    background = styleColorBar(iris$Petal.Length, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(
      unique(iris$Species), c('lightblue', 'lightgreen', 'lightpink')
    )
  )






# poisson robusto ----


model <-  glm(triage ~ ., family = poisson(link= log), data = df_modelo2, na.action = na.omit)

sandwich_poisson <- function(x) {
  
  coeftest(x, vcov = sandwich) %>% 
    .[,] %>% 
    as.data.frame() %>% 
    clean_names() %>% 
    mutate(
      name = rownames(.),
      rp = exp(estimate),
      cibajo = exp(estimate + qnorm(0.05/2) * std_error),
      cialto = exp(estimate + qnorm(1- 0.05/2) * std_error),
      p_value = if_else(pr_z < 0.05, "<0.05", "no"),
      across(where(is.numeric), ~ round(., 4))
    ) %>%
    as_tibble() %>% 
    filter(name != "(Intercept)") %>% 
    relocate(name, .before = estimate) %>% 
    print()
  
}


geom_poisson <- function(x) {
  
  x %>% 
    ggplot(aes(rp, name)) +
    geom_vline(xintercept = 1, color = "orange" ) +
    geom_pointrange(aes(xmax = cialto, xmin = cibajo, color = p_value), size = 1) +
    scale_color_manual(
      values = c(
        "<0.05" = "blue",
        "no" = "grey30"
      )
    ) +
    theme_bw()
}


dt_poisson <- function(x) {
  x %>% 
    datatable(rownames = FALSE,
              class = 'cell-border stripe',
              options = list(
                #columnDefs = list(list(targets = 7, visible = FALSE)),
                pageLength = 10, 
                dom = 'B'
              )
    ) %>% 
    formatStyle(c("rp", "p_value"), #OBJETIVOS a colorear
                "p_value",
                backgroundColor = styleEqual("<0.05", "lightgreen"))
}










































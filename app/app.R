library(shiny); library(plotly); library(readr); library(mirtCAT); library(rsconnect);
library(emayili); library(htmltools); library(reticulate); library(glue); library(dplyr);
library(gridExtra); library(grid); library(shinyjs)

### ----------------- Não Alterar ------------------------------------- ###

#reticulate::use_python("C:/ProgramData/miniconda3/envs/r-reticulate")
#reticulate::use_python("C:/Users/joao/miniconda3/envs/r-reticulate")

#reticulate::use_condaenv("r-reticulate")
#reticulate::import("kaleido")

py_install(c('kaleido', 'plotly'))
reticulate::import('kaleido')

### -------------------------------------------------------------------- ###


pars <- read_rds("params_by_age.rds")

cores <- c("green", "yellow", "orange", "darkorange", "red")

limiares <- c(40, 47, 51, 52, 55, 57, 58, 60, 61, 62, 63, 65, 66, 67, 68, 69, 70, 71, 73, 74, 75, 77, 78, 79, 81, 82, 84, 87)

cores <- c("green", "yellow", "orange", "darkorange", "red")

perguntas <- c(
  "Pouco interesse ou pouco prazer em fazer as coisas",
  "Se sentir “para baixo”, deprimido/a ou sem perspectiva",
  "Dificuldade para pegar no sono ou permanecer dormindo, ou dormir mais do que de costume",
  "Se sentir cansado/a ou com pouca energia",
  "Falta de apetite ou comendo demais",
  "Se sentir mal consigo mesmo/a — ou achar que você é um fracasso ou que 
  decepcionou sua família ou você mesmo/a",
  "Dificuldade para se concentrar nas coisas, como ler o jornal ou ver televisão",
  "Lentidão para se movimentar ou falar, a ponto das outras pessoas perceberem? 
  Ou o oposto – estar tão agitado/a ou irrequieto/a que você fica andando de um lado para o outro muito mais do que de costume",
  "Pensar em se ferir de alguma maneira ou que seria melhor estar morto/a"
)

#smtp <- server(
#  host = "smtp.gmail.com",
#  port = 465,
#  username = Sys.getenv("GMAIL_USERNAME"),
#  password = Sys.getenv("GMAIL_PASSWORD")
#)

smtp <- server(
  host = "smtp.gmail.com",
  port = 465,
  username = "mhegscoreapp@gmail.com",
  password = "kidi gowk ujvv arkl"
)

item <- function(id, label) {
  radioButtons(id, label, 
               choices = list("Nenhuma vez" = 0,
                              "Vários dias" = 1,
                              "Mais da metade dos dias" = 2,
                              "Quase todos os dias" = 3),
               selected = 0)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  fluidRow(
    div(id = "header-logos",
      column(4,tags$img(src = 'img/cism.svg', id = "cism-logo")),
      column(4,tags$img(src = 'img/ppg.svg', id = "ppg-logo")),
      column(4,tags$img(src = 'img/mh.svg', id = "mh-logo")),
    )
  ),
  
  tags$h2("PHQ-9"),
  tags$h3("Calculadora de níveis de depressão baseado nas normativas Brasileiras de 2019"),

  sidebarLayout(
    sidebarPanel(width = 6,
                 fluidRow(
                   column(6,
                          textInput("sender",
                                    label = "Digite o seu nome completo:",
                                    value = "",
                                    placeholder = "Nome"),
                          textInput("dest",
                                    label = "Insira o e-mail do seu profissional de saúde mental",
                                    value = "",
                                    placeholder = "E-mail do profissional")
                   ),
                   column(6,
                          textInput("age",
                                    label = "Digite sua idade:",
                                    value = "",
                                    placeholder = "Idade (somente número)"),
                          radioButtons("sexo", 
                                       label = "Qual o seu sexo?", 
                                       choices = list("Feminino" = 0,
                                                      "Masculino" = 1),
                                       selected = 0)
                   )
                 ),
                 fluidRow(
                   column(6,
                          markdown("Durante as últimas duas semanas, com que freqüência você foi incomodado/a por qualquer um dos problemas abaixo? \n"),
                          item("phq1", label = "1. Pouco interesse ou pouco prazer em fazer as coisas"),
                          item("phq2", label = "2. Se sentir “para baixo”, deprimido/a ou sem perspectiva"),
                          item("phq3", label = "3. Dificuldade para pegar no sono ou permanecer dormindo, ou dormir mais do que de costume"),
                          item("phq4", label = "4. Se sentir cansado/a ou com pouca energia"),
                          item("phq5", label = "5. Falta de apetite ou comendo demais"),
                   ),
                   column(6,
                          item("phq6", label = "6. Se sentir mal consigo mesmo/a — ou achar que você é um fracasso ou que decepcionou sua família ou você mesmo/a"),
                          item("phq7", label = "7. Dificuldade para se concentrar nas coisas, como ler o jornal ou ver televisão"),
                          item("phq8", label = "8. Lentidão para se movimentar ou falar, a ponto das outras pessoas perceberem? Ou o oposto – estar tão agitado/a ou irrequieto/a que você fica andando de um lado para o outro muito mais do que de costume"),
                          item("phq9", label = "9. Pensar em se ferir de alguma maneira ou que seria melhor estar morto/a"),
                          actionButton("calcular", "Calcular θ"),
                          
                          
                   ),
                 ),
                 fluidRow(
                   column(6,
                          actionButton("sendemail", "Enviar e-mail!", icon = icon("envelope")),
                          downloadButton("download", "Download", icon = icon("download")),
                          offset = 6
                   )
                 )
    ),
    mainPanel(width = 6,
              fluidRow(
                column(12, plotlyOutput("bargraph", height = "600px", width = "100%"))
              ),
              fluidRow(
                column(12, textOutput("resultado"))
              ),
              fluidRow(
                column(12,
                       uiOutput("init_text_r")
                )
              ),
              fluidRow(
                column(12,
                       uiOutput("dep_text_r")
                )
              ),
              fluidRow(
                column(12,
                       uiOutput("fin_text_r")
                )
              )
    )
  ),
  HTML("
       <footer>
          <a class='footer-link' href='https://mheg-ufsm.github.io/website/'>© Mental Health Epidemiology Group</a>
          <p>
            Auxiliaram na construção do App: 
            <a class='footer-link' href='https://www.linkedin.com/in/andresimi/?originalSubdomain=br'>André Simioni</a> 
            e
            <a class='footer-link' href='https://www.linkedin.com/in/igor-duarte-54478ba3/?originalSubdomain=br'>Igor Duarte</a>
          </p>
       </footer>
  "),
)

server <- function(input, output) {
  
  shinyjs::disable("download")
  shinyjs::disable("sendemail")

  valores <- reactiveValues(escoreT = NULL)
  
  observeEvent(input$calcular, {
    respostas <- sapply(1:9, function(i) {
      as.numeric(input[[paste0("phq", i)]])
    })
    
    theta <- try(
      {
        
        breaks <- c(seq(15,80, 5),Inf)
        
        pars_age <- 15 - length(breaks[input$age < breaks])
        
        if(input$sexo == 0) {
          
          age_seq <- seq(1,28,2)
          
          pars_age <- age_seq[pars_age]
        }
        
        age_seq <- seq(2,28,2)
        
        pars_age <- age_seq[pars_age]
        
        # Create generic model with desired params
        mod_generic <- generate.mirt_object(pars[[pars_age]], itemtype = "graded")
        
        # Get new theta in t score
        fscores(mod_generic, response.pattern = respostas)[1]
        
      }, silent = TRUE
    )
    
    
    if(inherits(theta, "try-error")) {
      theta <- NA
    }
    
    if(!is.na(theta)) {
      valores$escoreT <- 50 + 10 * theta
    }
    
    shinyjs::enable("download")
    
    shinyjs::enable("sendemail")
  })
  
  observeEvent(input$sendemail, {
    
    email <- envelope(
      to = input$dest,
      from = "mhegscoreapp@gmail.com",
      subject = "Avaliação de sintomas depressivos",
      text = paste("O escore de ",input$sender, "é: ", round(valores$escoreT)),
      html = '<html><body><img src="cid:plot"></body></html>'
    ) |> attachment(path = "plot.png", cid = "plot")
    
    smtp(email, verbose = TRUE)
    
    py_run_file("delete_emails.py")
    
  })
  
  observeEvent(input$calcular, {

    save_image(p = thePlot(), file = 'plot.png', width = 700, height = 700)

  })
  
  output$download <- downloadHandler(

    filename = "report.pdf",

    content = function(file) {

      tempReport <- file.path(tempdir(), "report.Rmd")

      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      pl <- thePlot()
      
      texto <- glue(init_text(), dep_text(), fin_text())
      
      params <- list(escore = valores$escoreT, nome = input$sender, plot = pl, texto = texto)
      
      rmarkdown::render(tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  # output$download <- downloadHandler(
  #   
  #   filename = "plot.png",
  #   
  #   content = function(file) {
  # 
  #     plot <- save_image(p = thePlot(), file = 'plot.png', width = 700, height = 700)
  #     
  #     file.copy(plot, file)
  #   }
  # )

    
  output$resultado <- renderText({
    paste("O valor do escore T é: ", formatC(valores$escoreT, format = "f", digits = 2))
  })
  
  thePlot <- reactive({
    if (!is.numeric(valores$escoreT) || is.null(valores$escoreT)) {
      valores$escoreT <- ""
    }
    cor <- ifelse(valores$escoreT <= 49, "green", 
                  ifelse(valores$escoreT <= 59.0, "yellow", 
                         ifelse(valores$escoreT <= 69.0, "orange", 
                                ifelse(valores$escoreT <= 69.0, "darkorange", "red"))))
    
    
    posicoes_x <- c(-0.6, -0.6, -0.6, -0.6)
    
    annotations <- data.frame(
      x = posicoes_x,
      y = c(37.0, 47, 57, 73),
      text = c("Sem sintomas", "Poucos sintomas", "Sintomas moderados", "Sintomas graves"),
      color = c("green", "#F2E744", "#F2AA52", "#F23E2E")
    )
    
    p <- plot_ly() %>%
      add_bars(
        x = ~c(0), 
        y = ~c(valores$escoreT),
        width = 0.6,
        name = "Escore T",
        marker = list(color = cor)
      )
    
    
    for(i in 1:nrow(annotations)) {
      p <- p %>%
        add_annotations(
          x = annotations$x[i],
          y = annotations$y[i],
          text = annotations$text[i],
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "middle",
          font = list(color = annotations$color[i], family = "Arial bold")
        )
    }
    
    
    p <- p %>%
      layout(
        shapes = list(
          list(type = "line", x0 = -1, x1 = 1, y0 = 40.0, y1 = 40.0, line = list(dash = "dash", color = "green")),
          list(type = "line", x0 = -1, x1 = 1, y0 = 50.0, y1 = 50.0, line = list(dash = "dash", color = "#F2E744")),
          list(type = "line", x0 = -1, x1 = 1, y0 = 60.0, y1 = 60.0, line = list(dash = "dash", color = "#F2AA52")),
          list(type = "line", x0 = -1, x1 = 1, y0 = 70.0, y1 =70.0, line = list(dash = "dash", color = "#F23E2E"))
        ),
        yaxis = list(range = c(0, 105), dtick= 10, title = "Escore T", fixedrange = TRUE),
        xaxis = list(showticklabels = FALSE, title = "Depressão", range=c(-1, 1), fixedrange = TRUE),
        dragmode = FALSE
      )
  })
  
  output$bargraph <- renderPlotly({
    thePlot()
  })
  
  
  # text
  ## static texts
  text_c_none <- glue("Sua pontuação padronizada (de 0 à 100) foi classificada como **'Sem sintomas'**. Isso significa que você não tem sintomas e é improvável que tenha um \\
                        problema clínico significativo nesta área")
  text_c_mild <- glue("Sua pontuação padronizada (de 0 à 100) foi classificado como **'Poucos sintomas'**. Isto significa que a sua pontuação corresponde à da maioria da população em geral no Brasil e é improvável \\
                        representar um problema clínico significativo nesta área.")
  text_c_mod <- glue("Sua pontuação padronizada (de 0 à 100) foi classificado como **'Sintomas moderados'**. Isso significa que sua pontuação está maior que 84% da população Brasileira nesta escala \\
                        É possível que os sintomas de depressão sejam intensos o suficientes para estarem causando algum prejuizo. \\
                        Pode ser necessária uma avaliação adicional com um profissional de saúde mental. Também é indicado que você \\
                        faça este teste novamente em duas semanas para ver como as coisas estão indo e reavaliar seu nível de depressão")
  text_c_severe <- glue("Sua pontuação padronizada (de 0 à 100) foi classificado como **'Sintomas graves'**. Isso significa que sua pontuação está maior que 97% da população Brasileira nesta escala \\
                        É possível que os sintomas de depressão sejam intensos o suficientes para estarem causando algum prejuizo e talvez esteja em algum risco para si mesmo. \\
                        É  necessária uma avaliação adicional com um profissional de saúde mental. Também é indicado que você \\
                        faça este teste novamente em duas semanas para ver como as coisas estão indo e reavaliar seu nível de depressão")
  
  #   ## dynamic texts
  init_text <- reactive({glue("### Relatório e recomendações iniciais
                        Abaixo você encontrará sua pontuação padronizada, comparanda com a população brasileira. \\
                        A pontuação padronizada (Escore T) é uma medida que utiliza um método denominado Teoria de Resposta ao Item, baseado nas análises de respostas \\
                        às mesmas perguntas por Brasileiros na Pesquisa Nacional de Saúde de 2019 (Damiano et al. 2023 - https://doi.org/10.47626/1516-4446-2022-2945). Essa pontuação padronizada varia de 0 a 100, sendo 0 a 50 \\
                        representando nenhum sintoma, de 50 a 60 representando poucos sintomas, entre 60 e 70 como sintomas moderados, e acima de 70 como sintomas graves e que merecem atenção clínica para depressão.")}) |>  bindEvent(input$calcular)
  fin_text <- reactive({glue("\\* *Quaisquer resultados do questionário precisam ser interpretados no contexto de outras informações em seu atendimento clínico, \\
                                portanto, é sempre aconselhável discutir os resultados dos testes com um profissional de saúde mental.*")}) |>   bindEvent(input$calcular)
  
  dep_text <- reactive({
    att_text_c <- case_when(valores$escoreT <= 50 ~ text_c_none,
                            valores$escoreT <= 60 ~ text_c_mild,
                            valores$escoreT <= 70 ~ text_c_mod,
                            valores$escoreT > 70 ~ text_c_severe)
  }) |> bindEvent(input$calcular)
  
  
  ## render for the UI
  output$init_text_r <- renderUI({init_text() |>   markdown()})
  output$fin_text_r <- renderUI({fin_text() |>   markdown()})
  output$dep_text_r <- renderUI({dep_text() |>   markdown()})
  
}

shinyApp(ui = ui, server = server)


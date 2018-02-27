ProjTemplate::reload()
library(shiny)
load(file.path(datadir, 'data','rda','exp_sepsis2','data.rda'))
choice_names <- names(select(hosp_data, lupus_sepsis:nonlupus_failure_renal))
hosp_data <- hosp_data %>% 
  mutate_at(vars(teach:hosp_region), as.factor)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'y',
                  label = 'Y-axis:',
                  choices = names(dplyr::select(hosp_data, lupus_dead:nonlupus_failure_renal)),
                  selected = "lupus_dead"),
      selectInput(inputId = 'x',
                  label = 'X-axis:',
                  choices = names(dplyr::select(hosp_data, lupus_dead:nonlupus_failure_renal)),
                  selected = 'nonlupus_dead'),
      selectInput(inputId = 'z', 
                  label = "Color by:",
                  choices = c('',names(dplyr::select(hosp_data, teach:hosp_region))),
                  selected = 'bedsize'),
      selectInput(inputId = 'w',
                  label = 'Size:',
                  choices = names(dplyr::select(hosp_data, contains('sepsis'))),
                  selected = 'n_sepsis')
    ),
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
    
  )
)

server <- function(input, output) {
  output$scatterplot = renderPlot({
    ggplot(data = hosp_data, aes_string(x = input$x, y = input$y,  size = input$w))+
      geom_point()+
      geom_abline() +
      geom_smooth(se = FALSE)
  })
}

shinyApp(ui, server)
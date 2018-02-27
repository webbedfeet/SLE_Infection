ProjTemplate::reload()
library(shiny)
load(file.path(datadir, 'data','rda','exp_sepsis2','data.rda'))
choice_names <- names(select(hosp_data, lupus_sepsis:nonlupus_failure_renal))
hosp_data <- hosp_data %>% 
  mutate_at(vars(teach:hosp_region), as.factor)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'x',
                  label = 'Variable:',
                  choices = names(dplyr::select(hosp_data, lupus_dead:nonlupus_failure_renal)),
                  selected = 'nonlupus_dead'),
      checkboxInput('check','Grouped', value=FALSE),
      selectInput(inputId = 'z', 
                  label = "Group by:",
                  choices = c('',names(dplyr::select(hosp_data, teach:hosp_region))),
                  selected = 'bedsize')
   ),
    mainPanel(
      h4('Density plot'),
      plotOutput(outputId = "densityplot"),
      hr(),
      h4("Kruskal test"),
      tableOutput(outputId = 'test')
    )
    
  )
)

server <- function(input, output) {
  output$densityplot <-  renderPlot({
    plt <- ggplot(data = hosp_data, aes_string(x = input$x))
    if(input$check){
      plt +  geom_density(aes_string(color = input$z))
    } else {
      plt+geom_density()
    }
  })
  output$test <-  renderTable({
    if(input$check){
    broom::tidy(kruskal.test(pull(hosp_data[,input$x]), pull(hosp_data[,input$z])))[,1:2]
    }
  })
}

shinyApp(ui, server)
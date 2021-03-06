#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rlang)
library(purrr)
library(tidyverse)
library(fpp2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exponential smoothing models"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      column(4,
             wellPanel(
               textInput("model",
                         "Model specification",
                         value = "hw(austourists)"),
               uiOutput("state_slider")
             ),
             htmlOutput("model_out")
      ),
      
      # Show a plot of the generated distribution
      column(8,
             plotOutput("model_plot"),
             div(tableOutput("model_states"), class="state_table"),
             tags$head(
               tags$script("
        $('body').on('DOMSubtreeModified', '#state_table', function() {
alert('test');
        });
           ")
               
             )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$state_slider <- renderUI({
    fit <- model_fit_debounce()
    if(is.null(fit)){
      return(NULL)
    }
    sliderInput("state_len", label = "State slider",
                min = pmax(3, frequency(fit$x)), max = NROW(fit$states), value = frequency(fit$x), step=1,
                animate = TRUE)
  })
  
  estimate_model <- safely(function(model_text){
    model_expr <- parse_expr(model_text)
    selected_model <- call_name(model_expr)
    if(!(selected_model %in% c("ses", "holt", "hw", "ets"))){
      stop("Inputted model must be either `ses()`, `holt()`, `hw()`, or `ets()`")
    }
    else{
      eval_tidy(model_expr)
    }
  })
  
  model_fit <- reactive({
    fit <- estimate_model(input$model)
    if(is.null(fit$error)){
      if(!is.null(fit$result$model)){
        # ses/holt/hw
        fit$result$model
      }
      else{
        # ets
        fit$result
      }
    }
    else{
      showNotification(fit$error$message, type = "error")
      NULL
    }
  })
  
  model_fit_debounce <- model_fit %>% debounce(1000)
  model_subset <- reactive({
    fit <- model_fit_debounce()
    if(is.null(fit)){
      return(NULL)
    }
    if(!is.null(input$state_len)){
      fit$states <- head(fit$states, input$state_len)
      fit$x <- head(fit$x, input$state_len)
    }
    fit
  })
  
  output$model_out <- renderUI({
    capture.output(print(model_fit_debounce())) %>%
      map(p) %>%
      invoke(tagList, .)
  })
  
  output$model_plot <- renderPlot({
    if(!is.null(model_subset())){
      autoplot(model_subset())
    }
  })
  
  output$model_states <- renderTable({
    if(!is.null(model_subset())){
      fit <- model_subset()
      fit$states %>% 
        as_tibble %>%
        mutate(t = row_number() - 1L,
               yhat = c(NA, fitted(fit)[seq_len(NROW(fit$states) - 1)]),
               y = c(NA, fit$x[seq_len(NROW(fit$states) - 1)])) %>% 
        select(t, y, everything()) %>%
        tail(pmax(3, frequency(fit$x)))
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


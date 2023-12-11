library(shiny)


ui <- fluidPage(
  titlePanel("BMI Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("weight_kg", "Enter Weight (kg):", value = 70, min = 1),
      numericInput("height_cm", "Enter Height (cm):", value = 170, min = 1),
      actionButton("calculate_bmi", "Calculate BMI")
    ),
    mainPanel(
      h4("BMI Result:"),
      textOutput("bmi_result"),
      textOutput("bmi_category")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$calculate_bmi, {
    weight_kg <- input$weight_kg
    height_m <- input$height_cm / 100
    
    bmi <- weight_kg / (height_m^2)
    
    output$bmi_result <- renderText({
      paste("Your BMI is:", round(bmi, 2))
    })
    
    output$bmi_category <- renderText({
      category <- classify_bmi(bmi)
      paste("BMI Category:", category)
    })
  })
  
  classify_bmi <- function(bmi) {
    if (bmi < 18.5) {
      return("Underweight")
    } else if (bmi >= 18.5 && bmi < 25) {
      return("Normal Weight")
    } else if (bmi >= 25 && bmi < 30) {
      return("Overweight")
    } else {
      return("Obesity")
    }
  }
}


shinyApp(ui = ui, server = server)

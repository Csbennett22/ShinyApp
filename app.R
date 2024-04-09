library(shiny)
library(httr)
library(jsonlite)

# Define UI
ui <- fluidPage(
  titlePanel("Recipe Ingredient Substituter"),
  sidebarLayout(
    sidebarPanel(
      textInput("recipe", "Enter your recipe:"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("suggestions")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Define the callChatGPT function inside the server function
  callChatGPT <- function(recipe) {
    url <- "https://api.openai.com/v4/completions" # OpenAI API endpoint
    body <- list(
      model = "text-davinci-003", # Specify the model
      prompt = paste("Here's a recipe:", recipe, "\nHow can I substitute ingredients to meet dietary restrictions?"),
      temperature = 0.7,
      max_tokens = 150
    )
    
    # Log the request details
    cat("Request URL:", url, "\n")
    cat("Headers:\n")
    cat("Authorization: Bearer YOUR_API_KEY\n") # For security, don't log your real API key
    cat("Content-Type: application/json\n")
    cat("Body:\n")
    print(toJSON(body))
    
    # Make the API call
    response <- POST(url, body = toJSON(body), add_headers(`Authorization` = paste("Bearer", "YOUR_ACTUAL_API_KEY"), `Content-Type` = "application/json"))
    cat("Status Code:", status_code(response), "\n")
    
    if (status_code(response) != 200) {
      cat("Response content:\n")
      print(content(response, "text"))
    } else {
      content <- fromJSON(rawToChar(response$content))
      print(content$choices[[1]]$text)
      return(content$choices[[1]]$text)
    }
    
    content <- fromJSON(rawToChar(response$content))
    
    # Log the response for debugging
    print(content$choices[[1]]$text)
    return(content$choices[[1]]$text)
  }
  
  
  # React to the submit button being pressed
  observeEvent(input$submit, {
    # Call the ChatGPT function with the recipe input and display the suggestions
    suggestions <- callChatGPT(input$recipe)
    output$suggestions <- renderText({
      suggestions
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)

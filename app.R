#install.packages("httr")
#setwd("C:/Users/chris/Documents/RecipeApp/ShinyApp")
#install.packages("shinythemes")
library(shiny)
library(httr)
library(httr2)
library(sass)
library(markdown)
library(waiter)
library(shinyjs)
library(shinyCopy2clipboard)
library(jsonlite)
library(data.table)
library(RcppHNSW)
library(tidyverse)
library(tm)
library(irlba)
library(shinythemes) 
library(stats)

# Install shinyCopy2clipboard
if (!requireNamespace("shinyCopy2clipboard", quietly = TRUE)) {
  remotes::install_github("deepanshu88/shinyCopy2clipboard")
}
#rm(myVar)

file_path <- "recipe_embeddings.rds"
if (file.exists(file_path)) {
  #load('www/recipe_embeddings.Rdata')
  recipe_embeddings <- readRDS("recipe_embeddings.rds")
  text_df <- readRDS("text_df.rds")
} else {
  print("File Not Found")
  #recipe_embeddings <- read.csv("www/recipe_embeddings.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  #text_df <- read.csv("www/text_df.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}
#load('recipe_embeddings.Rdata')
#recipe_hnsw_index <- readRDS(file = "recipe_hnsw_index.rds")
set.seed(123)
# K-means clustering
clusters <- kmeans(recipe_embeddings, centers = 50)  # 50 clusters
sample_size <- 200  # samples per cluster

sampled_rows <- unlist(sapply(1:50, function(cluster_id) {
  rows_in_cluster <- which(clusters$cluster == cluster_id)
  sample(rows_in_cluster, min(sample_size, length(rows_in_cluster)))
}))
sampled_embeddings <- recipe_embeddings[sampled_rows, ]
recipe_hnsw_index <- hnsw_build(X = as.matrix(sampled_embeddings), distance = 'cosine', M = 30, ef = 5)


#recipe_hnsw_index <- hnsw_build(X = as.matrix(recipe_embeddings), distance = 'cosine', M = 30, ef = 5)


# Use the API key
openai_api_key <- "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"
embedding_key <- "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"
Sys.setenv('OPENAI_API_EMBEDDING'= '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')
Sys.setenv('OPENAI_API_KEY'= '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')

apiKey <- openai_api_key

css <- sass(sass_file("www/chat.scss"))
jscode <- 'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'

GetEmbeddings <- function(input_strings) {
  base_url <- 'https://api.openai.com/v1'
  api_key <- Sys.getenv('OPENAI_API_EMBEDDING')
  model_type <- 'text-embedding-3-large' 
  body <- list(input = input_strings,
               model = model_type)
  resp <- request(base_url) %>%
    req_url_path_append('embeddings') %>%
    req_auth_bearer_token(token = api_key) %>%
    req_headers('Content-Type' = 'application/json') %>%
    req_user_agent('Christian Bennett @csbenn1') %>%
    req_body_json(body) %>%
    req_perform()
  
  resp <- resp %>% resp_body_json()
  
  embeddings <- lapply(resp$data, function(x){
    embedding = x[['embedding']]
  }) %>% rbindlist()
  
  return(embeddings)
}

GPTchat <- function(user_prompt = NULL,context = NULL) {
  if (class(user_prompt)!='character') {stop('Error: Please enter a character string into the prompt')}
  if (!inherits(fromJSON(context, simplifyVector = FALSE), "list")) {
    stop('Error: Please ensure context is a JSON formatted string')
  }
  if (!exists('chat_history')) {
    chat_history <<- list(Prompt='',Response='')
  }
  if (!exists('system_prompt2')) {
    system_prompt2 <- list(list(role="system",
                                content="You are a helpful assistant.")
    )
  }
  base_url <- 'https://api.openai.com/v1'
  api_key <- Sys.getenv('OPENAI_API_KEY')
  system_prompt2 <- c(system_prompt2,
                      list(list(role="system",content=chat_history$Response)),
                      list(list(role="system",content=context)), # add context, i.e. provide results of the embedding clusters
                      list(list(role="user",content=user_prompt))
  )
  body <- list(model = 'gpt-3.5-turbo',
               messages = system_prompt2)
  #print(body)
  #print(system_prompt2)

  req <- request(base_url)
  resp <- req %>%
    req_url_path_append('chat/completions') %>%
    req_auth_bearer_token(token = api_key) %>%
    req_headers('Content-Type' = 'application/json') %>%
    req_user_agent('Christian Bennett @csbenn1') %>%
    req_body_json(body) %>%
    req_perform()
  
  #response_content <- fromJSON(rawToChar(resp$content))
  #print(response_content)

  openai_chat_response <- resp %>% resp_body_json(simplifyVector = TRUE)
  chat_history <<- c(chat_history,list(Prompt = user_prompt, Response = openai_chat_response$choices$message$content))
  # return(openai_chat_response$choices$message$content)
  return(chat_history)
  #print(chat_history[-1:-2]$Response %>% unlist())
  #return(chat_history[-1:-2] %>% unlist())
}

execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
  observeEvent(once = TRUE, reactiveValuesToList(session$input), {
    force(expr)
  }, ignoreInit = TRUE)
}


# prompt =  "What would be the best way to make my pancakes egg-free"
# prompt_embeddings <- data.table(recipe_title = 'prompt', embeddings = GetEmbeddings(prompt))
# prompt_matrix <- as.matrix(prompt_embeddings[,-1, with = FALSE])
# hnsw_results_index <- hnsw_search(X = prompt_matrix, ann = recipe_hnsw_index, k = 30, ef = 5)
# recipe_embeddings$RowNumber <- text_df$RowNumber
# matched_row_numbers <- recipe_embeddings$RowNumber[hnsw_results_index$idx]
# hnsw_results_recipes <- text_df %>% filter(RowNumber %in% matched_row_numbers)
# 
# GPTchat <- GPTchat(user_prompt = prompt, context = toJSON(hnsw_results_recipes[,c("RowNumber","file_text")]) )

#chatGPT_R(openai_api_key, "What would be the best way to make my pancakes egg-free", hnsw_results_recipes, model="gpt-3.5-turbo")

# chatGPT_R <- function(user_prompt, context, model = "gpt-3.5-turbo") {
#   # Assuming context needs to be concatenated with prompt or handled separately if API supports it.
#   full_prompt <- paste(user_prompt, context)  # Modify this line based on how you want to use `context`.
#   api_key <- Sys.getenv('OPENAI_API_KEY')
#   response <- POST(
#     url = "https://api.openai.com/v1/chat/completions",
#     add_headers(
#       Authorization = paste("Bearer", api_key),
#       `Content-Type` = "application/json"
#     ),
#     encode = "json",
#     body = list(
#       model = model,
#       messages = list(
#         list(role = "user", content = full_prompt)  # Removed context from here
#       )
#     )
#   )
#   
#   if (status_code(response) > 200) {
#     if (!is.null(content(response)$error)) {
#       result <- trimws(content(response)$error$message)
#     } else {
#       result <- "An unknown error occurred."
#     }
#   } else {
#     result <- trimws(content(response)$choices[[1]]$message$content)
#   }
#   #print(result)
#   return(result)
# }

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),  # Set the theme to Superhero
  useWaiter(),
  useShinyjs(),
  use_copy(),
  tags$head(tags$style(css)),
  
  # Apply a flexbox layout to center the chat container
  div(
    style = "display: flex; justify-content: center; align-items: flex-start; height: 90vh; padding-top: 10vh;",
    tags$div(
      id = "chat-container",
      tags$div(
        id = "chat-header",
        tags$img(src = "robot_chef.avif", alt = "AI Profile Picture"),
        tags$h2("SubstiChef Recipe Bot")
      ),
      tags$div(
        id = "chat-history",
        uiOutput("chatThread"),
      ),
      tags$div(
        id = "chat-input",
        tags$form(
          column(12, textAreaInput(inputId = "prompt", label="", placeholder = "Type your message here...", width = "100%")),
          fluidRow(
            tags$div(style = "margin-left: 1.5em;",
                     actionButton(inputId = "submit",
                                  label = "Send",
                                  icon = icon("paper-plane")),
                     actionButton(inputId = "remove_chatThread",
                                  label = "Clear History",
                                  icon = icon("trash-can")),
                     CopyButton("clipbtn",
                                label = "Copy",
                                icon = icon("clipboard"),
                                text = "")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  historyALL <- reactiveValues(df = data.frame() , val = character(0))

  # On click of send button
  observeEvent(input$submit, {

    if (nchar(trimws(input$prompt)) > 0) {

      # Spinner
      w <- Waiter$new(id = "chat-history",
                      html = spin_3(),
                      color = transparent(.5))
      w$show()
      #print(input$prompt)

      # Response
      prompt_embeddings <- data.table(recipe_title = 'prompt', embeddings = GetEmbeddings(input$prompt))
      prompt_matrix <- as.matrix(prompt_embeddings[,-1, with = FALSE])
      hnsw_results_index <- hnsw_search(X = prompt_matrix, ann = recipe_hnsw_index, k = 60, ef = 5)
      recipe_embeddings$RowNumber <- text_df$RowNumber
      matched_row_numbers <- recipe_embeddings$RowNumber[hnsw_results_index$idx]
      hnsw_results_recipes <- text_df %>% filter(RowNumber %in% matched_row_numbers)
      recipe_embeddings <- subset(recipe_embeddings, select = -RowNumber)
      
      # Assume GetEmbeddings and hnsw_search functions are defined elsewhere
      prompt_embeddings <- data.table(recipe_title = 'prompt', embeddings = GetEmbeddings(input$prompt))
      prompt_matrix <- as.matrix(prompt_embeddings[,-1, with = FALSE])
      hnsw_results_index <- hnsw_search(X = prompt_matrix, ann = recipe_hnsw_index, k = 60, ef = 5)
      recipe_embeddings$RowNumber <- text_df$RowNumber
      matched_row_numbers <- recipe_embeddings$RowNumber[hnsw_results_index$idx]
      hnsw_results_recipes <- text_df %>% filter(RowNumber %in% matched_row_numbers)
      
      # Format historyALL$df and hnsw_results_recipes for context
      chat_history_interactions <- lapply(1:nrow(historyALL$df), function(i) {
        list(
          list(role = ifelse(historyALL$df$users[i] == "Human", "user", "assistant"), content = historyALL$df$content[i])
        )
      })
      hnsw_interactions <- lapply(1:nrow(hnsw_results_recipes), function(i) {
        list(role = "system", content = paste("Recipe", hnsw_results_recipes$RowNumber[i], ":", hnsw_results_recipes$file_text[i]))
      })
      
      # Combine interactions and convert to JSON
      combined_interactions <- c(do.call(c, chat_history_interactions), do.call(c, hnsw_interactions))
      combined_context_json <- jsonlite::toJSON(combined_interactions, auto_unbox = TRUE)
      
      # Make chat function call with combined context
      gpt_return <- GPTchat(user_prompt = input$prompt, context = combined_context_json)
      #print(chat_history[length(chat_history)]$Response %>% unlist())
      
      historyALL$val <- gpt_return
      new_history <- data.frame(users = c("Human", "AI"),
                            content = c(input$prompt, markdown::mark_html(text=chat_history[length(chat_history)]$Response %>% unlist())),
                            stringsAsFactors = FALSE)
      historyALL$df <- rbind(historyALL$df, new_history)
      updateTextInput(session, "prompt", value = "")

    # Conversation Interface
      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          tags$div(class = ifelse(historyALL$df[x, "users"] == "Human",
                                  "user-message",
                                  "bot-message"),
                   HTML(paste0(ifelse(historyALL$df[x, "users"] == "Human",
                                      "<div class='img-wrapper'><img src='girl.avif' class='img-wrapper2'></div>",
                                      "<div class='img-wrapper'><img src='boy.avif' class='img-wrapper2'></div>"),
                               historyALL$df[x, "content"])))
        })
        do.call(tagList, conversations)
      })
      
      w$hide()
      execute_at_next_input(runjs(jscode))
      
    }

  })

  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({return(NULL)})
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })

  observe({
    req(input$clipbtn)
    CopyButtonUpdate(session,
                     id = "clipbtn",
                     label = "Copy",
                     icon = icon("clipboard"),
                     text = as.character(historyALL$val))

  })


}


shinyApp(ui=ui, server=server, options = list(browser = "firefox"))

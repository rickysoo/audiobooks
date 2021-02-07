library(shiny)
library(shinythemes)
library(shinycustomloader)
library(tidyverse)
library(caret)
library(xml2)
library(rvest)
library(promises)
library(future)
plan(multisession)

rounds <- 5

ui <- fluidPage(
  tags$head(includeHTML('google-analytics.html')),
  theme = shinytheme('united'),
  
  titlePanel('Beat The Machine'),
  p('Can you do better than machine learning?'),
  
  tabsetPanel(
    id = 'tab',
    
    tabPanel(
      title = 'Welcome',
      
      sidebarLayout(
        sidebarPanel(
          withLoader(uiOutput('image_random'), loader = 'pacman')
        ),
        
        mainPanel(
          br(),
          
          h3('Human vs. Machine'),
          p('People say machines are going to take over our jobs. But do you know machines are not perfect? You still can do better than machines!'),
          p('In this game, some audiobooks on Audible.com have been labeled as "Popular", and the others have not. Can you guess which are the popular ones and which are not?'),
          p('Even the machine is not perfect. In this case, the machine has been trained but can only guess the right audiobooks 72% of the time. Play this game and show that you can do better than machine learning!'),
          
          h3('How to Play?'),
          tags$ol(
            tags$li('Click "Play Now!" above to start the game!'),
            tags$li(sprintf('There will be %d rounds of guessing. You will be presented with an audiobook in each round.', rounds)),
            tags$li('Decide whether the audiobook is one most popular item or not. Click "Yes" or "No"'),
            tags$li('The answer will show up in a message on the bottom right. The machine will also guess and the result will be shown.'),
            tags$li('The score will be shown on top. The game continues until the final round.')
          ),
          
          h3('Tips to Beat The Machine'),
          p('Check out the audiobook details and click on the picture to get some clues what it means to be a popular audiobook. Learn from the feedback given after each round too. Good luck!')
        )
      )
      
      # br(),
      # withLoader(tableOutput('data'), loader = 'pacman')
    ),
    
    tabPanel(
      title = 'Play Now!',
      
      br(),
      h3(textOutput('score')),
      
      br(),
      sidebarLayout(
        sidebarPanel(
          withLoader(uiOutput('image'), loader = 'pacman'),
          
          br(),
          withLoader(uiOutput('link'), loader = 'pacman')
        ),
        
        mainPanel(
          h4(textOutput('question')),
          
          fluidRow(
            column(width = 12, uiOutput('action_yes'), uiOutput('action_no'), uiOutput('action_restart'))
          ),
          
          br(),
          withLoader(tableOutput('item'), loader = 'pacman')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    model = NULL,
    quiz = NULL,
    round = 1,
    end = FALSE
  )
  
  load_data <- reactive({
    print('Loading data...')
    
    readRDS('audible-complete.rds')
    # readRDS('audible_changed.rds')
  })
  
  # load_sample_data <- reactive({
  #   df <- load_data() %>%
  #     slice_sample(n = 10) %>%
  #     select(-Popular, -Predicted) %>%
  #     mutate(
  #       Title = paste0('<a href="', GetURL(ASIN), '" target="_blank">', Title, '</a>')
  #     )
  #   
  #   df[, c('ASIN', 'Title', 'Author', 'Narrator', 'Category', 'Price', 'Rating', 'RatingCount', 'Length', 'DaysReleased')]
  # })
  
  load_sample <- function(number) {
    print('Loading sample...')
    
    df <- load_data() %>%
      slice_sample(n = number) %>%
      rename(
        'Number of Ratings' = 'RatingCount',
        'Release Date' = 'DaysReleased'
      ) %>%
      mutate(
        Price = paste0('$', Price),
        Rating = paste0(Rating, ' star(s)'),
        Length = paste0(Length, ' minutes'),
        `Release Date` = as.Date('1995-12-31') + `Release Date`
      )
    
    # df
    df[, c('ASIN', 'Title', 'Author', 'Narrator', 'Category', 'Price', 'Rating', 'Number of Ratings', 'Length', 'Release Date', 'Popular', 'Predicted')]
  }
  
  load_quiz <- function(number = rounds) {
    print('Loading quiz...')
    
    df <- load_sample(rounds)%>%
      mutate(
        Choice = NA,
        User = 0,
        Machine = 0
      )
    
    df[, c('ASIN', 'Title', 'Author', 'Narrator', 'Category', 'Price', 'Rating', 'Number of Ratings', 'Length', 'Release Date', 'Popular', 'Predicted', 'Choice', 'User', 'Machine')]
  }
  
  GetURL <- function(ASIN) {
    url <- paste0('https://www.audible.com/pd/', ASIN)
    url
  }
  
  GetImageURL <- function(ASIN) {
    # future({
    #   read_html(GetURL(ASIN))  
    # }) %...>%
    #   html_nodes(xpath = '//meta[@property="og:image"]') %...>%
    #   html_attr('content')
    
    # Old 2    
    read_html(GetURL(ASIN)) %>%
      html_nodes(xpath = '//meta[@property="og:image"]') %>%
      html_attr('content')
    
    # Old 1
    # tryCatch({
    #   url <- read_html(GetURL(ASIN)) %>%
    #     html_nodes(xpath = '//meta[@property="og:image"]') %>%
    #     html_attr('content')
    # }, error = function(e) {
    #   
    #   print('Error caught in GetImageURL()')
    #   print(url)
    #   print(e)
    #   
    #   ASIN <- load_sample(1) %>%
    #     pull('ASIN')
    #   
    #   url <- GetImageURL(ASIN)
    #   print(url)
    # })
    
    # return (url)
  }
  
  # output$data <- renderDT({
  #   datatable(
  #     load_sample_data(),
  #     rownames = FALSE,
  #     class = 'cell-border compact stripe',
  #     extensions = c('Responsive')
  #   )
  # })
  
  output$image_random <- renderUI({
    ASIN <- load_data() %>%
      slice_sample(n = 1) %>%
      pull('ASIN')
    
    ImageURL <- GetImageURL(ASIN)
    tags$img(src = ImageURL)
  })
  
  # output$data <- renderTable(
  #   load_sample_data() %>%
  #     select(-ASIN),
  #   rownames = FALSE,
  #   colnames = TRUE,
  #   bordered = TRUE,
  #   striped = TRUE,
  #   hover = TRUE,
  #   sanitize.text.function = function(x) x
  # )
  
  output$score <- renderText({
    if (is.null(values$quiz)) {
      values$quiz <- load_quiz()
    }
    
    if (is.null(values$round)) {
      values$round <- 1
    }
    
    user <- sum(values$quiz$User)
    machine <- sum(values$quiz$Machine)
    
    if (values$end) {
      round <- 'Final Score'
      
      if (user > machine) {
        result <- '| You Won!'
      }
      else if (user == machine) {
        result <- '| It\'s a draw!'
      }
      else if (user < machine) {
        result <- '| You Lost!'
      }
    }
    else {
      round <- paste0('Round ', values$round, ' of ', rounds)
      result <- ''
    }
    
    sprintf('%s | You %d-%d Machine %s', round, user, machine, result)
  })
  
  output$question <- renderText({
    if (values$end) {
      return (NULL)
    }
    
    if (is.null(values$quiz)) {
      values$quiz <- load_quiz()
    }
    
    if (is.null(values$round)) {
      values$round <- 1
    }
    
    sprintf('Is this one most popular audiobook?')
  })
  
  output$action_yes <- renderUI({
    if (!values$end) {
      actionButton(
        'action_yes',
        label = 'Yes, most popular indeed!',
        style = 'background-color: #004165'
      )
    }
    else {
      return (NULL)
    }
  })
  
  output$action_no <- renderUI({
    if (!values$end) {
      actionButton(
        'action_no',
        label = 'No, not at the top yet.',
        style = 'background-color: #004165'
      )
    }
    else {
      return (NULL)
    }
  })
  
  output$action_restart <- renderUI({
    if (values$end) {
      actionButton(
        'action_restart',
        label = 'Restart the Game',
        style = 'background-color: #004165'
      )
    }
    else {
      return (NULL)
    }
  })
  
  output$image <- renderUI({
    print('Loading image...')
    
    if (is.null(values$quiz)) {
      values$quiz <- load_quiz()
    }
    
    ASIN <- values$quiz[values$round, 'ASIN']
    URL <- GetURL(ASIN)
    
    tryCatch({
      ImageURL <- GetImageURL(ASIN)
      return (a(tags$img(src = ImageURL), href = URL, target = '_blank'))
    }, error = function(e) {
      print('Error caught in $image')
      print(URL)
      print(e)
      
      values$quiz[values$round, ] <- load_quiz(1)
      return (NULL)
    })
  })
  
  output$link <- renderUI({
    if (is.null(values$quiz)) {
      values$quiz <- load_quiz()
    }
    
    ASIN <- values$quiz[values$round, 'ASIN']
    URL <- GetURL(ASIN)
    
    tags$a('View and listen on Audible.com', href = URL, target = '_blank')
  })
  
  output$item <- renderTable(
    {
      print('Loading item...')
      
      if (is.null(values$quiz)) {
        values$quiz <- load_quiz()
      }
      
      df <- values$quiz  %>%
        select(-ASIN, -Popular, -Predicted, -Choice, -User, -Machine)
      
      df[values$round, ] %>%
        t()
    },
    
    rownames = TRUE,
    colnames = FALSE,
    bordered = TRUE,
    striped = TRUE,
    hover = TRUE
  )
  
  ShowAnswer <- function() {
    if (values$quiz[values$round, 'Popular'] == 1) {
      showNotification('Answer - This audiobook is at the top!')
    }
    else {
      showNotification('Answer - This audiobook is NOT at the top yet.')
    }
  }
  
  UpdateUser <- function() {
    if (values$quiz[values$round, 'Popular'] == values$quiz[values$round, 'Choice']) {
      showNotification('Well done! You got it right!', type = 'message')
      values$quiz[values$round, 'User'] <- 1
    }
    else {
      showNotification('Sorry. You didn\'t get it right.', type = 'error')
    }
  }
  
  UpdateMachine <- function() {
    if (values$quiz[values$round, 'Predicted'] == values$quiz[values$round, 'Popular']) {
      values$quiz[values$round, 'Machine'] <- 1
      showNotification('The machine predicted it correctly.', type = 'message')
    }
    else {
      values$quiz[values$round, 'Machine'] <- 0
      showNotification('The machine predicted it incorrectly!', type = 'message')
    }
  }
  
  ProcessChoice <- function(choice) {
    values$quiz[values$round, 'Choice'] <- choice
    
    ShowAnswer()
    UpdateUser()
    UpdateMachine()
    
    if (values$round == rounds) {
      showNotification('Thank you for playing. The game is over. Check your score above!')
      values$end <- TRUE
      return
    }
    
    values$round <- min(values$round + 1, rounds)
  }
  
  observeEvent(
    input$action_yes, 
    
    {
      ProcessChoice(1)
    }
  )
  
  observeEvent(
    input$action_no, 
    
    {
      ProcessChoice(0)
    }
  )
  
  observeEvent(
    input$action_restart, 
    
    {
      values$round <- 1
      values$end <- FALSE
      values$quiz <- load_quiz()
    }
  )
}

shinyApp(ui, server)
library(tidyverse)
library(shiny)

source("functions.R")

server <- function(input, output) {
  
  # Generierung der Daten als reaktives Element
  pwd_data <- reactive({
    input$button_generate
    
    isolate({
      # Input validation
      validate(
        need(input$text_pwdlength != "" && !is.na(as.numeric(input$text_pwdlength)), "Password length must be a number, not empty and bigger than 3!"),
        need(input$text_pwdcount != "" && !is.na(as.numeric(input$text_pwdcount)), "Number of passwords must be a number and not empty!"),
        need(input$check_lowercase == TRUE  || input$check_uppercase == TRUE || input$check_numbers == TRUE || input$check_specialcharacters == TRUE, "Choose at least one type of symbol!")
      )
      
      # Generation of the password(s)
      array_pwd <- rep(NULL, as.numeric(input$text_pwdcount))
      for(index in 1:as.numeric(input$text_pwdcount)){
        # Calling the generatePassword-function
        pwd <- generatePassword(as.numeric(input$text_pwdlength), input$check_lowercase, input$check_uppercase, input$check_numbers, input$check_specialchars, input$text_specialchars)
        # Assignment of the created password to the password array
        array_pwd[index] <- pwd
      }
      
      # Rename column
      data.frame(Password=array_pwd)
    })
  })
  
  # Generierung der Daten als reaktives Element
  pwd_data_inputfile <- reactive({
    input$button_multipwd
    
    isolate({
      # Assign the uploaded csv-file to the variable inFile
      inFile <- input$file
      
      # Check if there is an uploaded file
      if(is.null(inFile)){
        return(NULL)
      }
      
      # Read the csv-file
      data <- read_csv(inFile$datapath, col_names = FALSE)
      # Calculate the length of the dataset (the number of users)
      length_data <- as.integer(lengths(data))
      
      # Create an array with length equal to the number of users
      array_pwd <- rep(NULL, length_data)
      # Generation of the password(s)
      for(index in 1:length_data){
        # Calling the generatePassword-function
        pwd <- generatePassword(as.numeric(input$text_pwdlength), input$check_lowercase, input$check_uppercase, input$check_numbers, input$check_specialchars, input$text_specialchars)
        # Assignment of the created password to the password array
        array_pwd[index] <- pwd
      }
      
      # Assign the passwords to the users
      data <- cbind(data, array_pwd)
      # Change the column names
      colnames(data) <- c("User", "PW")
      # Return the dataset
      data
    })
  })
  
  # Output table including the passwords
  output$table_pwd <- renderTable({
    input$button_generate
    
    isolate({
      pwd_data()
    })
  })
  
  output$button_download <- downloadHandler(
    
    # Create the filename
    filename = function() {
      paste("output", input$radio_filetype, sep = ".")
    },
    
    # Creating csv-file out of the data
    content = function(file) {
      sep <- ","
      
      write.table(pwd_data_inputfile(), file, sep = sep, row.names = FALSE)
    }
  )
}

ui <- fluidPage(
  div(titlePanel("Password Generator"), align = "center"),
  
  hr(),
  
  fluidRow(
    column(1),
    column(10,
           h4(strong("Description:")),
           "This app allows you to create one or multiple passwords.",
           br(),
           "The safety of your password increases by the number of different symbols you allow. Another factor is the length of your password.",
           br(),
           "You can upload a csv-file including only one column filled with usernames. After that, you can download a file with a full list of passwords for every user."
    ),
    column(1)
  ),
  
  hr(),
  
  fluidRow(
    column(1),
    column(10,
           wellPanel(
             fluidRow(
               column(2, textInput("text_pwdlength", "Password Length: ", value = 8)),
               column(2, textInput("text_pwdcount", "Number of Passwords: ", value = 1)),
               column(1, 
                      checkboxInput("check_lowercase", "Lowercase", value = TRUE),
                      checkboxInput("check_uppercase", "Uppercase", value = TRUE)
               ),
               column(1,
                      checkboxInput("check_numbers", "Numbers", value = TRUE),
                      checkboxInput("check_specialchars", "Special characters", value = TRUE)
               ),
               column(2,
                      textInput("text_specialchars", "Allowed special chars: ", value = "!?@(){}[]=~$%&#*-+.,_")
               ),
               column(1,
                      fileInput("file", label = "Upload:", accept = c('text/csv', 'text/comma-separated-values'))
               ),
               column(1,
                      radioButtons("radio_filetype", "File Type:", choices = c("csv"))
               ),
               column(1,
                      actionButton("button_generate", "Generate password(s)"),
                      br(),
                      br(),
                      downloadButton("button_download", "Download password(s)")
               )
             )
           )
    ),
    column(1)
  ),
  
  hr(),
  
  div(tableOutput("table_pwd"), align = "center"),
  
  hr(),
  
  fluidRow(
    column(1),
    column(10,
           wellPanel(
             h4(strong("Notice: This app was created to test some shiny features. List of tested items:")),
             tags$li("Validate input."),
             tags$li("Upload csv-files and edit the data."),
             tags$li("Download a csv-file and setting up the data type of the provided file.")
           )
    ),
    column(1)
  ),
  
  hr(),
  
  div(strong("Copyright by Felix Großmann (2017). No warranties."), align = "center")
)

shinyApp(ui = ui, server = server)
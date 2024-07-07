#' PDF to Chromadb
#' 
#' RStudio addin to add a pdf to chromadb vector database. The function 
#'
#' @param api_key Llamacloud api key.
#' @param open_ai_api_key Open Ai api key
#' @param path_to_chromadb Path to chromadb
#' @param default_collection Default collection in chromadb
#'
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_to_chromadb()
#' }
#' @import shiny
#' @import miniUI
pdf_to_chromadb <- function(
    api_key = Sys.getenv('LLAMA_CLOUD_API_KEY'),
    open_ai_api_key = Sys.getenv('OPENAI_API_KEY'),
    path_to_chromadb = Sys.getenv('PATH_TO_CHROMADB'),
    default_collection = Sys.getenv('CHROMADB_DEFAULT_COLLECTION')
    ){
  
  ui <- miniPage(
    gadgetTitleBar("Add PDF to Chromadb"),
    miniContentPanel(
      fileInput(
        inputId = 'filein',
        label = 'Upload File',
        accept = 'application/pdf'
      ),
      textInput(
        inputId = 'chromadb_path',
        label = 'Path/to/Chromadb',
        value= ifelse(
          test = path_to_chromadb == '', 
          yes = 'chromadb',
          no = path_to_chromadb
          )
      ),
      textInput(
        inputId = 'chromadb_collection',
        label = 'Collection',
        value = ifelse(
          test = default_collection == '', 
          yes = 'my-collection', 
          no = default_collection
          )
      ),
      uiOutput('warnings'),
      uiOutput("buttons")
    )
  )
  
  server <- function(input, output, session) {
   
    # Store reactive values
    values <- reactiveValues(have_pdf = FALSE)
    
    # Display Warnings
    output$warnings <- renderUI({
      if(open_ai_api_key == '') 
        tags$div('No openai key.', style = 'color: red')
  
    })
    
    # When file is uploaded verify contents and extract metadata and uri.
    observeEvent(input$filein, {
      info <- pdftools::pdf_info(input$filein$datapath)
      if(pdftools::pdf_info(input$filein$datapath)$pages >750){
        values$text <- glue::glue(
          "Error: PDF has more than 750 pages."
        )
        values$have_pdf <- FALSE
      } else{
        values$have_pdf <- TRUE
        values$created <- as.Date(info$created) 
        values$author <- info$keys$Author
        values$title <- info$keys$Title
        values$datapath <- input$filein$datapath
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Display buttons if valid file is uploaded
    output$buttons <- renderUI({
      if(!values$have_pdf) return(NULL)
      actionButton(
        inputId = 'parse',
        label = 'Llamaparse',
        icon = icon('plus')
      )
    })
    
    # When parse button is clicked convert pdf to markdown and display results
    observeEvent(input$parse, {
      withProgress({
        values$llamaparse <- llamaparse(
          filepath = values$datapath, 
          api_key = api_key
        )
      }, value = 1, message = "Converting PDF to Markdown")
      
      
      print(values$llamaparse$job_metadata)
      
      
      showModal(modalDialog(
        title = 'Add Data',
        size = 'xl',
        tabsetPanel(
          tabPanel(
            title = 'Markdown',
            shiny::markdown(values$llamaparse$markdown)
            ),
          tabPanel(
            title = 'Metadata',
            meta_ui(values$title, values$author, values$created)
            )
          ),
        footer = tagList(
          actionButton(
            inputId = 'add',
            label = 'Add to Chromadb',
            icon = icon('plus')
            ),
          downloadButton(
            outputId = 'download_markdown',
            label = 'Download Markdown'
          ),
          modalButton(
            label = "Close",
            icon = icon("close")
            )
          )
        ))
    })
    
    # When the add button is clicked chunk data and store in database
    observeEvent(input$add, {
      
      # Collect the metadata
      meta <- list(
        title = values$title,
        author = values$author,
        created = as.character(values$created)
      )
      
      # Connect to database
      collection <- get_or_create_collection(
        collection_name = input$chromadb_collection,
        client = get_client(dbpath = input$chromadb_path),
        embedding_function = openai_ef(open_ai_api_key = open_ai_api_key)
      )
      
      # Chunk the data
      values$chunked <- chunk_data(values$llamaparse$markdown, meta)
      
      # Upload the chunks to chromadb
      collection$upsert(
       documents = values$chunked$documents,
       ids = values$chunked$ids,
       metadatas = values$chunked$metadata
      )
      
      removeModal()

    })
    
    # Downloadhandler to store markdown
    output$download_markdown <- downloadHandler(
      filename = function(){
        
        timedata_str <- Sys.time() |> stringr::str_remove_all('-|:') |>
          stringr::str_replace_all('\\s', '_') |>
          stringr::str_sub(1, 15)
        
        paste0(timedata_str, '_llamaparse.md')
      },
      content = function(file){
        yaml <- list(
          title = values$title,
          author = values$author,
          created = as.character(values$created)
        ) |>
          yaml::as.yaml()
        
        markdown <- values$llamaparse$markdown |>
          stringr::str_split('\n') |>
          unlist() 
        
        markdown[markdown=='---'] <- '\n'
        
        markdown <- paste(markdown, collapse = '\n')
        
        markdown_out <- glue::glue(
          '---\n{yaml}---\n{markdown}\n'
        )
        
        write(markdown_out, file)
      }
    )
    
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      if(!is.null(values$chunked)){
        returnValue <- values$chunked
      } else returnValue <- values$results
      
      stopApp(returnValue)
    })
  }
  
  runGadget(ui, server)
}

meta_ui <- function(title, author, created){
  tagList(
    tags$br(),
    tags$h4("Metadata"),
    textInput(
      inputId = 'title',
      label = 'Title',
      value = title
    ),
    textInput(
      inputId = 'author',
      label = 'Author',
      value = author
    ),
    dateInput(
      inputId = 'created',
      label = 'Date Created',
      value = as.Date(created)
    )
  )
}
#pdf_to_chromadb()

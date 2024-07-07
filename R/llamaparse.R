#' Upload pdf to llamacloud and start conversion job
#' @keywords internal
parse_up <- function(
    filepath = system.file(
      'extdata/realist-evaluation.pdf', 
      package = 'llamaparse2chromadb'
    ), 
    api_key = Sys.getenv("LLAMA_CLOUD_API_KEY")
    ){
  
  f <- httr::upload_file(filepath, type = "application/pdf")
  
  req <- httr::POST(
    url = 'https://api.cloud.llamaindex.ai/api/parsing/upload',
    httr::add_headers(
      accept = "application/json",
      `Content-Type`= "multipart/form-data",
      Authorization = glue::glue("Bearer {api_key}")
    ),
    body = list(file = f)
  )
  
  httr::content(req)
  
}

#' Check status of conversion
#' @keywords internal
check_status <- function(job_id =  "e778beda-74bd-4f55-a700-beafed677556",
                         api_key = Sys.getenv("LLAMA_CLOUD_API_KEY")){
  
  req <- httr::GET(
    url = glue::glue('https://api.cloud.llamaindex.ai/api/parsing/job/{job_id}'),
    httr::add_headers(
      accept = "application/json",
      Authorization = glue::glue("Bearer {api_key}")
    )
  )
  
  httr::content(req)
  
}

#' Fetch processed markdown from server
#' @keywords internal
get_markdown <- function(job_id =  "e778beda-74bd-4f55-a700-beafed677556",
                         api_key = "llx-XTMPRKlsgx8whmOpf4nKf5m1X4EgNnAvJprNjHfoXydNiSLY"){
  
  req <- httr::GET(
    url = glue::glue('https://api.cloud.llamaindex.ai/api/parsing/job/{job_id}/result/markdown'),
    httr::add_headers(
      accept = "application/json",
      Authorization = glue::glue("Bearer {api_key}")
    )
  )
  
  results <- httr::content(req)
  results
}

#' Convert PDF to Markdown using Llamaparse.
#' @keywords internal
llamaparse <- function(
    filepath = system.file(
      'extdata/realist-evaluation.pdf', 
      package = 'llamaparse2chromadb'
      ),
    api_key = Sys.getenv("LLAMA_CLOUD_API_KEY"),
    timeout = 60
    ){
  
  info <- pdftools::pdf_info(filepath)
  
  if(info$pages >=750) stop("Only 750 pages per file")
  
  req <- parse_up(filepath, api_key)
  
  i <- 1
  results <- list()
  while(i < timeout){
    
    Sys.sleep(1)
    i <- i+1
    
    if(check_status(req$id, api_key)$status=="SUCCESS") {
      results <- get_markdown(req$id, api_key)
      break
    }
  }
  
  if(!length(results)){
    return("Job timed out without a successful response")
  } else return(results)
  
}
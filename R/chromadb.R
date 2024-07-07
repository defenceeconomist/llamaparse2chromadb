get_client <- function(dbpath = 'chromadb'){
  chromadb <- reticulate::import('chromadb')
  chromadb$PersistentClient(path = dbpath)
}

openai_ef <- function(open_ai_api_key = Sys.getenv('OPENAI_API_KEY')){
  chromadb <- reticulate::import('chromadb')
  chromadb$utils$embedding_functions$OpenAIEmbeddingFunction(
    api_key=open_ai_api_key,
    model_name="text-embedding-ada-002"
  )
}

get_or_create_collection <- function(
    collection_name = 'defecon-kb',
    client = get_client(), 
    embedding_function = openai_ef()
    ){
  
  client$get_or_create_collection(
    name = collection_name,
    embedding_function=embedding_function
  )
}


#' Function to split text based on embedding threshold.
#' @keywords internal
semantic_split <- function(x){
  sts <- reticulate::import("semantic_text_splitter")
  splitter <- sts$TextSplitter$from_tiktoken_model(model = "gpt-3.5-turbo", capacity = 250L)
  sapply(x, function(y){splitter$chunks(y)})
}

#' Generate sha1 hash of variables.
#' @keywords internal
generate_id <- function(x){
  hash <- sapply(x,digest::sha1)
  attr(hash, "names") <- NULL
  hash
}

#' Convert metadata into chromadb format.
#' @keywords internal
meta_to_json <- function(df){
  df |>
    dplyr::select(-"chunks", -"id") |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON(simplifyDataFrame = FALSE)
}

#' Chunk the markdown, combine with metadata and generate id.
#' @keywords internal
chunk_data <- function(markdown, meta){
  
  text <- heading <- chunks <- author <- NULL
  title <- created <- chunk_id <- page <- pages <- NULL
  
  df <- dplyr::tibble(text = markdown |> stringr::str_split("\n") |> unlist()) |>
    dplyr::mutate(page = cumsum(stringr::str_detect(text, "^---$"))) |>
    dplyr::mutate(text = stringr::str_remove_all(text, "^---$")) |>
    dplyr::filter(text != "#") |>
    dplyr::mutate(heading = cumsum(stringr::str_detect(text, "^#"))) |>
    dplyr::group_by(heading) |>
    dplyr::summarise(text = paste(text, collapse = '\n'),
                     pages = paste(min(page), "-", max(page))) |>
    dplyr::ungroup() |>
    dplyr::mutate(chunks = semantic_split(text)) |>
    tidyr::unnest(chunks) |>
    dplyr::group_by(heading) |>
    dplyr::mutate(chunk_id = paste0(heading, '.', 1L:dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::select(chunk_id, pages, chunks) |>
    dplyr::bind_cols(meta) |>
    dplyr::mutate(id = generate_id(paste(author, title, created, chunk_id, chunks)))
  
  list(
    documents = df$chunks,
    ids = df$id,
    metadata = meta_to_json(df)
  )
  
}

# Testing Data
# text_in <- readLines(system.file('extdata/realist-evaluation.md', package = 'llamaparse2chromadb')) |>
#   paste(collapse ='\n')
# 
# yaml_end <- stringr::str_locate_all(text_in, '---')[[1]][2,2]
# markdown <- stringr::str_sub(text_in,  yaml_end+3, nchar(text_in))
# meta <- yaml::yaml.load(stringr::str_sub(text_in, 4, yaml_end-3))


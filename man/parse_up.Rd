% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llamaparse.R
\name{parse_up}
\alias{parse_up}
\title{Upload pdf to llamacloud and start conversion job}
\usage{
parse_up(
  filepath = system.file("extdata/realist-evaluation.pdf", package =
    "llamaparse2chromadb"),
  api_key = Sys.getenv("LLAMA_CLOUD_API_KEY")
)
}
\description{
Upload pdf to llamacloud and start conversion job
}
\keyword{internal}

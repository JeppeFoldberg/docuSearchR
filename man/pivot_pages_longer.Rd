% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_page_numbers.R
\name{pivot_pages_longer}
\alias{pivot_pages_longer}
\title{Adds word pages as their own document}
\usage{
pivot_pages_longer(readtext_object)
}
\arguments{
\item{readtext_object}{a readtext s3 class created by load_documents}
}
\value{
the readtext object
}
\description{
takes a readtext object and explodes each word document to be their
own document.
}

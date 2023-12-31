% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyData.R
\name{tidyData}
\alias{tidyData}
\title{Tidying EHRC data tables}
\usage{
tidyData(
  name,
  sheets,
  skip = 10,
  path = "",
  clean = TRUE,
  save = FALSE,
  name.out = "output"
)
}
\arguments{
\item{name}{name of excel spreadsheet to tidy}

\item{sheets}{a number or vector corresponding to which worksheet is being tidied}

\item{skip}{number of rows to be deleted from the top of the spreadsheet (the
first required row is the one with the years in)}

\item{path}{the path of the excel spreadsheet}

\item{clean}{remove all rows where SE / (point estimate) > 0.5, or
where SE does not exist}

\item{save}{excel spreadsheet output required (TRUE / FALSE)}

\item{name.out}{name of excel spreadsheet to be ouput (requires save = TRUE)}
}
\value{
data.frame and (optional) excel spreadsheet with tidied data
}
\description{
Creates a data.frame and an (optional) excel spreadsheet with data from the EHRC
spreadsheets released alongside the statutory reporting.
}

#' Tidying EHRC data tables
#' 
#' Creates a data.frame and an (optional) excel spreadsheet with data from the EHRC
#' spreadsheets released alongside the statutory reporting.
#'
#' @param name name of excel spreadsheet to tidy
#' @param sheets a number or vector corresponding to which worksheet is being tidied
#' @param skip number of rows to be deleted from the top of the spreadsheet (the
#' first required row is the one with the years in)
#' @param path the path of the excel spreadsheet
#' @param save excel spreadsheet output required (TRUE / FALSE)
#' @param name_out name of excel spreadsheet to be ouput (requires save = TRUE)
#'
#' @return data.frame and (optional) excel spreadsheet with tidied data
#'
#' @examples
#'  df <- tidyData(name = "WRK.EMP.1.xlsx", sheets = 2, 
#'                 skip = 10, path = "", save = TRUE)
#'  
#'  sheets = c(2, 3, 4, 5)
#'  df <- tidyData(name = "WRK.EMP.1.xlsx", sheets = sheets, 
#'                 skip = 10, path = "", save = TRUE,
#'                 name_out = "WRK.EMP.1_employment")
#' @import stats
#' @import readxl
#' @import reshape2
#' @import tidyr
#' @import writexl

#' @export
tidyData <- function(name, sheets, skip = 10, path = "", save = FALSE, name_out = "output") {
  # name: name of excel file. Must end with file type e.g. '.xlsx'
  # sheets: vector of worksheet numbers
  # skip (numeric): how many rows to remove when loading excel file
  # path (optional): required if excel file not saved in working directory
  # save: save to excel file
  # name_out: name of output file (without '.xlsx' at the end)
  
  cat(sprintf("loading names of worksheets from excel document: %s\n", name))
  name <- paste0(path, name)
  # get sheet names
  sheet_names <- readxl::excel_sheets(name)
  
  if (length(sheets) > 1) {
    cat(sprintf("Number of sheets required is %d\n", length(sheets)))
    df <- list()
    i <- 1
    for (sheet in sheets) {
      cat(sprintf("loading sheet %d from %s", sheet, name))
      df_tmp <- as.data.frame(suppressMessages(
                  readxl::read_xlsx(name, sheet = sheet, skip = skip, col_names = FALSE)))
      cat(sprintf("\ttidying sheet %d\n", sheet))
      df[[i]] <- .tidyDataFrame(dataFrame = df_tmp)
      i <- i + 1
    } 
    df <- setNames(df, sheet_names[sheets])
  } else {
    cat(sprintf("Only %d sheet required\n", length(sheets)))
    cat(sprintf("loading sheet %d from %s\n", sheets, name))
    df_tmp <- as.data.frame(suppressMessages(
                readxl::read_xlsx(name, sheet = sheets, skip = skip, col_names = FALSE)))
    cat(sprintf("\ttidying sheet %d: %s\n", sheets, sheet_names[sheets]))
    df <- .tidyDataFrame(dataFrame = df_tmp)
    df <- setNames(list(df), sheet_names[sheets])
  }
  if (save == TRUE) {
    name_out <- paste0(name_out, ".xlsx")
    cat(sprintf("Saving data to %s\n", name_out))
    writexl::write_xlsx(df, name_out)
  }
  return(df)
}
#}}}

#### additional info and test code

TEST <- exists("TEST") && isTRUE(TEST)

if (TEST) {
  message("
Sourced \'tidyData.R\', to tidy up EHRC data tables.
")

  source("tidyData.R")

  start.time <- Sys.time()
  sheet_names <- readxl::excel_sheets("WRK.EMP.1.xlsx")
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  
  
  
} ## end if (TEST) 

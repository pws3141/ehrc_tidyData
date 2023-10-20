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
#' @param clean remove all rows where SE / (point estimate) > 0.5, or
#' where SE does not exist
#' @param save excel spreadsheet output required (TRUE / FALSE)
#' @param name.out name of excel spreadsheet to be ouput (requires save = TRUE)
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
#'                 name.out = "WRK.EMP.1_employment")
#' @import stats
#' @import readxl
#' @import reshape2
#' @import tidyr
#' @import writexl
#' @import plyr
#' @import stringr

#' @export
tidyData <- function(name, sheets, skip = 10, path = "", clean = TRUE,
                     save = FALSE, name.out = "output") {
  # name: name of excel file. Must end with file type e.g. '.xlsx'
  # sheets: vector of worksheet numbers
  # skip (numeric): how many rows to remove when loading excel file
  # path (optional): required if excel file not saved in working directory
  # save: save to excel file
  # name.out: name of output file (without '.xlsx' at the end)
  
  cat(sprintf("loading names of worksheets from excel document: %s\n", name))
  name <- paste0(path, name)
  # get sheet names
  sheet_names <- readxl::excel_sheets(name)
  # formatting names: remove 'Table X' and numbers
  # remove 'Table'
  sheet_names <- sub("^\\s*Table\\s*", "", sheet_names)
  # remove numbers
  #sheet_names <- gsub("^\\d{1,2}\\s*", "", sheet_names)
  sheet_names <- sub(".*? ", "", sheet_names)
  
  if (length(sheets) > 1) {
    cat(sprintf("Number of sheets required is %d\n", length(sheets)))
    df <- list()
    i <- 1
    for (sheet in sheets) {
      cat(sprintf("loading sheet %d from %s", sheet, name))
      df_tmp <- as.data.frame(suppressMessages(
                  readxl::read_xlsx(name, sheet = sheet, skip = skip, col_names = FALSE)))
      cat(sprintf("\ttidying sheet %d\n", sheet))
      df_tmp <- .tidyDataFrame(df = df_tmp)
      if (clean == TRUE) df_tmp <- .tidyDataClean(df_tmp)
      df[[i]] <- df_tmp
      i <- i + 1
    } 
    df <- setNames(df, sheet_names[sheets])
  } else {
    cat(sprintf("Only %d sheet required\n", length(sheets)))
    cat(sprintf("loading sheet %d from %s\n", sheets, name))
    df_tmp <- as.data.frame(suppressMessages(
                readxl::read_xlsx(name, sheet = sheets, skip = skip, col_names = FALSE)))
    cat(sprintf("\ttidying sheet %d: %s\n", sheets, sheet_names[sheets]))
    df_tmp <- .tidyDataFrame(df = df_tmp)
    if (clean == TRUE) df_tmp <- .tidyDataClean(df_tmp)
    df <- setNames(list(df), sheet_names[sheets])
  }
  if (save == TRUE) {
    name.out <- paste0(name.out, ".xlsx")
    cat(sprintf("Saving data to %s\n", name.out))
    writexl::write_xlsx(df, name.out)
  }
  return(df)
}
#}}}

#' Merging tidy data.frames from same domain and indicator

#' Merge list obtained from a run of 'tidyData' into a single data frame
#' including new columns for country, domain and indicator
#' 
#' @param tidy.list list from 'tidyData' function (run on multiple sheets)
#' @param domain string of domain e.g. "work", "health" etc.
#' @param indicator string of indicator e.g. "employment", "median earnings" etc.
#' @param save excel spreadsheet output required (TRUE / FALSE)
#' @param name.out name of excel spreadsheet to be ouput (requires save = TRUE)
#' 
#' @return data.frame of merged list, and (optional) excel spreadsheet
#' 
#' @import writexl
#' @import plyr
#' 
#' @export
tidyDataMerge <- function(tidy.list, domain, indicator,
                save = FALSE, name.out = "output_merged") {
        # colapse list into data.frame
        # first, need to add new column with country
        countries <- names(tidy.list)
        tidy.list <- lapply(seq_along(tidy.list), function(y, n, i) {
                                out <- cbind(rep_len(n[[i]], length.out = nrow(y[[i]])),
                                             y[[i]])
                                names(out)[1] <- "country"
                                out
                                }, y = tidy.list, n = names(tidy.list)
                        )
        # 'rbind.fill' adds columns not present in all outputs
        tidy_df <- plyr::rbind.fill(tidy.list)
        # add domain and indication, and var information
        # if GB present, then set between = Yes, otherwise No
        var.between <- "No"
        if ("Great Britain" %in% countries) {
                var.between <- "Yes"
        }

        tidy_df <- cbind(rep_len(domain, length.out = nrow(tidy_df)),
                        rep_len(indicator, length.out = nrow(tidy_df)),
                        tidy_df,
                        rep_len(var.between, length.out = nrow(tidy_df))
                        )
        names(tidy_df)[1:2] <- c("domain", "indicator")
        ncol_df <- ncol(tidy_df)
        names(tidy_df)[ncol_df] <- "between"
        if (save == TRUE) {
                res_excel <- list()
                res_excel[[sprintf("%s_%s", domain, indicator)]] <- tidy_df
                name.out <- paste0(name.out, ".xlsx")
                cat(sprintf("Saving data to %s\n", name.out))
                writexl::write_xlsx(res_excel, name.out)
        }
        return(df)
}

#### additional info and test code

TEST <- exists("TEST") && isTRUE(TEST)

if (TEST) {
  message("
Sourced \'tidyData.R\', to tidy up EHRC data tables.
")

source("tidyData_util.R")
source("tidyData.R")

#  start.time <- Sys.time()
#  sheet_names <- readxl::excel_sheets("WRK.EMP.1.xlsx")
#  end.time <- Sys.time()
#  time.taken <- end.time - start.time
#  time.taken
  
  
sheets = c(2, 3, 4, 5)
df <- tidyData(name = "WRK.EMP.1.xlsx", sheets = sheets, 
                 skip = 10, path = "", save = TRUE,
                 name.out = "output2")

df_merged <- tidyDataMerge(tidy.list = df, domain = "Work",
                           indicator = "Employment", save = TRUE,
                          name.out = "output_merged")

  
} ## end if (TEST) 

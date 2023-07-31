### Utility functions  for 'tidyData'

# change name of groups that appear more than once
.duplicateGroups <- function(groups) {#{{{
  # Aim: find duplicated groups, separated by NA
  #     rename these groups by appending a number to duplicates
  
  # In: vector of group names
  # Out: vector of new group names
  
  # list of index values which 'groups' is NA
  groups_NA <- which(is.na(groups))
  # remove indices of NA's that are at the beginning of 'groups'
  groups_NA <- groups_NA[-which(groups_NA == 1:length(groups_NA))]
  # index_vector initialisation
  index_vector <- rep(1, length.out = length(groups))
  for (i in 1:length(groups_NA)) {
    tmp_elt <- groups_NA[i]
    # check that the previous category is the same as the next
    # e.g. (... ,"hello", NA, "hello", ...)
    bool_check <- (groups[tmp_elt - 1] == groups[tmp_elt + 1])
    if (!is.na(bool_check) && bool_check) {
      # if TRUE, add 1 to index vector from element
      # 'groups[tmp_elt + 1]' to 'groups[groups_NA[i + 1] - 1'
      # i.e. from this NA to the next NA
      # assume that all same groups are adjacent to each other in data tables
      # TODO: remove this assumption
      # add 1 to the next group if same as previous
      # nb all data tables start with 'All' will never get neg index here:
      previous_group <- index_vector[tmp_elt - 1]
      elts_change <- (tmp_elt + 1):(groups_NA[i + 1] - 1)
      index_vector[elts_change] = index_vector[elts_change] + previous_group
    }
  }
  # Concatenate name of groups with index_vector
  # except where groups is NA and/ or index_vector is 1
  # as index is 1 for all groups NA, can just look at index_vector > 1
  which_greater1 <- which(index_vector > 1)
  #which_conc <- groups_not_NA[groups_not_NA %in% which_greater1]
  groups[which_greater1] <- paste0(groups[which_greater1], 
                                   index_vector[which_greater1])
  # output
  groups
}
#}}}


### create 'year_beg' and 'year_end' variables
.tidyYears <- function(years) {#{{{
  # remove leading NAs
  first_year <- min(which(!is.na(years)))
  years <- years[first_year:length(years)]
  
  # fill in blank elements by top-filling
  isnotblank <- !is.na(years)
  years <- years[which(isnotblank)][cumsum(isnotblank)]
  
  # re-pad with NAs
  years_df <- c(rep(NA, first_year - 1), years)
 
  # check format of 'years':
  # two options: 20XX/XX or single years 20XX
  ## TODO: option 20xx - 20xx
  
  # if 20xx/xx split by deliminator, output 'year_beg' and 'year_end'
  # if 20xx, output 'year_beg' and 'year_end', where year_beg = year_end - 1
  # Currently all years have to be same format
  # check if all elements of 'years' contains a '/'
  years_backspace <- all(grepl("/", years))
  # contains four numbers and no other punctuation characters
  years_singular <- all(grepl("[[:digit:]]{4}", years) & !grepl("[[:punct:]]", years))
  if (years_backspace) {
    # split by deliminator
    years_df <- data.frame(do.call('rbind', strsplit(as.character(years_df), '/', fixed = TRUE)))
    # could use 'separate' here, but would need to transpose data frame first
    #df <- within(df, years <- dataFrame(do.call('rbind', strsplit(as.character(years), '/', fixed = TRUE))))
    years_df <- sapply(years_df, as.numeric)
    years_df[, 2] <- years_df[, 2] + 2000
    years_df <- as.data.frame(t(years_df))
  } else if (years_singular) {
    # split by deliminator
    years <- as.numeric(years_df)
    years_df <- rbind(years - 1, years)
  } else {
    stop("years not in consistent format 20xx/xx or 20xx")
  }
  rownames(years_df) <- c("year_beg", "year_end")
  # res
  return(years_df)
}
#}}}

#### tidy up statistics vector
.tidyStatistics <- function(statistics) {#{{{
  ## tidy up statistics vector

  # everyting lower case
  statistics <- tolower(statistics)

  # replace % with 'perc'
  statistics <- gsub("%", "perc", statistics)

  # remove brackets and everything within
  statistics <- gsub(r"{\s*\([^\)]+\)}","", statistics)

  # change spaces to unerscore '_'
  statistics <- gsub(" ","_", statistics)
  
  # 'se' appears twice, once for the % and once for the Difference
  # change s.t. the one relating to SE of Difference is renamed
  which_se <- which(statistics == "se")
  # which 'se' values relate to 'se' of Difference
  # every second 'se' is 'se' of Difference
  #nb, logical vectors are recycled when indexing
  which_se_difference <- which_se[c(FALSE, TRUE)]
  statistics[which_se_difference] <- "se_diff"
  
  # vector of unique statistics (not currently required)
  # statistics_unique <- unique(statistics)[!is.na(unique(statistics))]

  # res
  return(statistics)
}#}}}

#### function to tidy each spreadsheet independently
.tidyDataFrame <- function(dataFrame) {#{{{
  # input: data frame
  # output: tidy-er data frame

  # rename groups that appear > once
  #groups <- dataFrame[, 1]
  groups <- .duplicateGroups(groups = dataFrame[, 1])
  # insert new 'groups' into data frame
  dataFrame[, 1] <- groups
  
  # remove rows that are all na
  all_na_rows <- rowSums(is.na(dataFrame)) < ncol(dataFrame)
  dataFrame <- dataFrame[all_na_rows, ]
  
  # remove cols that are all na
  all_na_cols <- colSums(is.na(dataFrame)) < nrow(dataFrame)
  dataFrame <- dataFrame[, all_na_cols]
  
  # extract first row
  years <- unlist(dataFrame[1,], use.names = FALSE) 
  years_df <- .tidyYears(years)
  
  # replace years row of df with the two new rows
  colnames(years_df) <- colnames(dataFrame)
  dataFrame <- rbind(years_df, dataFrame[-1, ])
  
  # extract statistics and tidy
  statistics <- .tidyStatistics(dataFrame[3, ])
  
  # change them in the data_frame
  dataFrame[3, ] <- statistics
  
  # merge variables and levels together
  # to help with 'melt' later
  col_names = paste(dataFrame[, 1], dataFrame[, 2], sep = "_")[-(1:3)]

  # tidying up data frame
  dataFrame <- as.data.frame(t(dataFrame))
  colnames(dataFrame) <- c("year_beg", "year_end", "statistic", col_names)
  # remove column with 'END' in
  dataFrame <- dataFrame[, -dim(dataFrame)[2]]
  # remove top two rows with variables/ levels
  dataFrame <- dataFrame[-(1:2), ]
  # remove rows which only containt significance stars '**'
  star_rows <- rbind(which(dataFrame == "*", arr.ind = TRUE), 
                     which(dataFrame == "**", arr.ind = TRUE), 
                     which(dataFrame == "***", arr.ind = TRUE))
  dataFrame <- dataFrame[-unique(star_rows[, 1]), ]
  # set years cols to be numeric
  dataFrame[, 1:2] <- sapply(dataFrame[, 1:2], as.numeric)
  # set cols of data to be dumeric
  # nb. warning here is good:
  #   changing the '-' empty elements to NA
  dataFrame[, 4:dim(dataFrame)[2]] <- sapply(dataFrame[, 4:dim(dataFrame)[2]], as.numeric)
  
  # melt
  df_melt <- reshape2::melt(dataFrame, id = c("year_beg", "year_end", "statistic"), na.rm = FALSE)
  
  # dcast to make statistics columns
  # use 'factor' o.w. columns are ordered alphabetically
  # https://stackoverflow.com/questions/52190674/order-columns-in-dcast
  df_dcast <- reshape2::dcast(df_melt, year_beg + year_end + variable
                    ~ factor(statistic, levels = unique(statistic)))
  
  # split variables and levels into two columns
  # use 'separate' from 'tidyr'
  #df_dcast <- within(df_dcast,
  #variable <- dataFrame(do.call('rbind',
  #strsplit(as.character(variable),
  #'_', fixed = TRUE)))
  df_dcast <- tidyr::separate(data = df_dcast, col = variable, into = c("variable", "level"), sep = '_')
  
  # res
  df_dcast
}

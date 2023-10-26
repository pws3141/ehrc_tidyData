### Utility functions  for 'tidyData'

# change name of groups so that duplicated groups are distinct
# preend '(x categories)' to end 
.duplicateGroups <- function(groups) {#{{{
  # Aim: find duplicated groups, separated by NA
  #     rename these groups by appending a number to duplicates
  
  # In: vector of group names
  # Out: vector of new group names
  
  # list of index values which 'groups' is NA
  groups_NA_index <- which(is.na(groups))
  # index_vector initialisation
  index_vector <- rep(1, length.out = length(groups))
  # unique number initialisation
  number_vector <- 1:length(groups_NA_index)
  # create unique index for every group
  for (i in 1:length(groups_NA_index)) {
          number_from <- groups_NA_index[i]
          if (i == length(groups_NA_index)) {
                  number_to <- length(groups)
          } else {
                  number_to <- groups_NA_index[i + 1] - 1
          }
          # replace index vector with new number
          index_vector[number_from:number_to] <- number_vector[i]
  }
  # create new grouping names appended by unique index number
  for (i in 1:length(groups)) {
        if (!is.na(groups[i])) {
                    groups[i] <- paste(groups[i], index_vector[i],
                                            sep = "_")
        }
  }
  # vector of unique groups
  groups_unique <- na.omit(unique(groups))
  # if more than 2 category in group, then change to
  # 'group (x categories)'
  for (group in groups_unique) {
        which_group <- which(groups %in% group)
        # remove underscore and index value
        group_new <- gsub("_[[:digit:]]+$", "", group)
        if (length(which_group) > 2) {
                # append "(x categories)"
                categories <- sprintf("(%d categories)", 
                                      length(which_group))
                group_new <- paste(group_new, categories)
        }
        # replace names in original 'groups' vector
        groups[which_group] <- group_new
  }

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
    years_df <- sapply(years_df, as.numeric)
    # if in format 2011/12, then need to 'add 2000' to the '12' part
    # if in format 2011/2012, then can leave alone
    if (all(years_df[, 2] < 2000, na.rm = TRUE)) years_df[, 2] <- years_df[, 2] + 2000
    years_df <- as.data.frame(t(years_df))
  } else if (years_singular) {
    # split by deliminator
    years <- as.numeric(years_df)
    # assume each data point is a one year period
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
  # if statistic is 'Per_100' replace with 'perc'
  # if 'Per_1000', 'Per_10000' etc, leave along
  statistics <- gsub("(?i)per_?100$", "perc", statistics)

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
.tidyDataFrame <- function(df) {#{{{
  # input: data frame
  # output: tidy-er data frame

  # rename groups that appear > once
  groups <- .duplicateGroups(groups = df[, 1])
  # insert new 'groups' into data frame
  df[, 1] <- groups
  
  # replace unnecessary  punctuation and words with 'NA'
  # first, remove any leading or trailing space
  ## trims leading and trailing whitespace from all character columns 'df'
  ## without modifying other columns that are not of character type.
  cols_characters <- which(vapply(df, is.character, logical(1)) == TRUE)
  df[, cols_characters] <- lapply(df[, cols_characters], stringr::str_trim)

  # now, remove punct and words 
  to_remove <- c("-", "'", "*", "**", "***", ".", "END", "Stars", "Blank")
  for (rem in to_remove) {
          which_ind <- which(df == rem, arr.ind = TRUE)
          df[which_ind] <- NA
  }

  # remove rows that are all na
  all_na_rows <- rowSums(is.na(df)) < ncol(df)
  df <- df[all_na_rows, ]
  
  # remove cols that are all na
  all_na_cols <- colSums(is.na(df)) < nrow(df)
  df <- df[, all_na_cols]
  
  # extract first row
  years <- unlist(df[1,], use.names = FALSE) 
  years_df <- .tidyYears(years)
  
  # replace years row of df with the two new rows
  # 'year_beg' and 'year_end'
  colnames(years_df) <- colnames(df)
  df <- rbind(years_df, df[-1, ])
  
  # extract statistics and tidy
  statistics <- .tidyStatistics(df[3, ])
  
  # change them in the data_frame
  df[3, ] <- statistics
  
  # merge variables and levels together
  # to help with 'melt' later
  col_names = paste(df[, 1], df[, 2], sep = "_")[-(1:3)]

  # tidying up data frame
  df <- as.data.frame(t(df))
  colnames(df) <- c("year_beg", "year_end", "statistic", col_names)

  # remove top two rows with variables/ levels
  df <- df[-(1:2), ]


  # set years cols to be numeric
  df[, 1:2] <- sapply(df[, 1:2], as.numeric)

  # set cols of data to be numeric
  df[, 4:dim(df)[2]] <- sapply(df[, 4:dim(df)[2]], as.numeric)
  
  # melt
  df_melt <- reshape2::melt(df, id = c("year_beg", "year_end", "statistic"), na.rm = FALSE)
  
  # dcast to make statistics columns
  # use 'factor' o.w. columns are ordered alphabetically
  # https://stackoverflow.com/questions/52190674/order-columns-in-dcast
  df_dcast <- reshape2::dcast(df_melt, year_beg + year_end + variable
                    ~ factor(statistic, levels = unique(statistic)))
  
  # split variables and levels into two columns
  # use 'separate' from 'tidyr'
  df_dcast <- tidyr::separate(data = df_dcast, col = variable, into = c("variable", "level"), sep = '_')
  
  # res
  df_dcast
}

.tidyDataClean <- function(df) {
        # remove rows where SE / point estimate > 0.5,
        # or where SE does not exist

        # se column is consistent, but sometimes point estimate is percentage,
        # sometimes something else, e.g. per_1000, or £
        # but it is always the column that preceeds 'se',
        # so find 'se' column to find point estimate column
        which_col_se <- which(names(df) == "se")
        # find which se / point_estimate > 0.5, or NA
        error_prop <- df[which_col_se] / df[(which_col_se - 1)]
        to_keep <- which(!(is.na(error_prop) | error_prop > 0.5))
        # keep rows
        df <- df[to_keep, ]
        return(df)
}

# Overview of package changes
This file documents the major changes done to the functions between each package
update.

## From 0.1 -> 0.2:

* added DESCRIPTION file
* Changes to '.duplicateGroups' function: changed from appending '_X' to duplicate groups to appending '(X categories)' to all groups with more than one level.
    * using 'gsub' instead of 'seperate' or 'plyr::count' to remove package dependencies
* Changes to cleaning data.frame:
    * removing more punctuation and unnecessary works (e.g. 'END') closer to the
    beginning of the function
    * as cleaning done earlier, no warnings output for 'as.numeric' function, due to
    the "-" being removed

## From 0.2 -> 0.3

* if statistic is 'Per_100' or similar, change to 'perc'
* added 'tidyDataMerge' function to merge the list of seperate data.frames that our
returned from 'tidyData'
* Removed 'Table X' from name of each list element in 'tidyData', and from output worksheet
name

## From 0.3 -> 0.4
* added ability to merge sheets with the same name, for when we have different
worksheets for excl_SO and incl_SO data
    * nb: for this to work, require no duplicate levels so manual editing of data
    tables is required.
* truncated worksheet name in 'tidyDataMerge' if >31 characters, to avoid error in 'write_xlsx'

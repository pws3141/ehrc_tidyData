# Overview of package changes
This file documents the major changes done to the functions between each package
update.

## From 0.1 -> 0.2:

* added DESCRIPTION file
* Changes to '.duplicateGroups' function: changed from appending '_X' to duplicate groups to appending '(X categories)' to all
groups with more than one level.
    * using 'gsub' instead of 'seperate' or 'plyr::count' to remove package dependencies
* Changes to cleaning data.frame:
    * removing more punctuation and unnecessary works (e.g. 'END') closer to the
    beginning of the function
    * as cleaning done earlier, no warnings output for 'as.numeric' function, due to
    the "-" being removed





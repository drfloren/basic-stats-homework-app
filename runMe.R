list.of.packages <- c("shiny", "toOrdinal", "qboxplot", "DT", "R.utils")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
shiny::runGitHub("mth115-homework", username="mis-drfloren", launch.browser=TRUE)

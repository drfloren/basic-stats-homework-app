list.of.packages <- c("htmltools", "shiny", "toOrdinal", "qboxplot", "DT", "R.utils", "aplpack", "xtable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
shiny::runGitHub("basic-stats-homework-app", username="drfloren", launch.browser=TRUE)

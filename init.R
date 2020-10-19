# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("forecast", "numberFormattR", "ggfortify", "shinydashboard", "plotly", "data.table", "lubridate", "tibble", "jsonlite", "httr")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")
devtools::install_github('mraess/numberFormattR', force=TRUE)

invisible(sapply(my_packages, install_if_missing))

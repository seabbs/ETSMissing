#install.packages("devtools")
library(devtools)

# init stuffs
use_data_raw()
use_cran_badge()
use_mit_license()
use_readme_rmd()
use_testthat()
use_travis()
use_roxygen_md()


#Suggest
use_package("pkgdown", type = "Suggests")
use_package("devtools", type = "Suggests")
use_package("pomp", type = "Suggests")
#Import
use_package("tidyverse", type = "Imports")
use_package("tbinenglanddataclean", type = "Imports")
use_package("prettypublisher", type = "Imports")
use_package("ggplot2", type = "Imports")
use_package("purrr", type = "Imports")

#Vignettes
use_vignette("analysis_plan")
use_vignette("paper")
use_vignette("si")

##Build site, and make pkgdown
devtools::document()
devtools::build_vignettes()
devtools::build()
pkgdown::build_site()

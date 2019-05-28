.libPaths(c("~/R/x86_64-pc-linux-gnu-library/3.4", .libPaths()))

setwd("PosteriorBootstrap")

devtools::install_deps(dependencies = TRUE)
devtools::document()
devtools::check()
devtools::build()
#devtools::test()
devtools::install(reload = T, quick = T)
#devtools::build_vignettes()
#print(getwd())
#library("PosteriorBootstrap")
source('run-on-azure/gauge-n-centering-model-samples.R')

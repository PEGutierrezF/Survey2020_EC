

libraries <- c("xtable", "ggplot2", "likert", "plyr", "psych",
               "vctrs", "sjmisc", "pkgload", "sjPlot", "xlsx", 
               "writexl",'reshape2', "forcats", "dplyr", 'patchwork',
               'RColorBrewer', 'dplyr')

lapply(libraries, require, character.only = TRUE)

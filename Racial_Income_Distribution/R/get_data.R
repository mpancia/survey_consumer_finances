##%######################################################%##
#                                                          #
####                 Get 2016 SCF Data                  ####
#                                                          #
##%######################################################%##

library(lodown)
library(here)
data_directory <- paste(dirname(here()), "data", sep = "/")
scf_cat <- get_catalog( "scf" , output_dir = data_directory)
scf_cat <- subset( scf_cat , year == 2016 )
lodown( "scf" , scf_cat)

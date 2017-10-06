##%######################################################%##
#                                                          #
####                 Get SCF Data                  ####
#                                                          #
##%######################################################%##

library(lodown)
library(here)
data_directory <- paste(dirname(here()), "data", sep = "/")

scf_cat <- get_catalog( "scf" , output_dir = data_directory) %>%
  mutate(rw_exists = file.exists(rw_filename),
         exists = file.exists(output_filename)) %>%
  filter((!rw_exists) | (!exists))

if(nrow(scf_cat) > 0) {
  lodown( "scf" , scf_cat)
}

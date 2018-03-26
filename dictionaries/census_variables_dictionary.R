# This script will generate a spreadsheet that will then be manually edited to create a data dictrionary for the 
# census variables, enabling us to keep the union of the set of all census variables
library(rowr)

# we'll use 2006 data as our reference year and coerce all other data sets to that
# load data
temp_2001 <- read_csv(paste0('data/census_data/2001_census.csv'))
temp_2006 <- read_csv(paste0('data/census_data/2006_census.csv'))
temp_2011 <- read_csv(paste0('data/census_data/2011_census.csv'))
temp_2016 <- read_csv(paste0('data/census_data/2016_census/census_2016_1.csv'))

# get 2001 data first
temp_2001 <- as.data.frame(cbind.fill(colnames(temp_2001),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2001) <- c('old_2001', 'old_2006')
# write.csv(temp_2001, 'dictionaries/temp_2001.csv')

# get 2011 data first
temp_2011 <- as.data.frame(cbind.fill(colnames(temp_2011),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2011) <- c('old_2011', 'old_2006')
# write.csv(temp_2011, 'dictionaries/temp_2011.csv')


# get 2016 data first
temp_2016 <- as.data.frame(cbind.fill(colnames(temp_2016),
                                      colnames(temp_2006),
                                      fill = NA))
colnames(temp_2016) <- c('old_2016', 'old_2006')
# write.csv(temp_2016, 'dictionaries/temp_2016.csv')


# first get vector of data set names to loop through later
data_names <- list.files('data/census_data')
# get 2001, 2011, and 2016 dictionaries (2006 is the base reference)
dict_folder <- list.files('dictionaries/census_variables_dict')
census_vars_dict <- list()
# loop through and load all variable dictionaries 
for(i in 1:length(dict_folder)){
  dict_name <- dict_folder[i]
  census_vars_dict[[i]] <-  read_csv(paste0('dictionaries/census_variables_dict/', dict_name))
}


temp_2001 <- census_vars_dict[[1]]
temp_2011 <- census_vars_dict[[2]]
temp_2016 <- census_vars_dict[[3]]
temp_2006 <- as.data.frame(colnames(temp_2006))

# 2001 
temp_full <- temp_2001

not_1 <-temp_full$new_2001[!temp_full$new_2001 %in% temp_2016$new_2016]
not_2 <- temp_full$new_2001[!temp_full$new_2001 %in% temp_2011$new_2011]
not_3 <- temp_full$new_2001[!temp_full$new_2001 %in% temp_2006$`colnames(temp_2006)`]

temp_not_union <- Reduce(union, list(not_1, not_2, not_3))
remove_this <- Reduce(intersect, list(not_1, not_2, not_3))

left_out <- temp_not_union[!temp_not_union %in% remove_this]

# all of 2016 is in 2011
all(temp_2016$new_2016 %in% temp_2011$new_2011)
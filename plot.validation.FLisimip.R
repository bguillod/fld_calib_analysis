# test reading in a csv file and doing some analysis
# library(ggplot2)
# library(RColorBrewer)
# library(tidyverse)
source("init.R")

# this should not be necessary if defined as an R project
# setwd(WD_PATH_FLDCAL)

source("validation_isimipFL_module_load.R")
source("validation_isimipFL_module_process.R")
source("validation_isimipFL_module_plot.R")

# inputs:
# continent
continent_names <- get_regions_list()
continent_name <- "EUR"
# cost function
cost_funs <- get_costFunctions_list()
cost_fun <- "dlog2"
# TODO other parameters of calibration:
calib_methods <- get_calib_full_list()
# anything else is constant and default values
# So, now choose a name for each damage function calibration strategy.



# load data
data_steps <- load_calib_steps(continent_name,"patternsearch",cost_fun)
pars_calib <- load_calib_params(continent_name,"patternsearch",cost_fun)
data_calib <- load_calib_eval(continent_name,"patternsearch",cost_fun) %>% mutate(damage_fun=cost_fun)
data_JRC <- load_JRC_eval(continent_name) %>% mutate(used_in_calibration=F, damage_fun="JRC")
data <- bind_rows(data_calib, data_JRC)

# add data for regional sum
data <- add_regional_sum(data, continent_name)

# simple plots
# TODO implement line type or so as a function of damage_fun
test <- plot_yearly_line(data, show=T)
test <- plot_yearly_boxplot(data, show=T)
test <- plot_yearly_scatter(data, show=T)
test <- plot_yearly_env(data, show=T)
test <- plot_AED_scatter(data, show=T)



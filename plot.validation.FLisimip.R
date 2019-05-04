# test reading in a csv file and doing some analysis
source("init.R")

# this should not be necessary if defined as an R project
# setwd(WD_PATH_FLDCAL)

source("validation_isimipFL_module_load.R")
source("validation_isimipFL_module_process.R")
source("validation_isimipFL_module_plot.R")
source("validation_isimipFL_module_helpers.R")
# source("validation_isimipFL_module_damFun.R")

# inputs:
# continent
region_names <- get_regions_list()
region_name <- "EUR"
# All varying parameters of calibration:
calib_methods <- get_calib_methods_tibble()

# to call a function for a given calib_method, call:
# call_fun_by_calibMethod(calib_method, FUN, ...)
# e.g.
# data_steps <- call_fun_by_calibMethod('DFC', load_calib_steps, regionID=region_name)

# example here:
data <- load_all_evals_one_region(region_name)
# NOW ALL DATA ARE LOADED FOR ONE REGION.

# --------------------------------------------------------------------
# 1) Compare regional sums only
data_all_regsums <- foreach(rn=region_names,.combine=rbind) %do% {
    load_all_evals_one_region(rn, regSum_only = T)
}
# calibrated years:
data_rt <- data_all_regsums %>% filter(used_in_calibration==TRUE) %>% group_by(country, dataset, damage_source) %>% mutate(rank=min_rank(desc(damage)),ny=n())
data_rt <- data_rt %>% mutate(`Return time [yr]`=(ny+1)/(rank))
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
test <- ggplot(data_rt %>% group_by(`Return time [yr]`, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=`Return time [yr]`,y=damage))+
    # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
    geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10")
# all years:
data_rt <- data_all_regsums %>% group_by(country, dataset, damage_source) %>% mutate(rank=min_rank(desc(damage)),ny=n())
data_rt <- data_rt %>% mutate(`Return time [yr]`=(ny+1)/(rank))
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
test <- ggplot(data_rt %>% group_by(`Return time [yr]`, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=`Return time [yr]`,y=damage))+
    # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
    geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10")



# --------------------------------------------------------------------
# 2) BY COUNTRY - check selection and loading of used_in_calibration - maybe select by years rather?
data_sub <- data %>% filter(country %in% c("BEL","DEU", "CHE","FRA"))
test <- ggplot(data_sub %>% group_by(year, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=year,y=damage))+
    geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.2) +
    facet_wrap(.~country, scales="free_y") +
    scale_y_continuous(trans="log10")


# get return time curve
data_rt <- data %>% filter(used_in_calibration==TRUE) %>% group_by(country, dataset, damage_source) %>% mutate(rank=min_rank(desc(damage)),ny=n())
data_rt <- data_rt %>% mutate(`Return time [yr]`=(ny+1)/(rank))
data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
test <- ggplot(data_sub %>% group_by(`Return time [yr]`, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=`Return time [yr]`,y=damage))+
    # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
    geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10")

# add data for regional sum
data <- add_regional_sum(data, continent_name)

# simple plots
# TODO implement line type or so as a function of damage_fun
test <- plot_yearly_line(data, show=T)
test <- plot_yearly_boxplot(data, show=T)
test <- plot_yearly_scatter(data, show=T)
test <- plot_yearly_env(data, show=T)
test <- plot_AED_scatter(data, show=T)



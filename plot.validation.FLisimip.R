# test reading in a csv file and doing some analysis
source("init.R")

# this should not be necessary if defined as an R project
# setwd(WD_PATH_FLDCAL)

source("validation_isimipFL_module_load.R")
source("validation_isimipFL_module_process.R")
source("validation_isimipFL_module_plot.R")
source("validation_isimipFL_module_metrics.R")
source("validation_isimipFL_module_helpers.R")
# source("validation_isimipFL_module_damFun.R")

# output directory for figures
figs_out_path <- file.path(FIGS_PATH, "eval_plots")
if (!dir.exists(figs_out_path)) dir.create(figs_out_path)

# to be able to plot damages as log(x+1) (i.e. 0 for 0, 10 for 10**10)
# library(scales)
# log10p1_trans <- function() scales::trans_new("log10p1", function(x) log10(x+1), function(x) 10**(x)-1, breaks=function(x) log_breaks()(x+1))#, format=function(brk) ifelse(brk==1, "1",format(brk, scientific=T)))
# NOT NESSARY, because ggplot still shows all points in the plot. should be documented in the paper.



# inputs:
# continent
region_names <- get_regions_list()
# region_name <- "EUR"
# All varying parameters of calibration:
calib_methods <- get_calib_methods_tibble()

# to call a function for a given calib_method, call:
# call_fun_by_calibMethod(calib_method, FUN, ...)
# e.g.
# data_steps <- call_fun_by_calibMethod('DFC', load_calib_steps, regionID=region_name)

# example here:
# data <- load_all_evals_one_region(region_name)
# NOW ALL DATA ARE LOADED FOR ONE REGION.



# For validation of the calibration procedure, only use countries and years which are used in calibration, because these have reasonable damage data (only exception: yearly time series as on these it is easy to see filter years by eye, so there only calibrated countries but all years):

# OK (was a TODO but solved below). load the list of countries retained within each region, then load data only for those countries and years 1992-2010, then compute regional sums per year. Do all analysis with only these data.
data_all_countries <- foreach(rn=region_names,.combine=rbind) %do% {
    load_all_evals_one_region(rn, regSum_only = F)
}
countries_for_validation <- data_all_countries %>% filter(dataset=='CLM_gswp3',nchar(country)==3,damage_source=='DFC',used_in_calibration==T) %>% select(country) %>% unique() %>% arrange(country)
# so always retain only those countries!
# check countries
in_calib_ctrs <- read_csv(file.path(FIGS_PATH, "../data_small/NatID_RegID_isimip_flood_filtered_1992-2010.csv")) %>% filter(in_calib==2,!Reg_name %in% c("PIS1","PIS2"),!is.na(Reg_name)) %>% mutate(country=ISO)
# 
if (!all.equal(countries_for_validation,in_calib_ctrs %>% select(country=ISO))) {
    stop("** ERROR ** issue in country selection")
}

# first, filter out countries
data_calib_allyears <- data_all_countries %>% filter(country %in% (countries_for_validation %>% flatten_chr))

# compute regional sum
data_calib_regSum_allyears <- data_calib_allyears %>% group_by(region, year,dataset,damage_source) %>%
    summarise(damage=sum(damage)) %>%
    ungroup() %>%
    mutate(country=paste0("ALL (",region,")"), used_in_calibration=TRUE)

# add regional sum to data
data_calib_country_allyears <- data_calib_allyears
data_calib_allyears <- data_calib_allyears %>%
    bind_rows(data_calib_regSum_allyears)

# Second, filter out years
data_calib_country <- data_calib_country_allyears %>% filter(year >= 1992)
data_calib_regSum <- data_calib_regSum_allyears %>% filter(year >= 1992)
data_calib <- data_calib_allyears %>% filter(year >= 1992)

# Third, add return times (only for those with years filtered out)
data_calib_country <-  data_calib_country %>% compute_return_times()
data_calib_regSum <-  data_calib_regSum %>% compute_return_times()
data_calib <-  data_calib %>% compute_return_times()

# So now we have our 6 datasets:
# a) with only the calibrated years:
# - data_calib_country: data for all countries
# - data_calib_regSum: data for regional sums
# - data_calib: all data together
# b) with all years (for time series plots only):
# same with *_allyears appended to them


# --------------------------------------------------------------------
# 1) Compare regional sums only
# subset with calibrated years only, with return times
# data_calibYears_regsums <- foreach(rn=region_names,.combine=rbind) %do% {
#         load_all_evals_one_region(rn, regSum_only = T, calib_only=T)
#     } %>% mutate(country=substr(country,6,8))
# data_calibYears_regsums <- data_calibYears_regsums %>% compute_return_times()
data_calibYears_regsums <- data_calib_regSum
# BUT ISSUE: for regional sum, we want two values:
# (1) For each year, summing only the countries that are included in calibration FOR THAT YEAR (also for EM-DAT!)
# (2) All countries for the region.
# For calibration methods with 'exclude_years_0totals' set to FALSE, point (1) above is what is done...
# But for those with 'exclude_years_0totals' set to TRUE, it is more complicated as one should filter by !!!


# first, compute metrics
# note that for validation purpose, it makes sense to only keep countries and years of calibration (as data is considered not robust otherwise)
rmse_abs <- rmse_of_yearly_by_country(data_calibYears_regsums)
rmse_log <- rmse_of_yearly_by_country(data_calibYears_regsums %>% mutate(damage=pmax(1,damage)) %>% mutate(damage=log10(damage)))
# test2 <- aed_bias(data_calibYears_regsums)
# plot min, max and mean of rmse of all models (except MMM)
output_abs <- rmse_abs %>% filter(dataset!="MMM") %>%
    group_by(country,damage_source) %>%
    summarise(RMSE_mean=mean(rmse),RMSE_min=min(rmse),RMSE_max=max(rmse)) %>%
    ungroup() %>%
    gather(RMSE_mean,RMSE_max,RMSE_min,key="what",value="RMSE")
# ggplot(output_abs,aes(fill=RMSE,x=country,y=damage_source))+geom_raster()+scale_fill_continuous(trans="log10")+facet_wrap(.~what, nrow=5, scales="free_y")+xlab("Region")+ylab("damage source")
# maybe better: plot relative difference to JRC MMM?
get_metric_diff <- function(data) {
    jrc_dat <- data %>% filter(damage_source=='JRC')
    dat_diff <- data %>% filter(damage_source!='JRC') %>%
        left_join(jrc_dat, by=c("country","what")) %>%
        mutate(RMSE_diff=RMSE.x-RMSE.y, RMSE_rel_diff=100*RMSE_diff/RMSE.y,what=paste0(what,"_diff")) %>%
        rename(damage_source=damage_source.x, RMSE=RMSE.x, RMSE_JRC=RMSE.y) %>%
        select(-damage_source.y)
    return(dat_diff)
}
output_abs_diff <- get_metric_diff(output_abs %>% filter(what == "RMSE_mean")) %>% mutate(region=substr(country,6,8))
ggplot(output_abs_diff %>% filter(what=="RMSE_mean_diff"),aes(fill=RMSE_rel_diff,x=region,y=damage_source))+geom_raster()+scale_fill_continuous(type="viridis",name="Difference in RMSE relative to JRC [%]",direction=-1)
# adding average of all regions
output_abs_diff <- output_abs_diff %>%bind_rows(
    output_abs_diff %>% group_by(damage_source, what) %>% summarise(RMSE_rel_diff=mean(RMSE_rel_diff)) %>% ungroup() %>% add_column(region='ALL')
)
ggplot(output_abs_diff %>% filter(what=="RMSE_mean_diff"),aes(fill=RMSE_rel_diff,x=region,y=damage_source))+geom_raster()+scale_fill_continuous(type="viridis",name="Difference in RMSE relative to JRC [%]",direction=-1)
jitter <- position_jitter(width = 0.1, height = 0)

ggplot(output_abs_diff %>% filter(what=="RMSE_mean_diff"),aes(col=damage_source,x=region,y=RMSE_rel_diff))+geom_point(cex=2,pch=20)+ylab("Difference in RMSE relative to JRC [%]")


data <- output_abs_diff %>%
    filter(what=='RMSE_mean_diff') %>%
    select(-RMSE,-RMSE_JRC,-RMSE_diff) %>%
    spread(what,RMSE_rel_diff) %>%
    rename(y=RMSE_mean_diff, ymin=RMSE_min_diff,ymax=RMSE_max_diff)
ggplot(data,aes(x=region,colour=damage_source))+#,shape=what))+
    # ggplot(output_abs_diff,aes(y=RMSE_rel_diff,x=region,colour=damage_source))+#,shape=what))+
    geom_pointrange(aes(y=y,ymin=ymin,ymax=ymax),shape=21) +
    ylab("Difference in RMSE relative to JRC [%]")+
    labs(color="Damage source")





# MOST LIKELY we want to see the difference in metrics for MMM:
# TODO
get_metric_diff <- function(data) {
    jrc_dat <- data %>% filter(damage_source=='JRC')
    dat_diff <- data %>% filter(damage_source!='JRC') %>%
        left_join(jrc_dat, by=c("country","dataset")) %>%
        mutate(RMSE_diff=rmse.x-rmse.y, RMSE_rel_diff=100*RMSE_diff/rmse.y) %>%
        rename(damage_source=damage_source.x, RMSE=rmse.x, RMSE_JRC=rmse.y) %>%
        select(-damage_source.y)
    return(dat_diff)
}
rmse_abs <- rmse_of_yearly_by_country(data_calibYears_regsums) %>% filter(dataset=='MMM') %>% get_metric_diff
rmse_log <- rmse_of_yearly_by_country(data_calibYears_regsums %>% mutate(damage=pmax(1,damage)) %>% mutate(damage=log10(damage))) %>% filter(dataset=='MMM') %>% get_metric_diff

# first, keep only metric for MMM
rmse_all <- bind_rows(rmse_abs %>% mutate(var="RMSE of actual damages"),
                      rmse_log %>% mutate(var = "RMSE of log10 damages")) %>%
    mutate(region=substr(country,6,8))
# rmse_all <- left_join(rmse_abs,rmse_log,by=c("country","damage_source",'dataset')) %>% rename(RMSE_ABS_diff=RMSE_diff.x,RMSE_LOG10_diff=RMSE_diff.y,RMSE_ABS_rel_diff=RMSE_rel_diff.x,RMSE_LOG10_rel_diff=RMSE_rel_diff.y) %>%
    
# rmse_all <- rmse_all %>% filter(dataset=='MMM')
ggplot(rmse_all, aes(x=region,y=RMSE_rel_diff,col=damage_source)) +
    geom_point(pch=21) +
    facet_wrap(var~.,ncol=1)
# TODO (here I am)












# a) return time plot, calibrated years
data_calibYears_regsums <- data_calib_regSum
data_all_regsums_emdat <- data_calib_regSum %>% filter(dataset == "EM-DAT")

temp <- ggplot(data_calibYears_regsums %>% filter(dataset != "EM-DAT") %>% group_by(rt, region, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=rt,y=damage))+
    geom_line(aes(y=y,color=damage_source))+
    geom_point(data = data_calib_regSum %>% filter(dataset == "EM-DAT"),
               aes(x=rt,y=damage), col="black", size=0.5) +
    facet_wrap(.~region, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
    xlab("Return period [yr]") + ylab("damage [2005 USD]") +
    labs(color="Damage function")+
    ggtitle("Damage frequency curves by region (calibrated countries, 1992-2010)")

pdf(file = file.path(figs_out_path, "DFC_MMM_calib_all_regions.pdf"), height=8, width=7)
temp
dev.off()


# b) return time plot, all years:
# temp <- ggplot(data_all_regsums %>% filter(dataset != "EM-DAT") %>% group_by(rt, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
#                aes(x=rt,y=y))+
#     geom_line(aes(y=y,color=damage_source))+
#     geom_point(data = data_all_regsums %>% filter(dataset == "EM-DAT"),
#                aes(x=rt,y=damage), col="black", size=0.5) +
#     facet_wrap(.~country, nrow=5, scales="free_y") +
#     scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10")+
#     xlab("Return period [yr]") + ylab("damage [USD]") +
#     labs(color="Damage source")+
#     ggtitle("Damage frequency curves by region (all years, 1971-2010)")
# 
# pdf(file = file.path(figs_out_path, "DFC_MMM_calib_all_regions_allyears.pdf"), height=8, width=7)
# temp
# dev.off()

# c) yearly damages, calibrated years - not used
# temp <- ggplot(data_calibYears_regsums %>% group_by(year, region, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
#                aes(x=year,y=y))+
#     geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.16) +
#     geom_line(aes(y=y,color=damage_source))+
#     facet_wrap(.~region, nrow=5, scales="free_y") +
#     scale_y_continuous(trans="log10")+#+scale_x_continuous(trans="log10") +
#     xlab("Return period [yr]") + ylab("damage [USD]") +
#     labs(color="Damage source", fill="Damage source")+
#     ggtitle("Yearly damages by region (calibrated countries, 1992-2010)")
# 
# pdf(file = file.path(figs_out_path, "YD_calib_all_regions.pdf"), height=8, width=7)
# temp
# dev.off()


# d) yearly damages, all years
data_all_regsums <- data_calib_regSum_allyears
data_all_regsums_emdat <- data_all_regsums %>%
    filter(dataset == "EM-DAT") %>%
    mutate(isna=is.na(damage),
           damage = ifelse(is.na(damage),0,damage))
temp <- ggplot(data_all_regsums %>% filter(dataset != "EM-DAT") %>% group_by(year, region, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),# %>% mutate(y=na_if(y,0)),
               aes(x=year,y=y)) +
    # geom_vline(data=data.frame(y=1991.5),aes(xintercept=y)) +
    geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.16) +
    geom_line(aes(y=y,color=damage_source)) +
    geom_point(data = data_all_regsums_emdat %>% filter(isna),
               aes(x=year,y=damage), col="red") +
    geom_point(data = data_all_regsums_emdat %>% filter(damage==0 & !isna),
               aes(x=year,y=damage), col="black") +
    geom_point(data = data_all_regsums_emdat %>% filter(damage>0),
               aes(x=year,y=damage), col="black", size=0.5) +
    facet_wrap(.~region, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+
    xlab("Year") + ylab("damage [USD]") +
    labs(color="Damage function",fill="Damage function")+
    ggtitle("Yearly damages by region")+ 
    annotate("rect", xmin=-Inf, xmax=1991.5, ymin=0, ymax=Inf, alpha=0.4, fill="grey")

pdf(file = file.path(figs_out_path, "YD_calib_all_regions_allyears.pdf"), height=8, width=7)
temp
dev.off()



# --------------------------------------------------------------------
# 2) BY COUNTRY - check selection and loading of used_in_calibration - maybe select by years rather?
# This only includes calibrated countries

# define which countries were included in calibration and which not. Easier than working with used_in_calibration.

# subset with calibrated years only, with return times

# return times for all years
data_all_countries <- data_calib_country
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
for (i in 1:length(region_names)) {
    data_sub <- data_all_countries %>%
        filter(region==region_names[i]) %>%
        group_by(rt, country, damage_source) %>%
        summarise(ymin=min(damage,na.rm=T),ymax=max(damage,na.rm=T),y=round(mean(damage,na.rm=T))) %>%
        mutate(ymin=ifelse(is.finite(ymin),ymin,NA),ymax=ifelse(is.finite(ymax),ymax,NA),y=ifelse(is.finite(y),y,NA)) %>%
        ungroup()
    # remove countries without data (all 0 or NA)
    countries_all <- unique(data_sub$country)
    countries_in <- countries_all[!sapply(countries_all, function(c) data_sub %>% filter(country==c) %>% select(y) %>% transmute(out=is.na(.) | (.==0)) %>% unlist() %>% all(.))]
    data_sub <- data_sub %>% filter(country %in% countries_in)
    # make countries factors with the whole region first
    cntr_names <- sort(unique(data_sub$country))
    cntr_names <- c(cntr_names[nchar(cntr_names)>3],cntr_names[nchar(cntr_names)==3])
    data_sub <- data_sub %>%
        mutate(country=factor(country, levels = cntr_names))
    temp <- ggplot(data_sub, aes(x=rt,y=y))+
        geom_ribbon(aes(ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.16) +
        geom_line(aes(y=y,color=damage_source))+
        facet_wrap(.~country, ncol=3, scales="free_y") +
        scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
        xlab("Return period [yr]") + ylab("damage [USD]") +
        labs(color="Damage source",fill="Damage source")+
        ggtitle(paste0("Region ",region_names[i],": Damage Frequency Curve by country (calibrated countries, 1992-2010)"))
    pdf(file = file.path(figs_out_path, paste0("DFC_calib_",region_names[i],".pdf")), height=8/5.5*(1.5+length(countries_in)%/%3), width=7)
    print(temp)
    dev.off()
    temp <- ggplot(data_sub, aes(x=rt,y=y))+
        geom_line(aes(y=y,color=damage_source))+
        facet_wrap(.~country, ncol=3, scales="free_y") +
        scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
        xlab("Return period [yr]") + ylab("damage [USD]") +
        labs(color="Damage source",fill="Damage source")+
        ggtitle(paste0("Region ",region_names[i],": Damage Frequency Curve by country (calibrated countries, 1992-2010)"))
    pdf(file = file.path(figs_out_path, paste0("DFC_MMM_calib_",region_names[i],".pdf")), height=8/5.5*(1.5+length(countries_in)%/%3), width=7)
    print(temp)
    dev.off()
}



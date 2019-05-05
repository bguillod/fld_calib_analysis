# test reading in a csv file and doing some analysis
source("init.R")

# this should not be necessary if defined as an R project
# setwd(WD_PATH_FLDCAL)

source("validation_isimipFL_module_load.R")
source("validation_isimipFL_module_process.R")
source("validation_isimipFL_module_plot.R")
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

# --------------------------------------------------------------------
# 1) Compare regional sums only
data_all_regsums <- foreach(rn=region_names,.combine=rbind) %do% {
    load_all_evals_one_region(rn, regSum_only = T)
} %>% mutate(country=substr(country,6,8))
# subset with calibrated years only, with return times
data_calibYears_regsums <- data_all_regsums %>% filter(used_in_calibration==TRUE) %>% compute_return_times()
# return times for all years
data_all_regsums <- data_all_regsums %>% compute_return_times()

# a) return time plot, calibrated years
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
temp <- ggplot(data_calibYears_regsums %>% group_by(rt, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=rt,y=damage))+
    # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
    geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
    xlab("Return period [yr]") + ylab("damage [USD]") +
    labs(color="Damage source")+
    theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Damage frequency curves by region (calibrated years, 1992-2010)") #+
    # scale_color_brewer(palette="Dark2")

pdf(file = file.path(figs_out_path, "DFC_MMM_calib_all_regions.pdf"), height=8, width=7)
temp
dev.off()

# envelopes
# temp <- ggplot(data_calibYears_regsums %>% group_by(rt, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
#                aes(x=rt,y=damage))+
#     # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
#     geom_smooth(aes(y=y,ymin=ymin,ymax=ymax,color=damage_source,fill=damage_source),stat="identity", alpha=0.2)+
#     facet_wrap(.~country, nrow=5, scales="free_y") +
#     scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
#     xlab("Return period [yr]") + ylab("damage [USD]") +
#     labs(color="Damage source",fill="Damage source")+
#     theme(legend.position="bottom", legend.box = "horizontal")


# b) return time plot, all years:
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
temp <- ggplot(data_all_regsums %>% group_by(rt, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=rt,y=damage))+
    # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
    geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10")+
    xlab("Return period [yr]") + ylab("damage [USD]") +
    labs(color="Damage source")+
    theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Damage frequency curves by region (all years, 1971-2010)") #+
pdf(file = file.path(figs_out_path, "DFC_MMM_calib_all_regions_allyears.pdf"), height=8, width=7)
temp
dev.off()

# c) yearly damages, calibrated years
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
temp <- ggplot(data_calibYears_regsums %>% group_by(year, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=year,y=damage))+
    geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.16) +
    # geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+#+scale_x_continuous(trans="log10") +
    xlab("Return period [yr]") + ylab("damage [USD]") +
    labs(color="Damage source", fill="Damage source")+
    theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Yearly damages by region (calibrated years, 1992-2010)") #+
# scale_color_brewer(palette="Dark2")

pdf(file = file.path(figs_out_path, "YD_calib_all_regions.pdf"), height=8, width=7)
temp
dev.off()


# d) yearly damages, all years
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
temp <- ggplot(data_all_regsums %>% group_by(year, country, damage_source) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage))%>%ungroup(),
               aes(x=year,y=damage))+
    geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.16) +
    # geom_line(aes(y=y,color=damage_source))+
    facet_wrap(.~country, nrow=5, scales="free_y") +
    scale_y_continuous(trans="log10")+#+scale_x_continuous(trans="log10") +
    xlab("Return period [yr]") + ylab("damage [USD]") +
    labs(color="Damage source")+
    theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Yearly damages by region (all years, 1971-2010)") #+
# scale_color_brewer(palette="Dark2")

pdf(file = file.path(figs_out_path, "YD_calib_all_regions_allyears.pdf"), height=8, width=7)
temp
dev.off()



# --------------------------------------------------------------------
# 2) BY COUNTRY - check selection and loading of used_in_calibration - maybe select by years rather?
data_all_countries <- foreach(rn=region_names,.combine=rbind) %do% {
    load_all_evals_one_region(rn, regSum_only = F)
}
# subset with calibrated years only, with return times
data_calibYears_countries <- data_all_countries %>% filter(used_in_calibration==TRUE) %>% compute_return_times()
# return times for all years
data_all_countries <- data_all_countries %>% compute_return_times()
# data_sub <- data_rt %>% filter(country %in% c("ALL (EUR)","DEU", "CHE","FRA"))
for (i in 1:length(region_names)) {
    data_sub <- data_all_countries %>%
        filter(region==region_names[i]) %>%
        group_by(rt, country, damage_source) %>%
        summarise(ymin=min(damage,na.rm=T),ymax=max(damage,na.rm=T),y=mean(damage,na.rm=T)) %>%
        mutate(ymin=ifelse(is.finite(ymin),ymin,NA),ymax=ifelse(is.finite(ymax),ymax,NA),y=ifelse(is.finite(y),y,NA)) %>%
        ungroup()
        # remove countries without data (all 0 or NA)
    countries_all <- unique(data_sub$country)
    countries_in <- countries_all[!sapply(countries_all, function(c) data_sub %>% filter(country==c) %>% select(y) %>% transmute(out=is.na(.) | (.==0)) %>% unlist() %>% all(.))]
    data_sub <- data_sub %>% filter(country %in% countries_in)
    temp <- ggplot(data_sub, aes(x=rt,y=y))+
        geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
        # geom_line(aes(y=y,color=damage_source))+
        facet_wrap(.~country, ncol=3, scales="free_y") +
        scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
        xlab("Return period [yr]") + ylab("damage [USD]") +
        labs(color="Damage source",fill="Damage source")+
        theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
        ggtitle(paste0("Region ",region_names[i],": Damage Frequency Curve by country (calibrated years, 1992-2010)")) #+
    pdf(file = file.path(figs_out_path, paste0("DFC_calib_",region_names[i],".pdf")), height=8/5.5*(1.5+length(countries_in)%/%3), width=7)
    print(temp)
    dev.off()
    temp <- ggplot(data_sub, aes(x=rt,y=y))+
        # geom_smooth(aes(color=damage_source,y=y,ymin=ymin,ymax=ymax,fill=damage_source),stat="identity", alpha=0.1) +
        geom_line(aes(y=y,color=damage_source))+
        facet_wrap(.~country, ncol=3, scales="free_y") +
        scale_y_continuous(trans="log10")+scale_x_continuous(trans="log10") +
        xlab("Return period [yr]") + ylab("damage [USD]") +
        labs(color="Damage source",fill="Damage source")+
        theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
        ggtitle(paste0("Region ",region_names[i],": Damage Frequency Curve by country (calibrated years, 1992-2010)")) #+
    pdf(file = file.path(figs_out_path, paste0("DFC_MMM_calib_",region_names[i],".pdf")), height=8/5.5*(1.5+length(countries_in)%/%3), width=7)
    print(temp)
    dev.off()
}



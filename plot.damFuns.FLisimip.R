# test reading in a csv file and doing some analysis
source("init.R")

# this should not be necessary if defined as an R project
source("validation_isimipFL_module_load.R")
source("validation_isimipFL_module_process.R")
source("validation_isimipFL_module_plot.R")
source("validation_isimipFL_module_damFun.R")

figs_out_path <- file.path(FIGS_PATH, "damFuns")
if (!dir.exists(figs_out_path)) dir.create(figs_out_path)

# inputs:
# continent
regions_names <- get_regions_list()
# cost function
cost_funs <- get_costFunctions_list()
# All varying parameters of calibration:
calib_methods <- get_calib_methods_tibble()


# --------------------------
# JRC
jrc_df <- get_damFuns_JRC()
# plot JRC damage functions
temp <- ggplot(jrc_df, aes(x=fld_dph,y=MDR))+geom_line(aes(color=continent))+
    coord_cartesian(xlim=c(0,8))+
    ggtitle("JRC damage functions")+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Flood height [m]")+ylab("MDR [-]")
pdf(file = file.path(figs_out_path, "JRC_damFuns.pdf"), height=5, width=7)
temp
dev.off()

# --------------------------
# calibrated
calib_damfuns <- get_all_calib_damFuns() %>% mutate(continent=sapply(regionID, regions_to_continent))
temp <- ggplot(calib_damfuns, aes(x=fld_dph,y=MDR))+
    geom_line(aes(color=regionID))+
    geom_line(data=jrc_df, color=1, size=0.5)+
    facet_grid(continent~calib_method)+
    theme(plot.title = element_text(hjust = 0.5))+
    xlab("Flood height [m]")+ylab("MDR [-]")
pdf(file = file.path(figs_out_path, "all_damFuns_calib_continent.pdf"), height=7, width=7)
temp
dev.off()

# combine them
damfuns_all <- calib_damfuns %>% bind_rows(jrc_df %>% add_column(calib_method="JRC", regionID=""))
# make sure the regionID is somewhat random
damfuns_all <- damfuns_all %>% group_by(continent) %>% mutate(countryid=as.character(dense_rank(regionID))) %>% ungroup()
temp <- ggplot(damfuns_all, aes(x=fld_dph,y=MDR)) +
    geom_line(aes(color=countryid, linetype=calib_method)) +
    facet_grid(continent~.)


# ggplot(calib_damfuns %>% group_by(continent) %>% mutate(countryid=as.character(dense_rank(regionID))) %>% ungroup(),
#        aes(x=fld_dph,y=MDR)) +
#     geom_line(aes(color=countryid, linetype=calib_method)) +
#     geom_line(data=jrc_df %>% add_column(calib_method="JRC"),aes(x=fld_dph,y=MDR,linetype=calib_method),color=1) +    facet_wrap(~continent) +
#     guides(color=F)


# Maybe better: separate plot for each continent
all_cnts <- unique(damfuns_all$continent)
plot_list <- list()
for (i in 1:length(all_cnts)) {
    plot_list[[i]] <- ggplot(damfuns_all %>% filter(continent==all_cnts[i], calib_method!="JRC"), aes(x=fld_dph,y=MDR))+
        geom_line(aes(color=regionID)) +
        geom_line(data=damfuns_all %>% filter(continent==all_cnts[i], calib_method=="JRC") %>% select(-calib_method), aes(x=fld_dph,y=MDR), color=1)+
        facet_grid(calib_method~.)
}
# plot_list[[4]]

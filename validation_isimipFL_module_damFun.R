# set of functions to deal with damage functions themselves


get_damFun_from_pars <- function(pars, xvals=seq(0,15,by=0.5)) {
    # MDR_fun=@(x,pars)pars(1)*(1-exp(-pars(2)*x));
    # params_MDR.damFun_xVals=0:0.5:15
    fun <- function(x,pars) pars[1]*(1-exp(-pars[2]*x))
    dat <- tibble(fld_dph=xvals,MDR=fun(xvals,pars))
    return(dat)
}

get_damFun_from_calibMethod <- function(regionID, calib_method, calib_method_list=get_calib_methods_tibble()) {
    # step 1: get calib params
    pars_calib <- call_fun_by_calibMethod(calib_method, load_calib_params, calib_method_list=calib_method_list,
                                          regionID=regionID)
    get_damFun_from_pars(pars_calib)
}

get_all_calib_damFuns <- function(calib_method_list=get_calib_methods_tibble()) {
    regions_list <- get_regions_list()
    for (i in 1:length(regions_list)) {
        for (j in 1:nrow(calib_method_list)) {
            if (i==1 & j==1) {
                dat <- get_damFun_from_calibMethod(regions_list[i],calib_method_list$calibration_method_names[j]) %>%
                    add_column(regionID=regions_list[i],calib_method=calib_method_list$calibration_method_names[j])
            } else {
                dat <- dat %>% bind_rows(get_damFun_from_calibMethod(regions_list[i],calib_method_list$calibration_method_names[j]) %>%
                                      add_column(regionID=regions_list[i],calib_method=calib_method_list$calibration_method_names[j]))
            }
        }
    }
    return(dat)
}

get_damFun_JRC <- function(continent) {
    path_in <- c(rev(rev(strsplit(DATA_PATH_FLDCAL, "/")[[1]])[-1]), "damFun_JRC") %>% paste(collapse="/")
    library(readxl)
    read_excel(paste0(path_in, "/", continent, "_FL_JRCdamagefunc_residential_PAA1.xls"),
               sheet="damagefunctions") %>% select(fld_dph=Intensity, MDR=MDR)
}

get_damFuns_JRC <- function() {
    path_in <- c(rev(rev(strsplit(DATA_PATH_FLDCAL, "/")[[1]])[-1]), "damFun_JRC") %>% paste(collapse="/")
    continents <- sapply(strsplit(list.files(path_in), "_"), "[",1)
    for (i in 1:length(continents)) {
        if (i==1) {
            dat <- get_damFun_JRC(continents[i]) %>% add_column(continent=continents[i], .before=1)
        } else {
            dat <- dat %>% bind_rows(get_damFun_JRC(continents[i]) %>% add_column(continent=continents[i], .before=1))
        }
    }
    # Add spaces to continents
    dat$continent <- dat$continent %>%
        str_replace(pattern = "NorthAmerica",replacement = "North America") %>%
        str_replace(pattern = "SouthAmerica",replacement = "South America")
    return(dat)
}

regions_to_full <- function(regionID) {
    switch(regionID,
           "NAM"="North America",
           "CAR"="Central America",
           "LAN"="Latin America North",
           "LAS"="Latin America South",
           "EUR"="Europe",
           "NAF"="North Africa",
           "SAF"="South Africa",
           "SSA"="Sub-Saharan Africa",
           "ARA"="Arabic Peninsula and Middle East",
           "CAS"="Central Asia",
           "SWA"="South-West Asia",
           "SEA"="South-East Asia",
           "CHN"="China, Japan, Korea, Taiwan, Hongkong",
           "EUA"="Eurasia, Russia, Balkans",
           "AUS"="Australia and New Zealand",
           stop("** ERROR ** unexpected value for 'regionID' *****"))
}

regions_to_continent <- function(regionID) {
    switch(regionID,
           "NAM"="North America",
           "LAN"=,
           "LAS"=,
           "CAR"="South America",
           "EUR"="Europe",
           "NAF"=,
           "SAF"=,
           "SSA"="Africa",
           "ARA"=,
           "CAS"=,
           "SWA"=,
           "SEA"=,
           "EUA"=,
           "CHN"="Asia",
           "AUS"="Oceania",
           stop("** ERROR ** unexpected value for 'regionID' *****"))
}

match_regions_continents <- function() {
    tibble(regionID=get_regions_list()) %>% mutate(region_name=sapply(regionID, regions_to_full),
                                                   continent=sapply(regionID, regions_to_continent))
}
# validation_isimipFL_module_load.R
# set of functions to load output of ISIMIP FLOOD CLIMADA
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### LOADING FUNCTIONS
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
# source("validation_isimipFL_module_load.R")
require(tidyverse)


load_JRC_eval <- function(RegionID,
                          years_range = c(1971,2010),
                          hazard_protection="flopros",
                          subtract_matsiro=1,
                          entity_year=0) {
    # load damages computed with the JRC damage functions
    # test <- load_JRC_eval("NAM")
    filename_head <-  paste0('damages_JRC_', RegionID, '_', years_range[1], '-', years_range[2])
    filename_haz = paste0('Haz-Prot', hazard_protection, '-subMATSIRO', subtract_matsiro)
    filename_ent = paste0('Entity-Year', entity_year)
    output_eval_filename = paste0(file.path(DATA_PATH_FLDCAL, 'damages_JRCdamFun/'), filename_head, '_', filename_haz, '_', filename_ent, '_eval.csv')
    dat <- read_csv(output_eval_filename, col_types = cols()) %>% mutate(damage=round(damage))
    return(dat)
}

get_all_calib_filenames <- function(regionID, calib_method_list = get_calib_methods_tibble()) {
    fn <- sapply(1:nrow(calib_method_list), function(i) {
        get_calib_filename(regionID,
                           cost_function = calib_method_list$cost_function[i],
                           exclude_years_0totals = calib_method_list$exclude_years_0totals[i],
                           which_file = "mat")
    })
    calib_method_list %>% mutate(filename=fn)
}

get_calib_filename <- function(regionID,
                               calib_method="patternsearch",
                               cost_function,
                               which_file,
                               years_range=c(1992,2010),
                               calib_options=NULL,
                               n_per_dim=20,underestimation_factor=2,
                               hazard_protection='flopros',subtract_matsiro=1,entity_year=0,keep_countries_0emdat=2,
                               remove_years_0emdat=0,remove_years_0YDS=list(do=0),exclude_years_0totals=0,
                               MM_how='MMM',pars_range=list(c(0.0001,1),c(0.0001,5))) {
    
    # function to retrieve an output file name from the calibration
    # if calib_options is not passed, set default values
    if (is.null(calib_options)) {
        if (calib_method == 'regular_sampling') {
            calib_options <- list(n_per_dim=20)
        } else if (calib_method == 'patternsearch') {
            calib_options <- list(random=0,
                                  nstart=3,
                                  InitialMeshSize=0.25,
                                  step_tolerance=0.001)
        }
    }
    if (calib_method == 'regular_sampling') {
        filename_calib_method <- paste0(calib_method,'-',n_per_dim)
    } else if (calib_method == 'patternsearch') {
        filename_calib_method <- paste0(calib_method,
                                        c('-reg','-rand')[calib_options$random+1], calib_options$nstart,
                                        '-mesh', calib_options$InitialMeshSize,
                                        '-step', calib_options$step_tolerance)
    } else {
        stop("input parameter calib_method should be one of 'regular_sampling' or 'patternsearch'")
    }
    filename_calib <- paste0('calib_',regionID,'_',years_range[1], '-', years_range[2], '_',
                             cost_function, '-uf', underestimation_factor, '-',
                             MM_how, '-', filename_calib_method)
    filename_haz <- paste0('Haz-Prot', hazard_protection, '-subMATSIRO', subtract_matsiro)
    filename_ent <- paste0('Entity-Year', entity_year)
    filename_filter <- paste0('Filters-Country', keep_countries_0emdat,
                              '-emdat', remove_years_0emdat,
                              '-YDS', remove_years_0YDS$do)
        if (remove_years_0YDS$do) {
            filename_filter <- paste0(filename_filter, '-t', remove_years_0YDS$threshold,
                                      '-w', remove_years_0YDS$what,
                                      '-m', remove_years_0YDS$min_val)
        }
        if (exclude_years_0totals) {
            filename_filter <- paste0(filename_filter, '_Filter-RegYears0')
        }
    filename_pars <- paste0('pars',
                            format(pars_range[[1]][1], scientific = F), '-',
                            format(pars_range[[1]][2], scientific = F), '-',
                            format(pars_range[[2]][1], scientific = F), '-',
                            format(pars_range[[2]][2], scientific = F))
    which_file_list <- list(mat=c('calibration_opt_params/','.mat'),
                            steps=c('calibration_steps/','_steps.dat'),
                            eval=c('calibration_eval/','_eval.csv'))
    if (!(which_file %in% names(which_file_list))) {
        stop(paste("input parameter 'which_file' should have one of the following value : ", paste(names(which_file_list), collapse=", ")))
    }
    path_in <- DATA_PATH_FLDCAL
    filename <- paste0(path_in, "/", which_file_list[[which_file]][1], filename_calib, '_',
                       filename_haz, '_', filename_ent, '_', filename_filter, '_',
                       filename_pars, which_file_list[[which_file]][2])
    return(filename)
    
}

load_calib_steps <- function(...) {
    filename <- get_calib_filename(which_file='steps',...)
    dat <-  read_delim(filename, delim=" ")
    return(dat)
}

load_calib_params <- function(...) {
    library(h5)
    library(hdf5r)
    filename <- get_calib_filename(which_file='mat',...)
    file.h5 <- H5File$new(filename, mode = "r")
    optimal_pars <- file.h5[["optimal_pars"]]$read()
    return(optimal_pars)
}

load_calib_eval <- function(...) {
    filename <- get_calib_filename(which_file='eval',...)
    dat <-  read_csv(filename, col_types = cols()) %>% mutate(damage=round(damage),
                                          used_in_calibration=as.logical(used_in_calibration))
    return(dat)
}

load_all_evals_one_region <- function(regionID, regSum_only=FALSE, calib_only=FALSE) {
    calib_methods <- get_calib_methods_tibble()
    for (i in 1:nrow(calib_methods)) {
        if (i==1) {
            data_calib <- call_fun_by_calibMethod(calib_methods$calibration_method_names[i], load_calib_eval, regionID=regionID) %>%
                mutate(damage_source=ifelse(dataset=="EM-DAT", "EM-DAT",calib_methods$calibration_method_names[i]))
        } else {
            data_calib <- data_calib %>% bind_rows(call_fun_by_calibMethod(calib_methods$calibration_method_names[i], load_calib_eval, regionID=regionID) %>%
                                                       mutate(damage_source=ifelse(dataset=="EM-DAT", "EM-DAT",calib_methods$calibration_method_names[i])))
        }
    }
    if (calib_only) {
        data_calib <- data_calib %>% filter(used_in_calibration==T)
    }
    if (any(is.na(data_calib$damage) & data_calib$dataset!='EM-DAT')) {
        warning(paste0("** WARNING ** some damages are missing (calib_eval file for regionID ",regionID,") *****"))
    }
    data_JRC <- load_JRC_eval(regionID) %>%
        filter(dataset!='EM-DAT') %>%
        mutate(damage_source="JRC")
    if (calib_only) {
        data_JRC <- data_JRC %>% right_join(data_calib %>% select(country,year) %>% unique())
    }
    if (any(is.na(data_JRC$damage))) {
        warning(paste0("** WARNING ** some damages are missing (JRC file for regionID ",regionID,") *****"))
    }
    data <- bind_rows(data_calib, data_JRC) %>%
        unique() %>%
        mutate(damage_source=factor(damage_source, levels=c("EM-DAT","JRC",calib_methods$calibration_method_names)))
    # lines below not necessary: if any country has 'NA', the whole region will have NA for that year, dataset and damage_source.
    # Also, NAs should will only appear in EM-DAT, by definition
    # # filter out NAs to keep only the same set of countries for the regional sum
    # if (common_country_years_only) {
    #     data %>% spread(damage_source,damage) %>% group_by(country, year) %>% summarise(nobs=sum(!is.na(`EM-DAT`)),nJRC=)
    #     data %>% group_by(country,damage_source) %>% summarise(mean_damage=mean(damage)) %>% filter(is.na(mean_damage)) %>% ungroup() %>%select(country) %>% unique()
    # }
    # add regional sum data
    data_sum <- data %>%
        group_by(year, dataset,damage_source) %>%
        summarise(damage=sum(damage)) %>%
        ungroup() %>%
        right_join(data %>% select(year,dataset,damage_source) %>% unique() %>% mutate(used_in_calibration=year>=1992)) %>%
        mutate(country=paste0("ALL (",regionID,")"))
    data_sum <- data_sum[,names(data)]
    if (!regSum_only) {
        data_sum <- data %>% bind_rows(data_sum)
    }
    data_sum <- data_sum %>%
        add_column(region=regionID) %>%
        add_MMM()
    return(data_sum)
}
# validation_isimipFL_module_load.R
# set of functions to load output of ISIMIP FLOOD CLIMADA
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### LOADING FUNCTIONS
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
# source("validation_isimipFL_module_load.R")
require(tidyverse)

get_regions_list <- function() {
    # get the list of all regions
    return(sort(unique(sapply(strsplit(list.files(file.path(DATA_PATH_FLDCAL, "damages_JRCdamFun")), "_"), "[",3))))
}

get_costFunctions_list <- function() {
    # get the list of all cost functions combinations
    costfunc_combinations_list <- sort(unique(sapply(strsplit(list.files(file.path(DATA_PATH_FLDCAL, "calibration_eval")), "_"), "[",4)))
    costfunc_list <- sort(unique(sapply(strsplit(costfunc_combinations_list, "-"),"[",1)))
    
    return(list(costfunc=costfunc_list, costfunc_full=costfunc_combinations_list))
}

get_calib_full_list <- function() {
    # get the list of all cost functions combinations
    files <- sapply(strsplit(Sys.glob(file.path(DATA_PATH_FLDCAL, "calibration_eval/calib_EUR_*step0.001_*")), "/"), function(s) rev(s)[1])
    # get the corresponding arguments
    files.parts <- strsplit(files, "_") %>% sapply(function(s) if (length(s)==10) s else c(s[1:7],"",s[8:9])) %>% unlist() %>% t() %>% as.data.frame()
    which.cst <- sapply(1:ncol(files.parts), function(i) length(unique(files.parts[,i])))==1
    varying.pars <- tibble(cost_function=sapply(strsplit(files.parts[[4]],"-"),"[",1),
                               exclude_years_0totals=files.parts[[8]]!="")
    # (regionID,calib_method,cost_function,which_file,
    #     years_range=c(1992,2010),
    #     calib_options=NULL,
    #     n_per_dim=20,underestimation_factor=2,
    #     hazard_protection='flopros',subtract_matsiro=1,entity_year=0,keep_countries_0emdat=2,
    #     remove_years_0emdat=0,remove_years_0YDS=list(do=0),exclude_years_0totals=0,
    #     MM_how='MMM',pars_range=list(c(0.0001,1),c(0.0001,5)))
    # paste0('calib_',regionID,'_',years_range[1], '-', years_range[2], '_',
    #        cost_function, '-uf', underestimation_factor, '-',
    #        MM_how, '-', filename_calib_method)
    return(varying.pars)
}


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
    dat <- read_csv(output_eval_filename)
    return(dat)
}

get_calib_filename <- function(regionID,calib_method,cost_function,which_file,
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
    dat <-  read_csv(filename) %>% mutate(damage=round(damage),
                                          used_in_calibration=as.logical(used_in_calibration))
    return(dat)
}
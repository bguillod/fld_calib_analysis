# helper functions

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
    # get all varying parameters of calibration
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

get_calib_methods_tibble <- function() {
    calib_methods <- get_calib_full_list()
    # anything else is constant and default values
    # So, now choose a name for each damage function calibration strategy.
    calibration_method_names <- c("DFC","YL2","YL2aY")
    calib_methods <- calib_methods %>% add_column(calibration_method_names=calibration_method_names, .before=1)
    return(calib_methods)
}

call_fun_by_calibMethod <- function(calib_method, fun, calib_method_list=calib_methods, ...) {
    # calib_method can be a string or an integer
    if (is.character(calib_method)) calib_method <- which(calib_method_list$calibration_method_names == calib_method)
    all_args <- list()
    for (i in 2:ncol(calib_method_list)) all_args[[names(calib_method_list)[i]]] <- calib_method_list[[i]][calib_method]
    all_args <- c(all_args, list(...))
    do.call(what = fun, args = all_args)
}


# add_regional_sum <- function(data, continent_name) {
#     temp <- data %>% group_by(year, dataset) %>% summarise(damage=sum(damage)) %>% mutate(used_in_calibration=FALSE, country=paste0("ALL (",continent_name,")"))
#     return(data %>% bind_rows(temp)) #%>% select(country) %>% unique()
# }

add_MMM <- function(data) {
    # Compute Multi-Model average (MMM) and add as a dataset
    if ("rt" %in% names(df)) {
        data <- data %>% select(-rt)
    }
    data_with_mmm_old <- data %>%
        filter(damage_source!="EM-DAT") %>%
        group_by(country, year, damage_source) %>%
        summarise(damage=round(mean(damage))) %>%
        ungroup() %>%
        mutate(dataset="MMM",used_in_calibration=year>=1992) %>%
        left_join(data %>% select(country,region) %>% unique()) %>%
        bind_rows(data)
    data_with_mmm_new <- data %>%
        filter(damage_source!="EM-DAT") %>%
        group_by(country, year, damage_source) %>%
        summarise(damage=round(mean(damage))) %>%
        ungroup() %>%
        # mutate(dataset="MMM",used_in_calibration=( (year>=1992) & () )) %>%
        mutate(dataset="MMM") %>%
        left_join(data %>% filter(dataset=="CLM_gswp3") %>% select(country,year,used_in_calibration,damage_source,region) %>% unique()) %>%
        bind_rows(data)
    data_with_mmm <- data_with_mmm[,]
    return(data_with_mmm)
}

compute_return_times <- function(data, ties.method=c("first","min")) {
    ties.method=match.arg(ties.method)
    # data_rt <- data %>% group_by(country, dataset, damage_source) %>% mutate(rank=min_rank(desc(damage)),ny=n()) %>% ungroup()
    data_rt <- data %>% group_by(country, dataset, damage_source) %>% mutate(rank=rank(desc(damage),ties.method = ties.method),ny=n()) %>% ungroup()
    data_rt <- data_rt %>% mutate(rt=(ny+1)/(rank)) %>% select(-rank,-ny)
    return(data_rt)
}
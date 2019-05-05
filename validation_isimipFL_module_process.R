
# add_regional_sum <- function(data, continent_name) {
#     temp <- data %>% group_by(year, dataset) %>% summarise(damage=sum(damage)) %>% mutate(used_in_calibration=FALSE, country=paste0("ALL (",continent_name,")"))
#     return(data %>% bind_rows(temp)) #%>% select(country) %>% unique()
# }

compute_return_times <- function(data) {
    data_rt <- data %>% group_by(country, dataset, damage_source) %>% mutate(rank=min_rank(desc(damage)),ny=n()) %>% ungroup()
    data_rt <- data_rt %>% mutate(rt=(ny+1)/(rank)) %>% select(-rank,-ny)
    return(data_rt)
}
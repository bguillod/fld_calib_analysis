
add_regional_sum <- function(data, continent_name) {
    temp <- data %>% group_by(year, dataset) %>% summarise(damage=sum(damage)) %>% mutate(used_in_calibration=FALSE, country=paste0("ALL (",continent_name,")"))
    return(data %>% bind_rows(temp)) #%>% select(country) %>% unique()
}

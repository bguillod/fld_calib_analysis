
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### PERFORMANCE METRICS FUNCTIONS
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------

rmse_of_yearly_by_country <- function(df) {
    # return the 
    # group by damage function (without EM-DAT)
    # df_grouped <- df %>% filter(damage_source!="EM-DAT") %>% group_by(damage_source)
    # put each model in a column, append EM-DAT
    df_grouped <- df %>%
        select(-rt) %>%
        filter(damage_source!="EM-DAT") %>%
        # group_by(damage_source)
        spread(dataset, damage)%>%
        left_join(df %>%
                      filter(damage_source=="EM-DAT") %>%
                      spread(dataset, damage) %>%
                      select(-damage_source,-region,-rt,-used_in_calibration),by=c("country","year"))
    # first, compute squared difference between each model and EM-DAT
    a <- df_grouped %>% select(-country, -year, -used_in_calibration,-damage_source,-region) %>%
        map(.y=df_grouped$`EM-DAT`, .f=~(. - .y)**2) %>% as_tibble() %>%
        select(-`EM-DAT`) %>% 
        bind_cols(df_grouped[1:5],.)
    # second, compute root of average per country (models are in columns)
    output <-  a %>% group_by(country, damage_source) %>% select(-year) %>%
        # select(-year, -used_in_) %>%
        summarise_if(is.numeric, ~sqrt(sum(.,na.rm=T))) %>%
        gather(3:ncol(.),key="dataset",value="damage")
    return(output)
}
    
aed_bias <- function(df) {
    # put each model in a column
    df_temp <- df %>% spread(dataset, damage)
    # first, compute AED for each model
    a <-  df_temp %>% group_by(country) %>%
        select(-year, -used_in_calibration) %>%
        summarise_if(is.numeric, mean)
    # second, compute difference between each model and EM-DAT
    output <- a %>% select(-country) %>%
        map(.y=a$`EM-DAT`, .f=~(. - .y)) %>% as_tibble() %>%
        select(-`EM-DAT`) %>%
        bind_cols(a[1],.)
    
    return(output)
}
    
ycor <- function(df) {
        # put each model in a column
    df_temp <- df %>% spread(dataset, damage)
    # a <- df_temp %>% group_by(country) %>%
    #     select(-country, -year, -used_in_calibration) %>%
    #     summarise_at(.vars=setdiff(names(.),"country"),.funs=~cor(.,y), y=.data$`EM-DAT`)
    a <- df_temp %>%
        select(-year, -used_in_calibration) %>%
        plyr::ddply(.variables="country",.fun=function(x) cor(matrix(as.numeric(unlist(x[-1])),ncol=length(x[-1])),matrix(rep(x$`EM-DAT`,times=length(x[-1])),ncol=length(x[-1])))[,1]) %>%
        as_tibble()
    names(a)[-1] <- names(df_temp%>%select(-year,-used_in_calibration,-country))
    
    return(a)
}

rt_bias <- function(df, rt) {
    
}
source("init.R")
figs_out_path <- file.path(FIGS_PATH, "regionID")
source("validation_isimipFL_module_maps.R")

map_countries_isimip <- function() {
    
    library(rworldmap)
    library(rgdal)
    
    # load region names
    regID <- read_file(file.path(FIGS_PATH, "../data_small/regID/RegID_names_isimip_flood.txt")) %>% str_split("\n") %>% tibble(long=unlist(.)) %>% slice(1:15) %>% mutate(ID=substr(long,1,3), name=stri_sub(long,4,-2))
    
    # load country data
    country_dat <- read_csv(file.path(FIGS_PATH, "../data_small/regID/NatID_RegID_isimip_flood.csv")) %>% filter(Reg_name %in% regID$ID)
    
    # loading whether included in calibration
    in_calib <- read_csv(file.path(FIGS_PATH, "../data_small/NatID_RegID_isimip_flood_filtered_1992-2010.csv"))
    country_dat <- country_dat %>% left_join(in_calib %>% select(ISO,in_calib) %>% mutate(in_calib=ifelse(in_calib==2,1,0)))
    
    # fix country ISO to match the rworldmap data
    country_dat <- country_dat %>%
        bind_rows(
            country_dat %>% filter(ISO=='SCG') %>% mutate(ISO='MNE'),
            country_dat %>% filter(ISO=='SCG') %>% mutate(ISO='KOS'),
            country_dat %>% filter(ISO=='SCG') %>% mutate(ISO='SRB'),
            country_dat %>% filter(ISO=='SDN') %>% mutate(ISO='SSD'),
            country_dat %>% filter(ISO=='PSE') %>% mutate(ISO='Gaza'),
            country_dat %>% filter(ISO=='EGY') %>% mutate(ISO='ESH', in_calib=0),
        ) %>% mutate(hatched=!in_calib)
    

    #join to a coarse resolution map
    spdf <- my_joinCountryData2Map(country_dat %>% select(ISO,Reg_name,hatched), joinCode="ISO3", nameJoinColumn="ISO")
    
    # transform to Robinson projection
    spdf <- spTransform(spdf, CRS=CRS("+proj=robin +ellps=WGS84"))
    
    if (FALSE) {
        # this is to fix issues in country identification
        temp <- spdf@data[,1:51] %>% as.tibble
        # first, which countries in country_dat is not there?
        country_dat %>% select(ISO) %>% anti_join(temp%>%transmute(ISO=ISO3))
        temp %>% filter(is.na(Reg_ID)) %>% select(ADMIN,ISO_A3,ISO3,ISO3.1,ISO)
    }
    
    # define colours
    pal <- c(brewer.pal(12,"Paired"),"black","grey50","grey85")
    
    pdf(file = file.path(figs_out_path, "map_regions.pdf"), height=3.6, width=7)
    par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
    dat <- my_mapCountryData(spdf, nameColumnToPlot="Reg_name", catMethod="categorical",
                          colourPalette=pal,
                          # ylim=c(-60,90),
                          addLegend = FALSE,mapTitle = "",add=F,mar=c(4,0,2,0),
                          lwd=0.2,# lwd=0.2,
                          pty="m", nameColumnToHatch="hatched")
    # addMapLegendBoxes(colourVector = dat$colourVector, cutVector = dat$cutVector, 
    #                   catMethod = "categorical")
    legend(x = "bottom", horiz = T, title = NULL, pch = 22, 
           cex = 0.7, pt.cex = 1.8, pt.bg = c(dat$colourVector), 
           # col = "gray",
           col=1, #c(dat$colourVector),
           bg = NA, legend=dat$cutVector, box.lwd=0.5, pt.lwd=0.2)
    title(main= 'Calibration regions', line=-1)
    dev.off()
    
}
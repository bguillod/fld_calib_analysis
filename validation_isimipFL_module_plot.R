# module validation_isimipFL_module_plot

plot_yearly_line <- function(data, show=T) {
    test <- ggplot(data, aes(x=year, y=damage,color=dataset))+
        geom_line(aes(size=ifelse(data$dataset=="EM-DAT",2,0.05)))+
        facet_grid(country ~ ., scale="free")
    if (show) print(test)
    invisible(test)
}




# boxplot of yearly damages
plot_yearly_boxplot <- function(data, show=T) {
    library(scales)
    test <- ggplot(data, aes(x=year, y=damage)) +
        geom_boxplot(data=subset(data, dataset!="EM-DAT"), aes(group=year), alpha=0.7, outlier.size = 0.4, position="dodge", lwd=0.2) +
        geom_point(data=subset(data, dataset=="EM-DAT"), aes(x=year,y=damage), col=1, fill="white", pch=21, cex=1.2) +
        facet_grid(country ~ ., scale="free")+
        scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) 10^x))
    if (show) print(test)
    invisible(test)
}


# envelope plot of yearly damages
plot_yearly_env <- function(data, show=T) {
    test <- ggplot(data, aes(x=year,y=damage))+geom_smooth(data=df %>% group_by(year, country) %>% summarise(ymin=min(damage),ymax=max(damage),y=mean(damage)), aes(color=country,y=y,x=year,ymin=ymin,ymax=ymax),stat="identity")
    if (show) print(test)
    invisible(test)
}

# # scatter plot of yearly damages
# plot_yearly_scatter <- function(data, show=T) {
#     library(scales)
#     data.scatter <- left_join(subset(data, dataset!="EM-DAT"), subset(data,dataset=="EM-DAT"),by=c("country","year"))
#     test <- ggplot(data.scatter, aes(x=damage.y, y=damage.x)) +
#         geom_point() +
#         facet_grid(country ~ ., scale="free") +
#         scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) 10^x))+
#         scale_x_continuous(trans='log',breaks = trans_breaks("log", function(x) 10^x))
#     if (show) print(test)
#     invisible(test)
# }


# scatter plot of country AED
plot_AED_scatter <- function(data, show=T) {
    data.AED <- aggregate(data["damage"], by=list(country=data$country, dataset=data$dataset), FUN=mean)
    data.AED.scatter <- left_join(subset(data.AED, dataset!="EM-DAT"), subset(data.AED,dataset=="EM-DAT"),by=c("country"))
    test <- ggplot(data.AED.scatter, aes(x=damage.y, y=damage.x)) +
        geom_point() +
        scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) 10^x))+
        scale_x_continuous(trans='log',breaks = trans_breaks("log", function(x) 10^x))
    if (show) print(test)
    invisible(test)
}


# FOR RETURN TIME PLOTS PLEASE LOOK AT: plot.returntimes.highP.boxplot.R

# +
#     scale_y_continuous(name=expression("precipitation ["*mm~day^-1*"]"), trans="log10", breaks=c(1, 2, 5, 10, 20, 50, 100, 200)) + 
#     facet_wrap(~ region, ncol=4, scales="free_y") +
#     theme(axis.text.x=element_text(angle=90, hjust=1),
#           axis.title.x=element_blank(),
#           panel.background = element_rect(fill="grey"),
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           legend.key = element_rect(fill="grey"),
#           legend.background = element_rect(fill="grey"))+
#     scale_fill_discrete(name="return time\n[years]") +
#     scale_colour_manual(name="var", values=c(prbc="white", pr="black"))

# initialisation
source(".init_local.R")

# anything else?
library(RColorBrewer)
library(tidyverse)
library(foreach)
library(stringi)
options(stringsAsFactors = FALSE)

FIGS_PATH <- file.path("/Users/bguillod/Dropbox/ETH_WORK/figs")
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)

theme_set(theme_light())
theme_update(strip.text = element_text(colour = "black"), strip.background=element_rect(fill="grey85"),
             legend.position="bottom", legend.box = "horizontal",
             plot.title = element_text(hjust = 0.5))
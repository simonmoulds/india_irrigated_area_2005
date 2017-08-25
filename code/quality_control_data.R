## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

x = readRDS("data/apy_indiastat_combined_data_formatted.rds")

## ======================================
## data quality
## ======================================

check_data <- function(x, y, crop, plot_title, ...) {

    ## the point of this function is create a plot
    
    ## get column names from data frame
    columns = grep(crop, names(y), value=TRUE)
    apy_columns = grep("^apy_", columns, value=TRUE) %>% sort
    irr_columns = grep("^irr_", columns, value=TRUE) %>% sort
    map_columns = grep("^mapspam_", columns, value=TRUE) %>% sort

    test1 = length(apy_columns) == length(irr_columns)
    test2 = all(gsub("^apy_", "", apy_columns) == gsub("^irr_", "", irr_columns))

    if (!test1) stop()
    if (!test2) stop()
    
    columns = c(apy_columns, irr_columns, map_columns)
    n = length(apy_columns)
    ix = c(seq(1, by=2, length.out=n), seq(2, by=2, length.out=n), seq(2 * n + 1, length.out=2)) %>% order
    columns = c(apy_columns, irr_columns, map_columns)[ix]

    ix <- match(columns, names(y))
    d  <- as.data.frame(y[,ix]) %>% setNames(columns)
    
    ## if all NA, set to zero
    for (i in 1:ncol(d)) {
        dd = as.numeric(d[,i])
        if (all(is.na(dd))) {
            dd[] = 0
            d[,i] = dd
        }
    }

    d %<>%
        add_column(time=x) %>%
        gather(variable, value, -time) %>%
        add_column(type=rep(NA, nrow(.)))

    d$variable = factor(d$variable, levels=unique(d$variable))

    p = ggplot(d, aes(x=time, y=value, colour=variable, shape=variable, size=variable, group=variable)) +
        geom_line(size=0.5) +
        geom_point() +
        scale_shape_manual(values=rep(c(19,2), n + 1)) +
        scale_size_manual(values=rep(c(1.5,3), n + 1)) +
        scale_x_continuous(breaks=2000:2010) +
        labs(x="Time", y="Area (Ha)", colour = "", shape="", size="") +
        ggtitle(plot_title)
    p
}

## create directories

if (!dir.exists("data/qc_plots")) dir.create("data/qc_plots")
crop_nms = grep("^irr_", names(x), value=TRUE) %>% gsub("^irr_", "", .) %>% strsplit("-") %>% sapply(FUN=function(x) x[[1]]) %>% unique

state_nms = x[["State"]] %>% unique

for (i in 1:length(state_nms)) {

    state_nm = gsub(" ", "_", state_nms[i])
    state_dir = file.path("data", "qc_plots", state_nm)
    if (dir.exists(state_dir)) {
        unlink(state_dir, recursive=TRUE)
    }
    dir.create(state_dir)

    for (j in 1:length(crop_nms)) {
        crop_nm = gsub(" ", "_", crop_nms[j])
        dir.create(file.path(state_dir, crop_nm))
    }
}

state_nms = "Andhra Pradesh" ## TODO: remove this

for (i in 1:length(state_nms)) {
    state_nm = state_nms[i]
    dists = x[["ADM2_CODE"]][x$State %in% state_nm] %>% unique
    dist_nms = x[["District"]][match(dists, x[["ADM2_CODE"]])]
    
    for (j in 1:length(dists)) {

        dist = dists[j]
        dist_nm = dist_nms[j]
        print(dist_nm)
        ## state_nm = state_nms[i]

        row_ix = x[["ADM2_CODE"]] %in% dist
        xx = x[row_ix,]

        ## Andhra Pradesh
        ## ##############

        if (state_nm %in% "Andhra Pradesh") {

            ## correct confusion between irrigated rice seasons
            xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
            xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
            xx[["irr_rice-autumn"]] = 0

            mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-winter"]][c(2,4)])
            xx[["irr_rice-winter"]][3] = xx[["apy_rice-winter"]][3] * mean_irr_frac

            ## ## confusion between sunflower
            ## xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
            ## xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
            ## xx[["apy_sunflower-whole_year"]] = 0
            ## xx[["apy_sunflower-rabi"]] = 0

            ## district-level corrections
            if (dist_nm %in% "Adilabad") {
                xx[["irr_pearl_millet-summer"]][8:9] = xx[["apy_pearl_millet-rabi"]][8:9]
                xx[["apy_other_cereals-kharif"]][1:4] = NA
                xx[["apy_sugarcane-whole_year"]][c(8,10)] = xx[["irr_sugarcane-whole_year"]][c(8,10)]
            }

            if (dist_nm %in% "Karimnagar") {
                xx[["apy_wheat-rabi"]][8] = xx[["irr_wheat-rabi"]][8]
                xx[["apy_pearl_millet-rabi"]][8] = xx[["irr_pearl_millet-rabi"]][8]
            }   
        }

        ## create plot for checking purposes
        for (k in 1:length(crop_nms)) {
            plot_title = paste0(state_nm, ", ", dist_nm, ", ", gsub("_", " ", paste0(toupper(substring(crop_nms[k], 1, 1)), substring(crop_nms[k], 2))))
            p = check_data(x=2000:2010, y=xx, crop=crop_nms[k], plot_title=plot_title)
            plot_file_nm = file.path("data", "qc_plots", gsub(" ", "_", state_nm), gsub(" ", "_", crop_nms[k]), paste0(tolower(gsub(" ", "_", dist_nm)), "_", crop_nms[k], "_qc_plot.pdf"))
            ggsave(plot_file_nm, plot=p, width=7.5, height=5, device=pdf, path=".")
        }

        x[row_ix,] = xx
    }

    ## after all the plots have been made for a particular state, combine
    ## the plots into a single document (this makes it easier to scroll
    ## between them)
    for (k in 1:length(crop_nms)) {
        cwd = getwd()
        setwd(file.path("data", "qc_plots", gsub(" ", "_", state_nm), gsub(" ", "_", crop_nms[k])))
        system("pdftk `ls *.pdf | sort` output combined_plots.pdf")
        setwd(cwd)
    }
}

## ======================================
## rename columns
## ======================================

nms = names(x)

nms[nms == "irr_rice-summer"] = "irr_rice-rabi"
nms[nms == "irr_rice-winter"] = "irr_rice-kharif"
nms[nms == "irr_rice-autumn"] = "irr_rice-autumn"

nms[nms == "apy_rice-summer"] = "apy_rice-rabi"
nms[nms == "apy_rice-winter"] = "apy_rice-kharif"
nms[nms == "apy_rice-autumn"] = "apy_rice-autumn"

names(x) = nms

## get names of crops in each season
kharif_crops = grep("-kharif$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-kharif$", "", .) %>% unique %>% sort

rabi_crops = grep("-rabi$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-rabi$", "", .) %>% unique %>% sort

summer_crops = grep("-summer$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-summer$", "", .) %>% unique %>% sort

whole_year_crops = grep("-whole_year$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-whole_year$", "", .) %>% unique %>% sort

## make column names for each crop and season combination
whole_year_col_nms = paste0(c("irr_","rain_"), rep(whole_year_crops, each=2), "-whole_year")

kharif_col_nms = paste0(c("irr_","rain_"), rep(kharif_crops, each=2), "-kharif")

rabi_col_nms = paste0(c("irr_","rain_"), rep(rabi_crops, each=2), "-rabi")

summer_col_nms = paste0(c("irr_","rain_"), rep(summer_crops, each=2), "-summer")

col_nms = c(kharif_col_nms,
            rabi_col_nms,
            summer_col_nms,
            whole_year_col_nms)

y = as.data.frame(matrix(data=0, nrow=nrow(x), ncol=length(col_nms))) %>% setNames(col_nms)

## column names without irr/rain prefix
sub_col_nms = gsub("^([a-z]+)_(.*)-(.*)$", "\\2-\\3", col_nms) %>% unique

for (i in 1:length(sub_col_nms)) {
     
    total_col_nm = paste0("apy_", sub_col_nms[i])
    irr_col_nm = paste0("irr_", sub_col_nms[i])
    rain_col_nm = paste0("rain_", sub_col_nms[i])

    ## check total and irr column names exist in x
    if (!all(c(total_col_nm, irr_col_nm) %in% names(x))) {
        stop()
    }
    
    total = x[[total_col_nm]]
    irr = x[[irr_col_nm]]
    rain = total - irr

    irr[is.na(irr)] = 0
    rain[is.na(rain)] = 0

    irr[irr < 0] = 0
    rain[rain < 0] = 0

    y[[irr_col_nm]] = irr
    y[[rain_col_nm]] = rain
    
}

y = as_tibble(cbind(x[,c("State","ADM1_CODE","District","ADM2_CODE","Year")], y))

saveRDS(y, "data/apy_indiastat_combined_data_qc.rds")


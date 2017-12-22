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
    if (!dir.exists(state_dir)) {
        dir.create(state_dir)
    }

    for (j in 1:length(crop_nms)) {
        crop_nm = gsub(" ", "_", crop_nms[j])
        crop_dir = file.path(state_dir, crop_nm)
        if (!dir.exists(crop_dir)) {
            dir.create(crop_dir)
        }
    }
}

## state_nms = "Andhra Pradesh" ## TODO: remove this
## state_nms = c("Andhra Pradesh","Uttar Pradesh")
state_nms = c("Uttar Pradesh")

linear_interp = function(x, na_ix, ...) {
    x[na_ix] = NA
    x = zoo::na.approx(x, na.rm=FALSE)
    x
}
locf_interp = function(x, na_ix, ...) {
    x[na_ix] = NA
    x = zoo::na.locf(x, na.rm=FALSE, ...)
    x
}
irr_frac_interp = function(irr, tot, ix, nb_ix, ...) {
    mean_irr_frac = mean(irr[nb_ix] / tot[nb_ix])
    irr[ix] = tot[ix] * mean_irr_frac
    irr
}

tot_frac_interp = function(tot, irr, ix, nb_ix, ...) {
    mean_tot_frac = mean(tot[nb_ix] / irr[nb_ix])
    tot[ix] = irr[ix] * mean_tot_frac
    tot
}

crops =
    grep("^apy_.*$", names(x), value=TRUE) %>%
    gsub("apy_", "", x=.) %>%
    gsub("-(kharif|rabi|summer|autumn|winter|whole_year)", "", x=.) %>%
    unique

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

        ## here we set to zero all values below a certain threshold
        threshold = 10 ## 10 hectares (i.e. ~0.1% grid cell area)
        for (k in 1:length(crops)) {
            ptn = paste0("^apy_", crops[k], "-.*$")
            cols = grep(ptn, names(xx))
            tot = rowSums(xx[,cols], na.rm=TRUE)
            if (max(tot) <= threshold) {
                ptn = paste0("^(apy|irr)_", crops[k], ".*$")
                cols = grep(ptn, names(xx))
                xx[,cols] = 0
            }
        }

        ## Andhra Pradesh
        ## ##############

        if (state_nm %in% "Andhra Pradesh") {

            ## correct confusion between irrigated rice seasons
            xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
            xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
            xx[["irr_rice-autumn"]] = 0

            xx[["irr_rice-winter"]] %<>% irr_frac_interp(xx[["apy_rice-winter"]], 3, c(2,4))
            
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
            
        if (state_nm %in% "Uttar Pradesh") {

            xx[["apy_lentil-rabi"]] %<>% linear_interp(5:6)
            xx[["apy_pigeonpea-kharif"]] %<>% linear_interp(c(3,5,6))
            xx[["irr_pigeonpea-kharif"]] %<>% linear_interp(c(3,5,6))
            xx[["apy_potato-kharif"]] %<>% linear_interp(1:4)

            xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], c(6), c(5,7))
            xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], 11, 10)
            
            xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], c(6), c(5,7))
            xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], 11, 10)

            xx[["irr_cotton-rabi"]] %<>% irr_frac_interp(xx[["apy_cotton-rabi"]], c(6), c(5,7))
            xx[["irr_cotton-rabi"]] %<>% irr_frac_interp(xx[["apy_cotton-rabi"]], 11, 10)
            
            xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], c(6), c(5,7))
            xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], 11, 10)

            ## Aligarh
            ## #######
            if (dist_nm %in% "Aligarh") {
                xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], 6, 5)
            }

            ## Ambedkar Nagar
            ## ##############
            if (dist_nm %in% "Ambedkar Nagar") {
                xx[["irr_chickpea-rabi"]] %<>% linear_interp(na_ix=6)
            }

            ## Auraiya
            ## #######
            if (dist_nm %in% "Auraiya") {
                xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], c(4), c(3,5))
                xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], c(4), c(3,5))
                xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], c(4), c(3,5))
            }

            ## Badaun
            ## ######
            if (dist_nm %in% "Badaun") {
                xx[["irr_barley-rabi"]][10] = xx[["irr_barley-kharif"]][10] + xx[["irr_barley-rabi"]][10]
                xx[["apy_barley-rabi"]] %<>% tot_frac_interp(xx[["irr_barley-rabi"]], 10, 9)
                xx[["irr_barley-kharif"]]
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
        fn = paste0("combined_plots_", crop_nms[k], ".pdf")
        if (file.exists(fn)) {
            unlink(fn)
        }
        system(paste0("pdftk `ls *.pdf | sort` output ", fn, " dont_ask"))
        system(paste0("mv ", fn, " ", file.path(cwd, "data","qc_plots", gsub(" ", "_", state_nm))))
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


select_data <- function(x, state, district, yrs, ...) {
    x =
        x %>%
        filter(State %in% state) %>%
        filter(District %in% district) %>%
        filter(Year %in% yrs)
    x
}

check_data <- function(x, y, columns=c("Rice_Autumn","Rice_Winter","Rice_Summer"), na.ix, ..., plot=TRUE, legend.pos="topleft") {
    ## Function to check data... TODO
    ##
    ## Args:
    ##   x...
    ##   y...
    ##
    ## Returns:
    ##   A data.frame.

    if (!all(columns %in% names(y))) {
        stop()
    }

    ix <- match(columns, names(y))
    d  <- as.data.frame(y[,ix])
    names(d) <- columns
    
    if (!missing(na.ix)) {
        if (!all(names(na.ix) %in% names(d))) {
            stop()
        }

        for (i in 1:length(na.ix)) {
            nm <- names(na.ix)[i]
            d[na.ix[[i]],nm] <- NA
        }
    }

    ## check NA vals
    for (i in 1:ncol(d)) {
        dd = as.numeric(d[,i])
        if (all(is.na(dd))) {
            dd[] = 0
            d[,i] = dd
        }
    }
        
    if (plot) {
        matplot(x, d, type="o", pch=1, lty=1, col=rainbow(length(columns)), ...) #ylim=c(0, (max(as.matrix(d), na.rm=TRUE) * 1.05)), ...)
        ## dd <- matrix(data=NA, nrow=nrow(d), ncol=ncol(d))
        ## for (i in 1:length(columns)) {
        ##     tmp <- aggregate(d[,i], by=list(level), FUN=mean)[,2]
        ##     tmp <- (rep(tmp, each=5))
        ##     dd[1:length(tmp),i] <- tmp
        ## }
        ## matplot(x, dd, type="l", pch=1, lty=1, add=TRUE)
        legend(legend.pos, legend=columns, pch=1, lty=1, col=rainbow(length(columns)))
    }

    for (i in 1:length(columns)) {
        vals <- try(zoo::na.approx(d[,i], na.rm=FALSE), silent=TRUE)
        if (!inherits(vals, "try-error")) {
            d[,i] <- vals
        }
    }
    
    d <- zoo::na.locf(d, na.rm=FALSE)
    d <- zoo::na.locf(d, fromLast=TRUE)
    
    if (plot) {
        for (i in 1:length(columns)) {
            lines(x, d[,i], type="l", lty=2, col=rainbow(length(columns))[i])
            ## av <- aggregate(d[,i], by=list(level), FUN=mean)[,2]
            ## av <- av[as.numeric(level)]

            ## mx <- aggregate(d[,i], by=list(level), FUN=max)[,2]
            ## mx <- mx[as.numeric(level)]
            
            ## mn <- aggregate(d[,i], by=list(level), FUN=min)[,2]
            ## mn <- mn[as.numeric(level)]
            
            ## ## dd <- rep(NA, nrow(d))
            ## ## dd[1:length(tmp)] <- tmp
            ## lines(x, av, type="l", lty=1, lwd=2, col="black")
            ## lines(x, mx, type="l", lty=2, lwd=2, col="black")
            ## lines(x, mn, type="l", lty=2, lwd=2, col="black")
        }
    }
    ## abline(v=1970, lwd=2)
    ## abline(v=2010, lwd=2)
    y[,ix] <- d
    y
}

district_groundwater_frac <- function(x, y, useGIA=TRUE, na.ix, glm=FALSE, plot=TRUE) {

    if (useGIA) {
        total_gia = rowSums(y[c("canal_government_gia","canal_private_gia","tanks_gia","tubewells_gia","other_wells_gia","other_sources_gia")], na.rm=TRUE)
        
        gw.frac <- rowSums(y[c("tubewells_gia","other_wells_gia")], na.rm=TRUE) / total_gia
        
    } else {
        total_nia = rowSums(y[c("canal_government_nia","canal_private_nia","tanks_nia","tubewells_nia","other_wells_nia","other_sources_nia")], na.rm=TRUE)

        gw.frac <- rowSums(y[,c("tubewells_nia","other_wells_nia")], na.rm=TRUE) / total_nia
    }

    if (!missing(na.ix)) {
        gw.frac[na.ix] <- NA
    }
    
    if (glm) {
        gw.frac.glm <- glm(gw.frac~x, data=data.frame(x=x, gw.frac=gw.frac))
        gw.frac.predict <- predict(gw.frac.glm, data.frame(x=x))
        
    } else {
        gw.frac.predict <- zoo::na.approx(gw.frac, na.rm=FALSE)
        gw.frac.predict <- zoo::na.locf(gw.frac.predict, na.rm=FALSE)
        gw.frac.predict <- zoo::na.locf(gw.frac.predict, fromLast=TRUE)
    }
    
    gw.frac.predict[gw.frac.predict < 0] <- 0
    gw.frac.predict[gw.frac.predict > 1] <- 1

    if (plot) {
        plot(x, gw.frac, ylim=c(0,1), xlab="Year",ylab=""); lines(x,gw.frac.predict)

        ## tmp <- aggregate(gw.frac.predict, by=list(level), FUN=mean)[,2]
        ## tmp <- tmp[as.numeric(level)]
        ## ## dd <- rep(NA, length(gw.frac))
        ## ## dd[1:length(tmp)] <- tmp
        ## lines(x, tmp, type="l", lty=1, lwd=2, col="black")

    }

    gw.frac <- gw.frac.predict
    sw.frac <- 1 - gw.frac
    out <- data.frame(Year=x, SW_Frac=sw.frac, GW_Frac=gw.frac)

}

write_output = function(x, minval = 0, ...) {

    ## rice
    irri_rice_autumn = x[["irr_rice-autumn"]]
    irri_rice_winter = x[["irr_rice-winter"]]
    irri_rice_summer = x[["irr_rice-summer"]]
    rain_rice_autumn = x[["apy_rice-autumn"]] - irri_rice_autumn
    rain_rice_winter = x[["apy_rice-winter"]] - irri_rice_winter
    rain_rice_summer = x[["apy_rice-summer"]] - irri_rice_summer
    
    ## wheat
    irri_wheat_rabi   = pmax(pmin(x[["irr_wheat"]], x[["apy_wheat-rabi"]]), minval)
    rain_wheat_rabi   = pmax(x[["apy_wheat-rabi"]] - irri_wheat_rabi, minval)
    irri_wheat_kharif = pmax(x[["irr_wheat"]] - irri_wheat_rabi, minval)
    rain_wheat_kharif = pmax(x[["apy_wheat-kharif"]] - irri_wheat_kharif, minval)
    
    ## barley columns
    irri_barley_rabi   = pmax(pmin(x[["irr_barley"]], x[["apy_barley-rabi"]]), minval)
    rain_barley_rabi   = pmax(x[["apy_barley-rabi"]] - irri_barley_rabi, minval)
    irri_barley_kharif = pmax(x[["irr_barley"]] - irri_barley_rabi, minval)
    rain_barley_kharif = pmax(x[["apy_barley-kharif"]] - irri_barley_kharif, minval)
        
    ## sorghum columns
    irri_sorghum_kharif = pmax(x[["irr_sorghum-kharif"]], minval)
    rain_sorghum_kharif = pmax(x[["apy_sorghum-kharif"]] - irri_sorghum_kharif, minval)
    irri_sorghum_rabi   = pmax(x[["irr_sorghum-rabi"]], minval)
    rain_sorghum_rabi   = pmax(x[["apy_sorghum-rabi"]] - irri_sorghum_rabi, minval)
    
    ## maize columns
    irri_maize_rabi   = pmax(pmin(x[["irr_maize"]], x[["apy_maize-rabi"]]), minval)
    rain_maize_rabi   = pmax(x[["apy_maize-rabi"]] - irri_maize_rabi, minval)
    irri_maize_kharif = pmax(x[["irr_maize"]] - irri_maize_rabi, minval)
    rain_maize_kharif = pmax(x[["apy_maize-kharif"]] - irri_maize_kharif, minval)
    
    ## pearl millet columns
    irri_pearl_millet_rabi   = pmax(pmin(x[["irr_pearl_millet"]], x[["apy_pearl_millet-rabi"]]), minval)
    rain_pearl_millet_rabi   = pmax(x[["apy_pearl_millet-rabi"]] - irri_pearl_millet_rabi, minval)
    irri_pearl_millet_kharif = pmax(x[["irr_pearl_millet"]] - irri_pearl_millet_rabi, minval)
    rain_pearl_millet_kharif = pmax(x[["apy_pearl_millet-kharif"]] - irri_pearl_millet_kharif, minval)

    ## finger millet columns
    irri_finger_millet_rabi   = pmax(pmin(x[["irr_finger_millet"]], x[["apy_finger_millet-rabi"]]), minval)
    rain_finger_millet_rabi   = pmax(x[["apy_finger_millet-rabi"]] - irri_finger_millet_rabi, minval)
    irri_finger_millet_kharif = pmax(x[["irr_finger_millet"]] - irri_finger_millet_rabi, minval)
    rain_finger_millet_kharif = pmax(x[["apy_finger_millet-kharif"]] - irri_finger_millet_kharif, minval)
    
    ## other cereals
    irri_other_cereal_kharif = pmax(x[["irr_other_cereals-kharif"]], minval)
    rain_other_cereal_kharif = pmax(x[["apy_other_cereals-kharif"]] - irri_other_cereal_kharif, minval)
    irri_other_cereal_rabi   = pmax(x[["irr_other_cereals-rabi"]], minval)
    rain_other_cereal_rabi   = pmax(x[["apy_other_cereals-rabi"]] - irri_other_cereals_rabi, minval)
        
    ## chickpea columns
    irri_chickpea_rabi   = pmax(pmin(x[["irr_chickpea"]], x[["apy_chickpea-rabi"]]), minval)
    rain_chickpea_rabi   = pmax(x[["apy_chickpea-rabi"]] - irri_chickpea_rabi, minval)
    irri_chickpea_kharif = pmax(x[["irr_chickpea"]] - irri_chickpea_rabi, minval)
    rain_chickpea_kharif = pmax(x[["apy_chickpea-kharif"]] - irri_chickpea_kharif, minval)
    
    ## pigeonpea columns
    irri_pigeonpea_rabi   = pmax(pmin(x[["irr_pigeonpea"]], x[["apy_pigeonpea-rabi"]]), minval)
    rain_pigeonpea_rabi   = pmax(x[["apy_pigeonpea-rabi"]] - irri_pigeonpea_rabi, minval)
    irri_pigeonpea_kharif = pmax(x[["irr_pigeonpea"]] - irri_pigeonpea_rabi, minval)
    rain_pigeonpea_kharif = pmax(x[["apy_pigeonpea-kharif"]] - irri_pigeonpea_kharif, minval)

    ## cowpea columns
    cowpea_cols =
        c("apy_cowpea-kharif",
          "apy_cowpea-rabi")

    ## lentil columns
    lentil_cols =
        c("apy_lentil-kharif",
          "apy_lentil-rabi",
          "apy_lentil-whole_year")

    ## other pulses
    irri_other_pulses_kharif = pmax(x[["irr_other_pulses-kharif"]], minval)
    rain_other_pulses_kharif = pmax(x[["apy_other_pulses-kharif"]] - irri_other_pulses_kharif, minval)
    irri_other_pulses_rabi   = pmax(x[["irr_other_pulses-rabi"]], minval)
    rain_other_pulses_rabi   = pmax(x[["apy_other_pulses-rabi"]] - irri_other_pulses_rabi, minval)
    
    ## sugarcane
    irri_sugarcane_annual = pmax(pmin(x[["irr_sugarcane-whole_year"]], x[["apy_sugarcane-whole_year"]]), minval)
    rain_sugarcane_annual = pmax(x[["apy_sugarcane-whole_year"]] - irri_sugarcane_annual, minval)
    
    ## groundnut columns
    irri_groundnut_rabi   = pmax(pmin(x[["irr_groundnut"]], x[["apy_groundnut-rabi"]]), minval)
    rain_groundnut_rabi   = pmax(x[["apy_groundnut-rabi"]] - irri_groundnut_rabi, minval)
    irri_groundnut_kharif = pmax(x[["irr_groundnut"]] - irri_groundnut_rabi, minval)
    rain_groundnut_kharif = pmax(x[["apy_groundnut-kharif"]] - irri_groundnut_kharif, minval)

    ## rape/mustard seed columns
    irri_rape_and_mustard_rabi   = pmax(pmin(x[["irr_rape_and_mustard"]], x[["apy_rape_and_mustard-rabi"]]), minval)
    rain_rape_and_mustard_rabi   = pmax(x[["apy_rape_and_mustard-rabi"]] - irri_rape_and_mustard_rabi, minval)
    irri_rape_and_mustard_kharif = pmax(x[["irr_rape_and_mustard"]] - irri_rape_and_mustard_rabi, minval)
    rain_rape_and_mustard_kharif = pmax(x[["apy_rape_and_mustard-kharif"]] - irri_rape_and_mustard_kharif, minval)
    
    ## sesamum columns
    irri_sesamum_rabi   = pmax(pmin(x[["irr_sesamum"]], x[["apy_sesamum-rabi"]]), minval)
    rain_sesamum_rabi   = pmax(x[["apy_sesamum-rabi"]] - irri_sesamum_rabi, minval)
    irri_sesamum_kharif = pmax(x[["irr_sesamum"]] - irri_sesamum_rabi, minval)
    rain_sesamum_kharif = pmax(x[["apy_sesamum-kharif"]] - irri_sesamum_kharif, minval)
    
    ## soyabean columns
    irri_soyabean_rabi   = pmax(pmin(x[["irr_soyabean"]], x[["apy_soyabean-rabi"]]), minval)
    rain_soyabean_rabi   = pmax(x[["apy_soyabean-rabi"]] - irri_soyabean_rabi, minval)
    irri_soyabean_kharif = pmax(x[["irr_soyabean"]] - irri_soyabean_rabi, minval)
    rain_soyabean_kharif = pmax(x[["apy_soyabean-kharif"]] - irri_soyabean_kharif, minval)

    ## sunflower columns
    irri_sunflower_rabi   = pmax(pmin(x[["irr_sunflower"]], x[["apy_sunflower-rabi"]]), minval)
    rain_sunflower_rabi   = pmax(x[["apy_sunflower-rabi"]] - irri_sunflower_rabi, minval)
    irri_sunflower_kharif = pmax(x[["irr_sunflower"]] - irri_sunflower_rabi, minval)
    rain_sunflower_kharif = pmax(x[["apy_sunflower-kharif"]] - irri_sunflower_kharif, minval)

    ## other oil crops
    irri_other_oil_crops_rabi   = pmax(pmin(x[["irr_other_oil_crops"]], x[["apy_other_oil_crops-rabi"]]), minval)
    rain_other_oil_crops_rabi   = pmax(x[["apy_other_oil_crops-rabi"]] - irri_other_oil_crops_rabi, minval)
    irri_other_oil_crops_kharif = pmax(x[["irr_other_oil_crops"]] - irri_other_oil_crops_rabi, minval)
    rain_other_oil_crops_kharif = pmax(x[["apy_other_oil_crops-kharif"]] - irri_other_oil_crops_kharif, minval)
    
    ## banana columns
    banana_cols =
        c("apy_banana-kharif",
          "apy_banana-rabi",
          "apy_banana-summer",
          "apy_banana-whole_year")

    ## coconut columns
    coconut_cols =
        c("apy_coconut-kharif",
          "apy_coconut-whole_year")

    ## yams columns
    yams_cols =
        c("apy_yams-whole_year")

    ## sweet potato columns
    sweet_potato_cols =
        c("apy_sweet_potato-kharif",
          "apy_sweet_potato-rabi",
          "apy_sweet_potato-whole_year")

    ## potato columns
    potato_cols =
        c("apy_potato-kharif",
          "apy_potato-rabi",
          "apy_potato-summer",
          "apy_potato-winter",
          "apy_potato-whole_year")

    ## cassava columns
    cassava_cols =
        c("apy_cassava-kharif",
          "apy_cassava-rabi",
          "apy_cassava-whole_year")

    ## fruit and veg columns
    fruit_and_veg_cols =
        c("irr_fruit_and_veg",
          "apy_vegetables-kharif",
          "apy_vegetables-rabi",
          "apy_vegetables-summer",
          "apy_vegetables-whole_year",
          "apy_temperate_fruit-whole_year",
          "apy_tropical_fruits-kharif",
          "apy_tropical_fruits-rabi",
          "apy_tropical_fruits-whole_year")

    ## other fibre columns
    other_fibre_cols =
        c("apy_other_fibre_crops-kharif",
          "apy_other_fibre_crops-rabi",
          "apy_other_fibre_crops-autumn",
          "apy_other_fibre_crops-whole_year")

    ## cotton columns
    irri_cotton_rabi   = pmax(pmin(x[["irr_cotton"]], x[["apy_cotton-rabi"]]), minval)
    rain_cotton_rabi   = pmax(x[["apy_cotton-rabi"]] - irri_cotton_rabi, minval)
    irri_cotton_kharif = pmax(x[["irr_cotton"]] - irri_cotton_rabi, minval)
    rain_cotton_kharif = pmax(x[["apy_cotton-kharif"]] - irri_cotton_kharif, minval)
    
    ## coffee columns
    coffee_cols =
        c("apy_coffee-whole_year")

    ## tobacco columns
    tobacco_cols =
        c("apy_tobacco-kharif",
          "apy_tobacco-rabi",
          "apy_tobacco-summer",
          "apy_tobacco-whole_year")

    ## tea columns
    tea_cols =
        c("apy_tea-kharif",
          "apy_tea-whole_year")

    ## rest of crops
    rest_cols =
        c("apy_rest_of_crops-kharif",
          "apy_rest_of_crops-rabi",
          "apy_rest_of_crops-autumn",
          "apy_rest_of_crops-summer",
          "apy_rest_of_crops-whole_year")
    
}







## not used:


## district_energy_frac <- function(x, y, state, na.ix, glm=TRUE, level=cut(1950:2012, seq(1953,2013,5), right=F), plot=TRUE) {

##     efrac <- y$Electric_Pumpsets / (y$Diesel_Pumpsets + y$Electric_Pumpsets)
    
##     if (!missing(na.ix)) {
##         efrac[na.ix] <- NA
##     }
    
##     if (glm) {
##         efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
##         efrac.predict <- predict(efrac.glm, data.frame(x=x))

##     } else {
##         efrac.predict <- zoo::na.approx(efrac, na.rm=FALSE)
##         efrac.predict <- zoo::na.locf(efrac.predict, na.rm=FALSE)
##         efrac.predict <- zoo::na.locf(efrac.predict, fromLast=TRUE)
##     }

##     efrac.predict[efrac.predict < 0] <- 0
##     efrac.predict[efrac.predict > 1] <- 1
##     dfrac.predict <- 1 - efrac.predict

##     efrac.predict.5y <- aggregate(efrac.predict, by=list(level), FUN=mean)[,2]
##     efrac.predict.5y <- efrac.predict.5y[as.numeric(level)]
##     dfrac.predict.5y <- 1 - efrac.predict.5y
    
##     if (plot) {
##         plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
##         lines(x, efrac.predict, col=rainbow(2)[1])
##         lines(x, dfrac.predict, col=rainbow(2)[2])

##         lines(x, efrac.predict.5y, col="black", lwd=2)
##         lines(x, dfrac.predict.5y, col="black", lwd=2)
        
##         legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
##     }

##     efrac <- efrac.predict
##     dfrac <- dfrac.predict

##     out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
##     out

## }

## state_energy_frac <- function(x, y, state, na.ix, glm=FALSE, plot=TRUE) {
##     ## Function to estimate division between diesel/electric groundwater pumps
##     ## using data from the Minor Irrigation Census
##     ##
##     ## Args:
##     ##   x...
##     ##   y...
##     ##   state: name of state
##     ##   na.ix...
##     ##   glm: logical. Fit a straight line through the data?
##     ##   plot: logical. Plot the data?
##     ##
##     ## Return:
##     ##   A data.frame.

##     ## restrict data to state under consideration
##     y <- y[y$State %in% state,]

##     ## calculate electric/diesel fraction and fit trend line
##     efrac <- y$Electric / (y$Electric + y$Diesel)  ## electric fraction

##     if (!missing(na.ix)) {
##         efrac[na.ix] <- NA
##     }
    
##     if (glm) {
##         efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
##         efrac.predict <- predict(efrac.glm, data.frame(x=x))

##     } else {
##         efrac.predict <- zoo::na.approx(efrac, na.rm=FALSE)
##         efrac.predict <- zoo::na.locf(efrac.predict, na.rm=FALSE)
##         efrac.predict <- zoo::na.locf(efrac.predict, fromLast=TRUE)
##     }

##     efrac.predict[efrac.predict < 0] <- 0
##     efrac.predict[efrac.predict > 1] <- 1
##     dfrac.predict <- 1 - efrac.predict

##     if (plot) {
##         plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
##         lines(x, efrac.predict, col=rainbow(2)[1])
##         lines(x, dfrac.predict, col=rainbow(2)[2])
##         legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
##     }

##     efrac <- efrac.predict
##     dfrac <- dfrac.predict

##     out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
##     out

##     ## if (!missing(na.ix)) {
##     ##     efrac[na.ix] <- NA
##     ## }
    
##     ## if (plot) {
##     ##     plot(x, efrac, ylim=c(0,1), xlab="Year",ylab="",col=rainbow(2)[1])
##     ## }

##     ## if (glm) {
##     ##     efrac.glm <- glm(efrac~x, data=data.frame(x=x, efrac=efrac))
##     ##     efrac.predict <- predict(efrac.glm, data.frame(x=x))
##     ##     efrac.predict[efrac.predict < 0] <- 0
##     ##     efrac.predict[efrac.predict > 1] <- 1
##     ##     dfrac.predict <- 1 - efrac.predict

##     ##     if (plot) {
##     ##         lines(x, efrac.predict, col=rainbow(2)[1])
##     ##         lines(x, dfrac.predict, col=rainbow(2)[2])
##     ##     }
##     ##     efrac <- efrac.predict
##     ##     dfrac <- dfrac.predict

##     ## } else {
##     ##     efrac <- zoo::na.approx(efrac, na.rm=FALSE)
##     ##     efrac <- zoo::na.locf(efrac, na.rm=FALSE)
##     ##     efrac <- zoo::na.locf(efrac, fromLast=TRUE)
##     ##     dfrac <- 1 - efrac
##     ##     if (plot) {
##     ##         lines(x, efrac, col=rainbow(2)[1])
##     ##         lines(x, dfrac, col=rainbow(2)[2])
##     ##     }
##     ## }

##     ## if (plot) {
##     ##     legend("topleft", legend=c("Electric_Frac","Diesel_Frac"), lty=1, col=rainbow(2))
##     ## }

##     ## out <- data.frame(Year=x, Electric_Frac=efrac, Diesel_Frac=dfrac)
##     ## out

## }

## state_fv_frac <- function(x, y, state, na.ix, glm=FALSE, plot=TRUE) {

##     ## restrict data to state under consideration
##     y <- y[y$State %in% state,]

##     veg.frac <- y$Veg_Frac

##     if (!missing(na.ix)) {
##         veg.frac[na.ix] <- NA
##     }

##     if (glm) {
##         veg.frac.glm <- glm(veg.frac~x, data=data.frame(veg.frac=veg.frac, x=x))
##         veg.frac.predict <- predict(veg.frac.glm, data.frame(x=x))
##     } else {
##         veg.frac.predict <- zoo::na.approx(veg.frac, na.rm=FALSE)
##         veg.frac.predict <- zoo::na.locf(veg.frac.predict, na.rm=FALSE)
##         veg.frac.predict <- zoo::na.locf(veg.frac.predict, fromLast=TRUE)
##     }

##     veg.frac.predict[veg.frac.predict < 0] <- 0
##     veg.frac.predict[veg.frac.predict > 1] <- 1
##     fruit.frac.predict <- 1 - veg.frac.predict
    
##     if (plot) {
##         plot(x,veg.frac,ylim=c(0,1), type="p", col=rainbow(3)[1], xlab="Year",ylab="")
##         lines(x, veg.frac.predict, col=rainbow(2)[1])
##         lines(x, fruit.frac.predict, col=rainbow(2)[2])
##         legend("topleft", legend=c("Veg_Frac","Fruit_Frac"), lty=1, col=rainbow(2))
##     }

##     veg.frac <- veg.frac.predict
##     fruit.frac <- fruit.frac.predict
##     out <- data.frame(Year=x, Veg_Frac=veg.frac, Fruit_Frac=fruit.frac)
##     out
## }

## write_demand_scenario <- function(path, y) {
##     dist <- unique(y[,toupper(names(y)) %in% "ID"])
##     if (length(dist) != 1) {
##         stop("only data for one district should be provided")
##     }
##     saveRDS(y, file.path(path, paste0("lulc_demand_", formatC(dist, width=3, flag="0"), ".rds")))
##     y
## }
    
## write_indiam_input <- function(path, y, fv_frac, gw_frac, energ_frac, level=cut(1950:2012, seq(1953,2013,5), right=F), ...) {
    
##     dist <- unique(y[,toupper(names(y)) %in% "ID"])
##     if (length(dist) != 1) {
##         stop("only data for one district should be provided")
##     }

##     fruit <- y$Fruit_and_Veg * fv_frac$Fruit_Frac
##     veg   <- y$Fruit_and_Veg * fv_frac$Veg_Frac
    
##     y <- y[,names(y) %in% c("ID","Rice_Autumn","Rice_Winter","Rice_Summer","Sorghum_Kharif","Sorghum_Rabi","Bajra","Maize","Ragi","Wheat","Barley","Other_Cereals_Kharif","Other_Cereals_Rabi","Gram","Tur","Other_Pulses_Kharif","Other_Pulses_Rabi","Sugarcane","Condiments_and_Spices","Fruit_and_Veg","Other_Food_Crops","Groundnut","Sesamum","Rapeseed","Linseed","Other_Oilseed","Cotton","Tobacco","Fodder_Crops","Other_Nonfood_Crops")]

##     y <- cbind(y, data.frame(Fruit=fruit, Vegetables=veg, SW_Frac=gw_frac$SW_Frac, GW_Frac=gw_frac$GW_Frac, GW_Electric_Frac=energ_frac$Electric_Frac, GW_Diesel_Frac=energ_frac$Diesel_Frac))

##     for (i in 1:ncol(y)) {
##         if (is.numeric(y[,i])) {
##             yy <- y[,i]
##             yy[yy < 0] <- 0
##             yy[is.na(yy)] <- 0
##             y[,i] <- yy
##         }
##     }

##     ## aggregate
##     ave.y <- aggregate(y, by=list(level), FUN=mean)
##     min.y <- aggregate(y, by=list(level), FUN=min)
##     max.y <- aggregate(y, by=list(level), FUN=max)

##     midpoints <- function(x, ...) {
##         lower=as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
##         upper=as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
##         return(floor(lower + (upper-lower) / 2))
##     }

##     yrs <- sort(unique(midpoints(level)))
##     yrs <- paste(yrs, substr(yrs+1, start=3, stop=4), sep="-")
##     ave.y <- cbind(data.frame(Year=yrs), ave.y)
##     min.y <- cbind(data.frame(Year=yrs), min.y)
##     max.y <- cbind(data.frame(Year=yrs), max.y)

##     ## save data frames
##     saveRDS(ave.y, file.path(path, paste0("ave_irrigation_demand_", formatC(dist, width=3, flag="0"), ".rds")))

##     saveRDS(min.y, file.path(path, paste0("min_irrigation_demand_", formatC(dist, width=3, flag="0"), ".rds")))

##     saveRDS(max.y, file.path(path, paste0("max_irrigation_demand_", formatC(dist, width=3, flag="0"), ".rds")))
##     y
## }

## ## getDistrictData <- function(data, dist, cols) {  ## TODO: change this function to accept numeric or character

## ##     if (missing(dist)) {
## ##         stop("must supply district code")
## ##     }

## ##     if (missing(cols)) {
## ##         cols <- seq(1, ncol(data))  ## all columns
## ##     }

## ##     if (is.numeric(dist))   ix <- which(data[,3] %in% dist)
## ##     if (is.character(dist)) ix <- which(data[,2] %in% dist)
    
## ##     if (length(ix) < 1) {
## ##         stop(paste0("no data for district ", dist))
## ##     }
    
## ##     d <- data[ix,cols]  ## subset based on district    
## ##     d
## ## }

## ## getDistrictLandUseData <- function(data, dist) {
## ##     d     <- getDistrictData(data, dist=dist, cols=c(1:6,7:9,11:13,15:16,18))               
## ##     d[,6] <- apply(d[,7:15], 1, sum, na.rm=TRUE) 
## ##     names(d) <- c("STATE","DISTRICT","ID","YEAR","AREA","TOTAL","FOREST","NONAGRI","BARREN","PPASTUR","TREE CROPS","CWASTE","OTFALOW","CUFALOW","NCA")
## ##     d
## ## }

## ## getDistrictIrrigationData <- function(data, dist) { 
## ##     d <- getDistrictData(data, dist=dist, cols=c(1:5,15:76))                       
## ##     d
## ## }    

## #################################################################################

## updateLandUseTotal <- function(data) {
##     data$TOTAL <- apply(data[,4:12], 1, sum, na.rm=TRUE)
##     data
## }

## fillDataWithICRISAT <- function(d1, d2) {

##     dists <- unique(d1$DISTRICT)
##     data <- list()

##     if (!all(names(d1) == names(d2))) {
##         stop("data.frame names must be the same")
##     }
    
##     for (i in 1:length(dists)) {
##         ix1 <- which(d1$DISTRICT %in% dists[i])
##         dd1 <- d1[ix1,]
        
##         if (dists[i] %in% d2$DISTRICT) {
##             ix2 <- which(d2$DISTRICT %in% dists[i])
##             dd2 <- d2[ix2,]
##             for (j in 1:nrow(dd1)) {
##                 for (k in 1:ncol(dd1)) {
##                     if (is.na(dd1[j,k])) {
##                         dd1[j,k] <- dd2[j,k]
##                     }
##                 }
##             }
##         }

##         data[[i]] <- dd1
##     }

##     data <- do.call(rbind, data)
##     data
## }

## updateIrrigationTotals <- function(d) {

##     ## Total fallow
##     x <- apply(d[,c(4:5)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(4:5)], 1, function(x) all(is.na(x))))] <- NA
##     d[,6] <- x

##     ## Total canal NIA
##     x <- apply(d[,c(10:11)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(10:11)], 1, function(x) all(is.na(x))))] <- NA
##     d[,12] <- x

##     ## Total NIA
##     x <- apply(d[,c(10:11,13:16)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(10:11,13:16)], 1, function(x) all(is.na(x))))] <- NA
##     d[,17] <- x
    
##     ## Total canal GIA
##     x <- apply(d[,c(18:19)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(18:19)], 1, function(x) all(is.na(x))))] <- NA
##     d[,20] <- x

##     ## Total GIA
##     x <- apply(d[,c(18:19,21:24)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(18:19,21:24)], 1, function(x) all(is.na(x))))] <- NA
##     d[,25] <- x
    
##     ## Total rice
##     x <- apply(d[,c(26:28)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(26:28)], 1, function(x) all(is.na(x))))] <- NA
##     d[,29] <- x

##     ## Total cholum
##     x <- apply(d[,c(30:31)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(30:31)], 1, function(x) all(is.na(x))))] <- NA
##     d[,32] <- x

##     ## Total other cereals
##     x <- apply(d[,c(38:39)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(38:39)], 1, function(x) all(is.na(x))))] <- NA
##     d[,40] <- x

##     ## Total cereals/millets
##     x <- apply(d[,c(29,32,33:37,40)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(29,32,33:37,40)], 1, function(x) all(is.na(x))))] <- NA
##     d[,41] <- x

##     ## Total other pulses
##     x <- apply(d[,c(44:45)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(44:45)], 1, function(x) all(is.na(x))))] <- NA
##     d[,46] <- x

##     ## Total pulses
##     x <- apply(d[,c(42,43,46)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(42,43,46)], 1, function(x) all(is.na(x))))] <- NA
##     d[,47] <- x

##     ## Total food grains
##     x <- apply(d[,c(41,47)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(41,47)], 1, function(x) all(is.na(x))))] <- NA
##     d[,48] <- x

##     ## Total food
##     x <- apply(d[,c(48:52)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(48:52)], 1, function(x) all(is.na(x))))] <- NA
##     d[,53] <- x

##     ## Total oilseed
##     x <- apply(d[,c(54:58)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(54:58)], 1, function(x) all(is.na(x))))] <- NA
##     d[,59] <- x

##     ## Total nonfood
##     x <- apply(d[,c(59:63)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(59:63)], 1, function(x) all(is.na(x))))] <- NA
##     d[,64] <- x

##     ## Total irrigation
##     x <- apply(d[,c(53,64)], 1, sum, na.rm=TRUE)
##     x[which(apply(d[,c(53,64)], 1, function(x) all(is.na(x))))] <- NA
##     d[,65] <- x

##     d
## }

## ## ##############################################################################
## ## Possibly redundant functions
## ## ##############################################################################

## ## getStateLandUseData <- function(data, state, adm) {

## ##     if (missing(state)) {
## ##         stop("must supply district code")
## ##     }

## ##     if (length(state) > 1) {
## ##         stop("supply one state only")
## ##     }
    
## ##     ix <- which(data$State %in% state)
## ##     if (length(ix) < 1) {
## ##         stop(paste0("no data for state ", state))
## ##     }
    
## ##     data <- data[ix, ]  ## subset based on state
## ##     dist <- unique(data$District)
## ##     d <- getDistrictLandUseData(data=data, dist=dist, adm=adm)

## ##     dd <- as.data.frame(matrix(data=NA, nrow=63, ncol=13))
## ##     names(dd) <- names(d)[c(1,3:14)]
## ##     dd$STATE <- state
## ##     yrs <- unique(d$YEAR)
## ##     ## yrs  <- paste0(seq(from=1950, to=2012), "-", c(seq(from=51, to=99), formatC(seq(from=0, to=13), width=2, flag="0")))
    
## ##     for (j in 1:length(yrs)) {
## ##         dd1 <- d[which(d$YEAR %in% yrs[j]), 4:14]
## ##         dd[j,3:13] <- apply(dd1, 2, sum, na.rm=TRUE)
## ##     }

## ##     dd$YEAR <- yrs
## ##     dd
    
## ## }

## ## plotIrrigationData <- function(data) { ##, what="sia_nia") {

## ##     if ("DISTRICT" %in% names(data)) {
## ##         dist <- unique(data$DISTRICT)
## ##         if (length(dist) > 1) {
## ##             print(paste0("Warning: only plotting data for district ", dist[1]))
## ##         }
## ##         data <- data[(data$DISTRICT %in% dist[1]), ]
## ##     }

## ##     d <- data

## ##     ## if (what %in% c("sia_nia")) {
## ##     ##     d <- data[,c(10:17)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_NIA=apply(d[,c(1,2,4,5,6,7)], 1, sum, na.rm=TRUE)))
## ##     ## }
    
## ##     ## if (what %in% c("sia_gia")) {
## ##     ##     d <- data[,c(18:25)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_GIA=apply(d[,c(1,2,4,5,6,7)], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "rice") {
## ##     ##     d <- data[,c(26:29)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_RICE=apply(d[,c(1,2,3)], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "cholum") {
## ##     ##     d <- data[,c(30:32)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_CHOLUM=apply(d[,c(1,2)], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "misc_cereals") {
## ##     ##     d <- data[,c(33:37)]
## ##     ##     d <- cbind(d, data.frame(TOTAL=apply(d[,1:5], 1, sum, na.rm=TRUE)))
## ##     ## }
    
## ##     ## if (what %in% "other_cereals") {
## ##     ##     d <- data[,c(38:40)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_OTHERS=apply(d[,1:2], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "pulses") {
## ##     ##     d <- data[,c(42,43,46,47)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_PULSES=apply(d[,1:3], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "other_food") {
## ##     ##     d <- data[,c(49:52)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_OTHERS=apply(d, 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "oilseed") {
## ##     ##     d <- data[,c(54:59)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_OILSEED=apply(d[,1:5], 1, sum, na.rm=TRUE)))
## ##     ## }

## ##     ## if (what %in% "nonfood") {
## ##     ##     d <- data[,c(54:64)]
## ##     ##     d <- cbind(d, data.frame(TOTAL_NONFOOD=apply(d[,c(1:5),c(7:10)], 1, sum, na.rm=TRUE)))
## ##     ## }
    
## ##     d1 <- cbind(data.frame(YEAR=c(1951:2013)), d)
    
## ##     plotdata <- melt(d1, id.vars="YEAR")
## ##     p <- xyplot(value~YEAR, group=variable, data=plotdata, type="b", xlab="", ylab="", col=rainbow(ncol(d)), key=list(space="bottom", lines=list(col=rainbow(ncol(d))), text=list(names(d))))
## ##     p
## ## }

## ## getIPCCLandUseClasses <- function(data) {

## ##     ## URBAN
## ##     urban <- data$NONAGRI

## ##     ## CROPLAND
## ##     cropland <- data$NCA + data$CUFALOW + data$OTFALOW
    
## ##     ## FOREST
## ##     if ("TREE CROPS" %in% names(data)) {
## ##         tc <- data$"TREE CROPS"
## ##         tc[is.na(tc)] <- 0
## ##         forest <- data$FOREST + tc
## ##     } else {
## ##         forest <- data$FOREST
## ##     }

## ##     ## GRASSLAND
## ##     grassland <- data$PPASTUR

## ##     ## OTHER
## ##     other <- data$BARREN + data$CWASTE
    
## ##     lu <- data.frame(URBAN=urban,
## ##                      CROPLAND=cropland,
## ##                      FOREST=forest,
## ##                      GRASSLAND=grassland,
## ##                      OTHER=other)

## ##     if ("DISTRICT" %in% names(data)) {
## ##         data <- cbind(data[,c("STATE","DISTRICT","YEAR","AREA","TOTAL")], lu)
## ##     } else {
## ##         data <- cbind(data[,c("STATE","YEAR","AREA","TOTAL")], lu)
## ##     }

## ##     data
## ## }

## ## plotLandUseData <- function(data, cols=c("FOREST","NONAGRI","BARREN","PPASTUR","TREE CROPS","CWASTE","OTFALOW","CUFALOW","NCA","AREA","TOTAL")) {

## ##     if ("DISTRICT" %in% names(data)) {
## ##         dist <- unique(data$DISTRICT)
## ##         if (length(dist) > 1) {
## ##             print(paste0("Warning: only plotting data for district ", dist[1]))
## ##         }
## ##         data <- data[(data$DISTRICT %in% dist[1]), ]
## ##     }

## ##     if (!all(cols %in% names(data))) {
## ##         stop()
## ##     }
    
## ##     d <- data[ ,cols]
    
## ##     d <- cbind(data.frame(YEAR=c(1951:2013)), d)
    
## ##     plotdata <- melt(d, id.vars="YEAR")
## ##     p <- xyplot(value~YEAR, group=variable, data=plotdata, type="b", xlab="", ylab="", col=rainbow(length(cols)), key=list(space="bottom", lines=list(col=rainbow(length(cols))), text=list(cols)))
## ##     p
## ## }

## ## checkFertiliserConsumption <- function(x, y, fertiliser=c("N_TC","P_TC","K_TC"), na.ix, ..., plot=TRUE, legend.pos="topleft") {

## ##     nms <- names(y)

## ##     if (!all(fertiliser %in% nms)) {
## ##         stop("unrecognised crops")
## ##     }

## ##     ix <- which(names(y) %in% fertiliser)
## ##     d <- as.data.frame(y[,ix])
## ##     names(d) <- fertiliser
    
## ##     if (!missing(na.ix)) {
## ##         if (!all(names(na.ix) %in% names(d))) {
## ##             stop("'na.ix' must only refer to fertiliser types in 'fertiliser'")
## ##         }

## ##         for (i in 1:length(na.ix)) {
## ##             nm <- names(na.ix)[i]
## ##             d[na.ix[[i]],nm] <- NA
## ##         }
## ##     }

## ##     if (plot) {
## ##         dd <- d
## ##         ylab <- c("Consumption (Tonnes)")

## ##         matplot(x, dd, type="o", pch=1, lty=1, col=rainbow(length(fertiliser)), ylab=ylab, xlab="Year", ...)
## ##         legend(legend.pos, legend=fertiliser, pch=1, lty=1, col=rainbow(length(fertiliser)))
## ##     }

## ##     for (i in 1:length(fertiliser)) {
## ##         if (all(is.na(d[,i]))) {
## ##             d[,i] <- 0
## ##         }
## ##     }
    
## ##     ## dd <- try(zoo::na.approx(d, na.rm=FALSE), silent=FALSE)
## ##     ## if (!inherits(dd, "try-error")) {
## ##     ##     print("hello")
## ##     ##     d <- dd
## ##     ## }

## ##     for (i in 1:length(fertiliser)) {
## ##         vals <- try(zoo::na.approx(d[,i], na.rm=FALSE), silent=TRUE)
## ##         if (!inherits(vals, "try-error")) {
## ##             d[,i] <- vals
## ##         }
## ##     }
    
## ##     ## dd <- try(zoo::na.approx(d, na.rm=FALSE), silent=FALSE)
## ##     ## if (!inherits(dd, "try-error")) {
## ##     ##     print("hello")
## ##     ##     d <- dd
## ##     ## }

## ##     d <- zoo::na.locf(d, na.rm=FALSE)
## ##     d <- zoo::na.locf(d, fromLast=TRUE)
## ##     ## return(d)
    
## ##     if (plot) {
## ##         for (i in 1:length(fertiliser)) {
## ##             lines(x, d[,i], type="l", lty=2, col=rainbow(length(fertiliser))[i])
## ##         }
## ##     }

## ##     y[,ix] <- d
## ##     y

## ## }

## ## separateFruitVeg <- function(data, vegfrac) {
## ##     x <- data[,c("Fruits and Vegetables Inc. Root Crops")]
## ##     fruit <- x * (1-vegfrac)
## ##     veg <- x * vegfrac
## ##     data <- cbind(data, data.frame(Fruit=fruit, Vegetables=veg))
## ##     data
## ## }

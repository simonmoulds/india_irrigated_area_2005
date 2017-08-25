## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(raster)

options(stringsAsFactors = FALSE)

fs = list.files("data/gams", pattern="^gams_output_[0-9]+_[0-9]{4}_[a-z]+.txt$", full.names=TRUE)

for (i in 1:length(fs)) {
    fn = fs[i]
    dist = gsub("(gams_output)_([0-9]+)_([0-9]{4})_([a-z]+).txt", "\\2", basename(fn))
    year = gsub("(gams_output)_([0-9]+)_([0-9]{4})_([a-z]+).txt", "\\3", basename(fn))
    season = gsub("(gams_output)_([0-9]+)_([0-9]{4})_([a-z]+).txt", "\\4", basename(fn))
    dat = read_gams_output(fn)

    dist_frac_map = raster(file.path("data", "district_frac", paste0("dist_", dist, "_frac1_ll.tif")))
    dist_frac_pts = as(dist_frac_map, "SpatialPoints")
    
    
}


read_gams_output = function(fn, ...) {

    if (!file.exists(fn)) {
        stop("File does not exist")
    }
    
    x = readLines(fn)

    ## check for errors
    summary_ix = seq(grep("^\\*\\*\\*\\* REPORT SUMMARY", x), length.out=4)
    summary = x[summary_ix]
    n_error = summary[grep("ERRORS", summary)] %>% gsub("ERRORS", "", .) %>% trimws %>% as.numeric

    if (n_error > 0) {
        warning(paste0("File ", fn, " contains errors: exiting"))
        return()
    }

    ## restrict file to output
    start = grep("&START NEW MATRIX", x)
    end = grep("&END NEW MATRIX", x)
    x = x[seq(start, end)]

    ## remove empty lines
    x = x[nchar(x) > 0]

    ## get lines containing column header
    h1 = grep("CROP[0-9]+", x)[1]
    h2 = grep("^[ ]+\\+.*$", x) ## wrapped tables start with a '+' symbol
    header_lines = c(h1, h2)

    f1 = grep("^[ ]+\\+.*$", x)
    f2 = grep("&END NEW MATRIX", x)
    footer_lines = c(f1, f2)

    dat = vector(mode="list", length=length(header_lines))

    for (i in 1:length(header_lines)) {
        ix = seq(header_lines[i], footer_lines[i] - 1)
        xx = x[ix]
        xx[1] = gsub("\\+", " ", xx[1])
        dat[[i]] = read.table(textConnection(xx))
    }

    ## dat is a dataframe with the same characteristics as the data
    ## written to the GAMS script
    dat = do.call(cbind, dat)
    for (i in 1:ncol(dat)) {
        col = dat[,i]
        col[col %in% "zero"] = 0
        col = as.numeric(col)
        dat[,i] = col
    }

    dat
}


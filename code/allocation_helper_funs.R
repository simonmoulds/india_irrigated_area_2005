## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(raster)

write_gams_input = function(x, cropland_area, irri_area, fn, ...) {
    ## Function to write xml submission to NEOS server

    n_cell = nrow(x)
    n_crop = ncol(x)

    irri_ix = grepl("^irr_.*$", names(x))
    
    rownms = paste0("CELL", seq_len(n_cell))
    colnms = paste0("CROP", seq_len(n_crop))

    set_I  = paste0(c(rownms, "TOTAL_AR"), collapse=", ")
    set_I2 = paste0(rownms, collapse=", ")
    set_J  = paste0(c(colnms, "CRPLND_AR", "IRRI_AR"), collapse=", ")
    set_J2 = paste0(colnms, collapse=", ")
    set_J3 = paste0(colnms[irri_ix], collapse=", ")

    header = readLines("code/GPCEMA_header.txt")
    model  = readLines("code/GPCEMA_model.txt")

    ## open file
    con = file(fn, "w")

    cat("<document>\n", file=con)
    cat("<category>milp</category>\n", file=con)
    cat("<solver>CPLEX</solver>\n", file=con)
    cat("<inputType>GAMS</inputType>\n", file=con)
    cat("<client>Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.0@86.141.248.177</client>\n", file=con)
    cat("<priority>long</priority>\n", file=con)
    cat("<email>sim.moulds@gmail.com</email>\n", file=con)
    cat("<model><![CDATA[", file=con)
                                                                                    
    ## write model header
    writeLines(header, con)

    ## write sets, data
    cat("\n\nSET I Row labels", "/", set_I, "/;\n\n", file=con)
    cat("SET I2(I) Row labels without CTOTS", "/", set_I2, "/;\n\n", file=con)
    cat("SET J Column labels", "/", set_J, "/;\n", file=con)
    cat("SET J2(J) Column labels without RTOTS", "/", set_J2, "/;\n\n", file=con)
    cat("SET J3(J2) Column labels of irrigated crops", "/", set_J3, "/;\n\n", file=con)
    cat("ALIAS (I2,I2J);\n", file=con)
    cat("ALIAS (J2,J2J);\n", file=con)
    cat("\n", file=con)
    cat("TABLE\n", file=con)
    cat("    DATA(I,J)\n", file=con)
    cat(formatC(c("", colnms, "CRPLND_AR", "IRRI_AR"), width=12), "\n", file=con)
    for (i in 1:n_cell) {
        vals = x[i,] %>% as.numeric %>% unname %>% round(digits=3) %>% as.character
        rt1 = cropland_area[i] %>% round(digits=3) %>% as.character
        rt2 = irri_area[i] %>% round(digits=3) %>% as.character
        cat(formatC(c(rownms[i], vals, rt1, rt2), width=12), "\n", file=con)
    }

    col_target = colSums(x) %>% unname %>% round(digits=3)
    cat(formatC(c("TOTAL_AR", col_target), width=12), "\n", file=con)
    cat(";\n\n", file=con)

    ## add model code and close the connection
    writeLines(model, con)

    ## finish XML, then close file
    cat("\n]]></model>\n", file=con)
    cat("<options><![CDATA[]]></options>\n", file=con)
    cat("<gdx><![CDATA[]]></gdx>\n", file=con)
    cat("<comments><![CDATA[]]></comments>\n", file=con)
    cat("</document>\n", file=con)
    close(con)
}

any_user_errors = function(x, ...) {
    any(grepl("^\\*\\*\\*\\* USER ERROR\\(S\\) ENCOUNTERED$", x))
}

any_runtime_errors = function(x, ...) {

    if (any_user_errors(x)) {
        warning("Model was not executed")
        return(FALSE)
    }

    if (any(grepl("REPORT SUMMARY", x))) {
        n_error = x[grep("\\s+[0-9]+\\s+ERRORS$", x)] %>% gsub("ERRORS", "", .) %>% trimws %>% as.numeric
        res = n_error > 0
    } else {
        res = FALSE
    }
    res
}

any_server_errors = function(x, ...) {
    isTRUE(attr(x, "status") == 1)
}

adjust_totals = function(x, cropland_area, irri_area, ...) {
    ## This function adjusts totals of (irrigated) cropland to ensure
    ## a solution is possible
    
    cropland_avail = sum(cropland_area)
    irrigated_avail = sum(irri_area)

    irri_ix = grep("^irr_", names(x))
    cropland_dmd = sum(colSums(x))
    irrigated_dmd = sum(colSums(x)[irri_ix])

    if (cropland_dmd > cropland_avail) {
        sf = cropland_avail / cropland_dmd
        x = x * sf
    }

    if (irrigated_dmd > irrigated_avail) {
        sf = irrigated_avail / irrigated_dmd
        x[,irri_ix] = x[,irri_ix] * sf
    }

    x
}

read_gams_output = function(x, ...) {

    ## restrict file to output
    start = grepl("&START NEW MATRIX", x)
    end = grepl("&END NEW MATRIX", x)
    if (!any(start) || !any(end)) {
        stop("File does not contain results matrix")
    } else {
        start %<>% which
        end %<>% which
    }
    
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
        dat[[i]] = read.table(textConnection(xx), stringsAsFactors = FALSE)
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

    dat = dat %>% dplyr::select(-(CRPLND_AR:IRRI_AR)) %>% dplyr::slice(-n())
    dat
}

allocate_fun = function(x, dist, year, season, cropland_area, irri_area, dir, write_output=TRUE, ...) {
    ## Function to perform allocation

    x = adjust_totals(x, cropland_area, irri_area)
    
    if (sum(colSums(x)) > 0) {

        fn = file.path(dir, paste0("gams_input_", dist, "_", year, "_", season, ".xml"))
        write_gams_input(x, cropland_area, irri_area, fn)

        ## now execute script and retrieve output
        print(paste0("Executing ", fn, "..."))
        res = system2(command="code/PythonClient/NeosClient.py", args=fn, stdout=TRUE, stderr=TRUE)

        if (any_user_errors(res) || any_runtime_errors(res) || any_server_errors(res)) {
            stop("Allocation did not run properly")
        } else {
            x2 = read_gams_output(res)
            names(x2) = names(x)
        }

        if (write_output) {
            con = file(file.path(dir, paste0("gams_output_", dist, "_", year, "_", season, ".txt")))
            writeLines(res, con)
            close(con)
        }

    } else {
        x2 = x
    }
    x2
}

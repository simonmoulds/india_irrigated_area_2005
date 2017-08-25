## Author : Simon Moulds
## Date   : August 2017

fs = list.files("data/gams", "^gams_input_[0-9]+_[0-9{4}_[a-z]+.xml$", full.names = TRUE)

for (i in 1:length(fs)) {
    f = fs[i]
    dist = gsub("(gams_input)_([0-9]+)_([0-9]{4})_([a-z]+).xml", "\\2", basename(f))
    year = gsub("(gams_input)_([0-9]+)_([0-9]{4})_([a-z]+).xml", "\\3", basename(f))
    season = gsub("(gams_input)_([0-9]+)_([0-9]{4})_([a-z]+).xml", "\\4", basename(f))
    outfn = paste0("gams_output_", dist, "_", year, "_", season, ".txt")
    cmd = paste0("code/PythonClient/NeosClient.py ", f, " > ", "data/gams/", outfn)
    print(paste0("Executing ", f, "..."))
    system(cmd)
}

library(data.table)

noaa_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"

temp <- tempfile(fileext = ".txt")
download.file(noaa_url, temp, method = 'curl')

noaa_data <- fread(temp)

devtools::use_data(noaa_data, overwrite = TRUE)
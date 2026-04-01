library(jsonlite)

found_data_info <- lapply(fromJSON("C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU.example\\custom\\shared\\check-open_icu-to-ricu.json", simplifyVector=FALSE), function(x) toJSON(x, auto_unbox=TRUE))

for (values in found_data_info) {
  parsed <- fromJSON(values)
  if(parsed[1,]$short == "abx"){
    print("stop")
  }
}
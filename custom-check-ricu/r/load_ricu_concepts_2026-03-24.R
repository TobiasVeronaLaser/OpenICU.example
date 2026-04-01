library(ricu)
library(jsonlite)

output <- list()

# JSON laden
found_data_info <- lapply(fromJSON("C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU.example\\custom\\shared\\check-open_icu-to-ricu.json", simplifyVector=FALSE), function(x) toJSON(x, auto_unbox=TRUE))

for (values in found_data_info){
  info <- fromJSON(values)

  self_cre_cnpt_cnt = 0
  cnpt_from_evt_cnt = 0
  cnpt_cnt = 0

  cnpt <- load_concepts(info[1,]$short, src="miiv", aggregate=FALSE, interval=secs(1), id_type="patient")
  for(i in seq_along(info[, 1])){
    e = info[i, ]
    min_val <- if (length(e$min) > 0) as.numeric(e$min) else NULL
    max_val <- if (length(e$max) > 0) as.numeric(e$max) else NULL
  }
}
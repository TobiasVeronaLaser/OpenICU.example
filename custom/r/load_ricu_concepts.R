library(ricu)
library(jsonlite)

output <- list()

# JSON laden
found_data_info <- lapply(fromJSON("C:\\Users\\tobi\\Documents\\check-open_icu-to-ricu.json", simplifyVector=FALSE), function(x) toJSON(x, auto_unbox=TRUE))

local_patient <- as.data.frame(miiv$patients[, c("subject_id", "anchor_year", "anchor_age")])
local_patient$birthdate <- as.POSIXct(
  paste0(local_patient$anchor_year - local_patient$anchor_age, "-01-01 00:00:00"),
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
)

blacklist = c("abx")
wanted_list = c("ca")

for (values in found_data_info) {
  parsed <- fromJSON(values)
  
  self_created_concept_count = 0
  cnpt_from_event_count = 0
  cnpt_count = 0
  
  if (1 == length(parsed$short)){
    if (parsed$short %in% blacklist) next
    # if (!parsed$short %in% wanted_list ) next
    print(parsed$short)
    
    min_val <- if (length(parsed$min) > 0) as.numeric(parsed$min) else NULL
    max_val <- if (length(parsed$max) > 0) as.numeric(parsed$max) else NULL
    
    cnpt <- load_concepts(parsed$short, src="miiv", aggregate=FALSE, interval=secs(1), id_type="patient")
    cnpt_count = nrow(cnpt)
    
    cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
    cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
    
    tbl <- miiv[[parsed$table]][, c("itemid", "subject_id", "valuenum")]
    
    itemid_mask <- tbl[["itemid"]] %in% parsed$ids
    
    cnpt_from_event <- tbl[itemid_mask, ]
    cnpt_from_event_count = nrow(cnpt_from_event)
    
    # rm(tbl)
    
    self_created_concept <- cnpt_from_event[!is.na(valuenum) & valuenum >= min_val & valuenum <= max_val]
    self_created_concept_count = nrow(self_created_concept)
    
    # rm(cnpt, cnpt_from_event, self_created_concept, itemid_mask)
    # gc()
    
    
    
    if (cnpt_count == self_created_concept_count) {
      print(paste(parsed$short, ": works"))
    } else {
      print(paste(parsed$short, ": NOT", cnpt_count, "vs", self_created_concept_count))
    }
    
    
    output[[parsed$name]] = list(
      name = parsed$name,
      is_equal_in_ricu = cnpt_count == self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count
    )
    
  }else if (1 < length(parsed$short)){
    if (parsed$short[1] %in% blacklist) next
    # if (!parsed$short[1] %in% wanted_list ) next
    print(parsed$short[1])
    
    for (i in 1:length(parsed$short)){
      
      min_val <- if (length(parsed$min[i]) > 0) as.numeric(parsed$min[i]) else NULL
      max_val <- if (length(parsed$max[i]) > 0) as.numeric(parsed$max[i]) else NULL
      
      cnpt <- load_concepts(parsed$short[i], src="miiv", aggregate=FALSE, interval=secs(1), id_type="patient")
      cnpt_count = cnpt_count + nrow(cnpt)
      
      cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
      cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
      
      tbl <- miiv[[parsed$table[i]]][, c("itemid", "subject_id", "valuenum")]
      
      itemid_mask <- tbl[["itemid"]] %in% parsed$ids[i]
      
      cnpt_from_event <- tbl[itemid_mask, ]
      cnpt_from_event_count = cnpt_from_event_count + nrow(cnpt_from_event)
      
      # rm(tbl)
      
      self_created_concept <- cnpt_from_event[!is.na(valuenum) & valuenum >= min_val & valuenum <= max_val]
      self_created_concept_count = self_created_concept_count + nrow(self_created_concept)
    }
    # rm(cnpt, cnpt_from_event, self_created_concept, itemid_mask)
    # gc()
    
    if (cnpt_count == self_created_concept_count) {
      print(paste(parsed$short, ": works"))
    } else {
      print(paste(parsed$short, ": NOT", cnpt_count, "vs", self_created_concept_count))
    }
    
    
    output[[parsed$name[1]]] = list(
      name = parsed$name,
      is_equal_in_ricu = cnpt_count == self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count
    )
    
  }else{
    
  }
  
  if (1 <= length(parsed$short)){
  }
  
  if (1 == length(parsed$short)){
  }else if (1 < length(parsed$short)){
  }
  # break
  # gc()
  
}

write_json(output, "C:\\Users\\tobi\\Documents\\output.json", pretty = TRUE, auto_unbox = TRUE)

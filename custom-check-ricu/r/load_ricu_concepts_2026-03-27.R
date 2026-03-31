library(ricu)
library(jsonlite)
library(data.table)

trace(
  "load_ts.src_tbl",
  where = asNamespace("ricu"),
  tracer = quote(browser()),
  at = 5,
  print = FALSE
)

output <- list()

# JSON laden

found_data_info <- lapply(fromJSON("C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU-Example\\custom\\shared\\check-open_icu-to-ricu-2026-03-27_12-57-28.json", simplifyVector=FALSE), function(x) toJSON(x, auto_unbox=TRUE))

local_patient <- as.data.frame(miiv$patients[, c("subject_id", "anchor_year", "anchor_age")])
local_patient$birthdate <- as.POSIXct(
  paste0(local_patient$anchor_year - local_patient$anchor_age, "-01-01 00:00:00"),
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
)

active_wanted_list = TRUE
wanted_list = c("abx")

self_created_concepts = list()
print(length(found_data_info))
for (values in found_data_info) {
  parsed <- fromJSON(values)
  
  self_created_concept_count = 0
  cnpt_from_event_count = 0
  cnpt_count = 0
  if (1 == length(parsed$short)){
    print(1)
    if (active_wanted_list & !parsed$short %in% wanted_list ) {
      next
    }
    print(parsed$short)
    
    cnpt <- load_concepts(parsed$short, src="miiv", aggregate=FALSE, interval=secs(1), id_type="patient")
    cnpt_count = nrow(cnpt)
    
    min_val <- if (length(parsed$min) > 0) as.numeric(parsed$min) else NULL
    max_val <- if (length(parsed$max) > 0) as.numeric(parsed$max) else NULL
    
    cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
    cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
    
    tbl = miiv[[parsed$table]]
    
    cols = c(parsed$sub_var, "subject_id")
    
    if(!is.null(parsed$time) && length(parsed$time) > 0){
      cols <- c(cols, parsed$time)
    }
    
    if( -Inf != min_val | Inf != max_val){
      cols = c(cols, "valuenum")
    }
    
    cols <- unique(cols)
    tbl = tbl[, cols]
    
    
    if(!is.null(parsed$ids) && length(parsed$ids) > 0){
      itemid_mask <- tbl[[parsed$sub_var]] %in% parsed$ids
    }else if(!is.null(parsed$src_regex) && length(parsed$src_regex) > 0){
      itemid_mask <- grepl(parsed$src_regex, tbl[[parsed$sub_var]], ignore.case = TRUE)
    } else {
      cat("!NULL vor", parsed$short)
      break
    }
    
    if (!is.null(parsed$time) && length(parsed$time) > 0 && parsed$time %in% names(tbl)) {
      setnames(tbl, parsed$time, "event_time")
    }
    
    # itemid_mask <- tbl[["itemid"]] %in% parsed$ids
    
    cnpt_from_event <- tbl[itemid_mask, ]
    cnpt_from_event_count = nrow(cnpt_from_event)
    
    # rm(tbl)
    
    if( -Inf != min_val | Inf != max_val){
      # self_created_concept <- cnpt_from_event[!is.na(valuenum) & valuenum >= min_val & valuenum <= max_val]
      self_created_concept <- cnpt_from_event[
        !is.na(cnpt_from_event$valuenum) &
          cnpt_from_event$valuenum >= min_val &
          cnpt_from_event$valuenum <= max_val,
      ]
    }else{
      self_created_concept <- cnpt_from_event
    }
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
      ricu_cnpt_size = cnpt_count,
      self_created_cnpt_size = self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count,
    )
    
  }else if (1 < length(parsed$short)){
    print(2)
    if (active_wanted_list & !parsed$short[1] %in% wanted_list ) {
      next
    }
    print(parsed$short[1])
    
    cnpt <- load_concepts(parsed$short[1], src="miiv", aggregate=FALSE, interval=secs(1), id_type="patient")
    cnpt_count = nrow(cnpt)
    
    for (i in seq_along(parsed$short)){
      
      min_val <- if (length(parsed$min[i]) > 0) as.numeric(parsed$min[i]) else NULL
      max_val <- if (length(parsed$max[i]) > 0) as.numeric(parsed$max[i]) else NULL
      
      cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
      # cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
      
      tbl = miiv[[parsed$table[i]]]
      
      cols = c(parsed$sub_var[i], "subject_id")
      
      if(!is.null(parsed$time[i]) && length(parsed$time[i]) > 0){
        cols <- c(cols, parsed$time[i])
      }
      
      if( -Inf != min_val | Inf != max_val){
        cols = c(cols, "valuenum")
      }
      
      cols <- unique(cols)
      tbl = tbl[, cols]
      
      if (!is.null(parsed$time[[i]]) && length(parsed$time[[i]]) > 0 && parsed$time[[i]] %in% names(tbl)) {
        setnames(tbl, parsed$time[[i]], "event_time")
      }
      
      if(!is.null(parsed$ids[[i]]) && length(parsed$ids[[i]]) > 0){
        itemid_mask <- tbl[[parsed$sub_var[i]]] %in% parsed$ids[[i]]
      }else if(!is.null(parsed$src_regex[[i]]) && length(parsed$src_regex[[i]]) > 0){
        itemid_mask <- grepl(parsed$src_regex[[i]], tbl[[parsed$sub_var[[i]]]], ignore.case = TRUE)
      } else {
        cat("!NULL vor", parsed$short[i])
        break
      }
      
      cnpt_from_event <- tbl[itemid_mask, ]
      cnpt_from_event_count = cnpt_from_event_count + nrow(cnpt_from_event)
      
      # rm(tbl)
      
      
      if( -Inf != min_val | Inf != max_val){
        # self_created_concept <- cnpt_from_event[!is.na(valuenum) & valuenum >= min_val & valuenum <= max_val]
        self_created_concept <- cnpt_from_event[
          !is.na(cnpt_from_event$valuenum) &
            cnpt_from_event$valuenum >= min_val &
            cnpt_from_event$valuenum <= max_val,
        ]
      }else{
        self_created_concept <- cnpt_from_event
      }
      
      self_created_concept_count = self_created_concept_count + nrow(self_created_concept)
      
      self_created_concepts = c(self_created_concepts, list(self_created_concept))
    }
    
    if (cnpt_count == self_created_concept_count) {
      print(paste(parsed$short, ": works"))
    } else {
      print(paste(parsed$short, ": NOT", cnpt_count, "vs", self_created_concept_count))
    }
    
    
    
    output[[parsed$name[1]]] = list(
      name = parsed$name,
      is_equal_in_ricu = cnpt_count == self_created_concept_count,
      ricu_cnpt_size = cnpt_count,
      self_created_cnpt_size = self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count,
    )
    
    #rm(cnpt, tbl, cnpt_from_event, self_created_concept, itemid_mask)
    #gc()
    
  }else{
    
  }
  # gc()
  
}
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

filename = paste0("C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU-Example\\custom\\shared\\output_", timestamp, ".json")

write_json(output, filename, pretty = TRUE, auto_unbox = TRUE)

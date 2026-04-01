library(ricu)
library(jsonlite)
library(data.table)

output <- list()

# JSON load
found_data_info <- lapply(
  fromJSON(
    "C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU.example\\custom\\shared\\check-open_icu-to-ricu-2026-03-27_12-57-28.json",
    simplifyVector = FALSE
  ),
  function(x) toJSON(x, auto_unbox = TRUE)
)

local_patient <- as.data.frame(miiv$patients[, c("subject_id", "anchor_year", "anchor_age")])
local_patient$birthdate <- as.POSIXct(
  paste0(local_patient$anchor_year - local_patient$anchor_age, "-01-01 00:00:00"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

active_wanted_list <- TRUE
wanted_list <- c("abx")

self_created_concepts <- list()

# -----------------------------
# Helpers
# -----------------------------

to_scalar_or_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (length(x) == 1 && is.na(x)) return(NULL)
  x[[1]]
}

to_numeric_or_null <- function(x) {
  x <- to_scalar_or_null(x)
  if (is.null(x)) return(NULL)
  as.numeric(x)
}

has_nonempty_value <- function(x) {
  !is.null(x) && length(x) > 0 && !(length(x) == 1 && is.na(x)) && !(is.character(x) && all(x == ""))
}

aggregate_to_minute <- function(dt) {
  dt <- as.data.table(copy(dt))
  
  if (!"event_time" %in% names(dt)) {
    return(dt)
  }
  
  # Convert event_time to minute-based difftime
  dt[, charttime := as.difftime(
    floor(as.numeric(event_time, units = "secs") / 60),
    units = "mins"
  )]
  
  # Aggregate to one row per subject_id and minute
  if ("valuenum" %in% names(dt)) {
    dt <- dt[
      ,
      .(valuenum = mean(valuenum, na.rm = TRUE)),
      by = .(subject_id, charttime)
    ]
    
    # mean(..., na.rm=TRUE) returns NaN if all values were NA
    dt[is.nan(valuenum), valuenum := NA_real_]
  } else {
    dt <- unique(dt[, .(subject_id, charttime)])
  }
  
  dt[]
}

build_mask <- function(tbl, sub_var, ids_value, regex_value) {
  if (has_nonempty_value(ids_value)) {
    return(tbl[[sub_var]] %in% ids_value)
  }
  
  if (has_nonempty_value(regex_value)) {
    return(grepl(regex_value, tbl[[sub_var]], ignore.case = TRUE))
  }
  
  return(NULL)
}

# -----------------------------
# Main loop
# -----------------------------

print(length(found_data_info))

for (values in found_data_info) {
  parsed <- fromJSON(values)
  
  self_created_concept_count <- 0
  cnpt_from_event_count <- 0
  cnpt_count <- 0
  
  if (1 == length(parsed$short)) {
    print(1)
    
    if (active_wanted_list && !parsed$short %in% wanted_list) {
      next
    }
    
    print(parsed$short)
    
    cnpt <- load_concepts(
      parsed$short,
      src = "miiv",
      aggregate = FALSE,
      interval = mins(1),
      id_type = "patient"
    )
    cnpt_count <- nrow(cnpt)
    
    min_val <- to_numeric_or_null(parsed$min)
    max_val <- to_numeric_or_null(parsed$max)
    has_range <- !is.null(min_val) || !is.null(max_val)
    
    cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
    cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
    
    tbl <- miiv[[parsed$table]]
    
    cols <- c(parsed$sub_var, "subject_id")
    
    if (has_nonempty_value(parsed$time)) {
      cols <- c(cols, parsed$time)
    }
    
    if (has_range) {
      cols <- c(cols, "valuenum")
    }
    
    cols <- unique(cols)
    tbl <- tbl[, cols]
    
    if (has_nonempty_value(parsed$time) && parsed$time %in% names(tbl)) {
      setnames(tbl, parsed$time, "event_time")
    }
    
    itemid_mask <- build_mask(
      tbl = tbl,
      sub_var = parsed$sub_var,
      ids_value = parsed$ids,
      regex_value = parsed$src_regex
    )
    
    if (is.null(itemid_mask)) {
      cat("!NULL vor", parsed$short, "\n")
      break
    }
    
    cnpt_from_event <- tbl[itemid_mask, ]
    cnpt_from_event_count <- nrow(cnpt_from_event)
    
    if (has_range) {
      if (is.null(min_val)) min_val <- -Inf
      if (is.null(max_val)) max_val <- Inf
      
      self_created_concept <- cnpt_from_event[
        !is.na(cnpt_from_event$valuenum) &
          cnpt_from_event$valuenum >= min_val &
          cnpt_from_event$valuenum <= max_val,
      ]
    } else {
      self_created_concept <- cnpt_from_event
    }
    
    # Aggregate to minute intervals if event_time exists
    self_created_concept <- aggregate_to_minute(self_created_concept)
    self_created_concept_count <- nrow(self_created_concept)
    
    if (cnpt_count == self_created_concept_count) {
      print(paste(parsed$short, ": works"))
    } else {
      print(paste(parsed$short, ": NOT", cnpt_count, "vs", self_created_concept_count))
    }
    
    output[[parsed$name]] <- list(
      name = parsed$name,
      is_equal_in_ricu = cnpt_count == self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count
    )
    
  } else if (1 < length(parsed$short)) {
    print(2)
    
    if (active_wanted_list && !parsed$short[1] %in% wanted_list) {
      next
    }
    
    print(parsed$short[1])
    
    cnpt <- load_concepts(
      parsed$short[1],
      src = "miiv",
      aggregate = FALSE,
      interval = mins(1),
      id_type = "patient"
    )
    cnpt_count <- nrow(cnpt)
    
    for (i in seq_along(parsed$short)) {
      min_val <- to_numeric_or_null(parsed$min[i])
      max_val <- to_numeric_or_null(parsed$max[i])
      has_range <- !is.null(min_val) || !is.null(max_val)
      
      cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
      # cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
      
      tbl <- miiv[[parsed$table[i]]]
      
      cols <- c(parsed$sub_var[i], "subject_id")
      
      if (has_nonempty_value(parsed$time[i])) {
        cols <- c(cols, parsed$time[i])
      }
      
      if (has_range) {
        cols <- c(cols, "valuenum")
      }
      
      cols <- unique(cols)
      tbl <- tbl[, cols]
      
      if (has_nonempty_value(parsed$time[i]) && parsed$time[i] %in% names(tbl)) {
        setnames(tbl, parsed$time[i], "event_time")
      }
      
      ids_value <- parsed$ids[[i]]
      regex_value <- parsed$src_regex[[i]]
      
      itemid_mask <- build_mask(
        tbl = tbl,
        sub_var = parsed$sub_var[i],
        ids_value = ids_value,
        regex_value = regex_value
      )
      
      if (is.null(itemid_mask)) {
        cat("!NULL vor", parsed$short[i], "\n")
        break
      }
      
      cnpt_from_event <- tbl[itemid_mask, ]
      cnpt_from_event_count <- cnpt_from_event_count + nrow(cnpt_from_event)
      
      if (has_range) {
        if (is.null(min_val)) min_val <- -Inf
        if (is.null(max_val)) max_val <- Inf
        
        self_created_concept <- cnpt_from_event[
          !is.na(cnpt_from_event$valuenum) &
            cnpt_from_event$valuenum >= min_val &
            cnpt_from_event$valuenum <= max_val,
        ]
      } else {
        self_created_concept <- cnpt_from_event
      }
      
      # Aggregate to minute intervals if event_time exists
      self_created_concept <- aggregate_to_minute(self_created_concept)
      
      self_created_concept_count <- self_created_concept_count + nrow(self_created_concept)
      self_created_concepts <- c(self_created_concepts, list(self_created_concept))
    }
    
    if (cnpt_count == self_created_concept_count) {
      print(paste(parsed$short, ": works"))
    } else {
      print(paste(parsed$short, ": NOT", cnpt_count, "vs", self_created_concept_count))
    }
    
    output[[parsed$name[1]]] <- list(
      name = parsed$name,
      is_equal_in_ricu = cnpt_count == self_created_concept_count,
      in_ricu_df_size = cnpt_from_event_count
    )
    
  } else {
    # do nothing
  }
}

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

filename <- paste0(
  "C:\\Users\\q039tl\\Documents\\OpenICU\\OpenICU.example\\custom\\shared\\output_",
  timestamp,
  ".json"
)

write_json(output, filename, pretty = TRUE, auto_unbox = TRUE)
library(ricu)
library(jsonlite)

output <- list()

# JSON laden
found_data_info <- lapply(
  fromJSON("C:\\Users\\tobi\\Documents\\check-open_icu-to-ricu.json", simplifyVector = FALSE),
  function(x) toJSON(x, auto_unbox = TRUE)
)

local_patient <- as.data.frame(miiv$patients[, c("subject_id", "anchor_year", "anchor_age")])
local_patient$birthdate <- as.POSIXct(
  paste0(local_patient$anchor_year - local_patient$anchor_age, "-01-01 00:00:00"),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

blacklist <- c()
wanted_list <- c("ca")

# Hier für Tabellen mit Regex die passende Spalte angeben
# Bei prescriptions ist es typischerweise "drug"
regex_col_map <- list(
  prescriptions = "drug"
)

is_non_null <- function(x) {
  !is.null(x) && length(x) > 0
}

is_effectively_null <- function(x) {
  is.null(x) || (is.list(x) && length(x) == 0)
}

is_present <- function(x) {
  !is_effectively_null(x)
}

get_table_subset <- function(tbl, use_regex = FALSE, regex_col = NULL) {
  base_cols <- c("subject_id", "valuenum")
  
  if (use_regex) {
    needed_cols <- unique(c(base_cols, regex_col))
  } else {
    needed_cols <- unique(c(base_cols, "itemid"))
  }
  
  needed_cols <- needed_cols[needed_cols %in% names(tbl)]
  tbl[, needed_cols, drop = FALSE]
}

# get_filtered_events <- function(tbl, ids = NULL, src_regex = NULL, regex_col = NULL) {
#   has_ids <- is_non_null(ids)
#   has_regex <- is_non_null(src_regex)
#   
#   if (has_ids && !has_regex) {
#     return(tbl[tbl[["itemid"]] %in% ids, , drop = FALSE])
#   }
#   
#   if (!has_ids && has_regex) {
#     if (is.null(regex_col)) {
#       stop("regex_col must be provided when src_regex is used.")
#     }
#     if (!regex_col %in% names(tbl)) {
#       stop(paste("Column", regex_col, "not found in table."))
#     }
#     
#     text_values <- tbl[[regex_col]]
#     text_values <- ifelse(is.na(text_values), "", as.character(text_values))
#     
#     return(tbl[grepl(src_regex, text_values, ignore.case = TRUE), , drop = FALSE])
#   }
#   
#   stop("Exactly one of ids or src_regex must be non-NULL.")
# }

get_filtered_events <- function(tbl, ids = NULL, src_regex = NULL, regex_col = NULL) {
  has_ids <- is_present(ids)
  has_regex <- is_present(src_regex)
  
  if (has_ids && !has_regex) {
    return(tbl[tbl[["itemid"]] %in% ids, , drop = FALSE])
  }
  
  if (!has_ids && has_regex) {
    if (is.null(regex_col)) {
      stop("regex_col must be provided when src_regex is used.")
    }
    if (!regex_col %in% names(tbl)) {
      stop(paste("Column", regex_col, "not found in table."))
    }
    
    text_values <- tbl[[regex_col]]
    text_values <- ifelse(is.na(text_values), "", as.character(text_values))
    
    return(tbl[grepl(src_regex, text_values, ignore.case = TRUE), , drop = FALSE])
  }
  
  stop("Exactly one of ids or src_regex must be present.")
}

apply_value_filter <- function(df, min_val = NULL, max_val = NULL) {
  if (!"valuenum" %in% names(df)) {
    return(df[0, , drop = FALSE])
  }
  
  res <- df[!is.na(df$valuenum), , drop = FALSE]
  
  if (!is.null(min_val)) {
    res <- res[res$valuenum >= min_val, , drop = FALSE]
  }
  
  if (!is.null(max_val)) {
    res <- res[res$valuenum <= max_val, , drop = FALSE]
  }
  
  res
}

for (values in found_data_info) {
  parsed <- fromJSON(values)
  
  self_created_concept_count <- 0
  cnpt_from_event_count <- 0
  cnpt_count <- 0
  
  if (1 == length(parsed$short)) {
    if (parsed$short %in% blacklist) next
    # if (!parsed$short %in% wanted_list) next
    
    print(parsed$short)
    
    min_val <- if (is_non_null(parsed$min)) as.numeric(parsed$min) else NULL
    max_val <- if (is_non_null(parsed$max)) as.numeric(parsed$max) else NULL
    
    cnpt <- load_concepts(
      parsed$short,
      src = "miiv",
      aggregate = FALSE,
      interval = secs(1),
      id_type = "patient"
    )
    cnpt_count <- nrow(cnpt)
    
    cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
    cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
    
    ids_val <- parsed$ids
    regex_val <- parsed$src_regex
    
    has_ids <- is_non_null(ids_val)
    has_regex <- is_non_null(regex_val)
    
    use_regex <- !has_ids && has_regex
    
    regex_col <- NULL
    if (use_regex) {
      regex_col <- regex_col_map[[parsed$table]]
      if (is.null(regex_col)) {
        stop(paste("No regex column configured for table:", parsed$table))
      }
    }
    
    tbl <- get_table_subset(
      miiv[[parsed$table]],
      use_regex = use_regex,
      regex_col = regex_col
    )
    
    print("ids_val")
    print(ids_val)
    print("ids_val")
    
    print("regex_val")
    print(regex_val)
    print("regex_val")
    
    cnpt_from_event <- get_filtered_events(
      tbl = tbl,
      ids = ids_val,
      src_regex = regex_val,
      regex_col = regex_col
    )
    cnpt_from_event_count <- nrow(cnpt_from_event)
    
    self_created_concept <- apply_value_filter(
      cnpt_from_event,
      min_val = min_val,
      max_val = max_val
    )
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
    if (parsed$short[1] %in% blacklist) next
    # if (!parsed$short[1] %in% wanted_list) next
    
    print(parsed$short[1])
    
    for (i in seq_along(parsed$short)) {
      min_val <- if (is_non_null(parsed$min[[i]])) as.numeric(parsed$min[[i]]) else NULL
      max_val <- if (is_non_null(parsed$max[[i]])) as.numeric(parsed$max[[i]]) else NULL
      
      cnpt <- load_concepts(
        parsed$short[i],
        src = "miiv",
        aggregate = FALSE,
        interval = secs(1),
        id_type = "patient"
      )
      cnpt_count <- cnpt_count + nrow(cnpt)
      
      cnpt$birthdate <- local_patient$birthdate[match(cnpt$subject_id, local_patient$subject_id)]
      cnpt$charttime_origin <- cnpt$birthdate + cnpt$charttime - 3600
      
      ids_val <- parsed$ids[[i]]
      regex_val <- parsed$src_regex[[i]]
      
      has_ids <- is_non_null(ids_val)
      has_regex <- is_non_null(regex_val)
      
      use_regex <- !has_ids && has_regex
      
      regex_col <- NULL
      if (use_regex) {
        regex_col <- regex_col_map[[parsed$table[i]]]
        if (is.null(regex_col)) {
          stop(paste("No regex column configured for table:", parsed$table[i]))
        }
      }
      
      tbl <- get_table_subset(
        miiv[[parsed$table[i]]],
        use_regex = use_regex,
        regex_col = regex_col
      )
      
      print("ids_val")
      print(ids_val)
      print("ids_val")
      
      print("regex_val")
      print(regex_val)
      print("regex_val")
      
      cnpt_from_event <- get_filtered_events(
        tbl = tbl,
        ids = ids_val,
        src_regex = regex_val,
        regex_col = regex_col
      )
      cnpt_from_event_count <- cnpt_from_event_count + nrow(cnpt_from_event)
      
      self_created_concept <- apply_value_filter(
        cnpt_from_event,
        min_val = min_val,
        max_val = max_val
      )
      self_created_concept_count <- self_created_concept_count + nrow(self_created_concept)
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
  }
}

write_json(
  output,
  "C:\\Users\\tobi\\Documents\\output.json",
  pretty = TRUE,
  auto_unbox = TRUE
)
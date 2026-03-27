if (!exists(".ricu_patch_applied")) {
  assign(".ricu_patch_applied", TRUE, envir = .GlobalEnv)
  
  unlockBinding("load_ts.src_tbl", asNamespace("ricu"))
  
  assign(
    "load_ts.src_tbl",
    function (x, rows, cols = colnames(x), id_var = id_vars(x), 
              index_var = ricu::index_var(x), interval = hours(1L), time_vars = ricu::time_vars(x), 
              ...) 
    {
      warn_dots(...)
      assert_that(is.string(index_var))
      if (!index_var %in% cols) {
        cols <- c(cols, index_var)
      }
      res <- load_difftime(x, {
        {
          rows
        }
      }, cols, id_var, time_vars)
      res <- as_ts_tbl(res, id_vars(res), index_var, mins(1L), 
                       by_ref = TRUE)
      time_vars <- intersect(time_vars, colnames(res))
      res <- change_id(res, id_var, x, cols = time_vars, keep_old_id = FALSE)
      res <- change_interval(res, interval, time_vars, by_ref = TRUE)
      res
    },
    envir = asNamespace("ricu")
  )
  
  lockBinding("load_ts.src_tbl", asNamespace("ricu"))
}
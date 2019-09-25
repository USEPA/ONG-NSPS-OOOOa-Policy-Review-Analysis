#' Standardize output of a table to an xlsx sheet 
#' 
#' Can find wb in the global environment if not passed
#' 
#' @export
write_tbl_to_xlsx <- function(x, wb, sheetName=NA_character_) {

  exprx <- rlang::enexpr(x)
  if (is.na(sheetName)) {sheetName <- rlang::as_string(exprx)}
  
    if (! sheetName %in% names(wb)) {
      openxlsx::addWorksheet(wb, sheetName)
    }
  
  openxlsx::writeData(wb, sheetName, x, 2, 2)
  invisible(TRUE)
}

#' Helper function to read in all sheets of an xlsx file, to be used in tests
#' 
read_all_xlsx_sheets <- function(path) {
  sheets <- readxl::excel_sheets(path)
  data <- purrr::map(sheets, ~readxl::read_excel(path=path, sheet = .x)) %>% 
    purrr::set_names(sheets)
}


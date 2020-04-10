#' Download Enrollment Data by Year from the CA DOE
#'
#' @param year String, in '2018-19' format
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' enr178 <- get_school_enr_data("2017-18")
#' }
get_school_enr_data <- function(year) {
  if (!grepl("[0-9]{4}-[0-9]{2}", year)) stop("year param must be in '2017-18' format", call. = FALSE)
  url <- glue::glue("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear={year}&cCat=Enrollment&cPage=filesenr.asp")
  download_and_read_cde_file(url)
}

#' Get school discipline data from the CA DOE
#'
#' @param year String, in '1718' format
#' @param category Either 'susp' for suspensions or 'exp' for expulsions
#' @importFrom readr read_tsv cols col_character
#' @return a large data frame
#' @export
#' @examples
#' \dontrun{
#' suspensions1718 <- get_school_discipline_data("1718", "susp")
#' }
get_school_discipline_data <- function(year, category) {
  if (!category %in% c('susp', 'exp')) stop('category param must be either susp (Suspensions) or exp (Expulsions)', call. = FALSE)
  if (!grepl('^[0-9]{4}$', year)) stop('year param must be in "1718" format', call. = FALSE)
  url <- glue::glue('ftp://ftp.cde.ca.gov/demo/discipline/{category}{year}.txt')
  tmp_file <- tempfile()
  download.file(url = url, destfile = tmp_file)
  out <- readr::read_tsv(tmp_file,
                         col_types = cols(
                           .default = col_character(),
                           DistrictCode = col_character(),
                           SchoolCode = col_character(),
                           DistrictName = col_character(),
                           SchoolName = col_character(),
                           CharterYN = col_character())
  )
  file.remove(tmp_file)
  return(out)
}

#' Get graduate data from the CA DOE
#'
#' @param year String, in '2017-18' format
#' @param category String, either 'Graduates', 'GradEth', 'Dropouts', or 'UCGradEth'
#'
#' @return a large data frame
#' @export
#' @examples
#' \dontrun{
#' ucgrads178 <- get_school_grad_data("2017-18", "UCGradEth")
#' }
get_school_grad_data <- function(year, category) {
  if (!grepl("[0-9]{4}-[0-9]{2}", year)) stop("year param must be in '2017-18' format", call. = FALSE)

  if (!category %in% c('Graduates', 'GradEth', 'UCGradEth', 'Dropouts')) {
    stop("category param must be 'Graduates', 'GradEth', 'Dropouts', or 'UCGradEth.'", call. = FALSE)
  }
  url_suffix <- switch(category,
                       "Graduates" = "s",
                       "GradEth" = ".asp",
                       "UCGradEth" = "af.asp",
                       "Dropouts" = "dropouts")
  url <- glue::glue("https://dq.cde.ca.gov/dataquest/dlfile/dlfile.aspx?cLevel=School&cYear={year}&cCat={category}&cPage=files{url_suffix}")
  if (category == "Graduates") {
    url <- httr::modify_url(url, query = list(cLevel = "All"))
  }
  download_and_read_cde_file(url)
}

download_and_read_cde_file <- function(url) {
  tmp_file <- tempfile()
  download.file(url = url, destfile = tmp_file)
  out <- readr::read_tsv(tmp_file)
  file.remove(tmp_file)
  return(out)
}

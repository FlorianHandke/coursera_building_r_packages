#' Read fatal injuries data
#'
#' This is a functions which reads a CSV file with data
#' from the US National Highway Traffic Safety Administration's.
#' The file needs to be located in your working directory.
#'
#' @param filename A character string defing the filename to be loaded
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#'
#' @return A tibble object. If the filename does not excist an error returns.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(system.file("data", filename,
                              package = "farsdata")))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(system.file("data", filename,
                                package = "farsdata"), progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Creating a filename
#'
#' This function creates a filename by a given year.
#' Therefore the selected year is embedded in a standardised file name format
#'
#' @param year A integer value defining the corresponding year
#'
#' @return Returning a string with the full filename
#'
#' @examples
#' make_filename(2020)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reading multiple fars files
#'
#' Returning the column MONTH and year for given years.
#' \code{\link{make_filename}} and \code{\link{fars_read}} are used to read the files
#'
#' @param years A vector of integer values defining years to be loaded
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @import magrittr
#'
#' @return Returning lists of dataframes - one per defined year -
#' with the columns MONTH and year. If the year is invalid an error returns.
#'
#' @examples
#' fars_read_years(c(2013, 2015))
#' fars_read_years(2013:2015)
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Read and summarize all fatal injuries by month and year
#'
#' Summarizing the fatal injuries per month and year by given years.
#' Therefore \code{\link{fars_read_years}} is used.
#' To exceute the functions the following functions need to be imported:
#'
#' @param years A vector of integer values defining years to be loaded
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @import magrittr
#'
#' @return Returning a dataframe with the summarized injuries.
#' Month will be shown as rows and years as columns
#'
#' @examples
#' fars_summarize_years(c(2013, 2015))
#' fars_summarize_years(c(2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' Plotting fatal injuries
#'
#' Plotting fatal injuries by defing a state number and a year.
#' \code{\link{make_filename}} and \code{\link{fars_read}} are used to read the files.
#'
#' @param state.num A numeric value defining the number wanted to be analysed.
#' Possible entries are 1 to 56
#'
#' @param year A integer value defining the corresponding year
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#'
#' @return Returning a plot which shows the accidents as points in a state map.
#' If the '\code{state.num}' is invalid an error returns.
#' If there a no accidents there will be no plot but a message.
#'
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(30, 2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

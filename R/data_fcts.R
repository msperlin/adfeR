#' List available datasets with description
#'
#' This function will list all available datasets from the book. It also includes a description and origin, if applicable.
#'
#' @param be_silent Be silent?
#'
#' @return A dataframe with the data information
#' @export
#'
#' @examples
#' df_data <- afedR_list_available_data()
afedR_list_available_data <- function(be_silent = FALSE) {

  path_data <- system.file('extdata/data', package = 'afedR')

  my_files <- list.files(path_data)

  if (!be_silent) print(my_files)

  return(invisible(my_files))
}

#' Get path to data file
#'
#' This is a helper function of book "Analyzing Financial and Economic Data with R" by Marcelo S. Perlin.
#' With this function you'll be able to read the tables used in the book using only the filenames.
#'
#' @param name_dataset Name of the dataset filename (see \link{afedR_list_available_data} for more details)
#'
#' @return A path to the data file
#' @export
#'
#' @examples
#' path_to_file <- afedR_get_data_file('grunfeld.csv')
afedR_get_data_file <- function(name_dataset) {

  #if (!(name_dataset %in% df_available$file_name)) {
    #stop('Cant find name ', name_dataset, ' in list of available tables.')
  #}

  path_out <- system.file(paste0('extdata/data/', name_dataset),
                          package = 'afedR')

  if (path_out == '') {
    stop('Cant find name ', name_dataset, ' in list of available tables.')
  }

  return(path_out)
}


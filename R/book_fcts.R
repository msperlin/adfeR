#' Gets official links from book
#'
#' Use this function in the book so that all links are updated in a single location.
#'
#' @return A list with links
#' @export
#'
#' @examples
#'
#' print(afedR_get_links_book())
get_links_book <- function() {

  my_l <- list(book_site = 'https://www.msperlin.com/blog/publication/2021_book-adfer-pt/',
               book_site_zip = 'https://www.msperlin.com/blog/files/adfer-files/adfeR-code-and-data.zip',
               blog_site = 'https://www.msperlin.com/blog')

  return(my_l)

}

#' Copy all book files to a local folder
#'
#' This function will grab files from the afedR package and copy all of it to a local folder,
#' separated by directories including R code, data, slides and end-chapter exercises.
#'
#' @param path_to_copy Path to copy all the book files
#'
#' @return TRUE, if sucessful
#' @export
#'
#' @examples
#' \dontrun{
#' flag <- afedR_get_book_files()
#' }
copy_book_files <- function(path_to_copy = '~') {

  if (!dir.exists(path_to_copy)) {
    stop(paste0('Path ', path_to_copy, ' does not exists. Perhaps create it ?'))
  }

  # data files
  data_path_files <- system.file('extdata/data', package = 'adfeR')
  data_path_to_copy <- file.path(path_to_copy, 'adfeR-files/data')

  message('Copying data files files to ', data_path_to_copy)

  if (!dir.exists(data_path_to_copy)) dir.create(data_path_to_copy,
                                                 recursive = TRUE)

  files_to_copy <- list.files(data_path_files, full.names = TRUE)

  flag <- file.copy(from = files_to_copy, to = data_path_to_copy,
                    overwrite = TRUE)

  if (all(flag)) message(paste0('\t', length(flag), ' files copied'))

  # Slides
  slides_path_files <- system.file('extdata/slides', package = 'adfeR')
  slides_path_to_copy <- file.path(path_to_copy, 'adfeR-files/slides')

  message('Copying slides files files to ', slides_path_to_copy)
  if (!dir.exists(slides_path_to_copy)) dir.create(slides_path_to_copy,
                                                   recursive = TRUE)

  files_to_copy <- list.files(slides_path_files, full.names = TRUE,
                              include.dirs = TRUE)

  flag <- file.copy(from = files_to_copy, to = slides_path_to_copy,
                    overwrite = TRUE, recursive = TRUE)

  if (all(flag)) message(paste0('\t', length(flag), ' files copied'))

  # EOC exercises
  eoc_exerc_path_files <- system.file('extdata/eoc-exercises', package = 'adfeR')
  eoc_exerc_path_to_copy <- file.path(path_to_copy, 'adfeR-files/eoc-exercises')

  message('Copying end-of-chapter (eoc) exercises with solutions to ',
          eoc_exerc_path_to_copy)
  if (!dir.exists(eoc_exerc_path_to_copy)) dir.create(eoc_exerc_path_to_copy,
                                                      recursive = TRUE)

  files_to_copy <- list.files(eoc_exerc_path_files, full.names = TRUE)

  flag <- file.copy(from = files_to_copy,
                    to = eoc_exerc_path_to_copy,
                    overwrite = TRUE)

  if (all(flag)) message(paste0('\t', length(flag), ' files copied'))

  # R code
  r_code_path_files <- system.file('extdata/R-code', package = 'adfeR')
  r_code_exerc_path_to_copy <- file.path(path_to_copy, 'adfeR-files/R-code')

  message('Copying end-of-chapter (eoc) exercises with solutions to ',
          r_code_exerc_path_to_copy)
  if (!dir.exists(r_code_exerc_path_to_copy)) dir.create(r_code_exerc_path_to_copy,
                                                         recursive = TRUE)

  files_to_copy <- list.files(r_code_path_files, full.names = TRUE)

  flag <- file.copy(from = files_to_copy,
                    to = r_code_exerc_path_to_copy,
                    overwrite = TRUE)

  if (all(flag)) message(paste0('\t', length(flag), ' files copied'))

  return(invisible(TRUE))
}

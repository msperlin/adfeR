#' Builds pdf exercices
#'
#' @inheritParams build_exercises
#'
#' @return a list
#'
#' @examples
build_pdf_exercises <- function(students_names,
                                students_ids,
                                class_name,
                                exercise_name,
                                exercises_to_compile,
                                chapters_to_include,
                                dir_out = 'adfeR-pdf-exercises',
                                solution = FALSE) {

  # set template
  template_tex_file <- system.file('extdata/exams_files/templates/template_pdf.tex',
                                   package = 'adfeR')

  # loop all names
  df_files <- dplyr::tibble()
  l_exams <- list()
  for (i_std in seq_along(students_names) ) {

    i_name <- students_names[i_std]
    i_id <- students_ids[i_std]
    i_ver <- i_std

    message('Building exercise for ', i_name)
    my_temp_dir <- file.path(tempdir(), paste0('exams-pdf-',i_name))

    f_out <- tempfile(fileext = '.tex')

    # make sure duplicate labels are possible
    options(knitr.duplicate.label = "allow")

    # replacing latex template
    add_tex_content(f_in = template_tex_file,
                    f_out = f_out,
                    student_name = i_name,
                    student_version = i_std,
                    student_id = i_id,
                    n_q = length(exercises_to_compile),
                    class_name = class_name,
                    exam_links = rep(list(links_in_html), n_ver),
                    exercise_name = exercise_name)

    message('\tBuilding pdf')
    suppressWarnings({
      my_exam <- exams::exams2pdf(file = exercises_to_compile,
                                  n = 1,
                                  name = paste0(class_name, ' - ',
                                                exercise_name,
                                                ' - Version '),
                                  encoding = 'UTF-8',
                                  dir = my_temp_dir,
                                  mathjax = TRUE,
                                  template = f_out,
                                  question = '',
                                  verbose = FALSE,
                                  solution = solution)
    })

    # make sure duplicate labels are no longer possible
    options(knitr.duplicate.label = "deny")


    pdf_file <- list.files(my_temp_dir, full.names = TRUE)[1]
    tib_out <- dplyr::tibble(std_name = i_name,
                             std_id = i_id,
                             filename = pdf_file)
    df_files <- dplyr::bind_rows(df_files,
                                 tib_out )

    out_file <- file.path(dir_out,
                          paste0(exercise_name, '_',
                                 stringr::str_c('Ver ', sprintf("%02d",i_ver)), '_',
                                 i_name, '.pdf'))

    message('\tCopying final pdf')
    file.copy(from = pdf_file, to = out_file,
              overwrite = TRUE)

    l_exams <- c(l_exams, my_exam)

  }

  return(l_exams)
}



#' Replaces content in tex template file
#'
#' Used for replacing names, id, version and so on on tex exercise files.
#'
#' @param f_in File with tex code
#' @param dir_out Folder out
#' @param student_name Name of student
#' @param student_version Version of student
#' @param n_q Number of questions in exercise
#' @param class_name Name of class
#' @param exercise_name Name of exercise
#' @param exam_links Links to add
#'
#' @return TRUE, if sucessfull
#' @export
#'
#' @examples
#' \dontrun{
#' afedR_add_html_content(f_in = 'example.html', dir_out = tempdir(),
#'                        student_name = 'George', student_version = 1,
#'                        n_q = 10, class_name = 'example class', exercise_name = 'sample',
#'                        exam_links = NA)
#' }
add_tex_content <- function(f_in,
                            f_out,
                            student_name,
                            student_version,
                            student_id,
                            n_q,
                            class_name,
                            exercise_name,
                            exam_links) {


  if (is.na(student_name)) {
    std.name <- 'ZZ-NO NAME'
  }

  message('\tAdding content to tex')

  tex_content <- paste0(readr::read_lines(f_in), collapse = '\n')

  # replace links for html
  #base_str <- '<p><a href="%s"> %s </a>.</p>'
  #text_itself <- sapply(exam_links, names)

  #html_links <- paste0(sprintf(base_str, exam_links$url, exam_links$text),
  #                    collapse = '\n')

  # make replacements
  replace_vec <- list(#'EXAM_LINKS' = html_links,
    'EXAM_NAME' = exercise_name,
    'TAB_TITLE' = paste0(exercise_name, '-', student_name),
    'STD_NAME' = student_name,
    'STD_ID' = student_id,
    'STD_VERSION' = student_version,
    'N_QUESTIONS' = n_q,
    'CLASS_NAME' = class_name,
    'DATE_COMPILE' = paste0(Sys.time(), ' at ', Sys.info()['nodename']))

  for (i_vec in seq(length(replace_vec))) {
    tex_content <- stringr::str_replace_all(tex_content,
                                            pattern = stringr::fixed(names(replace_vec[i_vec])),
                                            replacement = replace_vec[[i_vec]])
  }

  cat(tex_content, file = f_out, append = FALSE)

  #browser()
  return(invisible(TRUE))
}

#' Creates exams in using exercises from the book
#'
#' This function uses the \link{exam} package to create exercises in the html format with
#' random selections. This means that each student will receive a different version of the same
#' exercise. All exercise files are taken from book "Analise de dados financeiros e economicos com o R".
#'
#' @param students_names Names of students (a vector)
#' @param students_ids Ids of students (a vector)
#' @param class_name The name of the class
#' @param exercise_name The name of the exercises
#' @param links_in_html A dataframe with links to be added in the html page. This can
#'     be anything that helps the students. The dataframe must have two columns: "text" with the text to
#'     appear in the html and "url" with the actual link (see default options for details).
#' @param chapters_to_include Chapter to include in exercise (1-13)
#' @param dir_out Folder to copy exercise html files
#' @param language Selection of language ("en" only so far)
#'
#' @return TRUE, if sucessfull
#' @export
#'
#' @examples
#' \dontrun{
#' afedR_build_exam(students_names = 'George', chapters_to_include = 2,
#'                  dir_out = tempdir())
#'  }
build_exercises <- function(students_names,
                            students_ids = paste0('Exam ', 1:length(students_names)),
                            dir_exercises = get_EOC_dir(),
                            format_out = 'pdf',
                            class_name = 'Nome da Disciplina',
                            exercise_name = paste0('Exercicios Teste'),
                            chapters_to_include = 1:13,
                            dir_out = 'adfeR-pdf-exercises',
                            language = 'en',
                            solution = FALSE,
                            log_answer_key = FALSE) {

  # check args
  available_formats <- c('pdf')
  if (!(format_out %in% available_formats)){
    stop('Only pdf for available format (check input format_out)')
  }


  if (length(students_names) != length(students_ids)) {
    stop('Length of students_names does no match the length of studends_ids. Check your inputs..')
  }

  if (!is.numeric(chapters_to_include)) {
    stop('Arg chapters_to_include should be of numeric type.')
  }

  if ( any((chapters_to_include < 0))|any((chapters_to_include > 13 )) ) {
    stop('Arg chapters_to_include should be between 1 and 13.')
  }

  available_exercises <- list.files(dir_exercises,
                                    full.names = TRUE,
                                    recursive = TRUE, pattern = '.Rmd|.Rnw')

  if (!dir.exists(dir_out)) dir.create(dir_out)

  chapter_names <- paste0('Cap-', sprintf('%02d', chapters_to_include))
  idx <- stringr::str_sub(basename(available_exercises), 7, 12) %in%
    chapter_names

  exercises_to_compile <- available_exercises[idx]

  if (length(exercises_to_compile) == 0) {
    stop('Cant find any exercises')
  }

  n_ver <- length(students_names)

  if (format_out == 'html') {

    # not implemented... maybe in the future?

  } else if (format_out == 'pdf') {

    l_out <- build_pdf_exercises(students_names = students_names,
                                 students_ids = students_ids,
                                 exercises_to_compile = exercises_to_compile,
                                 class_name = class_name,
                                 exercise_name = exercise_name,
                                 dir_out = dir_out)

  }

  df_answer_key <- build_answer_key(l_out, students_names)

  df_answer_key_wide <- tidyr::spread(df_answer_key,
                                      key = i_q,
                                      value = solution)

  info_activity <- list()
  info_activity$answer_key <- df_answer_key_wide
  info_activity$answer_key_log <- gsub(Sys.time(),
                                       pattern = ':', replacement = '', fixed=TRUE)

  if (log_answer_key) {
    # save answer key
    message('')
    message('Writing answer key to csv file')
    f_out <- file.path(dir_out,
                       paste0('Answerkey-', class_name, '-',
                              exercise_name, '-',
                              gsub(Sys.time(),
                                   pattern = ':', replacement = '', fixed=TRUE), '.csv'))
    readr::write_csv(x = df_answer_key_wide, path = f_out)
    message('Done. File available at ', f_out)
    message('')
  }
  message('All exam files are available at folder "', dir_out, '".')

  return(info_activity)

}

#' Generate random vectors for answers
#'
#' This function generates random vectors with first element equal to 1 and rest
#' equal to proportions. It is mostly used for building alternatives in numerical questions:
#' solution*afedR_gen_rnd_vec().
#'
#' @return A vector
#' @export
#' @import stats
#'
#' @examples
#' print(afedR_gen_rnd_vec())
gen_rnd_vec <- function(){
  rnd.vec.1 <- c(1, seq(runif(1,0.1,0.2), runif(1,0.7,0.8), length.out = 4))
  rnd.vec.2 <- c(1, seq(runif(1,1.1,1.2), runif(1,1.7, 1.8), length.out = 4))
  rnd.vec.3 <- c(1, seq(runif(1,0.25,0.5),runif(1,0.6,0.8), length.out = 2),
                 seq(runif(1,1.2,2), length.out = 2))

  rnd.l <- list(rnd.vec.1, rnd.vec.2, rnd.vec.3)
  rnd.vec <- sample(rnd.l,1)[[1]]
  return(rnd.vec)
}

#' Title
#'
#' @param solution
#' @param candidates
#' @param is_cash
#'
#' @return
#' @export
#'
#' @examples
make_random_answers <- function(solution,
                                candidates = NA,
                                is_cash = FALSE) {
  if (!any(is.na(candidates))) {
    candidates <- unique(candidates)
    candidates <- candidates[candidates != solution]

    if (length(candidates) < 4) {
      stop('Candidate vector is lower than 4!')
    }

    my_answers <- c(solution,
                    sample(candidates, 4))
  } else {
    # check if is numeric
    if (class(solution) %in% c('numeric', 'integer')) {
      # find number of decimais
      n_decimals <- decimal_places(solution)

      if (n_decimals ==0) {
        my_answers <- floor(solution*gen_rnd_vec())
      } else {
        if (n_decimals > 4) n_decimals <- 4
        my_answers <- format(solution*gen_rnd_vec(),
                             digits = n_decimals)
      }

      if (is_cash) {
        my_answers <- format_cash(
          as.numeric(my_answers),
          type_cash = 'BRL')
      }

    }

  }

  return(my_answers)
}

#' Builds answer key
#'
#' @param my_exam A single exam list
#' @param students_names Names of students
#'
#' @return A dataframe
#' @export
#'
#' @examples
build_answer_key <- function(my_exam, students_names) {

  n_ver <- length(my_exam)
  df_answer_key <- dplyr::tibble()
  for (i_ver in seq(n_ver)){

    exam_now <- my_exam[[i_ver]]

    n_q <- length(exam_now)

    for (i_q in seq(n_q)){

      type_question <- exam_now[[i_q]]$metainfo$type

      if (type_question == 'schoice') {
        sol_now <- letters[which(exam_now[[i_q]]$metainfo$solution)]
      } else if (type_question == 'string'){
        sol_now <- NA
      } else if (type_question == 'num') {
        sol_now <- as.character(exam_now[[i_q]]$metainfo$solution)
      }

      temp <- dplyr::tibble(i_name = students_names[i_ver],
                            i_ver = i_ver,
                            i_q = i_q,
                            solution = sol_now)

      df_answer_key <- dplyr::bind_rows(df_answer_key, temp)
    }
  }

  return(df_answer_key)
}


#' Returns EOC folder
#'
#' @return A string
#' @export
#'
#' @examples
get_EOC_dir <- function() {
  eoc_dir <- system.file('extdata/exams_files/EOC-Exercises/',
                         package = 'adfeR')

  return(eoc_dir)
}

#' Creates text for random questions
#'
#' @param text1 string 1
#' @param text2 string 2
#' @param text3 string 3
#'
#' @return A list
#' @export
#'
#' @examples
build_answers_text <- function(text1,
                               text2,
                               text3) {

  require(glue)

  text1_chosen <- text1[sample(1:nrow(text1), 1), ]
  text2_chosen <- text2[sample(1:nrow(text2), 1), ]
  text3_chosen <- text3[sample(1:nrow(text3), 1), ]

  right_answer <- paste0(c(text1_chosen$sol,
                           text2_chosen$sol,
                           text3_chosen$sol), collapse = ', ')

  other_answers <- tidyr::expand_grid(col1 = c('TRUE', 'FALSE'),
                               col2 = c('TRUE', 'FALSE'),
                               col3 = c('TRUE', 'FALSE')) %>%
    dplyr::mutate(answer = glue::glue('{col1}, {col2}, {col3}') ) %>%
    dplyr::filter(answer != right_answer)

  my_answers <- c(right_answer,
                  sample(other_answers$answer, 4))

  return(list(my_answers = my_answers,
              texts = c(text1_chosen$text,
                        text2_chosen$text,
                        text3_chosen$text)))

}

# Repository for R package adfeR

Includes functions and files for book "Análise de Dados Financeiros e Econômicos com o R", available in [Amazon](https://www.amazon.com.br/dp/B08WNC27ZY) and [Online](https://www.msperlin.com/adfeR/).

## Instalação

```
# only in github (will not pass cran checks due suplementary files)
devtools::install_github('msperlin/adfeR')
```

## Example of usage

### Listing available datasets

```
adfeR::list_available_data()
```

### Fetching data from book repository

```
file_name <- 'SP500.csv'
path_to_file <- adfeR::get_data_file(file_name)

df <- readr::read_csv(path_to_file)
```

### Copying all book files to local directory

```
flag <- adfeR::copy_book_files(path_to_copy = '~')
```

### Compiling pdf exercises 

See this [blog post](https://www.msperlin.com/blog/post/2021-02-18-dynamic-exercises-adfer/) for details.

```
library(adfeR)

names_students <- c('Marcelo', 'Ricardo', 'Tarcizio')
ids_students <- 1:length(names_students) # probably id card?
chapters <- 1:3 # chapters from 1 to 13
dir_output <- file.path(tempdir(), 'ExamsFiles')



l_exams <- build_exercises(students_names = names_students, 
                           students_ids = ids_students, 
                           chapters_to_include = chapters,
                           dir_out = dir_output)
```

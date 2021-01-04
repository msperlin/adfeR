# Repository for R package adfeR.ed3

Inclui funções e arquivos relacionados ao livro "Análise de Dados Financeiros e Econômicos com o R", logo disponível na [Amazon](PLACEHOLDER).

## Instalação

```
# only in github (will not pass cran checks)
devtools::install_github('msperlin/afedR-pt-ed3')
```

## Example of usage

### Listing available datasets

```
adfeR.ed3::afedR_list_available_data()
```

### Fetching data from book repository

```
file_name <- 'SP500.csv'
path_to_file <- adfeR.ed3::afedR_get_data_file(file_name)

df <- readr::read_csv(path_to_file)
```

### Copying all book files to local directory

```
flag <- adfeR.ed3::afedR_get_book_files(path_to_copy = '~')
```

### Building html exams (testing)

```
adfeR.ed3::afedR_build_exam(students_names = c('George', 'Michael'), 
                        chapters_to_include = 2,
                        dir_out = tempdir())
```

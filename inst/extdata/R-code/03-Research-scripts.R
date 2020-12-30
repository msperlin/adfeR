#' # Writing Research Scripts {#research-scripts}
#' 
#' So far we learned how to use R for basic tasks 
#' 
#' An organized code facilitates sharing and futur
#' 
#' 
#' ## Stages of Research
#' 
#' Unlike other software designs, every research s
#' 
#' 1. **Importation of data**: Raw (original) data
#' 
#' 2. **Cleaning and structuring the data**: The r
#' 
#' 3. **Visual analysis and hypothesis testing**: 
#' 
#' 4. **Reporting the results**: The final stage o
#' 
#' Each of the mentioned steps can be structured i
#' 
#' A practical example would be the analysis of a 
#' 
#' If you are working with multiple files, one sug
#' 
#' 
#' ## Folder Structure {#directories}
#' 
#' A proper folder structure also benefits the rep
#' 
#' A suggestion for an effective folder structure 
#' 
#' 	/Capital Markets and Inflation/
#' 		/data/
#' 			stock_indices.csv
#' 			inflation_data.csv
#' 		/figs/
#' 			SP500_and_inflation.png
#' 		/tables/
#' 			Table1_descriptive_table.tex
#' 			Table2_model_results.tex
#' 		/R-Fcts/
#' 			fct_models.R
#' 			fct_clean_data.R
#' 		0-run-it-all.R
#' 		1-import-and-clean-data.R
#' 		2-run-research.R
#' 
#' The research code should also be self-contained
#' 
#' The benefits of this directory format are as fo
#' 
#' An example for the content of file `0-run-it-al
#' 
## ----eval=FALSE, tidy=FALSE----------------------------------------------------------------------------------
## # clean up workspace
## rm(list=ls())
## 
## # close all figure windows created with x11()
## graphics.off()
## 
## # load packages
## library(pkg1)
## library(pkg2)
## library(pkg3)
## 
## # change directory
## my_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
## setwd(my_dir)
## 
## # list  functions in 'R-Fcts'
## my_R_files <- list.files(path='R-Fcts',
##                          pattern = '*.R',
##                          full.names=TRUE)
## 
## # Load all functions in R
## sapply(my_R_files, source)
## 
## # Import data script
## source('01-import-and-clean-data.R')
## 
## # run models and report results
## source('02-run-research.R')

#' 
#' This is the first time we use functions `graphi
#' 
#' Notice that, assuming all packages are installe
#' 
#' Another way of setting up directories in a rese
#' 
#' The benefit of this approach is that it is unne
#' 
#' 
#' ## Important Aspects of a Research Script
#' 
#' In this section I'll be making some suggestions
#' 
#' Firstly, **know your data!**. I can't stress en
#' 
#' - How was the data collected? To what purpose?
#' - How do the available data compare with data u
#' - Is there any possibility of bias within the d
#' 
#' Furthermore, you need to remember that the ulti
#' 
#' As an example, consider the case of analyzing t
#' 
#' The message is clear. **Be very cautious about 
#' 
#' The second point here is the code. After you fi
#' 
#' Remember that analyzing data is your profession
#' 
#' - Do the descriptive statistics of the variable
#' - Is there any relationship between the variabl
#' - Do the main findings of the research make sen
#' - Is it possible that a _bug_ in the code has p
#' 
#' I'm constantly surprised by how many studies su
#' 
#' All of the research work is, to some extent, ba
#' 
#' I clarify that it is possible that the results 
#' 
#' 
#' ## Exercises
#' 
#' 01. Imagine a survey regarding your household b
#' 
#' 02. Based on the previous exercise, create a fo
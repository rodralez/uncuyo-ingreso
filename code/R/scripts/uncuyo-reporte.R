rm(list = ls()) # clean global environment
cat("\014")  # clean the console
setwd("/home/rodralez/hostdir/uncuyo-ingreso/")
rmarkdown::render("code/R/notebooks/uncuyo_ingresantes_2023.rmd",
                  "html_document",
                  output_file = "/home/rodralez/hostdir/uncuyo-ingreso/code/R/notebooks/uncuyo_ingresantes_2023.html"
                  )

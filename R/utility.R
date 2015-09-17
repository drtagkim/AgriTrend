## =============================================== ##
##              CODED BY TAEKYUNG KIM              ##
##               ASSISTANT PROFESSOR               ##
##             THE UNIVERSITY OF SUWON             ##
##               (KIMTK@SUWON.AC.KR)               ##
##                   2015.09.16                    ##
##            RDA PANEL DATA ANALYSIS              ##
##  GYUNG-GI AGRICULTURAL RESEARCH AND EXTENSION   ##
##        CORRESPONDING TO DR. JUNG, G.            ##
## =============================================== ##

#' Exporting Data to Excel Worksheet
#'
#' Provided by Rob Kabacoff
#'
#' @param xlsx Excel file name
#' @examples
#' save.xlsx("test.xlsx",test_object)
save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

run_analysis_normal <- function(chr_data,folder_name="") {
  fname_01 <- paste(folder_name,chr_data,"_(fresh_year).xlsx",sep="")
  fname_02 <- paste(folder_name,chr_data,"_(fresh_month).xlsx",sep="")
  fname_03 <- paste(folder_name,chr_data,"_(store_year).xlsx",sep="")
  fname_04 <- paste(folder_name,chr_data,"_(store_year_processed).xlsx",sep="")
  fname_05 <- paste(folder_name,chr_data,"_(income_year).xlsx",sep="")
  fname_06 <- paste(folder_name,chr_data,"_(income_year_processed).xlsx",sep="")
  fname_07 <- paste(folder_name,chr_data,"_(age_year).xlsx",sep="")
  fname_08 <- paste(folder_name,chr_data,"_(age_year_processed).xlsx",sep="")
  fname_09 <- paste(folder_name,chr_data,"_(job_year).xlsx",sep="")
  fname_10 <- paste(folder_name,chr_data,"_(job_year_processed).xlsx",sep="")
  fname_11 <- paste(folder_name,chr_data,"_(natural_year).xlsx",sep="")
  ddset <- eval(parse(text=chr_data))
  result01 <- fresh_year(ddset)
  result02 <- fresh_month(ddset)
  result03 <- store_year(ddset)
  result04 <- store_year_processed(ddset)
  result05 <- income_year(ddset)
  result06 <- income_year_processed(ddset)
  result07 <- age_year(ddset)
  result08 <- age_year_processed(ddset)
  result09 <- job_year(ddset)
  result10 <- job_year_processed(ddset)
  result11 <- natural_year(ddset)
  save.xlsx(fname_01,result01)
  save.xlsx(fname_02,result02)
  save.xlsx(fname_03,result03)
  save.xlsx(fname_04,result04)
  save.xlsx(fname_05,result05)
  save.xlsx(fname_06,result06)
  save.xlsx(fname_07,result07)
  save.xlsx(fname_08,result08)
  save.xlsx(fname_09,result09)
  save.xlsx(fname_10,result10)
  save.xlsx(fname_11,result11)
}

test_f <- function() {
  dim(demographic)
}

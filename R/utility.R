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
  fname_01 <- paste(folder_name,chr_data,"_(item).xlsx",sep="")
  fname_02 <- paste(folder_name,chr_data,"_(store).xlsx",sep="")
  fname_03 <- paste(folder_name,chr_data,"_(income).xlsx",sep="")
  fname_04 <- paste(folder_name,chr_data,"_(age).xlsx",sep="")
  fname_05 <- paste(folder_name,chr_data,"_(job).xlsx",sep="")
  fname_06 <- paste(folder_name,chr_data,"_(natural).xlsx",sep="")
  fname_07 <- paste(folder_name,chr_data,"_(family).xlsx",sep="")
  fname_08 <- paste(folder_name,chr_data,"_(education).xlsx",sep="")
  ddset <- eval(parse(text=chr_data))
  result01 <- item_month(ddset)
  result02 <- store_month(ddset)
  result03 <- income_month(ddset)
  result04 <- age_month(ddset)
  result05 <- job_month(ddset)
  result06 <- natural_month(ddset)
  result07 <- family_month(ddset)
  result08 <- education_month(ddset)
  save.xlsx(fname_01,result01)
  save.xlsx(fname_02,result02)
  save.xlsx(fname_03,result03)
  save.xlsx(fname_04,result04)
  save.xlsx(fname_05,result05)
  save.xlsx(fname_06,result06)
  save.xlsx(fname_07,result07)
  save.xlsx(fname_08,result08)
}

run_analysis_subgroup <- function(chr_data,folder_name="") {
    #data
    ddset <- eval(parse(text=chr_data))
    subgroups <- unique(ddset$detail3)
    subgroups[subgroups == "null"] <- NULL #remove null
    rv <- foreach(sg = subgroups) %do% {
        fname_01 <- paste(folder_name,"[",sg,"]",chr_data,"_(item).xlsx",sep="")
        fname_02 <- paste(folder_name,"[",sg,"]",chr_data,"_(store).xlsx",sep="")
        fname_03 <- paste(folder_name,"[",sg,"]",chr_data,"_(income).xlsx",sep="")
        fname_04 <- paste(folder_name,"[",sg,"]",chr_data,"_(age).xlsx",sep="")
        fname_05 <- paste(folder_name,"[",sg,"]",chr_data,"_(job).xlsx",sep="")
        fname_06 <- paste(folder_name,"[",sg,"]",chr_data,"_(natural).xlsx",sep="")
        fname_07 <- paste(folder_name,"[",sg,"]",chr_data,"_(family).xlsx",sep="")
        fname_08 <- paste(folder_name,"[",sg,"]",chr_data,"_(education).xlsx",sep="")
        result01 <- item_month(ddset,sg)
        result02 <- store_month(ddset,sg)
        result03 <- income_month(ddset,sg)
        result04 <- age_month(ddset,sg)
        result05 <- job_month(ddset,sg)
        result06 <- natural_month(ddset,sg)
        result07 <- family_month(ddset,sg)
        result08 <- education_month(ddset,sg)
        cat("Exporting ",sg,"\n")
        cat("=============================\n")
        save.xlsx(fname_01,result01)
        save.xlsx(fname_02,result02)
        save.xlsx(fname_03,result03)
        save.xlsx(fname_04,result04)
        save.xlsx(fname_05,result05)
        save.xlsx(fname_06,result06)
        save.xlsx(fname_07,result07)
        save.xlsx(fname_08,result08)
        cat("-----------------------------\n")
    }
}

test_f <- function() {
  dim(demographic)
}

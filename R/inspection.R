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

#' @title Inspect Balacning
#' @param data Dataset
#'
inspect_balance <- function(data) {
  inspection_balanced_data <- data %>% group_by(year,month) %>% summarise(n_distinct(panel_c2))
  View(inspection_balanced_data)
}
#' Break up panel_data for later use
#'
#' @param detail2_chr Character representation of detail3
#' @return data.frame (dplyr tbr)
break_data <- function(detail2_chr) {
  rv <- panel_data %>% filter(detail2==detail2_chr)
  rv
}

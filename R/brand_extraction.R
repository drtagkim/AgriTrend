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

extract_brand <- function(dset,dic_filter) {
  named <- as.character(dset$grocery)
  dset$normal_brand <- ifelse(str_replace_all(named,dic_filter,"") == "","Normal","Brand")
  dset
}

#' Pork
#'
extract_brand_pork <- function(dset,dic_filter=NULL) {
  if(is.null(dic_filter)) {
    dic_filter <- "(돼지)|(뒷다리)|(앞다리)|(돈육)|(순대.{0,1})|
    (뒷다리)|(전지)|(찹쌀)|(갈비)|(찌개용)|(다짐육)|(족발.{0,1})|
    (통뼈없는)|(.{0,2}삼겹살)|(삼겹살)|(국내산_)|(등_)|(_특정)|
    (양념돈구이)|(_삽겹로스)|(편육)|
    (오겹삽)|(구이용)|(소세지)|(모듬)|(찌게용)|(안심)|(목심)|
    (모듬)|(소시지)|(소세지)"
  }
  named <- as.character(dset$grocery)
  dset$normal_brand <- ifelse(str_replace_all(named,dic_filter,"") == "","Normal","Brand")
  dset
}

extract_brand_rice <- function(dset,dic_filter=NULL) {
  if(is.null(dic_filter)) {
    dic_filter <- "(.{0,1}쌀)|(.?미)|(유기농)|(우렁)|(_)|(국내산)|
  (발아)|(다시마)|(무농약)|
  (일반)|(GAP)"
  }
  named <- as.character(dset$grocery)
  dset$normal_brand <- ifelse(str_replace_all(named,dic_filter,"") == "","Normal","Brand")
  dset
}

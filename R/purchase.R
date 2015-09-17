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

#' Purchase by Year
#'
#' @param dset Data set
#' @examples
#' data(data_pork)
#' test <- purchase_year(data_pork)
#' save.xlsx("Pork_by_year.xlsx",test)
#'
store_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset <- ddset %>% filter(quantity > 0)
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,retail) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,retail) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,retail=c("대형마트", "재래시장", "대형슈퍼마켓","소형슈퍼마켓","직거래"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","retail")))
  ddddset[is.na(ddddset)] <- 0
  ddddset <- ddddset %>% inner_join(demo_dist_year,by="year")
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Store Year Processed
store_year_processed <- function(dset) {
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset <- ddset %>% filter(quantity > 0)
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  ddset$processed <- ifelse(str_detect(ddset$detail3,"가공"),"가공","신선")
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,retail,processed) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,retail,processed) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,retail=c("대형마트", "재래시장", "대형슈퍼마켓","소형슈퍼마켓","직거래"),
                        processed=c("가공","신선"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  ddddset <- ddddset %>% inner_join(demo_dist_year,by="year")
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Income Year
income_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(income_new %in% c("A고소득","B중소득","C저소득"))
  ddset <- ddset %>% filter(quantity > 0 & purchase > 1)
  ddset$income_new <- as.character(ddset$income_new)
  ddset$income_new[ddset$income_new=="A고소득"] = "고소득"
  ddset$income_new[ddset$income_new=="B중소득"] = "중소득"
  ddset$income_new[ddset$income_new=="C저소득"] = "저소득"
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,income_new) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,income_new) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,income_new=c("고소득", "중소득", "저소득"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","income_new")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_income,by=c("year","income_new")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Income Year Processed
income_year_processed <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(income_new %in% c("A고소득","B중소득","C저소득"))
  ddset <- ddset %>% filter(quantity > 0)
  ddset$income_new <- as.character(ddset$income_new)
  ddset$income_new[ddset$income_new=="A고소득"] = "고소득"
  ddset$income_new[ddset$income_new=="B중소득"] = "중소득"
  ddset$income_new[ddset$income_new=="C저소득"] = "저소득"
  ddset$processed <- ifelse(str_detect(ddset$detail3,"가공"),"가공","신선")
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,income_new,processed) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,income_new,processed) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,income_new=c("고소득", "중소득", "저소득"),
                        processed=c("가공","신선"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","income_new","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_income,by=c("year","income_new")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Age Year
age_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,age_new) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,age_new) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,age_new=c("30대이하", "40대연령", "50대연령", "60대이상"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","age_new")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_age,by=c("year","age_new")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Age Year Processed
age_year_processed <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  ddset$processed <- ifelse(str_detect(ddset$detail3,"가공"),"가공","신선")
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,age_new,processed) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,age_new,processed) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,age_new=c("30대이하", "40대연령", "50대연령", "60대이상"),
                        processed=c("가공","신선"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","age_new","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_age,by=c("year","age_new")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Job Year
job_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  ddset$full_housewife <- ifelse(ddset$panel_job == "전업주부","전업주부","취업주부")
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,full_housewife) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,full_housewife) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,full_housewife=c("전업주부","취업주부"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","full_housewife")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_job,by=c("year","full_housewife")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Job Year Processed
job_year_processed <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  ddset$full_housewife <- ifelse(ddset$panel_job == "전업주부","전업주부","취업주부")
  ddset$processed <- ifelse(str_detect(ddset$detail3,"가공"),"가공","신선")
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,full_housewife,processed) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,full_housewife,processed) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,full_housewife=c("전업주부","취업주부"),
                        processed = c("가공","신선"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","full_housewife","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_job,by=c("year","full_housewife")))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Brand Year
brand_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,normal_brand) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,normal_brand) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,normal_brand=c("Normal","Brand"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","normal_brand")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Brand Year Store
brand_year_store <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset <- ddset %>% filter(quantity > 0)
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,normal_brand,retail) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,normal_brand,retail) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,normal_brand=c("Normal","Brand"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","normal_brand","retail")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Natural Year
natural_year <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0)
  ddset$natural <- "관행농업"
  ddset$natural[ddset$cultivation %in% c("유기농","유기농,친환경")] <- "친환경_유기농"
  ddset$natural[ddset$cultivation %in% c("무농약","무농약_친환경","친환경_무농약")] <- "친환경_무농약"
  ddset$natural[ddset$cultivation %in% c("저농약")] <- "친환경_저농약"
  ddset$natural[ddset$cultivation %in% c("친환경일반","친환경","직접재배")] <- "친환경_일반"
  # weighted average
  dddset0 <- ddset %>% group_by(year,panel_c2,natural) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,natural) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,natural=c("친환경_유기농","친환경_무농약","친환경_저농약","친환경_일반"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","natural")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Gram
pack_month_g <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset <- ddset %>% filter(unit_u %in% c("g","kg","Kg","KG","근"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("kg","Kg","KG")] <- ddset$quantity_u[ddset$unit_u %in% c("kg","Kg","KG")] * 1000 * ddset$quantity[ddset$unit_u %in% c("kg","Kg","KG")]
  ddset$pack[ddset$unit_u %in% c("근")] <- ddset$quantity_u[ddset$unit_u %in% c("근")] * 600 * ddset$quantity[ddset$unit_u %in% c("근")]
  ddset$pack_category <- "1kg초과"
  ddset$pack_category[ddset$pack <= 500] <- "500g이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500g초과1kg이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1kg초과","500g이하","500g초과1kg이하"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Gram Store
pack_month_g_store <- function(dset) {
  # select subcateogries
  # filter 1
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  # filter 2
  ddset <- ddset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset$unit_u <- as.character(ddset$unit_u)
  ddset <- ddset %>% filter(unit_u %in% c("g","kg","Kg","KG","근"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("kg","Kg","KG")] <- ddset$quantity_u[ddset$unit_u %in% c("kg","Kg","KG")] * 1000 * ddset$quantity[ddset$unit_u %in% c("kg","Kg","KG")]
  ddset$pack[ddset$unit_u %in% c("근")] <- ddset$quantity_u[ddset$unit_u %in% c("근")] * 600 * ddset$quantity[ddset$unit_u %in% c("근")]
  ddset$pack_category <- "1kg초과"
  ddset$pack_category[ddset$pack <= 500] <- "500g이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500g초과1kg이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category,retail) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1kg초과","500g이하","500g초과1kg이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Litter
pack_month_l <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset <- ddset %>% filter(unit_u %in% c("L","ML","ml","l"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("L","l")] <- ddset$quantity_u[ddset$unit_u %in% c("L","l")] * 1000 * ddset$quantity[ddset$unit_u %in% c("L","l")]
  ddset$pack_category <- "1L초과"
  ddset$pack_category[ddset$pack <= 500] <- "500ML이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500ML초과1L이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1L초과","500ML이하","500ML초과1L이하"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Litter Store
pack_month_l_store <- function(dset) {
  # select subcateogries
  # filter 1
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  # filter 2
  ddset <- ddset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset$unit_u <- as.character(ddset$unit_u)
  ddset <- ddset %>% filter(unit_u %in% c("L","ML","ml","l"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("L","l")] <- ddset$quantity_u[ddset$unit_u %in% c("L","l")] * 1000 * ddset$quantity[ddset$unit_u %in% c("L","l")]
  ddset$pack_category <- "1L초과"
  ddset$pack_category[ddset$pack <= 500] <- "500ML이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500ML초과1L이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category,retail) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1L초과","500ML이하","500ML초과1L이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Counting
pack_month_c <- function(dset) {
  # select subcateogries
  ddset <- dset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset <- ddset %>% filter(unit_u %in% c("개","판","줄","구","입","알"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("판")] <- ddset$quantity_u[ddset$unit_u %in%  c("판")] * 30 * ddset$quantity[ddset$unit_u %in%  c("판")]
  ddset$pack[ddset$unit_u %in% c("줄")] <- ddset$quantity_u[ddset$unit_u %in%  c("줄")] * 10 * ddset$quantity[ddset$unit_u %in%  c("줄")]
  ddset$pack_category <- "30개초과"
  ddset$pack_category[ddset$pack <= 15] <- "15개이하"
  ddset$pack_category[ddset$pack > 15 & ddset$pack <= 30] <- "15개초과30개이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("30개초과","15개이하","15개초과30개이하"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Pack Month Counting Store
pack_month_c_store <- function(dset) {
  # select subcateogries
  # filter 1
  ddset <- dset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  # filter 2
  ddset <- ddset %>% filter(quantity > 0 & !(is.na(quantity_u)))
  ddset <- ddset %>% filter(unit_u %in% c("개","판","줄","구","입","알"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("판")] <- ddset$quantity_u[ddset$unit_u %in%  c("판")] * 30 * ddset$quantity[ddset$unit_u %in%  c("판")]
  ddset$pack[ddset$unit_u %in% c("줄")] <- ddset$quantity_u[ddset$unit_u %in%  c("줄")] * 10 * ddset$quantity[ddset$unit_u %in%  c("줄")]
  ddset$pack_category <- "30개초과"
  ddset$pack_category[ddset$pack <= 15] <- "15개이하"
  ddset$pack_category[ddset$pack > 15 & ddset$pack <= 30] <- "15개초과30개이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,pack_category,retail) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("30개초과","15개이하","15개초과30개이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Fresh Year
fresh_year <- function(dset) {
  ddset <- dset %>% filter(quantity > 0)
  ddset$fresh <- ""
  ddset$fresh <- ifelse(str_detect(ddset$detail3,"일반"),"신선식품","가공식품")
  # weighted average
  dddset0 <- ddset %>% group_by(year,fresh,panel_c2) %>% summarize(amount_mean0=mean(purchase),price_mean0=mean(purchase/quantity),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,fresh) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,fresh=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","fresh")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
#' Fresh Month
fresh_month <- function(dset) {
  ddset <- dset %>% filter(quantity > 0)
  ddset$fresh <- ""
  ddset$fresh <- ifelse(str_detect(ddset$detail3,"일반"),"신선식품","가공식품")
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,fresh) %>% summarize(amount_mean0=mean(purchase,na.rm=T),price_mean0=mean(purchase/quantity,na.rm=T),f=n())
  dddset0 <- ungroup(dddset0)
  dddset <- dddset0 %>% group_by(year,month,fresh) %>% summarize(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  mirror <- expand.grid(year=2010:2014,month=1:12,fresh=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","fresh")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$price_mean <- ddddset$price_mean/ddddset$N
  ddddset
}
# === END OF PROGRAM === #

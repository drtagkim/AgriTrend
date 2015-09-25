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

calculate_ddddset <- function(ddddset) {
  ddddset$amount_mean <- ddddset$amount_mean/ddddset$N
  ddddset$SD_amount <- sqrt(ddddset$SD_amount/ddddset$N)
  ddddset$SD_amount[ddddset$SD_amount==0 & ddddset$amount_mean > 0] <- mean(ddddset$SD_amount)
<<<<<<< HEAD
=======
  ddddset$SD_price[ddddset$SD_price==0 & ddddset$price_mean > 0] <- mean(ddddset$SD_price)
>>>>>>> origin/master
  ddddset$proportion <- round(ddddset$freq2/ddddset$N,3)
  ddddset$purchase_frequency <- round(ddddset$p_frequency/ddddset$N,3)
  ddddset$freq2 <- NULL
  ddddset$p_frequency <- NULL
  ddddset
}

create_ddset <- function(dset,subfield=NULL) {
  if(!is.null(subfield)) {
    dset <- dset %>% filter(detail3 == subfield)
  }
  ddset <- dset %>% filter(quantity > 0)
  ddset$processed <- ""
  ddset$processed <- ifelse(str_detect(ddset$detail3,"가공"),"가공식품","신선식품")
  ddset
}

#' Fresh Month
item_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,processed) %>% summarize(
    amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
    price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
    f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,processed) %>% summarize(
    amount_mu_star = mean(purchase),
<<<<<<< HEAD
    freq0 = n(),
    freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","processed"))
  dddset <- dddset2 %>% group_by(year,month,processed) %>% summarize(
    amount_mean=sum(f*amount_mean0),
    SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
    price_mu_star=mean(purchase/quantity),
    freq0 = n(),
    freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,processed) %>% summarize(
    amount_mean=sum(f*amount_mean0),
    price_mean=sum(f*price_mean0),
    SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
    SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
    p_frequency = freq0[1],
    freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

#' Store Year Processed
store_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  #additional
<<<<<<< HEAD
  retail_cover = c("기업형슈퍼","전통시장","대형마트","소형슈퍼","전문점","무점포판매","백화점")
  retail_cover_all = c(retail_cover,"기타")
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기업형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="대형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="재래시장"] = "전통시장"
  ddset$retail[ddset$retail=="소형슈퍼마켓"] = "소형슈퍼"
  ddset$retail[ddset$retail=="편의점"] = "소형슈퍼"
  ddset$retail[ddset$retail=="유기농매장"] = "전문점"
  ddset$retail[ddset$retail=="정육점"] = "전문점"
  ddset$retail[ddset$retail=="무점포판매형"] = "무점포판매"
  ddset$retail[ddset$retail=="인터넷구매"] = "무점포판매"
  ddset$retail[ddset$retail=="백화점"] = "백화점"
  ddset$retail[!(ddset$retail %in% retail_cover)] = "기타"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
=======
  ddset$retail <- as.character(ddset$retail)
  ddset <- ddset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
  ddset$retail[ddset$retail=="기타"] = "직거래"
  ddset$retail[ddset$retail=="인터넷구매"] = "직거래"
  ddset$retail <- as.character(ddset$retail)
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,retail,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","retail","processed"))
  dddset <- dddset2 %>% group_by(year,month,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,retail=retail_cover_all,
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","retail","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,retail=c("대형마트", "재래시장", "대형슈퍼마켓","소형슈퍼마켓","직거래"),
>>>>>>> origin/master
                        processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  ddddset <- ddddset %>% inner_join(demo_dist_year,by="year")
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

#' Income Year Processed
income_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
  ddset$income_new <- as.character(ddset$income_new)
  ddset <- ddset %>% filter(income_new %in% c("A고소득","B중소득","C저소득"))
  ddset$income_new[ddset$income_new=="A고소득"] = "고소득"
  ddset$income_new[ddset$income_new=="B중소득"] = "중소득"
  ddset$income_new[ddset$income_new=="C저소득"] = "저소득"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,income_new,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,income_new,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","income_new","processed"))
  dddset <- dddset2 %>% group_by(year,month,income_new,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","income_new","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,income_new,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
    #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,income_new=c("고소득", "중소득", "저소득"),
                        processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","income_new","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_income,by=c("year","income_new")))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

family_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  #subgroup
  ddset$family_member <- "6인이상"
  ddset$family_member[ddset$family_num == 1] <- "1인"
  ddset$family_member[ddset$family_num == 2] <- "2인"
  ddset$family_member[ddset$family_num == 3] <- "3인"
  ddset$family_member[ddset$family_num == 4] <- "4인"
  ddset$family_member[ddset$family_num == 5] <- "5인"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,family_member,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,family_member,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","family_member","processed"))
  dddset <- dddset2 %>% group_by(year,month,family_member,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","family_member","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,family_member,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
    #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,family_member=c("1인","2인","3인","4인","5인","6인이상"),
                        processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","family_member","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_family,by=c("year","family_member")))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

education_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset$education[ddset$education=="null"] <- "고졸"
  ddset$education <- as.character(ddset$education)
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,education,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,education,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","education","processed"))
  dddset <- dddset2 %>% group_by(year,month,education,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","education","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,education,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
    #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,education=c("중졸이하","고졸","대졸_전문대포함_","대학원졸업이상"),
                        processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","education","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_education,by=c("year","education")))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

#' Age Year Processed
age_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,age_new,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,age_new,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","age_new","processed"))
  dddset <- dddset2 %>% group_by(year,month,age_new,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","age_new","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,age_new,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
  #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,age_new=c("30대이하", "40대연령", "50대연령", "60대이상"),
                        processed=c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","age_new","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_age,by=c("year","age_new")))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}

#' Job Year Processed
job_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
  ddset$full_housewife <- ifelse(ddset$panel_job == "전업주부","전업주부","취업주부")
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,full_housewife,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,full_housewife,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","full_housewife","processed"))
  dddset <- dddset2 %>% group_by(year,month,full_housewife,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","full_housewife","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,full_housewife,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
  #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,full_housewife=c("전업주부","취업주부"),
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","full_housewife","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_job,by=c("year","full_housewife")))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Brand Year
brand_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,normal_brand,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,normal_brand,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","normal_brand","processed"))
  dddset <- dddset2 %>% group_by(year,month,normal_brand,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","normal_brand","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,normal_brand,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
  #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,normal_brand=c("Normal","Brand"),processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","normal_brand","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Brand Year Store
brand_month_store <- function(dset,subfield=NULL) {
<<<<<<< HEAD
  retail_cover = c("기업형슈퍼","전통시장","대형마트","소형슈퍼","전문점","무점포판매","백화점")
  retail_cover_all = c(retail_cover,"기타")
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
=======
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
  ddset$retail <- as.character(ddset$retail)
  ddset <- ddset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
>>>>>>> origin/master
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기업형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="대형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="재래시장"] = "전통시장"
  ddset$retail[ddset$retail=="소형슈퍼마켓"] = "소형슈퍼"
  ddset$retail[ddset$retail=="편의점"] = "소형슈퍼"
  ddset$retail[ddset$retail=="유기농매장"] = "전문점"
  ddset$retail[ddset$retail=="정육점"] = "전문점"
  ddset$retail[ddset$retail=="무점포판매형"] = "무점포판매"
  ddset$retail[ddset$retail=="인터넷구매"] = "무점포판매"
  ddset$retail[ddset$retail=="백화점"] = "백화점"
  ddset$retail[!(ddset$retail %in% retail_cover)] = "기타"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,normal_brand,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,normal_brand,retail,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","normal_brand","retail","processed"))
  dddset <- dddset2 %>% group_by(year,month,normal_brand,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","normal_brand","retail","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,normal_brand,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
  #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,normal_brand=c("Normal","Brand"),
<<<<<<< HEAD
                        retail=retail_cover_all,
=======
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"),
>>>>>>> origin/master
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","normal_brand","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Natural Year
natural_month <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  # select subcateogries
  ddset$natural <- "관행농업"
  ddset$natural[ddset$cultivation %in% c("유기농","유기농,친환경")] <- "친환경_유기농"
  ddset$natural[ddset$cultivation %in% c("무농약","무농약_친환경","친환경_무농약")] <- "친환경_무농약"
  ddset$natural[ddset$cultivation %in% c("저농약")] <- "친환경_저농약"
  ddset$natural[ddset$cultivation %in% c("친환경일반","친환경","직접재배")] <- "친환경_일반"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,natural,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
<<<<<<< HEAD
=======
            price_mean0=mean(purchase/quantity,na.rm=T),
>>>>>>> origin/master
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,natural,processed) %>% summarize(
            amount_mu_star = mean(purchase),
<<<<<<< HEAD
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","natural","processed"))
  dddset <- dddset2 %>% group_by(year,month,natural,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
=======
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","natural","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,natural,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
>>>>>>> origin/master
            p_frequency = freq0[1],
            freq2 = freq1[1])
  #correction
  mirror <- expand.grid(year=2010:2014,month=1:12,natural=c("친환경_유기농","친환경_무농약","친환경_저농약","친환경_일반"),
        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","natural","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
<<<<<<< HEAD
  ddddset
}
#' Pack Month Gram Store
pack_month_100g <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  ddset <- ddset %>% filter(quantity_u > 0)
  # select subcateogries
  # filter 1
  retail_cover = c("기업형슈퍼","전통시장","대형마트","소형슈퍼","전문점","무점포판매","백화점")
  retail_cover_all = c(retail_cover,"기타")
=======
  ddddset
}
#' Pack Month Gram
pack_month_g <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  # select subcateogries
  ddset <- ddset %>% filter(unit_u %in% c("g","kg","Kg","KG","근"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("kg","Kg","KG")] <- ddset$quantity_u[ddset$unit_u %in% c("kg","Kg","KG")] * 1000 * ddset$quantity[ddset$unit_u %in% c("kg","Kg","KG")]
  ddset$pack[ddset$unit_u %in% c("근")] <- ddset$quantity_u[ddset$unit_u %in% c("근")] * 600 * ddset$quantity[ddset$unit_u %in% c("근")]
  ddset$pack_category <- "1kg초과"
  ddset$pack_category[ddset$pack <= 500] <- "500g이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500g초과1kg이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1kg초과","500g이하","500g초과1kg이하"),
        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Pack Month Gram Store
pack_month_g_store <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  # select subcateogries
  # filter 1
  ddset$retail <- as.character(ddset$retail)
  ddset <- ddset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
>>>>>>> origin/master
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기업형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="대형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="재래시장"] = "전통시장"
  ddset$retail[ddset$retail=="소형슈퍼마켓"] = "소형슈퍼"
  ddset$retail[ddset$retail=="편의점"] = "소형슈퍼"
  ddset$retail[ddset$retail=="유기농매장"] = "전문점"
  ddset$retail[ddset$retail=="정육점"] = "전문점"
  ddset$retail[ddset$retail=="무점포판매형"] = "무점포판매"
  ddset$retail[ddset$retail=="인터넷구매"] = "무점포판매"
  ddset$retail[ddset$retail=="백화점"] = "백화점"
  ddset$retail[!(ddset$retail %in% retail_cover)] = "기타"
  # filter 2
  ddset$unit_u <- as.character(ddset$unit_u)
  ddset <- ddset %>% filter(unit_u %in% c("g","kg","Kg","KG","근"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("kg","Kg","KG")] <- ddset$quantity_u[ddset$unit_u %in% c("kg","Kg","KG")] * 1000 * ddset$quantity[ddset$unit_u %in% c("kg","Kg","KG")]
  ddset$pack[ddset$unit_u %in% c("근")] <- ddset$quantity_u[ddset$unit_u %in% c("근")] * 600 * ddset$quantity[ddset$unit_u %in% c("근")]
  ddset$pack_category <- "1kg초과"
  ddset$pack_category[ddset$pack <= 500] <- "500g이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500g초과1kg이하"
  # weighted average
<<<<<<< HEAD
  dddset0 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            price_mean=mean(purchase/pack*100,na.rm=T),
            price_SD=sd(purchase/pack*100,na.rm=T),
            observation=n())
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1kg초과","500g이하","500g초과1kg이하"),
                        retail=retail_cover_all,
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset0,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  ddddset
}
#' Pack Month Litter Store
pack_month_l <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  ddset <- ddset %>% filter(quantity_u > 0)
  # select subcateogries
  # filter 1
  retail_cover = c("기업형슈퍼","전통시장","대형마트","소형슈퍼","전문점","무점포판매","백화점")
  retail_cover_all = c(retail_cover,"기타")
=======
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","retail","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1kg초과","500g이하","500g초과1kg이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"),
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Pack Month Litter
pack_month_l <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  # select subcateogries
  ddset <- ddset %>% filter(unit_u %in% c("L","ML","ml","l"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("L","l")] <- ddset$quantity_u[ddset$unit_u %in% c("L","l")] * 1000 * ddset$quantity[ddset$unit_u %in% c("L","l")]
  ddset$pack_category <- "1L초과"
  ddset$pack_category[ddset$pack <= 500] <- "500ML이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500ML초과1L이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1L초과","500ML이하","500ML초과1L이하"),
        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Pack Month Litter Store
pack_month_l_store <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  # select subcateogries
  # filter 1
  ddset$retail <- as.character(ddset$retail)
  ddset <- ddset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
>>>>>>> origin/master
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기업형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="대형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="재래시장"] = "전통시장"
  ddset$retail[ddset$retail=="소형슈퍼마켓"] = "소형슈퍼"
  ddset$retail[ddset$retail=="편의점"] = "소형슈퍼"
  ddset$retail[ddset$retail=="유기농매장"] = "전문점"
  ddset$retail[ddset$retail=="정육점"] = "전문점"
  ddset$retail[ddset$retail=="무점포판매형"] = "무점포판매"
  ddset$retail[ddset$retail=="인터넷구매"] = "무점포판매"
  ddset$retail[ddset$retail=="백화점"] = "백화점"
  ddset$retail[!(ddset$retail %in% retail_cover)] = "기타"
  # filter 2
  ddset$unit_u <- as.character(ddset$unit_u)
  ddset <- ddset %>% filter(unit_u %in% c("L","ML","ml","l"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("L","l")] <- ddset$quantity_u[ddset$unit_u %in% c("L","l")] * 1000 * ddset$quantity[ddset$unit_u %in% c("L","l")]
  ddset$pack_category <- "1L초과"
  ddset$pack_category[ddset$pack <= 500] <- "500ML이하"
  ddset$pack_category[ddset$pack > 500 & ddset$pack <= 1000] <- "500ML초과1L이하"
  # weighted average
<<<<<<< HEAD
  dddset0 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            price_mean=mean(purchase/pack*1000,na.rm=T),
            price_SD=sd(purchase/pack*1000,na.rm=T),
            observation=n())
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1L초과","500ML이하","500ML초과1L이하"),
                        retail=retail_cover_all,
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset0,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  ddddset
}
#' Pack Month Counting Store
pack_month_c <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  ddset <- ddset %>% filter(quantity_u > 0)
  # select subcateogries
  # filter 1
  retail_cover = c("기업형슈퍼","전통시장","대형마트","소형슈퍼","전문점","무점포판매","백화점")
  retail_cover_all = c(retail_cover,"기타")
=======
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","retail","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("1L초과","500ML이하","500ML초과1L이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"),
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Pack Month Counting
pack_month_c <- function(dset,subfield=NULL) {
  # select subcateogries
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  ddset <- ddset %>% filter(unit_u %in% c("개","판","줄","구","입","알"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("판")] <- ddset$quantity_u[ddset$unit_u %in%  c("판")] * 30 * ddset$quantity[ddset$unit_u %in%  c("판")]
  ddset$pack[ddset$unit_u %in% c("줄")] <- ddset$quantity_u[ddset$unit_u %in%  c("줄")] * 10 * ddset$quantity[ddset$unit_u %in%  c("줄")]
  ddset$pack_category <- "30개초과"
  ddset$pack_category[ddset$pack <= 15] <- "15개이하"
  ddset$pack_category[ddset$pack > 15 & ddset$pack <= 30] <- "15개초과30개이하"
  # weighted average
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("30개초과","15개이하","15개초과30개이하"),processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
  ddddset
}
#' Pack Month Counting Store
pack_month_c_store <- function(dset,subfield=NULL) {
  ddset <- create_ddset(dset,subfield)
  ddset <- ddset %>% filter(!(is.na(quantity_u)))
  # select subcateogries
  # filter 1
  ddset$retail <- as.character(ddset$retail)
  ddset <- ddset %>% filter(retail %in% c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","기타","인터넷구매"))
>>>>>>> origin/master
  ddset$retail <- as.character(ddset$retail)
  ddset$retail[ddset$retail=="기업형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="대형슈퍼마켓"] = "기업형슈퍼"
  ddset$retail[ddset$retail=="재래시장"] = "전통시장"
  ddset$retail[ddset$retail=="소형슈퍼마켓"] = "소형슈퍼"
  ddset$retail[ddset$retail=="편의점"] = "소형슈퍼"
  ddset$retail[ddset$retail=="유기농매장"] = "전문점"
  ddset$retail[ddset$retail=="정육점"] = "전문점"
  ddset$retail[ddset$retail=="무점포판매형"] = "무점포판매"
  ddset$retail[ddset$retail=="인터넷구매"] = "무점포판매"
  ddset$retail[ddset$retail=="백화점"] = "백화점"
  ddset$retail[!(ddset$retail %in% retail_cover)] = "기타"
  # filter 2
  ddset <- ddset %>% filter(unit_u %in% c("개","판","줄","구","입","알"))
  ddset$pack <- ddset$quantity_u * ddset$quantity
  ddset$pack[ddset$unit_u %in% c("판")] <- ddset$quantity_u[ddset$unit_u %in%  c("판")] * 30 * ddset$quantity[ddset$unit_u %in%  c("판")]
  ddset$pack[ddset$unit_u %in% c("줄")] <- ddset$quantity_u[ddset$unit_u %in%  c("줄")] * 10 * ddset$quantity[ddset$unit_u %in%  c("줄")]
  ddset$pack_category <- "30개초과"
  ddset$pack_category[ddset$pack <= 15] <- "15개이하"
  ddset$pack_category[ddset$pack > 15 & ddset$pack <= 30] <- "15개초과30개이하"
  # weighted average
<<<<<<< HEAD
  dddset0 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            price_mean=mean(purchase/pack,na.rm=T),
            price_SD=sd(purchase/pack,na.rm=T),
            observation=n())
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("30개초과","15개이하","15개초과30개이하"),
                        retail=retail_cover_all,
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset0,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
=======
  dddset0 <- ddset %>% group_by(year,month,panel_c2,pack_category,retail,processed) %>% summarize(
            amount_mean0=mean(purchase,na.rm=T),
            price_mean0=mean(purchase/quantity,na.rm=T),
            f=n())
  # weighted standard deviation
  dddset1 <- ddset %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mu_star = mean(purchase),
            price_mu_star=mean(purchase/quantity),
            freq0 = n(),
            freq1 = n_distinct(panel_c2))
  dddset2 <- dddset0 %>% inner_join(dddset1,by=c("year","month","pack_category","retail","processed"))
  dddset3 <- dddset2 %>% mutate(amount_mean=sum(f*amount_mean0),price_mean=sum(f*price_mean0))
  dddset <- dddset3 %>% group_by(year,month,pack_category,retail,processed) %>% summarize(
            amount_mean=sum(f*amount_mean0),
            price_mean=sum(f*price_mean0),
            SD_amount=sum(f*(amount_mean0 - amount_mu_star)^2),
            SD_price=sum(f*(price_mean0 - price_mu_star)^2),
            p_frequency = freq0[1],
            freq2 = freq1[1])
  # correction
  mirror <- expand.grid(year=2010:2014,month=1:12,pack_category=c("30개초과","15개이하","15개초과30개이하"),
                        retail=c("대형마트","재래시장","대형슈퍼마켓","소형슈퍼마켓","직거래"),
                        processed = c("신선식품","가공식품"))
  suppressWarnings(ddddset <- mirror %>% left_join(dddset,by=c("year","month","pack_category","retail","processed")))
  ddddset[is.na(ddddset)] <- 0
  suppressWarnings(ddddset <- ddddset %>% inner_join(demo_dist_year,by="year"))
  ddddset <- calculate_ddddset(ddddset)
>>>>>>> origin/master
  ddddset
}
# === END OF PROGRAM === #

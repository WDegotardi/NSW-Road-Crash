library(tidyverse)
crash <- read_csv("NSW_Road_Crash_Data_2016-2020_Crash.csv", col_types ="nfffffffffnffffffffnnffffffffffffffffffffffffnnnnn")
glimpse(crash)

crash <- crash %>%
  select(-`Reporting year`, -`Year of crash`, -`Street of crash`,
         -`Distance (from identifying feature)`, 
         -`Direction (from identifying feature)`,
         -`Identifying feature`, -`Identifying feature type`,
         -Town, -`Route no.`, -Latitude, -Longitude, -LGA,
         -`Conurbation 1`, -`Road classification (admin)`, -`RUM - code`,
         -`RUM - description`, -`DCA - description`, -`No. killed`,
         -`No. seriously injured`, -`No. moderately injured`, 
         -`No. minor-other injured`, -`Degree of crash - detailed`)

library(janitor)
colnames(crash) <- c('crashID','deg','m','d','t','st_type',
                     'schl_zone','schl_zone_a','loc_type',
                     'urban','align','perm_ft','temp_ft',
                     'haz_ft','st_light','rd_surf',
                     'surf_cond','weather','ntrl_light',
                     'signal_ope','traf_cntrl','spd_lim',
                     'dca','dca_sup','imp_type','key_tu',
                     'other_tu','number_tu')

sample_index <- sample(nrow(crash), round(nrow(crash)*.75), replace = FALSE)
crash_train <- crash[sample_index,]
crash_test <- crash[-sample_index, ]

library(ranger)

start <- Sys.time()
model <- ranger(deg ~ m + d + t + st_type + schl_zone + 
                  schl_zone_a + loc_type + urban + align + perm_ft + temp_ft +
                  haz_ft + st_light + rd_surf + surf_cond + weather + 
                  ntrl_light + signal_ope + traf_cntrl + spd_lim + dca +
                  dca_sup + imp_type + key_tu + other_tu + number_tu, 
                      data = crash_train, 
                      num.trees=5000,
                      classification = TRUE)

end <- Sys.time()
print(end-start)

summary(model)

crash_predict <- predict(model, crash_test)

crash_pred_table <- table(crash_test$deg, crash_predict$predictions)
crash_pred_table
sum(diag(crash_pred_table)) / nrow(crash_test)

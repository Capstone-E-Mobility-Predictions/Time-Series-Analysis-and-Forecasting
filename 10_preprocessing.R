# 1 Data Cleaning ----

# 1.1 Handle missing values. False Implies no missing values ----
any(is.na(data_battery))
any(is.na(data_grid))
any(is.na(data_photovoltaic))
any(is.na(data_wallboxes))

# We observe that there are no missing values


# 1.2 Handle duplicates. False implies each row is a unique time ----

any(duplicated(data_battery$V1))
any(duplicated(data_grid$V1))
any(duplicated(data_photovoltaic$V1))
any(duplicated(data_wallboxes$V1))

# We observe that there are no duplicate times recorded


# 1.3 Check for consistency in rows and columns ----
# CHECK IF DATES ARE CONSECUTIVE ----
unique_dates_db <- data.table(as.Date(data_battery$V1[!duplicated(as.Date(data_battery$V1))]))
unique_dates_db$consecutive <- c(NA,diff(as.Date(unique_dates_db$V1))==1)
unique_dates_db[unique_dates_db$consecutive==FALSE]

unique_dates_dg <- data.table(as.Date(data_grid$V1[!duplicated(as.Date(data_grid$V1))]))
unique_dates_dg$consecutive <- c(NA,diff(as.Date(unique_dates_dg$V1))==1)
unique_dates_dg[unique_dates_dg$consecutive==FALSE]

unique_dates_dp <- data.table(as.Date(data_photovoltaic$V1[!duplicated(as.Date(data_photovoltaic$V1))]))
unique_dates_dp$consecutive <- c(NA,diff(as.Date(unique_dates_dp$V1))==1)
unique_dates_dp[unique_dates_dp$consecutive==FALSE]

unique_dates_wb <- data.table(as.Date(data_wallboxes$V1[!duplicated(as.Date(data_wallboxes$V1))]))
unique_dates_wb$consecutive <- c(NA,diff(as.Date(unique_dates_wb$V1))==1)
unique_dates_dp[unique_dates_dp$consecutive==FALSE]

# 2022-09-27 and 2022-10-02 are not consecutive


# 1.4 Grouping data ----
# 1.4.1 SPLIT DATASETS INTO BEFORE AND AFTER GAP ----
data_battery_1 <- data_battery[data_battery$V1 < as.Date("2022-09-27"),]
data_battery_2 <- data_battery[data_battery$V1 >= as.Date("2022-10-02"),]

data_grid_1 <- data_grid[data_grid$V1 < as.Date("2022-09-27"),]
data_grid_2 <- data_grid[data_grid$V1 >= as.Date("2022-10-02"),]

data_photovoltaic_1 <- data_photovoltaic[data_photovoltaic$V1 < as.Date("2022-09-27"),]
data_photovoltaic_2 <- data_photovoltaic[data_photovoltaic$V1 >= as.Date("2022-10-02")]

data_wallboxes_1 <- data_wallboxes[data_wallboxes$V1 < as.Date("2022-09-27"),]
data_wallboxes_2 <- data_wallboxes[data_wallboxes$V1 >= as.Date("2022-10-02"),]


# 1.4.2 GROUP WALLBOX POWER PER DAY ----
wallboxes_jan_aug <- data.table(Date=data_wallboxes_1$V1,
                                KEBA_1=data_wallboxes_1$LEM.KEBA_P30_1.Wirkleistung_P,
                                KEBA_2=data_wallboxes_1$LEM.KEBA_P30_2.Wirkleistung_P,
                                KEBA_3=data_wallboxes_1$LEM.KEBA_P30_3.Wirkleistung_P,
                                Ladebox1=data_wallboxes_1$LEM.Ladebox1.P,
                                Ladebox2=data_wallboxes_1$LEM.Ladebox2.P,
                                Ladebox3=data_wallboxes_1$LEM.Ladebox3.P,
                                Delta_Wallbox=data_wallboxes_1$LEM.Delta_Wallbox.Wirkleistung_P,
                                Raption_50=data_wallboxes_1$LEM.Raption_50.Wirkleistung_P)

wallboxes_jan_aug <- wallboxes_jan_aug %>% mutate(Date=as.Date(Date))
wallboxes_jan_aug$total.P <- rowSums(wallboxes_jan_aug[, c(2:9)])
wallboxes_jan_aug <- wallboxes_jan_aug %>% group_by(Date) %>% 
  summarize(total_power = sum(total.P))

# 1.5 save as csv ---
write_xlsx(wallboxes_jan_aug, path="./data/preprocessed/wallboxes.xlsx")
library(dplyr)
library(writexl)
library(ggplot2)
library(readxl)
library(scales)
library(ggmap)
library(nycflights13)
library(maps)
library(usmap)

# pobranie danych z ramek załączonych w zadaniu (lata 1996-2006)

data_1996 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\1996.csv.bz2"))
data_1997 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\1997.csv.bz2"))
data_1998 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\1998.csv.bz2"))
data_1999 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\1999.csv.bz2"))
data_2000 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2000.csv.bz2"))
data_2001 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2001.csv.bz2"))
data_2002 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2002.csv.bz2"))
data_2003 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2003.csv.bz2"))
data_2004 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2004.csv.bz2"))
data_2005 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2005.csv.bz2"))
data_2006 <- as.data.frame(read.csv("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\data\\2006.csv.bz2"))



# df_1 <- read_excel("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\lotniska_nazwy.xlsx")
# df_2 <- read_excel("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\lotniska_stany.xlsx")
# df_3 <- read_excel("C:\\Users\\rysza\\Desktop\\Studia\\SEMESTR_2\\PDU\\PROJEKT_2\\strefy_czasowe.xlsx")
# lotniska <- left_join(df_1, df_2, by = "code")
# lotniska <- left_join(lotniska, df_3, by = c("time_zone_id" = "Time_Zone")) %>% arrange(code)
# View(lotniska)


rozszerzona_ramka_danych <- function(data_frame) {
  
  a <- left_join(data_frame, airports, by = c("Origin" = "faa")) %>%
       left_join(airports, by = c("Dest" = "faa"))
  
  result <- rename(a, "Origin_Name" = name.x, "Origin_Y" = lat.x, "Origin_X" = lon.x, "Origin_Alt" = alt.x, "Origin_GMT" = tz.x, "Origin_Dst" = dst.x, "Origin_Time_Zone" = tzone.x, "Dest_Name" = name.y, "Dest_Y" = lat.y, "Dest_X" = lon.y, "Dest_Alt" = alt.y, "Dest_GMT" = tz.y, "Dest_Dst" = dst.y, "Dest_Time_Zone" = tzone.y)
  
  result
  
  # a <- left_join(data_frame, lotniska, by = c("Origin" = "code"))
  # b <- left_join(data_frame, lotniska, by = c("Dest" = "code"))
  # 
  # data_frame %>%
  #   mutate(Origin_Name = a$name, Origin_Region = a$region_name, Origin_Time_Zone = a$time_zone_id, Origin_GMT = a$GMT) %>%
  #   mutate(Dest_Name = b$name, Dest_Region = b$region_name, Dest_Time_Zone = b$time_zone_id, Dest_GMT = b$GMT)
  
}

loty_na_przelomie_lat <- function(df_1, df_2) {
  
  a <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Planned_Flights = length(TailNum), .groups = 'keep')
  
  b <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    filter(Cancelled == 0) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Realized_Flights = length(TailNum), .groups = 'keep')
  
  c <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    filter(Cancelled == 1) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Cancelled_Flights = length(TailNum), .groups = 'keep')
  
  first_frame <- inner_join(a, b, by = c("Month", "DayofMonth")) %>%
                 inner_join(c, by = c("Month", "DayofMonth")) %>%
                 mutate(Year = df_1$Year[1]) %>%
                 relocate(Year, .before = 1) %>%
                 filter(Month > 9 | (Month == 9 & DayofMonth > 10))
  
  d <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Planned_Flights = length(TailNum), .groups = 'keep')
  
  e <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    filter(Cancelled == 0) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Realized_Flights = length(TailNum), .groups = 'keep')
  
  f <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled) %>%
    filter(Cancelled == 1) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Cancelled_Flights = length(TailNum), .groups = 'keep')
  
  second_frame <- inner_join(d, e, by = c("Month", "DayofMonth")) %>%
                  inner_join(f, by = c("Month", "DayofMonth")) %>%
                  mutate(Year = df_2$Year[1]) %>%
                  relocate(Year, .before = 1) %>%
                  filter(Month < 9 | (Month == 9 & DayofMonth < 11))
  
  bind_rows(first_frame, second_frame) %>%
  group_by(Month) %>%
  summarise(Planned_Flights = sum(Planned_Flights), Realized_Flights = sum(Realized_Flights), Cancelled_Flights = sum(Cancelled_Flights)) %>%
  mutate(Years = paste(substr(as.character(df_1$Year[1]), 3, 4), substr(as.character(df_2$Year[1]), 3, 4), sep = "/")) %>%
  relocate(Years, .before = 1)
  
}

loty_w_danym_roku_od_WTC <- function(data_frame) {
  
  data_frame %>%
    group_by(Years) %>%
    summarise(Planned_Flights = sum(Planned_Flights), Realized_Flights = sum(Realized_Flights), Cancelled_Flights = sum(Cancelled_Flights))
  
}

loty_w_danym_roku <- function(data_frame) {
  
  a <- data_frame %>%
       select(Month, TailNum) %>%
       group_by(Month) %>%
       summarise(Planned_Flights = length(TailNum))
  
  b <- data_frame %>%
       select(Month, TailNum, Cancelled) %>%
       filter(Cancelled == 0) %>%
       group_by(Month) %>%
       summarise(Realized_Flights = length(TailNum))
  
  c <- data_frame %>%
       select(Month, TailNum, Cancelled) %>%
       filter(Cancelled == 1) %>%
       group_by(Month) %>%
       summarise(Cancelled_Flights = length(TailNum))
  
  result <- inner_join(a, b, by = "Month") %>%
            inner_join(c, by = "Month") %>%
            mutate(Percent = round(Cancelled_Flights/Planned_Flights*100, 1)) %>%
            mutate(Year = data_frame$Year[1]) %>%
            relocate(Year, .before = 1)
  
  result
  
}

loty_w_danym_miesiacu <- function(data_frame, month) {
  
  a <- data_frame %>%
       select(Month, DayofMonth, TailNum) %>%
       filter(Month == month) %>%
       group_by(DayofMonth) %>%
       summarise(Planned_Flights = length(TailNum))
  
  b <- data_frame %>%
       select(Month, DayofMonth, TailNum, Cancelled) %>%
       filter(Month == month, Cancelled == 0) %>%
       group_by(DayofMonth) %>%
       summarise(Realized_Flights = length(TailNum))
  
  c <- data_frame %>%
       select(Month, DayofMonth, TailNum, Cancelled) %>%
       filter(Month == month, Cancelled == 1) %>%
       group_by(DayofMonth) %>%
       summarise(Cancelled_Flights = length(TailNum))
  
  result <- inner_join(a, b, by = "DayofMonth") %>%
            inner_join(c, by = "DayofMonth") %>%
            select(DayofMonth, Planned_Flights, Realized_Flights, Cancelled_Flights) %>%
            mutate(Year = data_frame$Year[1], Month = month) %>%
            relocate(Year, Month, .before = 1)
  result
  
}

loty_zrealizowane_danego_dnia <- function(data_frame, day, month) {
  
  data_frame %>%
    select(Month, DayofMonth, DepTime, Cancelled, Origin_Name, Dest_Name, FlightNum) %>%
    filter(Month == month, DayofMonth == day, Cancelled == 0) %>%
    mutate(Departure_Time = zmiana_formatu_godziny(DepTime)) %>%
    arrange(desc(DepTime)) %>%
    select(FlightNum, Origin_Name, Dest_Name, Departure_Time)
  
}

loty_11_wrzesnia_czas_nowojorski <- function(data_frame) {
  
  data_frame %>% 
  select(Month, DayofMonth, Cancelled, FlightNum, Origin, Origin_Name, Dest, Dest_Name, DepTime, Origin_GMT, Dest_GMT, Distance, Diverted) %>%
  filter(Month == 9, DayofMonth == 11, Cancelled == 0) %>%
  mutate(Departure_Time = zmiana_formatu_godziny(DepTime), DepTimeNY = zmiana_godziny(DepTime, Origin_GMT)) %>%
  select(FlightNum, Origin, Origin_Name, Origin_GMT, Dest, Dest_Name, Dest_GMT, DepTime, DepTimeNY, Distance, Diverted) %>%
  arrange(desc(DepTimeNY))
  
}

loty_z_danych_lotnisk_11_wrzesnia <- function(data_frame) {
  
 a <- data_frame %>%
      select(Month, DayofMonth, Origin, TailNum) %>%
      filter(Month == 9, DayofMonth == 11) %>%
      group_by(Origin) %>%
      summarise(Planned_Flights = length(TailNum))
  
 b <- data_frame %>%
      select(Month, DayofMonth, Origin, TailNum, Cancelled) %>%
      filter(Month == 9, DayofMonth == 11, Cancelled == 0) %>%
      group_by(Origin) %>%
      summarise(Realized_Flights = length(TailNum))
  
 c <- data_frame %>%
      select(Month, DayofMonth, Origin, TailNum, Cancelled) %>%
      filter(Month == 9, DayofMonth == 11, Cancelled == 1) %>%
      group_by(Origin) %>%
      summarise(Cancelled_Flights = length(TailNum))
  
 result <- inner_join(a, b, by = "Origin") %>%
           inner_join(c, by = "Origin") %>%
           mutate(Percent = round(Cancelled_Flights/Planned_Flights*100, 1)) %>%
           arrange(desc(Cancelled_Flights))
 
 inner_join(result, airports, by = c("Origin" = "faa")) %>%
 select(Origin_Name = name, Planned_Flights, Realized_Flights, Cancelled_Flights, Percent)
   
}

loty_danej_linii_lotniczej_na_przelomie_lat <- function(df_1, df_2, airline) {
  
  a <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Planned_Flights = length(TailNum), .groups = 'keep')
  
  b <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(Cancelled == 0, UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Realized_Flights = length(TailNum), .groups = 'keep')
  
  c <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(Cancelled == 1, UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Cancelled_Flights = length(TailNum), .groups = 'keep')
  
  first_frame <- inner_join(a, b, by = c("Month", "DayofMonth")) %>%
    inner_join(c, by = c("Month", "DayofMonth")) %>%
    mutate(Year = df_1$Year[1]) %>%
    relocate(Year, .before = 1) %>%
    filter(Month > 9 | (Month == 9 & DayofMonth > 10))
  
  d <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Planned_Flights = length(TailNum), .groups = 'keep')
  
  e <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(Cancelled == 0, UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Realized_Flights = length(TailNum), .groups = 'keep')
  
  f <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, Cancelled, UniqueCarrier) %>%
    filter(Cancelled == 1, UniqueCarrier == airline) %>%
    group_by(Month, DayofMonth) %>%
    summarise(Cancelled_Flights = length(TailNum), .groups = 'keep')
  
  second_frame <- inner_join(d, e, by = c("Month", "DayofMonth")) %>%
    inner_join(f, by = c("Month", "DayofMonth")) %>%
    mutate(Year = df_2$Year[1]) %>%
    relocate(Year, .before = 1) %>%
    filter(Month < 9 | (Month == 9 & DayofMonth < 11))
  
  bind_rows(first_frame, second_frame) %>%
    group_by(Month) %>%
    summarise(Planned_Flights = sum(Planned_Flights), Realized_Flights = sum(Realized_Flights), Cancelled_Flights = sum(Cancelled_Flights)) %>%
    mutate(Years = paste(substr(as.character(df_1$Year[1]), 3, 4), substr(as.character(df_2$Year[1]), 3, 4), sep = "/")) %>%
    relocate(Years, .before = 1)
  
}

loty_danej_linii_lotniczej_w_danym_roku <- function(data_frame, airline) {
  
  a <- data_frame %>%
       select(Month, UniqueCarrier, TailNum) %>%
       filter(UniqueCarrier == airline) %>%
       group_by(Month) %>%
       summarise(Planned_Flights = length(TailNum))
  
  b <- data_frame %>%
       select(Month, UniqueCarrier, Cancelled, TailNum) %>%
       filter(UniqueCarrier == airline, Cancelled == 0) %>%
       group_by(Month) %>%
       summarise(Realized_Flights = length(TailNum))
  
  c <- data_frame %>%
       select(Month, UniqueCarrier, Cancelled, TailNum) %>%
       filter(UniqueCarrier == airline, Cancelled == 1) %>%
       group_by(Month) %>%
       summarise(Cancelled_Flights = length(TailNum))
  
  result <- inner_join(a, b, by = "Month") %>%
            inner_join(c, by = "Month") %>%
            mutate(Year = data_frame$Year[1], Percent = round(Realized_Flights/Planned_Flights*100, 1)) %>%
            relocate(Year, .before = 1)
  
  result
  
}

loty_danej_linii_lotniczej_we_wrzesniu_2001 <- function(data_frame, airline) {
  
  a <- data_frame %>%
       select(Month, DayofMonth, UniqueCarrier, TailNum) %>%
       filter(Month == 9, UniqueCarrier == airline) %>%
       group_by(DayofMonth) %>%
       summarise(Planned_Flights = length(TailNum))
  
  b <- data_frame %>%
       select(Month, DayofMonth, UniqueCarrier, Cancelled, TailNum) %>%
       filter(Month == 9, UniqueCarrier == airline, Cancelled == 0) %>%
       group_by(DayofMonth) %>%
       summarise(Realized_Flights = length(TailNum)) %>%
       rbind(c(DayofMonth = 12, Realized_Flights = 0)) %>%
       arrange(DayofMonth)
  
  c <- data_frame %>%
       select(Month, DayofMonth, UniqueCarrier, Cancelled, TailNum) %>%
       filter(Month == 9, UniqueCarrier == airline, Cancelled == 1) %>%
       group_by(DayofMonth) %>%
       summarise(Cancelled_Flights = length(TailNum))
  
  result <- inner_join(a, b, by = "DayofMonth") %>%
            inner_join(c, by = "DayofMonth") %>%
            mutate(Percent = round(Realized_Flights/Planned_Flights*100, 1))
  
  result
  
}

linie_lotnicze_na_przelomie_lat <- function(df_1, df_2) {
  
  string = paste(substr(as.character(df_1$Year[1]), 3, 4), substr(as.character(df_2$Year[1]), 3, 4), sep = "/")
  
  a <- df_1 %>%
    select(Year, Month, DayofMonth, TailNum, UniqueCarrier) %>%
    filter(Month > 9 | (Month == 9 & DayofMonth > 10)) %>%
    group_by(UniqueCarrier) %>%
    summarise(Planned_Flights = length(TailNum))
  
  
  b <- df_2 %>%
    select(Year, Month, DayofMonth, TailNum, UniqueCarrier) %>%
    filter(Month < 9 | (Month == 9 & DayofMonth < 11)) %>%
    group_by(UniqueCarrier) %>%
    summarise(Planned_Flights = length(TailNum))
  
  result <- rbind(a, b) %>%
    group_by(UniqueCarrier) %>%
    summarise(sum(Planned_Flights))
  
  colnames(result)[2] <- string
  result
  
}

zmiana_godziny <- function(x, y) {
  
  czas <- ifelse(y == -10, x + 500, ifelse(y == -9, x + 400, ifelse(y == -8, x + 300, ifelse(y == -7, x + 200, ifelse(y == -6, x + 100, x)))))
  czas <- ifelse(czas > 2400, czas - 2400, czas)
  czas
  
}

zmiana_formatu_godziny <- function(x) {
  a <- as.numeric(x %/% 100)
  b <- as.numeric(x - x %/% 100 * 100)
  c <- ifelse(b < 10, paste(0, b, sep = ""), as.character(b))
  ifelse(is.na(x) == F, paste(a, c, sep = ":"), NA)
}

strefa_czasowa <- function(x) {
  
  ifelse(x <= 900, result <- "8:46-9:00", ifelse(x <= 915, result <- "9:01-9:15", result <- "9:16-9:30"))
  result
  
}



years_96_97 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_1996, data_1997))
years_97_98 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_1997, data_1998))
years_98_99 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_1998, data_1999))
years_99_00 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_1999, data_2000))
years_00_01 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2000, data_2001))
years_01_02 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2001, data_2002))
years_02_03 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2002, data_2003))
years_03_04 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2003, data_2004))
years_04_05 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2004, data_2005))
years_05_06 <- loty_w_danym_roku_od_WTC(loty_na_przelomie_lat(data_2005, data_2006))

years_96_97_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1996, data_1997, "AA"))
years_97_98_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1997, data_1998, "AA"))
years_98_99_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1998, data_1999, "AA"))
years_99_00_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1999, data_2000, "AA"))
years_00_01_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2000, data_2001, "AA"))
years_01_02_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2001, data_2002, "AA"))
years_02_03_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2002, data_2003, "AA"))
years_03_04_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2003, data_2004, "AA"))
years_04_05_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2004, data_2005, "AA"))
years_05_06_AA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2005, data_2006, "AA"))

years_96_97_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1996, data_1997, "UA"))
years_97_98_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1997, data_1998, "UA"))
years_98_99_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1998, data_1999, "UA"))
years_99_00_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_1999, data_2000, "UA"))
years_00_01_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2000, data_2001, "UA"))
years_01_02_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2001, data_2002, "UA"))
years_02_03_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2002, data_2003, "UA"))
years_03_04_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2003, data_2004, "UA"))
years_04_05_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2004, data_2005, "UA"))
years_05_06_UA <- loty_w_danym_roku_od_WTC(loty_danej_linii_lotniczej_na_przelomie_lat(data_2005, data_2006, "UA"))



data_2001_extended <- rozszerzona_ramka_danych(data_2001)



#_______________________PORWANE_SAMOLOTY_(LOT_93_i_LOT_175)_____________________

porwane_samoloty <- data_2001 %>%
                    select(Month, DayofMonth, Cancelled, FlightNum, UniqueCarrier, Origin, Dest, CRSDepTime, DepTime, DepDelay, TaxiOut, Diverted) %>%
                    filter( Month == 9 & DayofMonth == 11 & Cancelled == 0 & (FlightNum == 11 | FlightNum == 175 | FlightNum == 77 | FlightNum == 93) & (UniqueCarrier == "AA" | UniqueCarrier == "UA")) %>%
                    select(FlightNum, UniqueCarrier, Origin, Dest, CRSDepTime, DepTime, DepDelay, TaxiOut, Diverted)

View(porwane_samoloty)


# ________________________LOTY_W_LATACH_1996_2006_______________________________

zestawienie_lotow_w_latach_1996_2006 <- rbind(years_96_97, years_97_98, years_98_99, years_99_00, years_00_01, years_01_02, years_02_03, years_03_04, years_04_05, years_05_06)
View(zestawienie_lotow_w_latach_1996_2006)

### write_xlsx(zestawienie_lotow_w_latach_1996_2006, "zestawienie_lotow_w_latach_1996_2006.xlsx")



#______________________________LOTY_W_2001______________________________________

zestawienie_lotow_w_2001 <- loty_w_danym_roku(data_2001)
View(zestawienie_lotow_w_2001)

### write_xlsx(zestawienie_lotow_w_2001, "zestawienie_lotow_w_2001.xlsx")



# _________________________LOTY_WE_WRZEŚNIU_2001________________________________

zestawienie_lotow_we_wrzesniu_2001 <- loty_w_danym_miesiacu(data_2001, 9)
View(zestawienie_lotow_we_wrzesniu_2001)

### write_xlsx(zestawienie_lotow_we_wrzesniu_2001, "zestawienie_lotow_we_wrzesniu_2001.xlsx")



#_______________________ZREALIZOWANE_LOTY_11_WRZEŚNIA___________________________

loty_zrealizowane_11_wrzesnia <- loty_11_wrzesnia_czas_nowojorski(data_2001_extended) %>%
                                 select(DepTimeNY, Origin_Name, Dest_Name) %>%
                                 mutate(Departure_Time = zmiana_formatu_godziny(DepTimeNY)) %>%
                                 select(Departure_Time, Origin_Name, Dest_Name)

View(loty_zrealizowane_11_wrzesnia)   # loty_zrealizowane_danego_dnia(data_2001_extended, 11, 9)



#___________________LOTY_LOTNISKA_W_BOSTONIE_(lot 11 i 175)_____________________

zrealizowane_loty_z_Bostonu_11_wrzesnia <- data_2001 %>%
                                           select(Month, DayofMonth, DepTime, Cancelled, Origin, Dest, FlightNum) %>%
                                           filter(Month == 9, DayofMonth == 11, Cancelled == 0, Origin == 'BOS') %>%
                                           mutate(Departure_Time = zmiana_formatu_godziny(DepTime)) %>%
                                           arrange(desc(DepTime)) %>%
                                           select(FlightNum, Dest, Departure_Time)

View(zrealizowane_loty_z_Bostonu_11_wrzesnia)



#_______________________LOTY_LOTNISKA_W_NEWARK_(lot 93)_________________________

zrealizowane_loty_z_Newark_11_wrzesnia <- data_2001 %>%
                                          select(Month, DayofMonth, DepTime, Cancelled, Origin, Dest, FlightNum) %>%
                                          filter(Month == 9, DayofMonth == 11, Cancelled == 0, Origin == 'EWR') %>%
                                          mutate(Departure_Time = zmiana_formatu_godziny(DepTime)) %>%
                                          arrange(desc(DepTime)) %>%
                                          select(FlightNum, Dest, Departure_Time)

View(zrealizowane_loty_z_Newark_11_wrzesnia)


# ______________________LOTY_Z_DANYCH_LOTNISK_11_WRZEŚNIA_______________________

zestawienie_lotow_z_danych_lotnisk_11_wrzesnia <- loty_z_danych_lotnisk_11_wrzesnia(data_2001)
View(zestawienie_lotow_z_danych_lotnisk_11_wrzesnia)


#______________________________OSTATNIE_WYLOTY__________________________________

loty_11_wrzesnia <- loty_11_wrzesnia_czas_nowojorski(data_2001_extended)

ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia <- loty_11_wrzesnia %>%
                                                select(FlightNum, Origin, Origin_Name, Dest, Dest_Name, DepTimeNY, Origin_GMT, Dest_GMT, Distance, Diverted) %>%
                                                filter(DepTimeNY > 845) %>%
                                                group_by(Origin_Name) %>%
                                                filter(DepTimeNY == max(DepTimeNY)) %>%
                                                arrange(DepTimeNY) %>%
                                                mutate(Departure_Time = zmiana_formatu_godziny(DepTimeNY), Zone = strefa_czasowa(DepTimeNY))

ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia <- left_join(ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia, airports, by = c("Origin" = "faa")) %>%
                                                select(FlightNum, Origin_Name, Departure_Time, Dest_Name, Distance, Diverted, Origin_Y = lat, Origin_X = lon, Zone)

View(ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia)



zmiana_trasy_ostatnich_lotow_z_danych_lotnisk_11_wrzesnia <- ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia %>%
                                                             select(everything()) %>%
                                                             filter(Distance >= 300) %>%
                                                             group_by(Diverted) %>%
                                                             summarise(Count = length(Origin_Name)) %>%
                                                             mutate(Percent = round(Count/sum(Count)*100, 1))

View(zmiana_trasy_ostatnich_lotow_z_danych_lotnisk_11_wrzesnia)



#____________________WPŁYW_DANYCH_LINII_NA_BRANŻĘ_LOTNICZĄ_W_USA________________

wplyw_danych_linii_lotniczych_w_USA <- data_2001 %>%
                                       select(Year, Month, DayofMonth, TailNum, UniqueCarrier) %>%
                                       filter((Month < 9) | (Month == 9 & DayofMonth < 11)) %>%
                                       group_by(UniqueCarrier) %>%
                                       summarise(Number = length(TailNum)) %>%
                                       arrange(desc(Number)) %>%
                                       mutate(Percent = round(Number/sum(Number)*100, 0))

View(wplyw_danych_linii_lotniczych_w_USA)


#_______________________SKUTKI_ZAMACHU_DLA_AMERICAN_AIRLINES____________________

American_Airlines_wrzesien_2001 <- loty_danej_linii_lotniczej_we_wrzesniu_2001(data_2001, "AA")
View(American_Airlines_wrzesien_2001)


loty_AA_2001 <- loty_danej_linii_lotniczej_w_danym_roku(data_2001, "AA")
loty_AA_2002 <- loty_danej_linii_lotniczej_w_danym_roku(data_2002, "AA")
American_Airlines_2001_2002 <- rbind(loty_AA_2001, loty_AA_2002)
View(American_Airlines_2001_2002)


American_Airlines_w_latach_1996_2006 <- rbind(years_96_97_AA, years_97_98_AA, years_98_99_AA, years_99_00_AA, years_00_01_AA,
                                              years_01_02_AA, years_02_03_AA, years_03_04_AA, years_04_05_AA, years_05_06_AA)
View(American_Airlines_w_latach_1996_2006)



#_______________________SKUTKI_ZAMACHU_DLA_UNITED_AIRLINES______________________


United_Airlines_wrzesien_2001 <- loty_danej_linii_lotniczej_we_wrzesniu_2001(data_2001, "UA")
View(United_Airlines_wrzesien_2001)


loty_UA_2001 <- loty_danej_linii_lotniczej_w_danym_roku(data_2001, "UA")
loty_UA_2002 <- loty_danej_linii_lotniczej_w_danym_roku(data_2002, "UA")
United_Airlines_2001_2002 <- rbind(loty_UA_2001, loty_UA_2002)
View(United_Airlines_2001_2002)


United_Airlines_w_latach_1996_2006 <- rbind(years_96_97_UA, years_97_98_UA, years_98_99_UA, years_99_00_UA, years_00_01_UA,
                                            years_01_02_UA, years_02_03_UA, years_03_04_UA, years_04_05_UA, years_05_06_UA)
View(United_Airlines_w_latach_1996_2006)



#_____________________________LINIE_LOTNICZE____________________________________

airlines_96_97 <- linie_lotnicze_na_przelomie_lat(data_1996, data_1997)
airlines_97_98 <- linie_lotnicze_na_przelomie_lat(data_1997, data_1998)
airlines_98_99 <- linie_lotnicze_na_przelomie_lat(data_1998, data_1999)
airlines_99_00 <- linie_lotnicze_na_przelomie_lat(data_1999, data_2000)
airlines_00_01 <- linie_lotnicze_na_przelomie_lat(data_2000, data_2001)
airlines_01_02 <- linie_lotnicze_na_przelomie_lat(data_2001, data_2002)
airlines_02_03 <- linie_lotnicze_na_przelomie_lat(data_2002, data_2003)
airlines_03_04 <- linie_lotnicze_na_przelomie_lat(data_2003, data_2004)
airlines_04_05 <- linie_lotnicze_na_przelomie_lat(data_2004, data_2005)
airlines_05_06 <- linie_lotnicze_na_przelomie_lat(data_2005, data_2006)


linie_lotnicze_1996_2006 <- full_join(airlines_96_97, airlines_97_98, by = "UniqueCarrier") %>%
                            full_join(airlines_98_99, by = "UniqueCarrier") %>%
                            full_join(airlines_99_00, by = "UniqueCarrier") %>%
                            full_join(airlines_00_01, by = "UniqueCarrier") %>%
                            full_join(airlines_01_02, by = "UniqueCarrier") %>%
                            full_join(airlines_02_03, by = "UniqueCarrier") %>%
                            full_join(airlines_03_04, by = "UniqueCarrier") %>%
                            full_join(airlines_04_05, by = "UniqueCarrier") %>%
                            full_join(airlines_05_06, by = "UniqueCarrier")


linie_lotnicze_1996_2006[is.na(linie_lotnicze_1996_2006)] <- 0
View(linie_lotnicze_1996_2006)


  
#__________________________________WYKRES_1_1___________________________________

data_frame_1 <- zestawienie_lotow_w_latach_1996_2006 %>% 
                mutate(Index = rep(c("before", "after"), each = 5))


wykres_1_1 <- ggplot(data_frame_1) +
              geom_col(aes(x = factor(Years, levels = unique(Years)), y = Planned_Flights, fill = Index),
                       width = 0.5,
                       alpha = 0.75,
                       show.legend = FALSE) +
              scale_fill_manual(values = c("#3399FF", "#00008B"), breaks = unique(data_frame_1$Index)) +
              coord_cartesian(ylim = c(0, 7500000)) +
              scale_y_continuous(breaks = seq(0, 7500000, by = 1000000),
                                 minor_breaks = seq(0, 7500000, by = 250000),
                                 labels = comma_format()) +
              xlab("Lata") +
              ylab("Liczba lotów") +
              ggtitle("Zestawienie lotów w latach 1996 - 2006") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
                    plot.margin = margin(t = 20, r = 30, b = 10, l = 10))

ggsave("zaplanowane_1996_2006.png", wykres_1_1, width = 8, height = 5, units = "in")


#__________________________________WYKRES_1_2___________________________________

# wykres_1_2 <- ggplot(data_frame_1) +
#               geom_col(aes(x = factor(Years, levels = unique(Years)), y = Realized_Flights, fill = Index),
#                        width = 0.5,
#                        alpha = 0.75,
#                        show.legend = FALSE) +
#               scale_fill_manual(values = c("#3399FF", "#00008B"), breaks = unique(data_frame_1$Index)) +
#               coord_cartesian(ylim = c(4500000, 7500000)) +
#               scale_y_continuous(breaks = seq(4500000, 7500000, by = 500000), labels = comma_format()) +
#               xlab("Lata") +
#               ylab("Liczba lotów") +
#               ggtitle("Zestawienie zrealizowanych lotów w latach 1996 - 2006") +
#               theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
#                     axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
#                     plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
#                     plot.margin = margin(t = 20, r = 30, b = 10, l = 10))
# 
# ggsave("zrealizowane_1996_2006.png", wykres_1_2, width = 8, height = 5, units = "in")



#__________________________________WYKRES_2_1___________________________________

planowane_2001 <- zestawienie_lotow_w_2001 %>%
                  select(Month, Flights = Planned_Flights) %>%
                  mutate(Loty = "Planowane")

zrealizowane_2001 <- zestawienie_lotow_w_2001 %>%
                     select(Month, Flights = Realized_Flights) %>%
                     mutate(Loty = "Zrealizowane")

data_frame_2 <- bind_rows(planowane_2001, zrealizowane_2001)


wykres_2_1 <- ggplot(data_frame_2, aes(x = Month, y = Flights, color = Loty)) +
              geom_line(size = 0.75) +
              geom_point(size = 2) +
              scale_color_manual(values = c("#3399FF", "#228B22")) +
              coord_cartesian(ylim = c(350000, 550000)) +
              scale_y_continuous(breaks = seq(350000, 550000, by = 50000),
                                 minor_breaks = seq(350000, 550000, by = 10000),
                                 labels = comma_format()) +
              scale_x_continuous(breaks = seq(1, 12, by = 1), minor_breaks = NULL) +
              xlab("Miesiąc") +
              ylab("Liczba lotów") +
              ggtitle("Zestawienie lotów w 2001 roku") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)),
                    legend.title = element_text(face = "bold", size = 10, margin = margin(b = 5)),
                    legend.box.margin = margin(l = 0),
                    plot.margin = margin(t = 20, r = 5, b = 10, l = 10))
  
  
ggsave("loty_2001.png", wykres_2_1, width = 8, height = 5, units = "in")
 


#__________________________________WYKRES_2_2___________________________________

wykres_2_2 <- ggplot(zestawienie_lotow_w_2001) +
              geom_line(aes(x = Month, y = Cancelled_Flights), color = "#DC143C", size = 0.75) +
              geom_point(aes(x = Month, y = Cancelled_Flights), color = "#DC143C", size = 2) +
              coord_cartesian(ylim = c(0, 100000)) +
              scale_y_continuous(breaks = seq(0, 100000, by = 20000), labels = comma_format()) +
              scale_x_continuous(breaks = seq(1, 12, by = 1), minor_breaks = NULL) +
              xlab("Miesiąc") +
              ylab("Liczba lotów") +
              ggtitle("Zestawienie odwołanych lotów w 2001 roku") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
                    plot.margin = margin(t = 20, r = 30, b = 10, l = 10))


ggsave("odwolane_loty_2001.png", wykres_2_2, width = 8, height = 5, units = "in")



#__________________________________WYKRES_3_1___________________________________

planowane_wrzesien <- zestawienie_lotow_we_wrzesniu_2001 %>%
                         select(DayofMonth, Flights = Planned_Flights) %>%
                         mutate(Loty = "Planowane")

zrealizowane_wrzesien <- zestawienie_lotow_we_wrzesniu_2001 %>%
                         select(DayofMonth, Flights = Realized_Flights) %>%
                         mutate(Loty = "Zrealizowane")

data_frame_3 <- bind_rows(planowane_wrzesien, zrealizowane_wrzesien)


wykres_3_1 <- ggplot(data_frame_3, aes(x = DayofMonth, y = Flights, color = Loty)) +
              geom_line(size = 0.5) +
              geom_point(size = 1.5) +
              scale_color_manual(values = c("#3399FF", "#228B22")) +
              coord_cartesian(ylim = c(0, 18000)) +
              scale_y_continuous(breaks = seq(0, 18000, by = 2000),
                                 labels = comma_format()) +
              scale_x_continuous(breaks = seq(1, 30, by = 2),
                                 minor_breaks = seq(1, 30, by = 1)) +
              xlab("Dzień") +
              ylab("Liczba lotów") +
              ggtitle("Zestawienie lotów we wrześniu 2001 roku") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)),
                    legend.title = element_text(face = "bold", size = 10, margin = margin(b = 5)),
                    plot.margin = margin(t = 20, r = 5, b = 10, l = 10))
  
  
ggsave("loty_wrzesien_2001.png", wykres_3_1, width = 8, height = 5, units = "in")



#__________________________________WYKRES_3_2___________________________________

wykres_3_2 <- ggplot(zestawienie_lotow_we_wrzesniu_2001) +
              geom_line(aes(x = DayofMonth, y = Cancelled_Flights), color = "#DC143C", size = 0.5) +
              geom_point(aes(x = DayofMonth, y = Cancelled_Flights), color = "#DC143C", size = 1.5) +
              coord_cartesian(ylim = c(0, 18000)) +
              scale_y_continuous(breaks = seq(0, 18000, by = 2000),
                                 labels = comma_format()) +
              scale_x_continuous(breaks = seq(1, 30, by = 2),
                                 minor_breaks = seq(1, 30, by = 1)) +
              xlab("Dzień") +
              ylab("Liczba lotów") +
              ggtitle("Zestawienie odwołanych lotów we wrześniu 2001 roku") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)),
                    plot.margin = margin(t = 20, r = 40, b = 10, l = 10))


ggsave("odwolane_loty_wrzesien_2001.png", wykres_3_2, width = 8, height = 5, units = "in")



#__________________________________WYKRES_4_____________________________________

data_frame_4 <- American_Airlines_w_latach_1996_2006 %>% 
                mutate(Index = rep(c("before", "after"), each = 5))

wykres_4 <- ggplot(data_frame_4) +
            geom_col(aes(x = factor(Years, levels = unique(Years)), y = Planned_Flights, fill = Index),
                     width = 0.5,
                     alpha = 0.75,
                     show.legend = FALSE) +
            scale_fill_manual(values = c("#696969", "#800000"), breaks = unique(data_frame_4$Index)) +
            coord_cartesian(ylim = c(0, 800000)) +
            scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma_format()) +
            xlab("Lata") +
            ylab("Liczba lotów") +
            ggtitle("Zestawienie lotów linii American Airlines w latach 1996 - 2006") +
            theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                  axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                  plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
                  plot.margin = margin(t = 20, r = 30, b = 10, l = 10))



ggsave("American_Airlines_1996_2006.png", wykres_4, width = 8, height = 5, units = "in")



#__________________________________WYKRES_5_____________________________________

data_frame_5 <- United_Airlines_w_latach_1996_2006 %>% 
                mutate(Index = rep(c("before", "after"), each = 5))

wykres_5 <- ggplot(data_frame_5) +
            geom_col(aes(x = factor(Years, levels = unique(Years)), y = Planned_Flights, fill = Index),
                     width = 0.5,
                     alpha = 0.75,
                     show.legend = FALSE) +
            scale_fill_manual(values = c("#00008B", "#800000"), breaks = unique(data_frame_5$Index)) +
            coord_cartesian(ylim = c(0, 800000)) +
            scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma_format()) +
            xlab("Lata") +
            ylab("Liczba lotów") +
            ggtitle("Zestawienie lotów linii United Airlines w latach 1996 - 2006") +
            theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                  axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                  plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
                  plot.margin = margin(t = 20, r = 30, b = 10, l = 10))



ggsave("United_Airlines_1996_2006.png", wykres_5, width = 8, height = 5, units = "in")



#__________________________________WYKRES_6_1___________________________________

data_frame_6 <- wplyw_danych_linii_lotniczych_w_USA

# wykres_6_1 <- ggplot(data_frame_6, aes(x = "", y = Percent, fill = UniqueCarrier)) +
#               geom_bar(stat = "identity", color = "black", size = 1) +
#               scale_fill_manual(values = c(rep("#C0C0C0", 2), "#800000", "#00008B", rep("#C0C0C0", 8)), breaks = unique(data_frame_6$UniqueCarrier)) +
#               geom_text(aes(label = paste(Percent, "%"), x = 1.55), position = position_stack(vjust = 0.5), size = 3, fontface = "bold") + # color = "white"
#               theme_void() +
#               coord_polar(theta = "y") +
#               ggtitle("Wpływ linii lotnicznych w 2001 roku przed zamachem") +
#               labs(fill = "Linia lotnicza") +
#               theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t = 30)), # color = "white"
#                     # legend.title = element_text(color = "white"),
#                     # legend.text = element_text(color = "white"),
#                     plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
# 
# ggsave("linie_lotnicze_2001_kolowy.png", wykres_6_1, width = 8, height = 5, units = "in")



#__________________________________WYKRES_6_2___________________________________

wykres_6_2 <- ggplot(data_frame_6, aes(x = factor(UniqueCarrier, levels = unique(UniqueCarrier)), y = Percent, fill = UniqueCarrier)) +
              geom_col() +
              scale_fill_manual(values = c(rep("#808080", 2), "#800000", "#00008B", rep("#808080", 8)), breaks = unique(data_frame_6$UniqueCarrier)) +
              ggtitle("Wpływ linii lotnicznych w 2001 roku przed zamachem") +
              coord_cartesian(ylim = c(0, 15)) +
              scale_y_continuous(breaks = seq(0, 15, by = 3),
                                 minor_breaks = seq(0, 20, by = 1)) +
              xlab("Linia lotnicza") +
              ylab("Liczba lotów [%]") +
              theme(axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 10)),
                    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
                    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20)), 
                    plot.margin = margin(t = 20, r = 30, b = 10, l = 10),
                    legend.position = "none")

ggsave("linie_lotnicze_2001.png", wykres_6_2, width = 8, height = 5, units = "in")



#__________________________________WYKRES_7_____________________________________

data_frame_7 <- rename(ostatnie_wyloty_z_danych_lotnisk_11_wrzesnia, "Godzina" = Zone)


wykres_7 <- ggplot(data = map_data("state"), aes(x = long, y = lat, group = group)) + 
            geom_polygon(color = "white", fill = "#D3D3D3") +
            geom_point(data = data_frame_7, aes(x = Origin_X, y = Origin_Y, group = Origin_Name, color = Godzina), size = 1) +
            scale_color_manual(values = c("#33CC33", "#3333FF", "#FF0033"), breaks = unique(data_frame_7$Godzina)) +
            theme_minimal() +
            guides(fill = FALSE) +
            ggtitle("Czyszczenie przestrzeni powietrznej po atakach na WTC") +
            theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                  legend.title = element_text(face = "bold"), # color = "white"
                  # legend.text = element_text(color = "white"),
                  plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(t = 10)), #color = "white"
                  plot.margin = margin(t = 15, r = 10, b = 15, l = 10))

ggsave("ostatnie_odloty.png", wykres_7, width = 8, height = 5, units = "in")


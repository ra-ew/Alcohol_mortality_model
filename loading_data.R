library(tidyverse)
library(readxl)
library(readr)
library(imputeTS)
library(psych)

#List of column names for years 1995 - 1999
list19 <- c("`1995`","`1996`","`1997`","`1998`","`1999`")

#Reading file - Unemployment rate
#Source: https://ec.europa.eu/eurostat/databrowser/product/page/TIPSUN20
Unemplyment_rate <- read_excel("Dane/Unemplyment_rate.xlsx", 
                               sheet = "Sheet 1", skip = 8, n_max = 30) %>% 
  select(-starts_with(".")) %>% 
  filter(!row_number()==1L) %>% 
  na_if(":") %>% 
  mutate_each_(as.numeric, list19) %>%
  rename(COUNTRY = time) %>% 
  gather(key = YEAR, value = UNEMP, -COUNTRY) %>% 
  mutate(COUNTRY = replace(COUNTRY, COUNTRY == "Germany (until 1990 former territory of the FRG)", "Germany")) %>% 
  filter(YEAR %in% c(2001:2015))

summary(Unemplyment_rate)  
glimpse(Unemplyment_rate)  

#Reading file - Disposable income
#Source: https://ec.europa.eu/eurostat/databrowser/product/page/SDG_10_20
Disposable_income <- read_excel("Dane/Disposable_income.xlsx",
                                sheet = "Sheet 1", skip = 7, n_max = 31) %>% 
  select(-starts_with(".")) %>% 
  filter(!row_number()==1L) %>%
  na_if(":") %>%
  rename(COUNTRY = time) %>%
  gather(key = YEAR, value = INCOME, -COUNTRY) %>% 
  mutate(COUNTRY = replace(COUNTRY, COUNTRY == "Germany (until 1990 former territory of the FRG)", "Germany"))

summary(Disposable_income)
glimpse(Disposable_income)

#Reading files - Alcohol consumption
#Source: https://www.who.int/data/gho/indicator-metadata-registry/imr-details/462
alcohol_consumption_2000_2009 <- read_csv("Dane/alcohol_consumption_2000_2009.csv", 
                                          skip = 1) %>% 
  select(-`Data Source`) %>% 
  rename(COUNTRY = Country, 
         TYPE = `Beverage Types`) %>% 
  gather(key=YEAR, value = CONSUMP, -c(COUNTRY, TYPE)) %>% 
  spread(key=TYPE, value = CONSUMP)

alcohol_consumption_2010_2016 <- read_csv("Dane/alcohol_consumption_2010_2016.csv", 
                                          skip = 1) %>% 
  select(-`Data Source`) %>% 
  rename(COUNTRY = Country, 
         TYPE = `Beverage Types`) %>% 
  gather(key=YEAR, value = CONSUMP, -c(COUNTRY, TYPE)) %>% 
  spread(key=TYPE, value = CONSUMP)

alcohol_consump <- rbind(alcohol_consumption_2000_2009, alcohol_consumption_2010_2016) %>% 
  select(-`All types`) %>% 
  rename(BEER = Beer,
         WINE = Wine, 
         SPIRITS = Spirits,
         OTHER = `Other alcoholic beverages`) %>% 
  mutate(COUNTRY = replace(COUNTRY, COUNTRY == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))

#Reading file - Death rates
death_rates <- read_csv("Dane/rate-of-premature-deaths-due-to-alcohol.csv") %>% 
  select(-Code) %>% 
  rename(COUNTRY = Entity,
         YEAR = Year, 
         MORTALITY = 3) %>% 
  mutate(YEAR = as.character(YEAR), 
         COUNTRY = replace(COUNTRY, COUNTRY == "Czech Republic", "Czechia"))

#Merging the data
final_data <- Unemplyment_rate %>% 
  left_join(Disposable_income) %>% 
  left_join(alcohol_consump) %>% 
  left_join(death_rates) %>% 
  filter(COUNTRY != "Malta") %>% 
  mutate(OTHER = replace(OTHER, COUNTRY %in% c("Austria", "Hungary") & is.na(OTHER), 0),
         INCOME = as.numeric(INCOME))

#Viewing the missing values
#View(final_data[rowSums(is.na(final_data)) > 0,])

#Imputing missing values
final_data_imputed <- final_data %>% 
  mutate(BEER = replace(BEER, COUNTRY == "Hungary" & is.na(BEER), mean(c(4.47, 4.30))),
         SPIRITS = replace(SPIRITS, COUNTRY == "Hungary" & is.na(SPIRITS), mean(c(4.17, 4.14))),
         WINE = replace(WINE, COUNTRY == "Hungary" & is.na(WINE), mean(c(4.50, 4.60))))

#Temp. table - imputing data for Croatia with na_interpolation function
croatia_imputed <- final_data %>% 
  filter(COUNTRY == "Croatia") %>% 
  select(INCOME) %>% 
  na_interpolation(option="spline")

#Imputing data for Croatia
final_data_imputed[final_data_imputed$COUNTRY=="Croatia","INCOME"] <- croatia_imputed

#Viewing the missing values - no missing values
#View(final_data_imputed[rowSums(is.na(final_data_imputed)) > 0,])

#Writing final file
write.csv(final_data_imputed,'final_data.csv')

#Statistics for data with imputed values
summary(final_data_imputed)
describe(final_data_imputed[,-1])
wsp_zm <- apply(final_data_imputed[-c(1:2)], 2, function(x) sd(x, na.rm = T)/mean(x, na.rm = T))

#Correlation for imputed values
(korelacja <- cor(final_data_imputed[-c(1:2)]))

#writing csv with correlation matrix
write.csv(korelacja,'korelacja.csv')




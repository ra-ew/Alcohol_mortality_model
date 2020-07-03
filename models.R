library(plm)
library(lmtest) #bptest

data <- read_csv("final_data.csv", col_types = cols(X1 = col_skip()))

#Fixed Effects (FE) model
fixed <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data, index=c("COUNTRY", "YEAR"), model="within")
summary(fixed)

#Random Effects (RE) model
random <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data, index=c("COUNTRY", "YEAR"), model="random")
summary(random)

#Hausman test - model selection
phtest(fixed, random)
#RE model selected

###TESTS ####
#Testing the significance of effects in RE model
pool <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data, index=c("COUNTRY", "YEAR"), model="pooling")
summary(pool)
plmtest(random, type=c("bp"))

#Testing serial correlation
pbgtest(random)

#Testing heteroskedasticity
bptest(random)
#Heteroskedasticity 

#Testing the normality of error term
shapiro.test(residuals(random))
#Non-normality of the error term

###SCALED ####
data_scaled <- read.csv("final_data.csv") %>% 
  select(-X) %>% 
  mutate_at(c(3:9), list(~c(scale(.))))

#Fixed Effects (FE) model
fixed_scaled <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data_scaled, index=c("COUNTRY", "YEAR"), model="within")
summary(fixed_scaled)

#Random Effects (RE) model
random_scaled <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data_scaled, index=c("COUNTRY", "YEAR"), model="random")
summary(random_scaled)

#Hausman test - model selection
phtest(fixed_scaled, random_scaled)
#RE model selected

###TESTS FOR SCALED DATA MODEL####
#Testing the significance of effects in RE model
pool <- plm(MORTALITY~UNEMP+INCOME+BEER+OTHER+SPIRITS+WINE, data=data_scaled, index=c("COUNTRY", "YEAR"), model="pooling")
summary(pool)
plmtest(random_scaled, type=c("bp"))

#Testing serial correlation
pbgtest(random_scaled)

#Testing heteroskedasticity
bptest(random_scaled)
#Heteroskedasticity 

#Testing the normality of error term
shapiro.test(residuals(random_scaled))
#Non-normality of the error term

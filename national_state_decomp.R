#Libraries---------------------------------------

require(tidyverse)
library(maps)
library(xlsx)
library("readxl")
library(dplyr)
library(tidyr)
library(mclust)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(USAboundaries)
library(usmap)
library(sm)
library(gridExtra)
library(xtable)
library(gganimate)
library(gapminder)
library(directlabels)
library(AdaptGauss)
library(spdep)
library(ineq)
library(openintro)
library("GiniDistance")
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/National-State Decomposition/Data")

#Data formatting------------------------------------------------------
#Import and trim data

Comp <- read_excel("compensation less rrl.xlsx") #THOUSANDS of dollars
Empl <- read_excel("employment less rrl.xlsx")
Surplus <- read_excel("surplus less rrl.xlsx")#THOUSANDS of dollars
VA_Chained <- read_excel("va chained less rrl.xlsx")
VA_Current <- read_excel("va current less rrl.xlsx")




Comp <- subset( Comp, select = -c(x1963:x1976, 3:8))
Empl <- subset( Empl, select = -c(x1969:x1976, 3:8))
Surplus <- subset( Surplus, select = -c(x1972:x1976, 3:9))
VA_Chained <- subset( VA_Chained, select = -c(3:4))
VA_Current <- subset( VA_Current, select = -c(x1963:x1976, 3:4))

#Reshaping data
Comp_long <- gather(Comp, key = year, compensation, x1977:x2017, factor_key=FALSE)
Empl_long <- gather(Empl, key = year, employment, x1977:x2017, factor_key=FALSE)
Surplus_long <- gather(Surplus, key = year, Surplus, x1977:x2017, factor_key=FALSE)
VA_Chained_long <- gather(VA_Chained, key = year, va_chained, x1977:x2017, factor_key=FALSE)
VA_Current_long <- gather(VA_Current, key = year, va_current, x1977:x2017, factor_key=FALSE)
rm(Comp, Empl, Surplus, VA_Chained, VA_Current)



#Merging dataframes into single national dataframe
VA <- merge(VA_Chained_long, VA_Current_long, by=c("GeoName","year","GeoFIPS"))
Comp_empl <- merge(Comp_long, Empl_long, by=c("GeoName","year","GeoFIPS"))
VA_Surplus <- merge(VA, Surplus_long, by=c("GeoName","year","GeoFIPS"))
National <- merge(Comp_empl, VA_Surplus, by=c("GeoName","year","GeoFIPS"))
rm(Comp_empl, Comp_long, Empl_long, Surplus_long, VA, VA_Chained_long, VA_Current_long, VA_Surplus)


#Change Scale (Millions of dollars)
National$Surplus <- National$Surplus / 1000
National$compensation <- National$compensation / 1000

#National <- read_excel("national.xlsx")


#Create Variables -------------------------------------------------------------
calculate <-function(region)
{
  #create value added w/o taxes variable
  region$va_current_wo_taxes <- region$compensation + region$Surplus 
  
  #create price index variable
  region$price_index <- region$va_current/region$va_chained * 100
  print("here0")
  
  #create national private industry deflator
  middle <- region %>% dplyr::filter(str_detect(GeoName, pattern = "United States"))
  print("here")
  middle <- middle %>% dplyr::select(year, price_index, compensation, employment, va_current_wo_taxes) %>%
    dplyr::rename(national_price_index = price_index, national_comp = compensation, national_employment = employment, national_va_current_wo_taxes = va_current_wo_taxes)
  print("here2")
  
  region <- middle %>% left_join(region, by = "year")
  rm(middle)
  
  #WANT TO DELETE US ROW
  region <- subset(region, region$GeoName != "United States")
  
  #create va w/o taxes in state-specific chained dollars
  region$va_statechained_wo_taxes <- region$va_current_wo_taxes / region$price_index * 100
  
  
  #create compensation private chained dollars
  region$comp_nationalchained <- region$compensation / region$national_price_index * 100 
  
  
  #create wage in current dollars
  region$wage_current <- region$compensation / region$employment * 1000000
  
  #create wage bill share
  region$wage_bill_share <- region$compensation / region$national_comp
  
  #create wage rate in national (private) chained dollars (omega 1)
  region$wage_rate_nationalchained <- region$comp_nationalchained / region$employment * 1000000
  
  #create wage rate in state-specific chained dollars (omega 2)
  region$wage_rate_statechained <- region$wage_current / region$price_index * 100
  
  #create employment share
  region$empl_share <- region$employment / region$national_employment
  
  #create labor productivity
  region$labor_prod <- region$va_statechained_wo_taxes / region$employment * 1000000
  
  #create wage share (omega1/labor productivity)
  region$wage_share1 <- region$wage_rate_nationalchained / region$labor_prod
  
  #create wage share (omega2/labor productivity)
  region$wage_share2 <- region$wage_rate_statechained / region$labor_prod
  
  #create VA share
  region$va_share <- region$va_current_wo_taxes / region$national_va_current_wo_taxes
  
  #create price ratio 
  region$price_ratio <- region$price_index / region$national_price_index
  
  #cast year as a number
  region$year <- substring(region$year, 2)
  region$year <- as.numeric(region$year)
  return(region)
}

calc_region <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector of states
  region_list <- start$GeoName
  return(region_list)
}

calc_w_weight <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector w_weight
  w_weightv <- (start$wage_bill_share + end$wage_bill_share)/2
  return(w_weightv)
}

calc_va_weight <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  va_weightv <- (start$va_share + end$va_share)/2
  return(va_weightv)
}

calc_w_log <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  
  #create vector w_log
  w_logv <- (end$wage_bill_share - start$wage_bill_share)/log(end$wage_bill_share/start$wage_bill_share)
  return(w_logv)
  
}

calc_va_log <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_log
  va_logv <- (end$va_share - start$va_share)/log(end$va_share/start$va_share)
  return(va_logv)
  
}

calc_phi <- function(w_log)
{
  
  phiv <- w_log / sum(w_log[1:length(w_log)])
  return(phiv)
}

calc_theta <- function(va_log)
{
  thetav <- va_log / sum(va_log[1:length(va_log)])
  return(thetav)
}

calc_d_comp <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  
  d_compv <- log(end$wage_rate_statechained/start$wage_rate_statechained)*phi
  return(d_compv)
}

calc_d_str <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  
  d_strv <- log(end$empl_share/start$empl_share)*(phi-theta)
  return(d_strv)
}

calc_d_tech <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  
  d_techv <- log(end$labor_prod/start$labor_prod)*(theta)
  return(d_techv)
}

calc_d_price <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #different from industry decomp
  d_pricev <- log(end$price_ratio/start$price_ratio)*(phi - theta)
  return(d_pricev)
}

calc_d_index <- function(start_year, end_year,region)
{
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  d_indexv <- (d_comp*d_str*d_price)/(d_tech)
  #select last (hopefully national?)
  print(d_indexv)
  return(d_indexv)
}

calc_labor_prod_change <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  labor_prod_change <- (end$labor_prod - start$labor_prod)/start$labor_prod
  return(labor_prod_change)
}

calc_consumption_change_wage <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  consumption_change_wage <- (end$wage_rate_statechained - start$wage_rate_statechained)/start$wage_rate_statechained
  return(consumption_change_wage)
}

calc_labor_prod <- function(start_year, end_year, region)
{
  #get correct subset of data
  start <- region %>% dplyr::filter(year >= start_year & year <= end_year)
  
  #create vector of average labor prod
  #initialize a data frame with names
  avg <- data.frame(GeoName=region$GeoName[region$year == 1977])
  for(b in state)
  {
    avg$labor_prod[avg$GeoName == b] <- mean(start$labor_prod[start$GeoName == b])
  }
  return(avg)
}

calc_avg_va_share <- function(start_year, end_year, region)
{
  #get correct subset of data
  start <- region %>% dplyr::filter(year >= start_year & year <= end_year)
  
  #create vector of average labor prod
  #initialize a data frame with names
  avg <- data.frame(GeoName=region$GeoName[region$year == 1977])
  for(b in state)
  {
    avg$va_share[avg$GeoName == b] <- mean(start$va_share[start$GeoName == b])
  }
  return(avg)
}

calc_real_wage <- function(start_year, end_year, region)
{
  #get correct subset of data
  start <- region %>% dplyr::filter(year >= start_year & year <= end_year)

  #create vector of average real wage
  #initialize a data frame with names
  avg <- data.frame(GeoName=region$GeoName[region$year == 1977])
  for(b in state)
  {
    avg$real_wage[avg$GeoName == b] <- mean(start$wage_rate_statechained[start$GeoName == b])
  }
  return(avg)
}

calc_avg_wage_share2 <- function(start_year, end_year, region)
{
  #get correct subset of data
  start <- region %>% dplyr::filter(year >= start_year & year <= end_year)
  
  #create vector of average real wage
  #initialize a data frame with names
  avg <- data.frame(GeoName=region$GeoName[region$year == 1977])
  for(b in state)
  {
    avg$avg_wage_share2[avg$GeoName == b] <- mean(start$wage_share2[start$GeoName == b])
  }
  return(avg)
}


calc_empl <- function(start_year, end_year, region)
{
  #get correct subset of data
  start <- region %>% dplyr::filter(year >= start_year & year <= end_year)
  
  #create vector of average employment shares
  avg <- data.frame(GeoName=region$GeoName[region$year == 1977])
  for(b in state)
  {
    avg$empl_share[avg$GeoName == b] <- mean(start$empl_share[start$GeoName == b])
  }
  return(avg)
}

calc_empl_change <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  empl_change <- (end$empl_share - start$empl_share)
  return(empl_change)
}

calc_va_change <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  va_change <- (end$va_share - start$va_share)
  return(va_change)
}

calc_wage_share2_change_pp <- function(start_year, end_year, region)
{
  #get start and end data
  start <- region %>% dplyr::filter(year == start_year)
  end <- region %>% dplyr::filter(year == end_year)
  
  #create vector va_weight
  wage_share2_change <- (end$national_comp/end$national_va_current_wo_taxes) - (start$national_comp/start$national_va_current_wo_taxes)
  return(wage_share2_change)
}

#Divisia Decomposition ----------------------------------------------------------------------
contributions <- function()
{
  total_d_comp <- exp(sum(Results$d_comp))
  total_d_str <- exp(sum(Results$d_str))
  total_d_price <- exp(sum(Results$d_price))
  total_d_tech <- exp(sum(Results$d_tech))
  
  divisia_log_index <<- (total_d_comp*total_d_str*total_d_price)/total_d_tech
  print(divisia_log_index)
  
  #aggregate level contributions of components
  total_comp_contribution <<- log(total_d_comp)/log(divisia_log_index)*(divisia_log_index-1)*100
  total_str_contribution <<- log(total_d_str)/log(divisia_log_index)*(divisia_log_index-1)*100
  total_price_contribution <<- log(total_d_price)/log(divisia_log_index)*(divisia_log_index-1)*100
  total_tech_contribution <<- -1*log(total_d_tech)/log(divisia_log_index)*(divisia_log_index-1)*100
  total_change <<- total_comp_contribution + total_price_contribution + total_str_contribution + total_tech_contribution
  
  #state level contributions(% change)
  Results$comp_contribution <<- Results$d_comp/log(total_d_comp)*total_comp_contribution
  Results$ str_contribution <<- Results$d_str/log(total_d_str)*total_str_contribution
  Results$tech_contribution <<- Results$d_tech/log(total_d_tech)*total_tech_contribution
  Results$price_contribution <<- Results$d_price/log(total_d_price)*total_price_contribution
  Results$total_contribution <<- Results$comp_contribution +  Results$str_contribution + Results$tech_contribution + Results$price_contribution 
  
  #state level contributions(pp change)
  Results$comp_contribution_pp <<- Results$comp_contribution*(Results$wage_share2_pp_change)/(divisia_log_index-1)
  Results$str_contribution_pp <<- Results$str_contribution*(Results$wage_share2_pp_change)/(divisia_log_index-1)
  Results$tech_contribution_pp <<- Results$tech_contribution*(Results$wage_share2_pp_change)/(divisia_log_index-1)
  Results$price_contribution_pp <<- Results$price_contribution*(Results$wage_share2_pp_change)/(divisia_log_index-1)
  Results$total_contribution_pp <<- Results$total_contribution*(Results$wage_share2_pp_change)/(divisia_log_index-1)
  
}

divisia <- function(start_year, end_year, region)
{
  #create region list
  state <<- calc_region(start_year, end_year, region)
  
  #create vector w_weight
  w_weight <<- calc_w_weight(start_year, end_year, region)
  
  #create vector w_log
  w_log <<- calc_w_log(start_year, end_year, region)
  
  #create vector phi
  phi <<- calc_phi(w_log)
  
  #create vector va_weight
  va_weight <<- calc_va_weight(start_year, end_year, region)
  
  #create vector va_log
  va_log <<- calc_va_log(start_year, end_year, region)
  
  #create vector theta
  theta <<- calc_theta(va_log)
  
  #create d_comp vector
  d_comp <<- calc_d_comp(start_year, end_year, region)
  
  #create d_str vector
  d_str <<- calc_d_str(start_year, end_year, region)
  
  #create d_tech vector
  d_tech <<- calc_d_tech(start_year, end_year, region)
  
  #create d_price vector
  d_price <<- calc_d_price(start_year, end_year, region)
  
  labor_prod_change <<- calc_labor_prod_change(start_year, end_year, region)
  
  real_wage_change <<- calc_consumption_change_wage(start_year, end_year, region)
  
  empl_change <<- calc_empl_change(start_year, end_year, region)
  avg_labor_prod <<-calc_labor_prod(start_year, end_year, region)
  avg_empl <<- calc_empl(start_year, end_year, region)
  avg_real_wage <<- calc_real_wage(start_year, end_year, region)
  avg_wage_share2 <<- calc_avg_wage_share2(start_year, end_year, region)
  va_change <<- calc_va_change(start_year, end_year, region)
  avg_va_share <<- calc_avg_va_share(start_year, end_year, region)
  wage_share2_pp_change <<- calc_wage_share2_change_pp(start_year, end_year, region)
  Results <<- data.frame(state, d_comp, d_price, d_str, d_tech, phi, theta, va_log, va_weight, w_log, w_weight, labor_prod_change, empl_change,real_wage_change, avg_labor_prod$labor_prod, avg_empl$empl_share, avg_real_wage$real_wage, avg_wage_share2, va_change, wage_share2_pp_change, avg_va_share)
  rm(empl_change, avg_labor_prod, avg_empl, avg_real_wage, avg_wage_share2, va_change, avg_va_share, envir=globalenv())
  contributions()
}
#Main----------------------------------------------------------------------------------
National <- calculate(National)

divisia(1977, 2017, National)
cycle_77_17<-Results

divisia(1979, 1989, National)
cycle_79_89 <-Results
divisia(1989, 2000, National)
cycle_89_00 <-Results
divisia(2000, 2007, National)
cycle_00_07 <-Results
divisia(2007, 2017, National)
cycle_07_17 <-Results

divisia(1977, 2000, National)
cycle_77_00 <-Results

divisia(2000, 2017, National)
cycle_00_17 <-Results

#Graphs----------------------------------------------------------------------
#productivity histogram
hist(cycle_89_00$labor_prod_change, breaks = 15)+
  abline(v=median(cycle_89_00$labor_prod_change))

#graph of productivity elasticity of employment share (DESIGNED FOR EMPL_CHANGE IN %, NOT % POINT!!)
f1<-ggplot(cycle_79_89, aes(y=cycle_79_89$empl_change, x=cycle_79_89$labor_prod_change, size=cycle_79_89$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.2, 0.6))+
  scale_y_continuous(limits = c(-0.2, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab(NULL)+
  ylab("Empl. share change (%)")

f2<-ggplot(cycle_89_00, aes(y=cycle_89_00$empl_change, x=cycle_89_00$labor_prod_change, size=cycle_89_00$avg_empl) )+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.2, 0.6))+
  scale_y_continuous(limits = c(-0.2, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab(NULL)+
  ylab(NULL)

f3<-ggplot(cycle_00_07, aes(y=cycle_00_07$empl_change, x=cycle_00_07$labor_prod_change, size=cycle_00_07$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.2, 0.6))+
  scale_y_continuous(limits = c(-0.2, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab("Labor prod. change (%)")+
  ylab("Empl. share change (%)")

f4<-ggplot(cycle_07_17, aes(y=cycle_07_17$empl_change, x=cycle_07_17$labor_prod_change, size=cycle_07_17$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.2, 0.6))+
  scale_y_continuous(limits = c(-0.2, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  theme(legend.position=c(0.9,0.4))+
  xlab("Labor prod. change (%)")+
  ylab(NULL)

grid.arrange(f1, f2, f3, f4, nrow = 2, ncol = 2)

#graph of wage elasticity of employment share (DESIGNED FOR EMPL_CHANGE IN %, NOT % POINT!!)
g1<-ggplot(cycle_79_89, aes(y=cycle_79_89$empl_change, x=cycle_79_89$real_wage_change, size=cycle_79_89$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.3, 0.6))+
  scale_y_continuous(limits = c(-0.3, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab(NULL)+
  ylab("Empl. share change (%)")

g2<-ggplot(cycle_89_00, aes(y=cycle_89_00$empl_change, x=cycle_89_00$real_wage_change, size=cycle_89_00$avg_empl) )+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.3, 0.6))+
  scale_y_continuous(limits = c(-0.3, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab(NULL)+
  ylab(NULL)

g3<-ggplot(cycle_00_07, aes(y=cycle_00_07$empl_change, x=cycle_00_07$real_wage_change, size=cycle_00_07$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.3, 0.6))+
  scale_y_continuous(limits = c(-0.3, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab("Real wage change (%)")+
  ylab("Empl. share change (%)")

g4<-ggplot(cycle_07_17, aes(y=cycle_07_17$empl_change, x=cycle_07_17$real_wage_change, size=cycle_07_17$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.3, 0.6))+
  scale_y_continuous(limits = c(-0.3, 0.6))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  theme(legend.position=c(0.9,0.4))+
  xlab("Real wage change (%)")+
  ylab(NULL)

grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)

#labor productivity change vs real wage change by cycle graphh
p1<-ggplot(cycle_79_89, aes(x=cycle_79_89$labor_prod_change, y=cycle_79_89$real_wage_change, size=cycle_79_89$avg_empl))+
  geom_point(alpha=0.2)+geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
  scale_x_continuous(limits = c(-0.1, 0.3))+
  scale_y_continuous(limits = c(-0.1, 0.3))+
  scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
  xlab(NULL)+
  ylab("Real wage change (%)")

p2<-ggplot(cycle_89_00, aes(x=cycle_89_00$labor_prod_change, y=cycle_89_00$real_wage_change, size=cycle_89_00$avg_empl) )+
    geom_point(alpha=0.2)+geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
    scale_x_continuous(limits = c(-0.1, 0.3))+
    scale_y_continuous(limits = c(-0.1, 0.3))+
    scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
    xlab(NULL)+
    ylab(NULL)

p3<-ggplot(cycle_00_07, aes(x=cycle_00_07$labor_prod_change, y=cycle_00_07$real_wage_change, size=cycle_00_07$avg_empl))+
    geom_point(alpha=0.2)+geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
    scale_x_continuous(limits = c(-0.1, 0.3))+
    scale_y_continuous(limits = c(-0.1, 0.3))+
    scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
    xlab("Labor prod. change (%)")+
    ylab("Real wage change (%)")

p4<-ggplot(cycle_07_17, aes(x=cycle_07_17$labor_prod_change, y=cycle_07_17$real_wage_change, size=cycle_07_17$avg_empl))+
    geom_point(alpha=0.2)+geom_hline(yintercept=0)+
    geom_vline(xintercept=0)+geom_abline(intercept=, slope=1, linetype="dashed")+
    scale_x_continuous(limits = c(-0.1, 0.3))+
    scale_y_continuous(limits = c(-0.1, 0.3))+
    scale_size(name = "Empl share", breaks = c(0.03,0.06,0.09), guide = FALSE)+
    theme(legend.position=c(0.9,0.4))+
    xlab("Labor prod. change (%)")+
    ylab(NULL)

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

#Alligator graph
plot(National$year[National$GeoName=="Utah"],
     (National$labor_prod[National$GeoName=="Utah"]),
     type = 'l', ylim = c(30000,90000),
     xlab = "Year", ylab = "($)", col = "red",   
     panel.first = c(rect(1980,0.6,1980.5,0.68, col="azure2"),
                                                                
         rect(1981.5,0,1982.9,100000, col="azure2"),
        rect(1990.5,0,1991.2,100000, col="azure2"),
        rect(2001.2,0,2001.9,100000, col="azure2"),
        rect(2007.95,0,2009.5,100000, col="azure2")))
points(National$year[National$GeoName=="Utah"], 
       National$wage_rate_statechained[National$GeoName=="Utah"], 
       type='l', col = "blue")
legend("bottomright", legend=c("Labor Prod.", "Real Wage"),
       lty=1,col=c("red", "blue"), cex=0.8)



#####Bar Graphs ####


Results$regions <- "blah"
Results$regions[Results$state == "Ohio" |
                        Results$state == "Michigan" |
                        Results$state == "Indiana" |
                        Results$state == "Illinois" |
                        Results$state == "Wisconsin"
                      ] <- "Midwest"

Results$regions[Results$state == "Maine" |
                        Results$state == "Vermont" |
                        Results$state == "New Hampshire" |
                        Results$state == "Massachusetts" |
                        Results$state == "Connecticut"|
                        Results$state == "Rhode Island"|
                        Results$state == "Delaware" |
                        Results$state == "Maryland" |
                        Results$state == "New Jersey" |
                        Results$state == "New York" |
                        Results$state == "Pennsylvania"|
                        Results$state == "Rhode Island"
                      
                      ] <- "Northeast"

Results$regions[Results$state == "West Virginia" |
                        Results$state == "Virginia" |
                        Results$state == "North Carolina" |
                        Results$state == "South Carolina" |
                        Results$state == "Kentucky"|
                        Results$state == "Kentucky"|
                        Results$state == "Tennessee"|
                        Results$state == "Georgia"|
                        Results$state == "Florida"|
                        Results$state == "Louisiana"|
                        Results$state == "Alabama"|
                        Results$state == "Mississippi"|
                        Results$state == "Arkansas"|
                        Results$state == "Texas"
                      ] <- "Southeast"

Results$regions[Results$state == "Iowa" |
                        Results$state == "North Dakota" |
                        Results$state == "South Dakota" |
                        Results$state == "Missouri" |
                        Results$state == "Minnesota"|
                        Results$state == "Kansas"|
                        Results$state == "Oklahoma" |
                        Results$state == "Nebraska"
                      ] <- "Central"

Results$regions[Results$state == "Wyoming" |
                        Results$state == "Colorado" |
                        Results$state == "Idaho" |
                        Results$state == "Montana" |
                        Results$state == "Utah" |
                        Results$state == "Nevada"|
                        Results$state == "Arizona"|
                        Results$state == "New Mexico"
                      ] <- "West"

Results$regions[Results$state == "California" |
                        Results$state == "Oregon" |
                        Results$state == "Washington"|
                        Results$state == "Alaska" |
                        Results$state == "Hawaii"
                      ] <- "Pacific"

#wage share change bar plot:
Results$positive <- 0
Results$positive[Results$wage_share2_pp_change>0] <- 1

Results$state_val <-as.factor(Results$state)
Results$state_num <- as.numeric(Results$state_val)

ggplot(subset(Results,industries == "PRI" & state != "District of Columbia" ),
       aes(x=reorder(state, -state_num), y=wage_share2_pp_change, fill = positive))+
  geom_bar(stat = "identity")+
  # theme(legend.position = "none")+
  scale_x_discrete(breaks = Results$state, labels = Results$state)+
  facet_grid(rows=vars(regions), space="free", scales="free", switch="x") + 
  scale_fill_gradient2(low = "red", 
                       mid = "white",
                       midpoint = 0.5,
                       high = "blue",
                       name = expression(paste(Delta,psi)))+
  coord_flip()+
  geom_hline(yintercept = 0)+
  ylab("% Point")+
  xlab("")+
  ylim(-0.15,0.08)+
  guides(fill=guide_legend(title="Industry")) +
  theme( #axis.text.y=element_blank(),
    #axis.text.x=element_blank(),
    #axis.title=element_blank(),
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_rect(fill=NA,colour="grey50"),
    #panel.spacing=unit(0,"cm")
  )




#Maps --------------------------------------------------------------------------------
plot_usmap(data = subset(National, year == 2017), values = "wage_share2",regions = "states") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), 
        legend.position = c(0.86,0)
        ) +
  scale_fill_gradient(low = "white", 
                       high = "blue", name = "Payroll Share")
  #theme(legend.position = "none")

fit<-National %>%
  filter(year == 2017 & state != "Alaska" & state != "Louisiana" & state != "Wyoming" & state != "District of Columbia") %>%
  select(starts_with("labor_prod")) %>%
  Mclust()

Nat_sub<-National %>%
  filter(year == 2017 & state != "Alaska" & state != "Louisiana" & state != "Wyoming" & state != "District of Columbia") %>%
  mutate(classification=as.factor(fit$classification))

plot_usmap(data = Nat_sub, values = "classification", color = "#fdf1e5") + 
  #scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")+
  theme(plot.background = element_rect(color = NA, fill = "#fdf1e5"))


plot_usmap(data = contraction_79_83, values = "price_contribution", regions = "states") + 
  theme(panel.background = element_rect(colour = "black",
        fill = "lightblue"),
        legend.position = "none") +
  scale_fill_gradient2(low = "red", mid = "white",
                       high = "blue",
                       midpoint = 0,
                       limits = c(-0.1,0.1),
                       name = "Change (% Points)") 

#NEED TO REGENERATE FOR ALL CYCLES WITH CONSISTENT SCALE!!!
#Map for component contributions over whole period:
plot_usmap(data =cycle_77_17, values = "total_contribution_pp", regions = "states") + 
  theme(panel.background = element_rect(colour = "black", 
                                        fill = "lightblue"),
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=13.5)) +
  scale_fill_gradient(low = "black",
                      # mid = "white",
                       high = "white",
                      # midpoint = 0, 
                      # limits = c(-1,1),
                       breaks = c(0,-0.25,-0.5),
                       name = "Change \n(% Points)") +
  theme(legend.position = c(0.855,0))
  theme(legend.position = "none")

#map for change in labor produtivity (%) and wage (%)
plot_usmap(data =cycle_77_17, values = "state_wage_share_change", regions = "states") + 
  theme(panel.background = element_rect(colour = "black", 
                                        fill = "lightblue")) +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0, 
                    #  breaks = c(-1, 0, 1.5),
                     # labels = c(-1, 0, 1.5),
                       name = "Change (% Points)") +
  theme(legend.position = c(0.855,0.1))
  #theme(legend.position = "none")

#Map for total contributions of each period, change in shares: (DESIGNED FOR EMPL_CHANGE/VA_CHANGE IN % POINTS, NOT %!!)
plot_usmap(data =x, values = "delta_psi", regions = "states") + 
  theme(panel.background = element_rect(colour = "black", 
                                        fill = "lightblue")) +
  scale_fill_gradient(low = "black",
                       #mid = "white",
                       high = "white",
                      # midpoint = 0, 
                     # limits = c(-0.02,0.02),
                        labels = c(-15,-10, -5, 0, 5, 10),
                        breaks = c(-0.15,-0.1, -0.05, 0, 0.05, 0.1),
                       name = "Change (% points)") +
  theme(legend.position = c(0.87,0))
  theme(legend.position = "none")

National <- National %>%
  group_by(year) %>%
  mutate(relative_labor_prod=labor_prod/mean(labor_prod)) %>%
  ungroup()

National <- National %>%
  group_by(year) %>%
  mutate(relative_wage=wage_rate_statechained/mean(wage_rate_statechained)) %>%
  ungroup()

National$state<-National$GeoName

#Levels map (wage, productivity, wage share)
plot_usmap(data = subset(National, year==2017 & state != "District of Columbia" & state != "Alaska"),
           values = "relative_labor_prod", regions = "states") + 
  theme(panel.background = element_rect(colour = "black",
                                        fill = "lightblue"), 
        legend.position = c(0.86,0),
        legend.text = element_text(size=13.5),
        legend.title = element_text(size=13.5)
  ) +
  scale_fill_gradient(low = "black",
                       breaks= c(0.8, 0.9, 1, 1.1, 1.2, 1.3),
                      #limits = c(60000,140000),
                    #  mid = "white",
                     # midpoint =1,
                       high = "white",
                       name = "% Nat. Avg.",
                      labels = c(80,90, 100, 110, 120, 130)
                       )


#Productivity distrubution
plot1 = plot(density(National$wage_rate_statechained[National$year == 2016 & National$GeoName %in% progressive]), lty = 3,
             xlab= "Percent of National Average",
             main = "",
             ylim = c(0,0.00008), xlim = c(10000,80000)) +
  lines(density(National$wage_rate_statechained[National$year == 2016 & National$GeoName %in% middle]), lty = 2)+
  lines(density(National$wage_rate_statechained[National$year == 2016 & National$GeoName %in% stagnant]), lty = 1)
  legend(60000,y=0.00005, legend = c('Progressive','Middle','Stagnant'), lty = c(3,2,1))
  
  
plot_prod_rate = plot(density(labor_prod_data$compound_growth[labor_prod_data$GeoName %in% progressive]), lty = 1,
               xlab= "Compound growth rate",
               main = "",
               ylim = c(0,450), xlim = c(-0.001,0.025)) +
    lines(density(labor_prod_data$compound_growth[labor_prod_data$GeoName %in% middle]), lty = 2)+
    lines(density(labor_prod_data$compound_growth[labor_prod_data$GeoName %in% stagnant]), lty = 3)
  legend(0.021,y=200, legend = c('Progressive','Middle','Stagnant'),
         lty = c(1,2,3),
         cex = 0.75)
  

#wage distrubution
plot2 = plot(density((National$wage_rate_statechained[National$GeoName != "District of Columbia"  & National$GeoName != "Alaska"]/mean(National$wage_rate_statechained[National$GeoName != "District of Columbia" & National$GeoName != "Alaska" & National$GeoName!="California" & National$GeoName!="Texas" & National$GeoName!="New York" & National$GeoName!="Florida" & National$year==1977]))), lty = 3,
             xlab= "Percent of National Average",
             main = "",
             #ylim = c(0,0.000070),
             xlim=c(0.5,1.6)) +
  #lines(density(National$avg_real_wage.real_wage/mean(National$avg_real_wage.real_wage)), lty = 2)+
  #lines(density(National$avg_real_wage.real_wage/mean(National$avg_real_wage.real_wage)), lty = 1)+
  lines(density((National$wage_rate_statechained[National$GeoName != "District of Columbia"  & National$GeoName != "Alaska"]/mean(National$wage_rate_statechained[National$GeoName != "District of Columbia" & National$GeoName != "Alaska" & (National$GeoName=="California" | National$GeoName=="Texas" | National$GeoName=="New York" | National$GeoName=="Florida") & National$year==1977]))), lty = 1)
  legend("topright", legend = c('Real Wage','Productivity'), lty = c(3,1))

#wage and productivity variance graphs (Sigma convergecnce)-------------
lab_variance <- data.frame("year"=c(1977:2017), "variance" =c(1977:2017))
for(a in c(1977:2017))
{
  lab_variance$variance[lab_variance$year == a] <- sd(log(National$labor_prod[National$year==a & National$GeoName != "Alaska" & National$GeoName != "District of Columbia"]))
}

wage_variance <- data.frame("year"=c(1977:2017), "variance" =c(1977:2017))
for(b in c(1977:2017))
{
  wage_variance$variance[wage_variance$year == b] <- sd(log(National$wage_rate_statechained[National$year==b & National$GeoName != "District of Columbia" &  National$GeoName != "Alaska"]))
}


share_variance <- data.frame("year"=c(1977:2017), "variance" =c(1977:2017), "mean" =c(1977:2017))
for(b in c(1977:2017))
{
  share_variance$variance[share_variance$year == b] <- sd((National$wage_share2[National$year==b & National$GeoName != "District of Columbia" &  National$GeoName != "Alaska"]))
  share_variance$mean[share_variance$year == b] <- mean((National$wage_share2[National$year==b & National$GeoName != "District of Columbia" &  National$GeoName != "Alaska"]))
  
}

plot(wage_variance$year, lab_variance$variance, xlab = "Year", ylab=expression(paste(sigma," ln(", xi, ")")))+abline(lm(lab_variance$variance~lab_variance$year), col="black")
plot(wage_variance$year, wage_variance$variance, xlab = "Year", ylab=expression(paste(sigma," ln(", omega, ")")))+abline(lm(wage_variance$variance~wage_variance$year), col="black")
plot(share_variance$year, share_variance$variance, xlab = "Year", ylab=expression(paste(sigma,"(", Psi, ")")))+abline(lm(share_variance$variance~share_variance$year), col="black")
plot(share_variance$year, share_variance$mean, xlab = "Year", ylab=expression(paste("mean(", Psi, ")")))+abline(lm(share_variance$mean~share_variance$year), col="black")

#wage share time series graph--------------
BLS <- read.csv("bls_shares.csv") 
Nat <- subset(National, GeoName == "Alabama")
Nat$all_share <- BLS$All.worker.labor.share
Nat$employee_share <- BLS$Employee.only.labor.share/100
Nat$prop_income <- BLS$Proprietors..total.income.share/100
Nat$national_payroll_share<-BLS$Payroll.share/100
Nat$all_share<-Nat$all_share/100

plot(Nat$year, Nat$national_comp/(Nat$national_va_current_wo_taxes), 
     type="o",
     xlab="Year",
     ylab="Payroll Share",
     panel.first = c(rect(1980,0.6,1980.5,0.68, col="azure2"),
                     rect(1981.5,0.6,1982.9,0.68, col="azure2"),
                     rect(1990.5,0.6,1991.2,0.68, col="azure2"),
                     rect(2001.2,0.6,2001.9,0.68, col="azure2"),
                     rect(2007.95,0.6,2009.5,0.68, col="azure2")))
  abline(lm(Nat$national_comp/(Nat$national_va_current_wo_taxes)~Nat$year),lty=2)
 # text(2014,0.65,"Y=3.4-0.0019*X")+
#  text(2014,0.645,"(0.00)***")
  
G<-ggplot(Nat, aes(year)) + 
  geom_line(data=National, aes(x=year, y=wage_share2, group=GeoName), col="grey")+
    geom_line(aes(y = employee_share, linetype = "Employee Share"),size=1, alpha=1)+
    geom_line(aes(y = all_share, linetype = "Labor Share"),size=1,  alpha=1) + 
    geom_line(aes(y = national_comp/(national_va_current_wo_taxes), linetype = "Payroll Share"),size=1,  alpha=1)+
   # geom_rect(xmin=1980, xmax = 1980.5, ymin = 0, ymax = 70, colour = "azure2", alpha = 0.01)+ 
   # geom_rect(xmin=1981.5, xmax = 1982.9, ymin = 0, ymax = 70, colour = "azure2", alpha = 0.01)+ 
  #  geom_rect(xmin=1990.5, xmax = 1991.2, ymin = 0, ymax = 70, colour = "azure2", alpha = 0.01)+ 
   # geom_rect(xmin=2001.2, xmax = 2001.9, ymin = 0, ymax = 70, colour = "azure2", alpha = 0.01)+ 
  #  geom_rect(xmin=2007.95, xmax = 2009.5, ymin = 0, ymax = 70, colour = "azure2", alpha = 0.01)+
    theme(legend.position = c(0.9,0.1), legend.title = element_blank(),
          panel.background = element_rect(fill = 'white', colour="black"),
          panel.grid.major = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid",
                                   colour = "black"))+
    labs(x="Year", y="")+
   # guides(colour = guide_legend(reverse = TRUE))+
    scale_linetype_manual("Variabler",values=c("Employee Share"=3,"Labor Share"=4, "Payroll Share"=1))
 

plot(G)

  
#MULTIVARIATE mixture model##########

#create log values
cycle_77_17$log_real_wage <- log(cycle_77_17$avg_real_wage.real_wage)
cycle_79_89$log_real_wage <- log(cycle_79_89$avg_real_wage.real_wage)
cycle_89_00$log_real_wage <- log(cycle_89_00$avg_real_wage.real_wage)
cycle_00_07$log_real_wage <- log(cycle_00_07$avg_real_wage.real_wage)
cycle_07_17$log_real_wage <- log(cycle_07_17$avg_real_wage.real_wage)

cycle_77_17$log_prod <- log(cycle_77_17$avg_labor_prod.labor_prod)
cycle_79_89$log_prod <- log(cycle_79_89$avg_labor_prod.labor_prod)
cycle_89_00$log_prod <- log(cycle_89_00$avg_labor_prod.labor_prod)
cycle_00_07$log_prod <- log(cycle_00_07$avg_labor_prod.labor_prod)
cycle_07_17$log_prod <- log(cycle_07_17$avg_labor_prod.labor_prod)

cycle_77_17$log_share2 <- log(cycle_77_17$avg_wage_share2)
cycle_79_89$log_share2 <- log(cycle_79_89$avg_wage_share2)
cycle_89_00$log_share2 <- log(cycle_89_00$avg_wage_share2)
cycle_00_07$log_share2 <- log(cycle_00_07$avg_wage_share2)
cycle_07_17$log_share2 <- log(cycle_07_17$avg_wage_share2)


#create list of big contributors:
big_boys <- c('New York', 'California', 'Pennsylvania', 'Texas', 'Ohio', 'Michigan', 'Illinois', 'Indiana', 'North Carolina', 'Georgia', 'Connecticut', 'New Jersey', 'Washington', 'Minnesota', 'Arizona', 'Kentucky', 'Virginia', 'Louisiana', 'Florida')


#estimate mixed model for productivity and wage share by
#cycles:
Cyc <- subset(cycle_07_17, state != "District of Columbia" & state != "Alaska" & state != "Louisiana" & state != "Wyoming")
Cyc$abbv<-state2abbr(Cyc$state)
Cyc$prod_relative <- Cyc$avg_labor_prod.labor_prod/mean(Cyc$avg_labor_prod.labor_prod)
Cyc$prod_relative_change <- Cyc$labor_prod_change/mean(Cyc$labor_prod_change)
fit<-Mclust(Cyc[,c(19,38)])
fit$classification[fit$classification==1]<-3
fit$classification[fit$classification==2]<-1
fit$classification[fit$classification==3]<-2
Cyc$classification <- fit$classification
plot(fit, what = "uncertainty",addEllipses = TRUE, xlab = "Avg. payroll share", ylab = "Avg. relative productivity", ylim = c(0.8, 1.45), xlim= c(0.44,0.8))
text(Cyc$avg_wage_share2[as.character(Cyc$state) %in% big_boys], Cyc$prod_relative[as.character(Cyc$state) %in% big_boys], Cyc$abbv[as.character(Cyc$state) %in% big_boys], cex=1, pos=4, col="black", size=20)

points(Cyc$avg_wage_share2[Cyc$classification == 1], Cyc$prod_relative[Cyc$classification == 1], col = "black")
points(Cyc$avg_wage_share2[Cyc$classification == 2], Cyc$prod_relative[Cyc$classification == 2], col = "black")

legend("topright", "I", bty="o", bg = "grey")
legend("topleft", "II", bty="o", bg = "grey")
legend("bottomleft", "III", bty="o", bg = "grey")
legend("bottomright", "IV", bty="o", bg = "grey")
abline(h=mean(Cyc$prod_relative))
#abline(lm(Cyc$prod_relative[Cyc$classification == 1]~Cyc$avg_wage_share2[Cyc$classification == 1]), col="blue") # regression line (y~x)
#abline(lm(Cyc$prod_relative[Cyc$classification == 2]~Cyc$avg_wage_share2[Cyc$classification == 2]), col="red") # regression line (y~x)
abline(v=mean(cycle_07_17$avg_wage_share2[cycle_07_17$state != "Alaska" & cycle_07_17$state != "District of Columbia"]))
abline(v=mean(cycle_77_17$avg_wage_share2[cycle_77_17$state != "Alaska" & cycle_77_17$state != "District of Columbia"]), col="black", lty=3)
#segments(mean(cycle_77_17$avg_wage_share2[cycle_77_17$state != "Alaska" & cycle_77_17$state != "District of Columbia"]), 0 ,mean(cycle_77_17$avg_wage_share2[cycle_77_17$state != "Alaska" & cycle_77_17$state != "District of Columbia"]), 0.77, lwd=3, col="red")


#plot BIC graph
plot(fit, what="BIC")



plot_usmap(data = subset(Cyc), include = Cyc$states[Cyc$classification == 2], values = "classification", regions = "states") + 
  theme(panel.background = element_rect(colour = "white", fill = "white"), 
        legend.position = "none"
  ) +
  scale_fill_gradient(low = "white", 
                       high = "black",
                       name = "Payroll Share"
  )
theme(legend.position = "none")
rm(Cyc)


#estimate mixed model for productivity growth and wage share by
#cycles:
Cyc <- subset(cycle_89_00, state != "District of Columbia" & state != "Alaska" & state != "Louisiana" & state != "Wyoming")
Cyc$prod_relative_change <- Cyc$labor_prod_change/mean(Cyc$labor_prod_change)
fit<-Mclust(Cyc[,c(19,37)])
Cyc$classification <- fit$classification
plot(fit, what = "classification",addEllipses = TRUE, xlab = "Avg. payroll share", ylab = "Avg. relative productivity growth", ylim = c(0.6, 1.5), xlim= c(0.44,0.8))
text(Cyc$avg_wage_share2[Cyc$state == "California" | Cyc$state == "Florida" | Cyc$state == "Michigan" | Cyc$state == "New York" | Cyc$state == "Ohio" | Cyc$state == "Pennsylvania" | Cyc$state == "Texas" | Cyc$state == "Virginia"], Cyc$prod_relative_change[Cyc$state == "California" | Cyc$state == "Florida" | Cyc$state == "Michigan" | Cyc$state == "New York" | Cyc$state == "Ohio" | Cyc$state == "Pennsylvania" | Cyc$state == "Texas" | Cyc$state == "Virginia"], Cyc$state[Cyc$state == "California" | Cyc$state == "Florida" | Cyc$state == "Michigan" | Cyc$state == "New York" | Cyc$state == "Ohio" | Cyc$state == "Pennsylvania" | Cyc$state == "Texas" | Cyc$state == "Virginia"], cex=0.6, pos=4, col="black")
abline(h=mean(Cyc$prod_relative))
abline(v=mean(Cyc$avg_wage_share2))
rm(Cyc)

#plot BIC graph
plot(fit, what="BIC")


#estimate mixed model for relative productivity and real wage by
#cycles:
Cyc <- subset(cycle_07_17, state != "District of Columbia" & state != "Alaska" & state != "Louisiana" & state != "Wyoming")
Cyc$prod_relative <- Cyc$avg_labor_prod.labor_prod/mean(Cyc$avg_labor_prod.labor_prod)
Cyc$wage_relative <- Cyc$avg_real_wage.real_wage/mean(Cyc$avg_real_wage.real_wage)
fit<-Mclust(Cyc[,c(36,35)])
Cyc$classification <- fit$classification
plot(fit, what = "classification",addEllipses = TRUE, xlab = "Avg. relative wage", ylab = "Avg. relative productivity", ylim = c(0.6, 1.5), xlim = c(0.6, 1.5))
text(Cyc$wage_relative, Cyc$prod_relative, Cyc$state, cex=0.6, pos=4, col="black")
abline(h=mean(Cyc$prod_relative))
abline(v=1, h=1)
rm(Cyc)


#mixture model in growth rates of prod, wage
Cyc <- subset(cycle_07_17, state != "District of Columbia" & state != "Alaska" & state != "Louisiana" & state != "Wyoming")
fit<-Mclust(Cyc[,c(14,12)])
plot(fit, what = "classification",addEllipses = FALSE, xlab = "Delta real wage", ylab = "Delta productivity")
text(Cyc$real_wage_change, Cyc$labor_prod_change, Cyc$state, cex=0.6, pos=4, col="black")
abline(a=0, b=1)
rm(Cyc)

#changes over time
cycle_79_89$cycle <- 1
cycle_89_00$cycle <- 2
cycle_00_07$cycle <- 3
cycle_07_17$cycle <- 4

cycle_79_89$prod_relative <- cycle_79_89$avg_labor_prod.labor_prod/mean(cycle_79_89$avg_labor_prod.labor_prod)
cycle_89_00$prod_relative <- cycle_89_00$avg_labor_prod.labor_prod/mean(cycle_89_00$avg_labor_prod.labor_prod)
cycle_00_07$prod_relative <- cycle_00_07$avg_labor_prod.labor_prod/mean(cycle_00_07$avg_labor_prod.labor_prod)
cycle_07_17$prod_relative <- cycle_07_17$avg_labor_prod.labor_prod/mean(cycle_07_17$avg_labor_prod.labor_prod)



#estimate mixed model for productivity in logs and growth rates, create progressive-stagnant classification (1=stagnant; 2=progressive)
fit<-Mclust(cycle_89_00[-c(2,9),c(27)])
plot(fit, what = "classification")
cycle_89_00$classification[cycle_89_00$state != "Alaska" &cycle_89_00$state != "District of Columbia"] <- fit$classification

plot(density(cycle_00_07$log_prod[cycle_00_07$classification == 1 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]), main="",xlim =c(10.95,11.85), xlab = "Log of average productivity (2000-2007)")
abline(v=mean(cycle_00_07$log_prod[cycle_00_07$classification == 1 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]),col="red")
lines(density(cycle_00_07$log_prod[cycle_00_07$classification == 2 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]))
abline(v=mean(cycle_00_07$log_prod[cycle_00_07$classification == 2 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]),col="blue")
abline(v=mean(cycle_00_07$log_prod[cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]))

#UNIVARITE mixture model###########

fit<-Mclust(cycle_77_17[-c(2,9),c(34)])
#cycle_07_17$classification[cycle_07_17$state != "Alaska" &cycle_07_17$state != "District of Columbia"] <- fit$classification
#cycle_07_17$state[cycle_07_17$classification == 2 & cycle_07_17$state != "Alaska" & cycle_07_17$state != "District of Columbia"]

#plotDensityMclust1(densityMclust(cycle_07_17[-c(2,9),c(35)]), col = "red", lty = 3)
#plot(fit, what = "density", addEllipses = TRUE, lty = 2, col = "red", xlim =c(11,11.8))
plot(density(cycle_77_17$log_real_wage[cycle_77_17$state != "Alaska" & cycle_77_17$state != "District of Columbia"]), ylim = c(0, 2.7),lty = 1, xlab = "Log of average real wage (1977-2017)", main = "", title = NULL, cex.lab=1.5)

x<-seq(from = fit$parameters$mean[[1]]-3*sqrt(fit$parameters$variance$sigmasq[[1]]), to =  fit$parameters$mean[[1]]+3*sqrt(fit$parameters$variance$sigmasq[[1]]), by = 0.01)
hx<-dnorm(x, mean = fit$parameters$mean[[1]], sd = sqrt(fit$parameters$variance$sigmasq[[1]]))
lines(x,fit$parameters$pro[[1]]*hx, lty = 2, col="red")

x_1<-seq(from = fit$parameters$mean[[2]]-3*sqrt(fit$parameters$variance$sigmasq[[1]]), to =  fit$parameters$mean[[2]]+3*sqrt(fit$parameters$variance$sigmasq[[1]]), by = 0.01)
hx_1<-dnorm(x_1, mean = fit$parameters$mean[[2]], sd = sqrt(fit$parameters$variance$sigmasq[[1]]))
lines(x_1,fit$parameters$pro[[2]]*hx_1, lty = 2, col = "blue")

plot(fit, what="BIC")

par(new=T)
with(cycle_07_17,plot(fit, what="uncertainty", axes=F, xlab = NA, ylab = NA))
axis(side = 4)
mtext(side = 4, line = 3, 'Number genes selected')


#plot(density(cycle_00_07$log_prod[cycle_00_07$classification == 1 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]), main="",ylim =c(0,5), xlim =c(10.95,11.8),xlab = "Log of average productivity share (2000-2007)", lty = 3)
#abline(v=mean(cycle_00_07$log_prod[cycle_00_07$classification == 1 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]),col="red")
#lines(density(cycle_00_07$log_prod[cycle_00_07$classification == 2 & cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]), lty =3)
#lines(density(cycle_00_07$log_prod[cycle_00_07$state != "Alaska" & cycle_00_07$state != "District of Columbia"]), lty = 1)
#abline(v=mean(cycle_00_07$log_prod[cycle_00_07$state != "Alaska" &cycle_00_07$state != "District of Columbia"]))


#Chi2testMixtures for univariate model (NULL is that model is good)
#NOTE: If variance varies (V), can just include sigmasq, but must include twice if type (E)
#NOTE: used montecarlo sim
Chi2testMixtures(fit$data, fit$parameters$mean, c(fit$parameters$variance$sigmasq,fit$parameters$variance$sigmasq), fit$parameters$pro, MonteCarloSampling = T)

Chi2testMixtures(fit$data, fit$parameters$mean, c(fit$parameters$variance$sigmasq,fit$parameters$variance$sigmasq), fit$parameters$pro, MonteCarloSampling = F)

#plot evolution of distribution of payroll share (and animation)-----
ggplot(subset(National, year==1999),
       aes(wage_share2, fill = factor(year), colour = factor(year))) +
  geom_density(alpha=0.1)+
  xlab("payroll share")+
  xlim(c(0.4,0.8))

ggplot(National,
       aes(wage_share2, fill = factor(year), colour = factor(year))) +
  geom_density(alpha=0.1)+
  xlab("payroll share")+
  xlim(c(0.4,0.8))+
   transition_states(
    ordered(year),
    transition_length = 2,
    state_length = 1)

ggplot(National,
       aes(wage_share2, fill = factor(year), colour = factor(year))) +
  geom_density(alpha=0.1)+
  xlab("payroll share")+
  xlim(c(0.4,0.8))+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {round(frame_time, 0)}') +
  transition_time(year) +
  ease_aes('linear')+
  theme(legend.position = "none")
  
  
    #geom_vline(aes(xintercept=mean(wage_share2[year==1979])),
  #           color="red", linetype="dashed", size=0.5)+
  #geom_vline(aes(xintercept=mean(wage_share2[year==1989])),
  #           color="yellow", linetype="dashed", size=0.5)+
  #geom_vline(aes(xintercept=mean(wage_share2[year==2000])),
  #           color="green", linetype="dashed", size=0.5)+
  #geom_vline(aes(xintercept=mean(wage_share2[year==2007])),
  #           color="blue", linetype="dashed", size=0.5)+
  #geom_vline(aes(xintercept=mean(wage_share2[year==2017])),
  #           color="purple", linetype="dashed", size=0.5)
  
#plot aggregate--------------------
fit<-Mclust(cycle_07_17[-c(2,9),c(12, 14, 25, 24)])
plot(fit, what = "classification")

#plot state wage shares over time
p<-ggplot() +geom_line(data = subset(National, state != "Alaska" & state != "District of Columbia"), aes(x = year, y = wage_share2, color = factor(state), legend.position = "none")) + geom_line(data=subset(National, state == "Alabama"), aes(x=year, y = (national_comp/national_va_current_wo_taxes)),color="black", size=1) 
p+theme(legend.position = "none")+labs(y="Wage Share", x="Year")


#Rank correlation###########

#should we use VA_weight or avg_empl_share??

#test whether rank correlation in the latter two periods are statstically different than previous two
t.test(c(cor(rank(abs(cycle_00_07$total_contribution_pp)), rank(cycle_00_07$va_share)),cor(rank(abs(cycle_07_17$total_contribution_pp)), rank(cycle_07_17$va_share))), mu = mean(c(cor(rank(abs(cycle_79_89$total_contribution_pp)), rank(cycle_79_89$va_share)),cor(rank(abs(cycle_89_00$total_contribution_pp)), rank(cycle_89_00$va_share)))))

#test whether rank correlation in the latter two periods are statstically different than second
t.test(c(cor(rank(abs(cycle_00_07$total_contribution)), rank(cycle_00_07$va_share)),cor(rank(abs(cycle_07_17$total_contribution)), rank(cycle_07_17$va_share))), mu = cor(rank(abs(cycle_89_00$total_contribution)), rank(cycle_89_00$va_share)) )


#Moran's I##########

#source: https://mgimond.github.io/simple_moransI_example/
cycle_79_89$relative_avg_real_wage<- cycle_79_89$avg_real_wage.real_wage/mean(cycle_79_89$avg_real_wage.real_wage)
cycle_89_00$relative_avg_real_wage<- cycle_89_00$avg_real_wage.real_wage/mean(cycle_89_00$avg_real_wage.real_wage)
cycle_00_07$relative_avg_real_wage<- cycle_00_07$avg_real_wage.real_wage/mean(cycle_00_07$avg_real_wage.real_wage)
cycle_07_17$relative_avg_real_wage<- cycle_07_17$avg_real_wage.real_wage/mean(cycle_07_17$avg_real_wage.real_wage)

cycle_79_89$relative_avg_labor_prod<- cycle_79_89$avg_labor_prod.labor_prod/mean(cycle_79_89$avg_labor_prod.labor_prod)
cycle_89_00$relative_avg_labor_prod<- cycle_89_00$avg_labor_prod.labor_prod/mean(cycle_89_00$avg_labor_prod.labor_prod)
cycle_00_07$relative_avg_labor_prod<- cycle_00_07$avg_labor_prod.labor_prod/mean(cycle_00_07$avg_labor_prod.labor_prod)
cycle_07_17$relative_avg_labor_prod<- cycle_07_17$avg_labor_prod.labor_prod/mean(cycle_07_17$avg_labor_prod.labor_prod)




x<-subset(cycle_07_17, state != "Hawaii" & state != "Alaska" & state != "District of Columbia")

#Establish state shape file:
sf<-us_states(NULL,NULL,as.character(x$state))

sf<-merge(sf, x, by.x="name", by.y="state")


tm_shape(sf) + tm_fill(title="Relative avg. \n real wage",
                       col="relative_avg_real_wage",
                       style="cont",
                       n=8, palette=c("red", "white", "blue"),
                       midpoint = 1,
                       breaks = c(0.6,0.8,1,1.2, 1.4)) +
  tm_legend(outside=FALSE, title="2007-2017", title.position=c(0.85,0.05))


#Step 1: Define neighboring polygons:
nb <- poly2nb(sf,row.names = seq(10,48))


#Step 2: Assign weights to the neighbors:
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

#Compute the weighted neighbor mean values:
var.lag<-lag.listw(lw, sf$relative_avg_real_wage)
var.lag

#Step 4: Computing the Moran's I statistic:
I<-moran(sf$phi, lw, length(nb), Szero(lw))[1]

#Step 5: Performing a hypothesis test:
moran.test(sf$total_contribution_pp,lw, alternative="greater")
MC<-moran.mc(sf$total_contribution_pp, lw, nsim=999, alternative = "greater")
plot(MC, xlab="Total contribution (% point)")



##Spatial decomposition; follows Dawkins (2007):

#spatial dataframe
# National_spatial<-National[0,]
# for(i in 1977:2017){
#   df<-subset(National, year == i & GeoName != "Hawaii" & GeoName != "Alaska" & GeoName != "District of Columbia")
#   sf<-us_states(NULL,NULL,as.character(df$GeoName))
#   nb <- poly2nb(sf, queen=TRUE)
#   lw<-nb2listw(nb, style="W")
#   
#   df_sf<-merge(sf, df, by.x="name", by.y="GeoName")
#   df$wage_rate_statechained<-lag.listw(lw, df_sf$wage_rate_statechained)
#   
#   df$empl_share<-lag.listw(lw, df_sf$empl_share)
#   df$labor_prod<-lag.listw(lw, df_sf$labor_prod)
#   df$price_ratio<-lag.listw(lw, df_sf$price_ratio)
#   National_spatial<-rbind(National_spatial, df)
# }

sf<-us_states(NULL,NULL,as.character(cycle_79_89$state[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"]))
nb <- poly2nb(sf, queen=TRUE)
lw<-nb2listw(nb, style="W")


spatial_cycle_79_89<-subset(cycle_79_89[,c(1:5)], state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cycle_79_89$d_comp<-lag.listw(lw, cycle_79_89$d_comp[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])
spatial_cycle_79_89$d_tech<-lag.listw(lw, cycle_79_89$d_tech[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])
spatial_cycle_79_89$d_price<-lag.listw(lw, cycle_79_89$d_price[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])
spatial_cycle_79_89$d_str<-lag.listw(lw, cycle_79_89$d_str[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])
spatial_cycle_79_89$labor_prod_change<-lag.listw(lw, cycle_79_89$labor_prod_change[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])
spatial_cycle_79_89$total_contribution<-lag.listw(lw, cycle_79_89$total_contribution[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"])



spatial_cycle_89_00<-subset(cycle_89_00[,c(1:5)], state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cycle_89_00$d_comp<-lag.listw(lw, cycle_89_00$d_comp[cycle_89_00$state != "Alaska" & cycle_89_00$state != "Hawaii" & cycle_89_00$state != "District of Columbia"])
spatial_cycle_89_00$d_tech<-lag.listw(lw, cycle_89_00$d_tech[cycle_89_00$state != "Alaska" & cycle_89_00$state != "Hawaii" & cycle_89_00$state != "District of Columbia"])
spatial_cycle_89_00$d_price<-lag.listw(lw, cycle_89_00$d_price[cycle_89_00$state != "Alaska" & cycle_89_00$state != "Hawaii" & cycle_89_00$state != "District of Columbia"])
spatial_cycle_89_00$d_str<-lag.listw(lw, cycle_89_00$d_str[cycle_89_00$state != "Alaska" & cycle_89_00$state != "Hawaii" & cycle_89_00$state != "District of Columbia"])
spatial_cycle_89_00$total_contribution<-lag.listw(lw, cycle_89_00$total_contribution[cycle_89_00$state != "Alaska" & cycle_89_00$state != "Hawaii" & cycle_89_00$state != "District of Columbia"])


spatial_cycle_00_07<-subset(cycle_00_07[,c(1:5)], state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cycle_00_07$d_comp<-lag.listw(lw, cycle_00_07$d_comp[cycle_00_07$state != "Alaska" & cycle_00_07$state != "Hawaii" & cycle_00_07$state != "District of Columbia"])
spatial_cycle_00_07$d_tech<-lag.listw(lw, cycle_00_07$d_tech[cycle_00_07$state != "Alaska" & cycle_00_07$state != "Hawaii" & cycle_00_07$state != "District of Columbia"])
spatial_cycle_00_07$d_price<-lag.listw(lw, cycle_00_07$d_price[cycle_00_07$state != "Alaska" & cycle_00_07$state != "Hawaii" & cycle_00_07$state != "District of Columbia"])
spatial_cycle_00_07$d_str<-lag.listw(lw, cycle_00_07$d_str[cycle_00_07$state != "Alaska" & cycle_00_07$state != "Hawaii" & cycle_00_07$state != "District of Columbia"])
spatial_cycle_00_07$total_contribution<-lag.listw(lw, cycle_00_07$total_contribution[cycle_00_07$state != "Alaska" & cycle_00_07$state != "Hawaii" & cycle_00_07$state != "District of Columbia"])


spatial_cycle_07_17<-subset(cycle_07_17[,c(1:5)], state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cycle_07_17$d_comp<-lag.listw(lw, cycle_07_17$d_comp[cycle_07_17$state != "Alaska" & cycle_07_17$state != "Hawaii" & cycle_07_17$state != "District of Columbia"])
spatial_cycle_07_17$d_tech<-lag.listw(lw, cycle_07_17$d_tech[cycle_07_17$state != "Alaska" & cycle_07_17$state != "Hawaii" & cycle_07_17$state != "District of Columbia"])
spatial_cycle_07_17$d_price<-lag.listw(lw, cycle_07_17$d_price[cycle_07_17$state != "Alaska" & cycle_07_17$state != "Hawaii" & cycle_07_17$state != "District of Columbia"])
spatial_cycle_07_17$d_str<-lag.listw(lw, cycle_07_17$d_str[cycle_07_17$state != "Alaska" & cycle_07_17$state != "Hawaii" & cycle_07_17$state != "District of Columbia"])
spatial_cycle_07_17$total_contribution<-lag.listw(lw, cycle_07_17$total_contribution[cycle_07_17$state != "Alaska" & cycle_07_17$state != "Hawaii" & cycle_07_17$state != "District of Columbia"])


spatial_cycle_77_17<-subset(cycle_77_17[,c(1:5)], state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cycle_77_17$d_comp<-lag.listw(lw, cycle_77_17$d_comp[cycle_77_17$state != "Alaska" & cycle_77_17$state != "Hawaii" & cycle_77_17$state != "District of Columbia"])
spatial_cycle_77_17$d_tech<-lag.listw(lw, cycle_77_17$d_tech[cycle_77_17$state != "Alaska" & cycle_77_17$state != "Hawaii" & cycle_77_17$state != "District of Columbia"])
spatial_cycle_77_17$d_price<-lag.listw(lw, cycle_77_17$d_price[cycle_77_17$state != "Alaska" & cycle_77_17$state != "Hawaii" & cycle_77_17$state != "District of Columbia"])
spatial_cycle_77_17$d_str<-lag.listw(lw, cycle_77_17$d_str[cycle_77_17$state != "Alaska" & cycle_77_17$state != "Hawaii" & cycle_77_17$state != "District of Columbia"])
spatial_cycle_77_17$total_contribution<-lag.listw(lw, cycle_77_17$total_contribution[cycle_77_17$state != "Alaska" & cycle_77_17$state != "Hawaii" & cycle_77_17$state != "District of Columbia"])




sub_cyc<-subset(cycle_07_17, state != "Alaska" & state != "Hawaii" & state != "District of Columbia")
spatial_cyc<-spatial_cycle_07_17

G_r<-2*sum((rank(spatial_cyc$total_contribution)-49/2)*sub_cyc$total_contribution)/(sum(sub_cyc$total_contribution)*48)
G_b<-2*sum((rank(sub_cyc$total_contribution)-49/2)*sub_cyc$total_contribution)/(sum(sub_cyc$total_contribution)*48)
S_r<-G_r/G_b
print(S_r)



##SPATIAL AUTOCORRELATION INDEX CALCULATION######

#import industry shares:
industry_empl_shares <- read_csv("industry_empl_shares.csv")

sf<-us_states(NULL,NULL,as.character(cycle_79_89$state[cycle_79_89$state != "Alaska" & cycle_79_89$state != "Hawaii" & cycle_79_89$state != "District of Columbia"]))
sf<-sf[order(sf$name),]
nb <- poly2nb(sf, queen=TRUE)


National<-merge(National, industry_empl_shares, by.x=c("year","GeoName"),by.y=c("National.year","National.state"))

#customize neighbors here:
dropped_neighbors <- read_excel("dropped_neighbors.xlsx")
dropped_neighbors$new_neighbor_list_int<-strsplit(dropped_neighbors$new_neighbor_list_int, split = " ")
dropped_neighbors$new_neighbor_list_int<-lapply(dropped_neighbors$new_neighbor_list_int, as.integer)

for(i in 1:length(nb)){
  nb[i]<-dropped_neighbors$new_neighbor_list_int[i]
}


#create weights:
lw<-nb2listw(nb, style="W")
gamma <- data.frame(matrix(ncol = 9, nrow =41 ))
x <- c("year", "wage_rate_statechained", "labor_prod", "price_ratio", "empl_share",
       "wage_share2", "man_empl_share", "prog_empl_share", "stag_empl_share")
colnames(gamma) <- x
gamma$year<-1977:2017

G <- data.frame(matrix(ncol = 17, nrow =41 ))
x <- c("year", "G_r_wage_rate_statechained", "G_r_labor_prod", "G_r_price_ratio",
       "G_r_empl_share","G_r_wage_share2", "G_b_wage_rate_statechained", "G_b_labor_prod",
       "G_b_price_ratio", "G_b_empl_share", "G_b_wage_share2", "G_b_man_empl_share", "G_r_man_empl_share",
       "G_b_stag_empl_share", "G_r_stag_empl_share", "G_b_prog_empl_share", "G_r_prog_empl_share")
colnames(G) <- x
G$year<-1977:2017

for(i in 1977:2017){
  df<-subset(National, year == i & GeoName != "Alaska" & GeoName != "District of Columbia" & GeoName != "Hawaii")
  df_spatial<-df

  df_spatial$wage_rate_statechained<-lag.listw(lw, df$wage_rate_statechained)
  df_spatial$labor_prod<-lag.listw(lw, df$labor_prod)
  df_spatial$empl_share<-lag.listw(lw, df$empl_share)
  df_spatial$price_ratio<-lag.listw(lw, df$price_ratio)
  df_spatial$wage_current<-lag.listw(lw, df$wage_current)
  df_spatial$wage_share2<-lag.listw(lw, df$wage_share2)
  df_spatial$man_empl_share<-lag.listw(lw, df$man_empl_share)
  df_spatial$prog_empl_share<-lag.listw(lw, df$prog_empl_share)
  df_spatial$stag_empl_share<-lag.listw(lw, df$stag_empl_share)
  
  
  G_r<-2*sum((rank(df_spatial$wage_rate_statechained)-49/2)*df$wage_rate_statechained)/(sum(df$wage_rate_statechained)*48)
  G_b<-2*sum((rank(df$wage_rate_statechained)-49/2)*df$wage_rate_statechained)/(sum(df$wage_rate_statechained)*48)
  S_r<-G_r/G_b
  gamma$wage_rate_statechained[i-1977+1]<-S_r
  G$G_r_wage_rate_statechained[i-1977+1]<-G_r
  G$G_b_wage_rate_statechained[i-1977+1]<-G_b
  
  
  G_r<-2*sum((rank(df_spatial$labor_prod)-49/2)*df$labor_prod)/(sum(df$labor_prod)*48)
  G_b<-2*sum((rank(df$labor_prod)-49/2)*df$labor_prod)/(sum(df$labor_prod)*48)
  S_r<-G_r/G_b
  gamma$labor_prod[i-1977+1]<-S_r
  G$G_r_labor_prod[i-1977+1]<-G_r
  G$G_b_labor_prod[i-1977+1]<-G_b
  
  
  G_r<-2*sum((rank(df_spatial$empl_share)-49/2)*df$empl_share)/(sum(df$empl_share)*48)
  G_b<-2*sum((rank(df$empl_share)-49/2)*df$empl_share)/(sum(df$empl_share)*48)
  S_r<-G_r/G_b
  gamma$empl_share[i-1977+1]<-S_r
  G$G_r_empl_share[i-1977+1]<-G_r
  G$G_b_empl_share[i-1977+1]<-G_b
  
  G_r<-2*sum((rank(df_spatial$price_ratio)-49/2)*df$price_ratio)/(sum(df$price_ratio)*48)
  G_b<-2*sum((rank(df$price_ratio)-49/2)*df$price_ratio)/(sum(df$price_ratio)*48)
  S_r<-G_r/G_b
  gamma$price_ratio[i-1977+1]<-S_r
  G$G_r_price_ratio[i-1977+1]<-G_r
  G$G_b_price_ratio[i-1977+1]<-G_b

  G_r<-2*sum((rank(df_spatial$wage_share2)-49/2)*df$wage_share2)/(sum(df$wage_share2)*48)
  G_b<-2*sum((rank(df$wage_share2)-49/2)*df$wage_share2)/(sum(df$wage_share2)*48)
  S_r<-G_r/G_b
  gamma$wage_share2[i-1977+1]<-S_r
  G$G_r_wage_share2[i-1977+1]<-G_r
  G$G_b_wage_share2[i-1977+1]<-G_b
  
  G_r<-2*sum((rank(df_spatial$man_empl_share)-49/2)*df$man_empl_share)/(sum(df$man_empl_share)*48)
  G_b<-2*sum((rank(df$man_empl_share)-49/2)*df$man_empl_share)/(sum(df$man_empl_share)*48)
  S_r<-G_r/G_b
  gamma$man_empl_share[i-1977+1]<-S_r
  G$G_r_man_empl_share[i-1977+1]<-G_r
  G$G_b_man_empl_share[i-1977+1]<-G_b
  
  G_r<-2*sum((rank(df_spatial$prog_empl_share)-49/2)*df$prog_empl_share)/(sum(df$prog_empl_share)*48)
  G_b<-2*sum((rank(df$prog_empl_share)-49/2)*df$prog_empl_share)/(sum(df$prog_empl_share)*48)
  S_r<-G_r/G_b
  gamma$prog_empl_share[i-1977+1]<-S_r
  G$G_r_prog_empl_share[i-1977+1]<-G_r
  G$G_b_prog_empl_share[i-1977+1]<-G_b
  
  G_r<-2*sum((rank(df_spatial$stag_empl_share)-49/2)*df$stag_empl_share)/(sum(df$stag_empl_share)*48)
  G_b<-2*sum((rank(df$stag_empl_share)-49/2)*df$stag_empl_share)/(sum(df$stag_empl_share)*48)
  S_r<-G_r/G_b
  gamma$stag_empl_share[i-1977+1]<-S_r
  G$G_r_stag_empl_share[i-1977+1]<-G_r
  G$G_b_stag_empl_share[i-1977+1]<-G_b
}



plot(gamma$year, gamma$wage_rate_statechained, ylim=c(-0.1,0.9),type="l", col="blue", xlab="year", ylab=expression(gamma))
lines(gamma$year, gamma$labor_prod, col="red")
#lines(gamma$year, gamma$price_ratio, col="black")
#lines(gamma$year, gamma$empl_share, col="green")
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)
abline(h=0, lty=1)


plot(gamma$year, gamma$wage_share2, ylim=c(-0.1,0.9),type="l", col="black", xlab="year", ylab=expression(gamma), lwd=2)
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)
abline(h=0, lty=1)

plot(gamma$year, gamma$man_empl_share, ylim=c(0.1,0.7),type="l", col="gold3", xlab="year", ylab=expression(gamma), lwd=2)
lines(gamma$year, gamma$prog_empl_share, ylim=c(0.1,0.4),type="l", col="forestgreen", xlab="year", ylab=expression(gamma), lwd=2)
lines(gamma$year, gamma$stag_empl_share, ylim=c(0.1,0.4),type="l", col="red", xlab="year", ylab=expression(gamma), lwd=2)
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)
abline(h=0, lty=1)

plot(gamma$year, gamma$progressive_empl_share, ylim=c(0.1,0.4),type="l", col="black", xlab="year", ylab=expression(gamma), lwd=2)
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)
abline(h=0, lty=1)

plot(G$year, G$G_r_stag_empl_share, ylim=c(0,0.2),type="l",lty=2, col="black", xlab="year", ylab=expression(paste(G[r],", ",G[B])))
lines(G$year, G$G_b_stag_empl_share, ylim=c(0,0.2),type="l", col="black", xlab="year", ylab=expression(gamma))
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)

plot(G$year, G$G_r_prog_empl_share, ylim=c(-0.1,0.2),type="l",lty=2, col="black", xlab="year", ylab=expression(paste(G[r],", ",G[B])))
lines(G$year, G$G_b_prog_empl_share, ylim=c(-0.1,0.2),type="l", col="black", xlab="year", ylab=expression(gamma))
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)

#plot of G, gamma, for psi:
par(mfrow=c(1,2),mar=c(4, 4, 4, 4) + 0.1)
plot(G$year, G$G_r_wage_share2, ylim=c(0,0.2), xlab="", ylab="", 
     type="l",lty=1, main="",xlim=c(1977,2017))
#axis(2, ylim=c(0,1),lwd=2,line=3.5)
#points(G$year, G$G_b_wage_share2,pch=20)
#mtext(2,text=expression(paste(G[r],", ", G[b])),line=5.5)
lines(G$year, G$G_b_wage_share2, lty=1, lwd=3)
legend(x="topright",legend=c(expression(paste(G[B])),expression(paste(G[r]))),lty=c(1,1), lwd=c(3,1))
#par(new=T)
plot(gamma$year, gamma$wage_share2,ylim=c(0,1),type="l",lty=1, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))
#points(gamma$year, gamma$wage_share2, pch=20, col="black")
#axis(4, ylim=c(0,1),col="black",lwd=1)
#mtext(4,text=expression(paste(gamma)),line=1)
#legend(x="topright",legend=c(expression(paste(gamma))),lty=c(1), lwd=c(1))


#plot of G, gamma, for omega and varepsilon:
par(mfrow=c(1,2),mar=c(4, 4, 4, 4) + 0.1)
plot(G$year, G$G_r_wage_rate_statechained, ylim=c(0,0.2), col="black",xlab="", ylab="", 
     type="l",lty=5, main="",xlim=c(1977,2017))
lines(G$year, G$G_b_wage_rate_statechained, lty=5, lwd=3, col="black")
lines(G$year, G$G_r_labor_prod, col="black", lty=3)
lines(G$year, G$G_b_labor_prod, col="black", lty=3, lwd=3)
legend(x="topleft",legend=c(expression(paste(omega,"  ")),expression(paste(epsilon))),lty=c(5,3), lwd=c(1,1), col = c("black","black","black"))
legend(x="topright",legend=c(expression(paste(G[B])),expression(paste(G[r]))),lty=c(1,1), lwd=c(3,1))

par(new=F)
plot(gamma$year, gamma$wage_rate_statechained,ylim=c(0,1),type="l",lty=5, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))
lines(gamma$year, gamma$labor_prod, axes=F,ylim=c(0,1),type="l",lty=3, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))
#points(gamma$year, gamma$wage_share2, pch=20, col="black")
#axis(4, ylim=c(0,1),col="black",lwd=1)
#mtext(4,text=expression(paste(gamma)),line=1)
#legend(x=2010,y=0.85,legend=c(expression(paste(G[b]^omega)),expression(paste(G[b]^epsilon)),expression(paste(G[r]^omega)),expression(paste(G[r]^epsilon)),expression(paste(gamma^omega)),expression(paste(gamma^epsilon))),lty=c(2,2,3,3,1,1), col=c("blue", "red", "blue", "red", "blue", "red"))
#legend(x="topleft",legend=c(expression(paste(omega)),expression(paste(epsilon))),lty=c(0,0), col = c("blue", "red"), pch=c(15,15), bty='n')
#legend(x="topleft",legend=c(expression(paste(omega,"  ")),expression(paste(epsilon))),lty=c(5,3), lwd=c(1,1), col = c("black","black","black"))
#legend(x="topright",legend=c(expression(paste(gamma," "))),lty=c(1,5), lwd=c(1,1), col = c("black","black","black"))


#plot G for lambda s, m, p:
plot(G$year, G$G_r_man_empl_share, ylim=c(0,0.3), col="black",xlab="", ylab="", 
     type="l",lty=5, main="",xlim=c(1977,2017))
lines(G$year, G$G_b_man_empl_share, lty=5, lwd=3, col="black")

lines(G$year, G$G_r_stag_empl_share, lty=4, col="black")
lines(G$year, G$G_b_stag_empl_share, lty=4, lwd=3, col="black")

lines(G$year, G$G_r_prog_empl_share, lty=3, col="black")
lines(G$year, G$G_b_prog_empl_share, lty=3, lwd=3, col="black")
legend(x="topleft",legend=c(expression(paste(m)),expression(paste(s)),expression(paste(p))),lty=c(5,4,3), lwd=c(1,1,1), col = c("black","black","black"))
legend(x="topright",legend=c(expression(paste(G[B])),expression(paste(G[r]))),lty=c(1,1), lwd=c(3,1))

par(new=F)
plot(gamma$year, gamma$man_empl_share,ylim=c(0,1),type="l",lty=5, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))
lines(gamma$year, gamma$stag_empl_share, axes=F,ylim=c(0,1),type="l",lty=4, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))
lines(gamma$year, gamma$prog_empl_share, axes=F,ylim=c(0,1),type="l",lty=3, lwd=2, col="black", xlab="", ylab="", main="", xlim=c(1977,2017))








plot(G$year, G$G_b_wage_share2, ylim=c(-0.1,0.2),type="l", col="black", xlab="year", ylab=expression(paste(G[r],", ",G[B])))
plot(G$year, G$G_r_wage_share2, ylim=c(-0.1,0.2),type="l", col="black", xlab="year", ylab=expression(gamma))
abline(v=1979, lty=2)
abline(v=1989, lty=2)
abline(v=2000, lty=2)
abline(v=2007, lty=2)



National$state<-National$GeoName
plot_usmap(data = subset(National, year == 1977 & state != "Alaska" & state != "Hawaii" & state != "District of Columbia"), 
           values = "labor_prod",regions = "states") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), 
        legend.position = c(0.86,0)
  ) +
  scale_fill_gradient(low = "white", 
                      high = "blue", name = "Wage rate")
#theme(legend.position = "none")

plot(National$year[National$state=="Washington"], National$wage_rate_statechained[National$state=="Washington"], type="l", ylim=c(20000,70000))
lines(National$year[National$state=="Oregon"], National$wage_rate_statechained[National$state=="Oregon"], type="l")

plot(National$year[National$state=="Washington"], National$labor_prod[National$state=="Washington"], type="l", ylim=c(40000,120000))
lines(National$year[National$state=="Oregon"], National$labor_prod[National$state=="Oregon"], type="l")


plot(National$year[National$state=="California"], (National$labor_prod[National$state=="California"]-National$labor_prod[National$state=="Oregon"]), ylim=c(5000,35000),type="l", col="red",
  xlab="year", ylab="Disparity ($); CA, OR")
lines(National$year[National$state=="California"], (National$wage_rate_statechained[National$state=="California"]-National$wage_rate_statechained[National$state=="Oregon"]), type="l", col="blue")


plot(National$year[National$state=="Washington"], National$labor_prod[National$state=="Washington"]-National$labor_prod[National$state=="Idaho"], type="l", ylim=c(0, 45000), col="red",
     xlab="year", ylab="Disparity ($); WA, ID")
lines(National$year[National$state=="Washington"], National$wage_rate_statechained[National$state=="Washington"]-National$wage_rate_statechained[National$state=="Idaho"],type="l", col="blue")



#defining neighbors as shared MSA borders:
metro_counties <- read_csv("metro_counties.csv")
metro_counties$true<-1

plot_usmap(data = metro_counties, 
           values = "true",regions = "counties") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), 
        legend.position = c(0.86,0)
  ) +
  scale_fill_gradient(low = "white", 
                      high = "blue", name = "Wage rate")
#theme(legend.position = "none")


all_counties<-county.fips






calc_comp_growth<-function(start, end, n){
  x<-((end/start)^(1/n)-1)*100
  print(x)
}
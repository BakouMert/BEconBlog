#Simulation of tax effect of 

library(tidyverse)
library(data.table)
library(ggplot2)
load("data_BE_shiny.RData")

theme_bakou <- function(font = "serif" , fs_title = 18, fs_subtitle= 18, fs_ax_leg = 17) {
  
  theme_classic() + theme(
    axis.line = element_blank(),
    axis.text = element_text(family = font, size = fs_ax_leg),
    axis.text.x = element_blank(),
    axis.title.x = element_text(family = font, size = fs_ax_leg),
    axis.ticks.x = element_blank(),
    legend.text = element_text(family = font,size=fs_ax_leg),
    legend.title = element_text(family = font,size=fs_ax_leg),
    legend.position = "none",
    strip.background = element_blank(),
    plot.title = element_text( family = font, size = fs_title, face = 'bold'), 
    plot.subtitle = element_text(family = font, size=fs_subtitle, face="italic", margin=margin(b=-30), hjust = 0.5 ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),    
    
  )
}


deciles_maker <- function(data, vars, n) {
  for(i in vars) {
    data <- data %>% mutate(x = ntile(.data[[i]], n = n) )
    data <- rename(data, !!paste0(i, "_", n, "_tile") := x)
  }
  return(data)
}
share_b <- function(data, ntile, type) {
  total <- sum(data[,type], na.rm=TRUE)
  data <- data %>% group_by(.data[[ntile]]) %>% mutate(pshare =  sum(.data[[type]], na.rm=TRUE)/total,
                                                       psum= sum(.data[[type]], na.rm=TRUE),
                                                       na_mean = fifelse(.data[[type]]==0, NA_integer_, .data[[type]]),
                                                       na_group = fifelse(all(is.na(na_mean)), "yes", "no"),
                                                       na_mean_corr = fifelse(na_group == "yes", replace_na(0), na_mean),
                                                       pmean = mean(na_mean_corr, na.rm=TRUE))
  
  return(data)
}

data_simpl <- data %>% select(wage, vol_pension, pub_transfer_income, tot_income)
income_tax <- function(income, brackets, rates) {
  
  sum(diff(c(0,pmin(income, brackets)))*rates)
} 
data_simpl <- data_simpl %>% rowwise() %>% mutate(wpt = wage + pub_transfer_income +vol_pension,
                                                  taxes_now_wpt = income_tax(wpt, brackets = c(9050, 13450,23900, 41360,100000000), rates = c(0,0.25, 0.4,0.45,0.5)),
                                                  after_tax_now_wpt = wpt-taxes_now_wpt,
                                                  taxes_now_tot_income = income_tax(tot_income,brackets = c(9050, 13450,23900, 41360,100000000), rates = c(0,0.25, 0.4,0.45,0.5)),
                                                  after_tax_now_tot_income = tot_income-taxes_now_tot_income,
                                                  taxes_then_wpt = income_tax(wpt,brackets = c(8909, 15554, 19451, 23312, 31108, 38941, 58430, 77882, 116860, 155838, 233720, 311602, 1090281,1000000000), rates = c(0, 0.24, 0.277, 0.358, 0.39443,0.436,0.45,0.466,0.516,0.565,0.619,0.678,0.708,0.663)),
                                                  after_tax_then_wpt = wpt-taxes_then_wpt,
                                                  taxes_then_tot_income = income_tax(tot_income,brackets = c(8909, 15554, 19451, 23312, 31108, 38941, 58430, 77882, 116860, 155838, 233720, 311602, 1090281,1000000000), rates = c(0, 0.24, 0.277, 0.358, 0.39443,0.436,0.45,0.466,0.516,0.565,0.619,0.678,0.708,0.663)),
                                                  after_tax_then_tot_income = tot_income-taxes_then_tot_income,) %>% ungroup()
data_simpl <- deciles_maker(data_simpl, "tot_income", 10)
data_simpl <- share_b(data_simpl, "tot_income_10_tile", "after_tax_now_wpt") %>% mutate(pshare_after_now_wpt= pshare) %>% select(-c(psum:pmean))
data_simpl <- share_b(data_simpl, "tot_income_10_tile", "after_tax_now_tot_income") %>% mutate(pshare_after_now_tot_income= pshare) %>% select(-c(psum:pmean))
data_simpl <- share_b(data_simpl, "tot_income_10_tile", "after_tax_then_wpt") %>% mutate(pshare_after_then_wpt= pshare) %>% select(-c(psum:pmean))
data_simpl <- share_b(data_simpl, "tot_income_10_tile", "after_tax_then_tot_income") %>% mutate(pshare_after_then_tot_income= pshare) %>% select(-c(psum:pmean))
data_simpl <- share_b(data_simpl,"tot_income_10_tile", "tot_income" ) %>% mutate(pshare_before_tot_income= pshare) %>% select(-c(psum:pmean))
data_simpl <- share_b(data_simpl,"tot_income_10_tile", "wpt" ) %>% mutate(pshare_before_wpt= pshare) %>% select(-c(psum:pmean))
pshares <- data_simpl %>% select(tot_income_10_tile, pshare_after_now_wpt: pshare_before_wpt) %>% distinct(.keep_all = TRUE)
pshares <- pivot_longer(pshares, cols = pshare_after_now_wpt:pshare_before_wpt, names_to = "antepost", values_to = "share")
p1 <- pshares %>% filter(str_detect(antepost, "income")) %>% group_by(tot_income_10_tile) %>% ggplot(aes(x=tot_income_10_tile, y= share, fill= antepost)) + geom_col(position="dodge") + 
  scale_fill_brewer(limits= c("pshare_before_tot_income","pshare_after_now_tot_income", "pshare_after_then_tot_income" ), labels= c( "Before Taxes", "After today's taxes","After taxes 1988") , palette= "Set2")+ coord_cartesian(ylim = c(0,0.3)) + theme_bakou() + theme(legend.position = "right")
p1

#ook PMEAN doen ipv pshare dan zie je dat mediaangroep er bv 1500 euro op vooruit gaan (op inkomen van 30.000). hoogste deciel gaat er gemiddeld 4000 euro op achteruit op gem inkomen van 90.000
# Wel slecht aan systeem van 88, is dat de quasi top decielen er ook significant op vooruit gaan. Ook problematisch is dat er een licht belastingstekort ontstaat (ongeveer 4% minder belastingsinkomsten in 1988 tarief)
#som_nu  som_toen
#<dbl>     <dbl>
# 1 52858201. 50290480.

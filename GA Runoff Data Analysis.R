##### Important insight from the 2021 Georgia Senate runoffs
## The turnout game -- did Democrats show up in larger numbers than Republicans? ##

library(readxl)
library(tidyverse)

###### Election Data Import/Cleaning #####

## Import/Clean runoff results
# GA general runoff
GArunoff1 <- read_excel("GA Runoff (G).xlsx", sheet = 3) # import election results from Excel file

GArunoff1$Perdue_votes <- GArunoff1$...7
GArunoff1$Ossoff_votes <- GArunoff1$...12
GArunoff1$Tot_votes <- GArunoff1$...13
GArunoff1$county <- GArunoff1$`US Senate (Perdue)`

GArunoff <- GArunoff1 %>% 
  select(county, Perdue_votes, Ossoff_votes, Tot_votes)

GArunoff <- GArunoff[-(1:2), ]

# GA special runoff
GArunoff2 <- read_excel("GA Runoff (G).xlsx", sheet = 4) # import election results from Excel file

GArunoff2$Loeffler_votes <- GArunoff2$...7
GArunoff2$Warnock_votes <- GArunoff2$...12
GArunoff2$Tot_votes <- GArunoff2$...13
GArunoff2$county <- GArunoff2$`US Senate (Loeffler) - Special`

GArunoff_s <- GArunoff2 %>% 
  select(county, Loeffler_votes, Warnock_votes, Tot_votes)

GArunoff_s <- GArunoff_s[-c(1:2), ]

GArunoff_results <- merge(GArunoff, GArunoff_s, by = "county") # merge data

i <- c(2:7)
GArunoff_results[, i] <- apply(GArunoff_results[ , i], 2, as.numeric) # convert to numeric

str(GArunoff_results)

GArunoff_results <- GArunoff_results %>% 
  rename(
    Total_votes_general = "Tot_votes.x",
    Total_votes_special = "Tot_votes.y"
  )

GArunoff_results <- GArunoff_results %>% 
  mutate(
    Ossoff_pct = Ossoff_votes / Total_votes_general, # Ossoff share
    Warnock_pct = Warnock_votes / Total_votes_special, # Warnock share
    Perdue_pct = 1 - Ossoff_pct, # Perdue share
    Loeffler_pct = 1 - Warnock_pct, # Loeffler share
    Ossoff_marg = Ossoff_pct - Perdue_pct, 
    Warnock_marg = Warnock_pct - Loeffler_pct
  )

## import GA general election (Nov 2020) data
GAgeneral <- read_excel("detail 4.xlsx", sheet = 2) # import election results from Excel file

GAgeneral$Trump_votes <- GAgeneral$`Total Votes...6`
GAgeneral$Biden_votes <- GAgeneral$`Total Votes...11`

GAgeneral <- select(GAgeneral, County, Total, Trump_votes, Biden_votes)

GAgeneral <- GAgeneral %>% 
  mutate(Trump_pct = Trump_votes / Total, # Trump share
         Biden_pct = Biden_votes / Total, # Biden share
         Biden_marg = Biden_pct - Trump_pct #Biden margin
  ) %>% 
  rename(county = County)

GArunoff_results2 <- merge(GArunoff_results, GAgeneral, by = "county") # merge data

str(GArunoff_results2) # ensure all are numbers

GAturnout <- GArunoff_results2 %>% 
  group_by(county) %>% 
  summarise(avg_runoff_turnout = mean(Total_votes_general, Total_votes_special))

GArunoff_results3 <- merge(GArunoff_results2, GAturnout, by = "county")

GArunoff_results3 <- GArunoff_results3 %>% 
  mutate(turnout_pct_ch = (avg_runoff_turnout - Total) / Total,
         Dem = ifelse(Biden_marg > 0, "Dem", "GOP"))

##### Show the relationships graphically #####
library(ggthemes)

GArunoff_results3 %>% 
  mutate(Pres_margin = Trump_pct - Biden_pct,
         Pres_margin_100 = (Pres_margin*100),
         turnout_pct_ch_100 = (turnout_pct_ch * 100)) %>% 
  filter(Total < 4000000) %>% # take out "total" observation"
  ggplot(aes(Pres_margin_100, turnout_pct_ch_100)) +
  geom_point(aes(size = Total, color = Dem), alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", color = "black", se=FALSE) +
  geom_text(aes(label = ifelse(Total > 135000, county, ""), hjust = 0.6, vjust = -0.8)) +
  theme_clean() +
  theme(legend.position = "none") +
  labs(x = "Margin in November election", y="Percent change in turnout")

## Analysis: shows that on average, turnout was lower everywhere compared to general election
## but turnout in Republican-leaning counties saw lower turnout on average compared to November
## Georgia's population centers (i.e. Atlanta metro area) showed up in large numbers 
## Meanwhile, turnout was lacking on the Republican side
## and R-leaning counties in GA are generally less populous than D-leaning counties, so this hurt Perdue/Loeffler.

# another way of looking at it - as percentage of November turnout

GArunoff_results3 <- GArunoff_results3 %>% 
  mutate(turnout_pct_Nov = avg_runoff_turnout / Total * 100)
  
mean(GArunoff_results3$turnout_pct_Nov) # on average, turnout in counties at 88.3% of November turnout

GArunoff_results3 %>% 
  mutate(Pres_margin = Trump_pct - Biden_pct,
         Pres_margin_100 = (Pres_margin*100),
         turnout_pct_Nov = (avg_runoff_turnout / Total) * 100) %>% 
  filter(Total < 4000000)  %>% # take out "total" observation"
  ggplot(aes(Pres_margin_100, turnout_pct_Nov)) +
  geom_point(aes(size = Total, color = Dem), alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 88.3) +
  geom_smooth(method = "lm", color = "black", se=FALSE) +
  geom_text(aes(label = ifelse(Total > 135000, county, ""), hjust = 0.6, vjust = -0.8)) +
  theme_clean() +
  theme(legend.position = "none") +
  labs(x = "Margin in November election", y= "Turnout as percent of November election") 

## Overwhelming majority of Democratic-leaning counties had above average turnout as percent of November  
## Republican counties, meanwhile, were more split, with several below 84% of November turnout



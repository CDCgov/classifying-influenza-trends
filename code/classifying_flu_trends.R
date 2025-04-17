## Classifying Influenza Trends (A framework for classifying disease trends applied to influenza hospital admissions) 
## Mathis, SM, et al. 

library(tidyverse)
library(DT)
library(zoo)
library(cowplot) 
library(egg)

categories <- factor(c("large_decrease", "decrease", "stable", "increase", "large_increase"), levels= c("large_decrease", "decrease", "stable", "increase", "large_increase"))
nice_categories <- c("Large decrease", "Decrease", "Stable", "Increase", "Large increase")

# this function is to add a dummy guide to the plot below since the colors are not tied to data in the dataframe
dummy_guide <- function(
    labels = NULL,  
    ..., 
    title = NULL, 
    key   = draw_key_point,
    guide_args = list()
) {
  # Capture arguments
  aesthetics <- list(...)
  n <- max(lengths(aesthetics), 0)
  labels <- labels %||%  seq_len(n)
  
  # Overrule the alpha = 0 that we use to hide the points
  aesthetics$alpha <- aesthetics$alpha %||% rep(1, n)
  
  # Construct guide
  guide_args$override.aes <- guide_args$override.aes %||% aesthetics
  guide <- do.call(guide_legend, guide_args)
  
  # Allow dummy aesthetic
  update_geom_defaults("point", list(dummy = "x"))
  
  dummy_geom <- geom_point(
    data = data.frame(x = rep(Inf, n), y = rep(Inf, n), 
                      dummy = factor(labels)),
    aes(x, y, dummy = dummy), alpha = 0, key_glyph = key
  )
  dummy_scale <- discrete_scale(
    "dummy", "dummy_scale", palette = scales::identity_pal(), name = title,
    guide = guide
  )
  list(dummy_geom, dummy_scale)
}


### NHSN data
# #nhsn data were initially downloaded from healthdata.gov[https://healthdata.gov/resource/qqte-vkut.json] and transformed to weekly counts for FluSight forecasting. See https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-truth/get_truth.R for transformation 

#2021-2022 and 2022-2023 "truth" target data
nhsn_23 <- read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-truth/truth-Incident%20Hospitalizations.csv")


#location data (2021-2022/2022-2023)
locations23 <-read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv") %>%
  mutate(count_rate1=NULL,count_rate2=NULL,location=NULL)

# #join hospitalization and location data
# #filter for a couple of weeks after reporting became mandatory for an "adjustment" period, take out VI and calculate rates per 100k population
nhsn_pop <- right_join(nhsn_23,locations23) %>%
  filter(date > as.Date("2022-02-20"), abbreviation != "VI") %>%
  mutate(count = value, value = value*100000/population)

## getting the rest of the data 
early_period <-  right_join(nhsn_23,locations23) %>%
  filter(date >= as.Date("2022-02-02"), date < as.Date("2023-10-01"), abbreviation != "VI") %>%
  mutate(weekly_rate = value*100000/population,
         date = as.Date(date))

late_period <- readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/refs/heads/main/auxiliary-data/target-data-archive/target-hospital-admissions_2024-04-27.csv", show_col_types = FALSE) %>% select(-X) %>% 
  filter(date >= as.Date("2023-10-01")) %>% arrange(date)

mandatory_reporting <- bind_rows(select(early_period, date, location, location_name, value, weekly_rate), late_period)

### FSN data 
#flusurv data were downloaded from: https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html
# Data for the entire network for FluSurvNET prior to 2022-2023 was downloaded separately than site-specific datasets which were all downloaded separately. 
# FluSurvNET rates for the entire network was downloaded separately from the sites for the 2022-2023 season 

#data download, data cleaning, and data combining 
  # Dowloaded from FluView for entire network, each jurisdiction
  # FluSurvNET prior to 2022-23 season, leave out 2009-2010 season, entire network only
  fsn_22 <- read.csv(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/classifying-influenza-trends/data/FluSurv_Data_221107.csv")) %>%
    filter(MMWR.YEAR>2009, YEAR != "2009-10", YEAR != "2020-21", YEAR != "2022-23") %>% #this was pulled when only part of the 22-23 season was available
    filter(SEX.CATEGORY == "Overall" &
             RACE.CATEGORY == "Overall" &
             AGE.CATEGORY == "Overall")
  
  # add in 22-23 entire network downloaded from 
  fsn_23 <- read.csv(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/classifying-influenza-trends/data/FluSurveillance_Custom_Download_Data_2023.csv"), skip = 2) %>%
    filter(YEAR == "2022-23",SEX.CATEGORY == "Overall" & RACE.CATEGORY == "Overall" & AGE.CATEGORY == "Overall",NETWORK == "FluSurv-NET")
  
  # 22-23 fsn state specific data
  fsn_st_23 <- read.csv(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/classifying-influenza-trends/data/state_datasets/FluSurveillance_FSNStates2023.csv")) %>% mutate(WEEKLY.RATE = as.numeric(WEEKLY.RATE))
  
  # prior years state specific data
  setwd(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flu-Visualizations/Analyses/increase_decrease/state_datasets"))
  fsn_st_22 <- list.files(path = paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/classifying-influenza-trends/data/state_datasets"),
                          pattern = "*\\.csv") %>%
    map_df(.f = function(FILE){
      dat.out <- read_csv(FILE, show_col_types = FALSE) %>%
        mutate(`WEEKLY RATE` = as.numeric(`WEEKLY RATE`),
               `CUMULATIVE RATE` = as.numeric(`CUMULATIVE RATE`))
      return(dat.out)
    })
  
  #filter out entire network, 22-23 partial season and 09-10 season from old state data
  fsn_st_22 <-  fsn_st_22 %>%
    filter(CATCHMENT != "Entire Network", YEAR != "2022-23", YEAR != "2020-21", YEAR != "2009-10",
           `MMWR-YEAR` > 2009, `AGE CATEGORY` == "Overall", `SEX CATEGORY` == "Overall", `RACE CATEGORY` == "Overall") %>%
    select(CATCHMENT, YEAR, `MMWR-YEAR`, `MMWR-WEEK`, `CUMULATIVE RATE`, `WEEKLY RATE`)
  
  
  #FSN data cleaning and combining datasets
  fsn_us <- rbind(fsn_22, fsn_23) %>% mutate(SEX.CATEGORY = NULL, RACE.CATEGORY = NULL, AGE.CATEGORY = NULL,
                                             CATCHMENT = "US", NETWORK = NULL ) %>%
    mutate(CUMULATIVE.RATE = as.numeric(CUMULATIVE.RATE), YEAR = as.factor(YEAR)) %>%
    select(CATCHMENT, YEAR, MMWR.YEAR, MMWR.WEEK, CUMULATIVE.RATE, WEEKLY.RATE)
  
  #match column names for state and US FSN data
  colnames(fsn_st_22) <- colnames(fsn_us)
  
  fsn_st <-  fsn_st_23 %>%
    filter(CATCHMENT != "Entire Network", YEAR != "2009-10", YEAR != "2020-21",  MMWR.YEAR > 2009,
           AGE.CATEGORY == "Overall", SEX.CATEGORY == "Overall", RACE.CATEGORY == "Overall") %>%
    select(CATCHMENT, YEAR, MMWR.YEAR, MMWR.WEEK, CUMULATIVE.RATE, WEEKLY.RATE) %>%
    rbind(., fsn_st_22)
  
  #combining fsn data and censoring until there are at least 3 weeks of consecutive non-0s at beginning of each season
  fsn_all <- rbind(fsn_us, fsn_st) %>%
    mutate(MMWR.WEEK = as.character(MMWR.WEEK)) %>%
    group_by(CATCHMENT, YEAR) %>%
    mutate(idx = rollapply(WEEKLY.RATE, width = 3, min, align = "left", fill = 0, na.rm = TRUE)) %>%
    filter(MMWR.WEEK < 40 | (WEEKLY.RATE > 0 & idx > 0)) %>%
    select (-idx) %>%
    ungroup()


#FSN rate diff calculation
fsn_rt_diff <- fsn_all %>% group_by(CATCHMENT,YEAR) %>%
  mutate(diff_0 = WEEKLY.RATE-lag(WEEKLY.RATE))%>% ungroup() %>%
  pivot_longer(cols = c(diff_0), names_prefix = "diff_", names_to = "horizon", values_to = "rate_diff") %>%
  rename('location_name' = 'CATCHMENT') %>%
  mutate(dataset = "FluSurv-NET")

#FSN overall quantiles
fsn_quant <- fsn_rt_diff %>% group_by(horizon) %>%
  summarise(mean = mean(rate_diff, na.rm = TRUE),
            median = median(rate_diff, na.rm = TRUE),
            sd = sd(rate_diff, na.rm=TRUE),
            min = min(rate_diff, na.rm = TRUE),
            percentile_05 = quantile(rate_diff, probs = .05, na.rm = TRUE), #lower 90
            percentile_125 = quantile(rate_diff, probs = .125, na.rm = TRUE), #lower 75
            percentile_25  = quantile(rate_diff, probs = .25,  na.rm = TRUE), #lower 50
            percentile_375 = quantile(rate_diff, probs = .375, na.rm = TRUE), #lower 25
            percentile_625 = quantile(rate_diff, probs = .625, na.rm = TRUE), #upper 25
            percentile_75  = quantile(rate_diff, probs = .75,  na.rm = TRUE), #upper 50
            percentile_875 = quantile(rate_diff, probs = .875, na.rm = TRUE), #upper 75
            percentile_95 = quantile(rate_diff, probs = .95, na.rm = TRUE), #upper 90
            max = max(rate_diff, na.rm = TRUE)) %>%
  ungroup() %>% mutate(dataset = "FluSurv-NET")

colnames(fsn_rt_diff) <- tolower(names(fsn_rt_diff))


###
#nhsn data

#join hospitalization and location data
nhsnweekly_seasons <- mandatory_reporting %>% 
  mutate(mmwryear = epiyear(date), 
         epiweek = epiweek(date), 
         season = factor(case_when(mmwryear == 2022 & epiweek < 40  ~ "2122", 
                                   (mmwryear == 2022 & epiweek >=40)|(mmwryear == 2023 & epiweek < 40) ~ "2223", 
                                   (mmwryear == 2023 & epiweek >40) | mmwryear == 2024 ~ "2324"))) %>% 
  filter(!epiweek %in% (18:39)) %>%  
  group_by(location, location_name, season) %>%
  mutate(idx = rollapply(weekly_rate, width = 3, min, align = "left", fill = 0, na.rm = TRUE)) %>%
  filter(epiweek < 40 | (weekly_rate > 0 & idx > 0)) %>%
  select (-idx) %>%
  ungroup()

#calculate rate difference from 1-5 previous weeks
nhsndiff <- nhsnweekly_seasons %>% group_by(location_name) %>%
  mutate(diff_0 = weekly_rate-lag(weekly_rate)) %>%
  ungroup() %>%
  #longer format so it's easier to work with
  pivot_longer(cols = c(diff_0), names_prefix = "diff_", names_to = "horizon", values_to = "rate_diff") 


##nhsn/nhsn quantile calculations
nhsn_quant <- nhsndiff %>% group_by(horizon) %>%
  summarise(mean = mean(rate_diff, na.rm = TRUE),
            median = median(rate_diff, na.rm = TRUE),
            sd = sd(rate_diff, na.rm=TRUE),
            min = min(rate_diff, na.rm = TRUE), 
            percentile_05 = quantile(rate_diff, probs = .05, na.rm = TRUE), #lower 90
            percentile_125 = quantile(rate_diff, probs = .125, na.rm = TRUE), #lower 75
            percentile_25  = quantile(rate_diff, probs = .25,  na.rm = TRUE), #lower 50
            percentile_375 = quantile(rate_diff, probs = .375, na.rm = TRUE), #lower 25
            percentile_625 = quantile(rate_diff, probs = .625, na.rm = TRUE), #upper 25
            percentile_75  = quantile(rate_diff, probs = .75,  na.rm = TRUE), #upper 50
            percentile_875 = quantile(rate_diff, probs = .875, na.rm = TRUE), #upper 75
            percentile_95 = quantile(rate_diff, probs = .95, na.rm = TRUE),  #upper 90
            max = max(rate_diff, na.rm = TRUE)) %>%
  ungroup() %>% mutate(dataset = "NHSN")


#binding datasets and creating quantitative table 


all_quant <- rbind(fsn_quant, nhsn_quant) %>% 
  filter(horizon != "4") 

# pivot longer for ease of plotting
all_quant_long <- all_quant %>% 
  pivot_longer(cols = contains("percentile"), names_to = "percentile", values_to = "rate", names_prefix = "percentile_") 

## Mandatory_reporting_w23-24
all_quant_nice <- all_quant %>% mutate("Mean (SD)" = paste0(round(mean, 1), " (", round(sd,1), ")"), 
                                             "Median (range)" = paste0(round(median,1), " (", round(min,1), ", ",round(max,1),")"), 
                                             across(.cols = contains("percentile"), .fns = round, 1)) %>% 
  select(dataset, horizon, `Mean (SD)`, `Median (range)`, contains("percentile_")) 

# write.csv(all_quant_nice, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/supp_table1.csv"), row.names = FALSE)


## applying thresholds and creating categories 
defined_categories <- mandatory_reporting %>% arrange(date) %>% 
  group_by(location_name) %>% 
  mutate(rate_diff0 = weekly_rate - lag(weekly_rate, 1), 
         count_change0 = value - lag(value, 1)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(rate_diff0), 
               names_to = "horizon", 
               names_prefix = "rate_diff", 
               values_to = "rate_diff", names_transform = list(horizon = as.integer)) %>% 
  mutate(category = case_when(abs(count_change0) < 10 | horizon == 0 & rate_diff < 0.3 & rate_diff > -0.3 ~ "stable",
                              horizon == 0 & rate_diff > 1.7 ~ "large_increase", 
                              horizon == 0 & rate_diff < -1.7 ~ "large_decrease", 
                              horizon == 0 & rate_diff >= 0.3 ~ "increase", 
                              horizon == 0 & rate_diff <= -0.3 ~ "decrease")) 


defined_categoriesplt <-
  ggplot(filter(defined_categories, date > as.Date("2022-10-01"), horizon == 0), aes(x = date, y = location_name))+
  theme_bw()+
  geom_tile(aes(fill = factor(category, levels= c("large_decrease", "decrease", "stable", "increase", "large_increase"))))+
  scale_fill_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057", "grey50"),
                    breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase", NA),
                    labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase", "Not estimated"),
                    na.value = "grey50",
                    drop = FALSE)+
  scale_y_discrete(limits = rev)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.01))+
  labs(x = NULL, y = NULL, fill = "Category")+
  theme(axis.text.x = element_text(angle =45, hjust =1),
        legend.position = "bottom",
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"))+
  guides(fill = guide_legend(nrow = 1))


## proportion tables

inseason_defined_categories <-  filter(defined_categories, !is.na(category)) %>%
  mutate(epiweek = epiweek(date),
         epiyear = epiyear(date),
         season = case_when(epiweek <40 &epiyear == 2022 ~ "21/22",
                            epiweek %in% c(40:53) & epiyear == 2022 ~ "22/23",
                            epiweek %in% c(1:17)& epiyear == 2023 ~ "22/23",
                            epiweek %in% c(40:53) & epiyear == 2023 ~ "23/24",
                            epiweek %in% c(1: 17) & epiyear == 2024 ~ "23/24")) %>%
  filter(season == "22/23" | season == "23/24")


## separate by season 
prop.table(table(filter(inseason_defined_categories, horizon == 0, season == "22/23")$category))

proptable2223 <- as.data.frame(furniture::table1(.data = filter(inseason_defined_categories, horizon == 0, season == "22/23"), location_name,
                  splitby= ~ factor(category, levels = categories, labels = nice_categories),
                  row_wise = TRUE, output = "text"))

# write.csv(proptable2223, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/supp_table2.csv"), row.names = FALSE)

prop.table(table(filter(inseason_defined_categories, horizon == 0, season == "23/24")$category))

proptable2324 <- as.data.frame(furniture::table1(.data = filter(inseason_defined_categories, horizon == 0, season == "23/24"), location_name,
                  splitby= ~ factor(category, levels = categories, labels = nice_categories),
                  row_wise = TRUE, output = "text"))

# write.csv(proptable2324, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/supp_table3.csv"), row.names = FALSE)


## figure 1 
categorized_reporting <- mandatory_reporting %>% filter(date > as.Date("2022-10-01")) %>% 
  arrange(date) %>% 
  group_by(location_name) %>% 
  mutate(rate_diff0 = weekly_rate - lag(weekly_rate, 1),
         count_change0 = value - lag(value, 1)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(rate_diff0), names_to = "horizon", names_prefix = "rate_diff", values_to = "rate_diff", names_transform = list(horizon = as.integer)) %>% 
  mutate(category = case_when(horizon == 0 & (abs(count_change0) < 10 | rate_diff < 0.3 & rate_diff > -0.3 )~ "stable",
                              horizon == 0 & rate_diff > 1.7 ~ "large_increase", 
                              horizon == 0 & rate_diff < -1.7 ~ "large_decrease", 
                              horizon == 0 & rate_diff >= 0.3 ~ "increase", 
                              horizon == 0 & rate_diff <= -0.3 ~ "decrease")) 

uscat <-
  categorized_reporting %>% filter(horizon == 0, location_name == "US") %>% 
  ggplot(aes(x = date, y = weekly_rate, group = location_name))+ #, alpha = alpha_setting
  theme_bw()+
  geom_line(data = filter(categorized_reporting, location_name == "US", horizon == 0), linewidth = 1)+
  geom_point(data = filter(categorized_reporting, location_name == "US", horizon == 0),  
             size = 3,
             shape = 21,
             aes(fill = factor(category, 
                               levels= c("large_decrease", "decrease", "stable", "increase", "large_increase"))))+
  scale_fill_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057", "grey50"),
                    breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase", NA),
                    labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase", "Not estimated"),
                    na.value = "grey50", 
                    drop = FALSE)+
  scale_color_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057"),
                     breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase"),
                     labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase"),
                     na.value = "grey50", 
                     drop = FALSE)+
  labs(x = NULL, y = "per 100k", color = NULL, fill = NULL, alpha = NULL, title = NULL)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0.01,0.01))+
  scale_y_continuous(breaks = c(0,2,4,6,8), labels = c(" 0", " 2", " 4", "  6", " 8"))+
  theme(#strip.text.y.left = element_text(angle = 0),
    axis.title.y = element_text(hjust = 0), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"))



othercat <- categorized_reporting %>% filter(horizon == 0) %>%
  ggplot(aes(x = date, y = weekly_rate, group = location_name))+ #, alpha = alpha_setting
  geom_line(data = filter(categorized_reporting, location_name != "US", horizon == 0), 
            alpha = .2, 
            linewidth =.75)+
  geom_point(data = filter(categorized_reporting, location_name != "US", horizon == 0), 
             alpha = .5, 
             #size = 2,
             shape = 21,
             aes(fill = factor(category, 
                               levels= c("large_decrease", "decrease", "stable", "increase", "large_increase"))))+
  theme_bw()+
  
  scale_fill_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057", "grey50"),
                    breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase", NA),
                    labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase", "NA"),
                    na.value = "grey50", 
                    drop = FALSE)+
  scale_color_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057"),
                     breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase"),
                     labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase"),
                     na.value = "grey50", 
                     drop = FALSE)+
  labs(x = NULL, y = "Weekly rate", color = NULL, fill = NULL, alpha = NULL, title = NULL)+
  scale_x_date(expand = c(0.01,0.01))+
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  theme(#strip.text.y.left = element_text(angle = 0), 
    axis.title.y = element_text(hjust = 1),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))

cwplt3 <-
  ggarrange(uscat, othercat, defined_categoriesplt, nrow = 3, ncol = 1, heights = c(.3, .2, .5),  labels = c("a", "b", "c"))

# ggsave(plot = cwplt3, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/figure1.pdf"), height = 12, width = 10, dpi = 500)
# ggsave(plot = cwplt3, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/figure1.png"), height = 12, width = 10)



## Supplementary figure 1 
diffsetall2 <- rbind(select(fsn_rt_diff, location_name, year, mmwr.year,horizon, rate_diff, dataset), 
                     select(mutate(nhsndiff, mmwr.year = mmwryear, year = lubridate::year(date), dataset = "NHSN"),
                            location_name, year, mmwr.year, horizon, rate_diff, dataset)) %>% filter(horizon != "4")

s_figure1 <- diffsetall2 %>% filter(horizon == 0) %>% 
  ggplot(x = rate_diff)+
  geom_histogram(aes(x = rate_diff), binwidth = .1, fill = "gray70", color = "gray10")+
  theme_bw()+
  facet_grid(rows = vars(dataset), scales = "free")+
  
  annotate("rect", xmin = -Inf, xmax = -1.7, ymin = -Inf, ymax = Inf, fill = "#006166", alpha = .5)+
  annotate("rect", xmin = -1.7, xmax = -0.3, ymin = -Inf, ymax = Inf, fill = "#3BBBB0", alpha = .5)+
  annotate("rect", xmin = -0.3, xmax = 0.3, ymin = -Inf, ymax = Inf, fill = "#E3E3E3", alpha = .5)+
  annotate("rect", xmin = 0.3, xmax = 1.7, ymin = -Inf, ymax = Inf, fill = "#C13897", alpha = .5)+
  annotate("rect", xmin = 1.7, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#6B0057", alpha = .5)+
  theme(legend.position = "top")+
  geom_vline(data = filter(all_quant_long, horizon == 0), aes(xintercept = rate), linetype = "dashed",color = "black",  linewidth = 1)+
  scale_x_continuous(breaks = seq(-5, 5, 1), limits = c(-5,5))+
  labs(x = "Rate change", y = "Frequency", title = NULL, caption = "FSN seasons: 10/11-22/23, except 20/21; NHSN seasons: 21/22-23/24") +
  #this adds the dummy guide for the categories
  #maybe not necessary but nice to have. fct_inorder keeps your categories in the order listed, otherwise they will be alphabetical
  dummy_guide(labels = fct_inorder(c("Large decrease", "Decrease", "Stable", "Increase", "Large increase")), 
              fill = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057"), 
              color = NA, 
              title = "Category",
              key = draw_key_polygon)

# ggsave(plot = s_figure1, paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/classifying-influenza-trends/output/supp_figure1.png"), 
#        height = 6, width = 8)



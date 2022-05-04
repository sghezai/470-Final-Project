if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, plotly, gganimate, cobalt, ivpack, stargazer, haven, ggthemes,
               magick, rdrobust, haven, estimatr, lfe, rddensity, here, modelsummary,
               fixest, dotwhisker)

#load in data 
final.insurance <- read_rds("/home/econ470-test/R/insurance-access/insurance.rds")
kff.final <- read_rds("/home/econ470-test/R/insurance-access/medicaid-kff.rds")
ins.dat <- final.insurance %>%
  left_join(kff.final, by="State") %>%
  mutate(expand_year = year(date_adopted),
         expand = (year>=expand_year & !is.na(expand_year))) %>%
  rename(expand_ever=expanded)


#create variables that calculate the share of the population in each group 
ins.dat <- ins.dat %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop)

#creates a dataset of data >= the expansion year 
ins.dat.2014 <- ins.dat %>% 
  mutate(post = (year>=2014), treat=post*expand_ever) %>% 
  filter(is.na(expand_year) | expand_year==2014)


#share of Medicaid graph 
acs_data_plot <- acs_data %>% filter(!is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_medicaid))

ggplot(data=acs_data_plot, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = acs_data_plot %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype=FALSE) +
  labs(
    x="Year",
    y="Fraction Medicaid",
    title="Share of Medicaid over Time"
  )

#share of private insurance graph 
acs_data_plot2 <- acs_data %>% filter(!is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_private))

ggplot(data=acs_data_plot2, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = acs_data_plot2 %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype=FALSE) +
  labs(
    x="Year",
    y="Fraction Private",
    title="Share of Private over Time"
  )


#difference in differences regression analysis 
summary(lm(perc_private ~ post + expand_ever + post*expand_ever, data=acs_data_2014))

#fixed effect (unused)
summary(feols(perc_private ~ treat | State + year, data=acs_data_2014))

#event study with common treatment timing (unused)
event_data <- acs_data_2014 %>%
  mutate(expand_2012 = expand_ever*(year==2012),
         expand_2013 = expand_ever*(year==2013),
         expand_2014 = expand_ever*(year==2014),
         expand_2015 = expand_ever*(year==2015),
         expand_2016 = expand_ever*(year==2016),
         expand_2017 = expand_ever*(year==2017),
         expand_2018 = expand_ever*(year==2018))

event_ins_reg <- lm(perc_private ~ expand_2012 + expand_2014 + 
                      expand_2015 + expand_2016 + expand_2017 + 
                      expand_2018 + factor(year) + factor(State), data=event_data)
point_est <- as_tibble(c(event_ins_reg$coefficients[c("expand_2012","expand_2014","expand_2015",
                                                      "expand_2016","expand_2017","expand_2018")]),
                       rownames = "term")
ci_est <- as_tibble(confint(event_ins_reg)[c("expand_2012","expand_2014","expand_2015",
                                             "expand_2016","expand_2017","expand_2018"),],
                    rownames = "term")
point_est <- point_est %>% rename(estimate = value)
ci_est <- ci_est %>% rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
new_row <- tibble(
  term = "expand_2013",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  year = 2013
)

event_plot_data <- point_est %>%
  left_join(ci_est, by=c("term")) %>%
  mutate(year = c(2012, 2014, 2015, 2016, 2017, 2018)) %>%
  bind_rows(new_row) %>%
  arrange(year)

dwplot(event_plot_data, 
       vline=geom_vline(xintercept=0, linetype=2), 
       vars_order = c("expand_2018","expand_2017","expand_2016",
                      "expand_2015","expand_2014","expand_2013",
                      "expand_2012"),
       whisker_args = list(color="black", size=1.1),
       dot_args = list(color="black")) + 
  coord_flip() + theme_bw() + theme(legend.position = "none") +
  labs(y = "Year",
       x = "Estimate and 95% CI",
       title = "Event Study Estimates for Medicaid and Private Insurance Rate") +
  scale_y_discrete(labels = c("expand_2012" = "2012", 
                              "expand_2013" = "2013",
                              "expand_2014" = "2014",
                              "expand_2015" = "2015",
                              "expand_2016" = "2016",
                              "expand_2017" = "2017",
                              "expand_2018" = "2018"))

#differential treatment timing event study graph 
reg_data <- acs_data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, -1, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))

mod_twfe <- feols(perc_private~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg_data)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study')


save.image("470finalprojectanalysis.R")
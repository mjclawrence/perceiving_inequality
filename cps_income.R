library(tidyverse)
library(Hmisc)
library(gghighlight)
library(ggthemes)
library(scales)

#cps_income <- read.csv("/users/lawrence/desktop/perceiving_inequality/cps_income.csv")
cps_income <- read.csv("https://raw.githubusercontent.com/mjclawrence/perceiving_inequality/master/cps_income.csv")

names(cps_income) <- tolower(names(cps_income))

summary(cps_income$inctot)

cps_income <- cps_income %>%
  mutate(inctot = na_if(inctot, 99999999),
         inctot = ifelse(inctot<0, 0, inctot),
         uhrsworkly = na_if(uhrsworkly, 999))

cps_income_subset <- cps_income %>%
  filter(workly==2 | #respondent worked in previous year
           uhrsworkly>=1 | #respondent worked 1 or more hours a week 
           wantjob==2) #respondent not in the labor force wants a job

cps_income_subset_2 <- cps_income_subset %>%
  filter(age %in% 21:29 & #age restriction
           uhrsworkly>=35) #hours restriction
# filter(educ==111) #bachelor's degree only

distribution <- cps_income_subset_2 %>% 
  mutate(percentile = ntile(inctot, 100), # 100 for percentiles, 5 for quin
         percentile = percentile - 1 ) %>%
  group_by(percentile) %>%
  summarise(dollar = min(inctot))

wtd.distribution <- wtd.quantile(cps_income_subset_2$inctot,
                             probs = seq(0, 1, by= 0.01), 
                             weight = cps_income_subset_2$asecwt)

wtd.distribution


pctle <- seq(0, 100, by = 1)

wtd_distribution <- cbind.data.frame(pctle, wtd.distribution) %>%
  rename(salary = wtd.distribution)


wtd_distribution <- wtd_distribution %>%
  filter(pctle<100) %>%
  mutate(subset = ifelse(pctle == 90 | pctle == 95, 1, 
                         ifelse(pctle == 75 | pctle == 55, 2,
                                ifelse(pctle == 25, 3, 0))),
         job = ifelse(pctle==25, "Janitor",
                      ifelse(pctle==55, "Reporter",
                             ifelse(pctle==75, "Geology Research Associate",
                                    ifelse(pctle==90, "Electrical Engineer",
                                           ifelse(pctle==95, "Investment Banker", ""))))))


# Look at distribution of all incomes with focal jobs highlighted
salary_plot <- ggplot(wtd_distribution, aes(x = pctle/100, y = salary, fill = job))
salary_plot + geom_col() + coord_flip()


# First attempt at including text labels
salary_plot_0 <- salary_plot + geom_col() +
  scale_y_continuous(labels = scales::dollar, limits=c(0,200000)) +
  scale_x_continuous(labels = scales::percent) +
  theme_tufte() +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  labs(x = "Percent of Salaries That Are Lower",
       y = "Salary",
       title = "Actual Income Distribution for Full-Time Workers Ages 21-29",
       caption = "Salary information comes from US Bureau of Labor Statistics' 2018 Current Population Survey") +
  gghighlight(subset == 1 | subset == 2 | subset == 3, label_key = subset, use_group_by = FALSE) +
  guides(fill = FALSE) + scale_fill_viridis_d() +
  geom_text(x = .45, y = 190000, 
            label = "The average salary for an investment banker is $98,349. \nApproximately 95% of salaries are lower than that amount.",
            size = 5, color = "#258083") +
  geom_text(x = .45, y = 160000, 
            label = "The average salary for an electrical engineer is $73,493. \nApproximately 90% of salaries are lower than that amount.",
            size = 5, color = "#3C104F") +
  geom_text(x = .45, y = 130000, 
            label = "The average salary for a geology research associate is $52,576. \nApproximately 75% of salaries are lower than that amount.",
            size = 5, color = "#324883") +
  geom_text(x = .45, y = 100000,
            label = "The average salary for a reporter at a local newspaper is $35,231. \nApproximately 55% of salaries are lower than that amount.",
            size = 5, color = "#FFE420") +
  geom_text(x = .45, y = 70000, 
            label = "The average salary for a janitor at an office building is $22,303. \nApproximately 25% of salaries are lower than that amount.",
            size = 5, color = "#5ABC5A")

salary_plot_0



# This is the final plot used in the experiment. 
# May need to zoom and resize the window to see everything.
salary_plot_1 <- salary_plot + geom_col() +
  scale_y_continuous(labels = scales::dollar, limits=c(0,200000)) +
  scale_x_continuous(labels = scales::percent) +
  theme_tufte() +
  theme(axis.text = element_text(size = 16),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  labs(x = "Percent of Salaries That Are Lower",
       y = "Salary",
       title = "Actual Income Distribution for Full-Time Workers Ages 21-29",
       caption = "Salary information comes from US Bureau of Labor Statistics' 2018 Current Population Survey") +
  gghighlight(subset == 1 | subset == 2 | subset == 3, label_key = subset, use_group_by = FALSE) +
  guides(fill = FALSE) + scale_fill_manual(values = c("#3C104F", "#258083", "#324883", "#5ABC5A", "#BB3754")) + 
  geom_text(x = .25, y = 62000, 
    label = "Janitor: Average salary = $22,303
    25% of salaries are lower than this amount",
    color = "#5ABB5A", size = 5) +
  geom_text(x = .55, y = 89500, 
    label = "Local Newspaper Reporter: Average salary = $35,231
    55% of salaries are lower than this amount",
    color = "#BB3754", size = 5) +
  geom_text(x = .74, y = 107000, 
    label = "Geology Research Associate: Average salary = $52,576
    75% of salaries are lower than this amount",
    color = "#268083", size = 5) +
  geom_text(x = .85, y = 120000, 
            label = "Electrical Engineer: Average salary = $73,493
            90% of salaries are lower than this amount",
            color = "#3C104F", size = 5) +
  geom_text(x = .925, y = 144000, 
            label = "Investment Banker: Average salary = $98,349
    95% of salaries are lower than this amount",
            color = "#324883", size = 5) +
  coord_flip()

salary_plot_1





## Luna's Edits

# Look at distribution of all incomes with focal jobs highlighted
salary_plot <- ggplot(wtd_distribution, aes(x = pctle/100, y = salary, fill = job))
salary_plot + geom_area(fill = "dark gray") + coord_flip()

salary_plot_filter <- filter(wtd_distribution, subset>0)

salary_plot_filter <- salary_plot_filter %>%
  mutate(pctle_min = (pctle-1)/100,
         pctle_max = (pctle+1)/100,
         salary_min = 0,
         salary_max = salary)

salary_plot <- salary_plot + 
  geom_area(fill = "light gray") + 
  geom_rect(data = salary_plot_filter, aes(ymin = salary_min, ymax = salary_max,
                                           xmin = pctle_min, xmax = pctle_max,
                                           fill = job)) +
  scale_y_continuous(labels = scales::dollar, limits=c(0,200000)) +
  scale_x_continuous(labels = scales::percent) +
  theme_tufte() +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = .5)) +
  labs(x = "Percent of Salaries That Are Lower",
       y = "Salary",
       title = "Actual Income Distribution for Full-Time Workers Ages 21-29",
       caption = "Salary information comes from US Bureau of Labor Statistics' 2018 Current Population Survey") +
  guides(fill = FALSE) +
  geom_text(x = .25, y = 62000, 
            label = "Janitor: Average salary = $22,303
    25% of salaries are lower than this amount",
            color = "#5ABB5A", size = 5) +
  geom_text(x = .55, y = 89500, 
            label = "Local Newspaper Reporter: Average salary = $35,231
    55% of salaries are lower than this amount",
            color = "#BB3754", size = 5) +
  geom_text(x = .74, y = 107000, 
            label = "Geology Research Associate: Average salary = $52,576
    75% of salaries are lower than this amount",
            color = "#268083", size = 5) +
  geom_text(x = .85, y = 120000, 
            label = "Electrical Engineer: Average salary = $73,493
            90% of salaries are lower than this amount",
            color = "#3C104F", size = 5) +
  geom_text(x = .925, y = 144000, 
            label = "Investment Banker: Average salary = $98,349
    95% of salaries are lower than this amount",
            color = "#324883", size = 5) + coord_flip()

salary_plot




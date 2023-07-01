library(tidyverse)
library(stargazer)

data = read.csv('2022_player_logs.csv')

length(unique(data$Name))

# converts performance type and filters missing values
# adds Treatment variable (1 if treatment, 0 if control) and changes Date to Date format
data = data %>%
  mutate(Performance = as.double(Performance)) %>%
  filter(!is.na(Performance) & is.finite(Performance)) %>%
  group_by(Name) %>%
  mutate(Treatment = as.integer(n_distinct(Team) > 1)) %>%
  mutate(Date = as.Date(paste0('2022 ', Date), format = '%Y %b %d'))

mean_date <- mean(data$Date)

# adds Period variable (1 if post treatment, 0 if pre treatment)
data <- data %>%
  group_by(Name) %>%
  mutate(Period = if_else(Treatment == 1, 
                          as.integer(Team != first(Team)), 
                          Date >= mean_date))

# removes players who did not play in both periods or have missing performance data for either period
data <- data %>%
  group_by(Name, Is_Batter) %>%
  filter(any(Period == 0) & any(Period == 1)) %>%
  filter(!all(is.na(Performance))) 


## data without reducing dates
data_raw = data

# summarize mean performance by player and period
data <- data %>%
  group_by(Name, Period, Treatment, Is_Batter) %>%
  summarize(mean(Performance, na.rm = TRUE), .groups = 'rowwise') %>%
  `colnames<-`(c('Name', "Period", "Treatment", "Is_Batter", "Performance"))

data <- data %>%
  group_by(Name) %>%
  filter(case_when(length(Performance) > 2 ~ Is_Batter == 'False',
                   T ~ TRUE))
# remove batters who have performance = 0 for both periods

length(unique(data$Name))


## Compare mean performance for Period = 0
data %>%
  filter(Period == 0) %>%
  group_by(Treatment, Is_Batter) %>%
  summarise(Mean_Performance = mean(Performance))


## plot trends
batter_trend = data_raw %>%
  filter(Is_Batter == 'True' & Period == 0)

pitcher_trend = data_raw %>%
  filter(Is_Batter == 'False' & Period == 0)

ggplot(data = pitcher_trend, 
       aes(x = Date, y = Performance, color = factor(Treatment))) +
  # geom_bar(aes(fill = Performance))
  # geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') + 
  guides(color = guide_legend(title = "Treatment")) +
  ggtitle("2022 Pre-Treatment Pitcher Trend")



## Manually calculate did estimator for batters
Y_treat_after_batter = data %>% 
  filter(Is_Batter == 'True' & Treatment == 1 & Period == 1) 

Y_treat_before_batter = data %>% 
  filter(Is_Batter == 'True' & Treatment == 1 & Period == 0) 

Y_control_after_batter = data %>% 
  filter(Is_Batter == 'True' & Treatment == 0 & Period == 1) 

Y_control_before_batter = data %>% 
  filter(Is_Batter == 'True' & Treatment == 0 & Period == 0) 

did_estimator_batter = ((mean(Y_treat_after_batter$Performance, na.rm = TRUE) 
                         - mean(Y_treat_before_batter$Performance, na.rm = TRUE)) -
                          (mean(Y_control_after_batter$Performance, na.rm = TRUE)
                           - mean(Y_control_before_batter$Performance, na.rm = TRUE)))



## manually calculate did estimator for pitchers
Y_treat_after_pitcher = data %>% 
  filter(Is_Batter == 'False' & Treatment == 1 & Period == 1) 

Y_treat_before_pitcher = data %>% 
  filter(Is_Batter == 'False' & Treatment == 1 & Period == 0) 

Y_control_after_pitcher = data %>% 
  filter(Is_Batter == 'False' & Treatment == 0 & Period == 1) 

Y_control_before_pitcher = data %>% 
  filter(Is_Batter == 'False' & Treatment == 0 & Period == 0)

did_estimator_pitcher = ((mean(Y_treat_after_pitcher$Performance, na.rm = TRUE) 
                          - mean(Y_treat_before_pitcher$Performance, na.rm = TRUE)) -
                           (mean(Y_control_after_pitcher$Performance, na.rm = TRUE)
                            - mean(Y_control_before_pitcher$Performance, na.rm = TRUE)))

did_estimator_batter
did_estimator_pitcher



## calculate did estimator with regression
reg_data <- data %>%
  group_by(Name) %>%
  mutate(Diff = Performance[Period == 1] - Performance[Period == 0])


batter_data <- reg_data %>%
  filter(Is_Batter == 'True')

pitcher_data <- reg_data %>%
  filter(Is_Batter == 'False')

batter_reg = lm(batter_data$Diff ~ batter_data$Treatment)

pitcher_reg = lm(pitcher_data$Diff ~ pitcher_data$Treatment)


names(batter_reg$coefficients)
names(pitcher_reg$coefficients)

names(batter_reg$coefficients)[names(batter_reg$coefficients) == "batter_data$Treatment"] <- "Trade"
names(pitcher_reg$coefficients)[names(pitcher_reg$coefficients) == "pitcher_data$Treatment"] <- "Trade"


stargazer(batter_reg, pitcher_reg, type = 'text', header = FALSE, title = '2021 Regression', align=TRUE)


## BATTER PLOTTING

# plot control before
X_control_pre_batter = rep(-0.5, length(Y_control_before_batter$Performance))

plot(jitter(X_control_pre_batter, 1), 
     Y_control_before_batter$Performance, 
     ylim = c(0, 1.5), 
     col = alpha("steelblue", 0.1),
     pch = 20, 
     xlim = c(-0.75, 1.75),
     ylab = "Performance (OPS)",
     xlab = "Period",
     xaxt = 'n',
     main = "2021 Batters Pre vs Post Trade")

# plot treat before
X_treat_pre_batter = rep(0, length(Y_treat_before_batter$Performance))

points(jitter(X_treat_pre_batter, 1), 
       Y_treat_before_batter$Performance, 
       col = alpha("firebrick1", 0.1),
       pch = 20)

# plot control after
X_control_post_batter = rep(1, length(Y_control_after_batter$Performance))

points(jitter(X_control_post_batter, 1), 
       Y_control_after_batter$Performance, 
       col = alpha("steelblue", 0.1),
       pch = 20)

# plot treat after
X_treat_post_batter = rep(1.5, length(Y_treat_after_batter$Performance))

points(jitter(X_treat_post_batter, 1), 
       Y_treat_after_batter$Performance, 
       col = alpha("firebrick1", 0.1),
       pch = 20)

# add x axis
axis(1, at = c(-0.5, 1), labels = c('Control',"Control"))
axis(1, at = c(0, 1.5), labels = c("Treat", 'Treat'))


points(-0.5, mean(Y_control_before_batter$Performance, na.rm = TRUE), col = "darkblue", pch = 16, cex = 2)

points(0, mean(Y_treat_before_batter$Performance, na.rm = TRUE), col = "darkred", pch = 16, cex = 2)


points(1, mean(Y_control_after_batter$Performance, na.rm = TRUE), col = "darkblue", pch = 16, cex = 2)

points(1.5, mean(Y_treat_after_batter$Performance, na.rm = TRUE), col = "darkred", pch = 16, cex = 2)

lines(x=c(-0.5, 1), y=c(mean(Y_control_before_batter$Performance, na.rm = TRUE), mean(Y_control_after_batter$Performance, na.rm = TRUE)))

lines(x=c(0, 1.5), y=c(mean(Y_treat_before_batter$Performance, na.rm = TRUE), mean(Y_treat_after_batter$Performance, na.rm = TRUE)))


## PITCHER PLOTTING

# plot control before
X_control_pre_pitcher = rep(-0.5, length(Y_control_before_pitcher$Performance))

plot(jitter(X_control_pre_pitcher, 1), 
     Y_control_before_pitcher$Performance, 
     ylim = c(0, 10), 
     col = alpha("steelblue", 0.1),
     pch = 20, 
     xlim = c(-0.75, 1.75),
     ylab = "Performance (FIP, lower is better)",
     xlab = "Period",
     xaxt = 'n',
     main = "2021 Pitchers Pre vs Post Trade")

# plot treat before
X_treat_pre_pitcher = rep(0, length(Y_treat_before_pitcher$Performance))

points(jitter(X_treat_pre_pitcher, 1), 
       Y_treat_before_pitcher$Performance, 
       col = alpha("firebrick1", 0.1),
       pch = 20)

# plot control after
X_control_post_pitcher = rep(1, length(Y_control_after_pitcher$Performance))

points(jitter(X_control_post_pitcher, 1), 
       Y_control_after_pitcher$Performance, 
       col = alpha("steelblue", 0.1),
       pch = 20)

# plot treat after
X_treat_post_pitcher = rep(1.5, length(Y_treat_after_pitcher$Performance))

points(jitter(X_treat_post_pitcher, 1), 
       Y_treat_after_pitcher$Performance, 
       col = alpha("firebrick1", 0.1),
       pch = 20)

# add x axis
axis(1, at = c(-0.5, 1), labels = c('Control',"Control"))
axis(1, at = c(0, 1.5), labels = c("Treat", 'Treat'))


points(-0.5, mean(Y_control_before_pitcher$Performance, na.rm = TRUE), col = "darkblue", pch = 16, cex = 2)

points(0, mean(Y_treat_before_pitcher$Performance, na.rm = TRUE), col = "darkred", pch = 16, cex = 2)


points(1, mean(Y_control_after_pitcher$Performance, na.rm = TRUE), col = "darkblue", pch = 16, cex = 2)

points(1.5, mean(Y_treat_after_pitcher$Performance, na.rm = TRUE), col = "darkred", pch = 16, cex = 2)

lines(x=c(-0.5, 1), y=c(mean(Y_control_before_pitcher$Performance, na.rm = TRUE), mean(Y_control_after_pitcher$Performance, na.rm = TRUE)))

lines(x=c(0, 1.5), y=c(mean(Y_treat_before_pitcher$Performance, na.rm = TRUE), mean(Y_treat_after_pitcher$Performance, na.rm = TRUE)))


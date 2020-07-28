#install.packages('dplyr')
#install.packages('splitstackshape')
#install.packages('ggplot2')

library(dplyr)
library(ggplot2)
library(splitstackshape)

data <- read.csv('data/data.csv')

carcass_data <- data %>% 
  filter(Treatment == 'Carcass')

#code for the creation of Figure 3

data_pred <- carcass_data %>% 
  filter(date_diff <= 5)

date_diff_summarized <- data_pred %>% 
  group_by(Species, date_diff) %>% 
  summarise(n = n())

theme_set(
  theme_minimal() + theme(legend.position = "top")
)

date_diff_summarized$Species <- factor(date_diff_summarized$Species, levels = 
                                         c("Golden Eagle", "Bald Eagle", 
                                           'Turkey Vulture', 'Magpie', 
                                           "Corvid"), 
                        labels = c("(A)  Golden Eagle", "(B)  Bald Eagle", 
                                   "(C)  Turkey Vulture", '(D)  Magpie', 
                                   '(E)  Corvid'))

p <- ggplot(data = date_diff_summarized, aes(x = date_diff, y = n))+
  geom_col() +
  facet_wrap(~Species ) +
  labs(y = 'Number of animals present', x = 'Days after animal death')+
  theme_bw()

p

q <- p + theme(strip.text.x = element_text(size = 20, face = "bold"), 
               strip.background = element_rect(fill = 'white'),
               text = element_text(size=20))
q


ggsave('results/avian_scav_days_after_death_1200.jpg', width = 12, height = 7, dpi = 1200)


#code for the creation of Figure 5

data_split <- cSplit(carcass_data, splitCols = 'date_real', sep = "/")

data_split2 <- mutate(data_split, 
                      Month = ifelse(date_real_1 == '1', 'Jan', ifelse
                                                    (date_real_1 == '2', 'Feb', ifelse
                                                      (date_real_1 == '3', 'Mar', ifelse
                                                        (date_real_1 == '4', 'Apr', ifelse
                                                          (date_real_1 == '5', 'May', ifelse
                                                            (date_real_1 == '6', 'Jun', ifelse
                                                              (date_real_1 == '7', 'Jul', ifelse
                                                                (date_real_1 == '8', 'Aug', ifelse
                                                                  (date_real_1 == '9', 'Sep', ifelse
                                                                    (date_real_1 == '10', 'Oct', ifelse
                                                                      (date_real_1 == '11', 'Nov', 'Dec')
                                                                      )))))))))))    


bird_month <- data_split2 %>% 
  count(Month, Species)

bird_month$Species <- factor(bird_month$Species, 
                             levels = c("Golden Eagle", "Bald Eagle", 
                                        'Turkey Vulture', 'Magpie', 
                                        "Corvid"), 
                             labels = c("(A)  Golden Eagle", 
                                        "(B)  Bald Eagle", 
                                        "(C)  Turkey Vulture", 
                                        '(D)  Magpie', '(E)  Corvid'))


positions <- c('Jan', 'Feb', 'Mar', 'Apr', 
               'May', 'Jun', 'Jul', 
               'Aug', 'Sep', 'Oct', 'Nov',
               'Dec')

bar1 <- ggplot()+
  geom_bar(aes(y = n, x = Month), data = bird_month,
           color = 'black', stat = 'identity') +
  facet_wrap(Species ~ .)  +
  labs(y = 'Number of animals present', x = 'Month')+
  scale_x_discrete(limits = positions) +
  theme_bw()

bar1

bar2 <- bar1 + theme(strip.text.x = element_text(size = 20, face = "bold"), 
                     strip.background = element_rect(fill = 'white'),
                     text = element_text(size=20),
                     axis.title.x = element_text(vjust=-2.0), 
                     plot.margin = margin(10, 10, 20, 10),
                     axis.text.x = element_text(angle = 60, hjust = 1.2))
bar2

ggsave('results/yearly_presence_1200.jpg', width = 12, height = 7, dpi = 1200)

#code for the creation of Figure 4

split <- cSplit(data, splitCols = 'date_real', sep = "/")

data_4 <- data_split2 %>% 
  filter(date_diff < 16) %>% 
  filter(date_real_1 == '6' | date_real_1 == '7' | date_real_1 == '8')

date_diff_summarized3 <- data_4 %>% 
  group_by(Species, date_diff) %>% 
  summarise(n = n())

date_diff_summarized3 <- date_diff_summarized3 %>% 
  filter(Species == 'Corvid' | Species == 'Turkey Vulture' |
           Species == 'Bald Eagle')

date_diff_summarized3$Species <- factor(date_diff_summarized3$Species, 
                                        levels = c("Bald Eagle", 
                                                   'Turkey Vulture', 
                                                   "Corvid"), 
                         labels = c("(A)  Bald Eagle", 
                                    "(B)  Turkey Vulture", 
                                    "(C)  Corvid"))

bar3 <- ggplot(data = date_diff_summarized3, aes(x = date_diff, y = n))+
  geom_col() +
  facet_wrap(~Species ) +
  labs(y = 'Number of animals present', x = 'Days after animal death')+
  theme_bw() +
  xlim(0, 15)

bar3

bar4 <- bar3 + theme(strip.text.x = element_text(size = 20, face = "bold"), 
                 strip.background = element_rect(fill = 'white'),
                 text = element_text(size=20))
bar4

ggsave('results/risk_season_presence_1200.jpg', width = 12, height = 7, dpi = 1200)

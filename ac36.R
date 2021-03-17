library(tidyverse)
library(lubridate)

# https://en.wikipedia.org/wiki/2019-20_America's_Cup_World_Series
# https://en.wikipedia.org/wiki/2021_Prada_Cup

ac36_teams <- tribble(~ID, ~Team, ~Club, ~Yacht,
                      "NZ", "Emirates Team New Zealand", "Royal New Zealand Yacht Squadron", "Te Rehutai",
                      "IT", "Luna Rossa Prada Pirelli", "Circolo della Vela Sicilia", "Luna Rossa",
                      "US", "American Magic", "New York Yacht Club", "Patriot",
                      "UK", "Ineos Team UK", "Royal Yacht Squadron", "Britannia II")

ac36 <- tribble(~Cup, ~Round, ~Race, ~Date, ~Course, ~Legs, ~Port, ~Stbd, ~Winner, ~Time, ~Delta, ~Distance_m, ~Comment,
                "ACWS", "Day1", "R01", "2020-12-17", "C", 6, "NZ", "IT", "NZ", "25'24", "3'13", 3336, NA,
                "ACWS", "Day1", "R02", "2020-12-17", "C", 6, "US", "UK", "US", "27'03", "5'00", 4247, NA,
                "ACWS", "Day1", "R03", "2020-12-17", "C", 6, "UK", "IT", "IT", "27'41", NA, NA, "UK RET (Mainsail)",
                "ACWS", "Day1", "R04", "2020-12-17", "C", 6, "US", "NZ", "US", "26'20", "0'12", 179, NA,
                "ACWS", "Day2", "R05", "2020-12-18", "C", 6, "IT", "US", "IT", "23'42", "0'15", 225, NA,
                "ACWS", "Day2", "R06", "2020-12-18", "C", 6, "NZ", "UK", "NZ", "21'09", "1'32", 1250, NA,
                "ACWS", "Day2", "R07", "2020-12-18", "C", 8, "US", "IT", "US", "28'48", "0'30", 402, NA,
                "ACWS", "Day2", "R08", "2020-12-18", "C", 6, "UK", "NZ", "NZ", "26'54", "1'42", 1374, NA,
                "ACWS", "Day3", "R09", "2020-12-19", "B", 4, "IT", "UK", "IT", "25'01", "3'45", 5541, NA,
                "ACWS", "Day3", "R10", "2020-12-19", "B", 5, "NZ", "US", "NZ", "25'42", "1'19", 1004, NA,
                "ACWS", "Day3", "R11", "2020-12-19", "B", 6, "UK", "US", "US", "27'12", "5'00", 2226, NA,
                "ACWS", "Day3", "R12", "2020-12-19", "B", 4, "IT", "NZ", "NZ", "28'38", "0'16", 141, NA,
                "PC", "RR1", "R01", "2021-01-15", "C", 6, "US", "UK", "UK", "23'13", "1'20", 1296, NA,
                "PC", "RR1", "R02", "2021-01-15", "C", 6, "UK", "IT", "UK", "24'44", "0'28", 431, NA,
                "PC", "RR1", "R03", "2021-01-16", "C", 4, "IT", "US", "IT", "38'54", NA, 3341, "US DNF",
                "PC", "RR2", "R01", "2021-01-16", "C", 4, "UK", "US", "UK", "42'44", "4'59", 2865, NA,
                "PC", "RR2", "R02", "2021-01-17", "A", 6, "IT", "UK", "UK", "27'35", "0'18", 228, "Re-Race",
                "PC", "RR2", "R03", "2021-01-17", "A", 6, "US", "IT", "IT", "22'24", NA, NA, "US DNF (Capsized)",
                "PC", "RR3", "R02", "2021-01-23", "C", 6, "IT", "UK", "UK", "24'33", "0'33", 200, NA,
                "PC", "RR3", "R01", "2021-01-23", "C", 6, "UK", "US", "UK", NA, NA, NA, "US DNS",
                "PC", "RR3", "R03", "2021-01-23", "C", 6, "US", "IT", "IT", NA, NA, NA, "US DNS",
                "PC", "RR4", "R02", "2021-01-24", NA, NA, "UK", "IT", NA, NA, NA, NA, "Cancelled",
                "PC", "RR4", "R01", "2021-01-24", NA, NA, "US", "UK", NA, NA, NA, NA, "Cancelled",
                "PC", "RR4", "R03", "2021-01-24", NA, NA, "IT", "US", NA, NA, NA, NA, "Cancelled",
                "PC", "SF", "R01", "2021-01-29", "C", 8, "US", "IT", "IT", "26'42", "2'43", 3044, NA, # best of 7 vvv of RR seeds 2 & 3
                "PC", "SF", "R02", "2021-01-29", "C", 8, "IT", "US", "IT", "26'57", "3'07", 3206, NA,
                "PC", "SF", "R03", "2021-01-30", "A", 6, "IT", "US", "IT", "26'08", "0'34", 549, NA,
                "PC", "SF", "R04", "2021-01-30", "A", 6, "US", "IT", "IT", "23'30", "3'51", 3478, NA,
                "PC", "SF", "R05", "2021-01-31", NA, NA, "US", "IT", NA, NA, NA, NA, "IT wins Series 4-0",
                "PC", "SF", "R06", "2021-01-31", NA, NA, "IT", "US", NA, NA, NA, NA, "IT wins Series 4-0",
                "PC", "SF", "R07", "2021-02-02", NA, NA, "US", "IT", NA, NA, NA, NA, "IT wins Series 4-0",
                "PC", "F", "R01", "2021-02-13", "A", 6, "UK", "IT", "IT", "26'27", "1'51", 1635, NA, # best of 13 vvv of RR & SF winners
                "PC", "F", "R02", "2021-02-13", "A", 6, "IT", "UK", "IT", "21'44", "0'26", 479, NA,
                "PC", "F", "R03", "2021-02-14", "E", 6, "IT", "UK", "IT", "25'40", "0'13", 186, NA,
                "PC", "F", "R04", "2021-02-14", "E", 6, "UK", "IT", "IT", "25'01", "0'41", 781, NA,
                "PC", "F", "R05", "2021-02-20", "E", 6, "UK", "IT", "IT", "28'19", "1'20", 1150, NA,
                "PC", "F", "R06", "2021-02-20", "E", 6, "IT", "UK", "UK", "26'20", "0'14", 162, NA,
                "PC", "F", "R07", "2021-02-21", "A", 6, "IT", "UK", "IT", "31'53", "1'45", 1335, NA,
                "PC", "F", "R08", "2021-02-21", "A", 6, "UK", "IT", "IT", "30'05", "0'56", 730, NA,
                "PC", "F", "R09", "2021-02-22", NA, NA, "UK", "IT", NA, NA, NA, NA, "IT wins Series 7-1",
                "PC", "F", "R10", "2021-02-22", NA, NA, "IT", "UK", NA, NA, NA, NA, "IT wins Series 7-1",
                "PC", "F", "R11", "2021-02-23", NA, NA, "IT", "UK", NA, NA, NA, NA, "IT wins Series 7-1",
                "PC", "F", "R12", "2021-02-23", NA, NA, "UK", "IT", NA, NA, NA, NA, "IT wins Series 7-1",
                "PC", "F", "R13", "2021-02-24", NA, NA, "UK", "IT", NA, NA, NA, NA, "IT wins Series 7-1",
                "AC", NA, "R01", "2021-03-10", "E", 6, "NZ", "IT", "NZ", "23'08", "0'31", 591, NA,  # best of 13 vvv of PC winner and defender
                "AC", NA, "R02", "2021-03-10", "E", 6, "IT", "NZ", "IT", "24'41", "0'07", 139, NA,
                "AC", NA, "R03", "2021-03-12", "E", 6, "IT", "NZ", "IT", "27'18", "0'37", 438, NA,
                "AC", NA, "R04", "2021-03-12", "E", 6, "NZ", "IT", "NZ", "29'53", "1'03", 676, NA,
                "AC", NA, "R05", "2021-03-13", "A", 6, "NZ", "IT", "IT", "29'04", "0'18", 197, NA,
                "AC", NA, "R06", "2021-03-13", "A", 6, "IT", "NZ", "NZ", "27'26", "1'41", 1405, NA,
                "AC", NA, "R07", "2021-03-15", "E", 6, "IT", "NZ", "NZ", "25'17", "0'58", 829, NA,
                "AC", NA, "R08", "2021-03-15", "E", 5, "NZ", "IT", "NZ", "38'57", "3'55", 1793, NA,
                "AC", NA, "R09", "2021-03-16", "C", 6, "NZ", "IT", "NZ", "25'29", "0'30", 483, NA,
                "AC", NA, "R10", "2021-03-17", "AB", 6, "IT", "NZ", "NZ", "26'08", "0'46", NA, NA,
                "AC", NA, "R11", "2021-03-17", NA, NA, "IT", "NZ", NA, NA, NA, NA, "NZ wins Series 7-3",
                "AC", NA, "R12", "2021-03-18", NA, NA, "NZ", "IT", NA, NA, NA, NA, "NZ wins Series 7-3",
                "AC", NA, "R13", "2021-03-18", NA, NA, "NZ", "IT", NA, NA, NA, NA, "NZ wins Series 7-3")

ac36 <- ac36 %>%
  mutate(Date = ymd(Date)) %>% # convert Date
  extract(Time, into = c("m", "s"), "(\\d+)'(\\d+)", convert = TRUE, remove = FALSE) %>% # convert Time to duration object
  mutate(Time = minutes(m) + seconds(s)) %>% 
  extract(Delta, into = c("m", "s"), "(\\d+)'(\\d+)", convert = TRUE, remove = FALSE) %>% # convert Delta to duration object
  mutate(Delta = minutes(m) + seconds(s)) %>% 
  select(-m, -s)

### Distinct Colors
cols <- RColorBrewer::brewer.pal(4, "Set1") %>%
  set_names(., ac36_teams$Team[c(3, 1, 2, 4)])

### Legend
ac36_teams %>%
  ggplot(mapping = aes(x = rep(NA_real_, 4), y = rep(NA_real_, 4))) +
  geom_point(mapping = aes(color = Team), size = 3, show.legend = TRUE) +
  scale_color_manual(name = "36th America's Cup Competitors", values = cols) +
  scale_y_continuous(limits = c(2, 2.1), expand = c(0, 0)) +
  guides(color = guide_legend(title.position = "top",
                              nrow = 2,
                              override.aes = list(size = 4))) +
  labs(title = " \n ") +
  theme_void() +
  theme(legend.position = "top")-> p0

### America's Cup World Series
ac36 %>%
  filter(Cup == "ACWS", !is.na(Winner)) %>%
  select(Round, Winner) %>%
  bind_cols(Pts = 1) %>% 
  group_by(Round, Winner) %>%
  summarise(Pts = sum(Pts), .groups = "drop_last") %>% 
  complete(Winner = ac36_teams$ID, fill = list(Pts = 0)) %>% 
  group_by(Winner) %>% 
  mutate(Pts = cumsum(Pts)) %>% 
  ungroup() -> tmp
tmp %>% 
  add_row(Round = " ", Winner = unique(pull(tmp, Winner)), Pts = 0, .before = 1) %>% 
  left_join(ac36_teams, by = c("Winner" = "ID")) %>% 
  ggplot(mapping = aes(x = Round, y = Pts, group = Team, color = Team)) +
  geom_line(size = 2.5, alpha = .9, show.legend = FALSE) +
  geom_point(size = 7.5, color = "white") +
  geom_point(size = 5, alpha = .5, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_discrete(name = "Standings after ...", expand = c(.025, .025)) +
  scale_y_continuous(name = "Points", minor_breaks = NULL) +
  labs(title = "2020 America's Cup World Series") +
  theme_minimal() -> p1

### Prada Cup Round Robin
ac36 %>%
  filter(Cup == "PC", !is.na(Winner), Round %in% paste0("RR", 1:4)) %>%
  select(Round, Winner) %>%
  bind_cols(Pts = 1) %>% 
  group_by(Round, Winner) %>%
  summarise(Pts = sum(Pts), .groups = "drop_last") %>% 
  complete(Winner =  c("IT", "UK", "US"), fill = list(Pts = 0)) %>% # Teams competing
  group_by(Winner) %>% 
  mutate(Pts = cumsum(Pts)) %>% 
  ungroup() -> tmp
tmp %>% 
  add_row(Round = " ", Winner = unique(pull(tmp, Winner)), Pts = 0, .before = 1) %>% 
  left_join(ac36_teams, by = c("Winner" = "ID")) %>% 
  ggplot(mapping = aes(x = Round, y = Pts, group = Team, color = Team)) +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_point(size = 7.5, color = "white") +
  geom_point(size = 5, alpha = .5, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_discrete(name = "Standings after ...", expand = c(.025, .025)) +
  scale_y_continuous(name = "Points", minor_breaks = NULL) +
  labs(title = "2021 Prada Cup Round Robin") +
  theme_minimal() -> p2

### Prada Cup Semi-final
ac36 %>%
  filter(Cup == "PC", !is.na(Winner), Round == "SF") %>%
  select(Race, Winner) %>%
  bind_cols(Pts = 1) %>% 
  group_by(Race, Winner) %>%
  summarise(Pts = sum(Pts), .groups = "drop_last") %>% 
  complete(Winner = c("IT", "US"), fill = list(Pts = 0)) %>% # Teams competing
  group_by(Winner) %>% 
  mutate(Pts = cumsum(Pts)) %>% 
  ungroup() -> tmp
tmp %>% 
  add_row(Race = " ", Winner = unique(pull(tmp, Winner)), Pts = 0, .before = 1) %>% 
  left_join(ac36_teams, by = c("Winner" = "ID")) %>% 
  ggplot(mapping = aes(x = Race, y = Pts, group = Team, color = Team)) +
  geom_hline(yintercept = 4, size = 2, color = "grey75", linetype = "dashed") +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_point(size = 7.5, color = "white") +
  geom_point(size = 5, alpha = .5, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_discrete(name = "Standings after ...", expand = c(.025, .025)) +
  scale_y_continuous(name = "Points", minor_breaks = NULL) +
  labs(title = "2021 Prada Cup Semi-final (best-of-7)") +
  theme_minimal() -> p3

### Prada Cup Finals
ac36 %>%
  filter(Cup == "PC", !is.na(Winner), Round == "F") %>%
  select(Race, Winner) %>%
  bind_cols(Pts = 1) %>%
  group_by(Race, Winner) %>%
  summarise(Pts = sum(Pts), .groups = "drop_last") %>%
  complete(Winner = c("UK", "IT"), fill = list(Pts = 0)) %>% # Teams competing
  group_by(Winner) %>%
  mutate(Pts = cumsum(Pts)) %>%
  ungroup() -> tmp
tmp %>% 
  add_row(Race = " ", Winner = unique(pull(tmp, Winner)), Pts = 0, .before = 1) %>%
  left_join(ac36_teams %>% filter(ID != "NZ"), by = c("Winner" = "ID")) %>%
  ggplot(mapping = aes(x = Race, y = Pts, group = Team, color = Team)) +
  geom_hline(yintercept = 7, size = 2, color = "grey75", linetype = "dashed") +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_point(size = 7.5, color = "white") +
  geom_point(size = 5, alpha = .5, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_discrete(name = "Standings after ...", expand = c(.025, .025)) +
  scale_y_continuous(name = "Points", minor_breaks = NULL) +
  labs(title = "2021 Prada Cup Final (best-of-13)") +
  theme_minimal() -> p4

### America's Cup
ac36 %>%
  filter(Cup == "AC", !is.na(Winner)) %>%
  select(Race, Winner) %>%
  bind_cols(Pts = 1) %>%
  group_by(Race, Winner) %>%
  summarise(Pts = sum(Pts), .groups = "drop_last") %>%
  complete(Winner = c("NZ", "IT"), fill = list(Pts = 0)) %>% # Teams competing
  group_by(Winner) %>%
  mutate(Pts = cumsum(Pts)) %>%
  ungroup() -> tmp
tmp %>%
  add_row(Race = " ", Winner = unique(pull(tmp, Winner)), Pts = 0, .before = 1) %>%
  left_join(ac36_teams, by = c("Winner" = "ID")) %>%
  ggplot(mapping = aes(x = Race, y = Pts, group = Team, color = Team)) +
  geom_hline(yintercept = 7, size = 2, color = "grey75", linetype = "dashed") +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_point(size = 7.5, color = "white") +
  geom_point(size = 5, alpha = .5, show.legend = FALSE) +
  scale_color_manual(values = cols) +
  scale_x_discrete(name = "Standings after ...", expand = c(.025, .025)) +
  scale_y_continuous(name = "Points", minor_breaks = NULL) +
  labs(title = "2021 America's Cup (best-of-13)") +
  theme_minimal() -> p5

windows(16, 9)
gridExtra::grid.arrange(grobs = list(p0, p1, p2, p3, p4, p5),
                        layout_matrix = rbind(c(2,2,2,3,3,3,4,4,4,4),
                                              c(2,2,2,3,3,3,4,4,4,4),
                                              c(2,2,2,3,3,3,4,4,4,4),
                                              c(5,5,5,5,5,6,6,6,6,6),
                                              c(5,5,5,5,5,6,6,6,6,6),
                                              c(5,5,5,5,5,6,6,6,6,6),
                                              c(1,1,1,1,1,1,1,1,1,1))
)

rm(p0, p1, p2, p3, p4, p5, tmp, cols)

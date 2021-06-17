# libraries and globals ----
require(tidyverse)
library(patchwork)

colors <- c("firebrick", "gold", "forestgreen", colorRampPalette(c("forestgreen", "#d0f0c0"))(8)) %>%  # penalty + own goal + regular + 8 sequential: RColorBrewer::brewer.pal(9, "YlGn")[9:2])
  set_names(c("Penalty", "Own Goal", "Regular", "(0,15]", "(15,30]", "(30,45]", "(45,60]", "(60,75]", "(75,90]", "(90,105]", "(105,120]"))

# data ----
## participating teams ----
teams <- tribble(~FIFA, ~Team,
                 'bel', 'Belgien',
                 'ita', 'Italien',
                 'pol', 'Polen',
                 'rus', 'Russland',
                 'ukr', 'Ukraine',
                 'esp', 'Spanien',
                 'eng', 'England',
                 'fra', 'Frankreich',
                 'cze', 'Tschechien',
                 'tur', 'Türkei',
                 'fin', 'Finnland',
                 'swe', 'Schweden',
                 'ger', 'Deutschland',
                 'cro', 'Kroatien',
                 'ned', 'Niederlande',
                 'aut', 'Österreich',
                 'por', 'Portugal',
                 'den', 'Dänemark',
                 'sui', 'Schweiz',
                 'wal', 'Wales',
                 'mkd', 'Nordmazedonien',
                 'hun', 'Ungarn',
                 'svk', 'Slowakei',
                 'sco', 'Schottland')

## groups ----
groups <- tribble(~Group, ~FIFA,
                  'A', c('tur', 'ita', 'wal', 'sui'),
                  'B', c('den', 'fin', 'bel', 'rus'),
                  'C', c('ned', 'ukr', 'aut', 'mkd'),
                  'D', c('eng', 'cro', 'sco', 'cze'),
                  'E', c('esp', 'swe', 'pol', 'svk'),
                  'F', c('hun', 'por', 'fra', 'ger'))

## locations ----
locations <- tribble(~City, ~Stadium, ~Game,
                     'Amsterdam', 'Johan Cruijff ArenA', c(7, 18, 27, 37),
                     'Baku', 'Bakı Milli Stadionu', c(2, 14, 25, 47),
                     'Budapest', 'Puskas Arena', c(11, 22, 35, 39),
                     'Bukarest', 'Arena Nationala', c(6, 16, 28, 42),
                     'Glasgow', 'Hampden Park', c(8, 20, 31, 44),
                     'Kopenhagen', 'Parken', c(3, 17, 29, 41),
                     'London', 'Wembley Stadium', c(5, 21, 32, 38, 43, 49, 50, 51),
                     'München', 'Allianz Arena', c(12, 23, 36, 46),
                     'Rom', 'Stadio Olimpico', c(1, 15, 26, 48),
                     'Sankt Petersburg', 'Krestowski-Stadion', c(4, 9, 13, 19, 30, 34, 45),
                     'Sevilla', 'Estadio Olimpico', c(10, 24, 33, 40))


## games ----
games <- tribble(~Game, ~Stage, ~Team_A, ~Goals_A, ~Team_B, ~Goals_B, ~Goals, # standard: 1, 'Group', NA, NA, NA, NA, NULL; meanings below in mutate
                 1, 'Group', 'tur', 0, 'ita', 3, c("ita 53 0 own ld", "ita 66 0 reg ex", "ita 79 0 reg ex"),
                 2, 'Group', 'wal', 1, 'sui', 1, c("sui 49 0 reg ld", "wal 74 0 reg os"),
                 3, 'Group', 'den', 0, 'fin', 1, "fin 60 0 reg ld",
                 4, 'Group', 'bel', 3, 'rus', 0, c("bel 10 0 reg ld", "bel 34 0 reg ex", "bel 88 0 reg ex"),
                 5, 'Group', 'eng', 1, 'cro', 0, "eng 57 0 reg ld",
                 6, 'Group', 'aut', 3, 'mkd', 1, c("aut 18 0 reg ld", "mkd 28 0 reg os", "aut 78 0 reg ld", "aut 89 0 reg ex"),
                 7, 'Group', 'ned', 3, 'ukr', 2, c("ned 52 0 reg ld", "ned 58 0 reg ex", "ukr 75 0 reg cu", "ukr 79 0 reg os", "ned 85 0 reg ld"),
                 8, 'Group', 'sco', 0, 'cze', 2, c("cze 42 0 reg ld", "cze 52 0 reg ex"),
                 9, 'Group', 'pol', 1, 'svk', 2, c("svk 18 0 own ld", "pol 46 0 reg os", "svk 69 0 reg ld"),
                 10, 'Group', 'esp', 0, 'swe', 0, NULL,
                 11, 'Group', 'hun', 0, 'por', 3, c("por 84 0 reg ld", "por 87 0 pen ex", "por 90 2 reg ex"),
                 12, 'Group', 'fra', 1, 'ger', 0, "fra 20 0 own ld",
                 13, 'Group', 'fin', 0, 'rus', 1, "rus 45 2 reg ld",
                 14, 'Group', 'tur', 0, 'wal', 1, "wal 42 0 reg ld",
                 15, 'Group', 'ita', NA, 'sui', NA, NULL,
                 16, 'Group', 'ukr', NA, 'mkd', NA, NULL,
                 17, 'Group', 'den', NA, 'bel', NA, NULL,
                 18, 'Group', 'ned', NA, 'aut', NA, NULL,
                 19, 'Group', 'swe', NA, 'svk', NA, NULL,
                 20, 'Group', 'cro', NA, 'cze', NA, NULL,
                 21, 'Group', 'eng', NA, 'sco', NA, NULL,
                 22, 'Group', 'hun', NA, 'fra', NA, NULL,
                 23, 'Group', 'por', NA, 'ger', NA, NULL,
                 24, 'Group', 'esp', NA, 'pol', NA, NULL,
                 25, 'Group', 'sui', NA, 'tur', NA, NULL,
                 26, 'Group', 'ita', NA, 'wal', NA, NULL,
                 27, 'Group', 'mkd', NA, 'ned', NA, NULL,
                 28, 'Group', 'ukr', NA, 'aut', NA, NULL,
                 29, 'Group', 'rus', NA, 'den', NA, NULL,
                 30, 'Group', 'fin', NA, 'bel', NA, NULL,
                 31, 'Group', 'cro', NA, 'sco', NA, NULL,
                 32, 'Group', 'cze', NA, 'eng', NA, NULL,
                 33, 'Group', 'svk', NA, 'esp', NA, NULL,
                 34, 'Group', 'swe', NA, 'pol', NA, NULL,
                 35, 'Group', 'por', NA, 'fra', NA, NULL,
                 36, 'Group', 'ger', NA, 'hun', NA, NULL,
                 37, 'Final', NA, NA, NA, NA, NULL,
                 38, 'Final', NA, NA, NA, NA, NULL,
                 39, 'Final', NA, NA, NA, NA, NULL,
                 40, 'Final', NA, NA, NA, NA, NULL,
                 41, 'Final', NA, NA, NA, NA, NULL,
                 42, 'Final', NA, NA, NA, NA, NULL,
                 43, 'Final', NA, NA, NA, NA, NULL,
                 44, 'Final', NA, NA, NA, NA, NULL,
                 45, 'Final', NA, NA, NA, NA, NULL,
                 46, 'Final', NA, NA, NA, NA, NULL,
                 47, 'Final', NA, NA, NA, NA, NULL,
                 48, 'Final', NA, NA, NA, NA, NULL,
                 49, 'Final', NA, NA, NA, NA, NULL,
                 50, 'Final', NA, NA, NA, NA, NULL,
                 51, 'Final', NA, NA, NA, NA, NULL) %>%
  mutate(across(c(Goals_A, Goals_B), ~as.integer(.)),
         Goals = map(Goals, ~ tibble(data = .) %>%
                       separate(data, into = c("FIFA", "Minute_raw", "OT", "Type", "Course"), sep = " ") %>% 
                       separate(Minute_raw, into = c("Minute", "OT"), fill = "right") %>%
                       mutate(across(Minute:OT, ~as.integer(.)), # needed for later cut - only works on numbers
                              Type = factor(Type, levels = c("pen", "own", "reg"), labels = c("Penalty", "Own Goal", "Regular")),
                              Course = factor(Course, levels = c("ld", "os", "ex", "cu"), labels = c("Lead", "Offset", "Extend", "Catch-up")),
                              Time = cut(Minute, seq(0, 120, by = 15), right = TRUE))
         ))

# derivates ----
## games played ----
games_played <- games %>%
  filter(!is.na(Goals_A))

## table ----
table <- games_played %>%
  filter(Stage == "Group") %>%
  transmute(FIFA = Team_A,
            Points = case_when(Goals_A > Goals_B ~ 3L,
                               Goals_A == Goals_B ~1L,
                               TRUE ~ 0L),
            Diff = Goals_A - Goals_B,
            Scored = Goals_A) %>%
  bind_rows(games_played %>%
              filter(Stage == "Group") %>%
              transmute(FIFA = Team_B,
                        Points = case_when(Goals_B > Goals_A ~ 3L,
                                           Goals_B == Goals_A ~1L,
                                           TRUE ~ 0L),
                        Diff = Goals_B - Goals_A,
                        Scored = Goals_B)) %>%
  left_join(groups %>% unnest(cols = FIFA), by = "FIFA") %>%
  group_by(Group, FIFA) %>%
  summarise(Games = n(),
            Points = sum(Points),
            Diff = sum(Diff),
            Scored = sum(Scored),
            .groups = "drop") %>%
  arrange(Group, -Points, -Diff, -Scored) %>%
  left_join(teams, by = "FIFA") 

group_split(table, Group)

## goals ----
goals <- games_played %>%
  select(Game, Stage, Goals) %>%
  unnest(cols = Goals) %>% 
  left_join(teams, by = "FIFA") %>%
  relocate(Team, .after = FIFA) %>% 
  left_join(groups %>% unnest(cols = FIFA), by = "FIFA") %>% 
  relocate(Group, .after = Stage) %>% 
  left_join(locations %>% select(City, Game) %>% unnest(cols = Game), by = "Game") %>% 
  mutate(City = factor(City, levels = locations$City))
# graphics ----
## page 1 ----
ggplot(goals, mapping = aes(x = 1)) +
  geom_bar(mapping = aes(fill = Type), show.legend = FALSE) +
  scale_x_discrete(expand = expansion(mult = c(.5, .01))) +
  scale_y_continuous(minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors[1:3]) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()) -> p1

ggplot(goals, mapping = aes(x = Time)) +
  geom_bar(mapping = aes(fill = Type)) +
  geom_bar(mapping = aes(x = "(0,15]", y = .01), stat = "identity", fill = NA) +    # make sure first and
  geom_bar(mapping = aes(x = "(105,120]", y = .01), stat = "identity", fill = NA) + # last bars are plotted
  scale_x_discrete(expand = c(.01, .01), drop = FALSE) +
  scale_y_continuous(name = "count", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors[1:3]) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed")) -> p2

windows(16, 9)
p1 + p2 +
  plot_annotation(title = "UEFA Euro 2020 - Goals") &
  plot_layout(guides = "collect") &
  theme(legend.position = "top") -> p
plot(p)
P1 <- p
rm(p1, p2, p)

## page 2 ----
ggplot(goals, mapping = aes(x = Time)) +
  geom_bar(mapping = aes(fill = Time), show.legend = FALSE) +
  geom_bar(mapping = aes(x = "(0,15]", y = .01), stat = "identity", fill = NA) +    # make sure first and
  geom_bar(mapping = aes(x = "(105,120]", y = .01), stat = "identity", fill = NA) + # last bars are plotted
  scale_x_discrete(expand = c(.01, .01), drop = FALSE) +
  scale_y_continuous(name = "count", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed"),
        axis.title.x = element_blank()) -> p1

ggplot(goals, mapping = aes(x = Minute)) +
  geom_boxplot(size = 1.5, color = colors["Regular"]) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 15), minor_breaks = seq(0, 120, by = 5), expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.1, .1)) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_line(linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> p2

ggplot(goals, mapping = aes(x = City)) +
  geom_bar(mapping = aes(fill = fct_rev(Time)), show.legend = FALSE) +
  scale_x_discrete(expand = c(.01, .01), guide = guide_axis(n.dodge = 2), drop = FALSE) +
  scale_y_continuous(breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed")) -> p3

windows(16, 9)
p1 / p2 + p3 +
  plot_layout(design = "AC\nAC\nAC\nAC\nAC\nAC\nBC") +
  plot_annotation(title = "UEFA Euro 2020 - Goals") -> p 
plot(p)
P2 <- p
rm(p1, p2, p3, p)


## page 3 ----
games_played %>% 
  transmute(Stage, FIFA = Team_A, For = Goals_A, Against = Goals_B) %>%
  bind_rows(games_played %>%
              transmute(Stage, FIFA = Team_B, For = Goals_B, Against = Goals_A)) %>%
  group_by(FIFA, Stage) %>%
  summarise(For = sum(For), Against = sum(Against), .groups = "drop") %>%
  mutate(Diff = For - Against) %>%
  arrange(-Diff, -For) %>%
  rowid_to_column(var = "Rank") %>% 
  ggplot(mapping = aes(x = reorder(FIFA, Rank, sum))) +
  geom_bar(mapping = aes(y = For), stat = "identity", fill = "forestgreen") +
  geom_bar(mapping = aes(y = -Against), stat = "identity", fill = "firebrick") +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -0.3, ymax = 0.3, fill = "white") +
  geom_point(mapping = aes(x = Rank, y = Diff), stroke = 2, shape = 4, color = "gold") +
  geom_text(mapping = aes(x = Rank, label = reorder(FIFA, Rank, sum)), y = 0, size = 3.5) +
  labs(title = "UEFA Euro 2020 - Group Stage", subtitle = "Goals 'for' and 'against' Team") +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(color = "black")) -> p
windows(16, 9)
plot(p)
P3 <- p     
rm(p)

## pdf ----
pdf("Fussball/Euro2020/Euro2020.pdf", paper = "a4r", width = 16, height = 9)
plot(P1)
plot(P2)
plot(P3)
dev.off()
rm(P1, P2, P3)

# clean up ----
rm(colors, games_played) # helpers
# rm(table, goals) # derivates
# rm(groups, locations, teams, games) # basic data

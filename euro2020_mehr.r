# libraries and globals ----
require(tidyverse)
library(patchwork)

colors <- c("firebrick", "gold", "forestgreen", colorRampPalette(c("forestgreen", "#d0f0c0"))(8)) %>%  # penalty + own goal + regular + 8 sequential: RColorBrewer::brewer.pal(9, "YlGn")[9:2])
  set_names(c("Penalty", "Own Goal", "Regular", "(0,15]", "(15,30]", "(30,45]", "(45,60]", "(60,75]", "(75,90]", "(90,105]", "(105,120]"))

# data ----
## Teilnehmer ----
teams <- tribble(~FIFA, ~Land,
                 "bel", "Belgien",
                 "ita", "Italien",
                 "pol", "Polen",
                 "rus", "Russland",
                 "ukr", "Ukraine",
                 "esp", "Spanien",
                 "eng", "England",
                 "fra", "Frankreich",
                 "cze", "Tschechien",
                 "tur", "Türkei",
                 "fin", "Finnland",
                 "swe", "Schweden",
                 "ger", "Deutschland",
                 "cro", "Kroatien",
                 "ned", "Niederlande",
                 "aut", "Österreich",
                 "por", "Portugal",
                 "den", "Dänemark",
                 "sui", "Schweiz",
                 "wal", "Wales",
                 "mkd", "Nordmazedonien",
                 "hun", "Ungarn",
                 "svk", "Slowakei",
                 "sco", "Schottland")

## Gruppen ----
groups <- tribble(~Gruppe, ~FIFA,
                  "A", c("tur", "ita", "wal", "sui"),
                  "B", c("den", "fin", "bel", "rus"),
                  "C", c("ned", "ukr", "aut", "mkd"),
                  "D", c("eng", "cro", "sco", "cze"),
                  "E", c("esp", "swe", "pol", "svk"),
                  "F", c("hun", "por", "fra", "ger"))

## Spielorte ----
locations <- tribble(~Stadt, ~Stadion, ~Spiel,
                     "Amsterdam", "Johan Cruijff ArenA", c(7, 18, 27, 37),
                     "Baku", "Bakı Milli Stadionu", c(2, 14, 25, 47),
                     "Budapest", "Puskas Arena", c(11, 22, 35, 39),
                     "Bukarest", "Arena Nationala", c(6, 16, 28, 42),
                     "Glasgow", "Hampden Park", c(8, 20, 31, 44),
                     "Kopenhagen", "Parken", c(3, 17, 29, 41),
                     "London", "Wembley Stadium", c(5, 21, 32, 38, 43, 49, 50, 51),
                     "München", "Allianz Arena", c(12, 23, 36, 46),
                     "Rom", "Stadio Olimpico", c(1, 15, 26, 48),
                     "Sankt Petersburg", "Krestowski-Stadion", c(4, 9, 13, 19, 30, 34, 45),
                     "Sevilla", "Estadio Olimpico", c(10, 24, 33, 40))


## Spiele ----
games <- tribble(~Spiel, ~Phase, ~Begegnung, ~Tore_H, ~Tore_G, ~Tore, # standard: 1, "Group", NA, NA, NA, NA, NULL; meanings below in mutate
                 01, "Gr T1", "tur ita", 0, 3, c("ita 53 own ld", "ita 66 reg ex", "ita 79 reg ex"),
                 02, "Gr T1", "wal sui", 1, 1, c("sui 49 reg ld", "wal 74 reg os"),
                 03, "Gr T1", "den fin", 0, 1, "fin 60 reg ld",
                 04, "Gr T1", "bel rus", 3, 0, c("bel 10 reg ld", "bel 34 reg ex", "bel 88 reg ex"),
                 05, "Gr T1", "eng cro", 1, 0, "eng 57 reg ld",
                 06, "Gr T1", "aut mkd", 3, 1, c("aut 18 reg ld", "mkd 28 reg os", "aut 78 reg ld", "aut 89 reg ex"),
                 07, "Gr T1", "ned ukr", 3, 2, c("ned 52 reg ld", "ned 58 reg ex", "ukr 75 reg cu", "ukr 79 reg os", "ned 85 reg ld"),
                 08, "Gr T1", "sco cze", 0, 2, c("cze 42 reg ld", "cze 52 reg ex"),
                 09, "Gr T1", "pol svk", 1, 2, c("svk 18 own ld", "pol 46 reg os", "svk 69 reg ld"),
                 10, "Gr T1", "esp swe", 0, 0, NULL,
                 11, "Gr T1", "hun por", 0, 3, c("por 84 reg ld", "por 87 pen ex", "por 90+2 reg ex"),
                 12, "Gr T1", "fra ger", 1, 0, "fra 20 own ld",
                 13, "Gr T2", "fin rus", 0, 1, "rus 45+2 reg ld",
                 14, "Gr T2", "tur wal", 0, 2, c("wal 42 reg ld", "wal 90+5 reg ex"),
                 15, "Gr T2", "ita sui", 3, 0, c("ita 26 reg ld", "ita 56 reg ex", "ita 89 reg ex"),
                 16, "Gr T2", "ukr mkd", 2, 1, c("ukr 29 reg ld", "ukr 34 reg ex", "mkd 57 reg cu"),
                 17, "Gr T2", "den bel", 1, 2, c("den 2 reg ld", "bel 54 reg os", "bel 70 reg ex"),
                 18, "Gr T2", "ned aut", 2, 0, c("ned 11 pen ld", "ned 67 reg ex"),
                 19, "Gr T2", "swe svk", NA, NA, NULL,
                 20, "Gr T2", "cro cze", NA, NA, NULL,
                 21, "Gr T2", "eng sco", NA, NA, NULL,
                 22, "Gr T2", "hun fra", NA, NA, NULL,
                 23, "Gr T2", "por ger", NA, NA, NULL,
                 24, "Gr T2", "esp pol", NA, NA, NULL,
                 25, "Gr T3", "sui tur", NA, NA, NULL,
                 26, "Gr T3", "ita wal", NA, NA, NULL,
                 27, "Gr T3", "mkd ned", NA, NA, NULL,
                 28, "Gr T3", "ukr aut", NA, NA, NULL,
                 29, "Gr T3", "rus den", NA, NA, NULL,
                 30, "Gr T3", "fin bel", NA, NA, NULL,
                 31, "Gr T3", "cro sco", NA, NA, NULL,
                 32, "Gr T3", "cze eng", NA, NA, NULL,
                 33, "Gr T3", "svk esp", NA, NA, NULL,
                 34, "Gr T3", "swe pol", NA, NA, NULL,
                 35, "Gr T3", "por fra", NA, NA, NULL,
                 36, "Gr T3", "ger hun", NA, NA, NULL,
                 37, "Fi F8", NA, NA, NA, NULL,
                 38, "Fi F8", NA, NA, NA, NULL,
                 39, "Fi F8", "ned ", NA, NA, NULL,
                 40, "Fi F8", NA, NA, NA, NULL,
                 41, "Fi F8", NA, NA, NA, NULL,
                 42, "Fi F8", NA, NA, NA, NULL,
                 43, "Fi F8", NA, NA, NA, NULL,
                 44, "Fi F8", NA, NA, NA, NULL,
                 45, "Fi F4", NA, NA, NA, NULL,
                 46, "Fi F4", NA, NA, NA, NULL,
                 47, "Fi F4", NA, NA, NA, NULL,
                 48, "Fi F4", NA, NA, NA, NULL,
                 49, "Fi F2", NA, NA, NA, NULL,
                 50, "Fi F2", NA, NA, NA, NULL,
                 51, "Fi Fi", NA, NA, NA, NULL) %>%
  separate(Phase, into = c("Runde", "Phase")) %>% 
  separate(Begegnung, into = c("Heim", "Gast")) %>% 
  mutate(Runde = factor(Runde, levels = c("Gr", "Fi"), labels = c("Vorrunde", "Finale")),
         Phase = factor(Phase, levels = c("T1", "T2", "T3", "F8", "F4", "F2", "Fi"),
                        labels = c("Spieltag 1", "Spieltag 2", "Spieltag 3", "Achtelfinale", "Viertelfinale", "Halbfinale", "Finale")),
         across(c(Tore_H, Tore_G), ~as.integer(.)),
         Tore = map(Tore, ~ tibble(data = .) %>%
                       separate(data, into = c("FIFA", "Minute_raw", "Type", "Course"), sep = " ") %>% 
                       separate(Minute_raw, into = c("Minute", "OT"), sep = "\\+", fill = "right") %>%
                       mutate(across(Minute:OT, ~parse_integer(.)), # needed for later cut - only works on numbers
                              Type = factor(Type, levels = c("pen", "own", "reg"), labels = c("Penalty", "Own Goal", "Regular")),
                              Course = factor(Course, levels = c("ld", "os", "ex", "cu"), labels = c("Lead", "Offset", "Extend", "Catch-up")),
                              Time = cut(Minute, seq(0, 120, by = 15), right = TRUE))
         ))

# derivates ----
## games played ----
games_played <- games %>%
  filter(!is.na(Tore_H))

## table ----
table <- games_played %>%
  filter(Runde == "Vorrunde") %>%
  transmute(FIFA = Heim,
            Points = case_when(Tore_H > Tore_G ~ 3L,
                               Tore_H == Tore_G ~1L,
                               TRUE ~ 0L),
            Diff = Tore_H - Tore_G,
            Scored = Tore_H) %>%
  bind_rows(games_played %>%
              filter(Runde == "Vorrunde") %>%
              transmute(FIFA = Gast,
                        Points = case_when(Tore_G > Tore_H ~ 3L,
                                           Tore_G == Tore_H ~1L,
                                           TRUE ~ 0L),
                        Diff = Tore_G - Tore_H,
                        Scored = Tore_G)) %>%
  left_join(groups %>% unnest(cols = FIFA), by = "FIFA") %>%
  group_by(Gruppe, FIFA) %>%
  summarise(Games = n(),
            Points = sum(Points),
            Diff = sum(Diff),
            Scored = sum(Scored),
            .groups = "drop") %>%
  arrange(Gruppe, -Points, -Diff, -Scored) %>%
  left_join(teams, by = "FIFA") 

group_split(table, Gruppe)

## goals ----
goals <- games_played %>%
  select(Spiel, Runde, Tore) %>%
  unnest(cols = Tore) %>% 
  left_join(teams, by = "FIFA") %>%
  relocate(Land, .after = FIFA) %>% 
  left_join(groups %>% unnest(cols = FIFA), by = "FIFA") %>% 
  relocate(Gruppe, .after = Runde) %>% 
  left_join(locations %>% select(Stadt, Spiel) %>% unnest(cols = Spiel), by = "Spiel") %>% 
  mutate(Stadt = factor(Stadt, levels = locations$Stadt))
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
  plot_annotation(title = "UEFA Euro 2020 - Tore") &
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

ggplot(goals, mapping = aes(x = Stadt)) +
  geom_bar(mapping = aes(fill = fct_rev(Time)), show.legend = FALSE) +
  scale_x_discrete(expand = c(.01, .01), guide = guide_axis(n.dodge = 2), drop = FALSE) +
  scale_y_continuous(breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed")) -> p3

windows(16, 9)
p1 / p2 + p3 +
  plot_layout(design = "AC\nAC\nAC\nAC\nAC\nAC\nBC") +
  plot_annotation(title = "UEFA Euro 2020 - Tore") -> p 
plot(p)
P2 <- p
rm(p1, p2, p3, p)


## page 3 ----
games_played %>% 
  transmute(Runde, FIFA = Heim, For = Tore_H, Against = Tore_G) %>%
  bind_rows(games_played %>%
              transmute(Runde, FIFA = Gast, For = Tore_G, Against = Tore_H)) %>%
  group_by(FIFA, Runde) %>%
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
  labs(title = "UEFA Euro 2020", subtitle = "Tore 'für' und 'gegen' das jeweilige Team") +
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
pdf("Fussball/Euro2020/Euro2020.pdf", paper = "a4r", width = 16, height = 9) #Fussball/Euro2020/
plot(P1)
plot(P2)
plot(P3)
dev.off()
rm(P1, P2, P3)

# clean up ----
rm(colors, games_played) # helpers
# rm(table, goals) # derivates
# rm(groups, locations, teams, games) # basic data

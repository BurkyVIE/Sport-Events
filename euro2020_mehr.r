# libraries and globals ----
require(tidyverse)
library(patchwork)

# colors <- c("forestgreen", "gold", "orangered", colorRampPalette(c("forestgreen", "#d0f0c0"))(8)) %>%  # penalty + own goal + regular + 8 sequential: RColorBrewer::brewer.pal(9, "YlGn")[9:2])
colors <- c("forestgreen", "gold", "orangered", colorRampPalette(c("#006300", "#b5ff2a"))(8)) %>%  # "#b5ff2a", "#f6ff04", "#ff9d00"
  set_names(c("aus dem Spiel", "Eigentor", "Elfmeter", "(0,15]", "(15,30]", "(30,45]", "(45,60]", "(60,75]", "(75,90]", "(90,105]", "(105,120]"))

# data ----
## Teilnehmer ----
teams <- tribble(~FIFA, ~Land,
                 "bel", "Belgien",
                 "ita", "Italien",
                 "eng", "England",
                 "ger", "Deutschland",
                 "esp", "Spanien",
                 "ukr", "Ukraine",
                 "fra", "Frankreich",
                 "pol", "Polen",
                 "sui", "Schweiz",
                 "cro", "Kroatien",
                 "ned", "Niederlande",
                 "rus", "Russland",
                 "por", "Portugal",
                 "tur", "Türkei",
                 "den", "Dänemark",
                 "aut", "Österreich",
                 "swe", "Schweden",
                 "cze", "Tschechien",
                 "wal", "Wales",
                 "fin", "Finnland",
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
games <- tribble(~Anpfiff, ~Phase, ~Begegnung, ~Tore_H, ~Tore_G, ~Tore, # standard: No, "Runde Phase", "Heim Gast", NA, NA, NULL; Abkürzungen in den 'mutate'-Statements
                 "11/6/21/21/0", "Gr T1", "tur ita", 0, 3, c("ita 53 own ld", "ita 66 reg ex", "ita 79 reg ex"),
                 "12/6/21/15/0", "Gr T1", "wal sui", 1, 1, c("sui 49 reg ld", "wal 74 reg os"),
                 "12/6/21/18/0", "Gr T1", "den fin", 0, 1, "fin 60 reg ld",
                 "12/6/21/21/0", "Gr T1", "bel rus", 3, 0, c("bel 10 reg ld", "bel 34 reg ex", "bel 88 reg ex"),
                 "13/6/21/15/0", "Gr T1", "eng cro", 1, 0, "eng 57 reg ld",
                 "13/6/21/18/0", "Gr T1", "aut mkd", 3, 1, c("aut 18 reg ld", "mkd 28 reg os", "aut 78 reg ld", "aut 89 reg ex"),
                 "13/6/21/21/0", "Gr T1", "ned ukr", 3, 2, c("ned 52 reg ld", "ned 58 reg ex", "ukr 75 reg cu", "ukr 79 reg os", "ned 85 reg ld"),
                 "14/6/21/15/0", "Gr T1", "sco cze", 0, 2, c("cze 42 reg ld", "cze 52 reg ex"),
                 "14/6/21/18/0", "Gr T1", "pol svk", 1, 2, c("svk 18 own ld", "pol 46 reg os", "svk 69 reg ld"),
                 "14/6/21/21/0", "Gr T1", "esp swe", 0, 0, NULL,
                 "15/6/21/18/0", "Gr T1", "hun por", 0, 3, c("por 84 reg ld", "por 87 pen ex", "por 90+2 reg ex"),
                 "15/6/21/21/0", "Gr T1", "fra ger", 1, 0, "fra 20 own ld",
                 "16/6/21/15/0", "Gr T2", "fin rus", 0, 1, "rus 45+2 reg ld",
                 "16/6/21/18/0", "Gr T2", "tur wal", 0, 2, c("wal 42 reg ld", "wal 90+5 reg ex"),
                 "16/6/21/21/0", "Gr T2", "ita sui", 3, 0, c("ita 26 reg ld", "ita 56 reg ex", "ita 89 reg ex"),
                 "17/6/21/15/0", "Gr T2", "ukr mkd", 2, 1, c("ukr 29 reg ld", "ukr 34 reg ex", "mkd 57 reg cu"),
                 "17/6/21/18/0", "Gr T2", "den bel", 1, 2, c("den 2 reg ld", "bel 54 reg os", "bel 70 reg ld"),
                 "17/6/21/21/0", "Gr T2", "ned aut", 2, 0, c("ned 11 pen ld", "ned 67 reg ex"),
                 "18/6/21/15/0", "Gr T2", "swe svk", 1 ,0, "swe 77 pen ld",
                 "18/6/21/18/0", "Gr T2", "cro cze", 1 ,1 ,c("cze 37 pen ld", "cro 47 reg os"),
                 "18/6/21/21/0", "Gr T2", "eng sco", 0, 0, NULL,
                 "19/6/21/15/0", "Gr T2", "hun fra", 1, 1, c("hun 45+2 reg ld", "fra 66 reg os"),
                 "19/6/21/18/0", "Gr T2", "por ger", 2, 4, c("por 15 reg ld", "ger 35 pen os", "ger 39 pen ld", "ger 51 reg ex", "ger 60 reg ex", "por 67 reg cu"),
                 "19/6/21/21/0", "Gr T2", "esp pol", 1, 1, c("esp 25 reg ld", "pol 54 reg os"),
                 "20/6/21/18/0", "Gr T3", "sui tur", 3, 1, c("sui 6 reg ld", "sui 26 reg ex", "tur 62 reg cu", "sui 68 reg ex"),
                 "20/6/21/18/0", "Gr T3", "ita wal", 1, 0, "ita 39 reg ld",
                 "21/6/21/18/0", "Gr T3", "mkd ned", 0, 3, c("ned 24 reg ld", "ned 51 reg ex", "ned 58 reg ex"),
                 "21/6/21/18/0", "Gr T3", "ukr aut", 0, 1, "aut 21 reg ld",
                 "21/6/21/21/0", "Gr T3", "rus den", 1, 4, c("den 38 reg ld", "den 59 reg ex", "rus 70 pen cu", "den 79 reg ex", "den 82 reg ex"),
                 "21/6/21/21/0", "Gr T3", "fin bel", 0, 2, c("bel 74 own ld", "bel 81 reg ex"),
                 "22/6/21/21/0", "Gr T3", "cro sco", 3, 1, c("cro 17 reg ld", "sco 42 reg os", "cro 62 reg ld", "cro 77 reg ex"),
                 "22/6/21/21/0", "Gr T3", "cze eng", 0, 1, "eng 12 reg ld",
                 "23/6/21/18/0", "Gr T3", "svk esp", 0, 5, c("esp 30 own ld", "esp 45+3 reg ex", "esp 56 reg ex", "esp 67 reg ex", "esp 71 own ex"),
                 "23/6/21/18/0", "Gr T3", "swe pol", 3, 2, c("swe 2 reg ld", "swe 59 reg ex", "pol 61 reg cu", "pol 84 reg os", "swe 90+3 reg ld"),
                 "23/6/21/21/0", "Gr T3", "por fra", 2, 2, c("por 30 pen ld", "fra 45+2 pen os", "fra 47 reg ld", "por 60 pen os"),
                 "23/6/21/21/0", "Gr T3", "ger hun", 2, 2, c("hun 11 reg ld", "ger 66 reg os", "hun 68 reg ld", "ger 84 reg os"),
                 "26/6/21/18/0", "Fi F8", "wal den", 0, 4, c("den 27 reg ld", "den 48 reg ex", "den 88 reg ex", "den 90+4 reg ex"),
                 "26/6/21/21/0", "Fi F8", "ita aut", 2, 1, c("ita 95 reg ld", "ita 105 reg ex", "aut 114 reg cu"),
                 "27/6/21/18/0", "Fi F8", "ned cze", 0, 2, c("cze 68 reg ld", "cze 80 reg ex"),
                 "27/6/21/21/0", "Fi F8", "bel por", 1, 0, "bel 42 reg ld",
                 "28/6/21/18/0", "Fi F8", "cro esp", 3, 5, c("cro 20 own ld", "esp 38 reg os","esp 57 reg ld", "esp 76 reg ex", "cro 85 reg cu", "cro 90+2 reg os", "esp 100 reg ld", "esp 103 reg ex"),
                 "28/6/21/21/0", "Fi F8", "fra sui", 3, 3, c("sui 15 reg ld", "fra 57 reg os", "fra 59 reg ld", "fra 75 reg ex", "sui 68 reg cu", "fra 90 reg os", "sui NA pen ld", "fra NA pen os", "sui NA pen ld", "fra NA pen os", "sui NA pen ld", "fra NA pen os", "sui NA pen ld", "fra NA pen os", "sui NA pen ld"),
                 "29/6/21/18/0", "Fi F8", "eng ger", NA, NA, NULL,
                 "29/6/21/21/0", "Fi F8", "swe ukr", NA, NA, NULL,
                 "2/7/21/18/0", "Fi F4", "esp sui", NA, NA, NULL,
                 "2/7/21/21/0", "Fi F4", "bel ita", NA, NA, NULL,
                 "3/7/21/18/0", "Fi F4", "cze den", NA, NA, NULL,
                 "3/7/21/21/0", "Fi F4", NA, NA, NA, NULL,
                 "6/7/21/21/0", "Fi F2", NA, NA, NA, NULL,
                 "7/7/21/21/0", "Fi F2", NA, NA, NA, NULL,
                 "11/7/21/21/0", "Fi Fi", NA, NA, NA, NULL) %>%
  separate(Phase, into = c("Runde", "Phase")) %>% 
  separate(Begegnung, into = c("Heim", "Gast")) %>% 
  mutate(Anpfiff = lubridate::dmy_hm(Anpfiff, tz = "Europe/Vienna"),
         Runde = factor(Runde, levels = c("Gr", "Fi"), labels = c("Vorrunde", "Finale")),
         Phase = factor(Phase, levels = c("T1", "T2", "T3", "F8", "F4", "F2", "Fi"),
                        labels = c("1. Spieltag", "2. Spieltag", "3. Spieltag", "Achtelfinale", "Viertelfinale", "Halbfinale", "Finale")),
         across(c(Tore_H, Tore_G), ~as.integer(.)),
         Tore = map(Tore, ~ tibble(data = .) %>%
                       separate(data, into = c("FIFA", "Minute_roh", "Typ", "Verlauf"), sep = " ") %>% 
                       separate(Minute_roh, into = c("Minute", "OT"), sep = "\\+", fill = "right") %>%
                       mutate(across(Minute:OT, ~parse_integer(.)), # needed for later cut - only works on numbers
                              Typ = factor(Typ, levels = c("reg", "own", "pen"), labels = c("aus dem Spiel", "Eigentor", "Elfmeter")),
                              Verlauf = factor(Verlauf, levels = c("ld", "ex", "cu", "os"), labels = c("Führung", "Ausbau", "Anschluss", "Ausgleich")),
                              Zeit = cut(Minute, seq(0, 120, by = 15), right = TRUE)))) %>% 
  arrange(Anpfiff) %>% 
  rowid_to_column(var = "Spiel")

# Bestimme Gewinner; berücksichtigt Elferschießen
games <- games %>% 
  unnest(cols = Tore) %>%
  filter(Verlauf %in% c("Führung", "Ausgleich")) %>%
  group_by(Spiel) %>%
  summarise(Sieger = last(FIFA), he = last(Verlauf)) %>%
  mutate(Sieger = case_when(he ==  "Ausgleich" ~ NA_character_,
                            TRUE ~ Sieger)) %>%
  select(-he) %>%
  right_join(games, by = "Spiel") %>% 
  relocate(Sieger, .after = Tore) %>% 
  rowwise() %>% mutate(Ergebnis = paste(sort(c(Tore_H, Tore_G), decreasing = TRUE), collapse = ":"))

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
  select(Spiel, Runde, Phase, Tore) %>%
  unnest(cols = Tore) %>%  
  filter(!is.na(Minute)) %>% # keine Tore aus Elferschießen
  left_join(teams, by = "FIFA") %>%
  relocate(Land, .after = FIFA) %>% 
  left_join(groups %>% unnest(cols = FIFA), by = "FIFA") %>% 
  relocate(Gruppe, .after = Runde) %>% 
  left_join(locations %>% select(Stadt, Spiel) %>% unnest(cols = Spiel), by = "Spiel") %>% 
  mutate(Stadt = factor(Stadt, levels = locations$Stadt))

# graphics ----
## page 1 ----
ggplot(goals, mapping = aes(x = 1)) +
  geom_bar(mapping = aes(fill = fct_rev(Typ)), show.legend = FALSE) +
  scale_x_discrete(expand = expansion(mult = c(.5, .01))) +
  scale_y_continuous(breaks = function(x) seq(0, x[2], by = 10), minor_breaks = function(x) seq(0, x[2], by = 5)) +
  scale_fill_manual(name = "", values = colors[1:3]) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank()) -> p1

ggplot(goals, mapping = aes(x = Zeit)) +
  geom_bar(mapping = aes(fill = fct_rev(Typ))) +
  geom_bar(mapping = aes(x = "(0,15]", y = .01), stat = "identity", fill = NA) +    # Sicherstellen das erster und
  geom_bar(mapping = aes(x = "(105,120]", y = .01), stat = "identity", fill = NA) + # letzter 'Bar' dargestellt werden
  scale_x_discrete(expand = c(.01, .01), drop = FALSE) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
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
ggplot(goals, mapping = aes(x = Zeit)) +
  geom_bar(mapping = aes(fill = Zeit), show.legend = FALSE) +
  geom_bar(mapping = aes(x = "(0,15]", y = .01), stat = "identity", fill = NA) +    # make sure first and
  geom_bar(mapping = aes(x = "(105,120]", y = .01), stat = "identity", fill = NA) + # last bars are plotted
  scale_x_discrete(expand = c(.01, .01), drop = FALSE) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed"),
        axis.title.x = element_blank()) -> p1

ggplot(goals, mapping = aes(x = Minute)) +
  geom_boxplot(size = 1.5, color = "black") +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 15), minor_breaks = seq(0, 120, by = 5), expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.1, .1)) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_line(linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> p2

ggplot(goals, mapping = aes(x = Stadt)) +
  geom_bar(mapping = aes(fill = fct_rev(Zeit)), show.legend = FALSE) +
  scale_x_discrete(expand = c(.01, .01), guide = guide_axis(n.dodge = 2), drop = FALSE) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_line(linetype = "dashed")) -> p3

windows(16, 9)
p1 / p2 + p3 +
  plot_layout(design = "AC\nAC\nAC\nAC\nAC\nAC\nBC") +
  plot_annotation(title = "UEFA Euro 2020 - Tore nach Zeit bzw. Ort") -> p 
plot(p)
P2 <- p
rm(p1, p2, p3, p)

## page 3 ----
goals %>%
  filter(Runde == "Vorrunde") %>% 
  mutate(Gruppe = paste("Gruppe", Gruppe)) %>% 
  ggplot(mapping = aes(x = Zeit)) +
  geom_bar(mapping = aes(fill = Zeit), show.legend = FALSE) +
  scale_x_discrete(expand = c(.01, .01)) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  facet_wrap(~Gruppe) +
  labs(title = "UEFA Euro 2020 - Tore in der Vorrunde") +
  theme_minimal()  +
  theme(panel.spacing.x = unit(3, "lines")) -> p
windows(16, 9)
plot(p)
P3 <- p     
rm(p)

## page 4 ----
goals %>%
  mutate(Gruppe = paste("Gruppe", Gruppe)) %>% 
  ggplot(mapping = aes(x = Zeit)) +
  geom_bar(mapping = aes(fill = Zeit), show.legend = FALSE) +
  geom_bar(mapping = aes(x = "(0,15]", y = .01), stat = "identity", fill = NA) +    # Sicherstellen das erster und
  geom_bar(mapping = aes(x = "(105,120]", y = .01), stat = "identity", fill = NA) + # letzter 'Bar' dargestellt werden
  scale_x_discrete(expand = c(.01, .01), drop = FALSE, guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(0, x[2], by = 5), minor_breaks = function(x) seq(0, x[2], by = 2)) +
  scale_fill_manual(name = "", values = colors) +
  facet_wrap(~Runde + Verlauf, ncol = 4, drop = FALSE) +
  labs(title = "UEFA Euro 2020 - Tore nach Verlauf") +
  theme_minimal()  +
  theme(panel.spacing.x = unit(2, "lines")) -> p
windows(16, 9)
plot(p)
P4 <- p     
rm(p)

## page 5 ----
games_played %>% 
  transmute(Runde, FIFA = Heim, erzielt = Tore_H, kassiert = Tore_G) %>%
  bind_rows(games_played %>%
              transmute(Runde, FIFA = Gast, erzielt = Tore_G, kassiert = Tore_H)) %>%
  group_by(FIFA, Runde) %>%
  summarise(erzielt = sum(erzielt), kassiert = sum(kassiert), .groups = "drop") %>%
  pivot_wider(names_from = Runde, values_from = c(erzielt, kassiert), names_sep = "-") %>% 
  mutate(across(starts_with("kassiert"), ~. * -1),
         across(!FIFA, ~coalesce(., 0))) %>% 
  rowwise() %>% 
  mutate(Diff = sum(c_across(!FIFA)),
         erzielt = sum(c_across(starts_with("erzielt")))) %>% 
  pivot_longer(-c(FIFA, Diff, erzielt), names_to = "Tore", values_to = "Anzahl") -> he

ggplot(data = he, mapping = aes(x = reorder(reorder(FIFA, -erzielt, sum), -Diff, sum), y = Anzahl)) +
  geom_col(mapping = aes(fill = Tore)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -0.3, ymax = 0.3, fill = "white") +
  geom_point(mapping = aes(x = FIFA, y = Diff), stroke = 2, shape = 4, color = "gold", show.legend = FALSE) +
  geom_text(data = he %>% select(FIFA:erzielt) %>% unique(), mapping = aes(x = FIFA, label = reorder(reorder(FIFA, -erzielt, sum), -Diff, sum), y = 0), size = 3.5) +
  scale_fill_manual(values = c("erzielt-Finale" = colorspace::lighten("forestgreen"), "erzielt-Vorrunde" = "forestgreen",
                               "kassiert-Finale" = colorspace::lighten("firebrick"), "kassiert-Vorrunde" = "firebrick")) +
  scale_y_continuous(name = "Tore", breaks = function(x) seq(-20, x[2], by = 5), minor_breaks = function(x) seq(-20, x[2], by = 2)) +
  labs(title = "UEFA Euro 2020", subtitle = "Tore 'für' und 'gegen' das jeweilige Team") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom") -> p
windows(16, 9)
plot(p)
P5 <- p     
rm(he, p)

## pdf ----
pdf("Fussball/Euro2020/Euro2020.pdf", paper = "a4r", width = 16, height = 9) #Fussball/Euro2020/
plot(P1)
plot(P2)
plot(P3)
plot(P4)
plot(P5)
dev.off()
rm(P1, P2, P3, P4, P5)

# clean up ----
rm(colors, games_played) # helpers
# rm(table, goals) # derivates
# rm(groups, locations, teams, games) # basic data

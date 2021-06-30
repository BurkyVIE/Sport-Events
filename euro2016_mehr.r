# libraries and globals ----
require(tidyverse)
library(patchwork)

# colors <- c("forestgreen", "gold", "orangered", colorRampPalette(c("forestgreen", "#d0f0c0"))(8)) %>%  # penalty + own goal + regular + 8 sequential: RColorBrewer::brewer.pal(9, "YlGn")[9:2])
colors <- c("forestgreen", "gold", "orangered", colorRampPalette(c("#006300", "#b5ff2a"))(8)) %>%  # "#b5ff2a", "#f6ff04", "#ff9d00"
  set_names(c("aus dem Spiel", "Eigentor", "Elfmeter", "(0,15]", "(15,30]", "(30,45]", "(45,60]", "(60,75]", "(75,90]", "(90,105]", "(105,120]"))

# data ----
## Teilnehmer ----
teams <- tribble(~FIFA, ~Land,
                 "ger", "Deutschland",
                 "esp", "Spanien",
                 "eng", "England",
                 "por", "Portugal",
                 "bel", "Belgien",
                 "ita", "Italien",
                 "fra", "Frankreich",
                 "rus", "Russland",
                 "sui", "Schweiz",
                 "aut", "Österreich",
                 "cro", "Kroatien",
                 "ukr", "Ukraine",
                 "cze", "Tschechien",
                 "swe", "Schweden",
                 "pol", "Polen",
                 "rou", "Rumänien",
                 "svk", "Slowakei",
                 "hun", "Ungarn",
                 "tur", "Türkei",
                 "irl", "Irland",
                 "isl", "Island",
                 "wal", "Wales",
                 "alb", "Albanien",
                 "nir", "Nordirland")

## Gruppen ----
groups <- tribble(~Gruppe, ~FIFA,
                  "A", c("fra", "rou", "alb", "sui"),
                  "B", c("eng", "rus", "wal", "svk"),
                  "C", c("ger", "ukr", "pol", "nir"),
                  "D", c("esp", "cze", "tur", "cro"),
                  "E", c("bel", "ita", "irl", "swe"),
                  "F", c("por", "isl", "aut", "hun"))

## Spielorte ----
locations <- tribble(~Stadt, ~Stadion, ~Spiel,
                     "Saint-Denis", "Stade de France", c(1, 9, 18, 33, 43, 48),
                     "Marseille", "Stade Vélodrome", c(4, 15, 23, 29, 45, 50),
                     "Décines-Charpieu", "Parc Olympique Lyonnais", c(10, 17, 25, 34, 40, 49),
                     "Villeneuve-d'Ascq", "Stade Pierre-Mauroy", c(7, 13, 26, 35, 41, 46),
                     "Paris", "Parc des Princes", c(5, 14, 24, 30, 38, 51),
                     "Bordeaux", "Nouveau Stade de Bordeaux", c(3, 11, 22, 32, 47),
                     "Saint-Étienne", "Stade Geoffroy-Guichard", c(12, 20, 28, 37),
                     "Lens", "Stade Bollaert-Delelis", c(2, 16, 31, 39),
                     "Nice", "Stade de Nice", c(6, 21, 36, 44),
                     "Toulouse", "Stadium Municipal", c(8, 19, 27, 42))


## Spiele ----
games <- tribble(~Anpfiff, ~Phase, ~Begegnung, ~Tore_H, ~Tore_G, ~Tore, # standard: Start, "Runde Phase", "Heim Gast", NA, NA, NULL; Abkürzungen in den 'mutate'-Statements
                 "2016/6/10/21/0", "Gr T1", "fra rou", 2, 1, c("fra 57 reg ld", "rou 65 pen os", "fra 89 reg ld"),
                 "2016/6/11/15/0", "Gr T1", "alb sui", 0, 1, "sui 5 reg ld",
                 "2016/6/11/18/0", "Gr T1", "wal svk", 2, 1, c("wal 10 reg ld", "svk 61 reg os", "wal 81 reg ld"),
                 "2016/6/11/21/0", "Gr T1", "eng rus", 1, 1, c("eng 73 reg ld", "rus 90+2 reg os"),
                 "2016/6/12/15/0", "Gr T1", "tur cro", 0, 1, "cro 41 reg ld",
                 "2016/6/12/18/0", "Gr T1", "pol nir", 1, 0, "pol 51 reg ld",
                 "2016/6/12/21/0", "Gr T1", "ger ukr", 2, 0, c("ger 19 reg ld", "ger 90+2 reg ex"),
                 "2016/6/13/15/0", "Gr T1", "esp cze", 1, 0, "esp 87 reg ld",
                 "2016/6/13/18/0", "Gr T1", "irl swe", 1, 1, c("irl 48 reg ld", "swe 71 own os"),
                 "2016/6/13/21/0", "Gr T1", "bel ita", 0, 2, c("ita 32 reg ld", "ita 90+3 reg ex"),
                 "2016/6/14/18/0", "Gr T1", "aut hun", 0, 2, c("hun 62 reg ld", "hun 87 reg ex"),
                 "2016/6/14/21/0", "Gr T1", "por isl", 1, 1, c("por 31 reg ld", "isl 50 reg os"),
                 "2016/6/15/15/0", "Gr T2", "rus svk", 1, 2, c("svk 32 reg ld", "svk 45 reg ex", "rus 80 reg cu"),
                 "2016/6/15/18/0", "Gr T2", "rou sui", 1, 1, c("rou 18 pen ld", "sui 57 reg os"),
                 "2016/6/15/21/0", "Gr T2", "fra alb", 2, 0, c("fra 90 reg ld", "fra 90+6 reg ex"),
                 "2016/6/16/15/0", "Gr T2", "eng wal", 2, 1, c("wal 42 reg ld", "eng 56 reg os", "eng 90+2 reg ld"),
                 "2016/6/16/18/0", "Gr T2", "ukr nir", 0, 2, c("nir 49 reg ld", "nir 90+6 reg ex"),
                 "2016/6/16/21/0", "Gr T2", "ger pol", 0, 0, NULL,
                 "2016/6/17/15/0", "Gr T2", "ita swe", 1, 0, "ita 88 reg ld",
                 "2016/6/17/18/0", "Gr T2", "cze cro", 2, 2 ,c("cro 37 reg ld", "cro 59 reg ex", "cze 76 reg cu", "cze 89 pen os"),
                 "2016/6/17/21/0", "Gr T2", "esp tur", 3, 0, c("esp 34 reg ld", "esp 37 reg ex", "esp 48 reg ex"),
                 "2016/6/18/15/0", "Gr T2", "bel irl", 3, 0, c("bel 48 reg ld", "bel 61 reg ex", "bel 70 reg ex"),
                 "2016/6/18/18/0", "Gr T2", "isl hun", 1, 1, c("isl 40 own ld", "hun 88 own os"),
                 "2016/6/18/21/0", "Gr T2", "por aut", 0, 0, NULL,
                 "2016/6/19/21/0", "Gr T3", "rou alb", 0, 1, "alb 43 reg ld",
                 "2016/6/19/21/0", "Gr T3", "sui fra", 0, 0, NULL,
                 "2016/6/20/21/0", "Gr T3", "rus wal", 0, 3, c("wal 11 reg ld", "wal 20 reg ex", "wal 67 reg ex"),
                 "2016/6/20/21/0", "Gr T3", "svk eng", 0, 0, NULL,
                 "2016/6/21/18/0", "Gr T3", "ukr pol", 0, 1, "pol 54 reg ld",
                 "2016/6/21/18/0", "Gr T3", "nir ger", 0, 1, "ger 30 reg ld",
                 "2016/6/21/21/0", "Gr T3", "cze tur", 0, 2, c("tur 10 reg ld", "tur 65 reg ex"),
                 "2016/6/21/21/0", "Gr T3", "cro esp", 2, 1, c("esp 7 reg ld", "cro 45 reg os", "cro 87 reg ld"),
                 "2016/6/22/18/0", "Gr T3", "isl aut", 2, 1, c("isl 18 reg ld", "aut 60 reg os", "isl 90+4 reg ld"),
                 "2016/6/22/18/0", "Gr T3", "hun por", 3, 3, c("hun 19 reg ld", "por 42 reg os", "hun 47 reg ld", "por 50 reg os", "hun 55 reg ld", "por 62 reg os"),
                 "2016/6/22/21/0", "Gr T3", "ita irl", 0, 1, "irl 85 reg ld",
                 "2016/6/22/21/0", "Gr T3", "swe bel", 0, 1, "bel 84 reg ld",
                 "2016/6/25/15/0", "Fi F8", "sui pol", 1, 1, c("pol 39 reg ld", "sui 82 reg os", "sui NA pen ld", "pol NA pen os", "pol NA pen ld", "sui NA pen os", "pol NA pen ld", "sui NA pen os", "pol NA pen ld", "sui NA pen os", "pol NA pen ld"),
                 "2016/6/25/18/0", "Fi F8", "wal nir", 1, 0, "wal 75 own ld",
                 "2016/6/25/21/0", "Fi F8", "cro por", 0, 1, "por 117 reg ld",
                 "2016/6/26/15/0", "Fi F8", "fra irl", 2, 1, c("irl 2 pen ld", "fra 58 reg os", "fra 61 reg ld"),
                 "2016/6/26/18/0", "Fi F8", "ger svk", 3, 0, c("ger 8 reg ld", "ger 43 reg ex", "ger 63 reg ex"),
                 "2016/6/26/21/0", "Fi F8", "hun bel", 0, 4, c("bel 10 reg ld", "bel 78 reg ex", "bel 80 reg ex", "bel 90+1 reg ex"),
                 "2016/6/27/18/0", "Fi F8", "ita esp", 2, 0, c("ita 33 reg ld", "ita 90+1 reg ex"),
                 "2016/6/27/21/0", "Fi F8", "eng isl", 1, 2, c("eng 4 pen ld", "isl 6 reg ld", "isl 18 reg ex"),
                 "2016/6/30/21/0", "Fi F4", "pol por", 1, 1, c("pol 2 reg ld", "por 33 reg os", "por NA pen ld", "pol NA pen os", "por NA pen ld", "pol NA pen os", "por NA pen ld", "pol NA pen os", "por NA pen ld", "por NA pen ex"),
                 "2016/7/1/21/0", "Fi F4", "wal bel", 3, 1, c("bel 13 reg ld", "wal 31 reg os", "wal 55 reg ld", "wal 86 reg ex"),
                 "2016/7/2/21/0", "Fi F4", "ger ita", 1, 1, c("ger 65 reg ld", "ita 78 pen os", "ita NA pen ld", "ger NA pen os", "ita NA pen ld", "ger NA pen os", "ita NA pen ld", "ger NA pen os", "ita NA pen ld", "ger NA pen os", "ita NA pen ld", "ger NA pen os", "ger NA pen ld"),
                 "2016/7/3/21/0", "Fi F4", "fra isl", 5, 2, c("fra 12 reg ld", "fra 20 reg ex", "fra 43 reg ex", "fra 45 reg ex", "isl 56 reg cu", "fra 59 reg ex", "isl 84 reg cu"),
                 "2016/7/6/21/0", "Fi F2", "por wal", 0, 2, c("por 50 reg ld", "por 53 reg ex"),
                 "2016/7/7/21/0", "Fi F2", "ger fra", 0, 2, c("fra 45+2 pen ld", "fra 72 reg ex"),
                 "2016/7/10/21/0", "Fi Fi", "por fra", 1, 0, "por 109 reg ld") %>%
  separate(Phase, into = c("Runde", "Phase")) %>% 
  separate(Begegnung, into = c("Heim", "Gast")) %>% 
  mutate(Anpfiff = lubridate::ymd_hm(Anpfiff, tz = "Europe/Vienna"),
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
  rowwise() %>% mutate(Ergebnis = paste(sort(c(Tore_H, Tore_G), decreasing = TRUE), collapse = ":"),
                       Tore_ges = Tore_H + Tore_G)

# derivates ----
## games played ----
games_played <- games %>%
  filter(!is.na(Tore_H)) # nur Partien die bereits gespielt wurden

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
  plot_annotation(title = "UEFA Euro 2016 - Tore") &
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
  plot_annotation(title = "UEFA Euro 2016 - Tore nach Zeit bzw. Ort") -> p 
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
  labs(title = "UEFA Euro 2016 - Tore in der Vorrunde") +
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
  labs(title = "UEFA Euro 2016 - Tore nach Verlauf") +
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
  labs(title = "UEFA Euro 2016", subtitle = "Tore 'für' und 'gegen' das jeweilige Team") +
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
pdf("Euro2016.pdf", paper = "a4r", width = 16, height = 9) #Fussball/Euro2020/
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
library(tidyverse)
library(readxl)
## Read in Skater Stats

data <- read_csv("skater_stats_2019-01-08.csv")

## Create draft list of skaters, with metric TKA / 60, and merge with list of protected players to create dataset of all available players

x <- data
  select(Player, Team, Position, TOI, `iGVA/60`, `iTKA/60`)

y <- read_excel("draft.xlsx")

y <- y %>%
  select(Protected) %>%
  na.omit() %>%
  filter(Protected != "Protected") %>%
  separate(Protected, into = c("Player", "position"), sep = -4) %>%
  mutate(Player = toupper(Player))

y$Player = gsub(" ", ".", y$Player) 

draft_list <- x %>%
  anti_join(y) %>%
  separate(Player, into = c("first", "Player"))

## Read in and tidy age data

age <- read_csv("age.csv", skip = 1) %>%
  select(Player, Age)

age$Player = gsub("\\\\", "/", age$Player)

age <- age %>%
  separate(Player, into = c("Player", "x"), sep = "/") %>%
  mutate(Player = toupper(Player))

age$Player = gsub(" ", ".", age$Player) 

age <- age %>%
  filter(Age > 36)


## Read in filtered draft list

draft <- read_csv("draft.csv")

## Remove older players from draft list with anti-join

draft <- draft %>%
  anti_join(age)

## Create final dataset, filtering out Vegas Players and Players with small sample sizes

final <- draft %>%
  filter(TOI > 360,
         Team != "VGK") %>%
  select(Player, Team, `iTKA/60`, Season, Name)

## Filter out teams VGK selected goalies from

final_A <- final %>%
  filter(!Team %in% c("COL", "PIT", "NYI"))

## Filter out players VGK agreed not to take in trades

final_B <- final_A %>%
  filter(!Player %in% c("RYAN.O'REILLY", "COLTON.SCEVIOUR", "ERIC.STAAL", "MARCO.SCANDELLA", "MATT.DUMBA", "JOSH.ANDERSON", "JOHN-MICHAEL.LILES"))

## Filter out teams from which Vegas selected AHL prospects

final_C <- final_B %>%
  filter(!Team %in% c("CAR", "WPG", "ARI", "EDM", "TOR"))

## Filter out teams where Vegas' selections produced less than 25 points in the 2018 season

final_D <- final_C %>%
  filter(!Team %in% c("CGY", "VAN", "L.A", "NYR", "T.B", "DET", "PHI", "N.J", "BUF", "MTL", "CHI", "OTT", "S.J"))

## Show Shea Theodore's TKA / 60 with VGK

final_E <- final_D

final_E[nrow(final_E) + 1,] = list("SHEA.THEODORE","ANA", 1.37, "2017-2018", "Theodore 2018")

## Plot different stages of filtering

## Import image to use as background

img <- readPNG("logo.png")

g <- rasterGrob(img, interpolate = FALSE)

ggplot(final, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")

ggplot(final_A, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")

ggplot(final_B, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")

ggplot(final_C, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")

ggplot(final_D, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_text(aes(label = Name), nudge_y = 0.1, hjust = -0.1) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")

ggplot(final_E, aes(x = Team, y = `iTKA/60`, size = 2)) +
  background_image(img) +
  geom_text(aes(label = Name), nudge_y = 0.1, hjust = -0.1) +
  geom_point(aes(color = Season)) +
  scale_color_manual(values = c("Black", "Red", "Gold")) +
  labs(title = "Road to the Stanley Cup Finals", subtitle = "Vegas Golden Knight Draft Philosophy", y = "Takeaways / 60 Minutes", x = "Team")


## Read in PTS data from 2016-2018

x18 <- read_csv("2018.csv", skip = 1) %>%
  mutate("Team" = X2) %>%
  select(Team, PTS) 

x17 <- read_csv("2017.csv", skip = 1) %>%
  mutate("Team" = X2) %>%
  select(Team, PTS) 

x16 <- read_csv("2016.csv", skip = 1) %>%
  mutate("Team" = X2) %>%
  select(Team, PTS)

## Read in and tidy 2018 local TV Ratings Data

x <- read_csv("ratings.csv")

x$Team <- x$Team %>%
  str_replace_all("[[:punct:]]", "")

x18$Team <- x18$Team %>%
  str_replace_all("[[:punct:]]", "")

## Join PTS data and ratings data 

x <- x %>%
  inner_join(x18)

## Tidy Change in Ratings column for visualization

x <- x %>%
  separate(Ratings, into = c("Change", "Text"), sep = "%")

## Repeat for 2016-2017

x2 <- read_csv("ratings2.csv")

x2$Team <- x2$Team %>%
  str_replace_all("[[:punct:]]", "")

x17$Team <- x17$Team %>%
  str_replace_all("[[:punct:]]", "")

x2 <- x2 %>%
  inner_join(x17)

x2 <- x2 %>%
  separate(Ratings, into = c("Change", "Text"), sep = "%") 

x3 <- read_csv("ratings3.csv")

x3$Team <- x3$Team %>%
  str_replace_all("[[:punct:]]", "")

x16$Team <- x16$Team %>%
  str_replace_all("[[:punct:]]", "")

x3 <- x3 %>%
  inner_join(x16)

x3 <- x3 %>%
  separate(Rating, into = c("Change", "Text"), sep = "%") 

## Create dataset of pts and ratings from 2016-2018

ratings_data <- x %>%
  full_join(x2) %>%
  full_join(x3) %>%
  mutate(Change = as.numeric(Change)) %>%
  filter(Change != 163)

## Plot change in local tv ratings in a season against PTS

ggplot(ratings_data, aes(x = PTS, y = Change)) +
  geom_point() +
  labs(title = "Local TV Ratings of NHL Teams", y = "Change in TV Ratings over Preceding Season") +
  geom_smooth(colour = "black", se = FALSE) +
  theme(panel.background = element_rect("White", "Black", 1)) +
  theme(
    panel.grid.major = element_line("Grey", 0.5, linetype = "dashed",
                                    "square"),
    panel.grid.minor = element_line("Grey", 0.5, linetype = "dashed",
                                    "square")
  )



## To assess value of TKA metric, read in goal statistics from 2018

tka <- read_csv("18team.csv")

evga <- read_csv("18goals.csv")

veg <- tka %>%
  inner_join(evga, by = "Team")

## Plot relationship betweeen TKA and Goals 

ggplot(veg, aes(x = GF, y = TKA)) +
  geom_point() +
  geom_smooth()

## Create model of relationship between TKA / 60 and Goals, then summarize model

tk_model <- lm(`TKA/60` ~ GF, veg)

summary(tk_model)

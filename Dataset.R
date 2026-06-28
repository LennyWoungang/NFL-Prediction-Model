############################# Loading libraries and data
library(nflfastR)
library(dplyr)
library(purrr)
library(nflreadr)
library(readxl)
library(tibble)
library(tidyr)
setwd("~/Desktop/School/Courses/MSO6611/Projet Final")
Offense_and_Defense <- read_excel("Offense and Defense.xlsx")

############################## Creating dataset
##### Team wins
get_team_wins <- function(seasons) {
  map_dfr(seasons, function(s) {
    message("Processing season: ", s)
    pbp <- load_pbp(s) %>%
      filter(season_type == "REG")
    game_results <- pbp %>%
      group_by(game_id, home_team, away_team) %>%
      summarize(
        home_score = max(home_score, na.rm = TRUE),
        away_score = max(away_score, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        result = case_when(
          home_score > away_score ~ "home_win",
          away_score > home_score ~ "away_win",
          TRUE ~ "tie"
        )
      )
    team_games <- bind_rows(
      game_results %>%
        transmute(
          team = home_team,
          wins = result == "home_win",
          losses = result == "away_win",
          ties = result == "tie"
        ),
      game_results %>%
        transmute(
          team = away_team,
          wins = result == "away_win",
          losses = result == "home_win",
          ties = result == "tie"
        )
    )
    standings <- team_games %>%
      group_by(team) %>%
      summarize(
        wins   = sum(wins),
        losses = sum(losses),
        ties   = sum(ties),
        games_played = n(),
        .groups = "drop"
      )
    games_in_season <- ifelse(s >= 2021, 17, 16)
    standings %>%
      mutate(
        win_pct = (wins + 0.5 * ties) / games_in_season,
        season = s
      )
  })
}

seasons <- 2002:2024
team_wins <- get_team_wins(seasons)

#### Average Age
get_team_avg_age <- function(seasons) {
  team_fix <- tibble::tribble(
    ~team_old, ~team_new,
    "ARZ", "ARI",
    "BLT", "BAL",
    "CLV", "CLE",
    "HST", "HOU",
    "SL", "LA",   
    "SD",  "LAC",
    "OAK", "LV"
  )
  purrr::map_dfr(seasons, function(s) {
    message("Processing rosters: ", s)
    nflreadr::load_rosters(s) %>%
      filter(!is.na(team), !is.na(birth_date)) %>%
      left_join(team_fix, by = c("team" = "team_old")) %>%
      mutate(
        team = coalesce(team_new, team),
        birth_date = as.Date(birth_date),
        age = as.numeric(difftime(
          as.Date(paste0(s, "-09-01")),
          birth_date,
          units = "days"
        )) / 365.25
      ) %>%
      group_by(team) %>%
      summarize(
        avg_age = mean(age, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(season = s)
  })
}

team_avg_age <- get_team_avg_age(seasons)
team_data <- team_wins %>%
  left_join(team_avg_age, by = c("team", "season"))

#### Playoffs
get_playoffs_prev <- function(seasons) {
  playoff_teams <- purrr::map_dfr(seasons, function(s) {
    nflfastR::load_pbp(s) %>%
      dplyr::filter(season_type == "POST") %>%
      dplyr::select(home_team, away_team) %>%
      tidyr::pivot_longer(
        cols = c(home_team, away_team),
        values_to = "team"
      ) %>%
      dplyr::distinct(team) %>%
      dplyr::mutate(season = s)
  })
  playoff_teams %>%
    dplyr::mutate(
      season = season + 1,
      made_playoffs_prev = "Yes"
    ) %>%
    dplyr::select(team, season, made_playoffs_prev)
}

playoffs_prev <- get_playoffs_prev(seasons)
team_data <- team_data %>%
  dplyr::left_join(playoffs_prev, by = c("team", "season")) %>%
  dplyr::mutate(
    made_playoffs_prev = dplyr::coalesce(made_playoffs_prev, "No")
  )

#### Offense and Defense rating
team_name_map <- tibble::tribble(
  ~name, ~team,
  "Arizona Cardinals", "ARI",
  "Arizona", "ARI",
  "Atlanta Falcons", "ATL",
  "Atlanta", "ATL",
  "Baltimore Ravens", "BAL",
  "Baltimore", "BAL",
  "Buffalo Bills", "BUF",
  "Buffalo", "BUF",
  "Carolina Panthers", "CAR",
  "Carolina", "CAR",
  "Chicago Bears", "CHI",
  "Chicago", "CHI",
  "Cincinnati Bengals", "CIN",
  "Cincinnati", "CIN",
  "Cleveland Browns", "CLE",
  "Cleveland", "CLE",
  "Dallas Cowboys", "DAL",
  "Dallas", "DAL",
  "Denver Broncos", "DEN",
  "Denver", "DEN",
  "Detroit Lions", "DET",
  "Detroit", "DET",
  "Green Bay Packers", "GB",
  "Green Bay", "GB",
  "Houston Texans", "HOU",
  "Houston", "HOU",
  "Indianapolis Colts", "IND",
  "Indianapolis", "IND",
  "Jacksonville Jaguars", "JAX",
  "Jacksonville", "JAX",
  "Kansas City Chiefs", "KC",
  "Kansas City", "KC",
  "Las Vegas", "LV",
  "Oakland Raiders", "LV",
  "LA Rams", "LA",
  "St. Louis Rams", "LA",
  "LA Chargers", "LAC",
  "San Diego Chargers", "LAC",
  "Miami Dolphins", "MIA",
  "Miami", "MIA",
  "Minnesota Vikings", "MIN",
  "Minnesota", "MIN",
  "New England Patriots", "NE",
  "New England", "NE",
  "New Orleans Saints", "NO",
  "New Orleans", "NO",
  "New York Giants", "NYG",
  "NY Giants", "NYG",
  "New York Jets", "NYJ",
  "NY Jets", "NYJ",
  "Philadelphia Eagles", "PHI",
  "Philadelphia", "PHI",
  "Pittsburgh Steelers", "PIT",
  "Pittsburgh", "PIT",
  "San Francisco 49ers", "SF",
  "San Francisco", "SF",
  "Seattle Seahawks", "SEA",
  "Seattle", "SEA",
  "Tampa Bay Buccaneers", "TB",
  "Tampa Bay", "TB",
  "Tennessee Titans", "TEN",
  "Tennessee", "TEN",
  "Washington Redskins", "WAS",
  "Washington", "WAS"
)

rankings_long <- Offense_and_Defense %>%
  transmute(
    season = year1,
    def_rank = rankd,
    def_team = defence,
    off_rank = ranko,
    off_team = offense
  ) %>%
  pivot_longer(
    cols = c(def_team, off_team),
    names_to = "side",
    values_to = "name"
  ) %>%
  mutate(
    rank = ifelse(side == "def_team", def_rank, off_rank),
    side = ifelse(side == "def_team", "defence", "offense")
  ) %>%
  select(season, side, rank, name)

rankings_clean <- rankings_long %>%
  left_join(team_name_map, by = "name") %>%
  filter(!is.na(team))

rankings_wide <- rankings_clean %>%
  pivot_wider(
    names_from = side,
    values_from = rank,
    names_prefix = "rank_"
  )

team_data <- team_data %>%
  left_join(
    rankings_wide %>%
      select(team, season, rank_defence, rank_offense),
    by = c("team", "season")
  )
write.csv(team_data, "team_data.csv", row.names = FALSE)

##### Coaching tenure
team_data <- read.csv("team_data.csv")
coach_data <- tribble(
  ~team, ~coach, ~start_season, ~end_season,
  "BUF","Gregg Williams",2002,2003,
  "BUF","Mike Mularkey",2004,2005,
  "BUF","Dick Jauron",2006,2009,
  "BUF","Chan Gailey",2010,2012,
  "BUF","Doug Marrone",2013,2014,
  "BUF","Rex Ryan",2015,2016,
  "BUF","Sean McDermott",2017,2024,
  "MIA","Dave Wannstedt",2002,2004,
  "MIA","Nick Saban",2005,2006,
  "MIA","Cam Cameron",2007,2007,
  "MIA","Tony Sparano",2008,2011,
  "MIA","Joe Philbin",2012,2015,
  "MIA","Adam Gase",2016,2018,
  "MIA","Brian Flores",2019,2021,
  "MIA","Mike McDaniel",2022,2024,
  "NE","Bill Belichick",2002,2023,
  "NE","Jerod Mayo",2024,2024,
  "NYJ","Herm Edwards",2002,2005,
  "NYJ","Eric Mangini",2006,2008,
  "NYJ","Rex Ryan",2009,2014,
  "NYJ","Todd Bowles",2015,2018,
  "NYJ","Adam Gase",2019,2020,
  "NYJ","Robert Saleh",2021,2024,
  "BAL","Brian Billick",2002,2007,
  "BAL","John Harbaugh",2008,2024,
  "CIN","Dick LeBeau",2002,2002,
  "CIN","Marvin Lewis",2003,2018,
  "CIN","Zac Taylor",2019,2024,
  "CLE","Butch Davis",2002,2004,
  "CLE","Romeo Crennel",2005,2008,
  "CLE","Eric Mangini",2009,2010,
  "CLE","Pat Shurmur",2011,2012,
  "CLE","Rob Chudzinski",2013,2013,
  "CLE","Mike Pettine",2014,2015,
  "CLE","Hue Jackson",2016,2018,
  "CLE","Freddie Kitchens",2019,2019,
  "CLE","Kevin Stefanski",2020,2024,
  "PIT","Bill Cowher",2002,2006,
  "PIT","Mike Tomlin",2007,2024,
  "HOU","Dom Capers",2002,2005,
  "HOU","Gary Kubiak",2006,2013,
  "HOU","Bill OBrien",2014,2020,
  "HOU","David Culley",2021,2021,
  "HOU","Lovie Smith",2022,2022,
  "HOU","DeMeco Ryans",2023,2024,
  "IND","Tony Dungy",2002,2008,
  "IND","Jim Caldwell",2009,2011,
  "IND","Chuck Pagano",2012,2017,
  "IND","Frank Reich",2018,2022,
  "IND","Shane Steichen",2023,2024,
  "JAX","Tom Coughlin",2002,2002,
  "JAX","Jack Del Rio",2003,2011,
  "JAX","Mike Mularkey",2012,2012,
  "JAX","Gus Bradley",2013,2016,
  "JAX","Doug Marrone",2017,2020,
  "JAX","Urban Meyer",2021,2021,
  "JAX","Doug Pederson",2022,2024,
  "TEN","Jeff Fisher",2002,2010,
  "TEN","Mike Munchak",2011,2013,
  "TEN","Ken Whisenhunt",2014,2015,
  "TEN","Mike Mularkey",2015,2017,
  "TEN","Mike Vrabel",2018,2023,
  "TEN","Brian Callahan",2024,2024,
  "DEN","Mike Shanahan",2002,2008,
  "DEN","Josh McDaniels",2009,2010,
  "DEN","John Fox",2011,2014,
  "DEN","Gary Kubiak",2015,2016,
  "DEN","Vance Joseph",2017,2018,
  "DEN","Vic Fangio",2019,2021,
  "DEN","Nathaniel Hackett",2022,2022,
  "DEN","Sean Payton",2023,2024,
  "KC","Dick Vermeil",2002,2005,
  "KC","Herm Edwards",2006,2008,
  "KC","Todd Haley",2009,2011,
  "KC","Romeo Crennel",2011,2012,
  "KC","Andy Reid",2013,2024,
  "LV","Bill Callahan",2002,2003,
  "LV","Norv Turner",2004,2005,
  "LV","Art Shell",2006,2006,
  "LV","Lane Kiffin",2007,2008,
  "LV","Tom Cable",2008,2010,
  "LV","Hue Jackson",2011,2011,
  "LV","Dennis Allen",2012,2014,
  "LV","Jack Del Rio",2015,2017,
  "LV","Jon Gruden",2018,2021,
  "LV","Josh McDaniels",2022,2023,
  "LV","Antonio Pierce",2023,2024,
  "LAC","Marty Schottenheimer",2002,2006,
  "LAC","Norv Turner",2007,2012,
  "LAC","Mike McCoy",2013,2016,
  "LAC","Anthony Lynn",2017,2020,
  "LAC","Brandon Staley",2021,2023,
  "LAC","Jim Harbaugh",2024,2024,
  "DAL","Dave Campo",2002,2002,
  "DAL","Bill Parcells",2003,2006,
  "DAL","Wade Phillips",2007,2010,
  "DAL","Jason Garrett",2010,2019,
  "DAL","Mike McCarthy",2020,2024,
  "NYG","Jim Fassel",2002,2003,
  "NYG","Tom Coughlin",2004,2015,
  "NYG","Ben McAdoo",2016,2017,
  "NYG","Pat Shurmur",2018,2019,
  "NYG","Joe Judge",2020,2021,
  "NYG","Brian Daboll",2022,2024,
  "PHI","Andy Reid",2002,2012,
  "PHI","Chip Kelly",2013,2015,
  "PHI","Doug Pederson",2016,2020,
  "PHI","Nick Sirianni",2021,2024,
  "WAS","Steve Spurrier",2002,2003,
  "WAS","Joe Gibbs",2004,2007,
  "WAS","Jim Zorn",2008,2009,
  "WAS","Mike Shanahan",2010,2013,
  "WAS","Jay Gruden",2014,2019,
  "WAS","Ron Rivera",2020,2023,
  "WAS","Dan Quinn",2024,2024,
  "CHI","Dick Jauron",2002,2003,
  "CHI","Lovie Smith",2004,2012,
  "CHI","Marc Trestman",2013,2014,
  "CHI","John Fox",2015,2017,
  "CHI","Matt Nagy",2018,2021,
  "CHI","Matt Eberflus",2022,2024,
  "DET","Marty Mornhinweg",2002,2002,
  "DET","Steve Mariucci",2003,2005,
  "DET","Rod Marinelli",2006,2008,
  "DET","Jim Schwartz",2009,2013,
  "DET","Jim Caldwell",2014,2017,
  "DET","Matt Patricia",2018,2020,
  "DET","Dan Campbell",2021,2024,
  "GB","Mike Sherman",2002,2005,
  "GB","Mike McCarthy",2006,2018,
  "GB","Matt LaFleur",2019,2024,
  "MIN","Mike Tice",2002,2005,
  "MIN","Brad Childress",2006,2010,
  "MIN","Leslie Frazier",2010,2013,
  "MIN","Mike Zimmer",2014,2021,
  "MIN","Kevin OConnell",2022,2024,
  "ATL","Dan Reeves",2002,2003,
  "ATL","Jim Mora Jr.",2004,2006,
  "ATL","Bobby Petrino",2007,2007,
  "ATL","Mike Smith",2008,2014,
  "ATL","Dan Quinn",2015,2020,
  "ATL","Arthur Smith",2021,2023,
  "ATL","Raheem Morris",2024,2024,
  "CAR","John Fox",2002,2010,
  "CAR","Ron Rivera",2011,2019,
  "CAR","Matt Rhule",2020,2022,
  "CAR","Frank Reich",2023,2023,
  "CAR","Dave Canales",2024,2024,
  "NO","Jim Haslett",2002,2005,
  "NO","Sean Payton",2006,2021,
  "NO","Dennis Allen",2022,2024,
  "TB","Jon Gruden",2002,2008,
  "TB","Raheem Morris",2009,2011,
  "TB","Greg Schiano",2012,2013,
  "TB","Lovie Smith",2014,2015,
  "TB","Dirk Koetter",2016,2018,
  "TB","Bruce Arians",2019,2021,
  "TB","Todd Bowles",2022,2024,
  "ARI","Dave McGinnis",2002,2003,
  "ARI","Dennis Green",2004,2006,
  "ARI","Ken Whisenhunt",2007,2012,
  "ARI","Bruce Arians",2013,2017,
  "ARI","Steve Wilks",2018,2018,
  "ARI","Kliff Kingsbury",2019,2022,
  "ARI","Jonathan Gannon",2023,2024,
  "LA","Mike Martz",2002,2005,
  "LA","Scott Linehan",2006,2008,
  "LA","Steve Spagnuolo",2009,2011,
  "LA","Jeff Fisher",2012,2016,
  "LA","Sean McVay",2017,2024,
  "SF","Steve Mariucci",2002,2002,
  "SF","Dennis Erickson",2003,2004,
  "SF","Mike Nolan",2005,2008,
  "SF","Mike Singletary",2008,2010,
  "SF","Jim Harbaugh",2011,2014,
  "SF","Jim Tomsula",2015,2015,
  "SF","Chip Kelly",2016,2016,
  "SF","Kyle Shanahan",2017,2024,
  "SEA","Mike Holmgren",2002,2008,
  "SEA","Jim Mora Jr.",2009,2009,
  "SEA","Pete Carroll",2010,2023,
  "SEA","Mike Macdonald",2024,2024
)

coach_tenure_clean <- coach_data %>%
  rowwise() %>%
  mutate(season = list(start_season:end_season)) %>%
  unnest(season) %>%
  ungroup() %>%
  arrange(team, coach, season) %>%
  group_by(team, coach) %>%
  mutate(raw_tenure = row_number()) %>%
  ungroup() %>%
  group_by(team, season) %>%
  mutate(
    n_coaches = n(),
    fired = n_coaches == 2 & raw_tenure == min(raw_tenure),
    hired = n_coaches == 2 & raw_tenure == max(raw_tenure)
  ) %>%
  ungroup() %>%
  mutate(
    coach_tenure_year = raw_tenure +
      case_when(
        fired ~ -0.5,                                   
        hired & season > min(season[fired], na.rm = TRUE) ~ 0.5,
        TRUE ~ 0
      )
  ) %>%
  filter(!(hired & n_coaches == 2)) %>%
  select(team, season, coach, coach_tenure_year) %>%
  arrange(team, season)

team_data <- team_data %>%
  left_join(
    coach_tenure_clean,
    by = c("team", "season")
  )

team_data <- team_data %>%
  mutate(
    coach = case_when(
      team == "DAL" & season == 2010 ~ "Wade Phillips",
      team == "KC"  & season == 2011 ~ "Todd Haley",
      team == "LV"  & season == 2023 ~ "Josh McDaniels",
      team == "LV"  & season == 2008 ~ "Lane Kiffin",
      team == "TEN" & season == 2015 ~ "Ken Whisenhunt",
      team == "SF"  & season == 2008 ~ "Mike Nolan",
      team == "MIN" & season == 2010 ~ "Brad Childress",
      TRUE ~ coach
    ),
    coach_tenure_year = case_when(
      team == "DAL" & season == 2007 & coach == "Wade Phillips" ~ 1,
      team == "DAL" & season == 2008 & coach == "Wade Phillips" ~ 2,
      team == "DAL" & season == 2009 & coach == "Wade Phillips" ~ 3,
      team == "DAL" & season == 2010 & coach == "Wade Phillips" ~ 3.5,
      team == "KC" & season == 2009 & coach == "Todd Haley" ~ 1,
      team == "KC" & season == 2010 & coach == "Todd Haley" ~ 2,
      team == "KC" & season == 2011 & coach == "Todd Haley" ~ 2.5,
      team == "LV" & season == 2022 & coach == "Josh McDaniels" ~ 1,
      team == "LV" & season == 2023 & coach == "Josh McDaniels" ~ 1.5,
      team == "LV" & season == 2008 & coach == "Lane Kiffin" ~ 1.5,
      team == "TEN" & season == 2015 & coach == "Ken Whisenhunt" ~ 1.5,
      team == "LV" & season == 2008 & coach == "Lane Kiffin" ~ 1.5,
      team == "SF" & season == 2008 & coach == "Mike Nolan" ~ 3.5,
      team == "MIN" & season == 2010 & coach == "Brad Childress" ~ 4.5,
      TRUE ~ coach_tenure_year
    )
  )

team_data <- team_data %>%
  mutate(
    coach_tenure_year = case_when(
      coach == "Jason Garrett" & team == "DAL" ~ coach_tenure_year - 0.5,
      coach == "Romeo Crennel" & team == "KC"  ~ coach_tenure_year - 0.5,
      coach == "Antonio Pierce" & team == "LV" ~ coach_tenure_year - 0.5,
      coach == "Tom Cable" & team == "LV" ~ coach_tenure_year - 0.5,
      coach == "Mike Mularkey" & team == "TEN" ~ coach_tenure_year - 0.5,
      coach == "Mike Singletary" & team == "SF" ~ coach_tenure_year - 0.5,
      coach == "Leslie Frazier" & team == "MIN" ~ coach_tenure_year - 0.5,
      TRUE ~ coach_tenure_year
    )
  )

team_data <- team_data %>%
  mutate(
    rank_defence_cat = case_when(
      rank_defence <= 8  ~ 1L,
      rank_defence <= 16 ~ 2L,
      rank_defence <= 24 ~ 3L,
      rank_defence <= 32 ~ 4L,
      TRUE ~ NA_integer_
    ),
    rank_offense_cat = case_when(
      rank_offense <= 8  ~ 1L,
      rank_offense <= 16 ~ 2L,
      rank_offense <= 24 ~ 3L,
      rank_offense <= 32 ~ 4L,
      TRUE ~ NA_integer_
    )
  )

team_data <- team_data %>%
  mutate(
    made_playoffs_prev = case_when(
      made_playoffs_prev %in% c("Yes", "YES", "yes") ~ 1,
      made_playoffs_prev %in% c("No",  "NO",  "no")  ~ 0,
      TRUE ~ NA_real_
    )
  )

team_data %>%
  filter(coach_tenure_year %% 1 == 0.5) %>%
  select(team, season, coach, coach_tenure_year) %>%
  arrange(team, season)


########## Final datasets
Training_data <- team_data %>% filter(season != 2024 & season != 2002) %>% select(-rank_offense, -rank_defence) 
Test_data <- team_data %>% filter(season == 2024) %>% select(-rank_offense, -rank_defence) 
Final_data <- team_data %>% select(-rank_offense, -rank_defence) 

write.csv(Training_data, "Training_data.csv", row.names = FALSE)
write.csv(Test_data, "Test_data.csv", row.names = FALSE)
write.csv(Final_data, "Final_data.csv", row.names = FALSE)





library(httr)
library(dplyr)
library(rvest)
library(purrr)

## Returns a tibble of play by play events. (list of seasons, list of event types). Only Shots, Missed Shots, Goals, and Hits currently supported ----
get_events <- function(season_list, event_list){
  
  game_data <- tibble(gamePk = character(),
                      gameType = character(),
                      gameSeason = character(),
                      teamHome = character(),
                      teamAway = character(),
                      gameDate = date()
                      )
  # create tibble of each game
  for (season in season_list) {
    games_schedule <- content(GET(paste0("https://statsapi.web.nhl.com/api/v1/schedule/?season=", season,"&gameType=R&gameType=P")))
    print(season)
    for (day in games_schedule$dates) {
      for (game in day$games) {
        game_data <- add_row(game_data,
                             gamePk = game$gamePk,
                             gameType = game$gameType,
                             gameSeason = game$season,
                             teamHome = game$teams$home$team$name,
                             teamAway = game$teams$away$team$name,
                             gameDate = game$gameDate
        )
      }
    }
  }
  
  
  # Iterate through each game and get raw pbp data. 
  for (game in 1:nrow(game_data)) {
    
    # Set up event tibble
    event_data <- tibble(eventTypeId = character(),
                         secondaryType = character(),
                         period = numeric(),
                         periodTime = character(),
                         coordX = numeric(),
                         coordY = numeric(),
                         Shooter = character(),
                         Scorer = character(),
                         Assist1 = character(),
                         Assist2 = character(),
                         Hitter = character(),
                         Hittee = character(),
                         Goalie = character(),
                         
                         
                         gamePk = character(),
                         gameType = character(),
                         gameSeason = character(),
                         teamHome = character(),
                         teamAway = character(),
                         gameDate = date(),
                         
    )

    gamePk <- game_data[game, "gamePk"]
    data_pbp <- content(GET(paste0("https://statsapi.web.nhl.com/api/v1/game/",gamePk,"/feed/live")))
    print(paste("Game ID: ",gamePk, data_pbp$gameData$datetime$dateTime))
    
    # playCount = 1
    for (play in data_pbp$liveData$plays$allPlays) {
      # print(playCount)
      # playCount <- playCount + 1
      if (play$result$event %in% event_list){   #,
        
        # Init each type as NA. For each play matching event list, assign players to playerTypes.
        # Special case for assists, if second assist, assign player to AssistIn2
        for (type in c("ScorerIn","AssistIn","AssistIn2","GoalieIn","ShooterIn","HitterIn","HitteeIn")) {
          assign(type,NA)
        }
        for (player in play$players) {
          
          typeIn <- paste0(player$playerType,"In")
          
          if (is.na(get(typeIn))) {
            assign(typeIn,player$player$fullName)
          } else {
            assign(paste0(typeIn,"2"),player$player$fullName)
          }
        }
        
        # Add event row. If(length(.)) catches incomplete data
        event_data <- add_row(event_data,
                              eventTypeId = play$result$eventTypeId,
                              secondaryType =  play$result$secondaryType %>%
                                if(length(.) == 0) NA  else .,
                              period = play$about$period,
                              periodTime = play$about$periodTime,
                              coordX = play$coordinates$x %>%
                                if(length(.) == 0) "No Value" else .,
                              coordY = play$coordinates$y %>%
                                if(length(.) == 0) "No Value" else .,
                              
                              Scorer = ScorerIn,
                              Shooter = ShooterIn,
                              Assist1 = AssistIn,
                              Assist2 = AssistIn2,
                              Hitter = HitterIn,
                              Hittee = HitteeIn,
                              Goalie = GoalieIn,
                              gamePk = data_pbp$gamePk,
                              gameType = data_pbp$gameData$game$type,
                              gameSeason = data_pbp$gameData$game$season,
                              teamHome = data_pbp$gameData$teams$home$name,
                              teamAway = data_pbp$gameData$teams$away$name,
                              gameDate = data_pbp$gameData$datetime$dateTime
                              )
      }
    }
  #save/append
  write.table(event_data,file = "1718Export.csv", append = TRUE, sep = ",", quote = FALSE,row.names = FALSE, col.names = FALSE)
  
  Sys.sleep(sample(seq(1, 3, by=0.001), 1)) #Wait
  }
}

# Options
## "Goal","Shot","Missed_shot","Hit"
event_list <- c("Goal","Shot","Missed_shot","Hit")

# Options
## "20102011","20112012","20122013","20132014","20142015","20152016","20162017","20172018","20182019","20192020"
season_list <- c("20172018","20182019")

get_events(season_list, event_list)
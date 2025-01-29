#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... No arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Simulates the contestant making initial door choice.
#' @description
#' 'select_door()' will randomly select a door number between 1-3 using 'sample'. 
#' @details
#' This function is the first step after creating the game, with a 33.33% chance of selecting
#' the winning door that will reveal the car.
#' @param ... No arguments are used by the function.
#' @return
#' The function returns an integer between 1 and 3.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Simulates the host opening the remaining goat door.
#' @description
#' 'open_goat_door' will select a goat door using 'sample',
#' that was not selected by the contestant using 'which'.
#' @details
#' This function is the second step in the game. It eliminates one
#' of the goat doors, leaving only 1 goat door and only 1 car door.
#' @param 
#' game Character vector
#' @param 
#' a.pick Numeric integer
#' @return The function returns a numeric vector
#' @examples
#' open_goat_door(1:3, 2)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Determines whether the contestant keeps their initial door choice or switches
#' to the remaining door choice.
#' @description
#' 'change_door()' uses 'if' to determine whether the contestant keeps the initial
#' door choice of switches to the remaining door.
#' @details
#' This function is the third step of the game and represents the contestant's
#' final door choice.
#' @param
#' stay Logical integer
#' @param 
#' opened.door Numeric integer
#' @param 
#' a.pick Numeric integer
#' @return  Numeric integer representing the door that was opened.
#' @examples
#' change_door(stay=T, opened.door = 1, a.pick = 2)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determines whether the contestant wins or loses the game.
#' @description
#' 'determine_winner()' identifies whether the contestant's final door
#'  choice reveals a car door and wins or a goat door and loses.
#' @details
#' The function checks to see if the final door selection is a car or a goat.
#' @param
#' final.pick Numeric integer
#' @return 
#' Character string, "WIN" or "LOSE"
#' @examples
#' determine_winner(final.pick = 1, game = c("goat", "car", "goat"))
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Executes the game and stores the outcome values.
#' @description
#' 'play_game' plays through the game and returns the game outcome values.
#' @details
#' This function gives the output values for randomly placing the car and goats,
#' the contestant's initial choice, a goat door revealed by the host, the contestant's
#' choice to switch or stay, the final door selected by the contestant, and whether
#' or not the contestant wins or loses if they stay, and whether or not the contestant
#' wins or loses if they switch
#' @param ... No arguments are used by the function.
#' @return Data frame containing 2 columns: strategy and outcome.
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Monty Hall Game Simulation
#' @description
#' 'play_n_games' plays through the Monty Hall game simulation 'n' number of times
#' to produce an output table to determine the percentage of wins or losses
#' for staying or switching.
#' @details
#' This function loops through the game 100 times and returns a data frame of
#' game outcomes and proportion of wins and loses for the strategy of staying 
#' and the strategy of switching.
#' @param 
#' n Integer for number of game simulations
#' @return 
#' Data frame and proportion tables
#' @examples
#' play_n_games(100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

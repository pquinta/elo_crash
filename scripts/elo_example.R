# Elo Ratings
# ELO ratings cannot be updated on a transaction basis. 
#The entire win lose data has to be fed and elo rankings will get updated

install.packages('elo')
install.packages('PlayerRatings')
install.packages('lubridate')

####ELO Rating Manual
# teams <- data.frame(name=c('Cristian','Lucho','Santi','Juan'),
#                     elo=c(0,0,0,0))
teams <- data.frame(name=c('team1','team2','team3'),elo=c(2200,2200,2200))
teams$name <- as.character(teams$name)

# matches <- data.frame(matchNo <- c(1,2,3,4),winteam=c('team1','team1','team2'),
#                       loseteam=c('team2','team3','team3'))
matches <- data.frame(matchNo <- c(1,2,3),winteam=c('team1','team1','team2'),
                      loseteam=c('team2','team3','team3'))

matches$winteam <- as.character(matches$winteam)
matches$loseteam <- as.character(matches$loseteam)

# Call the function to update the ELo rankings
teams <- updateElo(teams, matches)

# Function to update ELO rakings
updateElo <- function(teams,matches)
{
  teams$updatedElo <- 0
  for(team in teams$name)
  {
    # Denominator
    matchCount <- nrow(matches[matches$winteam==team | matches$loseteam==team,])
    # Numerator
    numerator <- 0
    curteamMatches <- matches[matches$winteam==team | matches$loseteam==team,]
    for(i in 1:nrow(curteamMatches))
    {
      if(curteamMatches[i,2]==team) numerator <- numerator + 400 + 
          teams[teams$name==curteamMatches[i,3],2]
      if(curteamMatches[i,3]==team) numerator <- numerator - 400 + 
          teams[teams$name==curteamMatches[i,2],2]
    }
    teams[teams$name==team,3] <- numerator / matchCount
    
  }
  teams$elo <- teams$updatedElo
  teams$updatedElo <- NULL
  return(teams)
}





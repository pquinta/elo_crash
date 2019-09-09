###ELO Example Stackoverflow

k<-30
ateam<-paste0("a",1:5)
hteam<-paste0("h",1:5)
playersid <- unique(unname( unlist( datas[, c(ateam,hteam) ] ) ))
scores=as.list(rep(2200,length(playersid)))
names(scores)<-playersid

getPlayerScore <- function(player,team_score,opponents_scores) {
  old_score <- scores[[as.character(player)]][1]
  expect <- sum(1/10^((opponents_scores - old_score)/400))/5
  return(old_score + k*(team_score - expect))
}

updateTeamPlayersScore<-function(row,team) {
  opteam<-ifelse(team=="a","h","a") # get the team we're against
  players <- unlist(row[get(paste0(team,"team"))]) # get the players list
  opponents <- unlist(row[get(paste0(opteam,"team"))]) # get the oppenents list
  # Get the oppents scores 
  opponents_score <- sapply(scores[as.character(opponents)],function(x) { x[[1]] } ) 
  # loop over the players and return the list of updated scores
  r<-lapply(players,function(x) {
    new_score <- getPlayerScore(x,as.numeric(row[paste0(team,".evt.score")]),opponents_score)
    c(new_score,scores[[as.character(x)]])
  })
  # Update the list names
  names(r) <- as.character(opponents)
  r # return the new scores list
}

# loop over the rows.
# The update is done after calculation to avoid side-effect on h scores with updated a scores
for (i in 1:nrow(datas)) {
  row <- datas[i,]
  # Get updated scores for team a
  new_a <- updateTeamPlayersScore(row,"a")
  # Get updated scores for team h
  new_h <- updateTeamPlayersScore(row,"h")
  # update team 'a' scores
  scores[names(new_a)] <- new_a
  # update team 'h' scores
  scores[names(new_h)] <- new_h
}
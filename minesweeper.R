# To play in RStudio, click "Source" instead of "Run."
# To plant a flag, press enter without typing a row or column number when promped.
# To exit manually, press enter without typing a row or column number while planting a flag.

##############################
## level 1: 8x8, 10 mines   ##
## level 2: 16x16, 40 mines ##
## level 3: 24x24, 99 mines ##
##############################

as.display <- function(mask){
  n.row <- nrow(mask)
  n.col <- ncol(mask)
  for(i in 1:n.row){
    for(j in 1:n.col){
      if(mask[i,j] == "0"){
        mask[i,j] <- ""
      }
    }
  }
  return(as.data.frame(mask))
}

clear.zeros <- function(board, mask, coord){
  n.row <- nrow(board)
  n.col <- ncol(board)
  zero.count.0 <- 0
  zero.count.1 <- sum(mask == "0")
  
  while(zero.count.1 - zero.count.0 != 0){
    for(i in 1:n.row){
      for(j in 1:n.col){
        if(mask[i,j] == "0"){
          ro <- c((i - 1):(i + 1)); ro <- ro[ro > 0 & ro <= n.row]
          co <- c((j - 1):(j + 1)); co <- co[co > 0 & co <= n.col]
          mask[ro, co][mask[ro, co] != "P"] <- board[ro, co][mask[ro, co] != "P"]
        }
      }
    }
    zero.count.0 <- zero.count.1
    zero.count.1 <- sum(mask == "0")
  }
  
  return(mask)
}

reveal <- function(board, mask, coord){
  x <- coord[2]
  y <- coord[1]
  
  if(mask[y,x] == "P"){
    return(mask)
  }else if(board[y,x] == "*"){
    return("Boom!")
  }else{
    mask[y,x] <- board[y,x]
    if(board[y,x] == "0"){
      mask <- clear.zeros(board, mask, coord)
    }
    return(mask)
  }
}

plant.flag <- function(board, mask, coord){
  x <- coord[2]
  y <- coord[1]
  if(mask[y,x] == "P"){
    mask[y,x] <- "#"
  }else if(mask[y,x] == "#"){
    mask[y,x] <- "P"
  }
  mask
}

diff.level <- readline(prompt = "Difficulty level? ")

if(diff.level == "1"){
  n.row <- 8
  n.col <- 8
  n.mine <- 10
}else if(diff.level == "2"){
  n.row <- 16
  n.col <- 16
  n.mine <- 40
}else{
  n.row <- 24
  n.col <- 24
  n.mine <- 99
}

mine.loc <- sample(c(1:(n.row * n.col)), size = n.mine)
v <- vector("numeric", length = n.row * n.col)
v[mine.loc] <- "*"
edge <- c(1:9, letters)
board <- matrix(v, ncol = n.col, 
                dimnames = list(edge[1:n.row], 
                                edge[1:n.col]))
mask <- matrix("#", ncol = n.col, nrow = n.row, 
               dimnames = list(edge[1:n.row], 
                               edge[1:n.col]))

for(i in 1:n.row){
  for(j in 1:n.col){
    if(board[i,j] != "*"){
      ro <- c((i - 1):(i + 1))
      ro <- ro[ro > 0 & ro <= n.row]
      co <- c((j - 1):(j + 1))
      co <- co[co > 0 & co <= n.col]
      non <- board[ro, co]
      board[i,j] <- as.character(sum(non == "*"))
    }
  }
}

covered <- sum(mask == "#")
while(covered > n.mine){
  print(as.display(mask))
  
  ro <- readline(prompt = "Row?    ")
  co <- readline(prompt = "Column? ")
  ro <- which(edge[1:n.row] == ro)
  co <- which(edge[1:n.col] == co)
  if(length(ro) * length(co) == 0){ # plant flags
    ro <- readline(prompt = "Flag row?    ")
    co <- readline(prompt = "Flag column? ")
    ro <- which(edge[1:n.row] == ro)
    co <- which(edge[1:n.col] == co)
    
    if(length(ro) * length(co) == 0){break}
    coord <- c(ro, co)
    mask <- plant.flag(board, mask, coord)
  }else{
    coord <- c(ro, co)
    mask <- reveal(board = board, mask = mask, coord = coord)
    if(class(mask) == "character"){ # game over
      print(mask)
      print(as.display(board))
      break
    }
  }
  
  covered <- sum(mask == "#" | mask == "P") # check if all safe tiles are uncovered
  if(covered == n.mine){ # game won
    print("Clear!")
    print(as.display(mask))
  }
}
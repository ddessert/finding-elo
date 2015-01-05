# read_pgn.R - Read a PGN file for kaggle competition Elo
#
# 
read_pgn <- function()
{

    # Load the pgn data
    if (file.exists("./data/pgn.RDS"))
    {
        read_pgn <- readRDS("./data/pgn.RDS")
    } else
    {
        # Source data files
        inFile <- './data/data.pgn'
        
        # [Event "1"]
        # [Site "kaggle.com"]
        # [Date "??"]
        # [Round "??"]
        # [White "??"]
        # [Black "??"]
        # [Result "1/2-1/2"]
        # [WhiteElo "2354"]
        # [BlackElo "2411"]
        # 1. Nf3 Nf6 2. c4 c5 3. b3 g6 4. Bb2 Bg7 5. e3 O-O 6. Be2 b6 7. O-O Bb7 8. 
        # Nc3 Nc6 9. Qc2 Rc8 10. Rac1 d5 11. Nxd5 Nxd5 12. Bxg7 Nf4 13. exf4 Kxg7 14.
        # Qc3+ Kg8 15. Rcd1 Qd6 16. d4 cxd4 17. Nxd4 Qxf4 18. Bf3 Qf6 19. Nb5 Qxc3
        # 1/2-1/2
        
        
        # Extract just the WhiteElo fields
        # user  system elapsed 
        # 0.11    1.16    1.55 
        df_data_pgn <- read.table(pipe(sprintf('grep "^\\[WhiteElo" %s', inFile)), header=FALSE)
        WhiteElo <- c(df_data_pgn$V2, rep.int(NA, 25000))
        
        # Extract just the BlackElo fields
        df_data_pgn <- read.table(pipe(sprintf('grep "^\\[BlackElo" %s', inFile)), header=FALSE)
        BlackElo <- c(df_data_pgn$V2, rep.int(NA, 25000))
        
        # Extract just the Game Result fields
        df_data_pgn <- read.table(pipe(sprintf('grep "^\\[Result" %s', inFile)), header=FALSE)
        WhiteScore <- vector(mode = "numeric", length=50000)  # initialize to 50,000 0's
        WhiteScore[df_data_pgn[2] == "1-0"] <- 1              # Set the 1's
        WhiteScore[df_data_pgn[2] == "1/2-1/2"] <- 0.5        # Set the 0.5's
        BlackScore <- 1 - WhiteScore                          # Calculate black's scores
        
        
        # Extracts "n" lines of moves ending with the resulting game score
        temp_read <- read.table(pipe(sprintf('grep -v "^\\[" %s"', inFile)), sep = '\n', stringsAsFactors = FALSE)
        
        # Make a 2-d array with the beginning, end of each move string:
        #      [,1] [,2]
        # [1,]    1    4        MoveString[1] <- temp_read[1:4]
        # [2,]    5    6        MoveString[2] <- temp_read[5:6]
        # [3,]    7   15        MoveString[3] <- temp_read[7:15]
        move_start = c(grep("^1\\.", temp_read$V1))
        extract_lines = matrix(c(move_start, move_start[-1]-1, length(temp_read$V1)), ncol=2)
        
        MoveString <- vector(mode="character", length=nrow(extract_lines))
        #    for (ii in 1:nrow(extract_lines))
        #    {
        #        MoveString[ii] <- paste(temp_read$V1[extract_lines[ii,1]:extract_lines[ii,2]], collapse=" ")
        #    }
        MoveString <- apply(extract_lines, 1, function(x) {
            paste(temp_read$V1[min(x):max(x)], collapse=" ")
        })
        
        read_pgn <- data.frame(Event = 1:50000,
                               WhiteElo = WhiteElo,
                               BlackElo = BlackElo,
                               EloDiff = WhiteElo - BlackElo,
                               EloMean = (WhiteElo + BlackElo)/2,
                               WhiteScore = WhiteScore,
                               MoveString = MoveString, 
                               stringsAsFactors = FALSE)
        
        saveRDS(read_pgn, "./data/pgn.RDS")
    }
    return(read_pgn)   
}
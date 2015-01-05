load_training_data <- function()
{
    source('read_stockfish.R')
    source('read_pgn.R')
    
    # Read the StockFish file    
    df <- read_stockfish()
    
    # Read the PGN file
    df_pgn <- read_pgn()
    
    # Total number of plies in each game
    df$nPlies <- vapply(df$MoveScores, length, FUN.VALUE=0)
    
    # Fix Missing StockFish data for certain single-move games that are missing all StockFish data (NA only):
    # "1. Nf3", score should be 18
    df$MoveScores[intersect(grep("^1\\. Nf3", df_pgn$MoveString), df$Event[df$nPlies==1])] <- 18
    # "1. c4", score should be 2
    df$MoveScores[intersect(grep("^1\\. c4", df_pgn$MoveString), df$Event[df$nPlies==1])] <- 2
    # "1. d4", score should be 19
    df$MoveScores[intersect(grep("^1\\. d4", df_pgn$MoveString), df$Event[df$nPlies==1])] <- 19
    # "1. e4", score should be 26
    df$MoveScores[intersect(grep("^1\\. e4", df_pgn$MoveString), df$Event[df$nPlies==1])] <- 26
    # "1. f4", score should be -9
    df$MoveScores[intersect(grep("^1\\. f4", df_pgn$MoveString), df$Event[df$nPlies==1])] <- -9
    
    # Number of StockFish NA scores in each game
    df$NumStockFishNA <- vapply(df$MoveScores, function(x) { x <- sum(is.na(x))}, FUN.VALUE=0)
    # proportion of NA scores in each game (StickFish couldn't compute a score within 1 second)
    df$pNA <- df$NumStockFishNA / df$nPlies
    
    
    # Minimum score for the entire game
    df$MinScore <- vapply(df$MoveScores, min, FUN.VALUE=0, na.rm = TRUE)
    # Average score for the entire game
    df$MeanScore <- vapply(df$MoveScores, mean, FUN.VALUE=0, na.rm = TRUE)
    # Maximum score for the entire game
    df$MaxScore <- vapply(df$MoveScores, max, FUN.VALUE=0, na.rm = TRUE)
    # std deviation of all moves
    df$std <- sapply(df$MoveScores, sd, na.rm = TRUE)
    
    # Final score for the game
    df$FinalScore <- vapply(df$MoveScores, function(x) 
    {
        x <- tail(x[!is.na(x)], 1)
        return(x)
    }, FUN.VALUE=0)

    # Put the number 26 at the beginning of each MoveScores vector in the list
    # Pad the MoveScores vector into an even numbered length with an NA value
    df$MoveScores <- lapply(df$MoveScores, function(x)
                    {
                        # 25 in front, NA in back
                        x <- c(26, x, NA) 
                        # Truncate to an even length
                        x <- x[1:length(x) - (length(x) %% 2)]
                        return(x)
                    })
    
    
    # Board score just before White's nth move
    # {26, n2, n3, n4, ...}
    df$ScoreBeforeWhiteMove <- lapply(df$MoveScores, function(x)
                            {
                                # Turn into matrix with two rows, select first row
                                x <- matrix(x, 2)[1,]
                                return(x)
                            })
    
    # Board score just before Black's nth move (aka after White's nth move)
    # {n1, n2, n3, n4, ...}
    df$ScoreBeforeBlackMove <- lapply(df$MoveScores, function(x)
    {
        # Turn into matrix with two rows, select second row
        x <- matrix(x, 2)[2,]
        return(x)
    })
    
    # Change in board score as a result of White's nth move
    df$ScoreDeltaWhiteMove <- sapply(df$MoveScores, function(x)
    {
        # Compute difference in moves (alternating white, black differences)
        # Turn into matrix with two rows, select first row
        x <- as.numeric(matrix(c(diff(x), NA),2)[1,-max(length(x) %/% 2, 2)])
        return(x)
    })
    
    
    # Change in board score as a result of White's nth move
    df$ScoreDeltaBlackMove <- lapply(df$MoveScores, function(x)
    {
        # Compute difference in moves (alternating white, black differences)
        # Turn into matrix with two rows, select 2nd row
        x <- matrix(c(diff(x), NA),2)[2,-max(length(x) %/% 2, 2)]
        if ((length(x) == 1) && (is.na(x))) x <- 0
        return(x)
    })
    
    # Scaled Average Error (SAE)
    df$WhiteSAE <- lapply(df$ScoreDeltaWhiteMove, function(x)
    {
        x <- log(1+abs(mean(x)/100))
        return(x)
    })
    df$BlackSAE <- lapply(df$ScoreDeltaBlackMove, function(x)
    {
        x <- log(1+abs(mean(x)/100))
        return(x)
    })
    
    # Quantile scores
    df$WhiteDeltaScoreQuantile <- lapply(df$ScoreDeltaWhiteMove, function(x)
    {
        x <- quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=TRUE)
        return(x)
    })
    df$BlackDeltaScoreQuantile <- lapply(df$ScoreDeltaBlackMove, function(x)
    {
        x <- quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=TRUE)
        return(x)
    })
    # Quantile scores
    df$BothDeltaScoreQuantile <- lapply(df$MoveScores, function(x)
    {
        x <- quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm=TRUE)
        return(x)
    })
    
    # Mean score change before/after White's move
    df$DeltaWhiteMeanMoveScore <- vapply(df$ScoreDeltaWhiteMove, mean, FUN.VALUE = 0, na.rm = TRUE)
    
    # Mean score change before/after Black's move
    df$DeltaBlackMeanMoveScore <- vapply(df$ScoreDeltaBlackMove, mean, FUN.VALUE = 0, na.rm = TRUE)
    # For the 1-move games, black does not have a delta, so use the mean of all black deltas
    df$DeltaBlackMeanMoveScore[is.na(df$DeltaBlackMeanMoveScore)] <- mean(df$DeltaBlackMeanMoveScore, na.rm = TRUE)
    
    # Mean score change before/after all moves
    df$DeltaBothMeanMoveScore <- vapply(df$MoveScores, mean, FUN.VALUE = 0, na.rm = TRUE)
    
    # For the 1-move games, black does not have a sd, so use the mean sd of all black deltas
    df$std[is.na(df$std)] <- mean(df$std, na.rm = TRUE)
    
    
    train <- data.frame(WhiteElo = df_pgn$WhiteElo,
                        BlackElo = df_pgn$BlackElo,
                        EloMean = df_pgn$EloMean,
                        EloDiff = df_pgn$EloDiff,
                        WhiteScore = df_pgn$WhiteScore,
                        NumStockFishNA = df$NumStockFishNA,
                        pNA = df$pNA,
                        MinScore = df$MinScore,
                        MeanScore = df$MeanScore,
                        MaxScore = df$MaxScore,
                        DeltaWhiteMeanMoveScore = df$DeltaWhiteMeanMoveScore,
                        DeltaWhiteMoveMin = vapply(df$ScoreDeltaWhiteMove, min, FUN.VALUE = 0, na.rm = TRUE),
                        DeltaWhiteMovepct10 = sapply(as.numeric(lapply(df$WhiteDeltaScoreQuantile, function(x){return(as.numeric(x)[2])})), mean),
                        DeltaWhiteMovepct25 = sapply(as.numeric(lapply(df$WhiteDeltaScoreQuantile, function(x){return(as.numeric(x)[3])})), mean),
                        DeltaWhiteMovepct50 = sapply(as.numeric(lapply(df$WhiteDeltaScoreQuantile, function(x){return(as.numeric(x)[4])})), mean),
                        DeltaWhiteMovepct75 = sapply(as.numeric(lapply(df$WhiteDeltaScoreQuantile, function(x){return(as.numeric(x)[5])})), mean),
                        DeltaWhiteMovepct90 = sapply(as.numeric(lapply(df$WhiteDeltaScoreQuantile, function(x){return(as.numeric(x)[6])})), mean),
                        DeltaWhiteMoveMax = vapply(df$ScoreDeltaWhiteMove, max, FUN.VALUE = 0, na.rm = TRUE),
                        DeltaBlackMeanMoveScore = df$DeltaBlackMeanMoveScore,
                        DeltaBlackMoveMin = vapply(df$ScoreDeltaBlackMove, min, FUN.VALUE = 0, na.rm = TRUE),
                        DeltaBlackMovepct10 = sapply(as.numeric(lapply(df$BlackDeltaScoreQuantile, function(x){return(as.numeric(x)[2])})), mean),
                        DeltaBlackMovepct25 = sapply(as.numeric(lapply(df$BlackDeltaScoreQuantile, function(x){return(as.numeric(x)[3])})), mean),
                        DeltaBlackMovepct50 = sapply(as.numeric(lapply(df$BlackDeltaScoreQuantile, function(x){return(as.numeric(x)[4])})), mean),
                        DeltaBlackMovepct75 = sapply(as.numeric(lapply(df$BlackDeltaScoreQuantile, function(x){return(as.numeric(x)[5])})), mean),
                        DeltaBlackMovepct90 = sapply(as.numeric(lapply(df$BlackDeltaScoreQuantile, function(x){return(as.numeric(x)[6])})), mean),
                        DeltaBlackMoveMax = vapply(df$ScoreDeltaBlackMove, max, FUN.VALUE = 0, na.rm = TRUE),
                        DeltaBothMeanMoveScore = df$DeltaBothMeanMoveScore,
                        DeltaBothMoveMin = vapply(df$MoveScore, min, FUN.VALUE = 0, na.rm = TRUE),
                        DeltaBothMovepct10 = sapply(as.numeric(lapply(df$BothDeltaScoreQuantile, function(x){return(as.numeric(x)[2])})), mean),
                        DeltaBothMovepct25 = sapply(as.numeric(lapply(df$BothDeltaScoreQuantile, function(x){return(as.numeric(x)[3])})), mean),
                        DeltaBothMovepct50 = sapply(as.numeric(lapply(df$BothDeltaScoreQuantile, function(x){return(as.numeric(x)[4])})), mean),
                        DeltaBothMovepct75 = sapply(as.numeric(lapply(df$BothDeltaScoreQuantile, function(x){return(as.numeric(x)[5])})), mean),
                        DeltaBothMovepct90 = sapply(as.numeric(lapply(df$BothDeltaScoreQuantile, function(x){return(as.numeric(x)[6])})), mean),
                        DeltaBothMoveMax = vapply(df$MoveScore, max, FUN.VALUE = 0, na.rm = TRUE),
                        FinalScore = df$FinalScore,
                        nPlies = df$nPlies,
                        std = df$std
                        )                    
    
    return (train)
}
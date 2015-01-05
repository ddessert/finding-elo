# read_stockfish.R - Read a stockfsh file for kaggle competition Elo
#
# 
read_stockfish <- function()
{
    # Load the StockFish data
    if (file.exists("./data/stockfish.RDS"))
    {
        df <- readRDS("./data/stockfish.RDS")
    } else
    {
        # Source data files
        inFile <- './data/stockfish.csv'
        
        df <- read.csv(inFile, stringsAsFactors = FALSE)
        
        # Converts the first set of moves into a numeric vector
        df$MoveScores <- as.vector(sapply(strsplit(df$MoveScores, " "), as.numeric))
        
        # Save off as RDS file
        saveRDS(df, "./data/stockfish.RDS")
    }
    
    return (df)
}
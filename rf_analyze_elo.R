source('load_training_data.R')

# Load the training data
train <- load_training_data()

# Library needed to create a random forest
library(randomForest)

# Set my random seed (for reproducability)
set.seed(314)

# Items     1:25000 are training data
# Items 25001:50000 are test data
i_train <- 1:25000

# Compute a random forest for the White Elo score
rf_white_fit <- randomForest(WhiteElo ~
                             MeanScore + 
#                             MinScore +
#                             WhiteScore +
                             MaxScore +
                             FinalScore +
                             std +
                             DeltaBlackMeanMoveScore +
                             DeltaWhiteMoveMax +
                             DeltaWhiteMovepct90 +
#                             DeltaWhiteMovepct75 +
#                             DeltaWhiteMovepct50 +
#                             DeltaWhiteMovepct25 +
#                             DeltaWhiteMovepct10 +
                             DeltaWhiteMoveMin +
                             DeltaWhiteMeanMoveScore +
                             DeltaBlackMeanMoveScore +
                                 DeltaBlackMoveMax +
#                             pNA +
#                             NumStockFishNA +
                             nPlies,
                             data = train,
                             subset = i_train,
                             importance=TRUE, 
                                proximity=TRUE,
                                do.trace = 20,
                                ntree=100)


# Compute a random forest for the Black Elo score
rf_black_fit <- randomForest(BlackElo ~
                                 MeanScore + 
                                 MinScore +
#                                 WhiteScore +
                                 MaxScore +
                                 FinalScore +
                                 std +
                                 DeltaWhiteMeanMoveScore +
                                 DeltaBlackMoveMax +
#                                 DeltaBlackMovepct90 +
#                                 DeltaBlackMovepct75 +
#                                 DeltaBlackMovepct50 +
#                                 DeltaBlackMovepct25 +
                                 DeltaBlackMovepct10 +
                                 DeltaBlackMoveMin +
#                                 DeltaBlackMeanMoveScore +
                                 DeltaWhiteMeanMoveScore +
                                 DeltaWhiteMoveMax +
                                 #                             pNA +
                                 #                             NumStockFishNA +
                                 nPlies,
                             data = train,
                             subset = i_train,
                             importance=TRUE, 
                             proximity=TRUE,
                             do.trace = 20,
                             ntree=100)

# Make the prediction
predict_white <- predict(rf_white_fit, train)
predict_black <- predict(rf_black_fit, train)

# Result of prediction
cat(sprintf("White's Mean Average Error = %9.5f\n", mean(abs(train$WhiteElo - predict_white), na.rm=TRUE)))
cat(sprintf("Black's Mean Average Error = %9.5f\n", mean(abs(train$BlackElo - predict_black), na.rm=TRUE)))
cat(sprintf("Combined Mean Average Error = %9.5f\n", mean(abs(c(train$WhiteElo, train$BlackElo) - c(predict_white, predict_black)), na.rm=TRUE)))

# Build Output data frame
df_out = data.frame(Event = 25001:50000, 
                    WhiteElo = round(predict_white[25001:50000]), 
                    BlackElo = round(predict_black[25001:50000])
                    )

# Write the CSV file for submission
write.csv(df_out, file="sumbission.csv", quote=FALSE, row.names = FALSE)

# Plot of important variables
varImpPlot(rf_white_fit)
varImpPlot(rf_black_fit)

# 
plot(margin(rf_black_fit))
MDSplot(rf_black_fit, train$nPlies)


library(ggplot2)
# Correlation plot of actual vs predicted White Elo
p <- qplot(train$WhiteElo, predict_white)
p <- p + annotate("segment", 
                  x = 1000, xend = 2750, 
                  y = 1000, yend = 2750, 
                  colour = "red")
p <- p + xlab("Real Elo (White)")
p <- p + ylab("Predicted Elo")
print(p)

#---------Riley Feltner's Stuff + Code---------

#Special Shoutout to some friends who lend
  #A helping hand along the way and let me bounce questions
  #off of them: Johnny Davis, Joe Koetters, Robert Frey

  #I used Johnny's code as a guideline for my stuff plus model! I used a fairly
  #Similar process as he did. Check him out, his content is awesome and he's a great 
  #Dude! Twitter: @Johnny_Davis12


#Load Libraries
library(xgboost)
library(ggplot2)
#install.packages("gridGraphics")
library(gridGraphics)
library(vip)
library(dplyr)
library(knitr)
set.seed(1234)

#Import Datasets and Minor Cleaning
# data2015 <- read.csv("FullPitchbyPitchDataset2015.csv")
# data2016 <- read.csv("FullPitchbyPitchDataset2016.csv")
# data2017 <- read.csv("FullPitchbyPitchDataset2017.csv")
# data2018 <- read.csv("FullPitchbyPitchDataset2018.csv")
data2019 <- read.csv("FullPitchbyPitchDataset2019.csv")
data2020 <- read.csv("FullPitchbyPitchDataset2020.csv")
data2021 <- read.csv("FullPitchbyPitchDataset2021.csv")
data2022 <- read.csv("Full2022Dataset.csv")
data2023 <- read.csv("data2023.csv")
LinearWeights <- read.csv("Linear Weights.csv")

LinearWeights <- LinearWeights[c(1, 2)]
LinearWeights <- LinearWeights[-c(26, 27),]

combined_data <- rbind(data2019, data2020, data2021, data2022)

#----------------------Clean combined_data---------------------
#Make Event Blanks NAs
combined_data$events[combined_data$events == "" | combined_data$events == " "] <- NA

#Replace NAs in Events column with values from description column
combined_data_new <- combined_data
combined_data_new$events[is.na(combined_data_new$events)] <- combined_data_new$description[is.na(combined_data_new$events)]

#Replace strikeout (events) with description value
#This is because we are not using strikeout linear weight,
#but rather called_strike of swinging_strike
combined_data_new <- combined_data_new %>%
  mutate(events = if_else(events == "strikeout", description, events))

#Left Join data and weights
combined_data_new <- left_join(LinearWeights, combined_data_new, by = c("Event" = "events"))

#Take abs value of Horizontal break and convert to Inches and abs value of Horizontal Release
combined_data_new$pfx_x <- abs(combined_data_new$pfx_x) * 12
combined_data_new$pfx_z <- combined_data_new$pfx_z * 12

combined_data_new$release_pos_x <- abs(combined_data_new$release_pos_x)

#Rename Columns for Differential Break
colnames(combined_data_new)[colnames(combined_data_new) == "pfx_x"] <- "Horizontal_Break"
colnames(combined_data_new)[colnames(combined_data_new) == "pfx_z"] <- "Vertical_Break"

#Add in Differential Break
combined_data_new$Differential_Break <- combined_data_new$Vertical_Break - combined_data_new$Horizontal_Break

#rename columns back
colnames(combined_data_new)[colnames(combined_data_new) == "Horizontal_Break"] <- "pfx_x"
colnames(combined_data_new)[colnames(combined_data_new) == "Vertical_Break"] <- "pfx_z"

#----------------------Clean 2023 Data----------------------
#Make Event Blanks NAs
data2023$events[data2023$events == "" | data2023$events == " "] <- NA

#Replace NAs in Events column with values from description column
data2023_new <- data2023
data2023_new$events[is.na(data2023_new$events)] <- data2023_new$description[is.na(data2023_new$events)]

#Replace strikeout (events) with description value
data2023_new <- data2023_new %>%
  mutate(events = if_else(events == "strikeout", description, events))

#Left Join data and weights
data2023_new <- left_join(LinearWeights, data2023_new, by = c("Event" = "events"))

#Take abs value of Horizontal break and convert to Inches and abs value of horizontal release
data2023_new$pfx_x <- abs(data2023_new$pfx_x) * 12
data2023_new$pfx_z <- data2023_new$pfx_z * 12

data2023_new$release_pos_x <- abs(data2023_new$release_pos_x)

#Rename Columns for Differential Break
colnames(data2023_new)[colnames(data2023_new) == "pfx_x"] <- "Horizontal_Break"
colnames(data2023_new)[colnames(data2023_new) == "pfx_z"] <- "Vertical_Break"

#Add in Differential Break
data2023_new$Differential_Break <- data2023_new$Vertical_Break - data2023_new$Horizontal_Break

#rename columns back
colnames(data2023_new)[colnames(data2023_new) == "Horizontal_Break"] <- "pfx_x"
colnames(data2023_new)[colnames(data2023_new) == "Vertical_Break"] <- "pfx_z"


# Training data
training <- combined_data_new 

training <- subset(training, select = c(Linear_weight, release_speed, pfx_x, pfx_z, release_spin_rate,
                                        release_extension, release_pos_z, release_pos_x, player_name,
                                        pitcher, Differential_Break, pitch_type))
training <- na.omit(training)

x <- training[, c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                  "pfx_x", "pfx_z", "Differential_Break")]
y <- training$Linear_weight

# Test data
testing <- data2023_new 

testing <- subset(testing, select = c(Linear_weight, release_speed, pfx_x, pfx_z, release_spin_rate,
                                      release_extension, release_pos_z, release_pos_x, player_name,
                                      pitcher, Differential_Break, pitch_type))
testing <- na.omit(testing)

x_test <- as.matrix(testing[, c("release_speed", "release_extension", "release_pos_x", "release_pos_z",
                                "pfx_x", "pfx_z", "Differential_Break")])

# Define the parameter grid
param_grid <- expand.grid(
  ntree = c(100, 125, 150),  
  depth = c(3, 5, 7),        
  alpha = c(0.01, 0.1, 0.5)  
)

# Initialize variables to store best parameters and correlation
best_params <- NULL
best_correlation <- -Inf

# Iterate over parameter combinations
for (i in 1:nrow(param_grid)) {
  # Train xgboost model with current parameter combination
  model_xgb <- xgboost(data = as.matrix(x), label = y,
                       nrounds = param_grid$ntree[i],
                       max_depth = param_grid$depth[i],
                       alpha = param_grid$alpha[i],
                       objective = "reg:squarederror")
  
# Make predictions on test data
newdata <- xgb.DMatrix(data = x_test)
predictions <- predict(model_xgb, newdata)
  
# Combine predictions with test data
combined <- cbind(as.data.frame(predictions), testing)
  
  
# Add pitch_type to the final dataset
combined$pitch_type <- testing$pitch_type
  
  PredictionFinal <- combined %>%
    group_by(pitcher, player_name, pitch_type) %>%
    summarise(xRV = sum(predictions, na.rm = TRUE),
              n = n(),
              'xRV/100' = 100 * sum(predictions, na.rm = TRUE) / n,
              Velocity = mean(release_speed, na.rm = TRUE),
              Horizontal_Break = mean(pfx_x, na.rm = TRUE),
              Vertical_Break = mean(pfx_z, na.rm = TRUE),
              Spin_Rate = mean(release_spin_rate, na.rm = TRUE),
              Extension = mean(release_extension, na.rm = TRUE),
              Vertical_Release = mean(release_pos_z, na.rm = TRUE),
              Horizontal_Release = mean(release_pos_x, na.rm = TRUE),
              Pitches = n(),
              RV = sum(Linear_weight, na.rm = TRUE),
              .groups = "drop")  # Add .groups argument to override grouped output
  
  PredictionFinal <- filter(PredictionFinal, Pitches >= 100)
  
  
  # Calculate correlation between xRV and RV
  correlation <- cor(PredictionFinal$xRV, PredictionFinal$RV)
  
  # Check if current parameter combination gives better correlation
  if (correlation > best_correlation) {
    best_correlation <- correlation
    best_params <- param_grid[i, ]
  }
}

# Print the best parameters and correlation
cat("Best ntree:", best_params$ntree, "\n")
cat("Best depth:", best_params$depth, "\n")
cat("Best alpha:", best_params$alpha, "\n")
cat("Best correlation:", best_correlation, "\n")

#write to a csv
write.csv(PredictionFinal, "PredictFinal.csv")

#Save model to working directory
file_path <- "/Users/rileyfeltner/Desktop/Baseball Research/Stuff +/model_xgb.rds"

saveRDS(model_xgb, file = file_path)

#Checkout the R^2
lm_model <- lm(RV ~ xRV, data = PredictionFinal)

r_squared <- summary(lm_model)$r.squared

print(r_squared)

#-----------------Scale xRV/100 to Stuff + Scale----------------
PredictionFinal$`xRV/100ScaledNegative` <- PredictionFinal$`xRV/100` - max(PredictionFinal$`xRV/100`)

PredictionFinal$`ABSxRV/100ScaledNeg` <- abs(PredictionFinal$`xRV/100ScaledNegative`)

PredictionFinal$`Stuff+` <- (PredictionFinal$`ABSxRV/100ScaledNeg` / mean(PredictionFinal$`ABSxRV/100ScaledNeg`)) * 100

#---------------Filter by Pitch Types-------------
#---------------Fastball---------------
AllFBs <- filter(PredictionFinal, pitch_type == "FA" | pitch_type == "FF" | pitch_type == "SI" | 
                   pitch_type == "FT" | pitch_type == "FC")

AllFBsIndyScaled <- AllFBs[,-(16:18)]

AllFBsIndyScaled$`xRV/100ScaledNegative` <- AllFBsIndyScaled$`xRV/100` - max(AllFBsIndyScaled$`xRV/100`)

AllFBsIndyScaled$`ABSxRV/100ScaledNeg` <- abs(AllFBsIndyScaled$`xRV/100ScaledNegative`)

AllFBsIndyScaled$`Stuff+` <- (AllFBsIndyScaled$`ABSxRV/100ScaledNeg` / mean(AllFBsIndyScaled$`ABSxRV/100ScaledNeg`)) * 100

write.csv(AllFBs, "AllFBs.csv")
write.csv(AllFBsIndyScaled, "AllFBsIndyScaled.csv")

#-------------Sinkers------------
SI <- filter(PredictionFinal, pitch_type == "SI" | pitch_type == "FT" )

SIIndyScaled <- SI[,-(16:18)]

SIIndyScaled $`xRV/100ScaledNegative` <- SIIndyScaled $`xRV/100` - max(SIIndyScaled $`xRV/100`)

SIIndyScaled $`ABSxRV/100ScaledNeg` <- abs(SIIndyScaled $`xRV/100ScaledNegative`)

SIIndyScaled $`Stuff+` <- (SIIndyScaled $`ABSxRV/100ScaledNeg` / mean(SIIndyScaled $`ABSxRV/100ScaledNeg`)) * 100

write.csv(SI, "SI.csv")
write.csv(SIIndyScaled, "SIIndyScaled.csv")

#------------Cutters-----------
CT <- filter(PredictionFinal, pitch_type == "FC")

CTIndyScaled <- CT[,-(16:18)]

CTIndyScaled $`xRV/100ScaledNegative` <- CTIndyScaled $`xRV/100` - max(CTIndyScaled $`xRV/100`)

CTIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CTIndyScaled $`xRV/100ScaledNegative`)

CTIndyScaled $`Stuff+` <- (CTIndyScaled $`ABSxRV/100ScaledNeg` / mean(CTIndyScaled $`ABSxRV/100ScaledNeg`)) * 100

write.csv(CT, "CT.csv")
write.csv(CTIndyScaled, "CTIndyScaled")

#-----------Sliders-----------
SL <- filter(PredictionFinal, pitch_type == "SL" | pitch_type == "ST")

SLIndyScaled <- SL[,-(16:18)]

SLIndyScaled $`xRV/100ScaledNegative` <- SLIndyScaled $`xRV/100` - max(SLIndyScaled $`xRV/100`)

SLIndyScaled $`ABSxRV/100ScaledNeg` <- abs(SLIndyScaled $`xRV/100ScaledNegative`)

SLIndyScaled $`Stuff+` <- (SLIndyScaled $`ABSxRV/100ScaledNeg` / mean(SLIndyScaled $`ABSxRV/100ScaledNeg`)) * 100

write.csv(SL, "SL.csv")
write.csv(SLIndyScaled, "SLIndyScaled")

#-----------Curveballs-----------
CB <- filter(PredictionFinal, pitch_type == "CU" | pitch_type == "SV")

CBIndyScaled <- CB[,-(16:18)]

CBIndyScaled $`xRV/100ScaledNegative` <- CBIndyScaled $`xRV/100` - max(CBIndyScaled $`xRV/100`)

CBIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CBIndyScaled $`xRV/100ScaledNegative`)

CBIndyScaled $`Stuff+` <- (CBIndyScaled $`ABSxRV/100ScaledNeg` / mean(CBIndyScaled $`ABSxRV/100ScaledNeg`)) * 100

write.csv(CB, "CB.csv")
write.csv(CBIndyScaled, "CBIndyScaled")

#------------Changeups-----------
CH <- filter(PredictionFinal, pitch_type == "CH" | pitch_type == "FS" | pitch_type == "SC" | pitch_type == "FO")

CHIndyScaled <- CH[,-(16:18)]

CHIndyScaled $`xRV/100ScaledNegative` <- CHIndyScaled $`xRV/100` - max(CHIndyScaled $`xRV/100`)

CHIndyScaled $`ABSxRV/100ScaledNeg` <- abs(CHIndyScaled $`xRV/100ScaledNegative`)

CHIndyScaled $`Stuff+` <- (CHIndyScaled $`ABSxRV/100ScaledNeg` / mean(CHIndyScaled $`ABSxRV/100ScaledNeg`)) * 100

write.csv(CH, "CH.csv")
write.csv(CHIndyScaled, "CHIndyScaled")

#---------------Tables--------------
top_10<- head(PredictionFinal[order(PredictionFinal$`Stuff+`, decreasing = TRUE), c("player_name", "pitch_type","Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Spin_Rate", "Extension", "Vertical_Release", "Horizontal_Release")], 10)

table <- kable(top_10, caption = "Top 10 Stuff+",
               col.names = c("Player Name", "Pitch Type", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Spin Rate", "Extension", "Vertical Release", "Horizontal Release"))

print(tableFBs)

write.csv(top_10, file = "top_10_Stuff_plus.csv")





library(kableExtra)

# Create the table
top_10 <- head(PredictionFinal[order(PredictionFinal$`Stuff+`, decreasing = TRUE), c("player_name", "pitch_type", "Stuff+", "Velocity", "Vertical_Break", "Horizontal_Break", "Spin_Rate", "Extension", "Vertical_Release", "Horizontal_Release")], 10)

# Create the caption
caption <- "Top 10 Stuff+"

# Generate the table with kableExtra
table <- kable(top_10, caption = caption,
               col.names = c("Player Name", "Pitch Type", "Stuff+", "Velocity", "Vertical Break", "Horizontal Break", "Spin Rate", "Extension", "Vertical Release", "Horizontal Release")) %>%
  kable_styling()

# Save the table as a PNG file using the png function
png("table.png")
print(table)
dev.off()





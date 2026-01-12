# Objective: Figure out which passengers were most likely to be transported to 
# Alternative Dimension during the accident
# Create a model that beats previous score of 78%
#install.packages(c("rpart", "rpart.plot", "psych", "ggplot2"))
library(rpart)
library(rpart.plot)
library(psych)
library(ggplot2)

train <- read.csv('~/Desktop/DataScience/Kaggle/Titanic.Rproj/spaceship-titanic/train.csv')
test <- read.csv('~/Desktop/DataScience/Kaggle/Titanic.Rproj/spaceship-titanic/test.csv')

# Clean up NA's and stuff
clean_missing <- function(df) {
  rownames(df) <- NULL
  
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- trimws(x)  # remove leading/trailing spaces
      x[x %in% c("NA", "na", "")] <- NA
    }
    return(x)
  })
  
  return(df)
}

# Apply to both datasets
train <- clean_missing(train)
test <- clean_missing(test)

# Create a column so we can differentiate between the test set and training set
train["test"] <- FALSE
test["test"] <- TRUE
test["Transported"] <- NA

# Combine the sets for convenience
ship <- rbind(train, test)
summary(ship)

prop.table(table(train$Transported))

# Exploratory Data Analysis ====================================================
# Categorical Variables ======

# Home Planet ===
# Insights: Passengers from Earth are most likely to not be transported
# Home Planet does not influence Destination, most go to TRAPPIST
# ===============

homePlanet_T <- table(ship["HomePlanet"]) # 288 NA's
prop.table(homePlanet_T) * 100 # Earth: 53%, Europa: 24%, Mars: 21%, 2% NA

# HomePlanet and Transported Status
HomePlanetTransport_T <- table(ship$HomePlanet, ship$Transported)
prop.table(HomePlanetTransport_T) * 100


# Relationship between HomePlanet and Destination?
# More likely not be transported if from Earth
homeDestination_T <- table(ship$HomePlanet, ship$Destination)
prop.table(homeDestination_T) * 100

# Earth
earth <- ship[ship$HomePlanet == "Earth",]
earthDest_T <- table(earth$Destination)
prop.table(earthDest_T) * 100 # 68% of Earth Passengers go to TRAPPIST

# Mars
mars <- ship[ship$HomePlanet == "Mars",]
marsDest_T <- table(mars$Destination)
prop.table(marsDest_T) * 100 # 84% of Mars Passengers go to TRAPPIST

# Europa
europa <- ship[ship$HomePlanet == "Europa",]
europaDest_T <- table(europa$Destination)
prop.table(europaDest_T) * 100 # 55% of Europa Passengers go to TRAPPIST


# Destination ===
# Insights: Most go to Trappist, few to PSO
# Equal-ish distribution for transport status
#================
destination_T <- table(ship$Destination)
prop.table(destination_T) * 100 # 68% TRAPPIST, 20% Cancri, 9% PSO, 2% unknown

# Relationship between Destination and Transportation Status
destTransport_T <- table(ship$Destination, ship$Transported)
prop.table(destTransport_T) * 100 # Relatively equal distribution

# Cabin ===
# Cabin needs to be split into different columns: Floor, Number, Side
safe_split <- function(cabin) {
  if (is.na(cabin)) return(c(NA, NA, NA))
  parts <- unlist(strsplit(cabin, "/"))
  length(parts) <- 3  # pad if needed
  return(parts)
}

split_cabins <- do.call(rbind, lapply(ship$Cabin, safe_split))
colnames(split_cabins) <- c("CabinDeck", "CabinNumber", "CabinSide")
ship <- cbind(ship, split_cabins)
ship$CabinNumber <- as.numeric(ship$CabinNumber)
summary(ship)

# CabinSide ===
# Insights: Not Important
# =============
cabinSide_T <- table(ship$CabinSide)
prop.table(cabinSide_T) * 100 # Evenly split

cabinSideTransport_T <- table(ship$Transported, ship$CabinSide)
rownames(cabinSideTransport_T) <- c("Not Transported", "Transported")
barplot(cabinSideTransport_T, xlab="Side", ylab="Count", beside=TRUE, legend=TRUE)
# More Likely On S

# CabinDeck ===
# Insights: Not Important
# =============
cabinDeck_T <- table(ship$Transported, ship$CabinDeck )
rownames(cabinDeck_T) <- c("Not Transported", "Transported")
barplot(cabinDeck_T, xlab="Deck", ylab="Count", beside=TRUE, legend=TRUE)
# More likely on B, C and G

# CryoSleep ===
# Insights: CryoSleep could be important for Transport Status
# 67% of those outside of Cryosleep were not Transported
# 81% of those inside Cryo were Transported to a different dimension
# People In CryoSleep never spent money
# 
# =============

cryo_T <- table(ship$CryoSleep)
prop.table(cryo_T) * 100 # 1/3 opted for CryoSleep


# Does cryo status influence transport?
InCryo_T <- table(ship$CryoSleep, ship$Transported)
prop.table(InCryo_T) * 100 # 81% of people in CryoSleep were Transported
rownames(InCryo_T) <- c("Not Cryo", "Cryo")
barplot(InCryo_T, xlab="Transported", ylab="Count", beside=TRUE, legend=TRUE)

# Did people in CryoSleep Spend Money?
ship["MoneySpent"] <- rowSums(ship[, c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")], na.rm = TRUE)
table(ship$CryoSleep, ship$MoneySpent > 0) # People in CryoSleep never spent money!

# Were any VIPS in CryoSleep?
cryovip_T <- table(ship$CryoSleep, ship$VIP)
cryovip_T # 28 in cryo were VIP
prop.table(cryovip_T) * 100

# Was a specific floor associated with CryoSleep?
cabinCryo_T <- table(ship$CryoSleep, ship$CabinDeck)
cabinCryo_T
prop.table(cabinCryo_T) * 100

cryo <- ship[ship$CryoSleep == "True",]
cabinCryo_T <- table(cryo$CryoSleep, cryo$CabinDeck)
cabinCryo_T
prop.table(cabinCryo_T) * 100 # most people are on F and G regardless

summary(cryo$Age)
hist(ship$Age)
hist(cryo$Age)
table(ship$HomePlanet, ship$CryoSleep)
table(ship$Destination, ship$CryoSleep)
names(cryo)

# CabinDeck: More on G and B
cryoDeck_T <- table(ship$CryoSleep, ship$CabinDeck)
rownames(cryoDeck_T) <- c("Not Cryo", "Cryo")
barplot(cryoDeck_T, xlab="Deck", ylab="Count", beside=TRUE, legend=TRUE)

# CabinSide for Cryo: Completely the same
cryoSide_T <- table(ship$CryoSleep, ship$CabinSide)
rownames(cryoSide_T) <- c("Not Cryo", "Cryo")
barplot(cryoSide_T, xlab="Deck", ylab="Count", beside=TRUE, legend=TRUE)

# Destination
cryoDest_T <- table(ship$CryoSleep, ship$Destination)
rownames(cryoDest_T) <- c("Not Cryo", "Cryo")
barplot(cryoDest_T, xlab="Deck", ylab="Count", beside=TRUE, legend=TRUE)

# Homeplanet
cryoHome_T <- table(ship$CryoSleep, ship$HomePlanet)
rownames(cryoHome_T) <- c("Not Cryo", "Cryo")
barplot(cryoHome_T, xlab="Deck", ylab="Count", beside=TRUE, legend=TRUE)



# VIP ===
# Insights: More likely to not be transported if VIP
# 94% of VIP's spent money, Median: 2743, Mean: 4596, Max: 33666
# Between 18-73 years old (All adults?)
# No VIPs From Earth
# No VIPs on Decks G or T
# 90% of VIPs were not in CryoSleep
# =======

vip_T <- table(ship$VIP)
vip_T
prop.table(vip_T) * 100 # only 2% of passengers are VIP

vipTransport_T <- table(ship$VIP, ship$Transported)
prop.table(vipTransport_T) * 100

# Split up VIP and not VIP
vip <- ship[ship$VIP == "True",]
novip <- ship[ship$VIP == "False",]

vipTransport_T <- table(vip$VIP, vip$Transported)
prop.table(vipTransport_T) * 100 # only 38% of VIP were transported

novipTransport_T <- table(novip$VIP, novip$Transported)
prop.table(novipTransport_T) * 100 # Equal between non VIP

# Did all VIP's spend money?
moneySpent_T <- table(ship$VIP, ship$MoneySpent > 0) # No, 35 didn't spend money
prop.table(moneySpent_T) * 100 # (94% spent money)
# Were there VIP only areas? (spa, VRDeck?)
# We can check to see if they paid money to see if they were allowed in

summary(vip$Spa)
summary(novip$Spa) # Both Spent Money

summary(vip$Spa)
summary(novip$Spa) # Both Spent Money

# How much did VIP spend on average?
summary(vip$MoneySpent) # Median: 2743, Mean: 4596, Max: 33666
summary(novip$MoneySpent) # Median: 704, Mean: 1358, Max: 35987

# Were VIPs on certain decks?
deckvip_t <- table(vip$CabinDeck)
deckvip_t # VIP's never on G or T

# Were VIPs from a certain homeplanet?
planetvip_t <- table(vip$HomePlanet)
planetvip_t # No VIPS from Earth!

# Certain Destination?
destinationvip_t <- table(vip$Destination)
destinationvip_t # no


# Age determine VIP?
summary(vip$Age) # Youngest 18, oldest 73, mean 36.66, median 33


# if non vip spent like vip, would they be outliers? 
boxplot(novip$MoneySpent) # no, tons of outliers

summary(ship)

# Continuous Variables ======
# Convert Transported to Binary Values and store in Transported_bin
ship$Transported <- as.factor(ship$Transported)
ship$Transported_bin <- factor(ifelse(ship$Transported == "True", 1, 0),
                               levels = c(0, 1))

# Age ===
# Insights: Not Important for determining Transportation
# =======
hist(ship$Age)
summary(ship$Age) # Mean: 28.77, Median: 27, Max: 79
boxplot(ship$Age) # Lots of Outliers (~65+)

# age influence transportation status?
age <- na.omit(ship[, c("Age", "Transported_bin")])
age_test <- t.test(Age ~ Transported_bin, data = age)
age_test 

# Age Transport
AgeTransport <- table(ship$Transported, ship$Age)
rownames(AgeTransport) <- c("Not Transported", "Transported")
barplot(AgeTransport, xlab="Age", ylab="Count", beside=TRUE, legend=TRUE)

ageZero <- ship[ship$Age == 0,]
table(ageZero$Transported) # 80 % of new borns were transported


# Did different HomePlanets have different Age stats?
summary(earth$Age) # Median 23, Mean 25.96
summary(mars$Age) # Median 28, Mean 29.46
summary(europa$Age) # Median 33, Mean 34.34

# Destination have different age stats?
destTrap <- ship[ship$Destination == "TRAPPIST-1e",]
cancTrap <- ship[ship$Destination == "55 Cancri e",]
psoTrap <- ship[ship$Destination == "PSO J318.5-22",]


summary(destTrap$Age)
summary(cancTrap$Age)
summary(psoTrap$Age) # All very similar

# Correlation
cor(age$Age, as.numeric(age$Transported_bin)) # -0.075, very weak


# MoneySpent (Combination of Spa, VRDeck, ShoppingMall, etc.) ===
hist(ship$MoneySpent)
summary(ship$MoneySpent) # Mean: 1433, Median: 716, Max: 35987


# MoneySpent correlation with VIP Status?
ship$VIP <- as.factor(ship$VIP)

moneySpent <- na.omit(ship[, c("VIP","MoneySpent", "Age", "Transported_bin")])

cor(as.numeric(moneySpent$MoneySpent), as.numeric(moneySpent$VIP)) # 0.118, weak

# MoneySpent correlation with Transportation Status
cor(moneySpent$MoneySpent, as.numeric(moneySpent$Transported_bin)) # -0.199, weak

# MoneySpent correlation with Age?
cor(moneySpent$MoneySpent, moneySpent$Age) # 0.18, weak


# VRDeck ===
summary(ship$VRDeck) # Mean: 306.8, Max 24133
VRSpending <- ship[(ship$VRDeck > 0) & (ship$VRDeck < 200),]
hist(VRSpending$VRDeck)

gamers <- ship[ship$VRDeck > 0, c("VRDeck", "Transported")]
VR_T <- table(gamers$VRDeck > 0, gamers$Transported)
prop.table(VR_T) * 100 # Of those who spent money at the food court, 27% were transported

# Spa ===
summary(ship$Spa) # Mean: 308.5, Max: 22408
spaLovers <- ship[ship$Spa > 0, c("Spa", "Transported")]
spaLovers_T <- table(spaLovers$Spa > 0, spaLovers$Transported)
prop.table(spaLovers_T) * 100 # Of those who spent money at the food court, 27.7% were transported

# Shopping Mall ===
summary(ship$ShoppingMall) # Mean: 174.9, Max: 23492
shoppers <- ship[ship$ShoppingMall > 0, c("ShoppingMall", "Transported")]
shoppers_T <- table(shoppers$ShoppingMall > 0, shoppers$Transported)
prop.table(shoppers_T) * 100 # Of those who spent money at the food court, 31.5% were transported

# FoodCourt ===
summary(ship$FoodCourt) # Mean: 452, Max: 29813
foodCourtSpenders <- ship[ship$FoodCourt > 0, c("FoodCourt", "Transported")]
food_T <- table(foodCourtSpenders$FoodCourt > 0, foodCourtSpenders$Transported)
prop.table(food_T) * 100 # Of those who spent money at the food court, 34.5% were transported

# RoomService ===
summary(ship$RoomService) # Mean: 222.9, Max: 14327
room <- ship[ship$RoomService > 0, c("RoomService", "Transported")]
room_T <- table(room$RoomService > 0, room$Transported)
prop.table(room_T) * 100 # Of those who spent money at the food court, 25% were transported


# Data Cleaning ================================================================
# Notes:Groups usually involve family and friends according to doc. May be important for 
# determining things like CryoSleep, Cabin, Destination, HomePlanet, and VIP. 
# Additionally, we can create a column for "group member transported" which could
# be used to determine Transported.

# Once this is done, we can use the information above to fill in missing values.


# Clean HomePlanet ===
# HomePlanet could be important for determining Transportation Status
# Age, Destination, and CryoSleep could help determine HomePlanet
# We could also see if anyone in a matching group has their HomePlanet recorded

# Check if Group has someone with their HomePlanet recorded and use that value

# Split PassengerId into two columns: GroupId and ID
idSplit <- function(passengerId) {
  if (is.na(passengerId)) return(c(NA,NA))
  parts <- unlist(strsplit(passengerId, "_"))
  length(parts) <- 2
  return(parts)
}


splitId <- do.call(rbind, lapply(ship$PassengerId, idSplit))
colnames(splitId) <- c("GroupId", "ID")
ship <- cbind(ship, splitId)


# Group ID becomes the key, HomePlanet becomes the Value
ship$HomePlanet[ship$HomePlanet == ""] <- NA
create_key_value_vector <- function(keys, values) {
  # Create a named vector
  key_value_vector <- setNames(values, keys)
  return(key_value_vector)
}

keys <- ship$GroupId
values <- ship$HomePlanet

# Create the key-value vector using the function
key_value_result <- create_key_value_vector(keys, values)

# Print the result
sum(is.na(key_value_result)) # 288 Missing Values
IDLookUp <- na.omit(key_value_result)

ship$HomePlanet[is.na(ship$HomePlanet)] <- IDLookUp[ship$GroupId[is.na(ship$HomePlanet)]]

sum(is.na(ship$HomePlanet)) # 157 missing HomePlanet values left

missingHomePlanet <- ship[is.na(ship$HomePlanet), ] # all of these were the only ones in their groups

# Clean Destination ===
ship$Destination[ship$Destination == ""] <- NA

keys <- ship$GroupId
values <- ship$Destination

# Create the key-value vector using the function
key_value_result <- create_key_value_vector(keys, values)

# Print the result
sum(is.na(key_value_result)) # 274 Missing Values
IDLookUp <- na.omit(key_value_result)

ship$Destination[is.na(ship$Destination)] <- IDLookUp[ship$GroupId[is.na(ship$Destination)]]

ship[is.na(ship$Destination),] 
sum(is.na(ship$Destination)) # 154 Missing Values left, all the only one in their group

# We're just going to make these unknown for now
ship$Destination[is.na(ship$Destination)] <- "Unknown"
sum(is.na(ship$Destination)) # 0 left


# Clean HomePlanet Continued ===
# This is going to be determined using age, destination, CryoSleep and VIP 
# (since we know that VIP's aren't from Earth)
ship$HomePlanet[is.na(ship$HomePlanet)] <- "Unknown"

sum(is.na(ship$HomePlanet)) # 0 left

# Clean CryoSleep ===
# Passengers in CryoSleep never spent money
ship$CryoSleep[ship$CryoSleep == ""] <- NA
ship$CryoSleepKnown[is.na(ship$CryoSleep)] <- 0
ship$CryoSleepKnown[!is.na(ship$CryoSleep)] <- 1

# OOF Score
ship$fold5Cryo <- sample(rep(1:5, length.out = nrow(ship)))
ship$CryoSleep_f <- factor(ship$CryoSleep)
trainCryo <- ship[!is.na(ship$CryoSleep) & ship$test == FALSE,]
head(trainCryo)


cryoModel1 <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo[trainCryo$fold5Cryo != 1,], family=binomial())
cryoModel2 <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo[trainCryo$fold5Cryo != 2,], family=binomial())
cryoModel3 <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo[trainCryo$fold5Cryo != 3,], family=binomial())
cryoModel4 <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo[trainCryo$fold5Cryo != 4,], family=binomial())
cryoModel5 <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo[trainCryo$fold5Cryo != 5,], family=binomial())

ship$CryoSleepScore <- NA
ship$CryoSleepScore[(ship$fold5Cryo == 1) & (!is.na(ship$CryoSleep))] <- predict(cryoModel1, newdata=trainCryo[trainCryo$fold5Cryo == 1,], type="response")
ship$CryoSleepScore[(ship$fold5Cryo == 2) & (!is.na(ship$CryoSleep))] <- predict(cryoModel2, newdata=trainCryo[trainCryo$fold5Cryo == 2,], type="response")
ship$CryoSleepScore[(ship$fold5Cryo == 3) & (!is.na(ship$CryoSleep))] <- predict(cryoModel3, newdata=trainCryo[trainCryo$fold5Cryo == 3,], type="response")
ship$CryoSleepScore[(ship$fold5Cryo == 4) & (!is.na(ship$CryoSleep))] <- predict(cryoModel4, newdata=trainCryo[trainCryo$fold5Cryo == 4,], type="response")
ship$CryoSleepScore[(ship$fold5Cryo == 5) & (!is.na(ship$CryoSleep))] <- predict(cryoModel5, newdata=trainCryo[trainCryo$fold5Cryo == 5,], type="response")
ship$CryoSleepScore[ship$MoneySpent > 0] <- 0

sum(is.na(ship$CryoSleepScore)) # 480

cryoModel <- glm(formula = CryoSleep_f ~ HomePlanet + CabinDeck + Destination, data = trainCryo, family=binomial())
ship$CryoSleepScore[is.na(ship$CryoSleepScore)] <- predict(cryoModel, newdata=ship[is.na(ship$CryoSleep_f),], type="response")
sum(is.na(ship$CryoSleepKnown)) # 0
sum(is.na(ship$CryoSleepScore)) # 0


# Cleaning CabinDeck ===
keys <- ship$GroupId
values <- ship$CabinDeck

# Create the key-value vector using the function
key_value_result <- create_key_value_vector(keys, values)

# Print the result
sum(is.na(key_value_result)) # 288 Missing Values
IDLookUp <- na.omit(key_value_result)

ship$CabinDeck[is.na(ship$CabinDeck)] <- IDLookUp[ship$GroupId[is.na(ship$CabinDeck)]]

sum(is.na(ship$CabinDeck))

missingCabinDeck <- ship[is.na(ship$CabinDeck), ] # all of these were the only ones in their groups
dim(missingCabinDeck) # 162 Missing

# Cleaning CabinSide ===
keys <- ship$GroupId
values <- ship$CabinSide

# Create the key-value vector using the function
key_value_result <- create_key_value_vector(keys, values)

# Print the result
sum(is.na(key_value_result)) # 299 Missing Values
IDLookUp <- na.omit(key_value_result)

ship$CabinSide[is.na(ship$CabinSide)] <- IDLookUp[ship$GroupId[is.na(ship$CabinSide)]]

sum(is.na(ship$CabinSide))

missingCabinSide <- ship[is.na(ship$CabinSide), ] # all of these were the only ones in their groups
dim(missingCabinSide) # 162 Missing

# Cleaning Cabin Number ===
keys <- ship$GroupId
values <- ship$CabinNumber

# Create the key-value vector using the function
key_value_result <- create_key_value_vector(keys, values)

# Print the result
sum(is.na(key_value_result)) # 99 Missing Values
IDLookUp <- na.omit(key_value_result)

ship$CabinNumber[is.na(ship$CabinNumber)] <- IDLookUp[ship$GroupId[is.na(ship$CabinNumber)]]

sum(is.na(ship$CabinNumber))

missingCabinNumber <- ship[is.na(ship$CabinNumber), ] # all of these were the only ones in their groups
dim(missingCabinNumber) # 162 Missing

# Cleaning Cabin ===
# Instead of adding in random data, we will just encode the rest with Unknown and -1
ship$CabinSide[is.na(ship$CabinSide)] <- "Unknown"
ship$CabinDeck[is.na(ship$CabinDeck)] <- "Unknown"
ship$CabinNumber[is.na(ship$CabinNumber)] <- -1  

sum(is.na(ship$CabinSide)) # 0
sum(is.na(ship$CabinDeck)) # 0
sum(is.na(ship$CabinNumber)) # 0


# Binning CabinDeck 
prop.table(table(ship$CabinDeck, ship$HomePlanet)) * 100
prop.table(table(ship$CabinDeck, ship$CryoSleep)) * 100

# Informing Bins based on Transport 
DeckTransport <- table(ship$CabinDeck, ship$Transported)
prop.table(DeckTransport) * 100 
colnames(DeckTransport) <- c("False", "True")
barplot(DeckTransport, xlab="Transported", ylab="Count", beside=TRUE, legend=TRUE)


ship$CabinDeck_bin[(ship$CabinDeck == "F")] <- "F"
ship$CabinDeck_bin[(ship$CabinDeck == "G")] <- "G"
ship$CabinDeck_bin[(ship$CabinDeck == "E") | (ship$CabinDeck == "D")] <- "Middle"
ship$CabinDeck_bin[(ship$CabinDeck == "C") | (ship$CabinDeck == "B") | (ship$CabinDeck == "A") | (ship$CabinDeck == "T")] <- "Upper"
ship$CabinDeck_bin[(ship$CabinDeck == "Unknown")] <- "Unknown"
sum(is.na(ship$CabinDeck_bin))
head(ship$CabinDeck_bin)


# Cleaning VRDeck ===
med <- median(ship$VRDeck[ship$CryoSleep == "False"], na.rm = TRUE)
ship$VRDeck[is.na(ship$VRDeck) & ship$CryoSleep == "True"] <- 0
ship$VRDeck[is.na(ship$VRDeck)] <- med
sum(is.na(ship$VRDeck))

# Cleaning ShoppingMall ===
med <- median(ship$ShoppingMall[ship$CryoSleep == "False"], na.rm = TRUE)
ship$ShoppingMall[is.na(ship$ShoppingMall) & ship$CryoSleep == "True"] <- 0
ship$ShoppingMall[is.na(ship$ShoppingMall)] <- med
sum(is.na(ship$ShoppingMall))

# Cleaning RoomService ===
med <- median(ship$RoomService[ship$CryoSleep == "False"], na.rm = TRUE)
ship$RoomService[is.na(ship$RoomService) & ship$CryoSleep == "True"] <- 0
ship$RoomService[is.na(ship$RoomService)] <- med
sum(is.na(ship$RoomService))

# Cleaning Spa ===
med <- median(ship$Spa[ship$CryoSleep == "False"], na.rm = TRUE)
ship$Spa[is.na(ship$Spa) & ship$CryoSleep == "True"] <- 0
ship$Spa[is.na(ship$Spa)] <- med
sum(is.na(ship$Spa))

# Cleaning FoodCourt ===
med <- median(ship$FoodCourt[ship$CryoSleep == "False"], na.rm = TRUE)
ship$FoodCourt[is.na(ship$FoodCourt) & ship$CryoSleep == "True"] <- 0
ship$FoodCourt[is.na(ship$FoodCourt)] <- med
sum(is.na(ship$FoodCourt))


# Clean Age ===
# Due to Age having a lot of outliers, we will use the median
# Since HomePlanet seemed to have different medians, we will use those
#ship$Age[ (is.na(ship$Age)) & (ship$HomePlanet == "Earth") ] <- median(earth$Age, na.rm=TRUE)
#ship$Age[ (is.na(ship$Age)) & (ship$HomePlanet == "Mars") ] <- median(mars$Age, na.rm=TRUE)
#ship$Age[ (is.na(ship$Age)) & (ship$HomePlanet == "Europa") ] <- median(europa$Age, na.rm=TRUE)

summary(ship$Age) # 2 Remaining, overall median
ship$Age[is.na(ship$Age) ] <- median(ship$Age, na.rm=TRUE)
summary(ship$Age) # 0 NAs left, Cleaned



# Clean VIP ===
# VIP could be important for determining Transportation Status.
# VIP is going to be determined with a decision tree
# This model is going to be determined off of Age, MoneySpent, HomePlanet, CabinDeck
# VIP is going to get broken in three columns:
# VIPKnown: 1 if the data was provided in the data set, 0 if otherwise
# VIPScore: OOF score
# MoneySpent could be broken down into the individuals later on a revision.
set.seed(1)

# If the score was provided in the data set, 1
# otherwise, 0
ship$VIP[ship$VIP == ""] <- NA
ship$VIPKnown[is.na(ship$VIP)] <- 0
ship$VIPKnown[!is.na(ship$VIP)] <- 1

# OOF Score
ship$fold5 <- sample(rep(1:5, length.out = nrow(ship)))
trainVIP <- ship[!is.na(ship$VIP),]
head(trainVIP)

# Creating VIP models for OOF
vipModel1 <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP[trainVIP$fold5 != 1,], family=binomial())
vipModel2 <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP[trainVIP$fold5 != 2,], family=binomial())
vipModel3 <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP[trainVIP$fold5 != 3,], family=binomial())
vipModel4 <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP[trainVIP$fold5 != 4,], family=binomial())
vipModel5 <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP[trainVIP$fold5 != 5,], family=binomial())

# Predicting OOF scores
ship$VIPScore <- NA
ship$VIPScore[(ship$fold5== 1) & (!is.na(ship$VIP))] <- predict(vipModel1, newdata=trainVIP[trainVIP$fold5 == 1,], type="response")
ship$VIPScore[(ship$fold5 == 2) & (!is.na(ship$VIP))] <- predict(vipModel2, newdata=trainVIP[trainVIP$fold5 == 2,], type="response")
ship$VIPScore[(ship$fold5 == 3) & (!is.na(ship$VIP))] <- predict(vipModel3, newdata=trainVIP[trainVIP$fold5 == 3,], type="response")
ship$VIPScore[(ship$fold5 == 4) & (!is.na(ship$VIP))] <- predict(vipModel4, newdata=trainVIP[trainVIP$fold5 == 4,], type="response")
ship$VIPScore[(ship$fold5 == 5) & (!is.na(ship$VIP))] <- predict(vipModel5, newdata=trainVIP[trainVIP$fold5 == 5,], type="response")

sum(is.na(ship$VIPScore)) # Just unknowns have no score now

# Creating model for unknowns
vipModel <- glm(formula = VIP ~ Age + MoneySpent + HomePlanet + CabinDeck, data = trainVIP, family=binomial())
ship$VIPScore[is.na(ship$VIPScore)] <- predict(vipModel, newdata=ship[is.na(ship$VIP),], type="response")
sum(is.na(ship$VIPScore)) # 0
sum(is.na(ship$VIPKnown)) # 0

# Creating the Model ===========================================================
# Creating the Model off of variables: HomePlanet, CryoSleep, VIP, Age, CabinSide, CabinDeck, CabinNumber, MoneySpent,

ship$HomePlanet_n <- as.factor(ship$HomePlanet)
ship$CryoSleepKnown_n <- factor(ship$CryoSleepKnown)
ship$VIPKnown_n <- as.factor(ship$VIPKnown)
ship$Age_z <- scale(ship$Age)
#ship$MoneySpent_z <- scale(ship$MoneySpent)
ship$CabinSide_n <- as.factor(ship$CabinSide)
ship$CabinDeck_n <- as.factor(ship$CabinDeck)
ship$CabinNumber_z <- scale(ship$CabinNumber)
ship$Destination_n <- as.factor(ship$Destination)
ship$RoomService_z <- scale(ship$RoomService)
ship$ShoppingMall_z <- scale(ship$ShoppingMall)
ship$FoodCourt_z <- scale(ship$FoodCourt)
ship$Spa_z <- scale(ship$Spa)
ship$VRDeck_z <- scale(ship$VRDeck)
ship$CabinDeck_bin <- factor(ship$CabinDeck_bin)

head(ship$CabinDeck_bin)
train <- ship[ship$test == FALSE,]
test <- ship[ship$test == TRUE,]
dim(train)

train$Transported <- as.factor(train$Transported)
train$Transported_bin <- factor(ifelse(train$Transported == "True", 1, 0),
                                levels = c(0, 1))


summary(train)

train <- ship[ship$test == FALSE, ]

model_vars <- c(
  "Transported",
  "Age",
  "HomePlanet_n",
  "CryoSleepScore",
  "VIPScore",
  "CabinSide_n",
  "CabinDeck_n",
  "VRDeck",
  "ShoppingMall",
  "RoomService",
  "FoodCourt",
  "Spa",
  "Destination_n"
)

na_check <- sapply(train[, model_vars], function(x) sum(is.na(x)))
na_check[na_check > 0]


# Model: Random Forest =====
library("randomForest")
set.seed(19)
RFModel <- randomForest(
  formula = Transported ~ Age + HomePlanet_n + CryoSleepScore + VIPScore + CabinSide_n + CabinDeck_n + VRDeck + ShoppingMall + RoomService + FoodCourt + Spa + Destination_n,
  data = train,         
  ntree = 750,           # number of trees (default = 500)
  mtry = 3,              # roughly sqrt(predictors)
  importance = TRUE      
)


TransportPredictions <- predict(RFModel, newdata=test, type = "response")
test$Transported <- TransportPredictions



submission <- data.frame(PassengerId = test$PassengerId,
                         Transported = test$Transported)
submission <- submission[!is.na(submission$PassengerId), ]
write.csv(submission, "~/Desktop/DataScience/Kaggle/Titanic.Rproj/submissionRF1.csv", row.names = FALSE)

# 80.056 Performance

imp <- importance(RFModel)
imp[order(imp[,3], decreasing = TRUE), ] 

prop.table(table(TransportPredictions)) * 100
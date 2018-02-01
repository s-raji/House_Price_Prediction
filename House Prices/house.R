setwd("C:/Users/subra_000/OneDrive/Documents/Kaggle/House Prices")

library(ggplot2)

#Read data

training_set <- read.csv('train.csv')
test <- read.csv("test.csv")
check <- training_set
sample_submission <- read.csv('sample_submission.csv')

summary(training_set)

# There are values in some columns that have NAs which are not NAs but denotes a value. So we will replace such NA values to the meaningful values and then convert them to numerical values

p = ggplot(data = training_set, aes(x = YearBuilt, y = SalePrice )) + 
  geom_point() + geom_smooth(method = "lm")
p
#Year_built- More recently built houses are pricier compared to older houses

ggplot(data = training_set, aes(x = Neighborhood, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = training_set, aes(x = MSZoning, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = training_set, aes(x = LandContour, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = training_set, aes(x = Condition1, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = training_set, aes(x = YearBuilt, y = SalePrice, colour = Heating )) + geom_point() 


# Encoding the categorical data
training_set$MSZoning = as.numeric(factor(training_set$MSZoning, levels = c('A', 'C (all)', 'FV', 'I', 'RH', 'RL', 'RP', 'RM'),
                                          labels = c(1, 2, 3, 4, 5, 6, 7, 8)))
test$MSZoning = as.numeric(factor(test$MSZoning, levels = c('A', 'C (all)', 'FV', 'I', 'RH', 'RL', 'RP', 'RM'),
                                          labels = c(1, 2, 3, 4, 5, 6, 7, 8)))

training_set$Street = as.numeric(factor(training_set$Street, levels = c('Grvl', 'Pave')), labels = c(1:3))

test$Street = as.numeric(factor(test$Street, levels = c('Grvl', 'Pave')), labels = c(1:3))


training_set$Alley <- as.character(training_set$Alley)
training_set$Alley[is.na(training_set$Alley)] = "No Alley"
training_set$Alley = as.numeric(factor(training_set$Alley, levels = c('Grvl', 'Pave', 'No Alley')), labels = c(1:3))

test$Alley <- as.character(test$Alley)
test$Alley[is.na(test$Alley)] = "No Alley"
test$Alley = as.numeric(factor(test$Alley, levels = c('Grvl', 'Pave', 'No Alley')), labels = c(1:3))

training_set$LotShape = as.numeric(factor(training_set$LotShape, levels = c('IR1', 'IR2', 'IR3', 'Reg')), labels = c(1,2,3,4))
test$LotShape = as.numeric(factor(test$LotShape, levels = c('IR1', 'IR2', 'IR3', 'Reg')), labels = c(1,2,3,4))

training_set$LandContour = as.numeric(factor(training_set$LandContour, levels = c('Bnk', 'HLS', 'Low', 'Lvl')), labels = c(1,2,3,4))
test$LandContour = as.numeric(factor(test$LandContour, levels = c('Bnk', 'HLS', 'Low', 'Lvl')), labels = c(1,2,3,4))

training_set$Utilities = as.numeric(factor(training_set$Utilities, levels = c('AllPub','ELO', 'NoSeWa', 'NoSewr')), labels = c(1,2,3,4))
test$Utilities = as.numeric(factor(test$Utilities, levels = c('AllPub','ELO', 'NoSeWa', 'NoSewr')), labels = c(1,2,3,4))
# There are NAs in test$Utilities

training_set$LotConfig = as.numeric(factor(training_set$LotConfig, levels = c('Corner', 'CulDSac', 'FR2', 'FR3', 'Inside')), labels = c(1,2,3,4,5))
test$LotConfig = as.numeric(factor(test$LotConfig, levels = c('Corner', 'CulDSac', 'FR2', 'FR3', 'Inside')), labels = c(1,2,3,4,5))

training_set$LandSlope = as.numeric(factor(training_set$LandSlope, levels = c('Gtl','Mod','Sev' )), labels = c(1,2,3))
test$LandSlope = as.numeric(factor(test$LandSlope, levels = c('Gtl','Mod','Sev' )), labels = c(1,2,3))

training_set$Neighborhood = as.numeric(factor(training_set$Neighborhood, levels = c('Blmngtn', 'Blueste', 'BrDale', 'BrkSide', 'ClearCr', 'CollgCr',
                                                                                    'Crawfor', 'Edwards', 'Gilbert', 'IDOTRR',  'MeadowV', 'Mitchel',
                                                                                   'Names', 'NoRidge', 'NPkVill', 'NridgHt', 'NWAmes', 'OldTown',  
                                                                                    'SWISU', 'Sawyer','SawyerW','Somerst', 'StoneBr', 'Timber', 'Veenker')), labels = c(1:25))
test$Neighborhood = as.numeric(factor(test$Neighborhood, levels = c('Blmngtn', 'Blueste', 'BrDale', 'BrkSide', 'ClearCr', 'CollgCr',
                                                                                    'Crawfor', 'Edwards', 'Gilbert', 'IDOTRR',  'MeadowV', 'Mitchel',
                                                                                    'Names', 'NoRidge', 'NPkVill', 'NridgHt', 'NWAmes', 'OldTown',  
                                                                                    'SWISU', 'Sawyer','SawyerW','Somerst', 'StoneBr', 'Timber', 'Veenker')), labels = c(1:25))

training_set$Condition1 = as.numeric(factor(training_set$Condition1, levels = c('Artery','Feedr', 'Norm', 'PosA', 'PosN', 'RRAe', 'RRAn', 'RRNe', 'RRNn' )), labels = c(1:9))
test$Condition1 = as.numeric(factor(test$Condition1, levels = c('Artery','Feedr', 'Norm', 'PosA', 'PosN', 'RRAe', 'RRAn', 'RRNe', 'RRNn' )), labels = c(1:9))

training_set$Condition2 = as.numeric(factor(training_set$Condition2, levels = c('Artery','Feedr', 'Norm', 'PosA', 'PosN', 'RRAe', 'RRAn', 'RRNe', 'RRNn' )), labels = c(1:9))
test$Condition2 = as.numeric(factor(test$Condition2, levels = c('Artery','Feedr', 'Norm', 'PosA', 'PosN', 'RRAe', 'RRAn', 'RRNe', 'RRNn' )), labels = c(1:9))

training_set$BldgType =  as.numeric(factor(training_set$BldgType, levels = c('1Fam', '2fmCon', 'Duplex', 'Twnhs', 'TwnhsE' )), labels = c(1:5))
test$BldgType =  as.numeric(factor(test$BldgType, levels = c('1Fam', '2fmCon', 'Duplex', 'Twnhs', 'TwnhsE' )), labels = c(1:5))

training_set$HouseStyle =  as.numeric(factor(training_set$HouseStyle, levels = c('1.5Fin', '1.5Unf', '1Story', '2.5Fin', '2.5Unf', '2Story', 'SFoyer', 'SLvl' )), labels = c(1:8))
test$HouseStyle =  as.numeric(factor(test$HouseStyle, levels = c('1.5Fin', '1.5Unf', '1Story', '2.5Fin', '2.5Unf', '2Story', 'SFoyer', 'SLvl' )), labels = c(1:8))

training_set$RoofStyle =  as.numeric(factor(training_set$RoofStyle, levels = c('Flat', 'Gable', 'Gambrel', 'Hip', 'Mansard', 'Shed')), labels = c(1:5))
test$RoofStyle =  as.numeric(factor(test$RoofStyle, levels = c('Flat', 'Gable', 'Gambrel', 'Hip', 'Mansard', 'Shed')), labels = c(1:5))

training_set$RoofMatl=  as.numeric(factor(training_set$RoofMatl, levels = c('ClyTile', 'CompShg', 'Membran', 'Metal', 'Roll', 'Tar&Grv', 'WdShake', 'WdShngl')), labels = c(1:8))
test$RoofMatl=  as.numeric(factor(test$RoofMatl, levels = c('ClyTile', 'CompShg', 'Membran', 'Metal', 'Roll', 'Tar&Grv', 'WdShake', 'WdShngl')), labels = c(1:8))

training_set$Exterior1st =  as.numeric(factor(training_set$Exterior1st, levels = c('AsbShng', 'AsphShn', 'BrkComm', 'BrkFace', 'CBlock', 'CemntBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other', 'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd', 'Wd Sdng', 'WdShing')), labels = c(1:15))
test$Exterior1st =  as.numeric(factor(test$Exterior1st, levels = c('AsbShng', 'AsphShn', 'BrkComm', 'BrkFace', 'CBlock', 'CemntBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other', 'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd', 'Wd Sdng', 'WdShing')), labels = c(1:15))

training_set$Exterior2nd =  as.numeric(factor(training_set$Exterior2nd, levels = c('AsbShng', 'AsphShn', 'Brk Cmn', 'BrkFace', 'CBlock', 'CmentBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other', 'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd', 'Wd Sdng', 'Wd Shng')), labels = c(1:15))
test$Exterior2nd =  as.numeric(factor(test$Exterior2nd, levels = c('AsbShng', 'AsphShn', 'Brk Cmn', 'BrkFace', 'CBlock', 'CmentBd', 'HdBoard', 'ImStucc', 'MetalSd', 'Other', 'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd', 'Wd Sdng', 'Wd Shng')), labels = c(1:15))

training_set$MasVnrType =  as.numeric(factor(training_set$MasVnrType, levels = c('BrkCmn', 'BrkFace', 'None', 'Stone')), labels = c(1:4))
test$MasVnrType =  as.numeric(factor(test$MasVnrType, levels = c('BrkCmn', 'BrkFace', 'None', 'Stone')), labels = c(1:4))

training_set$ExterQual= as.numeric(factor(training_set$ExterQual, levels = c('Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))
test$ExterQual= as.numeric(factor(test$ExterQual, levels = c('Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))

training_set$ExterCond= as.numeric(factor(training_set$ExterCond, levels = c('Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))
test$ExterCond= as.numeric(factor(test$ExterCond, levels = c('Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))

training_set$Foundation= as.numeric(factor(training_set$Foundation, levels = c('BrkTil', 'CBlock',  'PConc', 'Slab', 'Stone', 'Wood' )), labels = c(1:6))
test$Foundation= as.numeric(factor(test$Foundation, levels = c('BrkTil', 'CBlock',  'PConc', 'Slab', 'Stone', 'Wood' )), labels = c(1:6))

training_set$BsmtQual= as.character(training_set$BsmtQual)
training_set$BsmtQual[is.na(training_set$BsmtQual)] = "NB"
training_set$BsmtQual= as.numeric(factor(training_set$BsmtQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NB'  )), labels = c(1:6))
test$BsmtQual= as.character(test$BsmtQual)
test$BsmtQual[is.na(test$BsmtQual)] = "NB"
test$BsmtQual= as.numeric(factor(test$BsmtQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NB'  )), labels = c(1:6))

training_set$BsmtCond = as.character(training_set$BsmtCond)
training_set$BsmtCond[is.na(training_set$BsmtCond)] = "NB"
training_set$BsmtCond= as.numeric(factor(training_set$BsmtCond, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NB' )), labels = c(1:6))
test$BsmtCond = as.character(test$BsmtCond)
test$BsmtCond[is.na(test$BsmtCond)] = "NB"
test$BsmtCond= as.numeric(factor(test$BsmtCond, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NB' )), labels = c(1:6))

training_set$BsmtExposure= as.character(training_set$BsmtExposure)
training_set$BsmtExposure[is.na(training_set$BsmtExposure)] ="NB"
training_set$BsmtExposure= as.numeric(factor(training_set$BsmtExposure, levels = c( 'Gd', 'Av', 'Mn', 'No', 'NB' )), labels = c(1:5))
test$BsmtExposure= as.character(test$BsmtExposure)
test$BsmtExposure[is.na(test$BsmtExposure)] ="NB"
test$BsmtExposure= as.numeric(factor(test$BsmtExposure, levels = c( 'Gd', 'Av', 'Mn', 'No', 'NB' )), labels = c(1:5))

training_set$BsmtFinType1 = as.character(training_set$BsmtFinType1)
training_set$BsmtFinType1[is.na(training_set$BsmtFinType1)] = "NB"
training_set$BsmtFinType1= as.numeric(factor(training_set$BsmtFinType1, levels = c( 'GLQ', 'ALQ', 'BLQ', 'Rec', 'LwQ', 'Unf', 'NB' )), labels = c(1:7))
test$BsmtFinType1 = as.character(test$BsmtFinType1)
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "NB"
test$BsmtFinType1= as.numeric(factor(test$BsmtFinType1, levels = c( 'GLQ', 'ALQ', 'BLQ', 'Rec', 'LwQ', 'Unf', 'NB' )), labels = c(1:7))

training_set$BsmtFinType2 = as.character(training_set$BsmtFinType2)
training_set$BsmtFinType2[is.na(training_set$BsmtFinType2)] = "NB"
training_set$BsmtFinType2= as.numeric(factor(training_set$BsmtFinType2, levels = c( 'GLQ', 'ALQ', 'BLQ', 'Rec', 'LwQ', 'Unf', 'NB' )), labels = c(1:7))
test$BsmtFinType2 = as.character(test$BsmtFinType2)
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "NB"
test$BsmtFinType2= as.numeric(factor(test$BsmtFinType2, levels = c( 'GLQ', 'ALQ', 'BLQ', 'Rec', 'LwQ', 'Unf', 'NB' )), labels = c(1:7))

training_set$Heating= as.numeric(factor(training_set$Heating, levels = c( 'Floor', 'GasA', 'GasW', 'Grav', 'OthW', 'Wall' )), labels = c(1:6))
test$Heating= as.numeric(factor(test$Heating, levels = c( 'Floor', 'GasA', 'GasW', 'Grav', 'OthW', 'Wall' )), labels = c(1:6))

training_set$HeatingQC= as.numeric(factor(training_set$HeatingQC, levels = c( 'Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))
test$HeatingQC= as.numeric(factor(test$HeatingQC, levels = c( 'Ex', 'Gd', 'TA', 'Fa', 'Po')), labels = c(1:5))

training_set$CentralAir= as.numeric(factor(training_set$CentralAir, levels = c( 'N', 'Y')), labels = c(1,2))
test$CentralAir= as.numeric(factor(test$CentralAir, levels = c( 'N', 'Y')), labels = c(1,2))

training_set$Electrical= as.numeric(factor(training_set$Electrical, levels = c( 'SBrkr', 'FuseA', 'FuseF', 'FuseP', 'Mix')), labels = c(1:5))
test$Electrical= as.numeric(factor(test$Electrical, levels = c( 'SBrkr', 'FuseA', 'FuseF', 'FuseP', 'Mix')), labels = c(1:5))

training_set$KitchenQual= as.numeric(factor(training_set$KitchenQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po' )), labels = c(1:5))
test$KitchenQual= as.numeric(factor(test$KitchenQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po' )), labels = c(1:5))

training_set$Functional= as.numeric(factor(training_set$Functional, levels = c( 'Typ', 'Min1','Min2', 'Mod', 'Maj1', 'Maj2', 'Sev', 'Sal' )), labels = c(1:8))
test$Functional= as.numeric(factor(test$Functional, levels = c( 'Typ', 'Min1','Min2', 'Mod', 'Maj1', 'Maj2', 'Sev', 'Sal' )), labels = c(1:8))

training_set$FireplaceQu = as.character(training_set$FireplaceQu)
training_set$FireplaceQu[is.na(training_set$FireplaceQu)] = "NF"
training_set$FireplaceQu= as.numeric(factor(training_set$FireplaceQu, levels = c( 'Ex', 'Gd', 'TA', 'Fa', 'LwQ', 'Unf', 'NF' )), labels = c(1:7))
test$FireplaceQu = as.character(test$FireplaceQu)
test$FireplaceQu[is.na(test$FireplaceQu)] = "NF"
test$FireplaceQu= as.numeric(factor(test$FireplaceQu, levels = c( 'Ex', 'Gd', 'TA', 'Fa', 'LwQ', 'Unf', 'NF' )), labels = c(1:7))

training_set$GarageType = as.character(training_set$GarageType)
training_set$GarageType[is.na(training_set$GarageType)] = "No Garage"
training_set$GarageType= as.numeric(factor(training_set$GarageType, levels = c( '2Types', 'Attchd', 'Basment', 'BuiltIn', 'CarPort', 'Detchd', 'No Garage' )), labels = c(1:7))
test$GarageType = as.character(test$GarageType)
test$GarageType[is.na(test$GarageType)] = "No Garage"
test$GarageType= as.numeric(factor(test$GarageType, levels = c( '2Types', 'Attchd', 'Basment', 'BuiltIn', 'CarPort', 'Detchd', 'No Garage' )), labels = c(1:7))

training_set$GarageFinish = as.character(training_set$GarageFinish)
training_set$GarageFinish[is.na(training_set$GarageFinish)] = "NG"
training_set$GarageFinish= as.numeric(factor(training_set$GarageFinish, levels = c('Fin', 'RFn', 'Unf', 'NG' )), labels = c(1:4))
test$GarageFinish = as.character(test$GarageFinish)
test$GarageFinish[is.na(test$GarageFinish)] = "NG"
test$GarageFinish= as.numeric(factor(test$GarageFinish, levels = c('Fin', 'RFn', 'Unf', 'NG' )), labels = c(1:4))

training_set$GarageQual = as.character(training_set$GarageQual)
training_set$GarageQual[is.na(training_set$GarageQual)] = "NG"
training_set$GarageQual= as.numeric(factor(training_set$GarageQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NG' )), labels = c(1:6))
test$GarageQual = as.character(test$GarageQual)
test$GarageQual[is.na(test$GarageQual)] = "NG"
test$GarageQual= as.numeric(factor(test$GarageQual, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NG' )), labels = c(1:6))

training_set$GarageCond = as.character(training_set$GarageCond)
training_set$GarageCond[is.na(training_set$GarageCond)] = "NG"
training_set$GarageCond= as.numeric(factor(training_set$GarageCond, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NG' )), labels = c(1:6))
test$GarageCond = as.character(test$GarageCond)
test$GarageCond[is.na(test$GarageCond)] = "NG"
test$GarageCond= as.numeric(factor(test$GarageCond, levels = c( 'Ex', 'Gd','TA', 'Fa', 'Po', 'NG' )), labels = c(1:6))

training_set$PavedDrive= as.numeric(factor(training_set$PavedDrive, levels = c( 'Y', 'P', 'N')), labels = c(1:3))
test$PavedDrive= as.numeric(factor(test$PavedDrive, levels = c( 'Y', 'P', 'N')), labels = c(1:3))

training_set$PoolQC = as.character(training_set$PoolQC)
training_set$PoolQC[is.na(training_set$PoolQC)] = "NP"
training_set$PoolQC= as.numeric(factor(training_set$PoolQC, levels = c( 'Ex', 'Gd','TA', 'Fa', 'NP' )), labels = c(1:5))
test$PoolQC = as.character(test$PoolQC)
test$PoolQC[is.na(test$PoolQC)] = "NP"
test$PoolQC= as.numeric(factor(test$PoolQC, levels = c( 'Ex', 'Gd','TA', 'Fa', 'NP' )), labels = c(1:5))

training_set$Fence = as.character(training_set$Fence)
training_set$Fence[is.na(training_set$Fence)] = "NF"
training_set$Fence= as.numeric(factor(training_set$Fence, levels = c( 'GdPrv', 'MnPrv','GdWo', 'MnWw', 'NF' )), labels = c(1:5))
test$Fence = as.character(test$Fence)
test$Fence[is.na(test$Fence)] = "NF"
test$Fence= as.numeric(factor(test$Fence, levels = c( 'GdPrv', 'MnPrv','GdWo', 'MnWw', 'NF' )), labels = c(1:5))

training_set$MiscFeature = as.character(training_set$MiscFeature)
training_set$MiscFeature[is.na(training_set$MiscFeature)] = "None"
training_set$MiscFeature= as.numeric(factor(training_set$MiscFeature, levels = c( 'Elev', 'Gar2', 'Othr', 'Shed', 'TenC', 'None' )), labels = c(1:6))
test$MiscFeature = as.character(test$MiscFeature)
test$MiscFeature[is.na(test$MiscFeature)] = "None"
test$MiscFeature= as.numeric(factor(test$MiscFeature, levels = c( 'Elev', 'Gar2', 'Othr', 'Shed', 'TenC', 'None' )), labels = c(1:6))

training_set$SaleType= as.numeric(factor(training_set$SaleType, levels = c( 'WD', 'CWD', 'VWD', 'New', 'COD', 'Con', 'ConLw', 'ConLI', 'ConLD', 'Oth' )), labels = c(1:10))
test$SaleType= as.numeric(factor(test$SaleType, levels = c( 'WD', 'CWD', 'VWD', 'New', 'COD', 'Con', 'ConLw', 'ConLI', 'ConLD', 'Oth' )), labels = c(1:10))

training_set$SaleCondition= as.numeric(factor(training_set$SaleCondition, levels = c( 'Normal', 'Abnorml', 'AdjLand', 'Alloca', 'Family', 'Partial' )), labels = c(1:6))
test$SaleCondition= as.numeric(factor(test$SaleCondition, levels = c( 'Normal', 'Abnorml', 'AdjLand', 'Alloca', 'Family', 'Partial' )), labels = c(1:6))

training_set <- training_set[,colSums(is.na(training_set)) == 0] 


sapply(training_set, class)

#Splitting the training dataset into a training set and crossvalidation

library(caret)
intrain <- createDataPartition(training_set$SalePrice, p=0.75, list=FALSE)
train <- training_set[intrain, ]
training_Xval <- training_set[-intrain,]

sapply(train, class)

### Running the Randomforest model

library(randomForest)
fit1 <- randomForest(SalePrice~., data = training_set)
pred_Xval <- predict(fit1, newdata = training_Xval[-75])

### Actual and predicted
com <- data.frame(Actual = training_Xval$SalePrice, Predicted_rf = pred_Xval)

## Taking the id variable and runnimg the random forest
# training <- train[-1]
# training_Xval1 <- training_Xval[-1]
# library(randomForest)
# fit2 <- randomForest(SalePrice~., data = training_set)
# pred_Xval1 <- predict(fit2, newdata = training_Xval[-75])

#com <- data.frame(x = training_Xval$SalePrice, y = pred_Xval, z= pred_Xval1)
# com$Predicted_rf1 <- pred_Xval1

# Dimensioanlity Reduction
#library(kernlab)
#kpca = kpca(~., data = train[-81], kernel = 'rbfdot', features =10)
#training_set_pca = as.data.frame(predict(kpca, train))
#training_set_pca$SalePrice = train$SalePrice

#training_Xval_pca = as.data.frame(predict(kpca, training_Xval))
#training_Xval_pca$SalePrice = training_Xval$SalePrice

#library(randomForest)
#fit1 <- randomForest(SalePrice~., data = training_set_pca)
#pred <- predict(fit1, training_Xval_pca)
#fit1
#head(pred)

#pred_Xval <- predict(fit1, newdata = training_Xval)

training_set <- training_set[-1]
training_Xval <- training_Xval[-1]
library(xgboost)
cl <- xgboost(data = as.matrix(training_set[-75]), label = training_set$SalePrice, nrounds = 20)
y_pred = predict(cl, newdata = as.matrix(training_Xval[-75]))

#com <- data.frame(actual = training_Xval$SalePrice, Pred_rf = pred_Xval, Pred_xg = y_pred)
com$Predicted_xg20 <- y_pred

c2 <- xgboost(data = as.matrix(training_set[-75]), label = training_set$SalePrice, nrounds = 25)
y_pred1 = predict(c2, newdata = as.matrix(training_Xval[-75]))

#com <- data.frame(actual = training_Xval$SalePrice, Pred_rf = pred_Xval, Pred_xg = y_pred, Pred_xg2 = y_pred1)
com$Predicted_xg25 <- y_pred1

c3 <- xgboost(data = as.matrix(training_set[-75]), label = training_set$SalePrice, nrounds = 500)

y_pred2 = predict(c3, newdata = as.matrix(training_Xval[-75]))

#com <- data.frame(actual = training_Xval$SalePrice, Pred_rf = pred_Xval, Pred_xg = y_pred, Pred_xg2 = y_pred1, Pred_xg3 = y_pred2)
#com$Predicted_xg50 <- y_pred2
#com$Predicted_xg75 <- y_pred2
#com$Predicted_xg100 <- y_pred2
#com$Predicted_xg250 <- y_pred2
com$Predicted_xg500 <- y_pred2


# c5 <- xgboost(data = as.matrix(training_set[-75]), label = training_set$SalePrice, nrounds = 600)
# y_pred3 = predict(c5, newdata = as.matrix(training_Xval[-75]))
# 
# com <- data.frame(actual = training_Xval$SalePrice, Pred_rf = pred_Xval, Pred_xg = y_pred, Pred_xg2 = y_pred1, Pred_xg3 = y_pred2, Pred_xg4 = y_pred3)
# com$Pred_xg600 = y_pred3 




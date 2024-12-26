### The intention of this project is to predict the student's academic performance, expressed in 
### GPA score for the 6th grade.
# install packages
install.packages(c("imputeTS","FNN","knnwtsim","kNNvs","KNNShiny",
                   "knnp","knn.covertree","car","readr","tidyr","vcd","forecast",
                   "gplots","reshape","GGally","MASS","dplyr","caret","party",
                   "plyr","corrplot","gridExtra","RColorBrewer","gains","caTools",
                   "ROCR","pROC","rpart","rpart.plot","ggplot2","cowplot",
                   "tidyverse","missForest","DMwR2","e1071","class","gmodels",
                   "psych","AICcmodavg","rpart.plot","rpart"))

sapply(c("imputeTS","FNN","knnwtsim","kNNvs","KNNShiny",
          "knnp","knn.covertree","car","readr","tidyr","vcd","forecast",
          "gplots","reshape","GGally","MASS","dplyr","caret","party",
          "plyr","corrplot","gridExtra","RColorBrewer","gains","caTools",
          "ROCR","pROC","rpart","rpart.plot","ggplot2","cowplot",
          "tidyverse","missForest","DMwR2","e1071","class","gmodels",
          "psych","AICcmodavg","rpart.plot","rpart"), require, character.only = TRUE)


# Penalize R for using scientific way to display number
options(scipen = 999)
#--------------------------Import data ----------------------------------------#
TestLRData.df <- read_csv("C:/Users/raque/OneDrive - Cal Poly Pomona/MSBA Project/Masters Export-V3.2.csv")
#View(TestLRData.df)

#-------------------------Find missing records---------------------------------#
#missing values
sum(is.na(TestLRData.df))

#----------Remove rows missing 7 Grade GPA values (MissingGPA=7)---------------#
TestLRData.df <- TestLRData.df[TestLRData.df$MissingGPA != 7, ]

#--------- Fill in NA values in UC_CSU_ELIGIBILITY with "N" as ----------------#
#------------ this is how it is reported to the State of CA. ------------------#

sum(is.na(TestLRData.df$UC_CSU_ELIGIBILITY))
TestLRData.df <- TestLRData.df %>% 
  mutate(UC_CSU_ELIGIBILITY = ifelse(is.na(UC_CSU_ELIGIBILITY),"N",UC_CSU_ELIGIBILITY))
sum(is.na(TestLRData.df$UC_CSU_ELIGIBILITY))


################################------- KNN -------##################################

# Data Preparation // Create dummies for categorical variables  
str(TestLRData.df)

# GENDER          
TestLRData.df$Masculine <- ifelse(TestLRData.df$GENDER == 'M', 1, 0)
TestLRData.df$Female <- ifelse(TestLRData.df$GENDER == 'F', 1, 0)

# Disability                  
TestLRData.df$Dis <- ifelse(TestLRData.df$Disability == 'Y', 1, 0)
TestLRData.df$NoDis <- ifelse(TestLRData.df$Disability == 'N', 1, 0)

# ELASTATUS                           
TestLRData.df$EO <- ifelse(TestLRData.df$ELASTATUS == 'EO', 1, 0)
TestLRData.df$RFEP <- ifelse(TestLRData.df$ELASTATUS == 'RFEP', 1, 0)
TestLRData.df$IFEP <- ifelse(TestLRData.df$ELASTATUS == 'IFEP', 1, 0)
TestLRData.df$EL <- ifelse(TestLRData.df$ELASTATUS == 'EL', 1, 0)

# ECO_DIS                                     
TestLRData.df$Paid <- ifelse(TestLRData.df$ECO_DIS == 'P', 1, 0)
TestLRData.df$Reduced <- ifelse(TestLRData.df$ECO_DIS == 'R', 1, 0)
TestLRData.df$Free <- ifelse(TestLRData.df$ECO_DIS == 'F', 1, 0)

# ETHNICITY
TestLRData.df$indian <- ifelse(TestLRData.df$ETHNICITY == '(100)', 1, 0)
TestLRData.df$chinese <- ifelse(TestLRData.df$ETHNICITY == '(201)', 1, 0)
TestLRData.df$japanese <- ifelse(TestLRData.df$ETHNICITY == '(202)', 1, 0)
TestLRData.df$korean <- ifelse(TestLRData.df$ETHNICITY == '(203)', 1, 0)
TestLRData.df$vietnamese <- ifelse(TestLRData.df$ETHNICITY == '(204)', 1, 0)
TestLRData.df$aindian <- ifelse(TestLRData.df$ETHNICITY == '(205)', 1, 0)
TestLRData.df$cambodian <- ifelse(TestLRData.df$ETHNICITY == '(207)', 1, 0)
TestLRData.df$oasian <- ifelse(TestLRData.df$ETHNICITY == '(299)', 1, 0)
TestLRData.df$opislander <- ifelse(TestLRData.df$ETHNICITY == '(399)', 1, 0)
TestLRData.df$filipino <- ifelse(TestLRData.df$ETHNICITY == '(400)', 1, 0)
TestLRData.df$black <- ifelse(TestLRData.df$ETHNICITY == '(600)', 1, 0)
TestLRData.df$white <- ifelse(TestLRData.df$ETHNICITY == '(700)', 1, 0)
TestLRData.df$latino <- ifelse(TestLRData.df$ETHNICITY == '(800)', 1, 0)
TestLRData.df$twoORmore <- ifelse(TestLRData.df$ETHNICITY == '(900)', 1, 0)
TestLRData.df$Declined <- ifelse(TestLRData.df$ETHNICITY == 'Declined', 1, 0)

# PARENTEDBOTH
TestLRData.df$Parentgraduate <- ifelse(TestLRData.df$PARENTEDBOTH == 'Graduate Degree or Higher', 1, 0)
TestLRData.df$Parentdecline <- ifelse(TestLRData.df$PARENTEDBOTH == 'Decline to State', 1, 0)
TestLRData.df$ParentNoHS <- ifelse(TestLRData.df$PARENTEDBOTH == 'Not a High School Graduate', 1, 0)
TestLRData.df$ParentSomeColege <- ifelse(TestLRData.df$PARENTEDBOTH == 'Some College or Associate Degree', 1, 0)
TestLRData.df$ParentColGrad <- ifelse(TestLRData.df$PARENTEDBOTH == 'College Graduate', 1, 0)
TestLRData.df$ParentHSGrad <- ifelse(TestLRData.df$PARENTEDBOTH == 'High School Graduate', 1, 0)

# SCHOOLRESIDENT
TestLRData.df$SchoolA <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 1', 1, 0)
TestLRData.df$SchoolB <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 2', 1, 0)
TestLRData.df$SchoolC <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 3', 1, 0)
TestLRData.df$SchoolD <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 4', 1, 0)
TestLRData.df$SchoolE <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 5', 1, 0)
TestLRData.df$SchoolF <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 6', 1, 0)
TestLRData.df$SchoolG <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 7', 1, 0)
TestLRData.df$SchoolH <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 8', 1, 0)
TestLRData.df$SchoolI <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 9', 1, 0)
TestLRData.df$SchoolJ <- ifelse(TestLRData.df$SCHOOLRESIDENT == 'School 10', 1, 0)

# ComputCollege
TestLRData.df$College <- ifelse(TestLRData.df$ComputCollege == 'College', 1, 0)
TestLRData.df$NoCollege <- ifelse(TestLRData.df$ComputCollege == 'No College', 1, 0)

# UC_CSU_ELIGIBILITY
TestLRData.df$UC_CSU_EligYes <- ifelse(TestLRData.df$UC_CSU_ELIGIBILITY == 'Y', 1, 0)
TestLRData.df$UC_CSU_EligNo <- ifelse(TestLRData.df$UC_CSU_ELIGIBILITY == 'N', 1, 0)

# SchoolGrad        
TestLRData.df$SchGrad <- ifelse(TestLRData.df$SchoolGrad == 'Grad', 1, 0)
TestLRData.df$SchGradUnk <- ifelse(TestLRData.df$SchoolGrad == 'Unknown', 1, 0)

# College      
TestLRData.df$College2Y <- ifelse(TestLRData.df$College == '2Year', 1, 0)
TestLRData.df$CollegeUnk <- ifelse(TestLRData.df$College == 'Unknown', 1, 0)
TestLRData.df$College4Y <- ifelse(TestLRData.df$College == '4Year', 1, 0)

# UCCSU_Status_Qual     
TestLRData.df$UCCSU_Status_QualMet <- ifelse(TestLRData.df$UCCSU_Status_Qual == 'Met', 1, 0)
TestLRData.df$UCCSU_Status_QualNotMet <- ifelse(TestLRData.df$UCCSU_Status_Qual == 'Not', 1, 0)

################################------- KNN -------##################################
###################### KNN method for continuous variables: GPA #####################
#--------------Replace na values in GPA columns (GR6GPA..GR12GPA)-------------------#


############################--------- GR6GPA -----------#############################
sum(is.na(TestLRData.df$GR6GPA))
dataknn <- TestLRData.df %>% filter(!is.na(GR6GPA))
sum(is.na(dataknn$GR6GPA))

# put outcome (gpa) in its own object
gpa_outcome <- dataknn %>% select(GR6GPA)

# remove original from the data set
dataknn <- dataknn %>% select(-GR6GPA)

dataknn <- dataknn[,c("Masculine","Female","Dis","NoDis","EO","RFEP",
                      "IFEP","EL","Paid","Reduced","Free","indian","chinese","japanese",
                      "korean","vietnamese","aindian","cambodian","oasian","opislander",
                      "filipino","black","white","latino","twoORmore","Declined",
                      "Parentgraduate","Parentdecline","ParentNoHS","ParentSomeColege",
                      "ParentColGrad","ParentHSGrad","SchoolA","SchoolB","SchoolC",
                      "SchoolD","SchoolE","SchoolF","SchoolG","SchoolH","SchoolI",
                      "SchoolJ","College","NoCollege","SchGrad","SchGradUnk","College2Y",
                      "CollegeUnk","College4Y",
                      "UC_CSU_EligYes","UC_CSU_EligNo",
                      "UCCSU_Status_QualMet","UCCSU_Status_QualNotMet",
                      'ELABinary', 'Eco_DIS_Binary',
                      'UCCSU_AREA_A', 'UCCSU_AREA_B', 'UCCSU_AREA_C', 
                      'UCCSU_AREA_D', 'UCCSU_AREA_E', 'UCCSU_AREA_F', 'UCCSU_AREA_G',
                      'UCCSU_AREA_A_Qual', 'UCCSU_AREA_B_Qual', 'UCCSU_AREA_C_Qual',
                      'UCCSU_AREA_D_Qual', 'UCCSU_AREA_E_Qual', 'UCCSU_AREA_F_Qual',
                      'UCCSU_AREA_G_Qual',"AVID_COUNTS_MS","YEARS_IN_DISTRICT",
                      "TOTALCUMGPA","AVID_COUNTS")]

# Check all variables are numeric now and that there are no na values
str(dataknn)
sum(is.na(dataknn))

# Scale numeric variables
dataknn[, c('ELABinary', 'Eco_DIS_Binary',
            'UCCSU_AREA_A', 'UCCSU_AREA_B', 'UCCSU_AREA_C', 
            'UCCSU_AREA_D', 'UCCSU_AREA_E', 'UCCSU_AREA_F', 'UCCSU_AREA_G',
            'UCCSU_AREA_A_Qual', 'UCCSU_AREA_B_Qual', 'UCCSU_AREA_C_Qual',
            'UCCSU_AREA_D_Qual', 'UCCSU_AREA_E_Qual', 'UCCSU_AREA_F_Qual',
            'UCCSU_AREA_G_Qual',"AVID_COUNTS_MS","YEARS_IN_DISTRICT",
            "TOTALCUMGPA","AVID_COUNTS")] <- 
  scale(dataknn[, c('ELABinary', 'Eco_DIS_Binary','UCCSU_AREA_A',
                    'UCCSU_AREA_B','UCCSU_AREA_C','UCCSU_AREA_D','UCCSU_AREA_E',
                    'UCCSU_AREA_F','UCCSU_AREA_G','UCCSU_AREA_A_Qual','UCCSU_AREA_B_Qual',
                    'UCCSU_AREA_C_Qual','UCCSU_AREA_D_Qual','UCCSU_AREA_E_Qual',
                    'UCCSU_AREA_F_Qual','UCCSU_AREA_G_Qual',"AVID_COUNTS_MS",
                    "YEARS_IN_DISTRICT","TOTALCUMGPA","AVID_COUNTS")])
head(dataknn)

# Normalize data
data_norm <- function(x) {((x - min(x))/(max(x)-min(x)))}
dataknn_norm <- as.data.frame(lapply(dataknn, data_norm))
summary(dataknn[,2:5])
summary(dataknn_norm[,2:5])

# Splitting data into train and test data
# Partition data into training (60%) and validation (40%) sets
set.seed(111)
sample_size = floor(0.6*nrow(dataknn_norm))

# Partition data into training (60%) and validation (40%) sets
# creating test and training sets that contain all of the predictors
train.index <- sample(seq_len(nrow(dataknn_norm)), size = sample_size)
reg_pred_train <- dataknn_norm[train.index, ]
reg_pred_test <- dataknn_norm[-train.index, ]

# check there are no na values in data sets, if so replace with "zero" -- dummy var
sum(is.na(reg_pred_train))
reg_pred_train[is.na(reg_pred_train)]=0
sum(is.na(reg_pred_train))
sum(is.na(reg_pred_test))
reg_pred_test[is.na(reg_pred_test)]=0
sum(is.na(reg_pred_test))

# Split outcome variable into training and test sets 
# using the same partition as above.
gpa_outcome_train <- gpa_outcome[train.index, ]
gpa_outcome_test <- gpa_outcome[-train.index, ]

cl = gpa_outcome_train[,1, drop = TRUE]

## KNN Regression Model

# Training 
reg_results_train11 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 11)
reg_results_train23 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 23)
reg_results_train33 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 33)
reg_results_train41 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 41)
reg_results_train71 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 71)
reg_results_train83 <- knn.reg(reg_pred_train, reg_pred_train, cl, k = 83)

## Test
reg_results_test11 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 11)
reg_results_test23 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 23)
reg_results_test33 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 33)
reg_results_test41 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 41)
reg_results_test71 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 71)
reg_results_test83 <- knn.reg(reg_pred_train, reg_pred_test, cl, k = 83)

# Calculate accuracy // Root Mean Square Error (RMSE)

# Training 
RMSE_train11 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train11$pred)^2))
RMSE_train23 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train23$pred)^2))
RMSE_train33 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train33$pred)^2))
RMSE_train41 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train41$pred)^2))
RMSE_train71 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train71$pred)^2))
RMSE_train83 <- sqrt(mean((gpa_outcome_train$GR6GPA - reg_results_train83$pred)^2))

# Test
RMSE_test11 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test11$pred)^2))
RMSE_test23 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test23$pred)^2))
RMSE_test33 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test33$pred)^2))
RMSE_test41 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test41$pred)^2))
RMSE_test71 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test71$pred)^2))
RMSE_test83 <- sqrt(mean((gpa_outcome_test$GR6GPA - reg_results_test83$pred)^2))

# Show RMSE Results in a Table
k = c(11,23,33,41,71,83)
RMSE_test = c(RMSE_test11,RMSE_test23,RMSE_test33,RMSE_test41,
              RMSE_test71,RMSE_test83)
RMSE_train = c(RMSE_train11,RMSE_train23,RMSE_train33,RMSE_train41,
               RMSE_train71,RMSE_train83)
best_k <- min(RMSE_test)
fit_status = ifelse(RMSE_test > best_k, "Over", ifelse(RMSE_test == best_k, "Best", "Under"))
# summarize results
knn_results = data.frame( k,RMSE_train,RMSE_test,fit_status)
colnames(knn_results) = c("k", "RMSE_train","RMSE_test","Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)
print(reg_results_test23)
min(reg_results_test23$pred)
max(reg_results_test23$pred)


# Create Scatter plot the predicted results
# k = 23
plot(x=reg_results_test23$pred, col = c("green","blue"), pch = 16, y= gpa_outcome_test$GR6GPA,
     xlab='Predicted Values',
     ylab='Actual Values',
     xlim=c(0,4),ylim=c(0,4),
     main='GR6GPA: Predicted vs. Actual Values')
abline(a=0, b=1, col = "pink",lwd = 5)


### Use the KNN Regression Model to predict the value GR6GPA in TestLRData.df ###

dataknn <- TestLRData.df[,c("Masculine","Female","Dis","NoDis","EO","RFEP",
                            "IFEP","EL","Paid","Reduced","Free","indian","chinese","japanese",
                            "korean","vietnamese","aindian","cambodian","oasian","opislander",
                            "filipino","black","white","latino","twoORmore","Declined",
                            "Parentgraduate","Parentdecline","ParentNoHS","ParentSomeColege",
                            "ParentColGrad","ParentHSGrad","SchoolA","SchoolB","SchoolC",
                            "SchoolD","SchoolE","SchoolF","SchoolG","SchoolH","SchoolI",
                            "SchoolJ","College","NoCollege","SchGrad","SchGradUnk","College2Y",
                            "CollegeUnk","College4Y",
                            "UC_CSU_EligYes","UC_CSU_EligNo",
                            "UCCSU_Status_QualMet","UCCSU_Status_QualNotMet",
                            'ELABinary', 'Eco_DIS_Binary',
                            'UCCSU_AREA_A', 'UCCSU_AREA_B', 'UCCSU_AREA_C', 
                            'UCCSU_AREA_D', 'UCCSU_AREA_E', 'UCCSU_AREA_F', 'UCCSU_AREA_G',
                            'UCCSU_AREA_A_Qual', 'UCCSU_AREA_B_Qual', 'UCCSU_AREA_C_Qual',
                            'UCCSU_AREA_D_Qual', 'UCCSU_AREA_E_Qual', 'UCCSU_AREA_F_Qual',
                            'UCCSU_AREA_G_Qual',"AVID_COUNTS_MS","YEARS_IN_DISTRICT",
                            "TOTALCUMGPA","AVID_COUNTS","GR6GPA")]

sum(is.na(dataknn$GR6GPA))
dataknn <- dataknn %>% 
  mutate(GR6GPA = ifelse(is.na(GR6GPA), reg_results_test23$pred, GR6GPA))
sum(is.na(dataknn$GR6GPA))

TestLRData.df$GR6GPA <-dataknn$GR6GPA
sum(is.na(TestLRData.df$GR6GPA))

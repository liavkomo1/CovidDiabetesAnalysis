library(readxl)
detach("package:readxl", unload = TRUE)
library(readxl)
library(readxl)
install.packages("readxl")
library(readxl)
install.packages("xlsx")
install.packages("openxlsx")
library(openxlsx)
library(xlsx)
library(readxl)
DATA_BASE <- read_excel("C:/Users/97255/Desktop/גיבוי/final project/DATA BASE.xlsx")
install.packages("caret")
library(caret)
DATA.BASE <- read.csv("C:/Users/97255/Desktop/גיבוי/final project/DATA BASE.xlsx", sep="")
View(DATA.BASE)
q()
#impoting data from excel
library(readxl)
install.packages("readxl")
install.packages("readxl")
library(readxl)
exceldata <- read_excel("excel_data.xlsx")
install.packages("csv")
library(csv)
exceldata.csv <- read.csv("corona gloc.csv")
confusionMatrix(testlogaft, trainlogaft)
#roc
library(verification)
testwithoutcorona<- c(train_test_withoutcorona$`aft_diabetes-test7`)
trainwithoutcorona<-c(train_test_withoutcorona$logaft)
library(readxl)
train_test_withoutcorona <- read_excel("C:/Users/97255/Desktop/מבחן אנובה ומבחני T/train_test withoutcorona.xlsx")
View(train_test_withoutcorona)
library(readxl)
train_test <- read_excel("C:/Users/97255/Desktop/מבחן אנובה ומבחני T/train_test.xlsx")
View(train_test)
library(readxl)
make_to_logistic_ <- read_excel("C:/Users/97255/Desktop/מבחן אנובה ומבחני T/make to logistic .xlsx")
View(make_to_logistic_)
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
#הגדרנו משתנים כקטגוריאלים
logaftseriouscorona<- glm(aft_diabetes~seriouscorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftseriouscorona)
logaftnocorona<- glm(aft_diabetes~nocorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftnocorona)
logaftcorona<- glm(aft_diabetes~corona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftcorona)
#רגרסיה קיטלוג סוכרת
logaft1seriouscorona<- glm(aft_diabetes~seriouscorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1seriouscorona)
logaft1nocorona<- glm(aft_diabetes~nocorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1nocorona)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
# gloc bef-נבצע רגרסיות ללא משתנה שקשור לקורונה
logaft<-glm(aft_diabetes~Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaft)
#קונפיוזין מטריקס
testlogaft<- factor(c(train_test_withoutcorona$`aft_diabetes-test7`))
trainlogaft<- factor(c(train_test_withoutcorona$`logaft`))
confusionMatrix(testlogaft, trainlogaft)
#roc
library(verification)
testwithoutcorona1<- c(train_test_withoutcorona$`aft_diabetes-test8`)
trainwithoutcorona1<-c(train_test_withoutcorona$logaft1)
roc.plot(testwithoutcorona1, trainwithoutcorona1)
#השטח שמתחת לקו ROC
roc.area(testwithoutcorona1,trainwithoutcorona1)
#קונפיוזן מטריקס עבור קורונה קלה bef dia
testcorona<- factor(c(train_test$`aft_diabetes-test3`))
traincorona<- factor(c(train_test$`logaft1corona-train`))
confusionMatrix(testcorona, traincorona)
#קונפיוזן מטריקס עבור קורונה חמורה bef dia
testcoronase<- factor(c(train_test$`aft_diabetes-test1`))
traincoronase<- factor(c(train_test$`logaft1seriouscorona-train`))
confusionMatrix(testcorona, traincorona)
#קונפיוזן מטריקס עבור ללא קורונה bef dia
testnocorona<- factor(c(train_test$`aft_diabetes-test2`))
trainnocorona<- factor(c(train_test$`logaft1nocorona-train`))
confusionMatrix(testcorona, traincorona)
#קונפיוזן מטריקס עבור קורונה קלה bef gloc
testcorona1<- factor(c(train_test$`aft_diabetes-test5`))
traincorona1<- factor(c(train_test$`logaftcorona-train`))
confusionMatrix(testcorona1, traincorona1)
#קונפיוזן מטריקס עבור קורונה חמורה bef gloc
testcoronase1<- factor(c(train_test$`aft_diabetes-test4`))
traincoronase1<- factor(c(train_test$`logaftseriouscorona-train`))
confusionMatrix(testcorona1, traincorona1)
#קונפיוזן מטריקס עבור ללא קורונה bef gloc
testnocorona1<- factor(c(train_test$`aft_diabetes-test6`))
trainnocorona1<- factor(c(train_test$`logaftnocorona-train`))
confusionMatrix(testcorona1, traincorona1)
# 1 roc בגלל ש-3 הקבוצות הראשונות זהות,נוכל לבצע עליהם רק
library(verification)
test1<- c(train_test$`aft_diabetes-test1`)
train1<-c(train_test$`logaft1seriouscorona-train`)
roc.plot(test1, train1)
#השטח שמתחת לקו ROC
roc.area(test1,train1)
#roc על 3 הקבוצות השונות
#roc קורונה חמורה
library(verification)
test2<- c(train_test$`aft_diabetes-test1`)
train2<-c(train_test$`logaftseriouscorona-train`)
roc.plot(test2, train2)
#השטח שמתחת לקו ROC
roc.area(test2,train2)
#roc קורונה רגילה
library(verification)
test3<- c(train_test$`aft_diabetes-test1`)
train3<-c(train_test$`logaftcorona-train`)
roc.plot(test3, train3)
#השטח שמתחת לקו ROC
roc.area(test3,train3)
#roc ללא קורונה
library(verification)
test4<- c(train_test$`aft_diabetes-test1`)
train4<-c(train_test$`logaftnocorona-train`)
roc.plot(test4, train4)
#השטח שמתחת לקו ROC
roc.area(test41,train4)
library(e1071)
install.packages("e1071")
install.packages("e1071")
library(e1071)
library(e1071)
install.packages('e1071')
library(e1071)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
classifier = svm(formula = Purchased ~ .,
data = make_to_logistic_,
type = 'C-classification',
kernel = 'linear')
classifier = svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
classifier = svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
# gloc bef-נבצע רגרסיות ללא משתנה שקשור לקורונה
logaft<-glm(aft_diabetes~Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(classifier)
classifier <- svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(classifier)
classifier = svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
classifier = svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
summary(classifier)
classifier = svm(formula = (aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
summary(classifier)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
#הגדרנו משתנים כקטגוריאלים
logaftseriouscorona<- glm(aft_diabetes~seriouscorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftseriouscorona)
logaftnocorona<- glm(aft_diabetes~nocorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftnocorona)
logaftcorona<- glm(aft_diabetes~corona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftcorona)
#רגרסיה קיטלוג סוכרת
logaft1seriouscorona<- glm(aft_diabetes~seriouscorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1seriouscorona)
logaft1nocorona<- glm(aft_diabetes~nocorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1nocorona)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector
data = make_to_logistic_,
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
#הגדרנו משתנים כקטגוריאלים
logaftseriouscorona<- glm(aft_diabetes~seriouscorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftseriouscorona)
logaftnocorona<- glm(aft_diabetes~nocorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftnocorona)
logaftcorona<- glm(aft_diabetes~corona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
#רגרסיה קיטלוג סוכרת
logaft1seriouscorona<- glm(aft_diabetes~seriouscorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1seriouscorona)
logaft1nocorona<- glm(aft_diabetes~nocorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1nocorona)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
#הגדרנו משתנים כקטגוריאלים
logaftseriouscorona<- glm(aft_diabetes~seriouscorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftseriouscorona)
logaftnocorona<- glm(aft_diabetes~nocorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftnocorona)
logaftcorona<- glm(aft_diabetes~corona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftcorona)
#רגרסיה קיטלוג סוכרת
logaft1seriouscorona<- glm(aft_diabetes~seriouscorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1seriouscorona)
logaft1nocorona<- glm(aft_diabetes~nocorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1nocorona)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
# gloc bef-נבצע רגרסיות ללא משתנה שקשור לקורונה
logaft<-glm(aft_diabetes~Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaft)
#training_set[-3] = scale(training_set[-3])
#test_set[-3] = scale(test_set[-3])
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
library(e1071)
#training_set[-3] = scale(training_set[-3])
#test_set[-3] = scale(test_set[-3])
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = make_to_logistic_,type = 'C-classification',kernel = 'linear')
summary(classifier)
summary(classifier)
set.seed(123)
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
training_set = subset(make_to_logistic_, split == TRUE)
test_set = subset(make_to_logistic_, split == FALSE)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
set.seed(123)
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
training_set = subset(make_to_logistic_, split == TRUE)
aftdiabetes<-factor(make_to_logistic_$`aft_diabetes`, levels = c("1","0" ))
nocorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("1","2","3" ))
seriouscorona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("2","1","3" ))
corona<-factor(make_to_logistic_$`TYPE_CORONA`, levels = c("3","1","2" ))
typesex<-factor(make_to_logistic_$`Male_Ind`, levels = c("0","1"))
typesexfemale<-factor(make_to_logistic_$`Male_Ind`, levels = c("1","0"))
typesector<-factor(make_to_logistic_$`TYPE_SECTOR`, levels = c("1","2","3","4" ))
typesmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("0","1" ))
typenosmoke<-factor(make_to_logistic_$`Smoke_Ind`, levels = c("1","0" ))
#הגדרנו משתנים כקטגוריאלים
logaftseriouscorona<- glm(aft_diabetes~seriouscorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftseriouscorona)
logaftnocorona<- glm(aft_diabetes~nocorona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftnocorona)
logaftcorona<- glm(aft_diabetes~corona+Glucose_Bef+Cust_Age+typesex+typesmoke+typesector,
data = make_to_logistic_, family =binomial() )
summary(logaftcorona)
#רגרסיה קיטלוג סוכרת
logaft1seriouscorona<- glm(aft_diabetes~seriouscorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1seriouscorona)
logaft1nocorona<- glm(aft_diabetes~nocorona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1nocorona)
logaft1corona<- glm(aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector, data = make_to_logistic_, family =binomial() )
summary(logaft1corona)
training_set[,] = scale(training_set[,])
test_set[,] = scale(test_set[,])
set.seed(123)
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
training_set = subset(make_to_logistic_, split == TRUE)
test_set = subset(make_to_logistic_, split == FALSE)
#קטע חדש של כריית מידע
install.packages('caTools')
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
training_set = subset(make_to_logistic_, split == TRUE)
test_set = subset(make_to_logistic_, split == FALSE)
training_set[,] = scale(training_set[,])
test_set[,] = scale(test_set[,])
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = training_set,type = 'C-classification',kernel = 'linear',scale=FALSE
classifier = svm(formula = aft_diabetes~corona+bef_diabetes+Cust_Age+typesex+typesmoke+typesector,data = training_set,type = 'C-classification',kernel = 'linear',scale=FALSE
set.seed(123)
split = sample.split(make_to_logistic_$aft_diabetes, SplitRatio = 0.75)
#splitting the dataset
library(caTools)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
#creating the model
library(e1071)
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)
print(cm)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
print(cm)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)
#splitting the dataset
library(caTools)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
#creating the model
library(e1071)
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
print(cm)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)
set.seed(123)
#splitting the dataset
library(caTools)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
#creating the model
library(e1071)
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
print(cm)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)
set.seed(123)
#splitting the dataset
library(caTools)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
#creating the model
library(e1071)
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
print(cm)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)
set.seed(123)
#splitting the dataset
library(caTools)
split= sample.split(make_to_logistic_$aft_diabetes,SplitRatio=0.75)
training_set=subset(make_to_logistic_,split==TRUE)
test_set=subset(make_to_logistic_,split==FALSE)
#feature scaling the dataset
library(verification)
training_set[, -1] <- scale(training_set[, -1])
test_set[, -1] <- scale(test_set[, -1])
#creating the model
library(e1071)
classifier=svm(formula=aft_diabetes ~ .,
data=  training_set,
type='C-classification',
kernel='linear')
#predicting using the test set
y_pred<-predict(classifier,newdata=test_set[-1])
print(y_pred)
#evaluation the model
#cm <- table(test_set[, 3], y_pred)
cm <- table(test_set$aft_diabetes, y_pred)
print(cm)
sumtptn = sum(diag(cm))
n = sum(cm)
accuracy = sumtptn / n
print(accuracy)

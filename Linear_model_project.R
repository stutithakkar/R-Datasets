dim(Property_Price_Train)
#1459   81
dim(Property_Price_Test)
# 1459   80

library(dplyr)
combined_data<-bind_rows(Property_Price_Train,Property_Price_Test)
dim(combined_data) # 2918   81

# Impute missing values using CT 
colsum_ms<-colSums(is.na(combined_data))
# if any column has more than 50 % of missing values we can remove that column 
colsum_ms<-as.data.frame(colsum_ms)
head(colsum_ms)
colsum_ms$column_name<-names(combined_data)
head(colsum_ms)
fix(colsum_ms)
sub1<-subset(colsum_ms,colsum_ms$missing_entries>1459,select=column_name)
sub1
combined_data$Lane_Type<-NULL
combined_data$Pool_Quality<-NULL
combined_data$Fence_Quality<-NULL
combined_data$Miscellaneous_Feature<-NULL
colSums(is.na(combined_data))
combined_data$Fireplace_Quality<-NULL

colsum_ms<-colSums(is.na(combined_data))
colsum_ms<-as.data.frame(colsum_ms)
colsum_ms$column_name<-names(combined_data)
fix(colsum_ms)
sub2<-subset(colsum_ms,colsum_ms$missing_entries>0,select=column_name)
sub2
vect1<-sub2$column_name
vect1<-vect1[1:29]
# Knn imputations 
install.packages("VIM")
library(VIM)
imputed_data<-kNN(combined_data,variable = vect1)
dim(imputed_data)
View(imputed_data)
final_data<-imputed_data[,1:76]
write.csv(final_data,file="C:/personal/dsp55/Exercise 2 - Property Price Prediction/final_data.csv")

str(final_data)
library(dplyr)
dim(final_data)
summary(final_data)
final_data$X<-NULL
final_data$Id<-NULL
num_variables<-select_if(final_data,is.numeric)
categorical_variables<-select_if(final_data,is.factor)
dim(num_variables) # 2918   37
dim(categorical_variables) #2918   38

# 1. create cor matrix 
num_variables$Id<-NULL
num_variables<-num_variables[1:1459,]
cor_matrix<-cor(num_variables)
cor_df<-as.data.frame(cor_matrix)
class(cor_df)
cor_var<-cor_df$Sale_Price
head(cor_df)
cor_var
colnames_num<-names(num_variables)
cor_num_df<-data.frame(colnames_num,cor_var)
cor_num_df
sig_var<-subset(cor_num_df,(cor_num_df$cor_var>0.15 & cor_num_df$cor_var<1) | 
                  (cor_num_df$cor_var< 0 & cor_num_df$cor_var < -0.15))
dim(sig_var)
sig_var

# factor variables 
dim(categorical_variables) # 2918   38
categorical_variables<-categorical_variables[1:1459,]
summary(categorical_variables)
# Road_Type,Utility_Type,Condition2,Roof_Quality,Heating_Type
final_data$Road_Type<-NULL
final_data$Utility_Type<-NULL
final_data$Condition2<-NULL
final_data$Roof_Quality<-NULL
final_data$Heating_Quality<-NULL

categorical_variables<-select_if(final_data,is.factor)
categorical_variables<-categorical_variables[1:1459,]
summary(categorical_variables)

#Lot_Configuration
#FR3P = FR2P 
final_data$Lot_Configuration<-as.character(final_data$Lot_Configuration)
final_data$Lot_Configuration[final_data$Lot_Configuration=="FR3P"]<-"FR2P"
final_data$Lot_Configuration<-as.factor(final_data$Lot_Configuration)

#Roof_Design
#Shed = Gable
final_data$Roof_Design<-as.character(final_data$Roof_Design)
final_data$Roof_Design[final_data$Roof_Design=="Shed"]<-"Gable"
final_data$Roof_Design<-as.factor(final_data$Roof_Design)

#Exterior_Condition
#Ex,Po:TA
final_data$Exterior_Condition<-as.character(final_data$Exterior_Condition)
final_data$Exterior_Condition[final_data$Exterior_Condition=="EX"|
                                final_data$Exterior_Condition=="Po"]<-"TA"
final_data$Exterior_Condition<-as.factor(final_data$Exterior_Condition)


#Foundation_Type:
#S,W =PC
final_data$Foundation_Type<-as.character(final_data$Foundation_Type)
final_data$Foundation_Type[final_data$Foundation_Type=="S"|
                                final_data$Foundation_Type=="W"]<-"PC"
final_data$Foundation_Type<-as.factor(final_data$Foundation_Type)


#Basement_Condition:
# Po:TA
final_data$Basement_Condition<-as.character(final_data$Basement_Condition)
final_data$Basement_Condition[final_data$Basement_Condition=="Po"]<-"TA"
final_data$Basement_Condition<-as.factor(final_data$Basement_Condition)


final_data$Heating_Type<-NULL

#Electrical_System
#FuseP,Mix  = SBrkr
final_data$Electrical_System<-as.character(final_data$Electrical_System)
final_data$Electrical_System[final_data$Electrical_System=="FuseP"|
                             final_data$Electrical_System=="Mix"]<-"SBrkr"
final_data$Electrical_System<-as.factor(final_data$Electrical_System)


#Functional_Rate
#MajD1,MajD2 = MajD
#MD,MD1,MD2 = MD
#Mod,MS,SD,Sev= TF

final_data$Functional_Rate<-as.character(final_data$Functional_Rate)
final_data$Functional_Rate[final_data$Functional_Rate=="MajD1"|
                             final_data$Functional_Rate=="MajD2"]<-"MAjD"

final_data$Functional_Rate[final_data$Functional_Rate=="MD"|
                             final_data$Functional_Rate=="MD1"|
                             final_data$Functional_Rate=="MD2"]<-"MD"

final_data$Functional_Rate[final_data$Functional_Rate=="Mod"|
                             final_data$Functional_Rate=="MS"|
                             final_data$Functional_Rate=="SD"|
                             final_data$Functional_Rate=="Sev"]<-"TF"
final_data$Functional_Rate<-as.factor(final_data$Functional_Rate)
 
#Garage:
#2TFes,2Types=Attchd
final_data$Garage<-as.character(final_data$Garage)
final_data$Garage[final_data$Garage=="2TFes"|
                    final_data$Garage=="2Types"]<-"Attchd"
final_data$Garage<-as.factor(final_data$Garage)

#Garage_Quality:
 # Ex,Po=TA
final_data$Garage_Quality<-as.character(final_data$Garage_Quality)
final_data$Garage_Quality[final_data$Garage_Quality=="Ex"|
                    final_data$Garage_Quality=="Po"]<-"TA"
final_data$Garage_Quality<-as.factor(final_data$Garage_Quality)


#Garage_Condition
#Ex,Po=TA
final_data$Garage_Condition<-as.character(final_data$Garage_Condition)
final_data$Garage_Condition[final_data$Garage_Condition=="Ex"|
                            final_data$Garage_Condition=="Po"]<-"TA"
final_data$Garage_Condition<-as.factor(final_data$Garage_Condition)


# Sale_Type
#Con ConLD ConLI ConLw   CWD, Oth  = Others
final_data$Sale_Type<-as.character(final_data$Sale_Type)
final_data$Sale_Type[final_data$Sale_Type=="Con"|
                       final_data$Sale_Type=="ConLD"|
                       final_data$Sale_Type=="ConLI"|
                       final_data$Sale_Type=="CWD"|
                       final_data$Sale_Type=="Oth"]<-"others"
final_data$Sale_Type<-as.factor(final_data$Sale_Type)

#AbnoRMDl  Abnorml=Abnorml
#AdjLand   Alloca=  Alloca
#Normal  NoRMDal= Normal  
final_data$Sale_Condition<-as.character(final_data$Sale_Condition)
final_data$Sale_Condition[final_data$Sale_Condition=="AbnoRMDl"|
                            final_data$Sale_Condition=="Abnorml" ]<-"Abnorml"
final_data$Sale_Condition[final_data$Sale_Condition=="AdjLand"|
                            final_data$Sale_Condition=="Alloca" ]<-"Alloca"
final_data$Sale_Condition[final_data$Sale_Condition=="Normal"|
                            final_data$Sale_Condition=="NoRMDal" ]<-"Normal"

final_data$Sale_Condition<-as.factor(final_data$Sale_Condition)
colSums(is.na(final_data))
library(dplyr)
categorical_variables<-dplyr::select_if(final_data,is.factor)
summary(final_data)

sig_var_names<-sig_var$colnames_num
sig_var_names
sub_sig_num<-subset(final_data,select = sig_var_names)
head(sub_sig_num)
dim(sub_sig_num)
categorical_variables<-dplyr::select_if(final_data,is.factor)
dim(categorical_variables)
house_data<-cbind(sub_sig_num,categorical_variables)
dim(house_data)
write.csv(house_data,file = "C:/personal/dsp55/Exercise 2 - Property Price Prediction/house_data.csv")
colSums(is.na(house_data))
summary(house_data$Functional_Rate)
Train_house_data<-house_data[1:1459,]
Test_house_data<-house_data[1460:2918,]
summary(Train_house_data)
Train_house_data$Sale_Price<-final_data$Sale_Price[1:1459]
summary(Train_house_data)
dim(Train_house_data)
# full model on the given data
# checking outliers 
boxplot(Train_house_data$Sale_Price)
quantile(Train_house_data$Sale_Price)
IQR(Train_house_data$Sale_Price)
UW<- 214000 + 1.5*(84050);UW #340075
Train_house_data$Sale_Price[Train_house_data$Sale_Price>UW]<-UW
summary(Train_house_data$Sale_Price)

set.seed(200)
index<-sample(1459,0.75*1459)
head(index)
length(index)
train_data<-Train_house_data[index,]
test_data<-Train_house_data[-index,]

# model on train data 
house_model<-lm(Sale_Price~.,data = train_data)
summary(house_model)
# stepwise regression 









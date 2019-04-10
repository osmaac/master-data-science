# Para cada dataset hay que mirar una descripción de dicho dataset. Mirar missings y mirar duplicates y ver que hacemos con ellos
#Hacer un análisis descriptivo de las variables

#Leemos el fichero de cuentas
account <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/account.csv", header=TRUE)
head(account)
str(account)
class(account)

#Leemos el fichero de clientes
client <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/client.csv", header=TRUE)
head(client)
str(client)
class(client)

#Leemos el fichero de relación entre clientes y cuentas (disp):Una cuenta puede tener un owner y un autorizado
disp <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/disp.csv", header=TRUE)
head(disp)
str(disp)
class(disp)
unique(disp$disp_id)
dim(disp$disp_id)
# Utilizando el fichero disp incorporamos en el data frame de cuentas los id de los clientes propietarios y autorizados de la cuenta  
library("dplyr")
account<-mutate(account,client_id_owner=NA,client_id_disponent=NA)
unique(account$account_id)
unique(client$client_id)
unique(disp$disp_id)

account <- account[order(account$account_id),]
row.names(account)<-account$account_id
#attr(account,"row.names")=in Algo no está bien
max(row.names(account))
max(account$account)

row_disp=1
row_account=1
for (row_disp in seq(1,dim(disp)[1])){
  for (row_account in seq(1,dim(account)[1])){
    if (disp[row_disp,"account_id"]==account[row_account,"account_id"]){
      if ((disp[row_disp,"type"])=="OWNER"){
        account[row_account,"client_id_owner"]=disp[row_disp,"client_id"]
      }
      else if ((disp[row_disp,"type"])=="DISPONENT"){
      account[row_account,"client_id_disponent"]=disp[row_disp,"client_id"]
      }
    }
  }
  print(row_disp)
 }
#Leemos el fichero de loans
loan <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/loan.csv", header=TRUE)
head(loan)
str(loan)
class(loan)
unique(loan$account_id) #Comprobamos que cada cuenta sólo tiene 1 préstamo

#Añadimos los datos de loans a account
account2=merge(account,loan,by="account_id", all.x=TRUE)

#Renombramos las columnas de Account2
names(account2)[4] = "Date_Account"
names(account2)[8] = "Date_Loan"
names(account2)[9] = "Amount_Loan"
names(account2)[10] = "Duration_Loan"
names(account2)[11] = "Payments_Loan"
names(account2)[12] = "Status_Loan"

#Cargamos los datos de las transferencias
order <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/order.csv", header=TRUE)
head(order)
str(order)
class(order)
unique(order$account_id)
#Recodificamos valores de la variable
recode_factor(order$k_symbol, `POJISTNE` = "Pago_Seguros", `SIPO` = "Household_Payment", `UVER` = "Loan_Payment",`LEASING`="Leasing" )
prueba<-order%>%group_by(account_id,k_symbol)%>% summarise(n = n())%>% summarise(max = max(n))#Hay tipos de orders con más de una order
#Creamos Order_mod para luego hacer merge con account
order_mod=data.frame(`account_id`=numeric(dim(account)[1]),
                     `POJISTNE`=numeric(dim(account)[1]),
                     `POJISTNE_imp`=numeric(dim(account)[1]),
                     `SIPO`=numeric(dim(account)[1]),
                     `SIPO_imp`=numeric(dim(account)[1]),
                     `UVER`=numeric(dim(account)[1]),
                     `UVER_imp`=numeric(dim(account)[1]),
                     `LEASING`=numeric(dim(account)[1]),
                     `LEASING_imp`=numeric(dim(account)[1])
                     )

order_mod$account_id<-seq(1:dim(account)[1])


install.packages("hashmap")
library("hashmap")

hm_acc<-hashmap(account$account_id,seq(1:dim(account)[1]))#Hashmap de account. Para una key= account_id me da la fila del df account en el que está account_id
#Incluimos las columnas que vamos a añadir con los datos de order
account3<-account2%>%mutate(
  `account_id`=numeric(dim(account)[1]),
  `POJISTNE`=numeric(dim(account)[1]),
  `POJISTNE_imp`=numeric(dim(account)[1]),
  `SIPO`=numeric(dim(account)[1]),
  `SIPO_imp`=numeric(dim(account)[1]),
  `UVER`=numeric(dim(account)[1]),
  `UVER_imp`=numeric(dim(account)[1]),
  `LEASING`=numeric(dim(account)[1]),
  `LEASING_imp`=numeric(dim(account)[1])
)
order[,"amount"]<-as.numeric(order$amount) #La variable amount es un factor la pasamos a numérica

account3$account_id<-account$account_id #Corregir en el final que no se modifique Account3
row_order=0
for (row_order in seq(1,dim(order)[1])){
  row_account<-hm_acc[[order[row_order,"account_id"]]]
  if (order[row_order,"k_symbol"]=="POJISTNE"){
    account3[row_account,"POJISTNE"]=account3[row_account,"POJISTNE"]+1
    account3[row_account,"POJISTNE_imp"]=account3[row_account,"POJISTNE_imp"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="SIPO"){
    account3[row_account,"SIPO"]=account3[row_account,"SIPO"]+1
    account3[row_account,"SIPO_imp"]=account3[row_account,"SIPO_imp"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="UVER"){
    account3[row_account,"UVER"]=account3[row_account,"UVER"]+1
    account3[row_account,"UVER_imp"]=account3[row_account,"UVER_imp"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="LEASING"){
    account3[row_account,"LEASING"]=account3[row_account,"LEASING"]+1
    account3[row_account,"LEASING_imp"]=account3[row_account,"LEASING_imp"]+order[row_order,"amount"]
  }
} # Falta incluir la categoría "" de tipo de order

order[,"amount"]<-as.numeric(order$amount)

#Leemos el fichero de cards
card <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/card.csv", header=TRUE)
head(card)
str(card)
class(card)
#Incluimos las columnas que vamos a añadir con los datos de card
account4<-account3%>%mutate(
     `owner_card`=numeric(dim(account)[1]),
     `owner_card_type`=numeric(dim(account)[1]),
     `disponent_card`=numeric(dim(account)[1]),
     `disponent_card_type`=numeric(dim(account)[1])
   )

unique(card$card_id)
unique(card$disp_id)#Cada disp_id tiene una sola tarjeta
hm_disp_acc<-hashmap(disp$disp_id, disp$account_id)
row_account2<-hm_acc[[hm_disp_acc[[2]]]]#Prueba de que funcionan los diccionarios encadenados
hm_disp_type<-hashmap(disp$disp_id, disp$type)#Que tipo de OWNER/DISPONENT es cada disp_id

for (row_order in seq(1,dim(card)[1])){
  row_account2<-hm_acc[[hm_disp_acc[[card[row_order,"disp_id"]]]]]
  if (hm_disp_type[[card[row_order,"disp_id"]]]=="2"){
    account4[row_account2,"owner_card"]=account4[row_account2,"owner_card"]+1
    account4[row_account2,"owner_card_type"]<-card[row_order,"type"]
  }
  
  if (hm_disp_type[[card[row_order,"disp_id"]]]=="1"){
    account4[row_account2,"disponent_card"]=account4[row_account2,"disponent_card"]+1
    account4[row_account2,"disponent_card_type"]<-card[row_order,"type"]
  }
}

#Leemos el fichero de trans
trans <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/trans.csv", header=TRUE)
head(trans)
str(trans)
class(trans)
trans[,"amount"]<-as.numeric(trans$amount) #La variable amount es un factor la pasamos a numérica
trans[,"balance"]<-as.numeric(trans$balance) #La variable balance es un factor la pasamos a numérica
levels(trans$type)<-c("Type_Credit","Type_VYBER","Type_Withdrawal") #Cambiamos la denominación de la variable type. La denominación "VYBER" parece que son débitos pq son todas de Operation VYBER
levels(trans$operation)<-c("Op_Null","Op_Remittance","Op_Collection","Op_CashCredit","Op_WithdrawalCash","Op_WithdrawalCreditCard") #Cambiamos la denominación de la variable Operation
#Las operaciones Op_WithdrawalCreditCard son todas de tipo Debit
#Las operaciones de Op_CashCredit son todas de tipo Credit
#Las operaciones de tipo Op_Collection son todas de tipo Credit
#Las operaciones de tipo Op_WithdrawalCash son todas de tipo Debit
#Las operaciones de tipo Op_Remittance son todas de tipo Debit
#Las operaciones de tipo Op_Null parece que son todas de Type_Credit de modalidad UROK (InterestCredited)
levels(trans$k_symbol)
prueba<-trans[trans$k_symbol=="DUCHOD",c("type","operation")]
str(prueba)
prueba2<-trans[trans$type==2,c("type","operation")]
str(prueba2)
#No hay valores negativos de Balance
prueba3<-trans[trans$balance<0,"balance"]
prueba4<-unique(prueba3)
class(prueba3)
#Cambiamos la denominación de la variable k_symbol
levels(trans$ k_symbol)<-c("Sym_Null","Sym_Null2","Sym_Pension","Sym_Insurance","Sym_NegBal","Sym_Household","Sym_Statement","Sym_IntDep","Sym_LoanPayment")
#Incluimos la variables de la columna type de trans
account5<-account4%>%mutate(
  `Num_Type_Credit`=numeric(dim(account)[1]),
  `Num_Type_VYBER`=numeric(dim(account)[1]),
  `Num_Type_Withdrawal`=numeric(dim(account)[1])
)
#Incluimos la variables de la columna operation de trans
account6<-account5%>%mutate(
  `Num_Op_Null`=numeric(dim(account)[1]),
  `Num_Op_Remittances`=numeric(dim(account)[1]),
  `Num_Op_Collection`=numeric(dim(account)[1]),
  `Num_Op_CashCredit`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCash`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCreditCard`=numeric(dim(account)[1])
)

#Incluimos la variables de la columna k_symbol de trans
account7<-account6%>%mutate(
  `Num_Sym_Null`=numeric(dim(account)[1]),
  `Num_Sym_Null2`=numeric(dim(account)[1]),
  `Num_Sym_Pension`=numeric(dim(account)[1]),
  `Num_Sym_Insurance`=numeric(dim(account)[1]),
  `Num_Sym_NegBal`=numeric(dim(account)[1]),
  `Num_Sym_Household`=numeric(dim(account)[1]),
  `Num_Sym_Statement`=numeric(dim(account)[1]),
  `Num_Sym_IntDep`=numeric(dim(account)[1]),
  `Num_Sym_LoanPayment`=numeric(dim(account)[1])
)
str(account7)

#Completamos el DF Account con los datos de Trans

row_order=0
for (row_order in seq(1,dim(trans)[1])){
  row_account<-hm_acc[[trans[row_order,"account_id"]]]
  #Completamos los atributos de tipo type
  
  if (trans[row_order,"type"]=="Type_Credit"){
    account7[row_account,"Num_Type_Credit"]=account7[row_account,"Num_Type_Credit"]+1
  }
  if (trans[row_order,"type"]=="Type_VYBER"){
    account7[row_account,"Num_Type_VYBER"]=account7[row_account,"Num_Type_VYBER"]+1
  }
  if (trans[row_order,"type"]=="Type_Withdrawal"){
    account7[row_account,"Num_Type_Withdrawal"]=account7[row_account,"Num_Type_Withdrawal"]+1
  }
}
  #Completamos los atributos de tipo operation
row_order=0
for (row_order in seq(1,dim(trans)[1])){
  row_account<-hm_acc[[trans[row_order,"account_id"]]] 
  if (trans[row_order,"operation"]=="Op_Null"){
    account7[row_account,"Num_Op_Null"]=account7[row_account,"Num_Op_Null"]+1
  }
  if (trans[row_order,"operation"]=="Op_Remittance"){
      account7[row_account,"Num_Op_Remittances"]=account7[row_account,"Num_Op_Remittances"]+1 
  }
  if (trans[row_order,"operation"]=="Op_Collection"){
    account7[row_account,"Num_Op_Collection"]=account7[row_account,"Num_Op_Collection"]+1 
  } 
  if (trans[row_order,"operation"]=="Op_CashCredit"){
    account7[row_account,"Num_Op_CashCredit"]=account7[row_account,"Num_Op_CashCredit"]+1 
  }
  if (trans[row_order,"operation"]=="Op_WithdrawalCash"){
    account7[row_account,"Num_Op_WithdrawalCash"]=account7[row_account,"Num_Op_WithdrawalCash"]+1 
  }  
  if (trans[row_order,"operation"]=="Op_WithdrawalCreditCard"){
    account7[row_account,"Num_Op_WithdrawalCreditCard"]=account7[row_account,"Num_Op_WithdrawalCreditCard"]+1 
  }  
}  
  #Completamos los atributos de tipo k_symbol
row_order=0
for (row_order in seq(1,dim(trans)[1])){ 
  if (trans[row_order,"k_symbol"]=="Sym_Null"){
    account7[row_account,"Num_Sym_Null"]=account7[row_account,"Num_Sym_Null"]+1
  }
  if (trans[row_order,"k_symbol"]=="Sym_Null2"){
    account7[row_account,"Num_Sym_Null2"]=account7[row_account,"Num_Sym_Null2"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_Pension"){
    account7[row_account,"Num_Sym_Pension"]=account7[row_account,"Num_Sym_Pension"]+1 
  } 
  if (trans[row_order,"k_symbol"]=="Sym_Insurance"){
    account7[row_account,"Num_Sym_Insurance"]=account7[row_account,"Num_Sym_Insurance"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_NegBal"){
    account7[row_account,"Num_Sym_NegBal"]=account7[row_account,"Num_Sym_NegBal"]+1 
  }  
  if (trans[row_order,"k_symbol"]=="Sym_Household"){
    account7[row_account,"Num_Sym_Household"]=account7[row_account,"Num_Sym_Household"]+1 
  } 
  if (trans[row_order,"k_symbol"]=="Sym_Statement"){
    account7[row_account,"Num_Sym_Statement"]=account7[row_account,"Num_Sym_Statement"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_IntDep"){
    account7[row_account,"Num_Sym_IntDep"]=account7[row_account,"Num_Sym_IntDep"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_LoanPayment"){
    account7[row_account,"Num_Sym_LoanPayment"]=account7[row_account,"Num_Sym_LoanPayment"]+1 
  }
}

#Vamos a empezar a trabajar sobre las variables
#Ponemos district_id como un factor
account7[,"district_id"]<-as.factor(account7$district_id)
#Ponemos client_id_disponent como un factor
install.packages("DataExplorer")
library(DataExplorer)
introduce(account7)
plot_intro(account7)
plot_missing(account7)
row_order=0
for (row_order in seq(1,dim(account7)[1])){
  if (is.na(account7[row_order,"client_id_disponent"])==TRUE){
    account7[row_order,"client_id_disponent"]<-0
  }
  else (account7[row_order,"client_id_disponent"]<-1)
}
account7$client_id_disponent<-as.factor(account7$client_id_disponent) 
levels(account7$client_id_disponent)<-c("0","1")
#Ponemos Loan_id como un factor
row_order=0
for (row_order in seq(1,dim(account7)[1])){
  if (is.na(account7[row_order,"loan_id"])==TRUE){
    account7[row_order,"loan_id"]<-0
  }
  else (account7[row_order,"loan_id"]<-1)
}
account7$loan_id<-as.factor(account7$loan_id) #Nos dice si una cuenta tiene loan o no tiene loan
#Ponemos los NA de Duration_Loan como ceros
row_order=0
for (row_order in seq(1,dim(account7)[1])){
  if (is.na(account7[row_order,"Duration_Loan"])==TRUE){
    account7[row_order,"Duration_Loan"]<-0
  }
}
#Ponemos Payments_Loan de Duration_Loan como un numerico
account7$Payments_Loan<-as.integer(account7$Payments_Loan) 
row_order=0
for (row_order in seq(1,dim(account7)[1])){
  if (is.na(account7[row_order,"Payments_Loan"])==TRUE){
    account7[row_order,"Payments_Loan"]<-0
  }
}
#¿Qué es mejor tener la variable como continua o como factor?. ¿Qué es mejor normalizarla o no normalizarla?
account8<-account7 #Para mantener un df "limpio" por si me equivoco haciendo transformaciones

#Trabajamos sobre la variable Status loan

levels(account8$Status_Loan)<-c("OK","NPL","OK_not_fin","NPL_not_fin") #NPL viene de Non-Performing-Loan

account9<-account8%>%mutate(
  `Status_Loan_Bin`=account8$Status_Loan)

levels(account9$Status_Loan_Bin)<-c("OK","NPL","OK","NPL")

#Trabajamos con la variable Date_Account
install.packages("lubridate")
library("lubridate")
account9[,"Date_Account"]=account9[,"Date_Account"]+19000000
as.character(account9$Date_Account)
account9[,"Date_Account"] <- as.Date.character(account9$Date_Account, "%Y%m%d")

#Trabajamos con la variable Date_Loan
account9[,"Date_Loan"]=account8[,"Date_Loan"]#Lo hago así porque me equivoqué al tratar la columna
account9[,"Date_Loan"]=account9[,"Date_Loan"]+19000000
as.character(account9$Date_Loan)
account9[,"Date_Loan"] <- as.Date.character(account9$Date_Loan, "%Y%m%d")

#Trabajamos con la variable frequency
levels(account9$frequency)<-c("Monthly","After_trans","Weekly")

#Renombramos las variables que venían del DF de Orders
library(tidyverse)
names(account9)
names(account9)[13]#El primer nombre a cambiar
names(account9)[20]#El último nombre a cambiar
names(account9)[13]='Ord_Insurance'
names(account9)[14]='Ord_Insurance_imp'
names(account9)[15]='Ord_Household'
names(account9)[16]='Ord_Household_imp'
names(account9)[17]='Ord_Loan'
names(account9)[18]='Ord_Loan_imp'
names(account9)[19]='Ord_Leasing'
names(account9)[20]='Ord_Leasing_imp'

# No funciona account9 %>% 
# No funciona  rename(
# No funciona    POJISTNE = 'Ord_Insurance', POJISTNE_imp = 'Ord_Insurance_imp',SIPO='Ord_Household',SIPO_imp='Ord_Household_imp', 
# No funciona    UVER='Ord_Loan', UVER_imp='Ord_Loan_imp', LEASING='Ord_Leasing', LEASING_imp='Ord_Leasing_imp'
# No funciona  )

# Vamos a binarizar las variables en las que tenemos frecuencias de operativa

account10<-account9%>%mutate(
  `Ord_Insurance_Bin`=numeric(dim(account)[1]),
  `Ord_Household_Bin`=numeric(dim(account)[1]),
  `Ord_Loan_Bin`=numeric(dim(account)[1]),
  `Ord_Leasing_Bin`=numeric(dim(account)[1]),
  `Num_Type_Credit_Bin`=numeric(dim(account)[1]),
  `Num_Type_VYBER_Bin`=numeric(dim(account)[1]),
  `Num_Type_Withdrawal_Bin`=numeric(dim(account)[1]),
  `Num_Op_Null_Bin`=numeric(dim(account)[1]),
  `Num_Op_Remittances_Bin`=numeric(dim(account)[1]),
  `Num_Op_Collection_Bin`=numeric(dim(account)[1]),
  `Num_Op_CashCredit_Bin`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCash_Bin`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCreditCard_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Null_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Null2_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Pension_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Insurance_Bin`=numeric(dim(account)[1]),
  `Num_Sym_NegBal_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Household_Bin`=numeric(dim(account)[1]),
  `Num_Sym_Statement_Bin`=numeric(dim(account)[1]),
  `Num_Sym_IntDep_Bin`=numeric(dim(account)[1]),
  `Num_Sym_LoanPayment_Bin`=numeric(dim(account)[1])
)

#Empezamos por la variable Ord_Insurance_Bin
account10[,"Ord_Insurance"]<-account8[,"Ord_Insurance"]

row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Ord_Insurance"]!=0){
    account10[row_order,"Ord_Insurance_Bin"]<-1
  }
}

#Variable Ord_Household_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Ord_Household"]!=0){
    account10[row_order,"Ord_Household_Bin"]<-1
  }
}

#Variable Ord_Loan_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Ord_Loan"]!=0){
    account10[row_order,"Ord_Loan_Bin"]<-1
  }
}

#Variable Ord_Leasing_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Ord_Leasing"]!=0){
    account10[row_order,"Ord_Leasing_Bin"]<-1
  }
}

#Variable Num_Type_Credit_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Type_Credit"]!=0){
    account10[row_order,"Num_Type_Credit_Bin"]<-1
  }
}

#Variable  Num_Type_VYBER_Bin (NO FUNCIONA¡¡¡)
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order," Num_Type_VYBER"]!=0){
    account10[row_order," Num_Type_VYBER_Bin"]<-1
  }
}

#Variable Num_Type_Withdrawal_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Type_Withdrawal"]!=0){
    account10[row_order,"Num_Type_Withdrawal_Bin"]<-1
  }
}

#Variable  Num_Op_Null_Bin (NO FUNCIONA¡¡¡)
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order," Num_Op_Null"]!=0){
    account10[row_order," Num_Op_Null_Bin"]<-1
  }
}

#Variable Num_Op_Remittances_Bin 
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Op_Remittances"]!=0){
    account10[row_order,"Num_Op_Remittances_Bin"]<-1
  }
}

#Variable Num_Op_Collection_Bin 
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Op_Collection"]!=0){
    account10[row_order,"Num_Op_Collection_Bin"]<-1
  }
}

#Variable Num_Op_CashCredit_Bin 
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Op_CashCredit"]!=0){
    account10[row_order,"Num_Op_CashCredit_Bin"]<-1
  }
}

#Variable Num_Op_WithdrawalCash_Bin 
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Op_WithdrawalCash"]!=0){
    account10[row_order,"Num_Op_WithdrawalCash_Bin"]<-1
  }
}

#Variable Num_Op_WithdrawalCreditCard_Bin 
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Op_WithdrawalCreditCard"]!=0){
    account10[row_order,"Num_Op_WithdrawalCreditCard_Bin"]<-1
  }
}

#Variable Num_Sym_Null_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Null"]!=0){
    account10[row_order,"Num_Sym_Null_Bin"]<-1
  }
}

#Variable Num_Sym_Null2_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Null2"]!=0){
    account10[row_order,"Num_Sym_Null2_Bin"]<-1
  }
}

#Variable Num_Sym_Pension_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Pension"]!=0){
    account10[row_order,"Num_Sym_Pension_Bin"]<-1
  }
}

#Variable Num_Sym_Insurance_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Insurance"]!=0){
    account10[row_order,"Num_Sym_Insurance_Bin"]<-1
  }
}

#Variable Num_Sym_NegBal_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_NegBal"]!=0){
    account10[row_order,"Num_Sym_NegBal_Bin"]<-1
  }
}

#Variable Num_Sym_NegBal_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_NegBal"]!=0){
    account10[row_order,"Num_Sym_NegBal_Bin"]<-1
  }
}

#Variable Num_Sym_Household_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Household"]!=0){
    account10[row_order,"Num_Sym_Household_Bin"]<-1
  }
}

#Variable Num_Sym_Statement_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_Statement"]!=0){
    account10[row_order,"Num_Sym_Statement_Bin"]<-1
  }
}

#Variable Num_Sym_IntDep_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_IntDep"]!=0){
    account10[row_order,"Num_Sym_IntDep_Bin"]<-1
  }
}

#Variable Num_Sym_LoanPayment_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account10[row_order,"Num_Sym_LoanPayment"]!=0){
    account10[row_order,"Num_Sym_LoanPayment_Bin"]<-1
  }
}

#Vamos a binarizar las variables owner_card y disponent_card


account11<-account10%>%mutate(
  `owner_card_Bin`=numeric(dim(account10)[1]),
  `disponent_card_Bin`=numeric(dim(account10)[1])
)

#Variable owner_card_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account11[row_order,"owner_card"]!=0){
    account11[row_order,"owner_card_Bin"]<-1
  }
}

#Variable disponent_card_Bin
row_order=0
for (row_order in seq(1,dim(account10)[1])){
  if (account11[row_order,"disponent_card"]!=0){
    account11[row_order,"disponent_card_Bin"]<-1
  }
}

#Factorización de variables
account11[,"disponent_card_Bin"]<-as.factor(account11$disponent_card_Bin)
account11[,"owner_card_Bin"]<-as.factor(account11$owner_card_Bin)
account11[,"Ord_Insurance_Bin"]<-as.factor(account11$Ord_Insurance_Bin)
account11[,"Ord_Household_Bin"]<-as.factor(account11$Ord_Household_Bin)
account11[,"Ord_Loan_Bin"]<-as.factor(account11$Ord_Loan_Bin)
account11[,"Ord_Leasing_Bin"]<-as.factor(account11$Ord_Leasing_Bin)
account11[,"Num_Type_Credit_Bin"]<-as.factor(account11$Num_Type_Credit_Bin)
account11[,"Num_Type_VYBER_Bin"]<-as.factor(account11$Num_Type_VYBER_Bin)
account11[,"Num_Type_Withdrawal_Bin"]<-as.factor(account11$Num_Type_Withdrawal_Bin)
account11[,"Num_Op_Null_Bin"]<-as.factor(account11$Num_Op_Null_Bin)
account11[,"Num_Op_Remittances_Bin"]<-as.factor(account11$ Num_Op_Remittances_Bin)
account11[,"Num_Op_Collection_Bin"]<-as.factor(account11$ Num_Op_Collection_Bin)
account11[,"Num_Op_CashCredit_Bin"]<-as.factor(account11$ Num_Op_Collection_Bin)
account11[,"Num_Op_WithdrawalCash_Bin"]<-as.factor(account11$Num_Op_WithdrawalCash_Bin)
account11[,"Num_Op_WithdrawalCreditCard_Bin"]<-as.factor(account11$Num_Op_WithdrawalCreditCard_Bin)
account11[,"Num_Sym_Null_Bin"]<-as.factor(account11$Num_Sym_Null_Bin)
account11[,"Num_Sym_Null2_Bin"]<-as.factor(account11$Num_Sym_Null2_Bin)
account11[,"Num_Sym_Pension_Bin"]<-as.factor(account11$Num_Sym_Null_Bin)
account11[,"Num_Sym_Insurance_Bin"]<-as.factor(account11$Num_Sym_Insurance_Bin)
account11[,"Num_Sym_NegBal_Bin"]<-as.factor(account11$Num_Sym_NegBal_Bin)
account11[,"Num_Sym_Household_Bin"]<-as.factor(account11$Num_Sym_Household_Bin)
account11[,"Num_Sym_Statement_Bin"]<-as.factor(account11$ Num_Sym_Statement_Bin)
account11[,"Num_Sym_IntDep_Bin"]<-as.factor(account11$Num_Sym_Statement_Bin)
account11[,"Num_Sym_LoanPayment_Bin"]<-as.factor(account11$Num_Sym_LoanPayment_Bin)
#Corregimos un error
account12<-account11
account12$" Num_Sym_Statement_Bin"<- NULL
account12$" Num_Sym_Null2_Bin"<- NULL
account12$" Num_Sym_Null_Bin"<- NULL

#Vamos a poner las variables de fecha como factor
D_A_F<-format(account12$Date_Account, "%Y")
D_L_F<-format(account12$Date_Loan, "%Y")
account13<-mutate(account12,Date_Account_Factor=D_A_F,Date_Loan_Factor=D_L_F)
account13$Date_Account_Factor<-as.factor(account13$Date_Account_Factor)
account13$Date_Loan_Factor<-as.factor(account13$Date_Loan_Factor)

#Exportación del fichero de R
setwd("C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo")
write.csv(account13, file="DFBancoCheco.csv")

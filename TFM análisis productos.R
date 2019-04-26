#Instalamos los paquetes que vamos a necesitar
install.packages("tidyverse")
install.packages("DataExplorer")
install.packages("lubridate")
install.packages("hashmap")
install.packages("dplyr")

library("tidyverse")
library("DataExplorer")
library("lubridate")
library("hashmap")
library("dplyr")
#En el proyecto hay 8 ficheros de datos. Partiendo de los datos de las cuentas (Account), vamos a ir construyendo nuestro
#Dataset 

#Cargamos el fichero de cuentas y revisamos su estructura y missings y duplicates
account <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/account.csv", header=TRUE)
head(account)
class(account)
str(account)
levels(account$frequency)#Traducimos los niveles la variable frequency (frecuencia de extracto de la cuenta)
levels(account$frequency)<-c("Monthly","After_trans","Weekly")# After_trans means "After Transaction" (después de cada transacción) 
which(is.na(account)) #No hay missings
introduce(account)
plot_bar(account)
unique(duplicated(account$account_id, incomparables = FALSE))#No hay duplicates en account_id
unique(duplicated(account, incomparables = FALSE))#No hay duplicates en el fichero
account$account_id #account_id no está ordenado
max(account$account_id)#los números de account_id no son correlativos, llegan hasta 11382 con 4.500 cuentas

#Cambiamos el formato de la variable date de Accounts: de número a fecha
account[,"date"]=account[,"date"]+19000000#Para incluir bien el año
as.character(account$date)
account[,"date"] <- as.Date.character(account$date, "%Y%m%d")

#Creamos un diccionario para saber en que fila del Dataframe account está un account_id
hm_acc<-hashmap(account$account_id,seq(1:dim(account)[1]))#Para una key= account_id me da la fila del df account en el que está account_id

#Cargamos el fichero de clientes y revisamos su estructura y missings y duplicates
client <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/client.csv", header=TRUE)
head(client)
class(client)
str(client)
which(is.na(client)) #No hay missings
introduce(client)
unique(duplicated(client$client_id, incomparables = FALSE))#No hay duplicates en client_id
unique(duplicated(client, incomparables = FALSE))#No hay duplicates en el fichero
plot_bar(client)

#Feature birth_number includes birthday and sex: the number is in the form YYMMDD for men, the number is in the
#form YYMM+50DD for women, where YYMMDD is the date of birth
substr(client$birth_number,3,4)%>%as.numeric()%>%max()#Comprobamos que la variable tiene la estructura comentada y que por tanto el mes "máximo" es 62
substr(client$birth_number,3,4)%>%as.numeric()%>%min()#Comprobamos que la variable tiene la estructura comentada y que por tanto el mes "mínimo" es 1
client<-client%>%mutate(`sex`="M",`birth`=birth_number)#Creamos las columnas que vamos a construir

#Generamos los datos en las nuevas columnas sex y birth
for (row_order in seq(1,dim(client)[1])){
  num_month=substr(client$birth_number[row_order],3,4)
  if (num_month>50){
    client[row_order,"sex"]="F"
    client[row_order,"birth"]=client[row_order,"birth_number"]-5000 #Para quitar el 50 que se suma en en mes de las mujeres
  }
}
#Transformamos la columna birth en fecha en formato YYYY-MM-DD 
client[,"birth"]=client[,"birth"]+19000000
client$birth<-as.character(client$birth)
client[,"birth"] <- as.Date.character(client$birth, "%Y%m%d")

#Vamos a leer el fichero disp, que nos va a permitir unir clientes (client_id) y cuentas (account_id)
disp <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/disp.csv", header=TRUE)
head(disp)
class(disp)
str(disp)#La variable type nos dice el tipo de relación del cliente con la cuenta. "Disponent" es autorizado 
which(is.na(disp)) #No hay missings
introduce(disp)
unique(duplicated(disp$disp_id, incomparables = FALSE))#No hay duplicates en disp_id
unique(duplicated(disp$client_id, incomparables = FALSE))#No hay duplicates en client_id
unique(duplicated(disp$account_id, incomparables = FALSE))#Sí hay duplicates en account_id porque hay 2 tipos de relación Owner y Disponent
unique(duplicated(disp, incomparables = FALSE))#No hay duplicates
disp %>% filter(type == "OWNER")%>%str()#Vemos que cada account_id tiene un owner
plot_bar(disp)

#Vamos a incorporar los datos de client_id a cada cuenta utilizando los datos del DataFrame disp 
account<-mutate(account,client_id_owner=NA,client_id_disponent=NA)
for (row_disp in seq(1,dim(disp)[1])){
  row_account_id<-hm_acc[[disp[row_disp,"account_id"]]] 
      if ((disp[row_disp,"type"])=="OWNER"){
        account[row_account_id,"client_id_owner"]=disp[row_disp,"client_id"]
      }
      else if ((disp[row_disp,"type"])=="DISPONENT"){
        account[row_account_id,"client_id_disponent"]=disp[row_disp,"client_id"]
      }
  }
which(is.na(account$client_id_owner))#No tiene missings como esperábamos
which(is.na(account$client_id_disponent))#Sí tiene missings como esperábamos
#Vamos a incorporar los datos de Sex y Birth a cada cuenta utilizando los datos del DF client
#account2<-mutate(account,sex_owner=NA,sex_disponent=NA,birth_owner=class(client$birth),birth_disponent=class(client$birth))

#Creamos un diccionario para saber en que fila del Dataframe client está un client_id
hm_cli_disp<-hashmap(disp$client_id,seq(1:dim(disp)[1]))#Para una key=client_id me da la fila del df client en el que está client_id

for (row_client in seq(1,dim(client)[1])){
  row_disp<-hm_cli_disp[[client[row_client,"client_id"]]]
  type_client<-disp[row_disp,"type"]
  account_num<-disp[row_disp,"account_id"]
  row_account<-hm_acc[[account_num]]
  if (type_client=="OWNER"){
    account[row_account,"sex_owner"]=client[row_client,"sex"]
    account[row_account,"birth_owner"]=client[row_client,"birth"]
  }
  else if (type_client=="DISPONENT"){
    account[row_account,"sex_disponent"]=client[row_client,"sex"]
    account[row_account,"birth_disponent"]=client[row_client,"birth"]
  }
}

#Cargamos el fichero de préstamos (Loans) y revisamos su estructura y missings y duplicates
#Leemos el fichero de loans
loan <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/loan.csv", header=TRUE)
head(loan)
class(loan)
str(loan)
which(is.na(loan)) #No hay missings
introduce(loan)
unique(duplicated(loan$loan_id, incomparables = FALSE))#No hay duplicates en loan_id
unique(duplicated(loan$account_id, incomparables = FALSE))#No hay duplicates en account_id (por lo tanto cada cuenta sólo tiene 1 loan)
unique(duplicated(loan, incomparables = FALSE))#No hay duplicates en el fichero Loan
plot_bar(loan)

#Vamos a trabajar sobre las variables del fichero Loans

#Traducimos los niveles de la variable status que nos dice si un préstamo es moroso
levels(loan$status)<-c("OK","NPL","OK_not_fin","NPL_not_fin") #NPL viene de Non-Performing-Loan y fin de finalizado
loan<-loan%>%mutate(status_loan_bin=loan$status)#Creamos una nueva variable que nos diga si el préstamos es moroso sin considerar si el préstamo está vivo
levels(loan$status_loan_bin)<-c("OK","NPL","OK","NPL")

#Pasamos la feature Payments a numérico
loan$payments<-as.numeric(as.character(loan$payments)) 

#Cambiamos el formato de la variable date de loans: de número a fecha
loan[,"date"]=loan[,"date"]+19000000#Para incluir bien el año
as.character(loan$date)
loan[,"date"] <- as.Date.character(loan$date, "%Y%m%d")

#Incluimos los datos de Loan en el DataFrame de Account (dataset que estamos construyendo)
account=merge(account,loan,by="account_id", all.x=TRUE)
str(account)#Ha cambiado el orden del DataFrame Account y el nombre de algunas variables 
#Revisamos los nombres de las variables después del merge de Account y Loan
names(account)
names(account)[4] = "Date_Account"
names(account)[12] = "Date_Loan"
names(account)[13] = "Amount_Loan"
names(account)[14] = "Duration_Loan"
names(account)[15] = "Payments_Loan"
names(account)[17] = "Status_Loan"

#Actualizamos el diccionario de account porque con el merge ha cambiado el nombre
hm_acc<-hashmap(account$account_id,seq(1:dim(account)[1]))#Para una key= account_id me da la fila del df account en el que está account_id

#Cargamos el fichero de transferencias (order) y revisamos su estructura y missings y duplicates
#Leemos el fichero de Orders
order <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/order.csv", header=TRUE)
head(order)
class(order)
str(order)
which(is.na(order)) #No hay missings
introduce(order)
unique(duplicated(order$account_id, incomparables = FALSE))#Puede haber varias orders por cada en account_id
unique(duplicated(order, incomparables = FALSE))#No hay duplicates en el fichero Loan
plot_bar(order)#En feature k_symbol hay operaciones que no están categorizadas
table(order$k_symbol)#Hay 1379 observaciones no categorizadas 

#Pasamos la variable amount de Order, de factor a numérica
order$amount<-as.numeric(as.character(order$amount)) 
#Traducimos los levels del Factor k_symbol correspondiente a la tipología de transferencia
levels(order$k_symbol)
order$k_symbol<-recode_factor(order$k_symbol, `POJISTNE` = "Insurance_Payment", `SIPO` = "Household_Payment", `UVER` = "Loan_Payment",`LEASING`="Leasing" )

#No vamos a transferir a Account las variables order_id,bank_to ni account_to porque no creemos que aporten información
#Vamos a transferir la tipología de transferencias que tiene cada cuenta y su cantidad e importe
#Creamos las nuevas features
account<-account%>%mutate(
  `Ord_Insurance`=numeric(dim(account)[1]),
  `Ord_Insurance_amount`=numeric(dim(account)[1]),
  `Ord_Household_Payment`=numeric(dim(account)[1]),
  `Ord_Household_Payment_amount`=numeric(dim(account)[1]),
  `Ord_Loan_Payment`=numeric(dim(account)[1]),
  `Ord_Loan_Payment_amount`=numeric(dim(account)[1]),
  `Ord_Leasing`=numeric(dim(account)[1]),
  `Ord_Leasing_amount`=numeric(dim(account)[1]),
  `Ord_Empty`=numeric(dim(account)[1]),#Hay una tipología de orders que son ""
  `Ord_Empty_amount`=numeric(dim(account)[1])
)

row_order=0
for (row_order in seq(1,dim(order)[1])){
  row_account<-hm_acc[[order[row_order,"account_id"]]]
  if (order[row_order,"k_symbol"]=="Insurance_Payment"){
    account[row_account,"Ord_Insurance"]=account[row_account,"Ord_Insurance"]+1
    account[row_account,"Ord_Insurance_amount"]=account[row_account,"Ord_Insurance_amount"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="Household_Payment"){
    account[row_account,"Ord_Household_Payment"]=account[row_account,"Ord_Household_Payment"]+1
    account[row_account,"Ord_Household_Payment_amount"]=account[row_account,"Ord_Household_Payment_amount"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="Loan_Payment"){
    account[row_account,"Ord_Loan_Payment"]=account[row_account,"Ord_Loan_Payment"]+1
    account[row_account,"Ord_Loan_Payment_amount"]=account[row_account,"Ord_Loan_Payment_amount"]+order[row_order,"amount"]
  }
  if (order[row_order,"k_symbol"]=="Leasing"){
    account[row_account,"Ord_Leasing"]=account[row_account,"Ord_Leasing"]+1
    account[row_account,"Ord_Leasing_amount"]=account[row_account,"Ord_Leasing_amount"]+order[row_order,"amount"]
  }
  
  if (order[row_order,"k_symbol"]==" "){
    account[row_account,"Ord_Empty"]=account[row_account,"Ord_Empty"]+1
    account[row_account,"Ord_Empty_amount"]=account[row_account,"Ord_Empty_amount"]+order[row_order,"amount"]
  }
} 


#Cargamos el fichero de tarjetas (cards) y revisamos su estructura y missings y duplicates
card <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/card.csv", header=TRUE)
head(card)
class(card)
str(card)
which(is.na(card)) #No hay missings
introduce(card)
unique(duplicated(card$card_id, incomparables = FALSE))#No hay duplicates en card_id
unique(duplicated(card$disp_id, incomparables = FALSE))#No hay disp_id con más de una card_id
unique(duplicated(card, incomparables = FALSE))#No hay duplicates en el fichero
plot_bar(card)

#La variable issued es un factor y queremos que tenga formato de fecha
card$issued<-as.character(card$issued)
card$issued<-substr(card$issued,1,6)
card$issued<-as.numeric(card$issued)
card$issued<-card$issued+19000000
card$issued<-as.Date.character(card$issued, "%Y%m%d")

#Incluimos las columnas que vamos a añadir con los datos de card( si la cuenta tiene tarjeta, que tipo de tarjeta y si es de Owner o disponent)
account<-account%>%mutate(
  `owner_card_type`=numeric(dim(account)[1]),
  `disponent_card_type`=numeric(dim(account)[1])
)

#El fichero card hay que relacionarlo a partir de la variable disp_id.
hm_disp_acc<-hashmap(disp$disp_id, disp$account_id)#Para cada disp_id, queremos saber a que account_id corresponde
hm_disp_type<-hashmap(disp$disp_id, disp$type)#Que tipo de OWNER/DISPONENT es cada disp_id

for (row_order in seq(1,dim(card)[1])){
  row_account<-hm_acc[[hm_disp_acc[[card[row_order,"disp_id"]]]]]
  if (hm_disp_type[[card[row_order,"disp_id"]]]=="2"){
    account[row_account,"owner_card_type"]<-card[row_order,"type"]
    account[row_account,"owner_card_date"]<-card[row_order,"issued"]
  }
  
  if (hm_disp_type[[card[row_order,"disp_id"]]]=="1"){
    account[row_account,"disponent_card_type"]<-card[row_order,"type"]
    account[row_account,"disponent_card_date"]<-card[row_order,"issued"]#No se genera esta columna, por lo tanto no hay disponents que tengan tarjeta
  }
}

#Vamos a factorizar la variable owner_card_type

account$owner_card_type=as.factor(account$owner_card_type)

#Cargamos el fichero de características socioeconómicas (district) y revisamos su estructura y missings y duplicates
district <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/district.csv", header=TRUE)
head(district)
class(district)
str(district)
which(is.na(district)) #No hay missings
introduce(district)
unique(duplicated(district, incomparables = FALSE))#No hay duplicates en el fichero
plot_bar(district)

#Pásamos a númerico las siguientes variables del fichero de district.La variable A10 que es de ratio of urban inhabitants.
#La variable A12 que es de Unemployment Rate '95.La variable A13 que es de Unemployment Rate '96
#La variable A15 que es de no. of committed crimes '95

district$A10=as.numeric(paste(district$A10))
district$A12=as.numeric(paste(district$A12)) #Hay que buscar NAs
district$A13=as.numeric(paste(district$A13))
district$A15=as.numeric(paste(district$A15))#Problema con el NA
position_A12<-which(is.na(district$A12))#La obs 69 es el NA
position_A15<-which(is.na(district$A15))#La obs 69 es el NA

#Reemplazamos los datos de la fila / distrito 69 para el unemployment rate'95 (A12) y para la variable A15 (no. of committed crimes '95)

#Para el missing value de unemployment rate'95, como es un ratio, consideramos la media de la región a la que pertence el NA
region_A12<-district[position_A12,"A3"]
district$A12[is.na(district$A12)] <-mean(district[district$A3==region,"A12"], na.rm = TRUE)

#Para el missing value de no.of committed crimes '95, como suponemos que esta cifra depende de la población, vamos a calcular un ratio entre no.of committed crimes '95
#y el total de la población y con la media de esa cifra para la región, estimamos el missing value 
#consideramos la media de la región a la que pertence el NA
region_A15<-district[position_A15,"A3"]
district<-mutate(district,crimes_95_ratio=district$A15/district$A4)
district$crimes_95_ratio[position_A15]
district$crimes_95_ratio[position_A15]<-mean(district[district$A3==region,"crimes_95_ratio"], na.rm = TRUE)
district$A15[is.na(district$A15)] <-mean(district[district$A3==region,"crimes_95_ratio"], na.rm = TRUE)*district[position_A15,"A4"]

#Cambiamos los nombres de district para que sean más inteligibles 
names(district)[1]<-names(account)[2]#"district_id"
names(district)[2]='district_name'
names(district)[3]='region'
names(district)[4]='num_inhabitants'#no. of inhabitants
names(district)[5]='municip < 499'#no. of municipalities with inhabitants < 499
names(district)[6]='municip 500-1999' #no. of municipalities with inhabitants 500-1999
names(district)[7]='municip 2000-9999'#no. of municipalities with inhabitants 2000-9999
names(district)[8]='municip > 10000'#no. of municipalities with inhabitants >10000
names(district)[9]='num_cities'#no. of cities
names(district)[10]='ratio_urban_inhabitants'#ratio of urban inhabitants
names(district)[11]='avg_salary'#average salary
names(district)[12]='unemployment_rate_95'#unemployment rate '95
names(district)[13]='unemployment_rate_96'#unemployment rate '96
names(district)[14]='entrepreneurs'#no. of entrepreneurs per 1000 inhabitants
names(district)[15]='crimes_95'#no. of committed crimes '95
names(district)[16]='crimes_96'#no. of committed crimes '96


#Vamos a pasar a ratio algunas variables de crimes 96 y entrepreneurs
district<-mutate(district,crimes_96_ratio=district$crimes_96/district$num_inhabitants,entrepreneurs_ratio=district$entrepreneurs/district$num_inhabitants)

#Incorporamos las columnas desde district
account=merge(account,district,by="district_id", all.x=TRUE)

#Actualizamos el diccionario de account
hm_acc<-hashmap(account$account_id,seq(1:dim(account)[1]))#Para una key= account_id me da la fila del df account en el que está account_id

#Cargamos el fichero de transacciones (trans) y revisamos su estructura y missings y duplicates
trans <- read.csv2(file="C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo/trans.csv", header=TRUE)
head(trans)#LA feature "bank account" parece que tiene missings 
class(trans)
str(trans)
which(is.na(trans)) #SI hay missings
introduce(trans)
unique(duplicated(trans, incomparables = FALSE))#No hay duplicates en el fichero
plot_intro(trans)
plot_missing(trans)#Hay missings en la feature account que no la vamos a utilizar porque es la cuenta de destino. No hay missings en account_id que es por la que vamos a vincular
plot_bar(trans)#Hay transacciones no categorizadas en las features "operation" y "k_symbol" 

#Vamos a buscar los no categorizados en las features de trans
table(trans$k_symbol)# En k_symbol hay 535.314 observaciones no categorizadas
table(trans$operation)# En operation hay 183.114 observaciones no categorizadas

#Las variables balance y amount aparecen como factores y deberían ser numéricas
trans$amount<-as.numeric(as.character(trans$amount)) 
trans$balance<-as.numeric(as.character(trans$balance))

#Revisamos la feature "balance". Se corresponde con el saldo que tiene la cuenta después de la transacción
min(trans$balance)#Hay saldos negativos. Puede ser un buen indicador para calidad crediticia
str(trans[trans$amount==trans$balance,])#En algunos casos (4.501) el saldo después de la transacción se corresponde con el importe de la transacción 

#Recodificamos las variables "type", "operation" y "k_symbol":

#Feature "type": Básicamente indica si la transacción es de cargo o de abono
trans$type<-recode_factor(trans$type,`PRIJEM` = "Type_Credit", `VYBER` = "Type_VYBER", `VYDAJ` = "Type_withdrawal")
table(trans$type)#La categoría de "type" VYBER no está traducida, pero sólo una minoría de observaciones las que tienen esa categoría (16.666)

#Feature "operation": Indica si la operación es un envío de dinero (Remittance), un ingreso (collection), un ingreso en efectivo (CashCredit),
#un reintegro en efectivo (WithdrawalCash) o un reintegro con tarjeta (WithdrawalCreditCard)
levels(trans$operation)<-c("Op_Null","Op_Remittance","Op_Collection","Op_CashCredit","Op_WithdrawalCash","Op_WithdrawalCreditCard") #Cambiamos la denominación de la variable "operation"
unique(trans[trans$operation=="Op_Remittance","type"])#Todas las Remittances se correponden con  Type_withdrawal de la feature type
unique(trans[trans$operation=="Op_Collection","type"])#Todas las Collections se correponden con  Type_Credit de la feature type
unique(trans[trans$operation=="Op_CashCredit","type"])#Todas las Collections se correponden con  Type_Credit de la feature type
unique(trans[trans$operation=="Op_WithdrawalCash","type"])#Todas las Collections se correponden con Type_withdrawal y Type_VYBE de la feature type
unique(trans[trans$operation=="Op_WithdrawalCreditCard","type"])#Todas las Collections se correponden con Type_withdrawal de la feature type

#Feature "k_symbol": Indica si la operación es cobro de pensión (Sym_Pension), un pago de seguro (Sym_Insurance),
#un pago por saldo negativo (Sym_NegBal), un cobro por intereses de un depósito (Sym_IntDep),
#un pago por un préstamo (Sym_LoanPayment) o un pago de Household o Statement
levels(trans$k_symbol)<-c("Sym_Null","Sym_Null2","Sym_Pension","Sym_Insurance","Sym_NegBal","Sym_Household","Sym_Statement","Sym_IntDep","Sym_LoanPayment")
unique(trans[trans$k_symbol=="Sym_Pension","operation"])#Todas se corresponden con la "operation" Collection 
unique(trans[trans$k_symbol=="Sym_Insurance","operation"])#Todas se corresponden con "Op_Remittance" y "Op_WithdrawalCash"
unique(trans[trans$k_symbol=="Sym_NegBal","operation"])#Todas se corresponden con "Op_WithdrawalCash"
unique(trans[trans$k_symbol=="Sym_Household","operation"])#Todas se corresponden con "Op_Remittance" y "Op_WithdrawalCash"
unique(trans[trans$k_symbol=="Sym_Statement","operation"])#Todas se corresponden con "Op_WithdrawalCash"
unique(trans[trans$k_symbol=="Sym_IntDep","operation"])#Todas se corresponden con "Op_Null"
unique(trans[trans$k_symbol=="Sym_LoanPayment","operation"])#Todas se corresponden con "Op_Remittance"

#Para cada account vamos a completar la información con los datos del DataFrame de transacciones (trans)
#Se incluye el tipo de opertiva que tiene cada cuenta. Vamos a incluir el tipo de transacciones que realiza cada cuenta

account<-account%>%mutate(
  #Categorías de la feature type
  `Num_Type_Credit`=numeric(dim(account)[1]),
  `Num_Type_VYBER`=numeric(dim(account)[1]),
  `Num_Type_Withdrawal`=numeric(dim(account)[1]),
  #Categorías de la feature operation
  `Num_Op_Null`=numeric(dim(account)[1]),
  `Num_Op_Remittances`=numeric(dim(account)[1]),
  `Num_Op_Collection`=numeric(dim(account)[1]),
  `Num_Op_CashCredit`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCash`=numeric(dim(account)[1]),
  `Num_Op_WithdrawalCreditCard`=numeric(dim(account)[1]),
  #Categorías de la feature k_symbol
  `Num_Sym_Null`=numeric(dim(account)[1]),
  `Num_Sym_Null2`=numeric(dim(account)[1]),
  `Num_Sym_Pension`=numeric(dim(account)[1]),
  `Num_Sym_Insurance`=numeric(dim(account)[1]),
  `Num_Sym_NegBal`=numeric(dim(account)[1]),
  `Num_Sym_Household`=numeric(dim(account)[1]),
  `Num_Sym_Statement`=numeric(dim(account)[1]),
  `Num_Sym_IntDep`=numeric(dim(account)[1]),
  `Num_Sym_LoanPayment`=numeric(dim(account)[1]),
  `Balance_in_negative`=numeric(dim(account)[1])
)

for (row_order in seq(1,dim(trans)[1])){
  row_account<-hm_acc[[trans[row_order,"account_id"]]]
  #Completamos los atributos de feature type
  
  if (trans[row_order,"type"]=="Type_Credit"){
    account[row_account,"Num_Type_Credit"]=account[row_account,"Num_Type_Credit"]+1
  }
  if (trans[row_order,"type"]=="Type_VYBER"){
    account[row_account,"Num_Type_VYBER"]=account[row_account,"Num_Type_VYBER"]+1
  }
  if (trans[row_order,"type"]=="Type_withdrawal"){
    account[row_account,"Num_Type_Withdrawal"]=account[row_account,"Num_Type_Withdrawal"]+1
  }
#Completamos los atributos de feature operation
  
  if (trans[row_order,"operation"]=="Op_Null"){
    account[row_account,"Num_Op_Null"]=account[row_account,"Num_Op_Null"]+1
  }
  if (trans[row_order,"operation"]=="Op_Remittance"){
    account[row_account,"Num_Op_Remittances"]=account[row_account,"Num_Op_Remittances"]+1 
  }
  if (trans[row_order,"operation"]=="Op_Collection"){
    account[row_account,"Num_Op_Collection"]=account[row_account,"Num_Op_Collection"]+1 
  } 
  if (trans[row_order,"operation"]=="Op_CashCredit"){
    account[row_account,"Num_Op_CashCredit"]=account[row_account,"Num_Op_CashCredit"]+1 
  }
  if (trans[row_order,"operation"]=="Op_WithdrawalCash"){
    account[row_account,"Num_Op_WithdrawalCash"]=account[row_account,"Num_Op_WithdrawalCash"]+1 
  }  
  if (trans[row_order,"operation"]=="Op_WithdrawalCreditCard"){
    account[row_account,"Num_Op_WithdrawalCreditCard"]=account[row_account,"Num_Op_WithdrawalCreditCard"]+1 
  }  
#Completamos los atributos de tipo k_symbol
  
  if (trans[row_order,"k_symbol"]=="Sym_Null"){
    account[row_account,"Num_Sym_Null"]=account[row_account,"Num_Sym_Null"]+1
  }
  if (trans[row_order,"k_symbol"]=="Sym_Null2"){
    account[row_account,"Num_Sym_Null2"]=account[row_account,"Num_Sym_Null2"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_Pension"){
    account[row_account,"Num_Sym_Pension"]=account[row_account,"Num_Sym_Pension"]+1 
  } 
  if (trans[row_order,"k_symbol"]=="Sym_Insurance"){
    account[row_account,"Num_Sym_Insurance"]=account[row_account,"Num_Sym_Insurance"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_NegBal"){
    account[row_account,"Num_Sym_NegBal"]=account[row_account,"Num_Sym_NegBal"]+1 
  }  
  if (trans[row_order,"k_symbol"]=="Sym_Household"){
    account[row_account,"Num_Sym_Household"]=account[row_account,"Num_Sym_Household"]+1 
  } 
  if (trans[row_order,"k_symbol"]=="Sym_Statement"){
    account[row_account,"Num_Sym_Statement"]=account[row_account,"Num_Sym_Statement"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_IntDep"){
    account[row_account,"Num_Sym_IntDep"]=account[row_account,"Num_Sym_IntDep"]+1 
  }
  if (trans[row_order,"k_symbol"]=="Sym_LoanPayment"){
    account[row_account,"Num_Sym_LoanPayment"]=account[row_account,"Num_Sym_LoanPayment"]+1 
  }
  #Para cada transacción verificamos si el saldo ha quedado en negativo y lo incluimos como una feature más 
  if (trans[row_order,"balance"]<0){
    account[row_account,"Balance_in_negative"]=account[row_account,"Balance_in_negative"]+1 
  }
}

#Ya hemos recpilado la información que consideramos relevante de la información disponible
#Hemos completado el DataFrame "account"con la información que inicialmente hemos considerado relevante
#y a continuación vamos a revisar las variables que hemos obtenido

#La variable district, la vamos a considerar un factor
account2[,"district_id"]<-as.factor(account2$district_id)

#Ahora vamos a trabajar sobre client_id_disponent y loan_id, 
account<-account%>%mutate(
  `account_disponent_bin`=numeric(dim(account)[1]),#Vemos si la cuenta tiene autorizado o no
  `account_loan_bin`=numeric(dim(account)[1]))#Vemos si la cuenta ha contratado préstamo o no
  
#Ahora completamos estas nuevas columnas
for (row_order in seq(1,dim(account)[1])){
        #Vemos si la cuenta ha contratado préstamo
            if (is.na(account[row_order,"loan_id"])==TRUE){
                  account[row_order,"account_loan_bin"]<-0
                }
            else (account[row_order,"account_loan_bin"]<-1)
      #Vemos si la cuenta tiene autorizado
            if (is.na(account[row_order,"client_id_disponent"])==TRUE){
                  account[row_order,"account_disponent_bin"]<-0
               }
           else (account[row_order,"account_disponent_bin"]<-1)
      }

#Convertimos en factor estas nuevas variables
account[,"account_loan_bin"]<-as.factor(account$account_loan_bin)
account[,"account_disponent_bin"]<-as.factor(account$account_disponent_bin)

#Factorizamos las variables de Sex
account[,"sex_owner"]<-as.factor(account$sex_owner)
account[,"sex_disponent"]<-as.factor(account$sex_disponent)

setwd("C:/Master Data Science/Master en Data Science/TFM/Transacciones de Banco Checo")
write.csv(account, file="DFTenenciaProductos.csv")



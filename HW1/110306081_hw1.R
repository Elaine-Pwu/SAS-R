##import library
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
options(dplyr.summarise.inform = FALSE)

#1. 
stock<-read_csv('stock.csv') #read stock.csv

stock <- stock %>% separate(id_name, into = c("id", "name"),sep = " ") %>% 
  gather(year_month_date, money, '2023/12/1':'2024/3/7') %>%
  separate (year_month_date, into = c("year", "month", "date"),sep = "/") %>% 
  spread(type,money)
stock
#step1 separate id_name into id and name 
#step2 set a column name year_month_date to gather the columns 2023/12/1:2024/3/7 and set column money to store the value of money of year_month_date 
#step3 separate the year_month_date into three columns: year, month, and date
#step4 spread type into close and open and store the money value


#2. 
sales.df <-read_csv('salesdata.csv')
sales.df
client.df <-read_csv('client_list.csv')
client.df
prod.df <-read_csv('product_list.csv')
prod.df

##2.1
prod.df<- prod.df %>% separate(Item, into = c("Product", "Item"), sep = "_", convert = TRUE)
prod.df

##2.2
full.table<- sales.df %>% 
  left_join(prod.df, by = "Product") %>% 
  left_join(client.df, by="Client", suffix=c("",".y"))%>%
  select(-ends_with(".y"))

full.table <-na.omit(full.table)
full.table

##2.3
full.table<-full.table%>%
  mutate(spend = UnitPrice*Quantity)
full.table

###2.4
#number of people
full.table$Group <- ifelse(full.table$Membership %in% c("gold", "diamond"), "Gold/Diamond", "Other")
full.table%>%
  group_by(Group)%>%
  distinct(Client)%>%
  summarize(n())

#age
full.table%>%
  group_by(Group)%>%
  distinct(Client,Age)%>%
  summarize(avg_age = mean(Age))

#gender
full.table%>%
  group_by(Group, Gender)%>%
  distinct(Client)%>%
  summarize(Number_of_people = n())

#region 
full.table%>%
  group_by(Group, Region)%>%
  summarize(Number_of_people = n())

#total spend
full.table%>%
  group_by(Group)%>%
  summarize(total_spend = sum(spend))

#mean spend
full.table%>%
  group_by(Group)%>%
  summarize(mean_spend = mean(spend))

#Info
full.table%>%
  group_by(Group, Gender, Region)%>%
  summarise(avg_age = mean(Age), total_spend = sum(spend))

###2.5
f.table<-full.table%>%
  filter(Gender %in% "female")
f.table

##remove na
sum(is.na(f.table$Item))

#number of people
f.table%>%
  group_by(Group)%>%
  distinct(Client)%>%
  summarise(Number_of_people = n())

#Item
table(f.table$Item)

#Region
table(f.table$Region)


f.table%>%
  group_by(Group)%>%
  distinct(Client,Age)%>%
  summarise(Average_age = sum(Age)/n())

c1 = table(f.table$Item)
c1 = as.data.frame(c1)

c2 = table(f.table$Group)
c2= as.data.frame(c2)

piepercent <- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%")
pie(c1$Freq,labels = piepercent, main = "Item",col = rainbow(length(c1$Var1)),cex=0.5)
legend("topright",as.character(c1$Var1), cex = 0.5,
       fill = rainbow(length(c1$Var1)))

piepercent <- paste(round(100*c2$Freq/sum(c2$Freq), 2), "%")
pie(c2$Freq,labels =piepercent, main = "Group",col = rainbow(length(c2$Var1)),cex=0.5)
legend("topright",as.character(c2$Var1), cex = 0.5,
       fill = rainbow(length(c2$Var1)))
#銷售額
ProductSale <- f.table %>%
  group_by(Item) %>%
  summarise(TotalPSale = sum(spend))
ProductSale %>% 
  ggplot(aes(x= Item, y=TotalPSale, fill = Item)) + geom_bar(stat="identity") + labs(y="Total Product Sales Amount") 

#各個顧客的花費細圖
ggplot(data = f.table) +
  geom_bar( aes( x = Item,
                 y = spend,fill = Item),
            stat = 'identity', ) +
  facet_wrap( ~ Client)

#各個地區的數量統計
ggplot(data= f.table,aes(x=factor(Region),fill=Region)) + geom_bar( ) 

#各個產品的銷售數目
ggplot(data = f.table) + geom_bar(aes(x=factor(Item),fill=Item) ) 

#各個顧客的花費
TotalSales <- f.table %>%
  group_by(Client) %>%  
  summarise(TotalSpend = sum(spend)) %>% 
  arrange(desc(TotalSpend))
TotalSales

#顧客與產品的花費和比例
Product <- f.table %>%
  group_by(Group, Item) %>%
  summarise(AmountSpend = sum(spend)) %>%
  mutate( Proportion = round(AmountSpend / sum(AmountSpend),1)*100)
Product

#各個顧客的總消費
ggplot(data = TotalSales) +
  geom_bar(aes(x=factor(Client), y=TotalSpend,fill=Client),stat = 'identity') 

# 從各組花費看各產品銷售比例
ggplot( data = Product, aes( x = Group, y = AmountSpend, fill = Item)) +  geom_bar(stat="identity")  +
  geom_text(aes( x = Group, y = AmountSpend,  label= paste(Proportion, '%', sep='')), position = position_stack(vjust = 0.5), color = I("white"), size = 3) 

# 從各產品銷售看各組比例
ggplot( data = Product, aes( x = Item, y = AmountSpend, fill = Group)) +  geom_bar(stat="identity")  +
  geom_text(aes( x = Item, y = AmountSpend,  label= paste(Proportion, '%', sep='')), position = position_stack(vjust = 0.5), color = I("white"), size = 3) 
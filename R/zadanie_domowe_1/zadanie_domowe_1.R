library(dplyr)
library(tidyr)
library(lubridate)

df_orders <- read.csv('dane/orders.csv')
df_order_items<- read.csv('dane/order_items.csv')
df_products <- read.csv('dane/products.csv')
df_brands <- read.csv('dane/brands.csv')
df_categories <-  read.csv('dane/categories.csv')
df_customers<- read.csv('dane/customers.csv')


####### Zadanie 1
# Który produkt był najczęściej kupowany w każdym kwartale w podziale na stany z których pochodzą klienci? 
# Podaj nazwę produktu i rok jego produkcji.
orders1 <-df_orders %>% 
  mutate(months_date=strftime(order_date,'%Y-%m')) %>% 
  group_by(months_date)
tym<-left_join(df_order_items,orders1)
ans<-left_join(tym,df_customers)
ANS<-ans %>%
  group_by(year=strftime(order_date,"%Y"), quarter=ceiling(as.integer(strftime(order_date,"%m"))/3), state,product_id) %>%
  summarise(sum_quantity=sum(quantity)) %>%
  group_by(year,quarter,state) %>%
  filter(sum_quantity==max(sum_quantity)) %>%
  left_join(df_products) %>%
  arrange(year,quarter,state) %>% 
  select(model_year,product_name,year,quarter,state)

## Odpowiedz przypisana do zmiennej

ANS_TASK_01 <- ANS


####### Zadanie 2 
# Jaki procent wszystkich zamowien nie został zrealizowany w kazdym miesiącu? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_02 <- df_orders %>% 
  group_by(year=strftime(required_date,'%Y'),months=strftime(required_date,'%m')) %>%
  summarise(all=n(), not_completed=sum(shipped_date=='NULL'), percent=(not_completed/all)*100) %>%
  select(months,percent)

####### Zadanie 3
# Jaki produkt przyniósł największy przychód w kazdym roku?
## Odpowiedz przypisana do zmiennej
ANS_TASK_03 <- df_order_items %>% 
  mutate(income=(list_price - list_price*discount)*quantity) %>%
  left_join(df_products) %>%
  left_join(df_orders) %>%
  group_by(product_id,year=strftime(order_date,'%Y'))%>%
  summarise(annual_income=sum(income),year,product_name) %>%
  group_by(year) %>%
  filter(annual_income==max(annual_income)) %>%
  distinct() %>%
  select(year, product_name)


####### Zadanie 4
## Ile klientów zrobilo najwieksze zakupy (czyli zrobili najwięcej zamówień) w kazdym roku i ile to było zamówień? 

## Odpowiedz przypisana do zmiennej
ANS_TASK_04 <-  df_orders %>%
  group_by(customer_id, year = strftime(order_date, "%Y")) %>%
  summarise(number_of_orders = n()) %>%
  group_by(year) %>%
  filter(number_of_orders==max(number_of_orders)) %>%
  group_by(number_of_orders, year) %>%
  summarise(number_of_customers=n())
  

####### Zadanie 5
# Z jakiej domeny mailowej najczęsciej robiono zamówienia w każdym roku? Ile było tych zamówień?

## Odpowiedz przypisana do zmiennej
ANS_TASK_05 <- df_orders %>% 
  left_join(df_customers) %>%
  group_by(year=strftime(order_date, "%Y"),domena= sub(".*@(.*)\\.com$", "\\1", email)) %>%
  summarise(count = n()) %>% 
  filter(count == max(count))



####### Zadanie 6
# Oblicz jaka była maksymalna i minimalna oraz mediana liczby zamówień złożonych każdego dnia w poszczególnych kwartalach.
orders<-df_orders
orders$order_date <- as.Date(orders$order_date)
daily_orders<-orders %>%
  mutate(quarter=quarters(order_date),year=strftime(order_date,'%Y'))  %>%
  group_by(order_date, quarter,year) %>%
  summarise(count=n()) %>%
  group_by(year,quarter) %>%
  summarise(min=min(count), max=max(count),median=median(count))

## Odpowiedz przypisana do zmiennej
ANS_TASK_06 <- daily_orders


####### Zadanie 7
# Jaki był średni czas dostarczania zamówienia w zależności od roku i  stanu w którym mieszkał klient. 
# Jako rozwiązanie przygotuj szeroka postac tabeli, która będzie miała informację o każdym stanie w innej kolumnie
tmp<- df_orders %>%
  inner_join(df_customers) %>%
  group_by(year=strftime(order_date,'%Y'),state) %>%
  summarise(average=mean(as.Date(shipped_date)-as.Date(order_date),na.rm=TRUE))
?pivot_wider
## Odpowiedz przypisana do zmiennej
ANS_TASK_07 <- pivot_wider(tmp,names_from = state,values_from = average)



####### Zadanie 8
# Zrób zestawienie (szeroka postać tabeli) ile razy każdy klient kupił rower każdej kategorii. 
# Jeśli nie kupował takiego roweru zaraportuj wartośc"0"
# Dodaj do zestawienia informację ile razy klient kupował najnowszy produkt (rower został wyprodukowany w tym roku, kiedy złożono zamówienie)
pivot<-df_orders %>%
  full_join(df_order_items)%>%
  full_join(df_products)%>%
  full_join(df_categories) %>%
  full_join(df_customers) %>%
  group_by(customer_id,category_name)%>%
  summarise(count=sum(quantity))


ans8<-pivot_wider(pivot, names_from = category_name,values_from = count,values_fill = 0)

counter_newest<-df_orders %>%
  full_join(df_order_items)%>%
  full_join(df_products)%>%
  full_join(df_categories) %>%
  full_join(df_customers) %>%
  filter(model_year==strftime(order_date,'%Y')) %>%
  group_by(customer_id) %>%
  summarise(newest=sum(quantity)) 

##Odpowiedz przypisana do zmiennej
ANS_TASK_8  <- left_join(ans8,counter_newest)%>%
  mutate(newest = ifelse(is.na(newest), 0, newest))

### Zadanie 9
# Jaki był średni rabat udzielony na każdy produkt w każdym dniu tygodnia?
# Jako średni rabat rozumiemy różnicę procentową miedzy przychodem wynikającym z ceny katalogowej a przychodem faktycznym uwzględniającym udzielony rabat
average_discount <- order_items %>%
  full_join(orders %>% select(order_id, order_date), by = "order_id") %>%
  mutate(price_after_discount = list_price * (1 - discount)) %>%
  mutate(discount_percentage = (list_price - price_after_discount) / list_price * 100) %>%
  mutate(order_date = ymd(order_date)) %>%
  mutate(day_of_week = wday(order_date, label = TRUE, abbr = FALSE)) %>%
  group_by(product_id, day_of_week) %>%
  summarise(average_discount = sum(discount_percentage * quantity) / sum(quantity))
## Odpowiedz przypisana do zmiennej
ANS_TASK_9 <- average_discount



### Zapisanie rozwiązań do pliku .RDS

### Proszę nic nie zmieniać 
solutions <- list(ANS_TASK_01, ANS_TASK_02, ANS_TASK_03, ANS_TASK_04, ANS_TASK_05, ANS_TASK_06,
                  ANS_TASK_07, ANS_TASK_08, ANS_TASK_09, ANS_TASK_10, ANS_TASK_11, ANS_TASK_12)

names(solutions) <- c("Task01", "Task02", "Task03", "Task04", "Task05", "Task06", "Task07",
                      "Task08", "Task09", "Task10", "Task11", "Task12")

### Proszę zmienić tylko nazwę pliku w komendzie saveRDS na swoje Nazwisko i Imię (bez polskich znaków)
saveRDS(solutions, file = "JamrozMateusz.rds")


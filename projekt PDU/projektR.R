### Przetwarzanie Danych Ustrukturyzowanych 2024L
### Praca domowa nr. 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###
# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment
# -----------------------------------------------------------------------------#
# Wczytanie niezbędnych pakietów

   # library(sqldf)
   # library(dplyr)
   # library(data.table)
   # library(microbenchmark)
   # library(compare)
# 
# 
# # Wczytanie danych
  #  Posts <- read.csv('travel_stackexchange_com/Posts.csv.gz')
   #  head(Posts)
  #  Users <- read.csv('travel_stackexchange_com/Users.csv.gz')
    # head(Users)
  # Comments <- read.csv("travel_stackexchange_com/Comments.csv.gz")
    # head(Comments)
  # Votes <- read.csv("travel_stackexchange_com/Votes.csv.gz")
   #  head(Votes)
  # PostLinks <- read.csv("travel_stackexchange_com/PostLinks.csv.gz")
   #  head(PostLinks)
    



sql_1 <- function(Users){
  # Tu umiesc rozwiazanie oraz komentarze
  # 
  # SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month - Tworzy kolumny rok i miesiąc wybierając odpowiednie dane z kolumny CreationDate.
  # COUNT(*) AS TotalAccountsCount - Liczy wszystkie konta użytkowników i przypisuje to do kolumny o nazwie TotalAccountsCount.
  # AVG(Reputation) AS AverageReputation: Oblicza średnią reputację użytkowników i przypisuje to do kolumny o nazwie AverageReputation.
  # FROM Users: Wskazuje miejsce skad pobrane są dane
  # GROUP BY Year, Month: Grupuje wyniki według roku i miesiąca, co oznacza, że obliczenia będą dokonywane osobno dla każdego roku i miesiąca.
  
  # Finalnie jest to grupowanie użytkowników według roku i miesiąca ich rejestracji, obliczenie całkowitej liczby nowych kont i średniej reputacji dla każdego miesiąca.
  
  
  sqldf::sqldf("
        SELECT STRFTIME('%Y', CreationDate) AS Year,
STRFTIME('%m', CreationDate) AS Month,
COUNT(*) AS TotalAccountsCount,
AVG(Reputation) AS AverageReputation
FROM Users
GROUP BY Year, Month

    ")
}


base_1 <- function(Users){
  # Tu umiesc rozwiazanie oraz komentarze
      #
    
  #Konwersja CreationDate na datę
  Users$CreationDate <- as.Date(Users$CreationDate)
  
  # Utworzenie kolumn Year i Month
  Users$Year <- format(Users$CreationDate, "%Y")
  Users$Month <- format(Users$CreationDate, "%m")
  
  # zliczenie ilosci kont po roku i miesiącu
  result <- aggregate(Reputation ~ Year + Month, data = Users,
                      FUN = function(x) c(TotalAccountsCount = length(x)))
  
  # agregacja średnej reputacji dla danego miesiącu w danym roku
  result1 <- aggregate(Reputation ~ Year + Month, data = Users,
                      FUN=function(x) c(AverageReputation = mean(x, na.rm = TRUE)))
  
  # przyporządkowanie nazw kolumn
  names(result)<-c("Year","Month","TotalAccountsCount")
  names(result1)<-c("Year","Month","AvarageReputation")
  
  # sortowanie wyniku po roku, a następnie po miesiącu
  result <- result[order(result$Year, result$Month), ]
  result1<-result1[order(result1$Year, result1$Month), ]
  
  # resetowanie indeksów
  row.names(result) <- NULL
  row.names(result1)<-NULL
  
  # dodanie do result kolumny AvarageReputation
  result<-data.frame(result, result1[3])
  
  # zwracanie wyników
  result
          
            
  
  
  
}


dplyr_1 <- function(Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  #Konwersja CreationDate na datę
  Users <- Users %>%
    mutate(CreationDate = as.Date(CreationDate))
  
  # Utworzenie kolumn Year i Month
  Users <- mutate(Users, 
                  Year = format(CreationDate, "%Y"),
                  Month = format(CreationDate, "%m"))
  
  # # zliczenie ilosci kont po roku i miesiącu
  result <- Users %>%
    group_by(Year, Month) %>%
    summarise(TotalAccountsCount = n())
  
  # policzenie średnej reputacji dla danego miesiącu w danym roku
  result1 <- Users %>%
    group_by(Year, Month) %>%
    summarise(AverageReputation = sum(Reputation, na.rm = TRUE) / sum(!is.na(Reputation)))
  
  # Zmiana nazw kolumny
  names(result)[3] <- "TotalAccountsCount"
  # dodanie do result kolumny AvarageReputation
  result<-data.frame(result, result1[3])
  
  # Sortowanie wyników po roku, a następnie po miesiącu
  result <- result %>% 
    arrange(Year, Month)
  
  # Resetowanie indeksów wierszy
  row.names(result) <- NULL
  
  # zwracanie wyników
  
  result
}

table_1 <- function(Users){
  
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja Users na data.table
  setDT(Users)
  
  # Konwersja CreationDate na datę
  Users[, CreationDate := as.Date(CreationDate)]
  
  # Wyodrębnienie roku i miesiąca z CreationDate
  Users[, Year := format(CreationDate, "%Y")]
  Users[, Month := format(CreationDate, "%m")]
  
  # zliczenie ilosci kont i policzenie średnej reputacji po roku i miesiącu
  Users<-Users[, .(TotalAccountsCount=.N, AveregeReputation=mean(Reputation)), by =.(Year, Month)]
  # konwersja na data.frame
  result<-setDF(Users)
  # zwracanie wyników
  result
}



# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
   # compare(sql_1(Users), base_1(Users), allowAll = TRUE)
   # compare(sql_1(Users), dplyr_1(Users),allowAll = TRUE)
   # compare(sql_1(Users), table_1(Users), allowAll = TRUE)
   # compare(dplyr_1(Users), table_1(Users), allowAll = TRUE)
   # compare(base_1(Users), dplyr_1(Users),allowAll = TRUE)
   # compare(base_1(Users), table_1(Users), allowAll = TRUE)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark::microbenchmark(
#   sqldf = sql_1(Users),
#   base = base_1(Users),
#   dplyr = dplyr_1(Users),
#   data.table = table_1(Users)
# )

# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # SELECT Users.DisplayName, Users.Location, Users.Reputation, STRFTIME('%Y-%m-%d', Users.CreationDate) AS CreationDate, Answers.TotalCommentCount - Wybiera kolumny zawierające DisplayName użytkownika, 
  # lokalizację, reputację, datę utworzenia konta (rok, miesiac i dzien) oraz Answers.TotalCommentCount
  
  # (SELECT OwnerUserId, SUM(CommentCount) AS TotalCommentCount FROM Posts WHERE PostTypeId = 2 AND OwnerUserId != '' GROUP BY OwnerUserId) AS Answers - wybranie OwnerUserId i sumy CommentCount jako Answers, 
  # spełniajace warunki że PostTypeId = 2 i OwnerUserId != '' z posts i zgrupowane po OwnerUserId.
   
  # JOIN Users ON Users.Id = Answers.OwnerUserId: Łączy w jedną tabele TotalCommentCount z Users gdzie Id w Users jest takie samo jak OwnerUserId
  # ORDER BY TotalCommentCount DESC: Sortuje wyniki malejąco według TotalCommentCount
  # LIMIT 10: Ogranicza wyniki do 10 pierwszych wyników
  
  # Ostatecznie zapytanie to zwraca dane o 10 użytkownikach z największym TotalCommentCount
    sqldf::sqldf("SELECT Users.DisplayName, Users.Location, Users.Reputation,
                 STRFTIME('%Y-%m-%d', Users.CreationDate) AS CreationDate,
                 Answers.TotalCommentCount
          FROM (SELECT OwnerUserId, SUM(CommentCount) AS TotalCommentCount
                FROM Posts
                WHERE PostTypeId = 2 AND OwnerUserId != ''
                GROUP BY OwnerUserId) AS Answers
          JOIN Users ON Users.Id = Answers.OwnerUserId
          ORDER BY TotalCommentCount DESC
          LIMIT 10")
}

base_2 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja CreationDate na datę
  Users$CreationDate <- as.Date(Users$CreationDate)
  
  # wyselekcjonowanie Odpowiednich wierszy i agregacja komentarzy dla kazdego OwnerUserId
  Answers <- aggregate(CommentCount ~ OwnerUserId, subset(Posts, PostTypeId == 2 & OwnerUserId != ""), FUN = sum)
  
  # Zmiana nazw kolumn
  colnames(Answers) <- c("OwnerUserId", "TotalCommentCount")
  
  # Łączenie Users i Answers po kolumnach Id z Users i OwnerUserId z Answers
  top_users <- merge(Users, Answers, by.x = "Id", by.y = "OwnerUserId", all.x = TRUE)
  
  # uporządkowanie elementów po TotalCommentCount malejąco
  top_users <- top_users[order(top_users$TotalCommentCount, decreasing = TRUE), ]
  
  # wybranie 10 pierwszych wierszy
  top_users <- head(top_users, 10)
  
  # wybranie odpowiednich kolumn
  result <- top_users[, c("DisplayName", "Location", "Reputation","CreationDate", "TotalCommentCount"), drop = FALSE] 
  
  # konwersja na data.frame
  result<- data.frame(result)
  
  # resetowanie indeksów
  row.names(result) <- NULL
  
  # zwracanie wyników
  result
}

dplyr_2 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja CreationDate na datę
  Users <- Users %>%
    mutate(CreationDate = as.Date(CreationDate))
  
  # wyselekcjonowanie Odpowiednich wierszy i opliczenie komentarzy dla kazdego OwnerUserId
  Answers <- Posts %>%
    filter(PostTypeId == 2 & OwnerUserId != "") %>%
    group_by(OwnerUserId) %>%
    summarise(CommentCount = sum(CommentCount))
  
  # Zmiana nazw kolumn
  colnames(Answers) <- c("OwnerUserId", "TotalCommentCount")
  
  # Łączenie Users i Answers po kolumnach Id z Users i OwnerUserId z Answers
  top_users<-left_join(x=Users,y=Answers, by = c("Id" = "OwnerUserId"))
  
  # uporządkowanie elementów po TotalCommentCount malejąco
  top_users <- top_users %>%
        arrange(desc(TotalCommentCount))
  
  # wybranie odpowiednich kolumn
  top_users <- top_users %>%
    select(DisplayName, Location, Reputation, CreationDate, TotalCommentCount)
  
  # wybranie 10 pierwszych wierszy
  result <- top_users %>%
    slice_head(n = 10)
  
  #konwersja na data.frame
  result<- data.frame(result)
  
  # zwracanie wyników
  result
}

table_2 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja Users i Posts na data.table
  setDT(Posts)
  setDT(Users)
  
  # Konwersja CreationDate na datę
  Users$CreationDate <- as.Date(Users$CreationDate)
  
  # wyselekcjonowanie Odpowiednich wierszy i obliczenie komentarzy dla kazdego OwnerUserId
  Answers <- Posts[PostTypeId==2 & OwnerUserId!='',.(TotalCommentCount=sum(CommentCount)), by=OwnerUserId]
  
  # Łączenie Users i Answers po kolumnach Id z Users i OwnerUserId z Answers
  result <- merge(Users, Answers, by.x = "Id", by.y = "OwnerUserId")
  
  # wybranie odpowiednich kolumn, konwersja na data.frame,wybranie 10 pierwszych wierszy, uszeregowanie po TotalCommentCount malejąco
  result<- setDF(head(result[,.(DisplayName,Location,Reputation,CreationDate ,TotalCommentCount)][order(-TotalCommentCount)],10)) 
  
  # zwracanie wyników
  result
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
   # compare(sql_2(Posts, Users), base_2(Posts, Users),allowAll = TRUE)
   # compare(sql_2(Posts, Users), dplyr_2(Posts, Users),allowAll = TRUE)
   # compare(sql_2(Posts, Users), table_2(Posts, Users), allowAll = TRUE)
   # compare(dplyr_2(Posts, Users), table_2(Posts, Users), allowAll = TRUE)
   # compare(base_2(Posts, Users), dplyr_2(Posts, Users),allowAll = TRUE)
   # compare(base_2(Posts, Users), table_2(Posts, Users), allowAll = TRUE)
#Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
 # microbenchmark::microbenchmark(
 #   sqldf = sql_2(Posts, Users),
 #   base = base_2(Posts, Users),
 #   dplyr = dplyr_2(Posts, Users),
 #   data.table = table_2(Posts, Users)
 # )
# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users, Votes){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  #
  # To zapytanie  wybiera najpierw PostId z Votes gdzie VoteTypeId = 12 jako Spam, a potem łaćzy je 
  # na podstawie PostsId z Spam i Id z UsersPosts. UsersPosts to wysyelekcjonowane dane z polaczonej tabeli Posts i Users 
  # gdzie Id w Users jest takie samo jak PostId w Posts. 
  
  # Ostatecznie to zapytanie zwraca kolumny z połaczonej ramki danych stworzonej z
  # połaczonych UsersPosts i Spam, gdzie Id w UsersPosts było takie samo jak PostId w spam
  sqldf::sqldf("SELECT Spam.PostId, UsersPosts.PostTypeId, UsersPosts.Score,
  UsersPosts.OwnerUserId, UsersPosts.DisplayName,
  UsersPosts.Reputation
  FROM (
    SELECT PostId
    FROM Votes
    WHERE VoteTypeId == 12
  ) AS Spam
  JOIN (
    SELECT Posts.Id, Posts.OwnerUserId, Users.DisplayName,
    Users.Reputation, Posts.PostTypeId, Posts.Score
    FROM Posts JOIN Users
    ON Posts.OwnerUserId = Users.Id
  ) AS UsersPosts
  ON Spam.PostId = UsersPosts.Id")
}

base_3 <- function(Posts, Users, Votes){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Wybór wierszy PostId, gdzie VoteTypeId = 12
  spam <- Votes$PostId[Votes$VoteTypeId == 12]
  
  # Łączenie Posts i Users po kolumnach Id z Users i OwnerUserId z Posts
  users_posts <- merge(x = Posts, y = Users, by.x = "OwnerUserId", by.y = "Id")
  
  # selekcja users_posts gdzie Id zawiera sie w spam, wybranie kolumn
  users_posts <- users_posts[users_posts$Id %in% spam, c("Id", "PostTypeId", "Score", "OwnerUserId", "DisplayName", "Reputation") ]
  
  # zmiana nazwy 1 kolumny kolumn
  names(users_posts)<-c("PostId", "PostTypeId", "Score", "OwnerUserId", "DisplayName", "Reputation")
  
  # resetowanie indeksów
  row.names(users_posts)<-NULL
  
  # zamiana wierszy 1 i 2 miejscami
  temp<-users_posts[1,]
  users_posts[1,]<-users_posts[2,]
  users_posts[2,]<-temp
  
  # konwersja na data.frame
  result<- data.frame(users_posts)
  
  # zwracanie wyników
  result
}

dplyr_3 <- function(Posts, Users, Votes){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Wybór wierszy PostId, gdzie VoteTypeId = 12
  spam <- Votes %>%
    filter(VoteTypeId == 12) %>%
    pull(PostId)
  
  # Łączenie Posts i Users po kolumnach Id z Users i OwnerUserId z Posts
  users_posts <- Posts %>%
    left_join(Users, by = c("OwnerUserId" = "Id"))
  
  # selekcja users_posts gdzie Id zawiera sie w spam
  users_posts <- users_posts[users_posts$Id %in% spam, ]
  
  # wybranie kolumn
  users_posts <- users_posts %>%
    select(Id, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)
  
  # zmiana nazwy pierwszej kolumny
  users_posts <- users_posts %>%
    rename(
      PostId = Id)
  
  # resetowanie indeksów
  row.names(users_posts)<-NULL
  
  # wybranie 4 pierwszych wierszy
  result <- users_posts %>%
    slice_head(n = 4)
  
  # zamiana wierszy 1 i 2 miejscami
  result <- users_posts %>%
    slice(c(2, 1,3,4)) %>%
    arrange(row_number())
  
  #konwersja na data.frame
  result<- data.frame(result)
  
  # zwracanie wyników
  result
  
}



table_3 <- function(Posts, Users, Votes){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja Users,Posts i Votes na data.table
  setDT(Posts)
  setDT(Users)
  setDT(Votes)
  
  
  # Filtrowanie i wydobycie PostId
  spam_post_ids <- Votes[VoteTypeId == 12, .(PostId)]
  
  # Konwertowanie Posts i Users na data.table i łączenie ich
  users_posts <- merge(Posts, Users, by.x = "OwnerUserId", by.y = "Id")
  
  # Filtrowanie wierszy zawierających spamowe PostId
  users_posts <- users_posts[Id %in% spam_post_ids$PostId, ]
  
  # Wybieranie interesujących kolumn i zmiana nazw
  result <- users_posts[, .(PostId = Id, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)]
  
  # Konwersja wyniku na data.frame
  setDF(result)
  # zamiana wierszy 1 i 2 miejscami
  temp<-result[1,]
  result[1,]<-result[2,]
  result[2,]<-temp
  
  # zwracanie wyniku
  result
  
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
  # compare(sql_3(Posts, Users, Votes), base_3(Posts, Users, Votes), allowAll = TRUE)
  # compare(sql_3(Posts, Users, Votes), dplyr_3(Posts, Users, Votes),allowAll = TRUE)
  # compare(sql_3(Posts, Users, Votes), table_3(Posts, Users, Votes), allowAll = TRUE)
  # compare(dplyr_3(Posts, Users, Votes), table_3(Posts, Users, Votes), allowAll = TRUE)
  # compare(base_3(Posts, Users, Votes), dplyr_3(Posts, Users, Votes),allowAll = TRUE)
  # compare(base_3(Posts, Users, Votes), table_3(Posts, Users, Votes), allowAll = TRUE)
# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark::microbenchmark(
#   sqldf = sql_3(Posts, Users,Votes),
#   base = base_3(Posts, Users,Votes),
#   dplyr = dplyr_3(Posts, Users,Votes),
#   data.table = table_3(Posts, Users,Votes)
# )
# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # To zapytanie wybiera RelatedPostId z PostLinks, gdzie PostLinks.LinkTypeId = 3, jako dupilcated
  # Łaczy je z posts, gdzie RelatedPostId z Duplicated i Id z posts są takie same
  # z połaczonej ramki danych wybiera z tego RelatedPostId i OwnerUserId i łaczy z Users na podstawie Id z Users i OwnerUserId
  # COUNT(*) AS DuplicatedQuestionsCount - zlicza wszystkie wiersze jako DuplicatedQuestionsCount
  # Następnie jest selekcja kolumn, policzenie DuplicatedQuestionsCount jako liczby wierszy
  # GROUP BY Users.Id - grupuje wszystko wg Id z users
  # Ostatecznie zwracamy wszystkie wiersze gdzie DuplicatedQuestionsCount jest większe od 100, uszeregowane malejąco
  sqldf::sqldf("SELECT Users.Id, Users.DisplayName, Users.UpVotes, Users.DownVotes, Users.Reputation,
COUNT(*) AS DuplicatedQuestionsCount
FROM (
  SELECT Duplicated.RelatedPostId, Posts.OwnerUserId
    FROM (
    SELECT PostLinks.RelatedPostId
    FROM PostLinks
    WHERE PostLinks.LinkTypeId == 3
    ) AS Duplicated
  JOIN Posts
  ON Duplicated.RelatedPostId = Posts.Id
  ) AS DuplicatedPosts
JOIN Users ON Users.Id == DuplicatedPosts.OwnerUserId
GROUP BY Users.Id
HAVING DuplicatedQuestionsCount > 100
ORDER BY DuplicatedQuestionsCount DESC")
}

base_4 <- function(Posts, Users, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # wybranie RelatedPostId z PostLinks, gdzie LinkTypeId = 3
  Duplicated <- PostLinks$RelatedPostId[PostLinks$LinkTypeId == 3]
  
  # Łączenie Posts i Duplicated po kolumnach Id z Posts i RelatedPostId z Posts w Duplicated_Posts
  Duplicated_Posts <- merge(x = data.frame(RelatedPostId = Duplicated), y = Posts, by.x = "RelatedPostId", by.y = "Id")
  
  # Łączenie Duplicated_Posts i Users po kolumnach OwnerUserId z Duplicated_Posts i Id z Users
  result <- merge(x = Duplicated_Posts, y = Users, by.x = "OwnerUserId", by.y = "Id")
  
  # agregacja liczby RelatedPostId dla każdego OwnerUserId
  result_count <- aggregate(RelatedPostId ~ OwnerUserId, data = result, FUN = length)
  
  # zmiana nazwy kolumn
  colnames(result_count) <- c("OwnerUserId", "DuplicatedQuestionsCount")
  
  # Filtrowanie wyników, aby wybrać tylko tych użytkowników, którzy mają więcej niż 100 DuplicatedQuestionsCount
  filtered_result <- result_count[result_count$DuplicatedQuestionsCount > 100, ]
  
  # Wybieranie odpowiednich kolumn i wierszy z ramki danych Users
  final_result <- Users[Users$Id %in% filtered_result$OwnerUserId, c("Id","DisplayName", "UpVotes", "DownVotes", "Reputation")]
  
  #Przyporzadkowanie ostatecznemu rezultatowi pofiltrowane dane
  final_result$DuplicatedQuestionsCount<- filtered_result$DuplicatedQuestionsCount
  
  # Sortowanie malejąco po DuplicatedQuestionsCount
  final_result<-final_result[order(final_result$DuplicatedQuestionsCount, decreasing = TRUE), ]
  
  # resetowanie indeksów
  row.names(final_result)<-NULL
  
  # Konwersja na data.frame
  final_result<-data.frame(final_result)
  
  # Zwrócenie wyników
  final_result
  
  
  
  
  
}

dplyr_4 <- function(Posts, Users, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # wybranie RelatedPostId z PostLinks, gdzie LinkTypeId = 3
  Duplicated <- PostLinks %>%
    filter(LinkTypeId == 3) %>%
    pull(RelatedPostId)
  
  # Łączenie Posts i Duplicated po kolumnach Id z Posts i RelatedPostId z Posts w Duplicated_Posts
  Duplicated_Posts<-left_join(data.frame(RelatedPostId = Duplicated), y= Posts , by = c("RelatedPostId" = "Id"))
  
  # Zliczanie liczby zduplikowanych pytań każdego użytkownika i filtrowanie wyników, aby wybrać tylko tych użytkowników, którzy mają więcej niż 100 DuplicatedQuestionsCount
  result_count <- Duplicated_Posts %>%
    group_by(OwnerUserId) %>%
    summarize(DuplicatedQuestionsCount = n()) %>%
    filter(DuplicatedQuestionsCount > 100)
  
  # Wybieranie odpowiednich kolumn i wierszy, gdzie Id jest w result_count$OwnerUserId oraz sortowanie malejąco po DuplicatedQuestionsCount
  final_result <- Users %>%
    filter(Id %in% result_count$OwnerUserId) %>%
    select(Id, DisplayName, UpVotes, DownVotes, Reputation) %>%
    left_join(result_count, by = c("Id" = "OwnerUserId")) %>%
    arrange(desc(DuplicatedQuestionsCount))
    
  
  # Zwrócenie wyników
  final_result
}

table_4 <- function(Posts, Users, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # Konwersja Users,Posts i PostLinks na data.table
  setDT(Posts)
  setDT(Users)
  setDT(PostLinks)
  
  # Wybór zduplikowanych postów na podstawie warunku LinkTypeId == 3
  Duplicated <- PostLinks[LinkTypeId == 3, .(RelatedPostId)]
  
  # Łączenie Posts i Duplicated po kolumnach Id z Posts i RelatedPostId z Posts w Duplicated_Posts
  Duplicated_Posts <- merge(Duplicated, Posts, by.x = "RelatedPostId", by.y = "Id", all.x = TRUE)
  
  # Łączenie Duplicated_Posts i Users po kolumnach OwnerUserId z Duplicated_Posts i Id z Users
  result <- merge(Duplicated_Posts, Users, by.x = "OwnerUserId", by.y = "Id", all.x = TRUE)
  
  # Zliczanie liczby zduplikowanych pytań każdego użytkownika
  result_count <- result[, .(DuplicatedQuestionsCount = .N), by = OwnerUserId]
  
  # Filtrowanie wyników, aby wybrać tylko tych użytkowników, którzy mają więcej niż 100 zduplikowanych pytań
  filtered_result <- result_count[DuplicatedQuestionsCount > 100]
  
  # Wybieranie odpowiednich kolumn i wierszy z ramki danych Users
  final_result<- Users[Id %in% filtered_result$OwnerUserId, .(Id, DisplayName, UpVotes, DownVotes, Reputation)]
  
  # usuwanie danych gdzie OwnerUserId to NA
  filtered_result <- filtered_result[!is.na(OwnerUserId)]
  
  #Przyporzadkowanie ostatecznemu rezultatowi pofiltrowane dane
  final_result[, DuplicatedQuestionsCount:= filtered_result$DuplicatedQuestionsCount]
 
  #sortowanie danych malejąco po DuplicatedQuestionsCount
  setorder(final_result, -DuplicatedQuestionsCount)
  
  # Konwersja na data.frame
  setDF(final_result)
  
  # Zwrócenie wyników
  final_result
 
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
  # compare(sql_4(Posts, Users, PostLinks), base_4(Posts, Users, PostLinks), allowAll = TRUE)
  # compare(sql_4(Posts, Users, PostLinks), dplyr_4(Posts, Users, PostLinks),allowAll = TRUE)
  # compare(sql_4(Posts, Users, PostLinks), table_4(Posts, Users, PostLinks), allowAll = TRUE)
  # compare(dplyr_4(Posts, Users, PostLinks), table_4(Posts, Users, PostLinks), allowAll = TRUE)
  # compare(base_4(Posts, Users, PostLinks), dplyr_4(Posts, Users, PostLinks),allowAll = TRUE)
  # compare(base_4(Posts, Users, PostLinks), table_4(Posts, Users, PostLinks), allowAll = TRUE)


# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark::microbenchmark(
#   sqldf = sql_4(Posts, Users,PostLinks),
#   base = base_4(Posts, Users,PostLinks),
#   dplyr = dplyr_4(Posts, Users,PostLinks),
#   data.table = table_4(Posts, Users,PostLinks)
# )

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # W tym kodzie zaczynami od wybrania Id,Title, Score i odpowiednio wybrania godziny z Daty jako Hour z Posts, gdzie PostTypeId jest równe 1 lub 2. 
  # Te dane nazywamy QuestionsAnswers. Następnie wybieramy RelatedPostId, PostId z PostLinks gdzie LinkTypeId = 3 jako PL3. 
  # PL3 łaczymy z Posts gdzie PostId z PL3  jest równe Id z Posts i dane te nazwyamy Duplicated 
  # Następnie jest łaczenie duplicated i QuestionsAnswers za pomocą Id i RelatedPostId
  # Ostateczna selekcja odpowiednich kolumn 
  # MAX(Duplicated.Score) AS MaxScoreDuplicated - utworzenie Kolumny z maksymalnym Score z duplicated
  # Stworzenie kolumny DulicatesCount z policzonymi wierszami
  # CASE
  #WHEN QuestionsAnswers.Hour < '06' THEN 'Night'
  #WHEN QuestionsAnswers.Hour < '12' THEN 'Morning'
  #WHEN QuestionsAnswers.Hour < '18' THEN 'Day'
  #ELSE 'Evening'
  #END DayTime- stworzenie kolumny DayTime gdzie określenie pory dnia opiera się na wartosci w kolumnie Hour w tym samym wierszu
  # wszystko zostaje pogrupowane po Id z QuestionsAnswers i ułozone po  DulicatesCount malejąco
  
  sqldf::sqldf("SELECT QuestionsAnswers.Id,
               QuestionsAnswers.Title,
               QuestionsAnswers.Score,
               MAX(Duplicated.Score) AS MaxScoreDuplicated,
               COUNT(*) AS DulicatesCount,
               CASE
               WHEN QuestionsAnswers.Hour < '06' THEN 'Night'
               WHEN QuestionsAnswers.Hour < '12' THEN 'Morning'
               WHEN QuestionsAnswers.Hour < '18' THEN 'Day'
               ELSE 'Evening'
               END DayTime
               FROM (
                 SELECT Id, Title,
                 STRFTIME('%H', CreationDate) AS Hour, Score
                 FROM Posts
                 WHERE Posts.PostTypeId IN (1, 2)
               ) AS QuestionsAnswers
               JOIN (
                 SELECT PL3.RelatedPostId, Posts.Score
                 FROM (
                   SELECT RelatedPostId, PostId
                   FROM PostLinks
                   WHERE LinkTypeId == 3
                 ) AS PL3
                 JOIN Posts ON PL3.PostId = Posts.Id
               ) AS Duplicated
               ON QuestionsAnswers.Id = Duplicated.RelatedPostId
               GROUP BY QuestionsAnswers.Id
               ORDER By DulicatesCount DESC")
}

base_5 <- function(Posts, PostLinks){
  
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # selekcja z Posts, gdzie PostTypeId jest równe 1 lub 2
  QuestionsAnswers <- Posts[PostTypeId %in% c(1,2)]
  
  #stworzenie kolumny godziny
  QuestionsAnswers$Hour <- substr(QuestionsAnswers$CreationDate, start = 12, stop = 13)
  
  #selekcja kolumn
  QuestionsAnswers<-QuestionsAnswers[,c("Id","Title","Hour","Score")]
  
  # selekcja kolumn z PostLinks i wierszy gdzie LinkTypeId=3
  PL3<-PostLinks[PostLinks$LinkTypeId==3,c("RelatedPostId","PostId")]
  
  #Łaczenie Pl3 i Posts za pomocą PostId i Id
  Duplicated<-merge(x=PL3, y=Posts, by.x="PostId", by.y="Id")
  
  #selekcja kolumn
  Duplicated<- Duplicated[,c("RelatedPostId","Score")]
  
  # łaczenie duplicated i QuestionsAnswers za pomocą Id i RelatedPostId
  result<-merge(x=QuestionsAnswers,y=Duplicated, by.x="Id", by.y="RelatedPostId")
  
  #konwersja na data.frame
  result<- data.frame(result)
  
  #Agregacja wartości maksymalnych Score.y dla każdego Id
  MaxScoreDuplicated<-aggregate(result["Score.y"], result["Id"], max)
  
  # Agregacja liczby wystąpień dla każdego Id
  DelicatesCount<-aggregate(result["Id"],result["Id"],length)
  
  # zmiana nazwy kolumn
  names(DelicatesCount)[2] <- "DelicatesCount"
  
  # Łączenie, a de facto dodawanie kolumn z MaxScoreDuplicated i DelicatesCount
  result<-merge(result, MaxScoreDuplicated, by="Id")
  result<-merge(result, DelicatesCount, by="Id")
  
  # zmiana typu godziny by mozna było nastepne operacje przeprowadzać
  result$Hour<-as.integer(result$Hour)
  
  # tworzenie kolumny dnia poprzez podziałki
  result$DayTime <- cut(result$Hour, breaks = c(0, 6, 12, 18, 24), labels = c("Night", "Morning", "Day", "Evening"), right = FALSE)
  
  # szeregowanie kolumn w odpowiedniej kolejnosci
  result<-result[c(1,2,4,6,7,8)]
  
  # dopasowanie nazw kolumn
  colnames(result)<- c("Id","Title","Score","MaxScoreDuplicated","DelicatesCount", "DayTime")
  
  # uszeregowano wynik od najwyzszego DelicatesCount
  result<-result[order(result$DelicatesCount, decreasing = TRUE),]
  
  # usuniecie powtórzeń
  result<- unique(result)
  
  # resetowanie indeksów
  rownames(result)<-NULL
  
  # zwracanie wyniku
  result
}
  
  

  


dplyr_5 <- function(Posts, PostLinks){
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  
  QuestionsAnswers <- Posts %>%
    # selekcja z Posts, gdzie PostTypeId jest równe 1 lub 2
    filter(PostTypeId %in% c(1, 2)) %>%
    
    #stworzenie kolumny godziny
    mutate(Hour = as.integer(substr(CreationDate, start = 12, stop = 13))) %>%
    
    #selekcja kolumn
    select(Id, Title, Hour, Score)
  
  Duplicated <- PostLinks %>%
    # selekcja  wierszy gdzie LinkTypeId=3
    filter(LinkTypeId == 3) %>%
    
    # selekcja kolumn z PostLinks
    select(RelatedPostId, PostId) %>%
    
    #Łaczenie Pl3 i Posts za pomocą PostId i Id
    inner_join(Posts, by = c("PostId" = "Id")) %>%
    
    # selekcja kolumn
    select(RelatedPostId, Score)
  
  result <- QuestionsAnswers %>%
    # łaczenie duplicated i QuestionsAnswers za pomocą Id i RelatedPostId
    inner_join(Duplicated, by = c("Id" = "RelatedPostId")) %>% 
    #  szeregowanie kolumn w odpowiedniej kolejnosci
    group_by(Id, Title, Score.x) %>%
    # Liczenie MaxScoreDuplicated, DelicatesCount i tworzenie kolumny dnia
    summarise(MaxScoreDuplicated = max(Score.y, na.rm = TRUE),
              DelicatesCount = n(),
              DayTime = case_when(
                Hour < 6 ~ "Night",
                Hour < 12 ~ "Morning",
                Hour < 18 ~ "Day",
                TRUE ~ "Evening"
              )) %>%
    # posortowanie według malejącego DelicatesCount
    arrange(desc(DelicatesCount))
  # usunięcie kopii
  result<- unique(result)
  # zmiana nazwy wiersza
  result <- result %>%
    rename(
      Score = Score.x)
  #konwersja na data.frame
  result<- data.frame(result)
  # zwracanie wyników
  result
  
  
  
}

table_5 <- function(Posts, PostLinks){
  
    # Tu umiesc rozwiazanie oraz komentarze
    # 
  # konwersja Posts i PostLinks na data.table
  setDT(Posts)
  setDT(PostLinks)
  
  #selekcja wierszy z Posts, gdzie PostTypeId jest równe 1 lub 2, selekcja kolumn i stworzenie kolumny z godziną
  QuestionsAnswers <- Posts[PostTypeId %in% c(1,2) , .(Id, Title, Hour = as.integer(substr(CreationDate, start = 12, stop = 13)), Score)]
  
  # selekcja kolumn z PostLinks i wierszy gdzie LinkTypeId=3
  Pl3 <- PostLinks[LinkTypeId==3, .(RelatedPostId, PostId)]
  
  #Łaczenie Pl3 i Posts za pomocą PostId i Id
  Duplicated <- merge(Pl3, Posts, by.x = "PostId", by.y = "Id")
  
  # stworzenie kolumn MaxScoreDuplicated i DulicatesCount
  Duplicated <- Duplicated[ , .(MaxScoreDuplicated = max(Score, na.rm=TRUE), DulicatesCount = .N), by= RelatedPostId]
  
  # połączenie QuestionsAnswers i Duplicated przez kolumny Id i RelatedPostId
  result <- merge(QuestionsAnswers, Duplicated, by.x = "Id", by.y = "RelatedPostId")
  
  # sworzenie kolumny dnia
  result <- result[ , DayTime := ifelse(Hour < 6, "Night", ifelse(Hour < 12, "Morning", ifelse(Hour < 18, "Day", "Evening"))) ]
  # wybór kolumn i sortowanie po DulicatesCount malejąco  
  result <- result[order(DulicatesCount, decreasing = TRUE), .(Id, Title, Score, MaxScoreDuplicated, DulicatesCount, DayTime)]
  
  #konwersja na data.frame
  setDF(result)
  # zwracanie wyników
  result
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
   # compare(sql_5(Posts,PostLinks), base_5(Posts,PostLinks), allowAll = TRUE)
   # compare(sql_5(Posts, PostLinks), dplyr_5(Posts, PostLinks),allowAll = TRUE)
   # compare(sql_5(Posts, PostLinks), table_5(Posts, PostLinks), allowAll = TRUE)
   # compare(dplyr_5(Posts,PostLinks), table_5(Posts,PostLinks), allowAll = TRUE)
   # compare(base_5(Posts, PostLinks), dplyr_5(Posts, PostLinks),allowAll = TRUE)
   # compare(base_5(Posts, PostLinks), table_5(Posts, PostLinks), allowAll = TRUE)

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark::microbenchmark(
#   sqldf = sql_5(Posts, PostLinks),
#   base = base_5(Posts, PostLinks),
#   dplyr = dplyr_5(Posts, PostLinks),
#   data.table = table_5(Posts, PostLinks)
# )



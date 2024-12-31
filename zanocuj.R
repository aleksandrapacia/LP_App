# Tworzenie przykładowego zestawu danych "Zanocuj w lesie"
library(tibble)
library(dplyr)

set.seed(42)  # Losowość danych

# Generowanie przykładowych nazw RDLP i NADL
rdlp <- c("BIAŁYSTOK", "GDAŃSK", "KRAKÓW", "KATOWICE", "SZCZECINEK", "PIŁA", "WROCŁAW")
nadl <- c("EŁK", "NURZEC", "OLECKO", "BRYNEK", "STRZEBIELINO", "KACZORY", "MYŚLENICE", "LIMANOWA")

# Generowanie przykładowych współrzędnych (randomizacja w obrębie Polski)
data <- tibble(
  id = 1:100,  # Unikalny identyfikator
  rdlp = sample(rdlp, 100, replace = TRUE),
  nadl = sample(nadl, 100, replace = TRUE),
  link = paste0("https://zanocujwlesie.example.gov.pl/obiekt", 1:100),
  longitude = runif(100, 14.0, 24.5),  # Zakres długości geograficznej Polski
  latitude = runif(100, 49.0, 54.5)     # Zakres szerokości geograficznej Polski
)

# Podgląd danych
print(data)

# Zapis danych do pliku CSV
write.csv(data, "zanocuj_w_lesie.csv", row.names = FALSE)
file.exists("C://Users//User//OneDrive//Pulpit//LP_App//gatunki.csv")


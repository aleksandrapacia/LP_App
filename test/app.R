library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(DT)

# Wczytanie danych z Google Sheets
sheet_url <- "https://docs.google.com/spreadsheets/d/1kYGd3G8jdrQ7wvgmBxBU3_iUSepDx50Mz5KvVKmZ6HM/edit?gid=0"
data <- read_sheet(sheet_url)

# Przekształcenie danych (przykładowe przekształcenie - dostosuj w zależności od struktury danych)
data_long <- data %>%
  pivot_longer(cols = starts_with("gatunki"),
               names_to = "region",
               values_to = "liczba") %>%
  mutate(region = ifelse(grepl("Małopolska", region), "Małopolska", "Polska"))

# UI aplikacji Shiny
ui <- fluidPage(
  titlePanel("Zestawienie Gatunków w Małopolsce i Polsce"),
  fluidRow(
    column(12, DTOutput("table"))
  ),
  fluidRow(
    column(6, plotOutput("plot_malopolska")),
    column(6, plotOutput("plot_polska"))
  )
)

# Serwer aplikacji Shiny
server <- function(input, output) {
  
  # Tabela z liczbą gatunków
  output$table <- renderDT({
    data %>%
      select(gatunki, Małopolska, Polska) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Wykres dla Małopolski
  output$plot_malopolska <- renderPlot({
    data_malopolska <- data_long %>% filter(region == "Małopolska")
    ggplot(data_malopolska, aes(x = gatunki, y = liczba)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Udział Gatunków w Małopolsce", x = "Gatunki", y = "Liczba")
  })
  
  # Wykres dla Polski
  output$plot_polska <- renderPlot({
    data_polska <- data_long %>% filter(region == "Polska")
    ggplot(data_polska, aes(x = gatunki, y = liczba)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Udział Gatunków w Polsce", x = "Gatunki", y = "Liczba")
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)
Y
library(shiny)
library(leaflet)
library(tibble)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(readr)
getwd()
# Tworzenie przykładowego zestawu danych "Zanocuj w lesie"
set.seed(42)  # Losowość danych

# Generowanie przykładowych nazw RDLP i NADL
rdlp <- c("Punkt 1", "Punkt 3", "Punkt 4", "Punkt 5", "Punkt 6", "Punkt 7", "Punkt 8")
nadl <- c("t", "n", "t", "n", "n", "t", "n", "t")
ochrona_przyrody <- read_csv("C://Users//User//OneDrive//Pulpit//LP_App//voivodeships_data.csv")
# Generowanie przykładowych współrzędnych (randomizacja w obrębie Polski)
data <- tibble(
  id = 1:100,  # Unikalny identyfikator
  rdlp = sample(rdlp, 100, replace = TRUE),
  nadl = sample(nadl, 100, replace = TRUE),
  link = paste0("https://zanocujwlesie.example.gov.pl/obiekt", 1:100),
  longitude = runif(100, 14.0, 24.5),  # Zakres długości geograficznej Polski
  latitude = runif(100, 49.0, 53.5)     # Zakres szerokości geograficznej Polski
)

# Dane do wykresów pozyskania drewna
harvest_data <- tibble(
  rodzaj_drewna = rep(c("tartaczne", "kopalniakowe", "papierówka sosnowa", "papierówka świerkowo-jodłowa", "drewno opałowe iglaste"), each = 5),
  rok = rep(2017:2021, times = 5),
  ilosc = c(13962.3, 242.3, 2279.9, 8846.0, 3615.9, 13899.2, 185.8, 2389.5, 9121.7, 3345.5, 
            14581.3, 169.9, 2224.4, 1327.9, 5002.1, 14713.6, 115.4, 2203.6, 1088.3, 4569.7,
            14711.3, 147.1, 2203.6, 1084.3, 4589.7)
)

# Wczytywanie danych z arkusza Google (dane dotyczące gatunków drzew)
# Załóżmy, że plik CSV został pobrany i zapisany lokalnie jako "gatunki.csv"
gatunki_drzew <- read_csv("C://Users//User//OneDrive//Pulpit//LP_App//gatunki.csv", locale = locale(encoding = "UTF-8"))

# Tworzenie aplikacji Shiny
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Lasy"),
  
  navbarPage("",
             tabPanel("Mapa",
                      leafletOutput("map", height = 600)
             ),
             tabPanel("Wykresy pozyskania",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("rodzaj_drewna", "Wybierz rodzaj drewna:", 
                                      choices = unique(harvest_data$rodzaj_drewna))
                        ),
                        mainPanel(
                          plotOutput("harvest_plot")
                        )
                      )
             ),
             tabPanel("Zestawienie gatunków",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region", "Wybierz region:", 
                                      choices = c("Cała Polska", "Małopolska")),
                          selectInput("bar_color", "Wybierz kolor wykresu:", 
                                      choices = c("Set1", "Set2", "Set3", "Paired", "Dark2", "Accent"),
                                      selected = "Set3"),
                        ),
                        mainPanel(
                          fluidRow(
                            column(6, tableOutput("gatunki_table")),
                            column(6, plotOutput("gatunki_barplot"))
                          )
                        )
                      )
             ),

             tabPanel("Tabela i Wykresy",
                      verbatimTextOutput("summary_1"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region", "Wybierz region:", 
                                      choices = c("Polska", unique(ochrona_przyrody$wojewodztwa))),
                          actionButton("update", "Aktualizuj"),
                          
                        ),
                        
                        
                        
                        mainPanel(
                          tableOutput("protection_table"),
                          plotOutput("bar_plot")
                        )
                      )
             )
  )
)


server <- function(input, output, session) {
  # Mapa
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, 
                       popup = ~paste("<b>Nazwa:</b>", rdlp, "<br><b>Dostepne(t/n):</b>", nadl, 
                                      "<br><a href='", link, "' target='_blank'>Link</a>"),
                       radius = 5, color = "blue", fillOpacity = 0.7)
  })
  
  # Wykres pozyskania drewna
  output$harvest_plot <- renderPlot({
    filtered_data <- harvest_data %>% filter(rodzaj_drewna == input$rodzaj_drewna)
    ggplot(filtered_data, aes(x = rok, y = ilosc)) +
      geom_line(color = "forestgreen", size = 1) +
      geom_point(color = "darkgreen", size = 3) +
      labs(title = paste("Pozyskanie dla:", input$rodzaj_drewna),
           x = "Rok", y = "Ilość [tys. m3]") +
      theme_minimal()
  })
  
  # Kolory słupków
  bar_colors <- reactiveVal("steelblue")  # Domyślny kolor
  
  observeEvent(input$change_color, {
    new_color <- sample(colors(), 1)  # Wybiera losowy kolor
    bar_colors(new_color)
  })
  
  # Zestawienie gatunków drzew
  output$gatunki_table <- renderTable({
    if (input$region == "Cała Polska") {
      gatunki_drzew %>%
        select(gatunki, PL_procent)
    } else {
      gatunki_drzew %>%
        select(gatunki, Malopolska_procent)
    }
  })
  
  # Wykres słupkowy gatunków drzew
  output$gatunki_barplot <- renderPlot({
    if (input$region == "Cała Polska") {
      plot_data <- gatunki_drzew %>% 
        select(gatunki, PL_procent) %>% 
        rename(procent = PL_procent)
    } else {
      plot_data <- gatunki_drzew %>% 
        select(gatunki, Malopolska_procent) %>% 
        rename(procent = Malopolska_procent)
    }
    
    ggplot(plot_data, aes(x = reorder(gatunki, -procent), y = procent, fill = gatunki)) +
      geom_bar(stat = "identity", color = "black") +
      coord_flip() +
      labs(title = paste("Udział gatunków drzew w regionie:", input$region), 
           x = "Gatunki", y = "Procent [%]") +
      theme_minimal() +
      scale_fill_brewer(palette = input$bar_color)
  })

  # Funkcja obliczająca dane dla tabeli i wykresu
  calculate_data <- reactive({
    region <- input$region
    
    if (region == "Polska (puste)") {
      summary_data <- ochrona_przyrody %>%
        summarise(
          parki_narod_ha = sum(parki_narod_ha),
          rezerwaty_przyr_ha = sum(rezerwaty_przyr_ha),
          parki_krajob_ha = sum(parki_krajob_ha),
          pow_calk_ha = sum(pow_calk_ha)
        )
    } else {
      summary_data <- ochrona_przyrody %>%
        filter(wojewodztwa == region) %>%
        summarise(
          parki_narod_ha = sum(parki_narod_ha),
          rezerwaty_przyr_ha = sum(rezerwaty_przyr_ha),
          parki_krajob_ha = sum(parki_krajob_ha),
          pow_calk_ha = sum(pow_calk_ha)
        )
    }
    
    summary_data <- summary_data %>%
      mutate(
        procent_parki_narod = round(parki_narod_ha / pow_calk_ha * 100, 2),
        procent_rezerwaty = round(rezerwaty_przyr_ha / pow_calk_ha * 100, 2),
        procent_parki_krajob = round(parki_krajob_ha / pow_calk_ha * 100, 2)
      )
    
    summary_data
  })
  
  # Wyświetlenie tabeli
  output$protection_table <- renderTable({
    summary_data <- calculate_data()
    
    tibble(
      "Forma ochrony" = c("Parki narodowe", "Rezerwaty przyrody", "Parki krajobrazowe"),
      "Powierzchnia (ha)" = c(
        summary_data$parki_narod_ha,
        summary_data$rezerwaty_przyr_ha,
        summary_data$parki_krajob_ha
      ),
      "Udział w regionie (%)" = c(
        summary_data$procent_parki_narod,
        summary_data$procent_rezerwaty,
        summary_data$procent_parki_krajob
      )
    )
  })
  
  # Wyświetlenie wykresu słupkowego
  output$bar_plot <- renderPlot({
    summary_data <- calculate_data()
    
    plot_data <- tibble(
      kategoria = c("Parki narodowe", "Rezerwaty przyrody", "Parki krajobrazowe"),
      powierzchnia_ha = c(
        summary_data$parki_narod_ha,
        summary_data$rezerwaty_przyr_ha,
        summary_data$parki_krajob_ha
      ),
      procent_powierzchni = c(
        summary_data$procent_parki_narod,
        summary_data$procent_rezerwaty,
        summary_data$procent_parki_krajob
      )
    )
    
    ggplot(plot_data, aes(x = kategoria, y = powierzchnia_ha, fill = kategoria)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = paste0(round(procent_powierzchni, 1), "%")), 
                vjust = -0.5, size = 5) +
      labs(title = paste("Formy ochrony przyrody w regionie:", input$region),
           x = "Forma ochrony", y = "Powierzchnia (ha)") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  })
}
  
 


shinyApp(ui, server)                                                                                                                                   
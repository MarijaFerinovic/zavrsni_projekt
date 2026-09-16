library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(httr)
library(readxl)
library(jsonlite)
library(rjstat)

svipodaci <- read_csv("data/svi_podaci.csv", show_col_types = FALSE)
inflacija <- read_csv("data/inflacija.csv", show_col_types = FALSE)

azurirajracune <- function() {
  najnovijiubazi <- max(svipodaci$DATUM, na.rm = TRUE)
  godina <- as.integer(format(najnovijiubazi, "%Y"))
  mjesec <- as.integer(format(najnovijiubazi, "%m"))
  prviprolaz <- TRUE
  imanovih <- FALSE
  
  repeat {
    
    if (prviprolaz) {
      prviprolaz <- FALSE
    } else {
      mjesec <- mjesec + 1
      if (mjesec > 12) {
        mjesec <- 1
        godina <- godina + 1
      }
    }
    
    if (as.Date(sprintf("%d-%02d-01", godina, mjesec)) > Sys.Date()) {
      break
    }
    
    url <- sprintf("https://porezna.gov.hr/fiskalizacija/izvjestaji/api/download/%d.%d_mjesecno_izvjesce_sva_podrucja_djel", godina, mjesec)
    putanja <- sprintf("data/mjesecno_%d_%d.xlsx", godina, mjesec)
    
    listovi <- tryCatch({
      GET(url, write_disk(putanja, overwrite = TRUE), timeout(10))
      excel_sheets(putanja)
    }, error = function(cond) { return(NULL) })
    
    if (is.null(listovi) || !("Izvješće broj računa" %in% listovi)) {
      if (file.exists(putanja)) {
        file.remove(putanja)
      }
      break
    }
    
    novi <- tryCatch({
      brojevi <- read_excel(putanja, sheet = "Izvješće broj računa")
      brojevi <- filter(brojevi, !is.na(DATUM))
      brojevi <- select(brojevi, DATUM, DAN, DJELATNOST, ŽUPANIJA, BROJ_RACUNA = `BROJ RAČUNA (SVI NAČINI PLAĆANJA)`)
      
      iznosi <- read_excel(putanja, sheet = "Izvješće iznos računa")
      iznosi <- filter(iznosi, !is.na(DATUM))
      iznosi <- select(iznosi, DATUM, DAN, DJELATNOST, ŽUPANIJA, IZNOS_RACUNA = `IZNOS RAČUNA (SVI NAČINI PLAĆANJA)`)
      
      spojeno <- inner_join(brojevi, iznosi, by = c("DATUM", "DAN", "DJELATNOST", "ŽUPANIJA"))
      spojeno <- mutate(spojeno, DATUM = as.Date(DATUM, format = "%d.%m.%Y"))
      spojeno
    }, error = function(cond) { return(NULL) })
    
    if (is.null(novi)) {
      break
    }
    
    oznakamjeseca <- sprintf("%d-%02d", godina, mjesec)
    staribroj <- nrow(filter(svipodaci, format(DATUM, "%Y-%m") == oznakamjeseca))
    
    if (nrow(novi) > staribroj) {
      svipodaci <<- filter(svipodaci, format(DATUM, "%Y-%m") != oznakamjeseca)
      svipodaci <<- bind_rows(svipodaci, novi)
      svipodaci <<- arrange(svipodaci, DATUM)
      imanovih <- TRUE
    }
  }
  
  if (imanovih) {
    write_csv(svipodaci, "data/svi_podaci.csv")
  }
}

azurirajracune()

azurirajinflaciju <- function() {
  sortirano <- arrange(inflacija, desc(Godina), desc(MjesecBr))
  najnovijiinfl <- slice(sortirano, 1)
  
  url <- "https://web.dzs.hr/PxWeb/api/v1/hr/Cijene/Indeksi potrošačkih cijena/Indeksi potrošačkih cijena – ECOICOP, ver. 2/ME_PS09.px"
  urlkodiran <- URLencode(url)
  
  upit <- list(
    query = list(
      list(code = "ECOICOP, ver. 2", selection = list(filter = "item", values = list("00"))),
      list(code = "Indikatori", selection = list(filter = "item", values = list("1", "4")))
    ),
    response = list(format = "json-stat2"))
  
  novatablica <- tryCatch({
    odgovor <- POST(urlkodiran, body = upit, encode = "json", timeout(15))
    sadrzaj <- content(odgovor, as = "text", encoding = "UTF-8")
    tablica <- fromJSONstat(sadrzaj)
    
    tablica <- mutate(tablica, Godina = as.integer(substr(Mjesec, 1, 4)), MjesecBr = as.integer(substr(Mjesec, 6, 7)))
    
    stope <- filter(tablica, grepl("godišnje stope", Indikatori))
    stope <- select(stope, Godina, MjesecBr, stopainflacije = value)
    
    indeksi <- filter(tablica, grepl("indeksi", Indikatori))
    indeksi <- select(indeksi, Godina, MjesecBr, indekscijena = value)
    
    spojeno <- full_join(stope, indeksi, by = c("Godina", "MjesecBr"))
    spojeno <- filter(spojeno, !is.na(stopainflacije) | !is.na(indekscijena))
    spojeno
  }, error = function(cond) { return(NULL) })
  
  if (is.null(novatablica)) {
    return(invisible(NULL))
  }
  
  sortiranonovo <- arrange(novatablica, desc(Godina), desc(MjesecBr))
  najnovijinovi <- slice(sortiranonovo, 1)
  
  imanovije <- najnovijinovi$Godina > najnovijiinfl$Godina || (najnovijinovi$Godina == najnovijiinfl$Godina && najnovijinovi$MjesecBr > najnovijiinfl$MjesecBr)
  
  if (imanovije) {
    inflacija <<- novatablica
    write_csv(inflacija, "data/inflacija.csv")
  }
}

azurirajinflaciju()

svezupanije <- sort(unique(svipodaci$ŽUPANIJA))
svedjelatnosti <- sort(unique(svipodaci$DJELATNOST))

redoslijeddana <- c("Ponedjeljak", "Utorak", "Srijeda", "Četvrtak", "Petak", "Subota", "Nedjelja")

ui <- page_sidebar(
  title = "Fiskalizirani računi u Hrvatskoj (2018 - danas)",
  
  sidebar = sidebar(
    dateRangeInput("raspon", "Odaberi razdoblje:",
                   start = min(svipodaci$DATUM), end = max(svipodaci$DATUM),
                   min = min(svipodaci$DATUM), max = max(svipodaci$DATUM),
                   language = "hr", weekstart = 1),
    
    selectInput("zupanija", "Odaberi županiju:",
                choices = c("Sve županije", svezupanije), selected = "Sve županije"),
    
    selectInput("djelatnost", "Odaberi djelatnost:",
                choices = c("Sve djelatnosti", svedjelatnosti), selected = "Sve djelatnosti"),
    
    radioButtons("metrika", "Prikaži:",
                 choices = c("Broj računa" = "broj", "Iznos računa" = "iznos"), selected = "broj"),
    
    hr(),
    
    helpText("Od 1. siječnja 2026. postupak fiskalizacije se primjenjuje na sve obveznike fiskalizacije u krajnjoj potrošnji neovisno o načinu plaćanja. Ranijih godina postupak fiskalizacije je bio neobavezan za transakcijska plaćanja, stoga podaci iz 2026. godine nisu u cijelosti usporedivi s prethodnim godinama.")
  ),
  
  tabsetPanel(
    tabPanel("Vremenska razdoblja", plotOutput("grafdana")),
    tabPanel("Usporedba po danima",
             plotOutput("grafdani"),
             br(),
             wellPanel(textOutput("tekstnedjelja"))),
    tabPanel("Usporedba po mjesecima", plotOutput("grafmjeseci")),
    tabPanel("Usporedba po županijama", plotOutput("grafzupanije")),
    tabPanel("Utjecaj neradnih nedjelja",
             plotOutput("grafzabrana", height = "400px"),
             br(),
             wellPanel(textOutput("tekstzabrana")),
             br(),
             plotOutput("grafnedjelje")),
    tabPanel("Utjecaj inflacije",
             plotOutput("grafrealni", height = "350px"),
             plotOutput("grafstopa", height = "250px"),
             br(),
             wellPanel(textOutput("tekstinflacija"))),
    tabPanel("Predviđanje",
             plotOutput("grafpredvidjanje", height = "400px"),
             br(),
             wellPanel(textOutput("tekstpredvidjanje")))
  )
)

server <- function(input, output) {
  
  nazivmetrike <- reactive({
    if (input$metrika == "broj") "Broj računa" else "Iznos računa (EUR)"
  })
  
  filtrirano <- reactive({
    tablica <- filter(svipodaci, DATUM >= input$raspon[1], DATUM <= input$raspon[2])
    
    if (input$zupanija != "Sve županije") {
      tablica <- filter(tablica, ŽUPANIJA == input$zupanija)
    }
    if (input$djelatnost != "Sve djelatnosti") {
      tablica <- filter(tablica, DJELATNOST == input$djelatnost)
    }
    
    if (input$metrika == "broj") {
      tablica <- mutate(tablica, VRIJEDNOST = BROJ_RACUNA)
    } else {
      tablica <- mutate(tablica, VRIJEDNOST = IZNOS_RACUNA)
    }
    tablica
  })
  
  podanu <- reactive({
    tablica <- filtrirano()
    tablica <- group_by(tablica, DATUM)
    tablica <- summarise(tablica, ukupnoracuna = sum(VRIJEDNOST, na.rm = TRUE))
    tablica
  })
  
  output$grafdana <- renderPlot({
    tablica <- podanu()
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema podataka za odabranu kombinaciju.")
    )
    
    ggplot(tablica, aes(x = DATUM, y = ukupnoracuna)) +
      geom_line(color = "blue", linewidth = 0.6) +
      labs(x = "Datum", y = nazivmetrike(), title = sprintf("%s po danu", nazivmetrike())) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal()
  })
  
  podanima <- reactive({
    tablica <- filtrirano()
    tablica <- group_by(tablica, DAN)
    tablica <- summarise(tablica, prosjekracuna = mean(VRIJEDNOST, na.rm = TRUE))
    tablica <- mutate(tablica, DAN = factor(DAN, levels = redoslijeddana))
    tablica <- arrange(tablica, DAN)
    tablica
  })
  
  output$grafdani <- renderPlot({
    tablica <- podanima()
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema podataka za odabranu kombinaciju.")
    )
    
    ggplot(tablica, aes(x = DAN, y = prosjekracuna, fill = DAN)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Dan u tjednu", y = sprintf("Prosječan %s", tolower(nazivmetrike())), title = sprintf("Prosječan %s po danu u tjednu", tolower(nazivmetrike()))) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal() +
      scale_fill_manual(values = c("Ponedjeljak" = "blue", "Utorak" = "darkgreen", "Srijeda" = "orange", "Četvrtak" = "brown", "Petak" = "gray40", "Subota" = "purple", "Nedjelja" = "red"))
  })
  
  tekstnedjelja <- reactive({
    tablica <- podanima()
    
    if (nrow(tablica) == 0) {
      return("Nema podataka za odabranu kombinaciju.")
    }
    
    tablicaradni <- filter(tablica, DAN %in% c("Ponedjeljak","Utorak","Srijeda","Četvrtak","Petak"))
    vrijednostiradni <- pull(tablicaradni, prosjekracuna)
    prosjekradni <- mean(vrijednostiradni, na.rm = TRUE)
    
    tablicanedjelja <- filter(tablica, DAN == "Nedjelja")
    prosjeknedjelja <- pull(tablicanedjelja, prosjekracuna)
    
    if (length(prosjeknedjelja) == 0 || is.na(prosjekradni) || prosjekradni == 0) {
      return("Nema dovoljno podataka za usporedbu nedjelje s radnim danima.")
    }
    
    padpct <- round((1 - prosjeknedjelja / prosjekradni) * 100, 1)
    smjer <- ifelse(padpct > 0, "niži", "viši")
    
    sprintf("Nedjeljom je promet u prosjeku %s%% %s nego radnim danom.", abs(padpct), smjer)
  })
  
  output$tekstnedjelja <- renderText({
    tekstnedjelja()
  })
  
  pomjesecu <- reactive({
    tablica <- filtrirano()
    tablica <- mutate(tablica, Godina = format(DATUM, "%Y"), Mjesec = format(DATUM, "%m"))
    tablica <- group_by(tablica, Godina, Mjesec)
    tablica <- summarise(tablica, ukupnoracuna = sum(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    tablica
  })
  
  output$grafmjeseci <- renderPlot({
    tablica <- pomjesecu()
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema podataka za odabranu kombinaciju.")
    )
    
    bojegodina <- c("blue", "red", "purple", "orange", "green", "brown", "pink", "gray40", "black", "darkgreen", "gold", "cyan")
    brojgodina <- length(unique(tablica$Godina))
    
    ggplot(tablica, aes(x = Mjesec, y = ukupnoracuna, color = Godina, group = Godina)) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(values = rep(bojegodina, length.out = brojgodina)) +
      labs(x = "Mjesec", y = nazivmetrike(), title = "Usporedba po mjesecima kroz godine") +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal()
  })
  
  zupanije <- reactive({
    tablica <- filter(svipodaci, DATUM >= input$raspon[1], DATUM <= input$raspon[2])
    
    if (input$djelatnost != "Sve djelatnosti") {
      tablica <- filter(tablica, DJELATNOST == input$djelatnost)
    }
    
    if (input$metrika == "broj") {
      tablica <- mutate(tablica, VRIJEDNOST = BROJ_RACUNA)
    } else {
      tablica <- mutate(tablica, VRIJEDNOST = IZNOS_RACUNA)
    }
    
    tablica <- group_by(tablica, ŽUPANIJA)
    tablica <- summarise(tablica, ukupnoracuna = sum(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    tablica <- arrange(tablica, desc(ukupnoracuna))
    tablica
  })
  
  output$grafzupanije <- renderPlot({
    tablica <- zupanije()
    
    if (input$zupanija != "Sve županije") {
      tablica <- filter(tablica, ŽUPANIJA == input$zupanija)
    }
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema dovoljno podataka za ovu kombinaciju.")
    )
    
    ggplot(tablica, aes(x = reorder(ŽUPANIJA, ukupnoracuna), y = ukupnoracuna)) +
      geom_col(width = 0.65, fill = "blue") +
      coord_flip() +
      labs(x = NULL, y = sprintf("Ukupan %s", tolower(nazivmetrike())), title = sprintf("%s po županijama", nazivmetrike())) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  }, height = function() {
    if (input$zupanija != "Sve županije") 220 else 60 + 35 * length(svezupanije)
  })
  
  zabrana <- reactive({
    datumzabrane <- as.Date("2023-07-01")
    
    tablica <- svipodaci
    if (input$zupanija != "Sve županije") {
      tablica <- filter(tablica, ŽUPANIJA == input$zupanija)
    }
    if (input$djelatnost != "Sve djelatnosti") {
      tablica <- filter(tablica, DJELATNOST == input$djelatnost)
    }
    
    if (input$metrika == "broj") {
      tablica <- mutate(tablica, VRIJEDNOST = BROJ_RACUNA)
    } else {
      tablica <- mutate(tablica, VRIJEDNOST = IZNOS_RACUNA)
    }
    
    prije <- filter(tablica, DATUM >= datumzabrane - 730, DATUM < datumzabrane)
    poslije <- filter(tablica, DATUM >= datumzabrane, DATUM < datumzabrane + 730)
    
    prije <- group_by(prije, DAN)
    prije <- summarise(prije, prosjekprije = mean(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    
    poslije <- group_by(poslije, DAN)
    poslije <- summarise(poslije, prosjekposlije = mean(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    
    spojeno <- inner_join(prije, poslije, by = "DAN")
    spojeno <- mutate(spojeno, DAN = factor(DAN, levels = redoslijeddana))
    spojeno <- mutate(spojeno, promjenapct = round((prosjekposlije / prosjekprije - 1) * 100, 1))
    spojeno <- arrange(spojeno, DAN)
    spojeno
  })
  
  output$grafzabrana <- renderPlot({
    tablica <- zabrana()
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema dovoljno podataka za usporedbu prije i poslije zabrane.")
    )
    
    ggplot(tablica, aes(x = DAN, y = promjenapct, fill = promjenapct < 0)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%s%%", promjenapct), vjust = ifelse(promjenapct < 0, 1.4, -0.5)), size = 4) +
      geom_hline(yintercept = 0, color = "grey40") +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
      labs(x = "Dan u tjednu", y = "Promjena prometa (%)", title = "Promjena prometa nakon uvođenja zabrane rada nedjeljom", subtitle = "Uspoređene su dvije godine prije i dvije godine nakon 1. srpnja 2023.") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$tekstzabrana <- renderText({
    tablica <- zabrana()
    
    if (nrow(tablica) == 0) {
      return("Nema dovoljno podataka za usporedbu prije i poslije zabrane.")
    }
    
    nedjeljaredak <- filter(tablica, DAN == "Nedjelja")
    subotaredak <- filter(tablica, DAN == "Subota")
    
    if (nrow(nedjeljaredak) == 0) {
      return("Nema podataka o nedjeljama za odabranu kombinaciju.")
    }
    
    ostali <- filter(tablica, DAN != "Nedjelja")
    prosjekostali <- round(mean(ostali$promjenapct, na.rm = TRUE), 1)
    
    smjernedjelja <- ifelse(nedjeljaredak$promjenapct < 0, "pao", "porastao")
    smjerostali <- ifelse(prosjekostali < 0, "pao", "porastao")
    
    tumacenje <- if (nedjeljaredak$promjenapct < 0 && prosjekostali > 0) {
      "Pad prometa nedjeljom praćen je rastom na drugim danima, što upućuje da se dio prometa preselio, a ne izgubio."
    } else if (nedjeljaredak$promjenapct > 0 && prosjekostali < 0) {
      "Promet nedjeljom je porastao, dok je na drugim danima pao. Za ovu kombinaciju uzorak je vjerojatno malen, pa postoci mogu biti nestabilni."
    } else if (nedjeljaredak$promjenapct < 0 && prosjekostali < 0) {
      "Promet je pao i nedjeljom i na drugim danima, što upućuje na opći pad prometa, ne samo preseljenje."
    } else {
      "Promet je porastao i nedjeljom i na drugim danima, što upućuje na opći rast prometa."
    }
    
    sprintf("Nakon 1. srpnja 2023. promet nedjeljom je %s za %s%%, subotom za %s%%, a prosječno na ostalim danima je %s za %s%%. %s", smjernedjelja, abs(nedjeljaredak$promjenapct), subotaredak$promjenapct, smjerostali, abs(prosjekostali), tumacenje)
  })
  
  nedjelje <- reactive({
    tablica <- filter(svipodaci, DATUM >= input$raspon[1], DATUM <= input$raspon[2])
    
    if (input$zupanija != "Sve županije") {
      tablica <- filter(tablica, ŽUPANIJA == input$zupanija)
    }
    
    if (input$metrika == "broj") {
      tablica <- mutate(tablica, VRIJEDNOST = BROJ_RACUNA)
    } else {
      tablica <- mutate(tablica, VRIJEDNOST = IZNOS_RACUNA)
    }
    
    tablica <- group_by(tablica, DJELATNOST, DAN)
    tablica <- summarise(tablica, prosjek = mean(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    tablica <- group_by(tablica, DJELATNOST)
    tablica <- summarise(tablica, prosjekradni = mean(prosjek[DAN %in% c("Ponedjeljak","Utorak","Srijeda","Četvrtak","Petak")], na.rm = TRUE), prosjeknedjelja = mean(prosjek[DAN == "Nedjelja"], na.rm = TRUE), .groups = "drop")
    tablica <- mutate(tablica, promjenapct = round((prosjeknedjelja / prosjekradni - 1) * 100, 1))
    tablica <- filter(tablica, !is.na(promjenapct))
    tablica <- arrange(tablica, promjenapct)
    tablica
  })
  
  output$grafnedjelje <- renderPlot({
    tablica <- nedjelje()
    
    if (input$djelatnost != "Sve djelatnosti") {
      tablica <- filter(tablica, DJELATNOST == input$djelatnost)
    }
    
    shiny::validate(
      shiny::need(nrow(tablica) > 0, "Nema dovoljno podataka za ovu kombinaciju.")
    )
    
    najvecavrijednost <- max(abs(tablica$promjenapct), na.rm = TRUE)
    granica <- najvecavrijednost * 1.15
    
    ggplot(tablica, aes(x = reorder(DJELATNOST, promjenapct), y = promjenapct, fill = promjenapct < 0)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%s%%", promjenapct), hjust = ifelse(promjenapct < 0, 1.1, -0.1)), size = 4, color = "black") +
      coord_flip(clip = "off", ylim = c(-granica, granica)) +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
      labs(x = NULL, y = "Promjena nedjeljom u odnosu na radni dan (%)", title = "Utjecaj nedjelje po djelatnostima", subtitle = "Crveno = manji promet nedjeljom, plavo = veći promet nedjeljom") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "bold"), panel.grid.minor = element_blank(), plot.margin = margin(10, 30, 10, 10))
  }, height = function() {
    if (input$djelatnost != "Sve djelatnosti") 220 else 60 + 35 * length(svedjelatnosti)
  })
  
  inflacijamj <- reactive({
    tablica <- filtrirano()
    tablica <- mutate(tablica, Godina = as.integer(format(DATUM, "%Y")), MjesecBr = as.integer(format(DATUM, "%m")))
    tablica <- group_by(tablica, Godina, MjesecBr)
    tablica <- summarise(tablica, ukupaniznos = sum(IZNOS_RACUNA, na.rm = TRUE), .groups = "drop")
    tablica <- inner_join(tablica, inflacija, by = c("Godina", "MjesecBr"))
    tablica <- mutate(tablica, MjesecDatum = as.Date(sprintf("%d-%02d-01", Godina, MjesecBr)))
    tablica <- arrange(tablica, MjesecDatum)
    
    if (nrow(tablica) > 1) {
      svimjeseci <- data.frame(MjesecDatum = seq(min(tablica$MjesecDatum), max(tablica$MjesecDatum), by = "month"))
      tablica <- full_join(svimjeseci, tablica, by = "MjesecDatum")
      tablica <- arrange(tablica, MjesecDatum)
    }
    
    tablica
  })
  
  output$grafrealni <- renderPlot({
    tablica <- inflacijamj()
    
    shiny::validate(
      shiny::need(sum(!is.na(tablica$ukupaniznos)) > 0, "Nema podataka za odabranu kombinaciju."),
      shiny::need(sum(!is.na(tablica$ukupaniznos)) >= 12, "Nema dovoljno podataka za pouzdanu usporedbu. Potrebno je najmanje 12 mjeseci."),
      shiny::need(sum(!is.na(tablica$indekscijena)) > 0, "Podaci o indeksu cijena trenutno nisu dostupni."),
      shiny::need(all(tablica$ukupaniznos > 0, na.rm = TRUE), "Odabrana kombinacija sadrži negativne ili nulte iznose, pa usporedba nije pouzdana.")
    )
    
    bazniindeks <- tablica$indekscijena[which(!is.na(tablica$indekscijena))[1]]
    tablica <- mutate(tablica, realniiznos = ukupaniznos * bazniindeks / indekscijena)
    
    nominalno <- data.frame(MjesecDatum = tablica$MjesecDatum, iznos = tablica$ukupaniznos, vrsta = "Nominalni iznos")
    realno <- data.frame(MjesecDatum = tablica$MjesecDatum, iznos = tablica$realniiznos, vrsta = "Realni iznos")
    zajedno <- bind_rows(nominalno, realno)
    
    ggplot(zajedno, aes(x = MjesecDatum, y = iznos, color = vrsta)) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(values = c("Nominalni iznos" = "darkblue", "Realni iznos" = "orange")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      labs(x = NULL, y = "Iznos računa (EUR)", color = NULL, title = "Nominalni i realni iznos fiskaliziranih računa", subtitle = "Realni iznos je preračunat u cijene prvog prikazanog mjeseca.") +
      theme_minimal()
  })
  
  output$grafstopa <- renderPlot({
    tablica <- inflacijamj()
    
    shiny::validate(
      shiny::need(sum(!is.na(tablica$stopainflacije)) > 0, "Nema podataka za odabranu kombinaciju.")
    )
    
    ggplot(tablica, aes(x = MjesecDatum, y = stopainflacije)) +
      geom_line(color = "red", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(x = "Mjesec", y = "Godišnja stopa inflacije (%)", title = "Stopa inflacije (DZS)") +
      theme_minimal()
  })
  
  output$tekstinflacija <- renderText({
    tablica <- inflacijamj()
    tablica <- filter(tablica, !is.na(ukupaniznos))
    
    if (nrow(tablica) == 0) {
      return("Nema podataka za odabranu kombinaciju.")
    }
    
    if (nrow(tablica) < 12) {
      return("Nema dovoljno podataka za pouzdanu usporedbu.")
    }
    
    if (sum(!is.na(tablica$indekscijena)) == 0) {
      return("Podaci o indeksu cijena trenutno nisu dostupni.")
    }
    
    if (!all(tablica$ukupaniznos > 0)) {
      return("Odabrana kombinacija sadrži negativne ili nulte iznose, pa usporedba nije pouzdana.")
    }
    
    bazniindeks <- tablica$indekscijena[which(!is.na(tablica$indekscijena))[1]]
    tablica <- mutate(tablica, realniiznos = ukupaniznos * bazniindeks / indekscijena)
    
    tablica <- slice(tablica, 2:(nrow(tablica) - 1))
    
    prvi <- slice(tablica, 1)
    zadnji <- slice(tablica, nrow(tablica))
    
    if (prvi$ukupaniznos < 100000) {
      return("Za odabranu kombinaciju iznosi su premali da bi postotna promjena bila pouzdana. Grafovi iznad prikazuju kretanje kroz vrijeme.")
    }
    
    rastnominalni <- round((zadnji$ukupaniznos / prvi$ukupaniznos - 1) * 100, 1)
    rastrealni <- round((zadnji$realniiznos / prvi$realniiznos - 1) * 100, 1)
    
    osnovno <- sprintf("Usporedba obuhvaća razdoblje od %s. mjeseca %s. do %s. mjeseca %s., pri čemu su prvi i zadnji mjesec odabranog razdoblja izostavljeni jer mogu biti nepotpuni. Nominalni iznos računa promijenio se za %s%%, a realni, uz uklonjen učinak rasta cijena, za %s%%. Razlika između te dvije vrijednosti pokazuje koliki je dio promjene posljedica inflacije, a koliki stvarne promjene potrošnje.", prvi$MjesecBr, prvi$Godina, zadnji$MjesecBr, zadnji$Godina, rastnominalni, rastrealni)
    
    if (prvi$Godina < 2026 && zadnji$Godina >= 2026) {
      return(sprintf("%s Od 1. siječnja 2026. postupak fiskalizacije se primjenjuje na sve obveznike fiskalizacije u krajnjoj potrošnji neovisno o načinu plaćanja, dok je ranijih godina bio neobavezan za transakcijska plaćanja, stoga podaci iz 2026. nisu u cijelosti usporedivi s prethodnim godinama.", osnovno))
    }
    
    osnovno
  })
  
  serijamj <- reactive({
    tablica <- filtrirano()
    
    zadnjidatum <- max(tablica$DATUM)
    pocetakmjeseca <- as.Date(format(zadnjidatum, "%Y-%m-01"))
    krajmjeseca <- seq(pocetakmjeseca, by = "month", length.out = 2)[2] - 1
    
    if (zadnjidatum < krajmjeseca) {
      tablica <- filter(tablica, format(DATUM, "%Y-%m") != format(zadnjidatum, "%Y-%m"))
    }
    
    tablica <- mutate(tablica, GodinaMjesec = format(DATUM, "%Y-%m"))
    tablica <- group_by(tablica, GodinaMjesec)
    tablica <- summarise(tablica, ukupno = sum(VRIJEDNOST, na.rm = TRUE))
    tablica <- arrange(tablica, GodinaMjesec)
    tablica
  })
  
  predvidjanje <- reactive({
    serija <- serijamj()
    
    shiny::validate(
      shiny::need(nrow(serija) >= 24, "Nema dovoljno podataka za pouzdano predviđanje. Potrebno je najmanje 24 mjeseca.")
    )
    
    prvagodina <- as.integer(substr(serija$GodinaMjesec[1], 1, 4))
    prvimjesec <- as.integer(substr(serija$GodinaMjesec[1], 6, 7))
    
    serijats <- ts(serija$ukupno, start = c(prvagodina, prvimjesec), frequency = 12)
    
    model <- tryCatch(HoltWinters(serijats), error = function(cond) { return(NULL) })
    
    shiny::validate(
      shiny::need(!is.null(model), "Predviđanje trenutno nije moguće za ovu kombinaciju.")
    )
    
    predikcija <- predict(model, n.ahead = 6, prediction.interval = TRUE, level = 0.95)
    
    zadnjimjesec <- as.Date(sprintf("%s-01", serija$GodinaMjesec[nrow(serija)]))
    bducidatumi <- seq(zadnjimjesec, by = "month", length.out = 7)
    bducidatumi <- bducidatumi[-1]
    
    stvarno <- data.frame(Datum = as.Date(sprintf("%s-01", serija$GodinaMjesec)), ukupno = serija$ukupno, donja = NA, gornja = NA, tip = "Stvarno")
    
    predvideno <- data.frame(Datum = bducidatumi, ukupno = as.numeric(predikcija[, "fit"]), donja = as.numeric(predikcija[, "lwr"]), gornja = as.numeric(predikcija[, "upr"]), tip = "Predviđeno")
    
    provjera <- svipodaci
    if (input$zupanija != "Sve županije") {
      provjera <- filter(provjera, ŽUPANIJA == input$zupanija)
    }
    if (input$djelatnost != "Sve djelatnosti") {
      provjera <- filter(provjera, DJELATNOST == input$djelatnost)
    }
    if (input$metrika == "broj") {
      provjera <- mutate(provjera, VRIJEDNOST = BROJ_RACUNA)
    } else {
      provjera <- mutate(provjera, VRIJEDNOST = IZNOS_RACUNA)
    }
    mjesectrenutni <- format(Sys.Date(), "%Y-%m")
    provjera <- filter(provjera, format(DATUM, "%Y-%m") != mjesectrenutni)
    provjera <- mutate(provjera, GodinaMjesec = format(DATUM, "%Y-%m"))
    provjera <- group_by(provjera, GodinaMjesec)
    provjera <- summarise(provjera, ukupno = sum(VRIJEDNOST, na.rm = TRUE), .groups = "drop")
    provjera <- mutate(provjera, Datum = as.Date(sprintf("%s-01", GodinaMjesec)))
    provjera <- filter(provjera, Datum %in% bducidatumi)
    
    if (nrow(provjera) > 0) {
      naknadno <- data.frame(Datum = provjera$Datum, ukupno = provjera$ukupno, donja = NA, gornja = NA, tip = "Stvarno nakon predviđanja")
      return(bind_rows(stvarno, predvideno, naknadno))
    }
    
    bind_rows(stvarno, predvideno)
  })
  
  output$grafpredvidjanje <- renderPlot({
    tablica <- predvidjanje()
    
    ggplot(tablica, aes(x = Datum, y = ukupno, color = tip)) +
      geom_ribbon(aes(ymin = donja, ymax = gornja), fill = "purple", alpha = 0.2, color = NA) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(values = c("Stvarno" = "blue", "Predviđeno" = "purple", "Stvarno nakon predviđanja" = "darkgreen")) +
      labs(x = "Mjesec", y = nazivmetrike(), color = NULL, title = "Predviđanje", subtitle = "Prikazano je predviđanje za sljedećih 6 mjeseci.") +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal()
  })
  
  output$tekstpredvidjanje <- renderText({
    tablica <- predvidjanje()
    imaprovjeru <- any(tablica$tip == "Stvarno nakon predviđanja")
    
    osnovno <- "Predviđanje koristi Holt-Wintersov model koji uzima u obzir trend i sezonske razlike. Model se ponovno procjenjuje za svako odabrano razdoblje. Ljubičasto područje prikazuje interval pouzdanosti od 95%."
    
    if (imaprovjeru) {
      return(sprintf("%s Zelena linija prikazuje stvarne podatke za razdoblje predviđanja, pa se može vidjeti koliko je predviđanje bilo točno.", osnovno))
    }
    
    osnovno
  })
}

shinyApp(ui = ui, server = server)
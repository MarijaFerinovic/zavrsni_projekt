# Fiskalizirani računi u Hrvatskoj

Ovo je Shiny web aplikacija za praćenje podataka o fiskaliziranim računima u Hrvatskoj. Napravljena je za pregled i analizu javno dostupnih podataka Porezne uprave i Državnog zavoda za statistiku.

Aplikacija prikazuje povijesne podatke od 2018. do danas i sama dohvaća najnovije podatke pri svakom pokretanju. Omogućuje prikaze po vremenskim razdobljima, mjesecima, danima, županijama i djelatnostima, te analizu utjecaja neradnih nedjelja i inflacije na promet. Autor Marija Ferinović

**Aplikacija:** https://mferinovic.shinyapps.io/fiskalizacija_racuna/

## Korištene tehnologije

* R i Shiny
* bslib (izgled sučelja)
* dplyr i ggplot2 (obrada podataka i grafovi)
* httr, jsonlite i rjstat (dohvat podataka s API-ja)
* readxl i readr (čitanje Excel i CSV datoteka)

## Pokretanje aplikacije

Potreban je R, preporučeno 4.5 ili noviji i RStudio.

Nakon kloniranja repozitorija treba instalirati potrebne pakete:

install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "readr", "httr", "readxl", "jsonlite", "rjstat"))

Datoteka app.R i folder data/ moraju biti u istom direktoriju.

Aplikacija se pokreće otvaranjem app.R u RStudiu i klikom na Run App, ili u konzoli:

shiny::runApp()

Podaci se nalaze u:

data/svi_podaci.csv
data/inflacija.csv

Pri pokretanju aplikacija sama provjerava jesu li dostupni noviji podaci na portalu Porezne uprave i DZS-a, te ih po potrebi preuzima i sprema natrag u iste datoteke.

## Napomena o .gitattributes

Datoteka .gitattributes koristi se za Git LFS, koji omogućuje da data/svi_podaci.csv, preko 1.14 milijuna redova i oko 108 MB, bude uključen u repozitorij unatoč GitHubovom ograničenju veličine datoteka.

## Izvori podataka

* Porezna uprava: https://porezna.gov.hr/fiskalizacija/izvjestaji/
* Državni zavod za statistiku: https://web.dzs.hr/PXWeb/

## Napomena

Od 1. siječnja 2026. postupak fiskalizacije primjenjuje se na sve obveznike u krajnjoj potrošnji neovisno o načinu plaćanja. Ranijih godina postupak je bio neobavezan za transakcijska plaćanja, stoga podaci iz 2026. nisu u cijelosti usporedivi s prethodnim godinama.

library(httr)
library(rvest)
library(tidyverse)
library(jsonlite)
library(progress)
library(stringr)


# URL til bilbasen - søger efter BMW med diesel
startlink <- "https://www.bilbasen.dk/brugt/bil/bmw?fuel=3&includeengroscvr=true&includeleasing=false&page="

# Definer User-Agent
UserA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"


readRenviron(".Renviron")
cookie <- Sys.getenv("cookie")

# Send GET-request med User-Agent
rawres <- GET(
  url = startlink,
  add_headers(
    `User-Agent` = UserA,
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Connection` = "keep-alive",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    `Cookie` = cookie
  )
)
print(rawres$status_code)

# Tjek statuskode
print(rawres$status_code)

# Hent HTML-indholdet fra svaret
rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
page <- read_html(rawcontent)

# Ekstraher biloplysninger fra siden
carlist <- page %>% html_elements("article")

# Definer CSS-selectors til at hente data
ptag <- ".Listing_price__6B3kE"
makertag <- ".Listing_makeModel__7yqgs"
bildetails <- ".ListingDetails_listItem___omDg"
bildescripts <- ".Listing_description__sCNNM"
billocation <- ".Listing_location__nKGQz"
prop <- ".Listing_properties___ptWv"

# Opret en dataframe til at gemme bildata
bilbasen100biler <- data.frame(matrix(data = NA, nrow = 0, ncol = 8))
ColnamesCars <- c("Pris", "Bilmodel", "Detaljer", "Beskrivelse", "Lokation", "Link", "Bil-ID", "Scrape-Dato")
carheader=c("pris","property","model","detailitems","description","location","link","carid","scrapedate")
colnames(bilbasen100biler) <- ColnamesCars



# Extract all spans on the page
all_spans <- page %>% html_elements("span") %>% html_text(trim = TRUE)
all_spans
# Her kan det ses at nummer 614 er den span der indeholder sidste sidetal

last_page <- as.numeric(all_spans[616]) 

for (i in 1:last_page) {
  loopurl <- paste0(startlink,i)
  Sys.sleep(runif(1, min = 0.5, max = 4))
  rawres <- GET(
    url = loopurl,
    add_headers(
      `User-Agent` = UserA,
      `Accept-Language` = "en-US,en;q=0.9",
      `Accept-Encoding` = "gzip, deflate, br",
      `Connection` = "keep-alive",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
      `Cookie` = cookie 
    )
    #pb$tick() progress bar, kan ikke få den til at virke
  )
  rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
  page <- read_html(rawcontent)
  #### Loop ####
  carlist <- page %>% html_elements("article")
  for (car in carlist) {
    tryCatch({
      pris <- car %>% html_element(ptag) %>% html_text(trim = TRUE)
      model <- car %>% html_element(makertag) %>% html_text(trim = TRUE)
      details <- car %>% html_elements(bildetails) %>% html_text(trim = TRUE) %>% paste0(collapse = "_")
      description <- car %>% html_element(bildescripts) %>% html_text(trim = TRUE)
      location <- car %>% html_element(billocation) %>% html_text(trim = TRUE)
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      property <- car %>% html_element(prop) %>% html_text(trim=T)
      
      # Opret en midlertidig dataframe og tilføj den til den samlede dataframe
      tmpDF <- data.frame(pris, model, details, property, description, location, link, carid, Sys.time(), stringsAsFactors = FALSE)
      bilbasen100biler <- rbind(bilbasen100biler, tmpDF)
    },
    error = function(cond) {
      print(cond)
    })
    current_row_count <- nrow(bilbasen100biler)
    Loop <- paste0("Loopet har nu fanget: ",current_row_count," biler, ved loop: ",i,", klokken: ",format(Sys.time(),"%a %b %d %X %Y"))
    print(Loop)
  }
  if (i==last_page){
    count <- as.numeric(n_distinct(bilbasen100biler$carid)) # dplyr pakke bruges til n_distinct
    if (nrow(bilbasen100biler)!=count) {
      IDcountDupe <- paste0("ID passer ikke, der er: ", nrow(bilbasen100biler)-count,"dublikationer")
      print(IDcount)
    } else if (nrow(bilbasen100biler) == count) {
      IDcount <- paste0("Der er ingen dublikationer i alle: ", count, " biler")
      print(IDcount)
    }
  }
} 

options(max.print = 10000) # ændre vores max print til 10000, så vi kan gøre hele nedestående linje
count(unique(bilbasen100biler,vars=c(carid,feature)),vars=carid) #891 unikke bil ID, betydende no dupes

count <- as.numeric(n_distinct(bilbasen100biler$carid)) # Samme men med dplyr



#### lav map over dk i kommune ####
# https://github.com/sebastianbarfort/mapDK


# Konverter location til 2; kommune og region
library(dplyr)
library(tidyr)
bildata <- as.data.frame(bilbasen100biler)
bildata <- df %>% separate(bildata$location, c("kommune","region"))

library(mapDK)
mapDK(values = "pris", id = "")




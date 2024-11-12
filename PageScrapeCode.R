library(httr)
library(rvest)
library(tidyverse)
library(jsonlite)



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

unique(bilbasen100biler$carid)

# Loop igennem biler og ekstraher data
for (i in 1:30) {
  loopurl <- paste0(startlink,i)
  Sys.sleep(3)
  Loop <- paste0("Loopet har nu fanget: ",i*30," biler, ved loop: ",i,", klokken: ",Sys.time())
  print(Loop)
  rawres <- GET(
    url = loopurl,
    add_headers(
      `User-Agent` = UserA,
      `Accept-Language` = "en-US,en;q=0.9",
      `Accept-Encoding` = "gzip, deflate, br",
      `Connection` = "keep-alive",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
      `Cookie` = "bbsession=id=dcf43cd2-6536-47dc-8ff4-51d24110e759; bbtracker=id=4885ecaf-3275-4e5f-b55f-e3856aa2a855; _pulse2data=099e8942-dca8-4e0e-84e6-3512469bbe85%2Cv%2C%2C1731950624000%2CeyJpc3N1ZWRBdCI6IjIwMjQtMTEtMTFUMTc6MjM6MzhaIiwiZW5jIjoiQTEyOENCQy1IUzI1NiIsInJlSXNzdWVkQXQiOiIyMDI0LTExLTExVDE3OjIzOjQ0WiIsImFsZyI6ImRpciIsImtpZCI6IjIifQ..JnRCQnyJHjCCfa6moauXlg.QWh1TCV3dkx3dOorgjvSyGLlUGq1ry-WsJrlGqhHjLwYsfo2Iud_f9VCCJWJUc8SZ3vHK2FEFCxkuwhgxNhoS0h8dEeDaG46IyQRDgVwb3TTxGqR-jxCyObOCSHwjmX_UyQjy23zKUTQHR9xZWYZiexjHtJ8h…p_marketing=1; _cmp_advertising=1; consentUUID=2093de02-b803-4dfa-9a2b-6560e6650e4e_37; consentDate=2024-11-11T17:23:40.525Z; aws-waf-token=e18a363b-7943-4583-9347-97efcace04d7:CgoAvqI8VjhyAAAA:hbVS7hABFKoTeNwIIRjIM4fU/LbRNMz0vyKoCm0Ac5yGXTNfuu9UUf7jYtvrNaep6g9YbXsz/P2uqvKVKwdQLvuPofO/rxjLufdaACwHXb22kAO98ZpBLuou0KmwCdiLAfzEaZwK3ai5QfNRzg0gXDP3MxF/C4KoaHG2SrS0JI5TXA4FeESkUe2TFE3esjpF; _pulsesession=%5B%22sdrn%3Aschibsted%3Asession%3A69cd6bb4-5a21-4fa3-8c3b-45b5dcd03a40%22%2C1731401287643%2C1731402689664%5D"
    )
  )
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
  }
}








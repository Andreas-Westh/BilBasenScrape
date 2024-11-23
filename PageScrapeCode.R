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
all_spans <- page %>% html_elements('span[data-e2e="pagination-total"]') %>% html_text(trim = TRUE)
last_page <- as.numeric(all_spans)

#### Webscrape Loop #### 
for (i in 1:last_page) { # Sleep + startlink til lastpage + headers
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
  carlist <- page %>% html_elements("article")
  for (car in carlist) { # Webscrape
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
  if (i==last_page){ #Bruges til at lave output i console
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

###########################################################
############################################################
##   Lav det, så man evt. selv kan vælge mærke, type osv  ##
##   I url så er alle biler = bil                         ##
############################################################
############################################################

options(max.print = 10000) # ændre vores max print til 10000, så vi kan gøre hele nedestående linje
count(unique(bilbasen100biler,vars=c(carid,feature)),vars=carid) #891 unikke bil ID, betydende no dupes

count <- as.numeric(n_distinct(bilbasen100biler$carid)) # Samme men med dplyr

#### Tyske hjemmesider ####
# 1. https://www.mobile.de/
# 2. https://www.autoscout24.de/
# 3. OOYYO.com
# 4. AutoUncle.de


# https://suchen.mobile.de/fahrzeuge/search.html?dam=false&ft=ELECTRICITY&isSearchRequest=true&ms=3500%3B%3B%3B&pageNumber=1&ref=srpNextPage&refId=bc536ead-c5ad-10c7-3f22-6b23e45d7bf4&s=Car&sb=rel&vc=Car

# https://www.autoscout24.de/lst/bmw?atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&page=2&powertype=kw&search_id=1pfsvl8rerg&sort=standard&source=listpage_pagination&ustate=N%2CU

#### Webscrape Tyskland ####
UserT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:132.0) Gecko/20100101 Firefox/132.0"
Tstartlink <- paste0("https://www.autoscout24.de/lst/bmw?atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&page=","","&powertype=kw&search_id=1pfsvl8rerg&sort=standard&source=listpage_pagination&ustate=N%2CU")
Trawres <- GET(
  url = Tstartlink,
  add_headers(
    `User-Agent` = UserT#,
    #`Accept-Language` = "en-US,en;q=0.9",
    #`Accept-Encoding` = "gzip, deflate, br",
    #`Connection` = "keep-alive",
    #`Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    #`Cookie` = cookie
  )
)
print(Trawres$status_code)
Trawcontent <- httr::content(Trawres, as = "text", encoding = "UTF-8")
Tpage <- read_html(Trawcontent)
Tcarlist <- Tpage %>% html_elements("article")

# CSS selectors til nede super kreative tags
Tpricetag <- "p.Price_price__APlgs.PriceAndSeals_current_price__ykUpx"
Tnametag <- "h2" 
Tmilagetag <- 'span[data-testid="VehicleDetails-mileage_road"]'
Tcalendertag <- 'span[data-testid="VehicleDetails-calendar"]'
Tgastag <- 'span[data-testid="VehicleDetails-gas_pump"]'
Tspeedometertag <- 'span[data-testid="VehicleDetails-speedometer"]'
Tdistancetag <- 'span[data-testid="VehicleDetails-distance"]'
Tlightningtag <- 'span[data-testid="VehicleDetails-lightning_bolt"]'
Tleaftag <- 'span[data-testid="VehicleDetails-leaf"]'
Tsellertag <- 'span[data-testid="sellerinfo-company-name"]'
Tratingtag <- 'span.BlackStars_wrapper__stcae'
Timagetag <- '.img.NewGallery_img__cXZQC'


Tyskebiler <- data.frame(matrix(data = NA, nrow = 0, ncol = 13))
ColnamesTysk <- c("Pris", "Navn", "Milage", "Calender", "Type", "Speedometer", "Distence", "Lightning", "Leaf", "Seller", "SellerRating", "Image", "Scrape-date")
colnames(Tyskebiler) <- ColnamesCars

#Tlast_page <- Tpage %>% html_elements('button') %>% html_text(trim = TRUE)

# Denne hjemmeside viser kun 20 sider......
# Kan evt genskabe, men for den anden side, den viste flere
for (i in 1:20) {
  Tloopurl <- paste0("https://www.autoscout24.de/lst/bmw?atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&page=",i,"&powertype=kw&search_id=1pfsvl8rerg&sort=standard&source=listpage_pagination&ustate=N%2CU")
  Sys.sleep(runif(1, min = 0.5, max = 4))
  print(i)
  Trawres <- GET(
    url = Tloopurl,
    add_headers(`User-Agent` = UserT)
  )
      if (Trawres$status_code != 200) {
      print(paste("Fejl i anmodning, statuskode:", Trawres$status_code))
       }
  
  Trawcontent <- httr::content(Trawres, as = "text", encoding = "UTF-8")
  Tpage <- read_html(Trawcontent)
  
  Tcarlist <- Tpage %>% html_elements("article")
  
  for (Tcar in Tcarlist) {
    Tpris <- Tcar %>% html_element(Tptag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tname <- Tcar %>% html_element(Tnametag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tmilage <- Tcar %>% html_element(Tmilagetag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tcalender <- Tcar %>% html_element(Tcalendertag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tgas <- Tcar %>% html_element(Tgastag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tspeedometer <- Tcar %>% html_element(Tspeedometertag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tdistance <- Tcar %>% html_element(Tdistancetag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tlightning <- Tcar %>% html_element(Tlightningtag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tleaf <- Tcar %>% html_element(Tleaftag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Tseller <- Tcar %>% html_element(Tsellertag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Trating <- Tcar %>% html_element(Tratingtag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
    Timage <- Tcar %>% html_element(Timagetag) %>% html_attr("src") %>% ifelse(is.na(.), "NA", .)
    
    tmpDF <- data.frame(
      Tpris, Tname, Tmilage, Tcalender, Tgas, Tspeedometer, 
      Tdistance, Tlightning, Tleaf, Tseller, Trating, Timage, 
      Sys.time(), stringsAsFactors = FALSE
    )
    
    Tyskebiler <- rbind(Tyskebiler, tmpDF)
  }
}





#for (i in 1:last_page) { # Sleep + startlink til lastpage + headers
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
  carlist <- page %>% html_elements("article")
  for (car in carlist) { # Webscrape
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
  if (i==last_page){ #Bruges til at lave output i console
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


#### lav map over dk i kommune ####
# https://github.com/sebastianbarfort/mapDK


# Konverter location til 2; kommune og region
library(dplyr)
library(tidyr)
bildata <- as.data.frame(bilbasen100biler)
bildata <- df %>% separate(bildata$location, c("kommune","region"))

library(mapDK)
mapDK(values = "pris", id = "")




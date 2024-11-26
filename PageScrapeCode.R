library(httr)
library(rvest)
library(tidyverse)
library(jsonlite)
library(progress)
library(stringr)

#### Opgave 1.1 ####
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


# Hent HTML-indholdet fra svaret
rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
page <- read_html(rawcontent)

# Ekstraher biloplysninger fra siden
carlist <- page %>% html_elements("article")

# Definer CSS-selectors til at hente data
ptag <- ".Listing_price__6B3kE"
makertag <- ".Listing_makeModel__7yqgs"
modalnametag <- "h3" # Kun udtage den specifikke bilmodel (bliver relevant ved den tyske scrape)
bildetails <- ".ListingDetails_listItem___omDg"
bildescripts <- ".Listing_description__sCNNM"
billocation <- ".Listing_location__nKGQz"
prop <- ".Listing_properties___ptWv"

# Opret en dataframe til at gemme bildata
bilbasenWebScrape <- data.frame(matrix(data = NA, nrow = 0, ncol = 9))
ColnamesCars <- c("Pris", "Bilmodel", "Specific Model", "Detaljer", "Beskrivelse", "Lokation", "Link", "Bil-ID", "Scrape-Dato")
colnames(bilbasenWebScrape) <- ColnamesCars


# Extract all spans on the page
last_page <- page %>% html_elements('span[data-e2e="pagination-total"]') %>% html_text(trim = TRUE) %>% as.numeric()
#bilbasenWebScrape <- readRDS("bilbasenWebScrape_2024-11-23-22-35.rds")

#### Webscrape Loop #### 
for (i in 1:last_page) { # Sleep + startlink til lastpage + headers
  loopurl <- paste0(startlink,i)
  Sys.sleep(runif(1, min = 0.5, max = 2))
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
  )
  rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
  page <- read_html(rawcontent)
  carlist <- page %>% html_elements("article")
  for (car in carlist) { # Webscrape
    tryCatch({
      pris <- car %>% html_element(ptag) %>% html_text(trim = TRUE)
      model <- car %>% html_element(makertag) %>% html_text(trim = TRUE)
      specificmodel <- car %>% html_element(modalnametag) %>% html_text(trim = TRUE)
      details <- car %>% html_elements(bildetails) %>% html_text(trim = TRUE) %>% paste0(collapse = "_")
      description <- car %>% html_element(bildescripts) %>% html_text(trim = TRUE)
        #### Opgave 1.2 Rens data ####
            description <- gsub("\n\\d[0-9]*","",description) # Fjerner HTML/CSS page break
            description <- gsub("[^A-Za-z0-9,. æøåÆØÅ]", "", description) # Fjerner unwanted chars via ^ (basically en NOT)
            description <- gsub("\\s+", " ", description) # Samler multiple spaces med 1
            description <- gsub("\\s*\\.\\s*\\.\\s*", ". ", description) # Samler multiple . (selv hvis der er space) til 1
            description <- gsub("^\\s*[0-9]*\\.\\s*", "", description) # fjerner start med tal eller space
            description <- gsub("^\\s+", "", description)
      location <- car %>% html_element(billocation) %>% html_text(trim = TRUE)
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      property <- car %>% html_element(prop) %>% html_text(trim=T)
      
      # Opret en midlertidig dataframe og tilføj den til den samlede dataframe
      tmpDF <- data.frame(pris, model, specificmodel, details, property, description, location, link, carid, Sys.time(), stringsAsFactors = FALSE)
      bilbasenWebScrape <- rbind(bilbasenWebScrape, tmpDF)
    },
    error = function(cond) {
      print(cond)
    })
    current_row_count <- nrow(bilbasenWebScrape)
    Loop <- paste0("Loopet har nu fanget: ",current_row_count," biler, ved loop: ",i,", klokken: ",format(Sys.time(),"%Y-%m-%d-%H-%M"))
    print(Loop)
  }
  if (i==last_page){ #Bruges til at lave output i console
    count <- as.numeric(n_distinct(bilbasenWebScrape$carid)) # dplyr pakke bruges til n_distinct
    if (nrow(bilbasenWebScrape)!=count) {
      IDcountDupe <- paste0("ID passer ikke, der er: ", nrow(bilbasenWebScrape)-count,"dublikationer")
      print(IDcount)
    } else if (nrow(bilbasenWebScrape) == count) { # Sker kun, når loopet er færdigt
      IDcount <- paste0("Der er ingen dublikationer i alle: ", count, " biler")
      print(IDcount)
      # Gemmer filsen som en RDS
      RDSname <- paste0("bilbasenWebScrape_",format(Sys.time(), "%Y-%m-%d-%H-%M"),".rds")
      saveRDS(bilbasenWebScrape, RDSname)
      RDSsave <- paste0("Gemmer den scrapede data i filen: ",RDSname)
      print(RDSsave)
      # Laver en kolonne med den specifikke bilmoden, bliver relevant til det tyske loop
      bilbasenWebScrape$specificmodel <- sub(".*? ", "", bilbasenWebScrape$specificmodel) #Fjerne BMW fra kolonnen
      bilmodeller <- data.frame(unique(bilbasenWebScrape$specificmodel))
    }
  }
} 
bilbasenWebScrape$specificmodel <- sub(".*? ", "", bilbasenWebScrape$specificmodel) #Fjerne BMW fra kolonnen
bilmodeller <- as.list(unique(bilbasenWebScrape$specificmodel))

###########################################################
############################################################
##   Lav det, så man evt. selv kan vælge mærke, type osv  ##
##   I url så er alle biler = bil                         ##
############################################################
############################################################

options(max.print = 10000) # ændre vores max print til 10000, så vi kan gøre hele nedestående linje
count(unique(bilbasenWebScrape,vars=c(carid,feature)),vars=carid) #891 unikke bil ID, betydende no dupes

count <- as.numeric(n_distinct(bilbasenWebScrape$carid)) # Samme men med dplyr

#### Opgave 1.2 ####
#description <- page %>% html_element(bildescripts) %>% html_text(trim = TRUE)
#description <- gsub("\n\\d[0-9]*","",description) # Fjerner HTML/CSS page break
#description <- gsub("[^A-Za-z0-9,. ]", "", description). # Fjerner unwanted chars via ^ (basically en NOT)
#description <- gsub("\\s+", " ", description) # Samler multiple spaces med 1
#description <- gsub("\\s*\\.\\s*\\.\\s*", ". ", description) # Samler multiple . (selv hvis der er space) til 1

#### Tyske hjemmesider ####
# 1. https://www.mobile.de/
# 2. https://www.autoscout24.de/
# 3. OOYYO.com
# 4. AutoUncle.de


# https://suchen.mobile.de/fahrzeuge/search.html?dam=false&ft=ELECTRICITY&isSearchRequest=true&ms=3500%3B%3B%3B&pageNumber=1&ref=srpNextPage&refId=bc536ead-c5ad-10c7-3f22-6b23e45d7bf4&s=Car&sb=rel&vc=Car

# https://www.autoscout24.de/lst/bmw?atype=C&cy=D&damaged_listing=exclude&desc=0&fuel=E&ocs_listing=include&page=2&powertype=kw&search_id=1pfsvl8rerg&sort=standard&source=listpage_pagination&ustate=N%2CU

#### Webscrape Tyskland ####
# Lave et loop der macther til biler i Bilmodeller df
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
colnames(Tyskebiler) <- ColnamesTysk


#Tlast_page <- Tpage %>%
#  html_elements("li.pagination-item button") %>%
#  html_text(trim = TRUE) %>%
#  tail(1) %>% as.numeric()

# Vælge en MakeModel, for at komme rundt om 20 maks sider 
# scrollable-list
for (model in bilmodeller) { # Looper i gennem modellisten, og indsætter i linket
  Modelscrape <- paste0("Scraper nu for model: ", model)
  print(Modelscrape)
  Tlast_page <- NULL
  modellink <- paste0("https://www.autoscout24.de/lst/bmw/", model, "/ft_elektro?atype=C&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&page=")
  Trawres <- GET(
    url = modellink,
    add_headers(`User-Agent` = UserT)
  )
  if (Trawres$status_code != 200) { # Mindre kode til at tjekke status 
    print(paste("Fejl i anmodning, statuskode:", Trawres$status_code))
  }
  Trawcontent <- httr::content(Trawres, as = "text", encoding = "UTF-8")
  Tpage <- read_html(Trawcontent)
  Tlast_page <- Tpage %>%
    html_elements("li.pagination-item button") %>%
    html_text(trim = TRUE) %>%
    tail(1) %>% as.numeric()
  Ttotalsider <- paste0("Total antal sider for denne model: ", Tlast_page)
  print(Ttotalsider)
  modellink <- paste0("https://www.autoscout24.de/lst/bmw/", model, "/ft_elektro?atype=C&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&page=")
  for (i in 1:Tlast_page) { # Loop gennem sider for den nuværende model
    Tloopurl <- paste0(modellink,i,"&powertype=kw&search_id=1pfsvl8rerg&sort=standard&source=listpage_pagination&ustate=N%2CU")
    side_progress <- paste0("side: ",i,"/",Tlast_page," for modellen: ",model)
    print(side_progress)
    Sys.sleep(runif(1, min = 0.5, max = 2))
    Trawres <- GET(
      url = Tloopurl,
      add_headers(`User-Agent` = UserT)
    )
    Trawcontent <- httr::content(Trawres, as = "text", encoding = "UTF-8")
    Tpage <- read_html(Trawcontent)
    
    Tcarlist <- Tpage %>% html_elements("article")
    
    for (Tcar in Tcarlist) { #Loop at nuværende side, aka det reele scrape af bil info
      tryCatch({
        Tpris <- Tcar %>% html_element(Tpricetag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
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
      })
      error = function(cond) {
        print(cond)
      }
      Tcurrent_row_count <- nrow(Tyskebiler)
      TLoop <- paste0("Loopet har nu fanget: ",Tcurrent_row_count," tyske biler, klokken: ",format(Sys.time(),"%Y-%m-%d-%H-%M"))
      print(TLoop)
    }
  }
}

TRDSname <- paste0("TyskWebScrape_",format(Sys.time(), "%Y-%m-%d-%H-%M"),".rds")
saveRDS(Tyskebiler, TRDSname)

#### 1.3 ####
#Gem som CSV og selv lav ændringerne
#Det store arbejder er at systematisk samligne de 2 dataframe, f.eks. anti join, hvilke nye række og hvad er forskellen?




# Kunne være spændende at se om man kan scrape geo data, og lave et map
#
#
#
#
##### lav map over dk i kommune ####
## https://github.com/sebastianbarfort/mapDK

#### To SQL ####
library(DBI)
library(RMariaDB)

# Hvis i skal bruge hjælp til at opsætte dette, kan i min repo guide: https://github.com/60gCreatine/Skjul_API-KEY_i_RStudio_Git
# Ellers bare i stedet for password i con skriv jeres password HUSK IKKE AT UPLOADE TIL GIT HVIS I GØR DET!!!!!
readRenviron(".Renviron")
password <- paste0(Sys.getenv("password"),'"')

# I workbench lav database med navnet newbilbasen
# CREATE DATABASE newbilbasen;
con <- dbConnect(MariaDB(),
                 db="empl",
                 host="localhost",
                 port=3306,
                 user="root",
                 password=password)
bil <- readRDS("bilbasenWebScrape_2024-11-23-22-35.rds")
dbWriteTable(con,"cars",bil)



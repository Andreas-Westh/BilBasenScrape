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
forh <- ".Listing_dealerLogoCard__wHl9H"

# Opret en dataframe til at gemme bildata
bilbasenWebScrape <- data.frame(matrix(data = NA, nrow = 0, ncol = 14))
ColnamesCars <- c("Pris", "Bilmodel", "Specific Model", "Detaljer", "Beskrivelse", "Lokation", "Link", "Bil-ID", "Scrape-Dato", "registrering", "kilometertal","Forhandlerid","by","region")
colnames(bilbasenWebScrape) <- ColnamesCars


# Extract all spans on the page
last_page <- page %>% html_elements('span[data-e2e="pagination-total"]') %>% html_text(trim = TRUE) %>% as.numeric()
#bilbasenWebScrape <- readRDS("bilbasenWebScrape_2024-11-23-22-35.rds")

#### Webscrape Loop #### 
for (i in 1:last_page) { # Sleep + startlink til lastpage + headers
  loopurl <- paste0(startlink, i)
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
      pris <- str_replace_all(pris, "\\s*kr\\.*|\\.", "")
      model <- car %>% html_element(makertag) %>% html_text(trim = TRUE)
      specificmodel <- car %>% html_element(modalnametag) %>% html_text(trim = TRUE)
      details <- car %>% html_elements(bildetails) %>% html_text(trim = TRUE) %>% paste0(collapse = "_")
      description <- car %>% html_element(bildescripts) %>% html_text(trim = TRUE)
      forhandlerid <- car %>% html_elements(forh) %>% html_attr("src")
      if (length(forhandlerid) > 0) {
        # Rens hvis der er en gyldig værdi
        forhandlerid <- gsub(".*/bilinfo/|\\..*", "", forhandlerid)
      } else {
        # Sæt til NA hvis mangler
        forhandlerid <- NA
      }
      
      #### Rens data i 'description' ####
      description <- gsub("\n\\d[0-9]*","",description) # Fjerner HTML/CSS page break
      description <- gsub("[^A-Za-z0-9,. æøåÆØÅ]", "", description) # Fjerner unwanted chars
      description <- gsub("\\s+", " ", description) # Samler multiple spaces til 1
      description <- gsub("\\s*\\.\\s*\\.\\s*", ". ", description) # Samler multiple .
      description <- gsub("^\\s*[0-9]*\\.\\s*", "", description) # Fjerner start med tal eller space
      description <- gsub("^\\s+", "", description)
      
      location <- car %>% html_element(billocation) %>% html_text(trim = TRUE)
      by <- str_split(location, ",", simplify = TRUE) [, 1]
      region <- str_split(location, ",", simplify = TRUE) [, 2]
      link <- car %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      property <- car %>% html_element(prop) %>% html_text(trim = TRUE)
      
      
      #### Brug str_extract til at opdele 'details' ####
      registrering <- str_extract(details, "^\\d+/\\d{4}")              # Matcher 'registrering'
      kilometertal <- str_extract(details, "\\d+\\.\\d+\\s*km")        # Matcher 'kilometertal'
      rækkevidde <- str_extract(details, "\\d+\\s*km(?= rækkevidde)")  # Matcher 'rækkevidde'
      brændstof <- str_extract(details, "rækkevidde_[^_]+$")           # Matcher 'brændstof'
      kilometertal <- str_extract(details, "\\d+\\.\\d+\\s*km")        
      rækkevidde <- str_extract(details, "\\d+\\s*km(?= rækkevidde)")  
      kilometertal <- str_extract(kilometertal, "[0-9]+\\.?[0-9]*")  
      rækkevidde <- str_extract(rækkevidde, "[0-9]+")
      kilometertal <- str_replace_all(kilometertal, "\\.", "") 
      
      # Fjern "rækkevidde_" fra 'brændstof'
      brændstof <- gsub("^rækkevidde_", "", brændstof)
      
      # Opret midlertidig dataframe
      tmpDF <- data.frame(
        pris, model, specificmodel, details, property, description, 
        location, link, carid, Sys.time(), 
        registrering, kilometertal, rækkevidde, brændstof, forhandlerid, by, region,
        stringsAsFactors = F
      )
      bilbasenWebScrape <- rbind(bilbasenWebScrape, tmpDF)
      
    }, error = function(cond) {
      print(cond)
    })
    
    current_row_count <- nrow(bilbasenWebScrape)
    Loop <- paste0("Loopet har nu fanget: ", current_row_count, " biler, ved loop: ", i, ", klokken: ", format(Sys.time(), "%Y-%m-%d-%H-%M"))
    print(Loop)
  }
  
  if (i == last_page) { # Når sidste side er gennemgået
    count <- as.numeric(n_distinct(bilbasenWebScrape$carid))
    if (nrow(bilbasenWebScrape) != count) {
      IDcountDupe <- paste0("ID passer ikke, der er: ", nrow(bilbasenWebScrape) - count, " dublikationer")
      print(IDcountDupe)
    } else {
      IDcount <- paste0("Der er ingen dublikationer i alle: ", count, " biler")
      print(IDcount)
      
      # Fjern "rækkevidde_" fra 'brændstof'
      bilbasenWebScrape$brændstof <- gsub("^rækkevidde_", "", bilbasenWebScrape$brændstof)
      
      # Slet kolonnerne 'details' og 'property'
      bilbasenWebScrape <- bilbasenWebScrape %>% select(-details, -property, -location)
      
      # Gem data som RDS
      RDSname <- paste0("bilbasenWebScrape_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".rds")
      saveRDS(bilbasenWebScrape, RDSname)
      RDSsave <- paste0("Gemmer den scrapede data i filen: ", RDSname)
      print(RDSsave)
      
      # Fjern mærkenavne fra 'specificmodel'
      bilbasenWebScrape$specificmodel <- sub(".*? ", "", bilbasenWebScrape$specificmodel)
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


Tyskebiler <- data.frame(matrix(data = NA, nrow = 0, ncol = 15))
ColnamesTysk <- c("Pris (EUR)","Pris (DKK)", "Navn","Model","Milage", "Calender", "Type", "Speedometer", "Distence", "Lightning", "Leaf", "Seller", "SellerRating", "Image", "Scrape-date")
colnames(Tyskebiler) <- ColnamesTysk

for (model in bilmodeller) { # Looper i gennem modellisten, og indsætter i linket
  Modelscrape <- paste0("Scraper nu for model: ", model)
  print(Modelscrape)
  Tlast_page <- NULL
  modellink <- paste0("https://www.autoscout24.de/lst/bmw/", model, "/ft_elektro?atype=C&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&page=")
  Trawres <- GET(
    url = modellink,
    add_headers(`User-Agent` = UserT)
  )
  kurs <- GET("https://api.frankfurter.app/latest?from=EUR&to=DKK") # API til at omregne Kurs
  kurs_data <- fromJSON(content(kurs, as = "text"))
  eur_to_dkk <- as.numeric(kurs_data$rates$DKK)
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
        Tpris <- Tcar %>% html_element(Tpricetag) %>% html_text(trim = TRUE)
        Tpris <- ifelse(Tpris == "" | is.null(Tpris), NA, as.numeric(gsub("[^0-9]", "", Tpris)))
        DKKpris <- ifelse(is.na(Tpris), NA, Tpris * eur_to_dkk)
        
        Tname <- Tcar %>% html_element(Tnametag) %>% html_text(trim = TRUE) %>% ifelse(is.na(.), "NA", .)
        Tmodel <- model
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
          Tpris, DKKpris, Tname, Tmodel, Tmilage, Tcalender, Tgas, Tspeedometer, 
          Tdistance, Tlightning, Tleaf, Tseller, Trating, Timage, 
          Sys.time(), stringsAsFactors = FALSE)
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

fcvrtag <- "p.bas-MuiSellerInfoComponent-cvr"

#### Hente forhandler ID ####
bilidlink_start <- "https://www.bilbasen.dk/brugt/bil/bmw/i/"
fnavntag <- "div.bas-MuiVipSectionComponent-sectionHeader.bas-MuiSellerInfoComponent-headerStyles"
faddresstag <- "a[data-e2e='seller-address']"
fcvrtag <- "p.bas-MuiSellerInfoComponent-cvr"

forhlist <- as.list(bilbasenWebScrape$carid)

forhandler_data <- data.frame(matrix(data = NA, nrow = 0, ncol = 5))
Forh_col <- c("Carid","Forhandler_id","Forhandler","Addresse","CVR")
colnames(forhandler_data) <- Forh_col

# Loop gennem forhandler data for hver carid
for (i in forhlist) {
  # Construct the URL for the car
  loopbilid <- paste0(bilidlink_start, i)
  
  # Sleep to prevent being flagged as a bot
  Sys.sleep(runif(1, min = 0.5, max = 5))
  
  # Make the GET request
  frawres <- GET(
    url = loopbilid,
    add_headers(
      `User-Agent` = UserA,
      `Accept-Language` = "en-US,en;q=0.9",
      `Accept-Encoding` = "gzip, deflate, br",
      `Connection` = "keep-alive",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
      `Cookie` = cookie
    )
  )
  
  # Check if the response is successful
  if (frawres$status_code == 200) {
    # Parse the HTML content
    frawcontent <- httr::content(frawres, as = "text", encoding = "UTF-8")
    fpage <- read_html(frawcontent)
    
    # Scrape the specific details
    fnavn <- fpage %>% html_element(fnavntag) %>% html_text(trim = TRUE)
    faddress <- fpage %>% html_element(faddresstag) %>% html_text(trim = TRUE)
    fcvr <- fpage %>% html_element(fcvrtag) %>% html_text(trim = TRUE)
        fcvr <- gsub("[^0-9]", "", fcvr)
    
    # Append the scraped data to the dataframe
    forhandler_data <- rbind(
      forhandler_data, 
      data.frame(Carid = i, Forhandler_id = NA, Forhandler = fnavn, Addresse = faddress, CVR = fcvr, stringsAsFactors = FALSE)
    )
  } else {
    print(paste("Failed to scrape car ID:", i, "Status code:", frawres$status_code))
  }
  floopstatus <- paste0(nrow(forhandler_data),"/",n_distinct(bilbasenWebScrape$carid))
  print(floopstatus)
}
RDSfname <- paste0("bilbasenWebScrapeForhandler_", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".rds")
saveRDS(forhandler_data, RDSfname)
RDSfsave <- paste0("Gemmer den scrapede data i filen: ", RDSfname)
print(RDSsave)

  
  
  # Afgifter for 'billige' og for 'dyre'
# nypriser < 448000 (2023 niveau) er afgiftsfritaget 
# dyrere har registreringsafgift

# Kopier den oprindelige dataframe
Tyskebiler_simple <- Tyskebiler

# 1. Beregn alder baseret på Tcalender
Tyskebiler_simple$Alder <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(sub(".*/", "", Tyskebiler_simple$Tcalender))
# Beregn alder i måneder direkte fra 'Tcalender'
Tyskebiler_simple$Alder_i_måneder <- with(
  Tyskebiler_simple,
  (as.numeric(format(Sys.Date(), "%Y")) - as.numeric(sub(".*/", "", Tcalender))) * 12 +
    (as.numeric(format(Sys.Date(), "%m")) - as.numeric(sub("/.*", "", Tcalender)))
)


# 2. Fjern " km" fra Tmilage og konverter til numerisk
Tyskebiler_simple$Km <- as.numeric(gsub("[^0-9]", "", Tyskebiler_simple$Tmilage))


# 3. Beregn registreringsafgift baseret på dansk afgiftssystem
# Moms for elbiler under 6 mpneder og mindre en 6000 km
# https://www.tjekbil.dk/import-af-elbil-fra-tyskland
Tyskebiler_simple$PrisMoms <- 
  ifelse(
    Tyskebiler_simple$Alder_i_måneder <= 6 | Tyskebiler_simple$Km <= 6000, 
    Tyskebiler_simple$DKKpris * 1.25, 
    Tyskebiler_simple$DKKpris
  )

# Afkifter tages for 
Tyskebiler_simple$Registreringsafgift <- with(Tyskebiler_simple, {
  vurderet <- PrisMoms
  
  ifelse(
    vurderet <= 308000, 
    vurderet, # Ingen ændring for værdier under eller lig med 308.000
    ifelse(
      vurderet <= 619000, 
      308000 + (vurderet - 308000) * 1.20, # 20% for værdien mellem 308.000 og 619.000
      308000 + (619000 - 308000) * 1.20 + (vurderet - 619000) * 1.65 # 65% for værdien over 619.000
    )
  )
})


# 5. Beregn gennemsnit for DKKpris og Registreringsafgift pr. model
model_gennemsnit <- as.data.frame(aggregate(cbind(PrisMoms, Registreringsafgift) ~ Tmodel, data = Tyskebiler_simple, FUN = mean, na.rm = TRUE))
model_gennemsnit$ProcentDifference <- round(with(model_gennemsnit, ((Registreringsafgift - PrisMoms) / PrisMoms) * 100),1)
bilbasenWebScrape$pris <- as.numeric(bilbasenWebScrape$pris)
model_gennemsnit$DKbiler <- aggregate(pris~specificmodel, data = bilbasenWebScrape, FUN = mean, na.rn=T)
model_gennemsnit$DiffTyskOgDansk <- round(with(model_gennemsnit, ((Registreringsafgift - DKbiler$pris) / DKbiler$pris) * 100),1)
model_gennemsnit$DiffTyskOgDanskUdenAfgiftMoms <- round(with(model_gennemsnit, ((DKKpris - DKbiler$pris) / DKbiler$pris) * 100),1)

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



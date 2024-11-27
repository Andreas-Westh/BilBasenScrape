library(httr)
library(rvest)


#### Opgave 2.4 ####
# Skal kunne eksikveres i terminalen, og med målestationen som argument
startlink <- "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3"
UserA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"


readRenviron(".Renviron")
cookie <- Sys.getenv("cookie")
rawres <- GET(
  url = startlink,
  add_headers(
    `User-Agent` = UserA,
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Connection` = "keep-alive",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8"#,
    #`Cookie` = cookie
  )
)
print(rawres$status_code)
rawcontent <- httr::content(rawres, as = "text", encoding = "UTF-8")
page <- read_html(rawcontent)
cat(rawcontent) # Se opstillin, her kan det ses at tablet er indlæst dynamisk gennem Java

##### Århus bandegårdsgade #####
# URL for tabellen, som indlæses i JS
# https://imgur.com/a/IXWlTUE URL
# https://imgur.com/a/4lrwu4F RESPONSE
js_url <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"

# Finder den specifikke __RequestVerificationToken, som skal bruges for at godkende vores POST (indlæsning af tabel)
# https://imgur.com/a/Ieeeyyn
token <- read_html(rawcontent) %>%
  html_element("input[name='__RequestVerificationToken']") %>% 
  html_attr("value") # Trækker bare værdierne ud

# Java Script-generaret HTML POST request på en unload
jsPOST <- POST(
  url = js_url,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(`__RequestVerificationToken` = token), # skal bruges, for at indlæse POST
  encode = "form"
)

# henter HTML
table_html <- content(jsPOST, as = "text", encoding = "UTF-8")
table_page <- read_html(table_html)

# Scraper tabbels rows (hver <tr> - table row)
rows <- table_page %>% html_elements("tr")
table_data <- rows %>%
        html_elements("td") %>% # td = table data per table row
        html_text(trim = TRUE)
# Snupper også lige hurtigt headeren 
header <- table_page %>% html_elements("th") %>% html_text(trim = T)
header_amount <- as.numeric(length(header)) # bruges til at finde ud af, hvor mange kolonner skal laves i data frame

# laver det til dataframe og renser data
unlist <- unlist(table_data)
Aarhus <- as.data.frame(matrix(data = unlist, ncol = header_amount, byrow = T))
colnames(Aarhus) <- header
str(Aarhus) # alle er chr
Aarhus[, 2:header_amount] <- lapply(Aarhus[, 2:header_amount], function(x) as.numeric(gsub(",", ".", x))) 
#alternativt kan man ungå lapply/function ved at lave gsub for hver kolonne
str(Aarhus)
Aarhus_RDSname <- paste0("Aarhus_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
saveRDS(Aarhus,Aarhus_RDSname)

##### Risø #####
ri_link <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"
rirawres <- GET(
  url = ri_link,
  add_headers(`User Agent`= UserA))
print(rirawres$status_code)
ri_content <- httr::content(rirawres, as = "text", encoding = "UTF-8")
ri_js <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/RISOE"
ri_token <- read_html(ri_content) %>% html_elements("input[name=__RequestVerificationToken]") %>% html_attr("value")

ri_post <- POST(
  url=ri_js,
  add_headers(`User Agent` = UserA),
body = list(`__RequestVerificationToken` = token)
)

ritable_html <- content(ri_post, as = "text", encoding = "UTF-8")
ri_table <- read_html(ritable_html)

ri_rows <- ri_table %>% html_elements("tr")
ri_table_data <- ri_rows %>%
  html_elements("td") %>%
  html_text(trim = TRUE)
ri_header <- ri_table %>% html_elements("th") %>% html_text(trim = T)
ri_header_amount <- as.numeric(length(ri_header))
ri_unlist <- unlist(ri_table_data)
Risø <- as.data.frame(matrix(data = ri_unlist, ncol = ri_header_amount, byrow = T))
colnames(Risø) <- ri_header
Risø[, 2:ri_header_amount] <- lapply(Risø[, 2:ri_header_amount], function(x) as.numeric(gsub(",", ".", x))) 
ri_RDSname <- paste0("Risø_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
saveRDS(Risø,ri_RDSname)

##### Anholt #####
an_link <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO"
anrawres <- GET(url = an_link, add_headers(`User Agent`= UserA))
print(anrawres$status_code)
an_content <- httr::content(anrawres,as = "text", encoding = "UTF-8")
an_js <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/ANHO"
an_token <- read_html(an_content) %>% html_element("input[name=__RequestVerificationToken]") %>% html_attr("value")
an_post <- POST(
  url = an_js,
  add_headers(`User Agent`= UserA),
  body = list(`__RequestVerificationToken` = token))

an_table_html <- content(an_post, as = "text", encoding = "UTF-8")
an_table <- read_html(an_table_html)

an_rows <- an_table %>% html_elements("tr")
an_table_data <- an_rows %>% html_elements("td") %>% html_text(trim = T)
an_header <- an_table %>% html_elements("th") %>% html_text(trim = T)
an_header_amount <- as.numeric(length(an_header))
an_unlist <- unlist(an_table_data)
Anholt <- as.data.frame(matrix(data = an_unlist, ncol = an_header_amount, byrow = T))
colnames(Anholt) <- an_header
Anholt[,2:an_header_amount] <- lapply(Anholt[,2:an_header_amount], function(x) as.numeric(gsub(",",".",x)))
Anholt_RDSname <- paste0("Anholt_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
saveRDS(Anholt,Anholt_RDSname)


##### Hc Andersens Boulevard # <- ####
hc_link = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB"
hcrawres <- GET(url = hc_link, add_headers(`User Agent`=UserA))
print(hcrawres$status_code)
hc_contant <- httr::content(hcrawres, as = "text", encoding = "UTF-8")
hc_js <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"
hc_token <- read_html(hc_contant) %>% html_element("input[name=__RequestVerificationToken]") %>% html_attr("value")
hc_post <- POST(
  url = hc_js,
  add_headers(`User Agent`= UserA),
  body = list(`__RequestVerificationToken` = token))

hc_table_html <- content(hc_post, as = "text", encoding = "UTF-8")
hc_table <- read_html(hc_table_html)

hc_rows <- hc_table %>% html_elements("tr")
hc_table_data <- hc_rows %>% html_elements("td") %>% html_text(trim = T)
hc_header <- hc_table %>% html_elements("th") %>% html_text(trim = T)
hc_header_amount <- as.numeric(length(hc_header))
hc_unlist <- unlist(hc_table_data)
HC_Andersens_Boulevard <- as.data.frame(matrix(data = hc_unlist,ncol = hc_header_amount,byrow = T))
colnames(HC_Andersens_Boulevard) <- hc_header
HC_Andersens_Boulevard[,2:hc_header_amount] <- lapply(HC_Andersens_Boulevard[,2:hc_header_amount], function(x) as.numeric(gsub(",",".",x)))
HC_Andersens_Boulevard_RDSname <- paste0("HC_Andersens_Boulevard_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
saveRDS(HC_Andersens_Boulevard,HC_Andersens_Boulevard_RDSname)







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


library(httr)
library(rvest)

# URL for tabellen, som indlæses i JS
# https://imgur.com/a/IXWlTUE URL
# https://imgur.com/a/4lrwu4F RESPONSE
js_url <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"

# Finder den specifikke __RequestVerificationToken, som skal bruges for at godkende vores POST (indlæsning af tabel)
token <- read_html(rawcontent) %>%
  html_element("input[name='__RequestVerificationToken']") %>% 
  html_attr("value") # Trækker bare værdierne ud

# Make the AJAX POST request
jsPOST <- POST(
  url = js_url,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(`__RequestVerificationToken` = token),
  encode = "form"
)

# Parse the response content
table_html <- content(jsPOST, as = "text", encoding = "UTF-8")
table_page <- read_html(table_html)

# Extract the table data
rows <- table_page %>% html_elements("tr")
table_data <- rows %>%
        html_elements("td") %>%
        html_text(trim = TRUE)
unlist <- unlist(table_data)
Aarhus <- as.data.frame(matrix(data = unlist, ncol = 5, byrow = T)


## Combine the scraped data into a single vector
#flat_data <- unlist(table_data)
#
## Convert the flat list into a matrix with 5 columns (one per table column)
#table_matrix <- matrix(flat_data, ncol = 5, byrow = TRUE)
#
## Convert the matrix into a data frame and add column names
#Aarhus <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
colnames(Aarhus) <- c("Målt (starttid)", "CO", "NO2", "NOx", "PM10")

# Hvis nødvendigt: Konvertér værdier til numerisk format (hvis CO, NO2 osv. har komma)
Aarhus[, 2:5] <- lapply(Aarhus[, 2:5], function(x) as.numeric(gsub(",", ".", x)))


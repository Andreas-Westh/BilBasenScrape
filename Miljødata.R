library(httr)
library(rvest)

# Lav et start output som viser mulige stationer

args = commandArgs(trailingOnly=TRUE)
operator=args[1]

df=NULL
UserA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
operator_link <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/Rural/",operator)
link <- operator_link
rawres <- GET(url = link, add_headers(`User Agent`= UserA))
print(rawres$status_code)
content <- httr::content(rawres,as = "text", encoding = "UTF-8")
operator_js <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/",operator)
js <- operator_js

token <- read_html(content) %>% html_element("input[name=__RequestVerificationToken]") %>% html_attr("value")
post <- POST(
  url = js,
  add_headers(`User Agent`= UserA),
  body = list(`__RequestVerificationToken` = token))

table_html <- content(post, as = "text", encoding = "UTF-8")
table <- read_html(table_html)

rows <- table %>% html_elements("tr")
table_data <- rows %>% html_elements("td") %>% html_text(trim = T)
header <- table %>% html_elements("th") %>% html_text(trim = T)
header_amount <- as.numeric(length(header))
unlist <- unlist(table_data)
df <- as.data.frame(matrix(data = unlist, ncol = header_amount, byrow = T))
colnames(df) <- header
df[,2:header_amount] <- lapply(df[,2:header_amount], function(x) as.numeric(gsub(",",".",x)))
RDSname <- paste0(operator,"_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".rds")
saveRDS(df,RDSname)

print(nrow(df))





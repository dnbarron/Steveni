library(pdftools)
library(tesseract)
library(tm)
library(stringr)
library(tidyverse)
library(rvest)

new1 <- read_html("https://en.wikipedia.org/wiki/Barbara_Steveni")
new1a <- html_text2(html_element(new1, 
              "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(5)"))
new1b <- html_text2(html_element(new1, 
              "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(8)"))
new1c <- html_text2(html_element(new1, 
               "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(9)"))
new1d <- html_text2(html_element(new1, 
           "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(11)"))
new1e <- html_text2(html_element(new1, 
           "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(12)"))
new1f <- html_text2(html_element(new1, 
              "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(13)"))
new1g <- html_text2(html_element(new1, 
           "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(14)"))
new1h <- html_text2(html_element(new1, 
       "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(15)"))
new1i <- html_text2(html_element(new1, 
        "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(16)"))
new1j <- html_text2(html_element(new1, 
          "#mw-content-text > div.mw-content-ltr.mw-parser-output > p:nth-child(18)"))

new1 <- c(new1a, new1b, new1c, new1d, new1e, new1f, new1g, new1h, new1i, new1j)

context1 <- read_html("https://en.contextishalfthework.net/exhibition-archive/exhibition-concept/")
context1 <- html_text2(html_element(context1, 
            "#content > section.page.page--page.page--content.width--half.alignment--right > div > p:nth-child(1)"))

context2 <- read_html("https://en.contextishalfthework.net/exhibition-archive/british-steel-corporation-1969-1971/")
context2 <- html_text2(html_element(context2, "#content > section.page.page--page.page--content.width--half.alignment--right > div"))

get_text <- function(url, selector){
  tmp <- read_html(url)
  html_text2(html_element(tmp, selector))
}

context3 <- get_text("https://en.contextishalfthework.net/exhibition-archive/scottish-television-1971/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context4 <- get_text("https://en.contextishalfthework.net/exhibition-archive/ocean-fleets-ltd-197475/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context5 <- get_text("https://en.contextishalfthework.net/exhibition-archive/department-of-the-environment-1975/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context6 <- get_text("https://en.contextishalfthework.net/exhibition-archive/scottish-office-1975-76/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context7 <- get_text("https://en.contextishalfthework.net/exhibition-archive/department-of-health-and-social-security-1976/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context8 <- get_text("https://en.contextishalfthework.net/exhibition-archive/department-of-health-and-social-security-1978-1979/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context9a <- get_text("https://en.contextishalfthework.net/exhibition-archive/apg-in-germany/",
                     "#content > section.page.page--page.page--content.width--half.alignment--right > div > p:nth-child(1)")
context9B <- get_text("https://en.contextishalfthework.net/exhibition-archive/apg-in-germany/",
                      "#content > section.page.page--page.page--content.width--half.alignment--right > div > p:nth-child(3)")
context9 <- c(context9a, context9B)

context10 <- get_text("https://en.contextishalfthework.net/about-apg/artist-placement-group/",
                      "#content > section.page.page--page.page--content.width--half.alignment--right > div")

context_pdf <- pdf_text("Report_Symposium_APG_Berlin2015_eng.pdf")

brisely1 <- get_text("https://www.stuartbrisley.com/pages/29/70s/Text/The_account_of_the_Hille_Project_by_Barbara_Steveni_APG_Administration/page:14",
                     "#content > p:nth-child(4)")
brisely1a <- get_text("https://www.stuartbrisley.com/pages/29/70s/Text/The_account_of_the_Hille_Project_by_Barbara_Steveni_APG_Administration/page:14",
                      "#content > p:nth-child(6)")
brisely1 <- c(brisely1, brisely1a)


all_new_text <- c(context1, context2, context3, context4, context5, context6, context7,
                  context8, context9, context10, context_pdf, new1, brisely1)

all_new_text <- removePunctuation(all_new_text, ucp = TRUE)
all_new_text <- str_replace_all(all_new_text, "\\n", " ")
all_new_text <- removeNumbers(all_new_text)
all_new_text <- str_to_lower(all_new_text)
all_new_text <- removeWords(all_new_text, stopwords("english"))
all_new_text <- stripWhitespace(all_new_text)
all_new_text <- str_replace_all(all_new_text, "stevenis", "steveni")
all_new_text <- str_replace_all(all_new_text, "apgs", "apg")
all_new_text <- str_replace_all(all_new_text, "lathams", "latham")
all_new_text <- str_replace_all(all_new_text, "barbaras", "barbara")
all_new_text <- str_replace_all(all_new_text, "\\+", " ")

token_new_text <- Boost_tokenizer(all_new_text)
write_lines(token_new_text, "Steveni2.txt")

token_text <- c(token_text, token_new_text)

library(ggwordcloud)
f <- termFreq(token_text)
fd <- as.data.frame(f)
ft <- rownames_to_column(fd, var = "term")
ft <- as_tibble(ft) 
ft %>% arrange(desc(f))
ft <- ft %>% mutate(freq = as.integer(f))
library(paletteer)
ft %>% filter(freq > 10) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
  ggplot(aes(label = term, size = freq, colour = freq)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 50, trans = power_trans(1/0.7)) +
  scale_colour_paletteer_c("viridis::plasma") +
  theme_minimal()

ggsave("wordcloud_colour3.png", device = "png", width = 200, height = 200, units = "mm",
       dpi = 'retina')

brisely2a <- get_text("https://www.stuartbrisley.com/pages/29/70s/Text/Peterlee_Project_1976_77/page:1",
                      "#content > p:nth-child(5)")
brisely2b <- get_text("https://www.stuartbrisley.com/pages/29/70s/Text/Peterlee_Project_1976_77/page:1",
                      "#content > p:nth-child(6)")

peterlee <- pdf_text("PeterleeReport.pdf")


peterlee <- removePunctuation(peterlee, ucp = TRUE)
peterlee <- str_replace_all(peterlee, "\\n", " ")
peterlee <- str_to_lower(peterlee)
peterlee <- removeWords(peterlee, stopwords("english"))
peterlee <- stripWhitespace(peterlee)
peterlee <- str_replace_all(peterlee, "stevenis", "steveni")
peterlee <- str_replace_all(peterlee, "apgs", "apg")
peterlee <- str_replace_all(peterlee, "lathams", "latham")
peterlee <- str_replace_all(peterlee, "barbaras", "barbara")
peterlee <- str_replace_all(peterlee, "\\+", " ")
peterlee <- removeNumbers(peterlee)

token_peterlee <- Boost_tokenizer(peterlee)
write_lines(token_peterlee, "Steveni3.txt")

token_text <- read_lines("Steveni.txt")
token_text2 <- read_lines("Steveni2.txt")
token_text3 <- read_lines("Steveni3.txt")
token_text <- c(token_text, token_text2, token_text3)

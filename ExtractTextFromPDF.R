library(pdftools)
library(tesseract)
library(tm)
library(stringr)
library(tidyverse)
library(rvest)

text1 <- pdf_text("IndividualAndOrganisation.pdf")
text2 <- pdf_text("RateOfReturn.pdf")

eng <- tesseract("eng")
text3 <- ocr("Sachsse1.jpg", eng)
text4 <- ocr("Sachsse2.jpg", eng)
text5 <- ocr("Sachsse3.jpg", eng)
text6 <- ocr("Sachsse4.jpg", eng)
text7 <- ocr("Sachsse5.jpg", eng)


url <- "https://barbarasteveni.org/Work-BS-Barbara-Steveni"
page1 <- read_html(url)
text_page1 <- page1 %>% html_text2()
t <- str_split_1(text_page1, "\\n")
text8 <- t[6]

url <- "https://barbarasteveni.org/Work-ALJ-Art-Life-Journey"
page2 <- read_html(url)
text_page2 <- page2 %>% html_text2()
t2 <- str_split_1(text_page2, "\\n")
text9 <- t2[6]

url <- "https://barbarasteveni.org/Work-IAAA-I-Am-An-Archive"
page3 <- read_html(url)
text_page3 <- page3 %>% html_text2()
t3 <- str_split_1(text_page3, "\\n")
text10 <- t3[6]


extract_text <- function(url){
  webpage <- read_html(url)
  pagetext <- html_text2(webpage)
  splittext <- str_split_1(pagetext, "\\n")
  splittext[6]
}

text11 <- extract_text("https://barbarasteveni.org/Work-TBSA-The-Barbara-Steveni-Archive")
text12 <- extract_text("https://barbarasteveni.org/Work-APG-Artist-Placement-group")
text13 <- extract_text("https://barbarasteveni.org/Work-O-I-Organisation-Imagination")
text14 <- extract_text("https://barbarasteveni.org/Work-IU-Incidental-Unit")
text15 <- extract_text("https://barbarasteveni.org/Work-BH-Barbaras-Home")

text16 <- pdf_text("Exhibition-Notes-Barbara-Steveni.pdf")

lissom <- read_html("https://www.lissongallery.com/news/barbara-steveni-i-find-myself-featuring-laure-provost-at-modern-art-oxford-oxford-united-kingdom")

text17 <- html_text2(html_element(lissom, "body > main > div > div:nth-child(1) > p:nth-child(3)"))
text18 <- html_text2(html_element(lissom, "body > main > div > div:nth-child(1) > p:nth-child(4)"))

conversation <- read_html("https://theconversation.com/barbara-steveni-i-find-myself-a-pioneering-artist-who-influenced-the-civil-service-252761")
text19 <- html_element(conversation, "#article > div:nth-child(2) > div.grid-twelve.large-grid-eleven > div.grid-ten.large-grid-nine.grid-last.content-body.content.entry-content.instapaper_body")
text19 <- html_text2(text19)

monoskop <- read_html("https://monoskop.org/Barbara_Steveni")
  
text20 <- html_element(monoskop, "#mw-content-text > div")
text20 <- html_text2(text20)



all_text <- c(text1, text2, text3, text4, text5, text6, text7, text8, text9,
              text10, text11, text12, text13, text14, text15, text16, text17,
              text18, text19, text20)

all_text <- removePunctuation(all_text, ucp = TRUE)
all_text <- str_replace_all(all_text, "\\n", " ")
all_text <- removeNumbers(all_text)
all_text <- str_to_lower(all_text)
all_text <- removeWords(all_text, stopwords("english"))
all_text <- stripWhitespace(all_text)
all_text <- str_replace_all(all_text, "stevenis", "steveni")
all_text <- str_replace_all(all_text, "apgs", "apg")
all_text <- str_replace_all(all_text, "lathams", "latham")
all_text <- str_replace_all(all_text, "barbaras", "barbara")
all_text <- str_replace_all(all_text, "\\+", " ")

token_text <- Boost_tokenizer(all_text)

write_lines(token_text, "Steveni.txt")

token_text <- read_lines("Steveni.txt")

seconds <- read_html("http://www.slashseconds.org/issues/002/004/articles/bsteveni2/index.php")
text20 <- html_element(seconds, "body > div.article")
text20 <- html_text2(text20)


f <- termFreq(token_text)
fd <- as.data.frame(f)
ft <- rownames_to_column(fd, var = "term")
ft <- as_tibble(ft) 
ft %>% arrange(desc(f))

ft <- ft %>% mutate(freq = as.integer(f))
library(RColorBrewer)
wordcloud::wordcloud(token_text, min.freq = 10, max.words = 75, 
                     random.order = FALSE, rot.per = 0.35,
                     colors = brewer.pal(8, "Dark2"))

library(ggwordcloud)
library(paletteer)
ft %>% filter(freq > 10) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
  ggplot(aes(label = term, size = freq, colour = freq)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 50, trans = power_trans(1/0.7)) +
  scale_colour_paletteer_c("viridis::plasma") +
  theme_minimal()

ggsave("wordcloud_colour.png", device = "png", width = 200, height = 200, units = "mm",
       dpi = 'retina')
####


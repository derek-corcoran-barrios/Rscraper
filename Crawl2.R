if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, stringr, mailR, lubridate, tidytext, topicmodels)


page <- read_html("http://cran.r-project.org/web/packages/available_packages_by_name.html") %>% html_node("table") %>% html_table(fill = TRUE, header = FALSE)
page <- page[-1,] 
page <- page %>% rename(Package = X1, Description = X2)
page <- page[complete.cases(page),]
page$link <- NA
page$link <- read_html("http://cran.r-project.org/web/packages/available_packages_by_name.html") %>% html_nodes("td a") %>%  html_attr("href") 


page <- page %>% mutate(link = str_replace(link, "../../", "https://cran.r-project.org/")) %>% mutate(archive = paste0("https://cran.r-project.org/src/contrib/Archive/", Package,"/"))


page$author <- NA
page$First_date <- NA
for (i in 1:nrow(page)){
  page$author[i] <- (t(read_html(page$link[i]) %>% html_nodes("table:nth-child(3)") %>% html_table(trim = TRUE, fill= TRUE))[[1]] %>% filter(X1 == "Author:"))$X2
  message(paste(i, "of", nrow(page), "ready!!"))
  try({temp <- read_html(page$archive[i]) %>% html_nodes("td:nth-child(3)") %>% html_text(trim = TRUE)
  page$First_date[i] <- min(lubridate::ymd_hm(temp), na.rm = TRUE)})
  gc()
}


page$First_date <- as.POSIXct(page$First_date, origin='1970-01-01')

saveRDS(page, "pageDate.rds")

pageDate <- readRDS("pageDate.rds")

PagebyDate2 <- read_html("https://cran.r-project.org/web/packages/available_packages_by_date.html") %>% html_node("table") %>% html_table(fill = TRUE, header = TRUE) %>% mutate(Date = ymd(Date)) %>% dplyr::select(-Title)

pageDate2 <- pageDate %>% filter(is.na(First_date)) %>% mutate(First_date=as.Date(First_date)) %>%  left_join(PagebyDate2) %>% mutate(First_date = Date) %>% dplyr::select(-Date)
pageDate <- pageDate %>% filter(!is.na(First_date)) %>% mutate(First_date=as.Date(First_date)) %>%  bind_rows(pageDate2) %>% arrange(First_date)



saveRDS(pageDate, "pageDateComplete.rds")

####Topic Modeling
data("stop_words")
Idea <- pageDate %>% unnest_tokens(word, Description) %>% anti_join(stop_words) %>% count(word, sort = TRUE)

###Graph
pageDate %>% unnest_tokens(word, Description) %>% anti_join(stop_words) %>% count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

###Bigram
Idea2 <- pageDate %>% unnest_tokens(ngram, Description, token = "ngrams", n = 2) %>% count(ngram, sort = TRUE)
Idea2 %>%
  filter(n > 150) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


Idea3 <- pageDate %>% unnest_tokens(ngram, Description, token = "ngrams", n = 3) %>% count(ngram, sort = TRUE)

Idea3 %>%
  filter(n > 25 & !is.na(ngram)) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(ngram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


DTM <-pageDate %>% group_by(Package) %>% unnest_tokens(word, Description) %>% anti_join(stop_words) %>% count(word, sort = TRUE) %>% cast_dtm(Package, word, n)


R_lda <- LDA(DTM, k = 4, control = list(seed = 1234))
R_topics <- tidy(R_lda, matrix = "beta")

R_top_terms <- R_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

R_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


R_documents <- tidy(R_lda, matrix = "gamma")
View(arrange(R_documents, desc(topic), desc(gamma)))
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, stringr, mailR)


page <- read_html("http://cran.r-project.org/web/packages/available_packages_by_name.html") %>% html_node("table") %>% html_table(fill = TRUE, header = FALSE)
page <- page[-1,] 
page <- page %>% rename(Package = X1, Description = X2)
page <- page[complete.cases(page),]
page$link <- NA
page$link <- read_html("http://cran.r-project.org/web/packages/available_packages_by_name.html") %>% html_nodes("td a") %>%  html_attr("href") 

page <- page %>% mutate(link = str_replace(link, "../../", "https://cran.r-project.org/"))

CranCrawl <-  function(Link, package){
  temp <- read_html(Link) %>% html_node("table:nth-child(3)") %>% html_table(fill = TRUE, header = FALSE) %>% filter(X1 == "Depends:" | X1 == "Imports:") %>% select(X2) %>% rename(Depends = X2) %>% mutate(Depends = str_remove_all(Depends, "\\s*\\([^\\)]+\\)"))
  Package <- data.frame(Depends = unlist(str_split(temp$Depends, ",")), Package = package)
  colnames(Package) <- c("Depends", "Package")
  return(Package)
}

NetworkCran <- list()
Empty <- vector()
for(i in 1:nrow(page)){
  gc()
  NetworkCran[[i]] <- try(CranCrawl(Link = page$link[i], package = page$Package[i]))
  if(class(NetworkCran[[i]]) == "try-error"){
    Empty <- append(Empty, page$Package[i])
  }
  message(paste(i, "of", nrow(page), "ready!!"))
  gc()
}
NetworkCran <- NetworkCran[lapply(NetworkCran, class) != "try-error"]
NetworkCran <-  do.call(rbind, NetworkCran) %>% filter(Depends != "R") %>% select(Package, Depends)

saveRDS(NetworkCran, paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"NetworkCran", ".rds"))

saveRDS(page, paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"page", ".rds"))


saveRDS(Empty, paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"Empty", ".rds"))


send.mail(from = "derek.corcoran.barrios@gmail.com", to = "derek.corcoran.barrios@gmail.com", subject = paste0("AWSCranDB", format(Sys.time(), "%Y_%m_%d")), body =  paste0("CranDB", format(Sys.time(), "%Y_%m_%d")),  attach.files = c(paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"Empty", ".rds"),paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"page", ".rds"),paste0("/home/rstudio/Crawl/", format(Sys.time(), "%Y_%m_%d"),"NetworkCran", ".rds")), smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.corcoran.barrios", passwd = "duo5241K+", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

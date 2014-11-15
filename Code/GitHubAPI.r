library(rjson)
library(RCurl)
library(plyr)

# Add proxy server address here if nececssary
proxy.server=""
opts <- c(useragent="RCurl", proxy=proxy.server)

# Basic query stack targeting JSON data
github.api <- function(query, acceptHeaders=""){
  custom.header <- c(Accept=acceptHeaders)
  json <- getURL(query, httpheader=custom.header, ssl.verifyPeer=FALSE, .opts=opts)
  fromJSON(json)
}

# First 100 R-language Projects URL
github.search <- "https://api.github.com/search/repositories?q=language:R&page=1&per_page=100"

# Generic Query URL Template
github.template <- "https://api.github.com/search/repositories?q=language:%s&page=%d&per_page=%d"

# Generate URL from Parameters
github.api.search <- function(language="R", page=1, per_page=100) {
  sprintf(github.template, language, page, per_page)
}

# Function to execute query and return the page of results
page.data <- function(query=github.api.search()) {
  repos <- github.api(query, "application/vnd.github.preview")
  
  repoRowMin <- function(repo) {   data.frame(repo$name, repo$owner$login, as.character(as.Date(repo$created_at, "%Y-%m-%d")), as.character(as.Date(repo$pushed_at, "%Y-%m-%d")), repo$watchers, repo$forks) }
  
  page <- do.call('rbind', lapply(1:length(repos$items), function(i) repoRowMin(repos$items[[i]])))
  colnames(page) <- c("Name", "Owner", "Created", "Pushed", "Watchers", "Forks")
  mutate(page, Collected = as.character(Sys.Date()))
  page
}

# Pull 10 pages from API with 60 second delay to accomodate API rules
repos.r <- function(){
  repos <- data.frame(Name=character(),
                      Owner=character(),
                      Created=character(),
                      Pushed=character(),
                      Watchers=integer(),
                      Forks=integer(),
                      Collected=character())
  
  repos <- rbind(repos, do.call('rbind', lapply(1:5, function(i){
    page.data(github.api.search(page=i))
  })))
  Sys.sleep(60)
  repos <- rbind(repos, do.call('rbind', lapply(6:10, function(i){
    page.data(github.api.search(page=i))
  })))
  repos
}

parseName <- function(packageEntry) {   
  start <- regexpr("index.html", packageEntry)   
  end <- regexpr("</a", packageEntry)
  substr(packageEntry, start+12, end-1) 
}

# Read CRAN package directory
cran.packages <- "http://cran.us.r-project.org/web/packages/available_packages_by_date.html"
packageEntries <- readLines(cran.packages)

# Web scrape package names and dates, convert to data frame
tagSignature <- "<a href=\"../../web/packages/"
packageEntries <- packageEntries[grep(tagSignature, packageEntries)]
dates <- substr(packageEntries, 11, 20)
names <- sapply(1:length(packageEntries), function(i) parseName(packageEntries[i]))
cran <- data.frame(dates, names)

if (data.live) {
  
  # Read data from GitHub API
  github <- repos.r()
  
} else {
  
  # Read previously created data set hosted on GitHub
  url.data <- "https://raw.githubusercontent.com/patricktoohey/GitHubR/master/Data//repos.csv"
  github.history <- read.csv(text = getURL(url.data, ssl.verifypeer = FALSE, .opts=opts))
  
  github <- transform(github.history, Collected = as.Date(Collected, "%Y-%m-%d"))
  github <- subset(github, Collected == as.Date("2014-03-01", "%Y-%m-%d"))
  github <- transform(github, Collected = as.character(Collected))
  github <- transform(github, Created = as.character(as.Date(Created, "%Y-%m-%d")))
  github <- transform(github, Pushed = as.character(as.Date(Pushed, "%Y-%m-%d")))
}

# Filter data set
github <- mutate(github, CRAN = cran$dates[match(Name, cran$names)])
github.cran <- na.omit(github)

cran.authors <- unique(github.cran$Owner)
future.cran <- subset(github, is.na(CRAN) & Owner %in% cran.authors)
future.cran <- arrange(future.cran, desc(as.Date(Created)))
future.cran <- subset(future.cran, select = -c(CRAN))
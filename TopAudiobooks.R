library(xml2)
library(rvest)

pages <- 5
items <- 20

cols <- c('Rank', 'Title', 'Subtitle', 'Author', 'Narrator', 'Length', 'Release', 'Language', 'Stars', 'Ratings', 'Price', 'URL')
data <- matrix('', nrow = pages * items, ncol = length(cols))
colnames(data) <- cols

# Loop through the pages
for (page in 1:pages) {
  url <- paste0('https://www.audible.com/adblbestsellers?page=', page)
  html <- read_html(url)
  
  # Loop through the items on a page
  for (item_num in 1:items) {
    row <- (page - 1) * items + item_num
    
    item_selector <- paste0('#product-list-a11y-skiplink-target > span > ul > div > li:nth-child(', item_num, ') > div > div.bc-col-responsive.bc-spacing-top-none.bc-col-8 > div > div.bc-col-responsive.bc-col-6 > div > div > span > ul')
    item_node <- html_node(html, item_selector)
    
    # The audiobook ranking
    data[row, 'Rank'] <- row
    
    # The audiobook title
    title_selector <- 'li:nth-child(1) > h3 > a'
    title_node <- html_node(item_node, title_selector)
    title <- html_text(title_node, trim = TRUE)
    data[row, 'Title'] <- title
    
    # The audiobook subtitle. It's empty for some items.
    subtitle_selector <- 'li.bc-list-item.subtitle > span'
    subtitle_node <- html_node(item_node, subtitle_selector)
    subtitle <- html_text(subtitle_node, trim = TRUE)
    data[row, 'Subtitle'] <- subtitle
    
    # The author. There might be more than one.
    author_selector <- 'li.bc-list-item.authorLabel > span > a'
    author_nodes <- html_nodes(item_node, author_selector)
    authors <- html_text(author_nodes, trim = TRUE)
    author <- paste(authors, collapse = ', ')
    data[row, 'Author'] <- author
    
    # The narrator. There might be more than one.
    narrator_selector <- 'li.bc-list-item.narratorLabel > span > a'
    narrator_nodes <- html_nodes(item_node, narrator_selector)
    narrators <- html_text(narrator_nodes, trim = TRUE)
    narrator <- paste(narrators, collapse = ', ')
    data[row, 'Narrator'] <- narrator
    
    # The audiobook length in hours and minutes
    length_selector <- 'li.bc-list-item.runtimeLabel > span'
    length_node <- html_node(item_node, length_selector)
    length <- html_text(length_node, trim = TRUE)
    length <- gsub('Length: ', '', length)
    data[row, 'Length'] <- length
    
    # The release date    
    release_selector <- 'li.bc-list-item.releaseDateLabel > span'
    release_node <- html_node(item_node, release_selector)
    release <- html_text(release_node, trim = TRUE)
    release <- gsub('Release date:\n\\s+', '', release)
    data[row, 'Release'] <- release
    
    # The audiobook language
    language_selector <- 'li.bc-list-item.languageLabel > span'
    language_node <- html_node(item_node, language_selector)
    language <- html_text(language_node, trim = TRUE)
    language <- gsub('Language:\n\\s+', '', language)
    data[row, 'Language'] <- language
    
    # The number of stars received
    stars_selector <- 'li.bc-list-item.ratingsLabel > span.bc-text.bc-pub-offscreen'
    stars_node <- html_node(item_node, stars_selector)
    stars <- html_text(stars_node, trim = TRUE)
    data[row, 'Stars'] <- stars
    
    # The number of ratings received
    ratings_selector <- 'li.bc-list-item.ratingsLabel > span.bc-text.bc-size-small.bc-color-secondary'
    ratings_node <- html_node(item_node, ratings_selector)
    ratings <- html_text(ratings_node, trim = TRUE)
    data[row, 'Ratings'] <- ratings
    
    # The selling price
    price_selector <- paste0('#buybox-regular-price-', item_num - 1, ' > span:nth-child(2)')
    price_node <- html_node(html, price_selector)
    price <- html_text(price_node, trim = TRUE)
    data[row, 'Price'] <- price
    
    # Web page address
    url <- paste0('https://www.audible.com', html_attr(title_node, 'href'))
    data[row, 'URL'] <- url
  }
}

df <- as.data.frame(data)
head(df, 10)

View(df)
df[ , c('Rank', 'Title')]

filename <- paste0('TopAudiobooks-', format(Sys.time(), '%Y%m%d-%H%M%S'), '.csv')
write.csv(df, filename, row.names = FALSE)

library(shiny)
library(tidyverse)
library(curl)
library(jsonlite)
#Last.FM
last.fm.api.key = "8ffea6fc9001df75c1e3abff811a1bac"

last_fm_endpoint <- paste0("http://ws.audioscrobbler.com/2_0/?method=chart.gettoptracks&api_key=", last.fm.api.key, "&format=json")
json_results_last_fm <- last_fm_endpoint %>% curl() %>% readLines()

### Song-names
songs_last_fm <- fromJSON(json_results_last_fm)$tracks$track$name
###Artists-names
artists_last_fm <- fromJSON(json_results_last_fm)$tracks$track$artist$name

###Concatenating title and artist for search
search_list_last_fm <- list() #Initialize empty list
for (i in 1:length(songs_last_fm)){
  search_list_last_fm[i] <- paste0(songs_last_fm[i], ", ", artists_last_fm[i])
}
# a = mapply(c, songs_last_fm, artists_last_fm, SIMPLIFY=FALSE)
search_list_last_fm[1:5]

#---------x-----------------x-------------------x-------------

#Youtube search API

youtube.api.key = "AIzaSyAa60io3BZgOJjzxU6W-hZeH1YAV3qVQbM"
search_query = search_list_last_fm[5]
get_youtube_videoID <- function(query) {
  youtube_search_endpoint = paste0("https://www.googleapis.com/youtube/v3/search?q=",
                                   curl_escape(query), "&maxResults=1&part=snippet&key=", youtube.api.key)
  json_results_youtube = youtube_search_endpoint %>% curl() %>% readLines()
  
  youtube_videoID = fromJSON(json_results_youtube)$items$id$videoId
  youtube_videoID
}
get_youtube_videoID(search_query[3])

#---------x-----------------x-------------------x-------------
#iTunes charts

itunes_charts_url <- "https://www.apple.com/itunes/charts/songs/"
itunes_charts_page = read_html(itunes_charts_url)

intermediate <- itunes_charts_page %>% 
  html_nodes(".chart-grid a:nth-child(1)") %>%
  html_text()

search_list_itunes <- list() #Initialize empty list
for (i in 1:length(intermediate)/2){
  j = i*2
  search_list_itunes[i] <- paste0(intermediate[j-2], ", ", intermediate[j-1])
}

#---------x-----------------x-------------------x-------------
# App code starts here:

ui <- shinyUI(navbarPage("Top Music Charts",
            tabPanel("Last.FM",                      
             sidebarLayout(
               sidebarPanel(
                 selectInput("videoNameLastFM", "Select your choice",
                             paste(c(1:100), sep = ". ", unlist(search_list_last_fm))[1:20])
                 ),
               uiOutput("video_lastfm")
             )
    ),
            
            tabPanel("iTunes", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("videoNameITunes", "Select your choice",
                             paste(c(1:100), sep = ". ", unlist(search_list_itunes))[1:20])
               ),
               uiOutput("video_itunes")
             )
    )
            
))

server <- function(input, output) {  

  choiceLastFM <- reactive({
    str_replace(input$videoNameLastFM, pattern = "(\\d+. )", "")
    })
  
  output$video_lastfm <- renderUI({
    if(!is.null(choiceLastFM())){
      HTML(paste0('<iframe width="500" height="300" src="https://www.youtube.com/embed/',
                  get_youtube_videoID(choiceLastFM()) ,'" frameborder="0" allowfullscreen></iframe>'))
    }
  })

  
  choiceITunes <- reactive({
    str_replace(input$videoNameITunes, pattern = "(\\d+. )", "")
  })
  
  output$video_itunes <- renderUI({
    if(!is.null(choiceITunes())){
      HTML(paste0('<iframe width="500" height="300" src="https://www.youtube.com/embed/',
                  get_youtube_videoID(choiceITunes()) ,'" frameborder="0" allowfullscreen></iframe>'))
    }
  })
  
  }


shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(flexdashboard)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(janitor)
library(shiny)
library(ggplot2)
library(maps)
library(maptools)
library(gpclib)
library(stringi)
library(sp)


us_tweets <- read_csv("us_tweets.csv") 

us_tweets$tweet_content_stripped <- gsub("[^[:alpha:] ]", "",
                                         us_tweets$tweet_content) 

us_tweets$tweet_content_stripped <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ",
                                         us_tweets$tweet_content_stripped) 


#getting state from latitude longitude data
state_tweets = us_tweets %>%
  select("longitude", "latitude")



latlong2state <- function(state_tweets) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  states_tweets_SP <- SpatialPoints(state_tweets, 
                                    proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  indices <- over(states_tweets_SP, states_sp)
  
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


us_tweets$state_name <- stri_trans_totitle(latlong2state(state_tweets))



#PARSING TO DECREASE STATE MISSINGNESS
us_tweets = us_tweets %>%
  filter(country == "US") %>%
  mutate(state = str_sub(place_as_appears_on_bio, start = -2), state = trimws(state)) 

#formatting place that was state, USA
state_fix = us_tweets %>%
  filter(state == "SA") %>%
  mutate(fixed_state = NA)


list = strsplit(state_fix$place_as_appears_on_bio, split=',', fixed=TRUE)
df <- plyr::ldply(list)
colnames(df) <- c("State", "USA")

#fixing state name to be formatted correctly
state_fix = state_fix %>%
  mutate(state = trimws(state), fixed_state = df$State, fixed_state = trimws(fixed_state)) %>%
  mutate(fixed_state = state.abb[match(fixed_state,state.name)]) %>%
  mutate(fixed_state = state.name[match(fixed_state,state.abb)])

#add district of columbia
state_fix$fixed_state[is.na(state_fix$fixed_state)] <- "District of Columbia"

#fix SA of USA to be NA
us_tweets$state[us_tweets$state == "SA"] <- NA

#join datasets and keep most comprehensive
us_tweets = full_join(us_tweets, state_fix)%>%
  mutate(updated_state = ifelse(is.na(state), fixed_state, state)) %>%
  mutate(updated_state = state.name[match(updated_state,state.abb)]) %>%
  mutate(updated2_state = ifelse(is.na(fixed_state), updated_state,
                                 fixed_state)) %>%
  select(-state, -fixed_state, -updated_state) %>%
  mutate(final_state = ifelse(is.na(state_name), updated2_state,
                              state_name)) %>%
  select(-state_name, -updated2_state)



#tidy: long form
us_tweets_long <- gather(us_tweets, key = sentiment, value = count, anger:trust, factor_key = TRUE)


#dataset to plot
tweets_shiny = us_tweets_long %>%
  filter(country == "US") %>%
  drop_na(final_state)


which_state = tweets_shiny %>% distinct(final_state) %>% 
  pull() %>% as.character() %>% sort()



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   fluidRow(sidebarLayout(sidebarPanel(
        selectInput("state_choice1", label = h3("Select first state"),
                    choices = which_state, selected = "New York")
        ,
        
        selectInput("state_choice2", label = h3("Select second state"),
                  choices = which_state, selected = "New Jersey")
   )
   ,
   # Application title
   titlePanel("State Comparisons")),
   
      # Show a plot of the generated distribution
      fluidRow(splitLayout(
         plotlyOutput("state1"), plotlyOutput("state1bar")
      )),
   fluidRow(splitLayout(

    splitLayout(plotlyOutput("state2"), plotlyOutput("state2bar")
        )
   )
    
)
)
)






server <- function(input, output) {
   
   output$state1 <- renderPlotly({
     tweets_shiny %>%
       filter(count > 0, final_state == input$state_choice1)%>%
       mutate(text_label = str_c("sentiment: ", sentiment, '\nlocation: ', place_as_appears_on_bio)) %>% 
       plot_ly(x = ~longitude, y = ~latitude, type = "scatter", mode = "markers",
               alpha = 0.5, 
               color = ~sentiment, text = ~text_label)
   })
   output$state2 <- renderPlotly({
     tweets_shiny %>%
       filter(count > 0, final_state == input$state_choice2)%>%
       mutate(text_label = str_c("sentiment: ", sentiment, '\nlocation: ', place_as_appears_on_bio)) %>% 
       plot_ly(x = ~longitude, y = ~latitude, type = "scatter", mode = "markers",
               alpha = 0.5, 
               color = ~sentiment, text = ~text_label)
   })
   output$state1bar <- renderPlotly({
     tweets_shiny %>%
       filter(final_state == input$state_choice1)%>%
       group_by(sentiment) %>%
       summarize(sent_count = sum(count)) %>%
       mutate(sentiment = as.factor(sentiment), sentiment = fct_reorder(sentiment, sent_count, .desc = TRUE))%>%
       arrange(sent_count) %>%
       top_n(15) %>%
       plot_ly(x = ~sentiment, y = ~sent_count, type = "bar", color = ~sentiment)
   })
   output$state2bar <-  renderPlotly({
     tweets_shiny %>%
       filter(final_state == input$state_choice2)%>%
       group_by(sentiment) %>%
       summarize(sent_count = sum(count)) %>%
       mutate(sentiment = as.factor(sentiment), sentiment = fct_reorder(sentiment, sent_count, .desc = TRUE))%>%
       arrange(sent_count) %>%
       top_n(15) %>%
       plot_ly(x = ~sentiment, y = ~sent_count, type = "bar", color = ~sentiment)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


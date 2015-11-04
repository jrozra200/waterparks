## LOAD THE NECCESSARY LIBRARIES ##
library(httr)           ## TOOLS FOR WORKING WITH HTTP
library(XML)            ## TOOLS FOR WORKING WITH XML DATA
library(plotly)         ## VISUALIZATION TOOLS

## CREDENTIALS FOR PLOTLY ##
Sys.setenv("plotly_username"="YOUR_USER_NAME")
Sys.setenv("plotly_api_key"="YOUR_API_KEY")

## LIST OF STATES TO BUILD THE SEARCHES BELOW ## 
list_of_states <- read.csv("list_of_states.csv", header = FALSE)      

## EMPTY DATA FRAME TO GATHER RESULTS ##
data <- data.frame()
counter <- 1            ## WILL BE THE ROW NUMBER IN THE DATA FRAME

## ITERATE OVER ALL 50 STATES (AND THE DISTRICT) ##
for(j in 1:51){
        ## CREATE THE SEARCH STRING WITH STATE NAME ##
        q1 <- paste("waterparks in ", list_of_states[j,1], sep = "")
        
        ## QUERY GOOGLE ##
        response <- GET("https://maps.googleapis.com/",
                path = "maps/api/place/textsearch/xml",
                query = list(query = q1, key = "YOUR_API_KEY"))
        
        ## PARSE THE XML INTO A LIST OF RESULTS ##
        result <- xmlParse(response)
        result1 <- xmlRoot(result)
        result2 <- getNodeSet(result1, "//result")
        
        ## FOR EACH RESULT, GET THE INTERESTING DATA INTO A DATA FRAME ##
        for(i in 1:length(result2)){
                data[counter,1] <- xmlValue(result2[[i]][["name"]])  ## NAME DATA
                data[counter,2] <- xmlValue(result2[[i]][["formatted_address"]]) ## ADDRESS DATA
                data[counter,3] <- xmlValue(result2[[i]][["geometry"]][["location"]][["lat"]]) ## LATITUDE
                data[counter,4] <- xmlValue(result2[[i]][["geometry"]][["location"]][["lng"]]) ## LONGITUDE
                data[counter,5] <- xmlValue(result2[[i]][["rating"]])
                
                ## INCREASE THE ROW COUNTER ##
                counter <- counter + 1
        }
}

## ASSIGN NAMES TO THE DATA FRAME ##
names(data) <- c("Name", "Address", "Latitude", "Longitude", "Rating")

## CHANGE THE Ratings WITH NO VALUES TO 0s AND THE FACTORS TO NUMERIC ##
data$Rating[is.na(data$Rating)] <- 0
data$Rating <- as.numeric(data$Rating)

## ADD THE DATE TO THE NAME FOR HOVERING IN THE VISUALIZATION ##
data$hover <- with(data, paste(Name, " Rating: ", Rating))

## STYLING FOR THE COLOR BAR IN THE VIZ ##
m <- list(colorbar = list(title = "Waterpark Rating"))

## PULLED THIS DIRECTLY FROM THE PLOTLY WEBSITE ##
g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray95"),
        subunitcolor = toRGB("gray85"),
        countrycolor = toRGB("gray85"),
        countrywidth = 0.5,
        subunitwidth = 0.5
)

## CREATES THE MAP ##
plot_ly(data, lat = Latitude, lon = Longitude, text = hover, color = Rating,
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers',
        marker = m) %>%
        layout(title = 'Waterparks in the US', geo = g)
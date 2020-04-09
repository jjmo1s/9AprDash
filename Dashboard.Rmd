---
title: "Covid-19 Dashboard"
author: "Joey"
date: "April 8, 2020"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: scroll
---
<style>                     
.navbar {
  background-color:#737373;
  border-color:white;
}
.navbar-brand {
color:white!important;
}
</style>    

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
#Load packages
library(flexdashboard)
library(utils)
library(httr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(plotly)
library(extrafont)
library(extrafontdb)

### ggplot2 theme ###


theme_black = function(base_size = 12, base_family = "sans") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 0.4, color = "#737373"),
      axis.text.x = element_text(size = base_size,  family = "sans", color = "dark grey", lineheight = 0.9, angle = 50, hjust=1, vjust = 1),  
      axis.text.y = element_text(size = base_size, color = "dark gray", lineheight = 0.9, family = "sans"),  
      axis.ticks = element_line(color = "#737373", size  =  0.2),  
      axis.title.x = element_text(size = base_size*1.6, family = "sans",  color = "#737373", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size*1.4, family = "sans",color = "#737373", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "#283747"),  
      legend.key = element_rect(color = "#23074E",  fill = "#23074E"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white", family = "sans"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "#283747"),  
      legend.position = "right",
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "#283747", color  = "NA"),
      panel.border = element_rect(fill = NA, color = "NA"),  
      panel.grid.major = element_line(linetype = "dotted", color = "#260033"),  
      panel.grid.minor = element_blank(),  
      panel.spacing = unit(0.5, "lines"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "#283747"),  
      plot.title = element_text(size = base_size*1.6,family = "sans", color = "dark gray", vjust = 2),
      plot.margin = unit(rep(1, 4), "lines")
    )
}





### US Data ###


#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into "R". The dataset will be called "data".
data <- read.csv(tf)




#Filter on United States, starting from March
US <- filter(data, countriesAndTerritories == 'United_States_of_America', month >= 3 & month < 6)

#Reformat date 
US2 <- mutate(US, dateRep = as.Date(dateRep, format = "%d/%m/%Y"))

## Attempt at plotting the full cumulative sum of cases per day since february
## Just like they do in this link https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6

#Manipulate to show cumulative sums of cases by day 
US3 <- US2 %>% arrange((dateRep)) %>% mutate(cumcases = cumsum(cases)) 

# US3_log <- US3 %>% mutate(logcumcases = log(cumcases))

#Show cases for february and onward 
US4 <- US3 %>% filter(countriesAndTerritories == 'United_States_of_America', month >= 2 & month < 6) %>%
  mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y"))



### State Data ###

#Load the state data
state_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", header = TRUE)

#View total cases by state. must use 'max' function because case data is cumulative
cases_by_state <- state_raw %>% group_by(state) %>% summarise(state_cases = max(cases))

#Missouri total cases
MO_State <- cases_by_state %>% filter(state == 'Missouri')
#Virginia total cases
VA_State <-  cases_by_state %>% filter(state == 'Virginia')

### County Data ###

#Load county cases data
county_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", header = TRUE)

#Filter on MO
MO_county_cases <- county_raw %>% filter(state == 'Missouri') %>% group_by(county) %>% summarise(county_cases = max(cases))

#Filter on data for counties surrounding ironton
surround_MO_county_cases <- MO_county_cases %>% filter(county == 'Franklin' | county == 'Madison'| county == 'Reynolds' | county == 'St. Francois' | county == 'Ste. Genevieve'  | county == 'Henry') %>% arrange(desc(county_cases))


#Filter on VA
VA_county_cases <- county_raw %>% filter(state == 'Virginia') %>% group_by(county) %>% summarise(county_cases = max(cases))

#Filter on data for counties surrounding ironton
surround_VA_county_cases <- VA_county_cases %>% filter(county == 'Hampton city' | county == 'Newport News city'
                                                       | county == 'Norfolk city' | county == 'Williamsburg city' | county == 'York' | county == 'Gloucester' | county == 'Poquoson city')

surround_VA_county_cases$county[surround_VA_county_cases$county == "Hampton city"] <-  "Hampton"
surround_VA_county_cases$county[surround_VA_county_cases$county == "Norfolk city"] <-  "Norfolk"
surround_VA_county_cases$county[surround_VA_county_cases$county == "Williamsburg city"] <-  "Williamsburg"
```




Row
-------------------------------------

###ValueBox1 {data-height=100}

``` {r}
valueBox(value = sum(US$cases),icon = "fa-user-plus",caption = "Total U.S. Cases",color = "#283747")
```




###Value Box 2 {data-height=100}

``` {r}
valueBox(value = MO_State[1,2],icon = "fa-user-plus",caption = "MO Cases",color = "#283747")
```


###Value box 3 {data-height=100}
``` {r}
valueBox(value = VA_State[1,2],icon = "fa-user-plus",caption = "VA Cases",color = "#283747")
```



Row
-------------------------------------

###Daily U.S. Cases {.no-padding}

``` {r}
#Plot of US cases by day
p <- ggplot(US2, aes(x = dateRep, y = cases, group = 1), stat="identity") + 
  geom_line(color = "steelblue") + 
  scale_x_date(breaks = US2$dateRep[seq(1, length(US2$dateRep), by = 5)], date_labels = "%b %d") +
  geom_point(color = "steelblue", size = 2.3) +
  theme_black() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 


print(p)
```




###Total U.S. Cases {.no-padding}

``` {r}
p2 <- ggplot(US3, aes(x = dateRep, y = cumcases, group = 1), stat="identity") +
  geom_line(color="steelblue") +
  scale_x_date(breaks = US4$dateRep[seq(1, length(US4$dateRep), by = 5)], date_labels = "%b %d") +
  geom_point(color = "steelblue", size = 2.3) +
  theme_black() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_y_continuous(labels = function(x) format(x, scientific = F))


print(p2)
```


Row
-------------------------------------


### Cases near Ironton, MO {.no-padding}

``` {r}
ggplot(surround_MO_county_cases, aes(reorder(county,-county_cases), county_cases, fill = -county_cases)) +
  geom_bar(color = 'black', stat = 'identity') +
  geom_text(aes(label= county_cases), vjust= -.3, color="white", size=4) +
  theme_black() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + theme(legend.position = 'none')

```


### Cases near Hampton, VA {.no-padding}

``` {r}
ggplot(surround_VA_county_cases, aes(reorder(county,-county_cases), county_cases, fill = -county_cases)) +
  geom_bar(color = 'black', stat = 'identity') +
  geom_text(aes(label= county_cases), vjust= -.3, color="white", size=4) +
  theme_black() + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +  theme(legend.position = 'none')
```


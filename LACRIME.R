# Library Used
library(readr)
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(naivebayes)


# Data Import 
crimeLA <- read_csv("crime_in_la.csv")


# Train and Test Data

## To prevent data leakage, we split first

set.seed(1)

idx <- sample(seq(1,3),size = nrow(crimeLA),replace = T,prob = c(.7,.3,.3))
train <- crimeLA[idx == 1,]
test <- crimeLA[idx == 2,]
valid <- crimeLA[idx == 3,]

dim(train)
dim(test)
dim(valid)

summary(train)
summary(test)
summary(valid)



# Data Cleaning and Wrangling 

## Check Duplicates
sum(duplicated(train))
sum(duplicated(test))

## Filter Responses that doesn't make sense
train <- train[train$`Vict Age` != -1 & train$`Vict Age` <= 100,]

## Check the Data Type and information of each variable
lapply(train,class)
unique(train$`Crm Cd Desc`)

### Should categorize 'Crm Cd Desc'
train <- train %>% mutate(`Crm Cd Desc` = str_split(`Crm Cd Desc`, "\\-",simplify = T)[,1])
train <- train %>% mutate(`Crm Cd Desc` = gsub("^RAPE.*","RAPE",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("DOCUMENT WORTHLESS.*","DOCUMENT WORTHLESS",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("THEFT.*","THEFT",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("PETTY THEFT.*","THEFT",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("ATTEMPTED ROBBERY.*","ROBBERY",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^ASSAULT WITH DEADLY WEAPON.*","ASSAULT WITH DEADLY WEAPON",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^BATTERY.*","BATTERY",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^BUNCO.*","BUNCO",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^BURGLARY.*","BURGLARY",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^CREDIT CARDS.*","CREDIT CARDS FRAUD",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^EMBEZZLEMENT.*","EMBEZZLEMENT",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^LYNCHING.*","LYNCHING ",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^PURSE SNATCHING.*","PURSE SNATCHING",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^KIDNAPPING.*","KIDNAPPING",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^PICKPOCKET.*","PICKPOCKET",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^SHOTS FIRED.*","SHOTS FIRED GENERAL",`Crm Cd Desc`))
train <- train %>% mutate(`Crm Cd Desc` = gsub("^SHOPLIFTING.*","SHOPLIFTING",`Crm Cd Desc`))
x<-unique(train$`Crm Cd Desc`)
sort(x)


## Convert Date and Time
train$`DATE OCC` <- as.Date(train$`DATE OCC`,format = "%m/%d/%Y")
train$`TIME OCC` <- format(as.POSIXct.Date(as.character(train$`TIME OCC`)),"%H:%M")
class(train$`TIME OCC`)

## Look for NA values
sum(is.na(train))
which(is.na(train))

vis_miss(test,warn_large_data = F)
vis_miss(train,warn_large_data = F) # 60% missing for Data occured


### Drop Unnecessary Variables
train <- train %>% select(!c(Mocodes,Status,`Status Desc`,DR_NO,`Weapon Desc`,`DATE OCC`,`TIME OCC`))
prop.table(table(train$`Vict Sex`)) # 20% unknown - keep for now. 
prop.table(table(train$`Vict Descent`))
table(train$`Vict Age`) 


## Check for outliars

### All values including outliars are important in this predictive model


# Check crimeLA

tibble(train)

## Do the same for test & valid
test <- test[test$`Vict Age` != -1 & test$`Vict Age` <= 100,]
test <- test %>% select(!c(Mocodes,Status,`Status Desc`,DR_NO,`Weapon Desc`,`DATE OCC`,`TIME OCC`))
test <- test %>% mutate(`Crm Cd Desc` = str_split(`Crm Cd Desc`, "\\-",simplify = T)[,1])
test <- test %>% mutate(`Crm Cd Desc` = gsub("^RAPE.*","RAPE",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("DOCUMENT WORTHLESS.*","DOCUMENT WORTHLESS",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("THEFT.*","THEFT",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("PETTY THEFT.*","THEFT",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("ATTEMPTED ROBBERY.*","ROBBERY",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^ASSAULT WITH DEADLY WEAPON.*","ASSAULT WITH DEADLY WEAPON",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^BATTERY.*","BATTERY",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^BUNCO.*","BUNCO",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^BURGLARY.*","BURGLARY",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^CREDIT CARDS.*","CREDIT CARDS FRAUD",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^EMBEZZLEMENT.*","EMBEZZLEMENT",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^LYNCHING.*","LYNCHING ",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^PURSE SNATCHING.*","PURSE SNATCHING",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^KIDNAPPING.*","KIDNAPPING",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^PICKPOCKET.*","PICKPOCKET",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^SHOTS FIRED.*","SHOTS FIRED GENERAL",`Crm Cd Desc`))
test <- test %>% mutate(`Crm Cd Desc` = gsub("^SHOPLIFTING.*","SHOPLIFTING",`Crm Cd Desc`))

valid <- valid[valid$`Vict Age` != -1 & valid$`Vict Age` <= 100,]
valid <- valid %>% select(!c(Mocodes,Status,`Status Desc`,DR_NO,`Weapon Desc`,`DATE OCC`,`TIME OCC`))
valid <- valid %>% mutate(`Crm Cd Desc` = str_split(`Crm Cd Desc`, "\\-",simplify = T)[,1])
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^RAPE.*","RAPE",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("DOCUMENT WORTHLESS.*","DOCUMENT WORTHLESS",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("THEFT.*","THEFT",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("PETTY THEFT.*","THEFT",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("ATTEMPTED ROBBERY.*","ROBBERY",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^ASSAULT WITH DEADLY WEAPON.*","ASSAULT WITH DEADLY WEAPON",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^BATTERY.*","BATTERY",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^BUNCO.*","BUNCO",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^BURGLARY.*","BURGLARY",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^CREDIT CARDS.*","CREDIT CARDS FRAUD",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^EMBEZZLEMENT.*","EMBEZZLEMENT",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^LYNCHING.*","LYNCHING ",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^PURSE SNATCHING.*","PURSE SNATCHING",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^KIDNAPPING.*","KIDNAPPING",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^PICKPOCKET.*","PICKPOCKET",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^SHOTS FIRED.*","SHOTS FIRED GENERAL",`Crm Cd Desc`))
valid <- valid %>% mutate(`Crm Cd Desc` = gsub("^SHOPLIFTING.*","SHOPLIFTING",`Crm Cd Desc`))


# Further Preprocessing

## Make Ordinal Categorical Variables -> Order
train$`Vict Age` <- as.factor(train$`Vict Age`)
test$`Vict Age` <- as.factor(test$`Vict Age`)
valid$`Vict Age` <- as.factor(valid$`Vict Age`)
train$`Crm Cd` <- as.factor(train$`Crm Cd`)
test$`Crm Cd` <- as.factor(test$`Crm Cd`)
valid$`Crm Cd` <- as.factor(valid$`Crm Cd`)
train$`AREA NAME` <- as.factor(train$`AREA NAME`)
test$`AREA NAME` <- as.factor(test$`AREA NAME`)
valid$`AREA NAME` <- as.factor(valid$`AREA NAME`)

head(train)



train <- train %>% select(c(`Crm Cd Desc`,`AREA NAME`,`Weapon Used Cd`,`Vict Age`,`Vict Descent`,`Premis Desc`,`Rpt Dist No`,LAT,LON))
test <- test %>% select(c(`Crm Cd Desc`,`AREA NAME`,`Weapon Used Cd`,`Vict Age`,`Vict Descent`,`Premis Desc`,`Rpt Dist No`,LAT,LON))
valid <- valid %>% select(c(`Crm Cd Desc`,`AREA NAME`,`Weapon Used Cd`,`Vict Age`,`Vict Descent`,`Premis Desc`,LAT,LON,`Rpt Dist No`))

train$`Crm Cd Desc`<- as.factor(train$`Crm Cd Desc`)
train$`Weapon Used Cd` <- as.numeric(train$`Weapon Used Cd`)
train$`Vict Descent` <- as.factor(train$`Vict Descent`)
train$`Premis Desc` <- as.factor(train$`Premis Desc`)
train$`Rpt Dist No` <- as.factor(train$`Rpt Dist No`)

test$`Crm Cd Desc`<- as.factor(test$`Crm Cd Desc`)
test$`Weapon Used Cd` <- as.numeric(test$`Weapon Used Cd`)
test$`Vict Descent` <- as.factor(test$`Vict Descent`)
test$`Premis Desc` <- as.factor(test$`Premis Desc`)
test$`Rpt Dist No` <- as.factor(test$`Rpt Dist No`)

tibble(train)


train <- train %>% rename(crm_desc=`Crm Cd Desc` ,  wep_used = `Weapon Used Cd`, area = `AREA NAME`, vict_desc = `Vict Descent` , place = `Premis Desc`, vict_age = `Vict Age`, rpt_dist_no = `Rpt Dist No`)
test <- test %>% rename(crm_desc=`Crm Cd Desc` ,  wep_used = `Weapon Used Cd`, area = `AREA NAME`, vict_desc = `Vict Descent` , place = `Premis Desc`, vict_age = `Vict Age`, rpt_dist_no = `Rpt Dist No`)

train <- transform(train, group = as.numeric(area))
test <- transform(test, group = as.numeric(area))


## Further Analysis 
# train %>% select(c(`Crm Cd Desc`,`Weapon Used Cd`,`AREA NAME`,`Crm Cd`,LOCATION))
# test %>% select(c(`Crm Cd Desc`,`Weapon Used Cd`,`AREA NAME`,`Crm Cd`,LOCATION))
# valid %>% select(c(`Crm Cd Desc`,`Weapon Used Cd`,`AREA NAME`,LOCATION,`Crm Cd`))


## EDA

### Check for multicolinearity and all the necessary checking of the feature. 

eda <- train %>% group_by(area,crm_desc) %>% summarize(count = n())
ggplot(eda,aes(x = area, y = count, fill = crm_desc)) + geom_bar(position = "stack",stat = "identity") + theme(legend.position = "None")
eda %>% arrange(desc(count))
eda1 <- train %>% group_by(area) %>% summarize(count = n()) %>% mutate(perc = count / sum(count))
eda1 %>% arrange(desc(count)) %>% data.frame()


# Model Selection

## Supervised - Classifaction Algo

### Naive Bayes Model

head(train)

new_train <- train[,-c(8:10)]

classifer <- naiveBayes(area~., data = new_train)
pred <- predict(classifer,test[-2])
analysis <- data.frame(predict = pred, crm = test$crm_desc,lat = test$LAT,long = test$LON)
test_graphic <- analysis %>% group_by(crm) %>% ggplot(mapping = aes(x = crm)) + geom_bar(aes(y = (..count..)/sum(..count..))) + coord_flip()


# Use analysis data when showing the graphic on shiny 


# Map

library(sf)
library(plotly)
library(raster)
library(rgdal)
library(geosphere)

analysis <- analysis %>% filter(lat != 0 & long != 0)
display <- analysis %>% group_by(predict, crm) %>% summarize(count = n())

display <-merge(x = display,y = analysis, by = c("predict","crm"),all.x = T)
display <- display %>% filter(lat != 0 & long != 0)

display <- display %>% group_by(predict,crm) %>% summarize(count = n(),avglat = mean(lat), avglong = mean(long))

coord_analysis <- analysis[,3:4]
coord_analysis <- coord_analysis %>% filter(lat != 0 & long != 0)

my.sf.point <- st_as_sf(x = coord_analysis, 
                        coords = c("long", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

coord_analysis <- inner_join(coord_analysis,analysis)


LA <- read.sf("City_Boundaries.shp")
LA <- LA %>% 
  sf::st_coordinates() %>%
  as_tibble() %>%
  select("long" = X, "lat" = Y)

join_data <- coord_analysis %>% group_by(crm,predict) %>% summarize(number = n()) %>% ungroup()%>%  group_by(crm) %>%mutate(prop = number/sum(number))

km <- kmeans(cbind(coord_analysis$lat,coord_analysis$long),centers = 89 )

plot(coord_analysis$lat,coord_analysis$long, col = km$cluster,pch = 20)

coord_analysis<-coord_analysis %>% mutate(cluster = as.factor(km$cluster))

ggplot(LA) + geom_sf(fill = "white")

ggplot(LA) + geom_sf(fill = "white")+ xlim(-118.75,-118.1) + ylim(33.6,34.4) + geom_point(data = coord_analysis,aes(x = long,y = lat,color = cluster))

g<-ggplot(LA) + geom_sf()+ xlim(-118.75,-118.1) + ylim(33.6,34.4) + geom_point(data = display,mapping = aes(x = avglong,y = avglat,color = crm), shape = 20, stroke = F) + theme(legend.position = "none")


# try to implement it through plotly and then work on UI 


# Experiment


is.factor(coord_analysis$crm)
unique(coord_analysis$crm) # try to categorize crimes that are less than 0.01% to others


#  xlim(-118,-118.9) + ylim(33,35)
#  geom_point(data = coord_analysis,aes(x = long,y = lat)) +
max(coord_analysis$lat)
min(coord_analysis$lat)

max(coord_analysis$long)
min(coord_analysis$long)


# turn into list for ui
choices<-as.list(analysis$crm)
choices <- unique(choices)
choices


# New Map

US <- map_data("state")
california <- subset(US, region %in% c("california"))
graphic <- ggplot() + geom_polygon(california, mapping = aes(long, lat),alpha = 0.3) + 
  geom_polygon(color = "black", fill = NA) +
  geom_point(display, mapping =aes(avglong,avglat, color = predict,size = crm,alpha = 0.3),shape = 20, stroke = F) +
  viridis::scale_color_viridis(option="inferno",discrete = T ) +
  theme_void() +
  theme(legend.position = "none")+ 
  coord_fixed(xlim = c(-118.75, -118),  ylim = c(33.5, 34.5), ratio = 1.3) + 
  guides( colour = guide_legend())



ggplotly(graphic,tooltip= c("crm","predict"))



ui <- fluidPage(  
  theme = shinytheme("sandstone"),
  titlePanel("Crime LA Prediction"),
  sidebarLayout(
    sidebarPanel(
      h3("Filter"),
      pickerInput("crm",label = "Select Crime: ", choices = crmchoices,multiple = T) #predict = area Maybe do crm instead
      
    ),
    mainPanel(
      h1("Crime Prediction Graphic"),
      h3("Output"),
      plotlyOutput("plot2"))))

server <- function(input, output, session){
  
  displaysub <- reactive({
    display %>% filter(crm %in% input$predict)
  })
  
  
  output$plot2 <- renderPlotly({
    ggplotly(
      ggplot() + geom_polygon(california, mapping = aes(long, lat),alpha = 0.3) + 
        geom_polygon(color = "black", fill = NA) +
        geom_point(displaysub(), mapping =aes(avglong,avglat, color = predict,size = crm,alpha = 0.3),shape = 20, stroke = F) +
        theme_void() +
        theme(legend.position = "none")+ 
        coord_fixed(xlim = c(-118.75, -118),  ylim = c(33.5, 34.5), ratio = 1.3) + 
        guides( colour = guide_legend()))
  })
}

shinyApp(ui, server)


## Model Evaluation 

### Naive Bayes

data<- as_tibble(tibble(analysis$predict,test$area)) 
confusionMatrix(data = analysis$predict, reference = as.factor(test$area))
plot_confusion_matrix(data,target_col = "target",prediction_col = "pred",counts_col = "n")

par(pty = "s")
rocinfo<-roc(as.numeric(test$area),as.numeric(analysis$predict),plot = TRUE,legacy.axes = TRUE,xlab = "False Positive",ylab = "True Postiive",percent = TRUE,lwd = 4,col = "red",print.auc = T)

rocdf<-data.frame(tpp = rocinfo$sensitivities,fpp = (1-rocinfo$specificities),thresholds = rocinfo$thresholds)
rocdf[rocdf$tpp > 80 & rocdf$tpp <= 100]

library(plotly)

##### Adding points(markers) and lines to the same axes

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)

x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

(fig <- plot_ly(data, x = ~x) %>% 
    add_trace(y = ~trace_0, name = 'trace 0', mode = 'lines') %>% 
    add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>% 
    add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers'))

#### or

(fig <- plot_ly(data, x = ~x, y = ~trace_0,type = 'scatter' ,name = 'trace 0', mode = 'lines') %>% 
    add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>% 
    add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers'))

## need to define what type of graphs that's needed.


########## bar charts


x <- c('January', 'February', 'March', 'April', 'May', 'June', 
       'July', 'August', 'September', 'October', 'November', 'December')

y1 <- c(20, 14, 25, 16, 18, 22, 19, 15, 12, 16, 14, 17)
y2 <- c(19, 14, 22, 14, 16, 19, 15, 14, 10, 12, 12, 16)

data <- data.frame(x, y1, y2)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])

(p = plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Primary Product', 
             marker = list(color = 'rgb(49,130,189)')) %>% 
  add_trace(y = ~y2, name = 'Secondary Product', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
              yaxis = list(title = ""),
              margin = list(b = 100),
              barmode = 'group'))




##### Maps :Using ggplotly

library(maps)

# map data
county_df <- map_data("county")
state_df <- map_data("state")

### removing spaces in the names
county_df$subregion <- gsub(" ", "", county_df$subregion)   # gsub(pattern, replacement, x)

#election data
df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/votes.csv")
df <- subset(df, select = c(Obama, Romney, area_name))

df$area_name <- tolower(df$area_name) # changing area names to lower case 
df$area_name <- gsub(" county", "", df$area_name) # removing the name county
df$area_name <- gsub(" ", "", df$area_name) # removing the spaces in the name
df$area_name <- gsub("[.]", "", df$area_name) # removing '.' from names

# convert decimals in to percentages
df$Obama <- df$Obama*100
df$Romney <- df$Romney*100


## deciding the winner in each county


for (i in 1:length(df[,1])) 
  {
    if (df$Obama[i] > df$Romney[i]) {
      df$Percent[i] = df$Obama[i]   # saving the winning percentage as the 'Percent'
      
    } else {
      df$Percent[i] = -df$Romney[i] # differentiating between each winner
    }
  }

names(df) <- c("Obama", "Romney", "subregion", "Percent")

# join data
US <- inner_join(county_df, df, by = "subregion") # lots of duplicated entries
US <- US[!duplicated(US$order), ] # removing the duplicates entries



# colorramp
blue <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
red <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

#plot
(p <- ggplot(US, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percent),
               colour = alpha("white", 1/2), size = 0.05)  +
  geom_polygon(data = state_df, colour = "white", fill = NA) +
  ggtitle("2012 US Election") +
  scale_fill_gradientn(colours=c(blue,"white", red))  +
  theme_void())

ggplotly(p) 

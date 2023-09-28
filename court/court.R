library(plotly)

# Sample shot data (replace with your actual data)
shots <- data.frame(
  x = c(10, 15, 20, 25, 30), # X-coordinates of shots
  y = c(5, 10, 15, 20, 25)  # Y-coordinates of shots
)

# Create a scatter plot
plot <- plot_ly(data = shots, x = ~x, y = ~y, type = 'scatter', mode = 'markers', marker = list(size = 10))

# Define the path to your local basketball court image
court_image_path <- "C:/Users/Marc/Documents/GitHub/9 semester/Data-Visualization/court/court.jpg"  # Replace with your file path

# Add the background image
plot <- plot %>% 
  add_image(
    source = court_image_path,
    x = 0,  # Adjust the coordinates to position the image as needed
    y = 0,  # Adjust the coordinates to position the image as needed
    xref = "x",
    yref = "y",
    sizex = 50,  # Adjust the size to fit your plot
    sizey = 47   # Adjust the size to fit your plot
  ) 

plot <- plot %>% layout(
  xaxis = list(range = c(0, 50)),  # Adjust the range to fit your plot
  yaxis = list(range = c(0, 47)),  # Adjust the range to fit your plot
  showlegend = FALSE,
  title = "Player Shots on Background Image"
)

# Show the plot
plot



library('plotly')
library(tools)
library(base64enc)

court_image_path <- "C:/Users/Marc/Documents/GitHub/9 semester/Data-Visualization/court/court.jpg"  # Replace with your file path
# court_image_path <- tempfile(court_image_path)

image_binary <- readBin(court_image_path, "raw", file.info(court_image_path)$size)
image1 <- base64encode(image_binary)

library(htmltools)
img_widget <- tags$img(src = image1, alt = "Base64 Image")

# Display the HTML widget


#image1 <- base64.b64encode(open(court_image_path, 'rb').read())

# Create figure
plot_ly(x = c(0, 0.5, 1, 2, 2.2), y = c(1.23, 2.5, 0.42, 3, 1), type = 'scatter', mode = 'markers') %>% 
  # Add trace
  layout(
    images = list(
      list(
        # Add images
        source =  img_widget, #"https://images.plot.ly/language-icons/api-home/r-logo.png?raw=true",
        xref = "x",
        yref = "y",
        x = 0.2,
        y = 3,
        sizex = 2,
        sizey = 2,
        sizing = "stretch",
        opacity = 0.4,
        layer = "below"
      )
    )
  )%>%
  layout(plot_bgcolor='#e5ecf6',  
         xaxis = list(  
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff'),  
         yaxis = list(  
           zerolinecolor = '#ffff',  
           zerolinewidth = 2,  
           gridcolor = 'ffff')  
  )






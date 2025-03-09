# Load required libraries
library(shiny)
library(ggplot2)

# Function to calculate Euclidean distance
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}

# Function to assign clusters based on minimum distance
assign_clusters <- function(data_points, centers) {
  cluster_assignments <- sapply(1:nrow(data_points), function(i) {
    distances <- apply(centers, 1, function(center) euclidean_distance(data_points[i, ], center))
    which.min(distances)
  })
  data_points$cluster <- factor(cluster_assignments)
  data_points
}

# Function to create the plot
create_plot <- function(data_points, centers, selected_point = NULL) {
  p <- ggplot() +
    geom_point(data = data_points, aes(x = x, y = y), color = "blue", size = 3) +
    geom_point(data = centers, aes(x = x, y = y), color = "red", size = 5, shape = 8) +
    labs(title = "Data Points and Cluster Centers",
         x = "X Coordinate", y = "Y Coordinate") +
    theme_minimal()
  
  if (!is.null(selected_point)) {
    selected_data <- data_points[selected_point, ]
    distances <- apply(centers, 1, function(center) euclidean_distance(selected_data[, c("x", "y")], center))
    closest_center <- which.min(distances)
    p <- p +
      geom_point(data = selected_data, aes(x = x, y = y), color = "black", size = 4, shape = 17) +
      geom_segment(data = centers, aes(x = selected_data$x, y = selected_data$y, xend = x, yend = y),
                   color = "gray", linetype = "dashed") +
      geom_segment(aes(x = selected_data$x, y = selected_data$y,
                       xend = centers[closest_center, "x"], yend = centers[closest_center, "y"]),
                   color = "green", linetype = "solid") +
      labs(subtitle = paste("Selected Point Assigned to Center:", closest_center))
  }
  
  p
}

# Generate fixed data points and cluster centers
set.seed(123)
data_points <- data.frame(
  x = runif(10, min = -10, max = 10),
  y = runif(10, min = -10, max = 10)
)
centers <- data.frame(
  x = runif(3, min = -10, max = 10),
  y = runif(3, min = -10, max = 10)
)

# Assign clusters
data_points <- assign_clusters(data_points, centers)

# Define UI
ui <- fluidPage(
  titlePanel("Cluster Assignment Based on Minimum Distance"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("point_index", "Select Data Point Index:",
                  min = 1, max = nrow(data_points), value = 1, step = 1)
    ),
    mainPanel(
      plotOutput("clusterPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$clusterPlot <- renderPlot({
    create_plot(data_points, centers, input$point_index)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

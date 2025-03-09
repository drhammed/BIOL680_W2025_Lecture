library(shiny)
library(ggplot2)
library(dplyr)

# Generate synthetic data
set.seed(42)
n_samples <- 10
n_clusters <- 3

# Randomly generate 10 data points
X <- data.frame(
  X1 = runif(n_samples, min = 0, max = 10),
  X2 = runif(n_samples, min = 0, max = 10)
)

# Initialize cluster centers by randomly selecting 3 points from X
initial_centers_idx <- sample(1:n_samples, n_clusters)
initial_centers <- X[initial_centers_idx, ]

# Function to assign clusters based on the nearest center
assign_clusters <- function(X, centers) {
  dist_matrix <- as.matrix(dist(rbind(X, centers)))[1:nrow(X), (nrow(X) + 1):(nrow(X) + nrow(centers))]
  cluster_assignments <- apply(dist_matrix, 1, which.min)
  return(cluster_assignments)
}

# Function to update cluster centers based on current assignments
update_centers <- function(X, assignments, k) {
  new_centers <- data.frame(X1 = numeric(k), X2 = numeric(k))
  for (i in 1:k) {
    cluster_points <- X[assignments == i, ]
    if (nrow(cluster_points) > 0) {
      new_centers[i, ] <- colMeans(cluster_points)
    } else {
      # If a cluster has no points assigned, reinitialize its center randomly
      new_centers[i, ] <- X[sample(1:nrow(X), 1), ]
    }
  }
  return(new_centers)
}

# Function to plot the current state of clustering
plot_kmeans <- function(X, assignments, centers, previous_centers, iteration) {
  p <- ggplot(X, aes(x = X1, y = X2, color = as.factor(assignments))) +
    geom_point(size = 3) + 
    geom_point(data = centers, aes(x = X1, y = X2), color = "black", size = 5, shape = 4) # Current centroid as cross
  
  # Plot previous centers and dashed lines (if available)
  if (!is.null(previous_centers)) {
    # Create a data frame to hold previous and current positions for geom_segment
    transition_df <- data.frame(
      x = previous_centers$X1,
      y = previous_centers$X2,
      xend = centers$X1,
      yend = centers$X2
    )
    
    # Plot old centers as red stars
    p <- p + 
      geom_point(data = previous_centers, aes(x = X1, y = X2), color = "red", size = 5, shape = 8) +
      geom_segment(data = transition_df, aes(x = x, y = y, xend = xend, yend = yend),
                   color = "black", linetype = "dashed", linewidth = 0.7)
  }
  
  p <- p + 
    labs(
      title = paste("K-Means Clustering: Iteration", iteration),
      x = "Feature 1",
      y = "Feature 2",
      color = "Cluster"
    ) +
    theme_minimal() +
    scale_color_discrete(name = "Cluster")
  
  return(p)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("K-Means Clustering Process"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("iteration", "Iteration:", min = 0, max = 10, value = 0, step = 1)
    ),
    mainPanel(
      plotOutput("clusterPlot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive values to store centers, assignments, and previous centers
  rv <- reactiveValues(
    centers = initial_centers,
    assignments = assign_clusters(X, initial_centers),
    previous_centers = NULL
  )
  
  # Observe iteration changes
  observeEvent(input$iteration, {
    if (input$iteration == 0) {
      # Reset to initial centers and assign clusters
      rv$centers <- initial_centers
      rv$assignments <- assign_clusters(X, rv$centers)
      rv$previous_centers <- NULL
    } else {
      for (i in 1:input$iteration) {
        # Assign clusters based on current centers
        rv$assignments <- assign_clusters(X, rv$centers)
        # Store previous centers before updating
        prev_centers <- rv$centers
        # Update centers based on current assignments
        new_centers <- update_centers(X, rv$assignments, n_clusters)
        # Check for convergence
        if (all(prev_centers == new_centers)) {
          break
        }
        rv$previous_centers <- prev_centers
        rv$centers <- new_centers
      }
    }
  })
  
  # Render plot
  output$clusterPlot <- renderPlot({
    plot_kmeans(X, rv$assignments, rv$centers, rv$previous_centers, input$iteration)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

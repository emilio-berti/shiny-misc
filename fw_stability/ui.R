library(shiny)
    library(igraph)

#' @param n is the number of species - int.
#' @param C is the connectivity of the food web: number of links / N^2 - float.
#' @param rho correlation between opposite matrix elements - int.
#' @return a matrix @param n x @param n with a fraction ~@param C of non-zero
#'   entries randomly distributed ~@param N(0,1) .
gen_fw <- function(n, C, rho = 0) {
    m <- matrix(runif(n * n) <= C / 2, nrow = n, ncol = n) * 1
    m[which(m > 0)] <- rnorm(length(which(m > 0)), 0, 1)
    if (rho != 0) {
        m <- m + rho * t(m)
    } else{
        m2 <- t(m) > 0
        m2[which(m2 > 0)] <- rnorm(length(which(m2 > 0)), 0, 1)
        m <- m + m2
    }
    diag(m) <- 0
    return(m)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    n <- reactive({input$n})
    C <- reactive({input$C})
    rho <- reactive({input$rho})
    m <- reactive({gen_fw(n(), C(), rho())})
    output$fw_plot <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        raster::plot(raster::raster(m()),
                     box = FALSE, axes = FALSE,
                     col = RColorBrewer::brewer.pal(11, "RdBu"))
    })
    output$fw_graph <- renderPlot({
        g <- graph_from_adjacency_matrix(m(), weighted = TRUE)
        g <- delete_vertices(g, degree(g) == 0)
        par(mar = c(0, 0, 0, 0))
        plot(g, vertex.color = "steelblue",
             vertex.label = NA, vertex.size = 3,
             edge.arrow.size = 0, 
             edge.color = rgb(0.7, 0.7, 0.7, 1 / sqrt(n())))})
    #outputOptions(output, "fw_plot", suspendWhenHidden = FALSE)  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel(""),
    # Sidebar with a slider input for number of bins
    fluidRow(
        column(4,
               offset = 0,
               sliderInput("n",
                           "Number of species:",
                           min = 10,
                           max = 500,
                           value = 50),
               sliderInput("C",
                           "Connectance:",
                           min = 0,
                           max = 1,
                           value = 0.1),
               sliderInput("rho",
                           "\u03c1:",
                           min = -1,
                           max = 1,
                           value = 0,
                           step = 0.1)
               ),
        column(4,
               offset = 0,
               plotOutput("fw_graph")
        ),
        column(4,
               offset = 0,
               plotOutput("fw_plot")
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)

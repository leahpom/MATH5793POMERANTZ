#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library statements
library(shiny)
library(ggplot2)

source("appSource.R") # store functions in here to make the app look nicer

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application Title

    titlePanel("Input Data and Select Options"),

    # Now do the sidebar input

    sidebarLayout(
        sidebarPanel(
            fileInput("file", # the input name to reference in the app
                      "Choose a CSV file", # what will appear to the user
                      multiple = FALSE, # allows only one file to be selected
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), # only allows .csv extensions to be uploaded (from Shiny gallery)

            # allows the user to select what variables they want
            textInput("var1", # the input name to reference in app
                      "Enter the column number of the first variable",
                      value = 1),

            # allows the user to select second variable
            textInput("var2",
                      "Enter the column number of the second variable",
                      value = 2), # allows the user to select second variable

            # allows the user to select a color for the points on the scatterplot
            textInput("color", # label for reference later
                      "Hexadecimal color number",
                      value = "#000000"),

            # allows the user to select the size of the points on the scatterplot
            sliderInput("size",
                        "Size of the Points",
                        1, 4, 2.5, step = 0.5),

            # allows the user to enter a label for the y-axis
            textInput("ylab",
                      "Enter the label for the y-axis",
                      value = "yLab"),

            # allows the user to enter a label for the x-axis
            textInput("xlab",
                      "Enter the label for the x-axis",
                      value = "xLab"),

            # allows the user to select the value of theta
            sliderInput("theta",
                        "Theta Value for Rotation",
                        -round(pi, 4), round(pi, 4), 0, step = 0.0001),

            # allows the user to select how long they want the rotated axes to be
            textInput("radius",
                      "Radius of Rotated Axes",
                      value = "50"),

            # allows the user to select a color for the rotated axes
            textInput("color2", # label for reference later
                      "Hexadecimal color number for rotated axes",
                      value = "#ff0000"),


        ),

        # Now, define the main panel for output

        mainPanel(
            tableOutput("table"), # output the data frame, for reference (check that it's the right file)

            plotOutput("plot", click = "plotClick"), # the ggplot object that will appear, SHOULD BE CLICKABLE
            #tableOutput("table2") # temporary to check subsetting - WORKED

            # allows the drop-1 correlations to be put onto the graph
            verbatimTextOutput("drop1Corr"),

            # creates the second graph
            plotOutput("plot2"),

            # allows the sample correlation to be put onto the graph
            verbatimTextOutput("sampleCorr"),

            # allows the first quad solution to be put onto the graph
            verbatimTextOutput("fQuadSol")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$table <- renderTable({
        req(input$file)
        tryCatch(
            {
                df <- read.csv(input$file$datapath)
            }
        )

        return(head(df))

    })

    # CHANGING THE TYPE OF OUTPUT TEMPORARILY TO SEE IF THE SUBSETTING WORKED - WORKED!!!!
    output$plot <- renderPlot({
        #get the data available for use
        df <- if(!is.null(input$file)){
                as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
            }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number ot help the subsetting
            } # try the updateSelectInput to get reactive ??
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
            }
        xCol <- df[, xNum] # pull first column from the input
        yCol <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(xCol, yCol)) # the data frame

        # get the color into a variable - consider adding hash back?? idk
        pointColor <- input$color

        # get the size input into the correct format
        pointSize <- as.numeric(input$size)

        # get the radius input into the correct format
        radius <- as.numeric(input$radius)

        g <- ggplot(df, aes(x = xCol, y = yCol)) # this part works
        g <- g + geom_point(colour = pointColor, size = pointSize) # scatterplot works, including color and size
        g <- g + ggplot2::labs(title = "Data Scatterplot", x = input$xlab, y = input$ylab) # user axis labels
        g # return
    })

    output$drop1Corr <- renderText({
        req(input$plotClick) # stops an error on the filter function

        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        } # try the updateSelectInput to get reactive ??
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }
        xCol <- df[, xNum] # pull first column from the input
        yCol <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(xCol, yCol)) # the data frame

        dfDropPoints <- nearPoints(df, input$plotClick, maxpoints = 1)
        dfDrop1 <- dplyr::filter(df, !xCol == dfDropPoints[1,1] & !yCol == dfDropPoints[1, 2])

        drop1Corr <- cor(dfDrop1)[1, 2]

        paste0("The Drop-1 Correlation is: ", drop1Corr)
    })

    output$plot2 <- renderPlot({
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number ot help the subsetting
        } # try the updateSelectInput to get reactive ??
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }
        xCol <- df[, xNum] # pull first column from the input
        yCol <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(xCol, yCol)) # the data frame

        # get the color into a variable - consider adding hash back?? idk
        pointColor <- input$color

        # get the size input into the correct format
        pointSize <- as.numeric(input$size)

        # get the radius input into the correct format
        radius <- as.numeric(input$radius)

        g <- ggplot(df, aes(x = xCol, y = yCol)) # this part works
        g <- g + geom_point() # scatterplot works
        g <- g + ggplot2::labs(title = "Rotated Axes" ,x = input$xlab, y = input$ylab) # user axis labels
        g <- g + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) # add the axes
        g <- g + geom_spoke(aes(x = 0, y = 0, angle = input$theta), radius = radius, color = input$color2) +
            geom_spoke(aes(x = 0, y = 0, angle = pi + input$theta), radius = radius, color = input$color2) +
            geom_spoke(aes(x = 0, y = 0, angle = input$theta + 0.5*pi), radius = radius, color = input$color2) +
            geom_spoke(aes(x = 0, y = 0, angle = 1.5*pi + input$theta), radius = radius, color = input$color2)
        g # return
    })

    output$sampleCorr <- renderText({

        req(input$file) # gets an annoying error to go away

        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        } # try the updateSelectInput to get reactive ??
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }
        xCol <- df[, xNum] # pull first column from the input
        yCol <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(xCol, yCol)) # the data frame

        X <- as.matrix(df) # turn into a matrix
        Xt <- myxtilde(X = X, theta = input$theta) # calculate tilde{x_1} and tilde{x_2}
        sampCorr <- cov(Xt[, 1], Xt[, 2]) # sample correlation, unbiased formula
        sampCorr <- round(sampCorr, 4) # round to stop from being too long
        paste0("The sample correlation with Theta equals ", input$theta, " is ", sampCorr)
        # this works, but the formula seems wrong. Come back to when uniroot works
    })

    output$fQuadSol <- renderText({

        req(input$file) # gets an annoying error to go away

        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        } # try the updateSelectInput to get reactive ??
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }
        xCol <- df[, xNum] # pull first column from the input
        yCol <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(xCol, yCol)) # the data frame

        X <- as.matrix(df) # turn into a matrix
        S <- cov(X) # finds the covariance matrix, sigma
        s12Tilde = function(x) (S[2, 2] - S[1, 1])*cos(x)*sin(x) + (S[1, 2])*cos(2*x) # create the function
        rootInterval = c(0, 0.5*pi) # the interval of values in the first quadrant
        funRoots <- rootSolve::uniroot.all(s12Tilde, interval = rootInterval) # calculate the roots

        paste0("The first-quadrant solution is theta = ", round(funRoots, 4), " radians")
    })

    # works up to here

    # THIS WORKED
    # output$table2 <- renderTable({
    #         df <- if(!is.null(input$file)){
    #                 as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
    #             }
    #         xNum <- if(!is.null(input$var1)){
    #             as.numeric(input$var1)
    #             } # try the updateSelectInput to get reactive
    #         yNum <- if(!is.null(input$var2)){
    #             as.numeric(input$var2)
    #             }
    #         xCol <- df[, xNum] # still not subsettable # took away reactive, seems to be working??
    #         yCol <- df[, yNum]
    #         df <- data.frame(cbind(xCol, yCol))
    #         return(head(df))
    # })
}

# Run the application
shinyApp(ui = ui, server = server)

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
library(MATH5793POMERANTZ)
library(ggplot2)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Input Data and Control"),

    # now the sidebar layout
    sidebarLayout(
        sidebarPanel(
            # file input works!
            fileInput("file", # the input name to reference in the app
                      "Choose a CSV file", # what will appear to the user
                      multiple = FALSE, # allows only one file to be selected
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), # only allows .csv extensions to be uploaded (from Shiny gallery)

            # allow the user to select the column number of the first variable
            textInput("var1", # the input name to reference in app
                      "Enter the column number of the first variable",
                      value = 1),

            # allow the user to select the column number of the second variable
            textInput("var2",
                      "Enter the column number of the second variable",
                      value = 2), # allows the user to select second variable

            # allow the user to select the significance level for the confidence intervals
            sliderInput("alpha",
                        "Enter the alpha value for the Confidence Intervals",
                        0, 1, 0.05, step = 0.01),

            # allows the user to enter a label for the y-axis
            textInput("ylab",
                      "Enter the label for the y-axis",
                      value = "mu_2"),

            # allows the user to enter a label for the x-axis
            textInput("xlab",
                      "Enter the label for the x-axis",
                      value = "mu_1"),

            # ??
        ),

        # Show a table of first six entries of the data for the user to look at
        mainPanel(
            tableOutput("table"), # output the data frame, for reference (check that it's the right file)

            plotOutput("T2Plot"), # output the confidence ellipse with the T^2 values

            DTOutput("T2Int"),  # output the matrix of the confidence intervals below the graph

            plotOutput("bonfP"), # output the confidence ellipse with the Bonferroni intervals

            DTOutput("bonfTab"), # output the matrix of the confidence intervals below the graph

            plotOutput("T2andBF"), # output graph showing comparison of BF and T2 intervals

            DTOutput("diffInMeans") # output showing table of the difference in means
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output table works!
    output$table <- renderTable({
        req(input$file)
        tryCatch(
            {
                df <- read.csv(input$file$datapath)
            }
        )

        return(head(df))

    })

    output$T2Plot <- renderPlot({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        }
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }

        g <- T2conf.ellipse(df, alpha = input$alpha, col1 = xNum, col2 = yNum)
        g <- g + xlab(input$xlab) + ylab(input$ylab)
        g

    })

    output$T2Int <- renderDT({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }

        # calculate the confidence intervals and store output
        ci <- ci_T2(data = df, alpha = input$alpha)

        rn <- colnames(df)

        tab <- as.data.frame(round(ci, 4))

        datatable(tab, editable = 'cell', rownames = rn, caption = "T^2 Confidence Intervals")

    })

    output$bonfP <- renderPlot({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        }
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }

        g <- BFconf.ellipse(df, alpha = input$alpha, col1 = xNum, col2 = yNum)
        g <- g + xlab(input$xlab) + ylab(input$ylab)
        g
    })

    output$bonfTab <- renderDT({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }

        # calculate the confidence intervals and store output
        ci <- bfMean_int(data = df, alpha = input$alpha)

        rn <- colnames(df)

        tab <- as.data.frame(round(ci, 4))

        datatable(tab, editable = 'cell', rownames = rn, caption = "Bonferroni Confidence Intervals")
    })

    output$T2andBF <- renderPlot({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        }
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }

        g <- BFconf.ellipse(df, alpha = input$alpha, col1 = xNum, col2 = yNum, addT2 = TRUE)
        g <- g + xlab(input$xlab) + ylab(input$ylab)
        g
    })

    output$diffInMeans <- renderDT({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$var1)){
            as.numeric(input$var1) # turn the input into a number to help the subsetting
        }
        yNum <- if(!is.null(input$var2)){
            as.numeric(input$var2)
        }

        # calculate the confidence intervals and store output
        ci <- ciDiffMeans(data = df, alpha = input$alpha, i = xNum, k = yNum)

        rn <- colnames(df)
        rn <- paste(rn[xNum], " - ", rn[yNum])

        tab <- as.data.frame(round(ci, 4))

        datatable(tab, editable = 'cell', rownames = rn, caption = "T^2 Confidence Intervals for Difference in Means")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

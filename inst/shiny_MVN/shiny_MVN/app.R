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
library(ggforce)
library(dplyr)
library(RColorBrewer)
library(gt)
library(stringi)
library(ggExtra)
library(gridExtra)
library(MATH5793POMERANTZ)

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

            # allows the user to select what variables they want
            textInput("varNorm", # the input name to reference in app
                      "Enter the column number of the variable for Normal Proportion check",
                      value = 1),

            textInput("varQQ", # the input name to reference in app
                      "Enter the column number of the variable for QQ-Plot and Shapiro-Wilk Test",
                      value = 1),

            # textInput("varSW", # the input name to reference in app
            #           "Enter the column number of the variable for the Shapiro-Wilk Test",
            #           value = 1),

            # allows the user to select what variables they want
            textInput("var1", # the input name to reference in app
                      "Enter the column number of the first variable for Bivariate normality check",
                      value = 1),

            # allows the user to select second variable
            textInput("var2",
                      "Enter the column number of the second variable for Bivariate normality check",
                      value = 2), # allows the user to select second variable

            # allows the user to select the alpha level
            sliderInput("alpha",
                        "Enter the alpha value for Bivariate normality check",
                        0, 1, 0.5, step = 0.01),

            # allows the user to select which observations they would like to view in the table
            textInput("subs",
                      "Enter the consecutive rows of the Generalized Distances table, separated by a comma",
                      value = "1,10"),

            # allows the user to select which observations they would like to view in the z-table
            textInput("zrows",
                      "Enter the consecutive rows of the Standardized Values table, separated by a comma",
                      value = "1,10"),

            # allows the user to enter a range of values for the Box-Cox transformation
            textInput("rlamb",
                      "Enter the range of lambda values for the Box-Cox transformation, separated by a comma",
                      value = "0,1"),

            # allows the user to have a dynamic lambda for the plot
            textInput("lambda",
                      "Enter the specific lambda value for the Box-Cox transformation, up to four decimal places",
                      value = "0.5"),

            # allows the user to select what column they want to use for the Box-Cox transformation
            textInput("boxcox", # the input name to reference in app
                      "Enter the column number of the variable for the Box-Cox Evaluation",
                      value = 1),
        ),

        # And now the main panel to show the output
        mainPanel(
            tableOutput("table"), # output the data frame, for reference (check that it's the right file)

            plotOutput("propPlot1"), # plot to show the proportion test for normality

            verbatimTextOutput("propText1"), # text to show the results of the proportion test

            plotOutput("propPlot2"), # plot to show the second proportion test for normality

            verbatimTextOutput("propText2"), # text to show the results of the proportion test

            plotOutput("QQPlot"), # plot space for the QQ-Plot

            verbatimTextOutput("ShapWilk"), # text to show the p-value from the Shapiro-Wilk Test

            tableOutput("rTab"), # a table to show the correlation coefficients

            plotOutput("ellipse"), # a plot for an ellipse containing approximately 1-alpha of the data

            tableOutput("dist"), # a table that will display the generalized distances

            plotOutput("chisq"), # a plot to do the chi-square plot for assessing normality

            tableOutput("stnd"), # a table to display the z-values and the generalized distances

            plotOutput("scat_marg"), # a plot to show the scatterplot and marginal dotplot of selected variables

            plotOutput("updatechisq"), # FIX ME: figuring out how to get the chisquare plot to show outlier marked

            plotOutput("llambda"), # a plot of the l(lambda) Box-Cox transformation

            verbatimTextOutput("bcshapWilk"), # text to show the p-value from the Shapiro-Wilk Test on BC data

            plotOutput("qqplotS", click = "plotClick"), # the ggplot object that will appear, SHOULD BE CLICKABLE

            plotOutput("qqplotDrop"), # the qqplot that should come as a result of clicking on the previous plot


        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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

    # results for the normality proportion test

    # proportion test for normality plot 1
    output$propPlot1 <- renderPlot({ # FIX ME: title
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varNorm)){
            as.numeric(input$varNorm) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        test <- prop_normal(df[,1]) # store text for passing or not
        df <- mutate(df, inInterval = between(df[,1], test$lower.bound1, test$upper.bound1))
        textTest1 <- test$pass1
        pTop <- ggplot(df, aes(x = df[,1], color = inInterval)) +
            geom_dotplot(fill = "white") + xlab("Variable") +
            ggtitle("Normality Proportion Test - 1 Standard Deviation")
        # FIX ME: good spot for user input
        pTop

    })

    output$propText1 <- renderText({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varNorm)){
            as.numeric(input$varNorm) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        test <- prop_normal(df[,1]) # store text for passing or not
        df <- mutate(df, inInterval = between(df[,1], test$lower.bound1, test$upper.bound1))
        textTest1 <- test$pass1
        paste0(textTest1)
    })


    # proportion test for normality plot 2
    output$propPlot2 <- renderPlot({ #FIX ME: title
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varNorm)){
            as.numeric(input$varNorm) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        test <- prop_normal(df[,1]) # store text for passing or not
        df <- mutate(df, inInterval = between(df[,1], test$lower.bound2, test$upper.bound2))
        textTest2 <- test$pass2
        pTop2 <- ggplot(df, aes(x = df[,1], color = inInterval)) +
            geom_dotplot(fill = "white") + xlab("Variable") +
            ggtitle("Normality Proportion Test - 2 Standard Deviations")
        # FIX ME: good spot for user input
        pTop2

    })

    output$propText2 <- renderText({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varNorm)){
            as.numeric(input$varNorm) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        test <- prop_normal(df[,1]) # store text for passing or not
        df <- mutate(df, inInterval = between(df[,1], test$lower.bound1, test$upper.bound1))
        textTest2 <- test$pass2
        paste0(textTest2)
    })

    # QQ-plot based on user input
    output$QQPlot<- renderPlot({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varQQ)){
            as.numeric(input$varQQ) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        quant <- q_quantiles(df[,1])
        df2 <- data.frame(cbind(quant$orderedObs, quant$stndNormQuant))
        names(df2)[1] <- "x_j"
        names(df2)[2] <- "q_j"
        quantPlot <- ggplot(df2, aes(x = q_j, y = x_j)) + geom_point() + ggtitle("QQ-Plot")
        quantPlot

    })

    output$ShapWilk <- renderText({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$varQQ)){
            as.numeric(input$varQQ) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame
        SW <- shapiro.test(df[,1])
        pVal <- SW$p.value
        paste0("The p-value from the Shapiro-Wilk Test is ", pVal)
    })

    output$rTab <- renderTable({
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        df <- data.frame(df) # the data.frame

        ri <- rep(0, ncol(df)) # create an empty vector to store the values
        # ri now contains each value
        for (i in 1:ncol(df)) {
            ri[i] <- cor_coefqq(df[,i])
        }

        # rn contains the appropriate name
        rn <- rep("", ncol(df))
        for (i in 1:ncol(df)) {
            rn[i] <- paste("r", i)
        }

        tab <- as_tibble(t(ri))
        colnames(tab) <- rn
        gt_tbl <- gt(data = tab)
        gt_tbl <- gt_tbl %>%
            tab_header(title = md("**Correlation Coefficients**")) # FIX ME: title isn't working
        gt_tbl
    }, caption = paste("Correlation Coefficients (r_Q values)"),
    caption.placement = getOption("xtable.caption.placement", "top"))

    output$ellipse <- renderPlot({
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

        xBar1 <- colMeans(df)
        S <- cov(df)
        x0 <- xBar1[1]
        y0 <- xBar1[2]
        eigenS <- eigen(S)
        lambda1 <- eigenS$values[1]
        lambda2 <- eigenS$values[2]
        e1 <- eigenS$vectors[,1]
        e2 <- eigenS$vectors[,2]
        cSq <- qchisq(input$alpha, 2, lower.tail = FALSE)
        axis1 <- sqrt(cSq) * sqrt(lambda1)
        axis2 <- sqrt(cSq) * sqrt(lambda2)
        ctheta <- cor(df[,1], df[,2])
        theta <- acos(ctheta)

        g <- ggplot(data = df, aes(x = df[,1], y = df[,2])) +
            geom_point() + xlab("x1") + ylab("x2") +
            ggtitle("Ellipse containing approximately 1-alpha of the data") +
            geom_ellipse(aes(x0 = x0, y0 = y0, a = axis1, b = axis2, angle = theta))
        g
    })

    output$dist <- renderTable({
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

        # calculate necessary values
        xBar <- colMeans(df)
        S <- cov(df)
        Sinv <- solve(S)
        compValue <- qchisq(input$alpha, 2, lower.tail = FALSE) # the value to compare distances to CHANGED HERE

        gd <- rep(0, nrow(df)) # create a vector to hold the generalized distances

        for (i in 1:nrow(df)) {
            xi <- df[i,] # the observation
            di <- as.matrix((xi - xBar)) # the observation minus the mean

            gd[i] <- di %*% Sinv %*% t(di) # the generalized distance
        }

        # bind all the information together into one data frame to output as a table
        critValCol <- rep(compValue, nrow(df)) # create a vector for the critical value to be used as a column
        obsNum <- 1:nrow(df)
        comparison <- (gd <= compValue)
        df2 <- data.frame(cbind(obsNum, gd, critValCol, comparison))
        namesdf2 <- c("Observation Number", "Generalized Distance", "Critical Value", "Comparison") # store the names

        # organize all the information into a table
        tabrows <- as.character(input$subs)
        commaLoc <- stri_locate_first(pattern = ",", tabrows, fixed = TRUE) # find where the comma is
        end <- commaLoc - 1 # where first row number ends
        row1 <- as.numeric(substr(tabrows, 1, end))
        begin <- commaLoc + 1 # where second row number begins
        row2 <- as.numeric(substr(tabrows, begin, nchar(tabrows)))

        df3 <- df2[row1:row2,] # from user input
        tab <- as_tibble(df3)
        colnames(tab) <- namesdf2

        gt_tbl <- gt(data = tab)
        gt_tbl <- gt_tbl %>%
            tab_header(title = md("**Generalized Distances**")) # FIX ME: title isn't working
        gt_tbl
    }, caption = paste("Generalized Distances"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    striped = TRUE
    )

    output$chisq <- renderPlot({
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

        chisq <- chi_sqQ(df)

        q <- chisq$q_j
        dsq <- chisq$d_jsq

        df.chisq <- data.frame(cbind(q, dsq))

        g <- ggplot(data = df.chisq, aes(x = q, y = dsq)) +
            geom_point(color = "#7C79B7") + ggtitle("Chi-Square Plot")
        g
    })

    output$stnd <- renderTable({
        # the data frame
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        df <- data.frame(df) # the data.frame

        # calculate the z-values using function
        z_values <- stnd_values(df)

        n <- nrow(df)

        jObs <- rep(0, n) # create an empty vector to store the order of the observations, j
        for (i in 1:n){
            jObs[i] <- i
        }
        z_values_plus <- cbind(jObs, z_values)

        # put the standardized values into a table
        tabrows <- as.character(input$zrows)
        commaLoc <- stri_locate_first(pattern = ",", tabrows, fixed = TRUE) # find where the comma is
        end <- commaLoc - 1 # where first row number ends
        row1 <- as.numeric(substr(tabrows, 1, end))
        begin <- commaLoc + 1 # where second row number begins
        row2 <- as.numeric(substr(tabrows, begin, nchar(tabrows)))

        z_values_disp <- z_values_plus[row1:row2,] # from user input

        # zn contains the appropriate column names
        nc <- ncol(df)
        zn <- rep("", nc)
        for (i in 1:nc) {
            zn[i] <- paste("z", i)
        }

        znpl <- nc + 1 # calculate the length znp should be
        znp <- rep(0, znpl)
        znp[1] <- "Observation"
        znp[2:length(znp)] <- zn

        tab <- as_tibble(z_values_disp)
        colnames(tab) <- znp

        gt_tbl <- gt(data = tab)

    }, caption = paste("Standardized Values (Z_j for each column, j)"),
    caption.placement = getOption("xtable.caption.placement", "top"),
    striped = TRUE)

    output$scat_marg <- renderPlot({

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
        x1 <- df[, xNum] # pull first column from the input
        x2 <- df[, yNum] # pull second column from the input
        df <- data.frame(cbind(x1, x2)) # the data frame

        # create the plot
        pMain <- ggplot(df, aes(x = x1, y = x2)) +
            geom_point() + ggtitle("Scatterplot of the Two Variables")
        pTop <- ggplot(df, aes(x = x1)) +
            geom_dotplot(color = "gray", fill = "#456789") + ggtitle("Marginal Plot x1")
        pRight <- ggplot(df, aes(x = x2)) +
            geom_dotplot(color = "gray", fill = "#456789") + coord_flip() + ggtitle("Marginal Plot x2")
        pEmpty <- ggplot(df, aes(x = x1, y = x2)) +
            geom_blank() +
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  line = element_blank(),
                  panel.background = element_blank())
        grid.arrange(pTop, pEmpty, pMain, pRight,
                     ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))
    })

    output$updatechisq <- renderPlot({
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

        chisq <- chi_sqQ(df)

        q <- chisq$q_j # q_j
        dsq <- chisq$d_jsq # generalized distance

        df.chisq <- data.frame(cbind(q, dsq)) # the data frame

        critq <- qchisq(0.2, df = 2, lower.tail = FALSE)

        dGcritq <- (dsq > critq)

        df.chisq <- data.frame(cbind(df.chisq, dGcritq)) # add the T/F by Z-values
        df.chisq

        # the chisquare plot
        g <- ggplot(data = df.chisq, aes(x = q, y = dsq)) +
            geom_point(aes(colour = dGcritq)) + ggtitle("Updated Chi-Square Plot") +
            scale_colour_brewer(palette = "Set2") +
            labs(color = "dsq > upper fifth percentile")
        g
    })

    output$llambda <- renderPlot({
        # get the data
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$boxcox)){
            as.numeric(input$boxcox) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame

        # get lambda
        user_lambda <- as.numeric(input$lambda)
        user_lambda <- round(user_lambda, digits = 4) # round to four decimal places, just in case

        # get lambda range
        intBounds <- as.character(input$rlamb)
        commaLoc <- stri_locate_first(pattern = ",", intBounds, fixed = TRUE) # find where the comma is
        end <- commaLoc - 1 # where first row number ends
        lower.bound <- as.numeric(substr(intBounds, 1, end))
        begin <- commaLoc + 1 # where second row number begins
        upper.bound <- as.numeric(substr(intBounds, begin, nchar(intBounds)))

        # create lambda vector
        lambda_vector <- seq(lower.bound, upper.bound, by = 0.0001)

        # transform data
        l_lamb <- l_lambda(df[,1], lambda_vector)
        data <- data.frame(cbind(lambda_vector, l_lamb))
        spec_val <- filter(data, lambda_vector == user_lambda) # specific value to add as a point - from user input

        # plot
        g <- ggplot(data = data, aes(x = lambda_vector, y = l_lamb)) + geom_line() + geom_point() +
            geom_point(aes(x = spec_val$lambda_vector, y = spec_val$l_lamb, colour = "red"))
        g
    })

    output$bcshapWilk <- renderText({
        # get the data
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$boxcox)){
            as.numeric(input$boxcox) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame

        # transform the data
        user_lambda <- as.numeric(input$lambda)
        user_lambda <- round(user_lambda, digits = 4)
        x_lamb <- box_cox(df[,1], lambda = user_lambda)

        # perform the Shapiro-Wilk test
        SW <- shapiro.test(x_lamb)
        pVal <- SW$p.value
        paste0("The p-value from the Shapiro-Wilk Test on the Box-Cox tranformed data is ", pVal)
    })

    output$qqplotS <- renderPlot({
        # first, get first plot and clicking working

        # get the data
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$boxcox)){
            as.numeric(input$boxcox) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame

        # do box-cox transformation
        lambda <- as.numeric(input$lambda)
        x_lamb <- box_cox(df[,1], lambda = lambda)

        # create CLICKABLE qqplot
        quant <- q_quantiles(x_lamb)
        df2 <- data.frame(cbind(quant$orderedObs, quant$stndNormQuant))
        names(df2)[1] <- "x_j"
        names(df2)[2] <- "q_j"
        quantPlot <- ggplot(df2, aes(x = q_j, y = x_j)) + geom_point(size = 3) +
            ggtitle("QQ-Plot of Transformed Data")
        quantPlot

    })

    output$qqplotDrop <- renderPlot({
        req(input$plotClick) # stops an error on the filter function

        # get the data
        req(input$file) # gets an annoying error to go away
        #get the data available for use
        df <- if(!is.null(input$file)){
            as.data.frame(read.csv(input$file$datapath, stringsAsFactors = FALSE))
        }
        xNum <- if(!is.null(input$boxcox)){
            as.numeric(input$boxcox) # turn the input into a number to help the subsetting
        }
        xCol <- df[, xNum] # pull first column from the input
        df <- data.frame(xCol) # the variable as a data.frame

        # do box-cox transformation
        lambda <- as.numeric(input$lambda)
        x_lamb <- box_cox(df[,1], lambda = lambda)

        # filter by clicked point
        quant <- q_quantiles(x_lamb)
        df2 <- data.frame(cbind(quant$orderedObs, quant$stndNormQuant))
        names(df2)[1] <- "x_j"
        names(df2)[2] <- "q_j"
        dfDrop <- nearPoints(df2, input$plotClick, maxpoints = 1)
        quant2 <- drop1_quants(x = x_lamb, dfDropPoints = dfDrop)

        # create qqplot with clicked point dropped
        df3 <- data.frame(cbind(quant2$orderedObs, quant2$stndNormQuant))
        names(df3)[1] <- "x_j"
        names(df3)[2] <- "q_j"
        quantPlot <- ggplot(df3, aes(x = q_j, y = x_j)) + geom_point(size = 3) +
            ggtitle("QQ-Plot of Transformed Data Minus Clicked Point")
        quantPlot

    })

}

# Run the application
shinyApp(ui = ui, server = server)

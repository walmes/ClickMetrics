#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-out-05 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Packages.

library(imager)
library(shiny)
# library(shinythemes)

#-----------------------------------------------------------------------
# Global variables passed thought function.

# Set working directory.
setwd(getShinyOption(name = "wd", default = getwd()))

# CSV to write out the click coordinates.
file_CSV <- getShinyOption(name = "csv", default = NULL)
# unlink(file_CSV)

# Images.
choices_IMAGE <- getShinyOption(name = "files", default = NULL)
names(choices_IMAGE) <-
    paste(basename(dirname(choices_IMAGE)),
          basename(choices_IMAGE),
          sep = "/")

# Component names.
choices_COMPONENT <- getShinyOption(name = "labels", default = NULL)

#-----------------------------------------------------------------------
# Blocking used to develop the application.

if (Sys.info()["user"] == "walmes" &&
    interactive() &&
    environmentName(env = environment()) == "R_GlobalEnv") {
    file_CSV <- "mytable.csv"
    choices_IMAGE <- c(
        "~/Projects/ClickMetrics/inst/images/petri-mycelium-1.png",
        "~/Projects/ClickMetrics/inst/hexsticker/ClickMetrics.png")
    choices_COMPONENT <- c("Plate", "Mycelium")
}

#-----------------------------------------------------------------------

# Type of click.
choices_TYPE <- c("Points" = "PT",
                  "Opened polygon" = "OP",
                  "Closed polygon" = "CP")

# Style for buttons.
style_btn <- ".btn {display: block; margin: 0.5em 0;}"

#-----------------------------------------------------------------------
# Frontend.

ui <- fluidPage(
    # theme = shinytheme("yeti"),
    tags$head(tags$style(style_btn)),
    headerPanel(title = "ClickMetrics"),
    sidebarPanel(
        width = 3,
        selectInput(
            inputId = "IMAGE",
            label = "Image:",
            choices = choices_IMAGE),
        fluidRow(
            column(
                width = 6,
                actionButton(
                    inputId = "PREV_IMAGE",
                    label = "Previous image",
                    width = "100%",
                    icon = icon("arrow-left"),
                    class = "btn")),
            column(
                width = 6,
                actionButton(
                    inputId = "NEXT_IMAGE",
                    label = "Next image",
                    width = "100%",
                    icon = icon("arrow-right")))),
        radioButtons(
            inputId = "TYPE",
            label = "Type:",
            choices = choices_TYPE),
        conditionalPanel(
            condition = "input.TYPE == 'OP'",
            numericInput(
                inputId = "OP_PTS_NUMBER",
                label = "Number of points",
                value = 10L,
                min = 2L,
                step = 1L)
        ),
        conditionalPanel(
            condition = "input.TYPE == 'CP'",
            numericInput(
                inputId = "CP_PTS_NUMBER",
                label = "Number of points",
                value = 10L,
                min = 3L,
                step = 1L)
        ),
        checkboxInput(
            inputId = "GET_RGB",
            label = "Get RGB color?"),
        radioButtons(
            inputId = "COMPONENT",
            label = "Component:",
            choices = choices_COMPONENT),
        textInput(
            inputId = "OBSERVATION",
            label = "Observation:",
            value = getShinyOption(name = "obs", default = NA),
            placeholder = "Done by John Smith"),
        actionButton(
            inputId = "REGISTER",
            label = "Record clicks",
            width = "100%",
            icon = icon("file-import"),
            class = "btn btn-success"),
        fluidRow(
            column(
                width = 6,
                actionButton(
                    inputId = "UNDO",
                    label = "Undo",
                    width = "100%",
                    icon = icon("eraser"),
                    class = "btn btn-warning")),
            column(
                width = 6,
                actionButton(
                    inputId = "RESTORE",
                    label = "Restore",
                    width = "100%",
                    icon = icon("undo"),
                    class = "btn btn-info"))),
        actionButton(
            inputId = "EXIT",
            label = "Exit",
            width = "100%",
            icon = icon("times-circle"),
            class = "btn btn-danger"),
        verbatimTextOutput(outputId = "INFO")
    ), # sidebarPanel().
    mainPanel(
        tabsetPanel(
            tabPanel(
                title = "Plot",
                plotOutput(
                    outputId = "PLOT_IMAGE",
                    click = "IMAGE_CLICK",
                    dblclick = "IMAGE_DBLCLICK",
                    width = getShinyOption(name = "width",
                                           default = "800px"),
                    height = getShinyOption(name = "height",
                                            default = "800px")),
                tableOutput(outputId = "TABLE_CLICK")
            ),
            tabPanel(
                title = "Documentation",
                includeMarkdown("README.md")
            )
        ) # tabsetPanel().
    ) # mainPanel().
) # fluidPage().

#-----------------------------------------------------------------------
# Backend.

emptify_vector <- function(vec) {
    stopifnot(!is.null(names(vec)))
    stopifnot(is.list(vec))
    for (i in names(vec)) {
        vec[[i]] <- NULL
    }
    return(vec)
}

replace_null <- function(x, replace) {
    if (is.null(x)) {
        replace
    } else {
        x
    }
}

resize_image <- function(img, max_size = 600) {
    img_dim <- head(dim(img), n = 2)
    if (any(img_dim > max_size)) {
        f <- 100 * max_size/max(img_dim)
        img_res <-
            imager::resize(im = img,
                           size_x = -f,
                           size_y = -f)
        return(img_res)
    } else {
        return(img)
    }
}

server <- function(input, output, session) {
    observeEvent(
        eventExpr = input$EXIT,
        handlerExpr = {
            stopApp()
        })
    CLICKS <- reactiveValues(
        x = NULL,
        y = NULL,
        n = NULL,
        pair = NULL,
        component = NULL,
        observation = NULL,
        image = NULL)
    CONTROL <- reactiveValues(
        image_number = 1,
        pts_number = 3)
    observeEvent(
        eventExpr = input$NEXT_IMAGE,
        handlerExpr = {
            i <- which(input$IMAGE == choices_IMAGE)
            CONTROL$image_number <- min(i + 1, length(choices_IMAGE))
            updateSelectInput(
                session = session,
                inputId = "IMAGE",
                selected = choices_IMAGE[CONTROL$image_number])
        })
    observeEvent(
        eventExpr = input$PREV_IMAGE,
        handlerExpr = {
            i <- which(input$IMAGE == choices_IMAGE)
            CONTROL$image_number <- max(i - 1, 1)
            updateSelectInput(
                session = session,
                inputId = "IMAGE",
                selected = choices_IMAGE[CONTROL$image_number])
        })
    LOAD_IMAGE <- reactive({
        # img <- imager::load.image(choices_IMAGE[2])
        img <- imager::load.image(input$IMAGE)
        resize_image(img = img)
    })
    observeEvent(
        eventExpr = input$IMAGE_CLICK$x,
        handlerExpr = {
            CLICKS$x <- append(CLICKS$x, input$IMAGE_CLICK$x)
            CLICKS$y <- append(CLICKS$y, input$IMAGE_CLICK$y)
            CLICKS$n <- append(CLICKS$n, length(CLICKS$x))
            # CLICKS$pair <-
            #     append(CLICKS$pair,
            #            as.integer(ceiling(length(CLICKS$x)/2)))
            CLICKS$component <-
                append(CLICKS$component,
                       input$COMPONENTE)
            CLICKS$obs <-
                append(CLICKS$obs,
                       input$IDENTIFICACAO)
            CLICKS$image <-
                append(CLICKS$image,
                       input$IMAGEM)
        })
    observeEvent(
        eventExpr = input$UNDO,
        handlerExpr = {
            CLICKS$x <- head(CLICKS$x, n = -1)
            CLICKS$y <- head(CLICKS$y, n = -1)
            CLICKS$n <- head(CLICKS$n, n = -1)
            # CLICKS$pair <-
            #     head(CLICKS$pair, n = -1)
            CLICKS$component <-
                head(CLICKS$component, n = -1)
            CLICKS$obs <-
                head(CLICKS$obs, n = -1)
            CLICKS$image <-
                head(CLICKS$image, n = -1)
        })
    observeEvent(
        eventExpr = input$RESTORE,
        handlerExpr = {
            CLICKS <- emptify_vector(CLICKS)
        })
    # observeEvent(
    #     eventExpr = input$IMAGE_DBLCLICK$x,
    #     handlerExpr = {
    #         u <- which(input$COMPONENTE == choices_COMPONENTE)
    #         u <- (u %% length(choices_COMPONENTE)) + 1
    #         updateRadioButtons(
    #             session,
    #             inputId = "COMPONENTE",
    #             selected   = choices_COMPONENTE[u])
    #     })
    # TB_CLICKS <- reactive({
    #     if (is.null(CLICKS$x)) {
    #         NULL
    #     } else {
    #         tb <- data.frame(
    #             x = CLICKS$x,
    #             y = CLICKS$y,
    #             n = CLICKS$n,
    #             pair = CLICKS$pair,
    #             component = CLICKS$component,
    #             obs = CLICKS$obs,
    #             image = CLICKS$image,
    #             stringsAsFactors = FALSE)
    #         return(tb)
    #     }
    # })
    # observeEvent(
    #     eventExpr = input$REGISTRAR,
    #     handlerExpr = {
    #         fe <- file.exists(file_CSV)
    #         tb <- TB_CLICKS()
    #         if (!is.null(tb) && nrow(tb) > 0) {
    #             tb <- cbind(ts = as.integer(Sys.time()),
    #                         tb)
    #             write.table(tb,
    #                         file = file_CSV,
    #                         sep = ",",
    #                         row.names = FALSE,
    #                         col.names = !fe,
    #                         append = fe)
    #         }
    #         CLICKS <- emptify_vector(CLICKS)
    #         u <- which(choices_IMAGEM == input$IMAGEM)
    #         u <- min(u + 1, length(choices_IMAGEM))
    #         updateSelectInput(
    #             session,
    #             inputId = "IMAGEM",
    #             selected = choices_IMAGEM[u])
    #     })
    output$PLOT_IMAGE <- renderPlot(
        expr = {
            img <- LOAD_IMAGE()
            par(mar = c(0.5, 0.5, 1.75, 0.5))
            plot(img, axes = FALSE)
            box(col = "gray")
            mtext(
                text = input$IMAGE, side = 3,
                line = 0.5, adj = 0.5, cex = 1.25)
            if (!is.null(CLICKS$x) && length(CLICKS$x) > 0) {
                points(
                    x = CLICKS$x, y = CLICKS$y,
                    pch = 19, cex = 0.75, col = "red")
                text(
                    x = CLICKS$x, y = CLICKS$y,
                    label = CLICKS$n, pos = 3)
                # n_par <- 2 * floor(length(CLICKS$x)/2)
                # tb_pairs <- cbind(
                #     matrix(CLICKS$x[1:n_par], ncol = 2, byrow = TRUE),
                #     matrix(CLICKS$y[1:n_par], ncol = 2, byrow = TRUE))
                # segments(x0 = tb_pairs[, 1],
                #          x1 = tb_pairs[, 2],
                #          y0 = tb_pairs[, 3],
                #          y1 = tb_pairs[, 4],
                #          col = "yellow")
            } # if ()
        } # expr =
    )
    # output$TABELA <- renderTable({
    #     TB_CLICKS()
    # })
    output$INFO <- renderText({
        input$IMAGE_CLICK$x
        fmt <- paste(sep = "\n",
                     "x: %0.5f",
                     "y: %0.5f",
                     "Clicks: %d",
                     "Type: %s",
                     "Number: %d",
                     "RGB: %d",
                     "Image: %s",
                     "Image number: %d",
                     "Component: %s",
                     "Obs: %s")
        sprintf(fmt = fmt,
                replace_null(tail(CLICKS$x, n = 1), NA_real_),
                replace_null(tail(CLICKS$y, n = 1), NA_real_),
                replace_null(length(CLICKS$x), 0),
                input$TYPE,
                CLICKS$pts_number,
                input$GET_RGB,
                input$IMAGE,
                CLICKS$image_number,
                input$COMPONENT,
                input$OBSERVATION)
    })
}

#-----------------------------------------------------------------------
# Calls application.

shinyApp(ui, server)

#-----------------------------------------------------------------------

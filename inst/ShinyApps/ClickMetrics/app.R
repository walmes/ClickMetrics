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
library(shinyjs)
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
    names(choices_IMAGE) <-
        paste(basename(dirname(choices_IMAGE)),
              basename(choices_IMAGE),
              sep = "/")
    choices_COMPONENT <- c("Plate", "Mycelium")
}

#-----------------------------------------------------------------------

# Type of click.
choices_TYPE <- c("Points" = "point",
                  "Segments" = "segment",
                  "Polygons" = "polygon")

# CSS Style rules.
style_btn <- ".btn {display: block; margin: 0.5em 0;}"
style_GROUP <- "#GROUP {display: block; margin: 1em 0;}"

#-----------------------------------------------------------------------
# Frontend.

ui <- fluidPage(
    shinyjs::useShinyjs(),
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
        numericInput(
            inputId = "MAX_PTS",
            label = "Number of points per group",
            value = 10L,
            min = 1L,
            step = 1L),
        checkboxInput(
            inputId = "GET_RGB",
            label = "Get RGB color?"),
        radioButtons(
            inputId = "COMPONENT",
            label = "Component:",
            choices = choices_COMPONENT),
        tags$head(tags$style(style_GROUP)),
        htmlOutput(outputId = "GROUP", container = div),
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

draw_clicks <- function(tb_clicks) {
    with(tb_clicks,
         points(x = x, y = y, col = "red", pch = 19, cex = 0.75))
    with(tb_clicks,
         text(x = x, y = y, labels = n, pos = 3))
    by(data = tb_clicks,
       INDICES = tb_clicks$group,
       FUN = function(g) {
           if (g$type[1] != "point" && nrow(g) > 1) {
               if (g$type[1] == "segment") {
                   segments(x0 = head(g$x, n = -1),
                            y0 = head(g$y, n = -1),
                            x1 = tail(g$x, n = -1),
                            y1 = tail(g$y, n = -1))
               } else if (g$type[1] == "polygon") {
                   polygon(x = g$x,
                           y = g$y,
                           lty = 2)
               }
           }
       })
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
        group = NULL,
        type = NULL,
        component = NULL,
        observation = NULL,
        image = NULL)
    CONTROL <- reactiveValues(
        type_activated = TRUE,
        image_number = 1L,
        group = 1L)
    observeEvent(
        eventExpr = input$NEXT_IMAGE,
        handlerExpr = {
            # CONTROL$group <- 1L
            i <- which(input$IMAGE == choices_IMAGE)
            CONTROL$image_number <- min(i + 1L, length(choices_IMAGE))
            updateSelectInput(
                session = session,
                inputId = "IMAGE",
                selected = choices_IMAGE[CONTROL$image_number])
        })
    observeEvent(
        eventExpr = input$PREV_IMAGE,
        handlerExpr = {
            # CONTROL$group <- 1L
            i <- which(input$IMAGE == choices_IMAGE)
            CONTROL$image_number <- max(i - 1L, 1L)
            updateSelectInput(
                session = session,
                inputId = "IMAGE",
                selected = choices_IMAGE[CONTROL$image_number])
        })
    LOAD_IMAGE <- reactive({
        # img <- imager::load.image(choices_IMAGE[2])
        img <- imager::load.image(input$IMAGE)
        CONTROL$group <- 1L
        CONTROL$type_activated <- TRUE
        shinyjs::enable(id = "TYPE")
        resize_image(img = img)
    })
    observeEvent(
        eventExpr = input$IMAGE_CLICK$x,
        handlerExpr = {
            # if (!is.null(CLICKS$x)) {
            if (CONTROL$type_activated && !is.null(input$IMAGE_CLICK$x)) {
                CONTROL$type_activated <- FALSE
                shinyjs::disable(id = "TYPE")
            }
            CLICKS$x <- append(CLICKS$x, input$IMAGE_CLICK$x)
            CLICKS$y <- append(CLICKS$y, input$IMAGE_CLICK$y)
            CLICKS$n <- append(CLICKS$n, length(CLICKS$x))
            CLICKS$group <-
                append(CLICKS$group, CONTROL$group)
            CLICKS$type <-
                append(CLICKS$type,
                       input$TYPE)
            CLICKS$component <-
                append(CLICKS$component,
                       input$COMPONENT)
            CLICKS$observation <-
                append(CLICKS$observation,
                       input$OBSERVATION)
            CLICKS$image <-
                append(CLICKS$image,
                       input$IMAGE)
        })
    observeEvent(
        eventExpr = input$UNDO,
        handlerExpr = {
            CLICKS$x <- head(CLICKS$x, n = -1)
            CLICKS$y <- head(CLICKS$y, n = -1)
            CLICKS$n <- head(CLICKS$n, n = -1)
            CLICKS$group <-
                head(CLICKS$group, n = -1)
            CLICKS$type <-
                head(CLICKS$type, n = -1)
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
            CONTROL$type_activated <- TRUE
            shinyjs::enable(id = "TYPE")
            CONTROL$group <- 1L
            CLICKS <- emptify_vector(CLICKS)
        })
    observeEvent(
        eventExpr = input$IMAGE_DBLCLICK$x,
        handlerExpr = {
            CONTROL$type_activated <- TRUE
            shinyjs::enable(id = "TYPE")
            CONTROL$group <- CONTROL$group + 1L
        })
    observeEvent(
        eventExpr = input$IMAGE_CLICK$x,
        handlerExpr = {
            cond <- sum(CLICKS$group == CONTROL$group) == input$MAX_PTS
            if (cond) {
                CONTROL$type_activated <- TRUE
                shinyjs::enable(id = "TYPE")
                CONTROL$group <- CONTROL$group + 1L
            }
        })
    output$GROUP <- renderText({
        sprintf("<strong>Group</strong>: %d", CONTROL$group)
    })
    TB_CLICKS <- reactive({
        if (is.null(CLICKS$x)) {
            NULL
        } else {
            tb <- as.data.frame(reactiveValuesToList(CLICKS),
                                stringsAsFactors = FALSE)
            col <- c("image", "component", "x", "y",
                     "type", "group", "n", "observation")
            return(tb[, col])
        }
    })
    output$PLOT_IMAGE <- renderPlot({
        img <- LOAD_IMAGE()
        par(mar = c(0.5, 0.5, 1.75, 0.5))
        plot(img, axes = FALSE)
        box(col = "gray")
        mtext(
            text = input$IMAGE, side = 3,
            line = 0.5, adj = 0.5, cex = 1.25)
        tb_clicks <- TB_CLICKS()
        if (!is.null(tb_clicks)) {
            draw_clicks(tb_clicks)
        }
    }) # renderPlot()
    output$TABLE_CLICK <- renderTable({
        TB_CLICKS()
    })
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
                     "Group: %d",
                     "Obs: %s")
        sprintf(fmt = fmt,
                replace_null(tail(CLICKS$x, n = 1), NA_real_),
                replace_null(tail(CLICKS$y, n = 1), NA_real_),
                replace_null(length(CLICKS$x), 0),
                input$TYPE,
                input$MAX_PTS,
                input$GET_RGB,
                input$IMAGE,
                CONTROL$image_number,
                input$COMPONENT,
                CONTROL$group,
                input$OBSERVATION)
    })
    observeEvent(
        eventExpr = input$REGISTER,
        handlerExpr = {
            fe <- file.exists(file_CSV)
            tb <- TB_CLICKS()
            if (!is.null(tb) && nrow(tb) > 0) {
                tb <- cbind(ts = as.integer(Sys.time()),
                            tb)
                write.table(tb,
                            file = file_CSV,
                            sep = ",",
                            row.names = FALSE,
                            col.names = !fe,
                            append = fe)
            }
            CLICKS <- emptify_vector(CLICKS)
            CONTROL$image_number <- min(CONTROL$image_number + 1L,
                                        length(choices_IMAGE))
            updateSelectInput(
                session = session,
                inputId = "IMAGE",
                selected = choices_IMAGE[CONTROL$image_number])
            CONTROL$type_activated <- TRUE
            shinyjs::enable(id = "TYPE")
            CONTROL$group <- 1L
        })
}

#-----------------------------------------------------------------------
# Calls application.

shinyApp(ui, server)

#-----------------------------------------------------------------------

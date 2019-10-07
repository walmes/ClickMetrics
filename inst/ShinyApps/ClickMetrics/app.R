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

# rm(list = objects())

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
style_imagebuttons <- "
.image-buttons {
    display: block;
    margin-top: -20px;
    margin-bottom: 15px;
}"
style_radiotype <- "#TYPE
.radio-inline { width: 30%; }
.radio-inline + .radio-inline { margin-left: 1%; }
"

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
        tags$head(tags$style(style_imagebuttons)),
        fluidRow(
            class = "image-buttons",
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
        hr(),
        checkboxInput(
            inputId = "PICK_RGB",
            label = "Pick color at clicks?"),
        tags$head(tags$style(style_radiotype)),
        radioButtons(
            inputId = "TYPE",
            label = "Type:",
            choices = choices_TYPE,
            inline = TRUE),
        numericInput(
            inputId = "MAX_PTS",
            label = "Number of points per group",
            value = 10L,
            min = 1L,
            step = 1L),
        tags$head(tags$style(style_GROUP)),
        htmlOutput(outputId = "GROUP", container = div),
        hr(),
        radioButtons(
            inputId = "COMPONENT",
            label = "Component:",
            choices = choices_COMPONENT),
        textInput(
            inputId = "OBSERVATION",
            label = "Observation:",
            value = getShinyOption(name = "obs", default = NA),
            placeholder = "Done with ClickMetrics"),
        hr(),
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
    with(tb_clicks,
         points(x = x, y = y, col = "red", pch = 19, cex = 0.75))
    with(tb_clicks,
         text(x = x, y = y, labels = n, pos = 3))
}

enable_widgets <- function() {
    enable(id = "TYPE")
    enable(id = "IMAGE")
    enable(id = "NEXT_IMAGE")
    enable(id = "PREV_IMAGE")
}

disable_widgets <- function() {
    disable(id = "TYPE")
    disable(id = "IMAGE")
    disable(id = "NEXT_IMAGE")
    disable(id = "PREV_IMAGE")
}

pick_rgb <- function(img, x, y) {
    rgb_color <- img[round(x)[1], round(y)[1], 1, ]
    # img_array <- as.array(img)
    # rgb_color <- img_array[round(x)[1], round(y)[1], 1, ]
    color <- rgb(red = rgb_color[1],
                 green = rgb_color[2],
                 blue = rgb_color[3],
                 alpha = rgb_color[4])
    return(color)
}

server <- function(input, output, session) {
    observeEvent(
        label = "observeEvent > input$EXIT",
        eventExpr = input$EXIT,
        handlerExpr = {
            stopApp()
        })
    CLICKS <- reactiveValues(
        x = NULL,
        y = NULL,
        n = NULL,
        color = NULL,
        group = NULL,
        type = NULL,
        component = NULL,
        observation = NULL,
        image = NULL)
    CONTROL <- reactiveValues(
        control_activated = TRUE,
        image_number = 1L,
        group = 1L)
    observeEvent(
        label = "observeEvent > input$NEXT_IMAGE",
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
        label = "observeEvent > input$PREV_IMAGE",
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
    LOAD_IMAGE <- reactive(
        label = "reactive > LOAD_IMAGE",
        x = {
            # img <- imager::load.image(choices_IMAGE[2])
            img <- imager::load.image(input$IMAGE)
            CONTROL$group <- 1L
            CONTROL$control_activated <- TRUE
            enable_widgets()
            resize_image(img = img)
        })
    observeEvent(
        label = "observeEvent > input$IMAGE_CLICK",
        eventExpr = input$IMAGE_CLICK,
        handlerExpr = {
            # if (!is.null(CLICKS$x)) {
            if (CONTROL$control_activated &&
                !is.null(input$IMAGE_CLICK$x)) {
                CONTROL$control_activated <- FALSE
                disable_widgets()
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
            if (!is.null(input$IMAGE_CLICK)) {
                if (isolate(input$PICK_RGB)) {
                    img <- LOAD_IMAGE()
                    color <-
                        pick_rgb(img = img,
                                 x = input$IMAGE_CLICK$x,
                                 y = input$IMAGE_CLICK$y)
                } else {
                    color <- NA
                }
                CLICKS$color <- append(CLICKS$color, color)
            }
        })
    observeEvent(
        label = "observeEvent > input$UNDO",
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
            CLICKS$observation <-
                head(CLICKS$observation, n = -1)
            CLICKS$image <-
                head(CLICKS$image, n = -1)
            CLICKS$color <-
                head(CLICKS$color, n = -1)
        })
    observeEvent(
        label = "observeEvent > input$RESTORE",
        eventExpr = input$RESTORE,
        handlerExpr = {
            CONTROL$control_activated <- TRUE
            enable_widgets()
            CONTROL$group <- 1L
            CLICKS <- emptify_vector(CLICKS)
        })
    observeEvent(
        label = "observeEvent > input$IMAGE_DBLCLICK",
        eventExpr = input$IMAGE_DBLCLICK,
        handlerExpr = {
            CONTROL$control_activated <- TRUE
            enable_widgets()
            CONTROL$group <- CONTROL$group + 1L
        })
    observeEvent(
        label = "observeEvent > input$IMAGE_CLICK",
        eventExpr = input$IMAGE_CLICK,
        handlerExpr = {
            cond <- sum(CLICKS$group == CONTROL$group) == input$MAX_PTS
            if (cond) {
                CONTROL$control_activated <- TRUE
                enable_widgets()
                CONTROL$group <- CONTROL$group + 1L
            }
        })
    output$GROUP <- renderText({
        sprintf("<strong>Group</strong>: %d", CONTROL$group)
    })
    TB_CLICKS <- reactive(
        label = "reactive > TB_CLICKS",
        x = {
            if (is.null(CLICKS$x)) {
                NULL
            } else {
                L <- reactiveValuesToList(CLICKS)
                col <- c("image", "component", "x", "y", "color",
                         "type", "group", "n", "observation")
                tb <- as.data.frame(L[col],
                                    stringsAsFactors = FALSE)
                return(tb)
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
    # observeEvent(
    #     eventExpr = input$IMAGE_CLICK,
    #     handlerExpr = {
    #         img <- LOAD_IMAGE()
    #         if (!is.null(input$IMAGE_CLICK)) {
    #             if (isolate(input$PICK_RGB)) {
    #                 color <-
    #                     pick_rgb(img = img,
    #                              x = input$IMAGE_CLICK$x,
    #                              y = input$IMAGE_CLICK$y)
    #             } else {
    #                 color <- NA
    #             }
    #             CLICKS$color <- append(CLICKS$color, color)
    #         }
    #     })
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
                     "Color: %s",
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
                replace_null(tail(CLICKS$color, n = 1), NA_character_),
                input$MAX_PTS,
                input$PICK_RGB,
                input$IMAGE,
                CONTROL$image_number,
                input$COMPONENT,
                CONTROL$group,
                input$OBSERVATION)
    })
    observeEvent(
        label = "observeEvent > input$REGISTER",
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
            CONTROL$control_activated <- TRUE
            enable_widgets()
            CONTROL$group <- 1L
        })
}

#-----------------------------------------------------------------------
# Calls application.

shinyApp(ui, server)

#-----------------------------------------------------------------------

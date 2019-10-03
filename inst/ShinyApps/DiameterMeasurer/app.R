#-----------------------------------------------------------------------
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-set-29 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

library(imager)
library(shiny)
# library(shinythemes)

#-----------------------------------------------------------------------
# Variáveis globais imutáveis a serem passadas para a aplicação.

setwd(getShinyOption(name = "wd", default = getwd()))

# Nome para o arquivo de registro.
file_CSV <- getShinyOption(name = "csv", default = NULL)
# unlink(file_CSV)

# Conjunto de imagens.
choices_IMAGEM <- getShinyOption(name = "files", default = NULL)
names(choices_IMAGEM) <-
    paste(basename(dirname(choices_IMAGEM)),
          basename(choices_IMAGEM),
          sep = "/")

# Nome dos tratamentos.
choices_COMPONENTE <- getShinyOption(name = "labels", default = NULL)

#-----------------------------------------------------------------------

# Bloco usado para desenvolver a aplicação.
# if (interactive() && Sys.info()["user"] == "walmes") {
#     choices_COMPONENTE <- c("Plate", "Mycelium")
#     choices_IMAGEM <- dir(system.file("images", package = "ClickMetrics"),
#                           full.names = TRUE)
#     file_CSV <- "mytable.csv"
# }

# Estilo para os botões.
style_btn <- ".btn {display: block; margin: 0.5em 0;}"

#-----------------------------------------------------------------------
# Frontend.

ui <- fluidPage(
    # theme = shinytheme("yeti"),
    headerPanel(title = "Diameter measurament"),
    sidebarPanel(
        width = 3,
        selectInput(
            inputId = "IMAGEM",
            label = "Image:",
            choices = choices_IMAGEM),
        radioButtons(
            inputId = "COMPONENTE",
            label = "Component:",
            choices = choices_COMPONENTE),
        textInput(
            inputId = "IDENTIFICACAO",
            label = "Observation:",
            value = getShinyOption(name = "obs", default = NA),
            placeholder = "Done by John Smith"),
        tags$head(tags$style(style_btn)),
        actionButton(
            inputId = "REGISTRAR",
            label = "Record clicks",
            width = "100%",
            icon = icon("file-import"),
            # icon = icon("sdcard"),
            # icon = icon("pen"),
            class = "btn btn-success"),
        fluidRow(
            column(
                width = 6,
                actionButton(
                    inputId = "DESFAZER",
                    label = "Undo",
                    width = "100%",
                    icon = icon("eraser"),
                    class = "btn btn-warning")),
            column(
                width = 6,
                actionButton(
                    inputId = "RESTAURAR",
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
        plotOutput(
            outputId = "PLOT_IMAGEM",
            click = "IMAGE_CLICK",
            dblclick = "IMAGE_DBLCLICK",
            width = getShinyOption(name = "width", default = "800px"),
            height = getShinyOption(name = "height", default = "800px")),
        tableOutput(outputId = "TABELA")
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
        Component = NULL,
        Identification = NULL,
        Image = NULL)
    observeEvent(
        eventExpr = input$IMAGE_CLICK$x,
        handlerExpr = {
            CLICKS$x <- append(CLICKS$x, input$IMAGE_CLICK$x)
            CLICKS$y <- append(CLICKS$y, input$IMAGE_CLICK$y)
            CLICKS$n <- append(CLICKS$n, length(CLICKS$x))
            CLICKS$pair <-
                append(CLICKS$pair,
                       as.integer(ceiling(length(CLICKS$x)/2)))
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
        eventExpr = input$IMAGE_DBLCLICK$x,
        handlerExpr = {
            u <- which(input$COMPONENTE == choices_COMPONENTE)
            u <- (u %% length(choices_COMPONENTE)) + 1
            updateRadioButtons(
                session,
                inputId = "COMPONENTE",
                selected   = choices_COMPONENTE[u])
        })
    observeEvent(
        eventExpr = input$DESFAZER,
        handlerExpr = {
            CLICKS$x <- head(CLICKS$x, n = -1)
            CLICKS$y <- head(CLICKS$y, n = -1)
            CLICKS$n <- head(CLICKS$n, n = -1)
            CLICKS$pair <-
                head(CLICKS$pair, n = -1)
            CLICKS$component <-
                head(CLICKS$component, n = -1)
            CLICKS$obs <-
                head(CLICKS$obs, n = -1)
            CLICKS$image <-
                head(CLICKS$image, n = -1)
        })
    observeEvent(
        eventExpr = input$RESTAURAR,
        handlerExpr = {
            CLICKS <- emptify_vector(CLICKS)
        })
    TB_CLICKS <- reactive({
        if (is.null(CLICKS$x)) {
            NULL
        } else {
            tb <- data.frame(
                x = CLICKS$x,
                y = CLICKS$y,
                n = CLICKS$n,
                pair = CLICKS$pair,
                component = CLICKS$component,
                obs = CLICKS$obs,
                image = CLICKS$image,
                stringsAsFactors = FALSE)
            return(tb)
        }
    })
    observeEvent(
        eventExpr = input$REGISTRAR,
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
            u <- which(choices_IMAGEM == input$IMAGEM)
            u <- min(u + 1, length(choices_IMAGEM))
            updateSelectInput(
                session,
                inputId = "IMAGEM",
                selected = choices_IMAGEM[u])
        })
    output$PLOT_IMAGEM <- renderPlot(
        expr = {
            img <- imager::load.image(input$IMAGEM)
            par(mar = c(0.5, 0.5, 1.75, 0.5))
            plot(img, axes = FALSE)
            box(col = "gray")
            mtext(text = input$IMAGEM,
                  side = 3,
                  line = 0.5,
                  adj = 0.5,
                  cex = 1.25)
            if (!is.null(CLICKS$x) && length(CLICKS$x) > 0) {
                points(x = CLICKS$x,
                       y = CLICKS$y,
                       pch = 19,
                       cex = 0.75,
                       col = "red")
                text(x = CLICKS$x,
                     y = CLICKS$y,
                     label = CLICKS$n,
                     pos = 3)
                n_par <- 2 * floor(length(CLICKS$x)/2)
                tb_pairs <- cbind(
                    matrix(CLICKS$x[1:n_par], ncol = 2, byrow = TRUE),
                    matrix(CLICKS$y[1:n_par], ncol = 2, byrow = TRUE))
                segments(x0 = tb_pairs[, 1],
                         x1 = tb_pairs[, 2],
                         y0 = tb_pairs[, 3],
                         y1 = tb_pairs[, 4],
                         col = "yellow")
            } # if ()
        } # expr =
    )
    output$TABELA <- renderTable({
        TB_CLICKS()
    })
    output$INFO <- renderText({
        input$IMAGE_CLICK$x
        fmt <- paste(sep = "\n",
                     "x: %0.5f",
                     "y: %0.5f",
                     "Image: %s",
                     "Component: %s",
                     "Obs: %s",
                     "Clicks: %d")
        sprintf(fmt = fmt,
                replace_null(tail(CLICKS$x, n = 1), NA_real_),
                replace_null(tail(CLICKS$y, n = 1), NA_real_),
                input$IMAGEM,
                input$COMPONENTE,
                input$IDENTIFICACAO,
                replace_null(length(CLICKS$x), 0))
    })
}

#-----------------------------------------------------------------------
# Chama a aplicação.

shinyApp(ui, server)

#-----------------------------------------------------------------------

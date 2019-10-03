#' @export
#' @import shiny
#' @author Walmes Zeviani.
#' @title Shiny dashboard to measure length between click pairs
#' @description This function launch a \pkg{shiny} application in
#'     browser to measure the distance between a pair of clicks. See
#'     section Details for usage of the application.
#' @param files \code{character[>0]} Path to images that will be loaded
#'     by \code{\link[imager]{load.image}()}.
#' @param labels \code{character[>0]} Names that represent treatments
#'     for the case when phots are from an experiments.
#' @param obs \code{character[1]} Any name used to identify the records,
#'     e.g. the user that made the clicks.
#' @param csv \code{character[1]} Path to save a csv with recorded
#'     clicks. If \code{NULL}, the file is written in a temporary
#'     directory.
#' @param width \code{character[1]} Value passad as argumento to
#'     \code{width} parameter of \code{\link[shiny]{plotOutput}()}
#'     function.
#' @param height \code{character[1]} Value passad as argumento to
#'     \code{height} parameter of \code{\link[shiny]{plotOutput}()}
#'     function.
#' @param app_path \code{character[1]} (optional, default is
#'     \code{NULL}) A path for shiny application is case of
#'     improving/developing the existing one.
#' @return The function creates a CSV file with recorded clicks. After
#'     pressing \emph{Exit} button, a \code{data.frame} is returned. It
#'     contains all click coordinates.
#' @details \describe{
#'
#' \item{Sidebar panel}{Contains widgets to user interact with.}
#'
#' \item{Main panel}{Shows the image and a table with clicks already
#'     done. Each click adds a dot on the plot. A pair of consective
#'     cliks are linked by a line segment.}
#'
#' \item{\emph{Image} dropdown list}{Selects the image to be measured. A
#'     column named \code{image} in the returned \code{data.frame} will
#'     contain this information.}
#'
#' \item{\emph{Component} radio buttons}{Selects the treament, condition
#'     or component associated with the clicks. A column named
#'     \code{component} in the returned \code{data.frame} will contain
#'     this information. \strong{Note}: double click in the image cycles
#'     among itens, so, a double click move to the next component in the
#'     list.}
#'
#' \item{\emph{Observation} text input}{A text field used to inform
#'     anything, like the user name (a global information), or the color
#'     of the mycelium, the growth temperature (local
#'     information). \strong{Note}: this information must be filled
#'     before the clicks. A column named \code{obs} in the returned
#'     \code{data.frame} will contain this information.}
#'
#' \item{\emph{Record clicks} button}{It triggers \code{write.csv()} to
#'     save clicks in disc. The button should be pressed after all
#'     clicks have been made. A column named \code{ts} in the returned
#'     \code{data.frame} will contain the time stamp of the recorded
#'     data.}
#'
#' \item{\emph{Undo} button}{Undoes the last click.}
#'
#' \item{\emph{Restore} button}{Clean all clicks.}
#'
#' \item{\emph{Exit} button}{Stops the application.}
#'
#' }
#' @examples
#'
#' \dontrun{
#'
#' files <- dir(path = system.file("images", package = "ClickMetrics"),
#'              pattern = "^petri.*\\.png$",
#'              full.names = TRUE)
#' files
#'
#' tb <- diameter_measurer(files = files,
#'                         labels = c("Plate", "Trat1", "Trat2"),
#'                         obs = "Done with ClickMetrics",
#'                         csv = "my_clicks.csv")
#' str(tb)
#'
#' with(tb, by(cbind(x, y),
#'             INDICES = pair,
#'             FUN = dist))
#'
#' }
#'
diameter_measurer <- function(files,
                              labels = "Plate",
                              obs = "Done with ClickMetrics",
                              csv = "my_clicks.csv",
                              width = "800px",
                              height = "800px",
                              app_path = NULL) {

    if (!requireNamespace(package = "shiny", quietly = TRUE)) {
        stop(paste("`shiny` package not found.",
                   "Please, install it."),
             call. = FALSE)
    }

    # if (!requireNamespace(package = "imager", quietly = TRUE)) {
    #     stop(paste("`imager` package not found.",
    #                "Please, install it."),
    #          call. = FALSE)
    # }

    if (is.null(app_path)) {
        # Endereço da aplicação shiny na raíz do pacote/projeto.
        app_path <- "ShinyApps/DiameterMeasurer"
        appDir <- system.file(app_path, package = "ClickMetrics")
        # Verifica existência do diretório.
        if (!dir.exists(appDir)) {
            stop(paste(
                "Directory not found for `DiameterMeasurer`.",
                "Please, try to reinstall `ClickMetrics`."),
                call. = FALSE)
        }
    } else {
        appDir <- app_path
        if (!dir.exists(appDir)) {
            stop(paste(
                "Directory not found."),
                call. = FALSE)
        }
    }

    if (missing(files)) {
        stop(paste("`files` argument must be provided."))
    }

    if (!all(file.exists(files))) {
        stop("Some files were not found.")
    }

    if (!(length(labels) > 0 && is.character(labels))) {
        stop("`labels` must be non empty character vector.")
    }

    if (!(length(obs) > 0 && is.character(obs))) {
        stop("`obs` must be non empty character vector.")
    }

    if (is.null(csv)) {
        csv <- tempfile(fileext = ".csv")
    } else if (!(length(csv) == 1 && is.character(labels))) {
        stop("When provided, `csv` must be a length one character vector.")
    }

    # Passa parâmetrod para a aplicação shiny.
    shiny::shinyOptions(files = files,
                        labels = labels,
                        obs = obs,
                        csv = csv,
                        wd = getwd())

    # Chama a aplicação.
    shiny::runApp(appDir, display.mode = "normal")

    # Lê a tabela que foi gerada.
    tb <- utils::read.csv(file = csv,
                          stringsAsFactors = FALSE)
    tb$ts <- base::as.POSIXct(tb$ts, origin = "1960-01-01")
    return(tb)

}

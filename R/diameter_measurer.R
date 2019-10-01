#' @export
#' @import shiny
#' @author Walmes Zeviani.
#' @title Shiny dashboard to measure micelial length
#' @description This function launch a \pkg{shiny} application in
#'     browser to measure micelial length on petri dishes.
#' @param files \code{character[>0]} Path to images that will be loaded
#'     by \code{\link[imager]{load.image}()}.
#' @param labels \code{character[>0]} Names that represent treatments
#'     for the case when phots are from an experiments.
#' @param obs \code{character[>0]} Any name used to identify the
#'     records, e.g. the user that made the clicks.
#' @param csv \code{character[>0]} Path to save a csv with recorded
#'     clicks.
#' @param app_path \code{character[>0]} (optional, default is
#'     \code{NULL}) A path for shiny application is case of
#'     improving/developing the existing one.
#' @return The function creates a CSV file with recorded clicks.
#' @examples
#'
#' \dontrun{
#'
#' files <- dir(path = system.file("images", package = "ClickMetrics"),
#'              pattern = "\\.png$",
#'              full.names = TRUE)
#'
#' diameter_measurer(
#'     files = files,
#'     labels = c("Plate", "Trat1"),
#'     obs = "Done by Walmes",
#'     csv = "my_clicks.csv")
#'
#' }
diameter_measurer <- function(files,
                              labels = "Plate",
                              obs = Sys.info()["user"],
                              csv = "my_clicks.csv",
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

    if (!(length(csv) == 1 && is.character(labels))) {
        stop("`csv` must be a length one character vector.")
    }

    # Passa parâmetrod para a aplicação shiny.
    shiny::shinyOptions(files = files,
                        labels = labels,
                        obs = obs,
                        csv = csv,
                        wd = getwd())

    # Chama a aplicação.
    shiny::runApp(appDir, display.mode = "normal")

}

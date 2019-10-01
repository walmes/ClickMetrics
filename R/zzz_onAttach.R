.onAttach <- function(lib, pkg) {

    pkg.info <- drop(read.dcf(
        file = system.file("DESCRIPTION", package = "ClickMetrics"),
        fields = c("Package", "Title", "Version", "Date", "URL")
    ))

    dashes <- paste0(rep("----------", times = 7), collapse = "")

    line1 <- sprintf("%s (%s)%s was loaded.",
                     pkg.info["Package"],
                     pkg.info["Version"],
                     ifelse(is.na(pkg.info["Date"]),
                            yes = "",
                            no = sprintf(" built on %s",
                                         pkg.info["Date"])))

    line2 <- "For information, execute: packageDescription(\"ClickMetrics\")."

    line3 <- "To access documentation, execute: help(package = \"ClickMetrics\")"

    packageStartupMessage(paste0(dashes, "\n  ",
                                 line1, "\n  ",
                                 line2, "\n  ",
                                 line3, "\n",
                                 dashes))

}
# cat(stringi::stri_escape_unicode("informações"), "\n")
# cat(stringi::stri_escape_unicode("documentação"), "\n")

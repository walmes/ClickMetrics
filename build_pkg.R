#-----------------------------------------------------------------------
# Construção do pacote.

(find-file "~/Projects/ClickMetrics/DESCRIPTION")
(find-file "~/Projects/ClickMetrics/NAMESPACE")

# Carrega conteúdo do pacote.
devtools::load_all()

wzRfun::pkgs_versions("ClickMetrics")

# Gera a documentação e escreve o NAMESPACE.
devtools::document()

# Faz verificação de conformidade do pacote.
# devtools::check(env_vars = c("_R_CHECK_LICENSE_" = FALSE))
devtools::check()

# Gera o tar.gz do pacote.
devtools::build(vignettes = FALSE,
                manual = FALSE)

# Instala o pacote.
devtools::install(reload = TRUE,
                  build_vignettes = FALSE,
                  dependencies = FALSE,
                  upgrade_dependencies = FALSE)

devtools::session_info()
sessionInfo()

#-----------------------------------------------------------------------
# Inspecina o pacote.

quit("no")

library(ClickMetrics)
packageVersion("ClickMetrics")
packageDescription("ClickMetrics")

ls("package:ClickMetrics")

help(package = "ClickMetrics")

rm(list = objects())
ls()

#-----------------------------------------------------------------------
# Zona de testes.

files <- dir(path = system.file("images", package = "ClickMetrics"),
             pattern = "\\.png$",
             full.names = TRUE)

diameter_measurer(
    files = files,
    labels = c("Plate", "Trat1"),
    obs = "Done by Walmes",
    csv = "my_clicks.csv")

#-----------------------------------------------------------------------

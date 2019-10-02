#-----------------------------------------------------------------------
# Construção do pacote.

(find-file "~/Projects/ClickMetrics/DESCRIPTION")
(find-file "~/Projects/ClickMetrics/NAMESPACE")

# Carrega conteúdo do pacote.
devtools::load_all()

# wzRfun::pkgs_versions("ClickMetrics")

# Gera a documentação e escreve o NAMESPACE.
devtools::document()

# Faz verificação de conformidade do pacote.
# devtools::check(env_vars = c("_R_CHECK_LICENSE_" = FALSE))
devtools::check(args = "--no-build-vignettes",
                build_args = "--no-build-vignettes",
                env_vars = c("_R_CHECK_LICENSE_" = FALSE))

# Gera o tar.gz do pacote.
devtools::build(vignettes = FALSE,
                manual = FALSE)

# Gera as vinhetas.
# build_vignettes(dependencies = FALSE)

# Instala o pacote.
devtools::install(reload = TRUE,
                  build_vignettes = FALSE,
                  dependencies = FALSE,
                  upgrade_dependencies = FALSE)

devtools::session_info()
sessionInfo()

#-----------------------------------------------------------------------
# Gera a página em pkgdown do pacote.

library(pkgdown)

# Constrói os elementos da página do pacote.
pkgdown::build_site()      # Todo o site que inclui tudo acima.

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
files

diameter_measurer(
    files = files,
    labels = c("Plate", "Trat1"),
    obs = "Done by Walmes",
    csv = "my_clicks.csv")

#-----------------------------------------------------------------------

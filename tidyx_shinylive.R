shinylive::export(appdir = "myapp", destdir = "docs")

httpuv::runStaticServer(dir="docs", port =8888)
# install.packages("tibble", dependencies = TRUE, INSTALL_opts = "--no-lock")
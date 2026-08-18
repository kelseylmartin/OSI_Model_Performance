app_namespace <- asNamespace("Optics")

shiny::shinyApp(
  ui = get("optics_portal_ui", envir = app_namespace)(),
  server = get("optics_portal_server", envir = app_namespace)
)

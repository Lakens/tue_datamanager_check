# The TU/e Data Manager Check page as a Shiny app.
#
#   shiny::runApp("web")                 # on your own computer
#   copy web/ into a Shiny Server's app folder, to serve it there
#
# The app only serves the page (index.html and www/): the visitor's browser
# sends the package to the metacheck server on SURF Research Cloud itself, as
# when the page is on any other website or opened from disk. The server's
# address is set in www/config.js.
library(shiny)

addResourcePath("www", "www")

# htmlTemplate() serves index.html as the whole page, as written (without
# Shiny's own scripts and styles, which the page does not need).
shinyApp(
  ui = htmlTemplate("index.html"),
  server = function(input, output, session) {}
)

library(usethis)
create_package("d:/dev/Estadistica/")

#' Mi función
#'
#' Esta función eleva al cuadrado un número.
#'
#' @param tabla Un número.
#' @return El cuadrado de x.
#' @examples
#' mi_funcion(tabla)
#' @export
mi_funcion <- function(tabla) 
{
  
  library(DBI)
  library(odbc)
  # Establece la conexión a la base de datos
  con <- dbConnect(odbc::odbc(),
                   Driver = "ODBC Driver 17 for Sql Server", 
                   Server = "localhost",
                   Database = "DataEstadistica",
                   UID = "rodrigo",
                   PWD = "enter",
                   Port = 1433)
  datos <- dbGetQuery(con, "SELECT * FROM [dbo].[" + tabla + "]")
  dbDisconnect(con)
  return (datos)
}

library(devtools)
document()
build()
install()
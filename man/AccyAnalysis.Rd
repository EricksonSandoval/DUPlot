\docType{function}
\name{AccyAnalysis}
\alias{AccyAnalysis}
\title{Analizar modelos basados en la asertividad del pron?stico}
\description{
Este paquete segmenta productos seg?n su asertividad del pron?stico en el modelo analizado y genera gr?ficos relacionados al desempe?o general y las cantidades de los "sobrantes / faltantes" (errores absolutos).
}
\usage{
AccyAnalysis(df_base, campana, lugar, exportar, ruta)
}
\arguments{
  \item{df_base}{la base de datos para el an?lisis, constituida por las columnas:}
    \itemize{
      \item 1) CODI_CAMP: campanas de venta (ej. 201718..201909)
      \item 2) NOMB_LINE: linea del SKU (ej. "JO JOYERIA")
      \item 3) CODI_VENT: codigo de venta del SKU (ej. 123456)
      \item 4) DESCRIPCION: nombre del SKU
      \item 5) REAL: demanda real del SKU
      \item 6..) Desde esta columna, se agregan pron?sticos a comparar frente a la cantidad real (ej. "RG3", "MOD")
      }
  \item{campana}{la campana seleccionada para el an?lisis}
  \item{lugar}{los 3 caracteres que representa el lugar (ej. "PER")}
  \item{exportar}{valor binario (1: para exportar, 0: no exportar)}
  \item{ruta}{el directorio donde se exportan los archivos. Si no se exporta solo incluir una cadena vac?a \code{""}}
}
\examples{
## Base de datos generada
DF_TOTAL <- data.frame(CODI_CAMP = 201901,
                       NOMB_LINE = "RE ROPA EXTERIOR",
                       CODI_VENT = ceiling(runif(100,100000,999999)),
                       DESCRIPCION = paste0("P",c(1:100)),
                       REAL = 1000+(1:100)*40,
                       RG3 = 1000+(1:100)*40 + rnorm(100,300,800),
                       MOD = 1000+(1:100)*40 + rnorm(100,100,700))

## realizar analisis
AccyAnalysis(DF_TOTAL, 201901, "PER", 0, "")

}
\keyword{accuracy}
}

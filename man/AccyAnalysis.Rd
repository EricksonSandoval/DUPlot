\docType{function}
\name{AccyAnalysis}
\alias{AccyAnalysis}
\title{Analizar modelos basados en la asertividad del pronóstico}
\description{
Este paquete segmenta productos según su asertividad del pronóstico en el modelo analizado y genera gráficos relacionados al desempeño general y las cantidades de los "sobrantes / faltantes" (errores absolutos).
}
\usage{
AccyAnalysis(df_base, campana, lugar, exportar, ruta)
}
\arguments{
  \item{df_base}{la base de datos para el análisis, constituida por las columnas:}
    \itemize{
      \item 1) CODI_CAMP: campanas de venta (ej. 201718..201909)
      \item 2) NOMB_LINE: nombre de la linea del SKU (ej. "RE ROPA EXTERIOR")
      \item 3) CODI_VENT: codigo de venta del SKU (ej. 123456)
      \item 4) DESCRIPCION: nombre del SKU
      \item 5) REAL: demanda real del SKU
      \item 6..) A partir de esta columna, se agregan los pronósticos a analizar (ej. "RG3", "MOD")
      }
  \item{campana}{la campana seleccionada para el análisis}
  \item{lugar}{los 3 caracteres que representa el lugar (ej. "PER")}
  \item{exportar}{valor binario(1: si se quiere exportar, 0: cc)}
  \item{ruta}{el directorio donde se exportan los archivos. Si \code{exportar<-0} entonces ""}
}
\examples{

## Base de datos generada
DF_TOTAL <- data.frame(CODI_CAMP = 201901,
                       NOMB_LINE = "RE ROPA EXTERIOR",
                       CODI_VENT = ceiling(runif(100,100000,999999)),
                       DESCRIPCION = paste0("P",c(1:100)),
                       REAL = 2000+(1:100)*80,
                       RG3 = 2000+(1:100)*80 + rnorm(100,0,100),
                       MOD = 2000+(1:100)*80 + rnorm(100,0,75))

## realizar analisis
AccyAnalysis(DF_TOTAL, 201901, "PER", 0, "")

}
\keyword{accuracy}
}

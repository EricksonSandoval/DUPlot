\docType{function}
\name{SankeyDiagram}
\alias{SankeyDiagram}
\title{Analizar evolución de individuos por intervalos definidos}
\description{
Este paquete determina el cambio de los individuos a través del tiempo en función a una variable cuantitativa. Se utiliza un diagrama de barras con flujos comunmente conocido como diagrama de Sankey.
}
\usage{
SankeyDiagram(df_base, interv, titulo, exportar, ruta)
}
\arguments{
  \item{df_base}{la base de datos para el análisis, constituida por las columnas:}
    \itemize{
      \item 1) CAMPANA: campanas de venta (ej. 201718..201909)
      \item 2) ID: identificadores de los individuos (ej. ASESORA, ZONA, SECTOR, DEPARTAMENTO)
      \item 3) VARIABLE: campo de análisis (ej. pedidos, ventas, PPP, PPU, UPP, faltantes)
      }
  \item{interv}{los valores que determinan los intervalos; se recomienda utilizar percentiles representativos (ej. \code{c(10,50,90)}}
  \item{titulo}{texto en la posición superior de la grafica}
  \item{exportar}{valor binario (1: para exportar, 0: no exportar)}
  \item{ruta}{el directorio donde se exportan los archivos. Si no se exporta solo incluir una cadena vacía \code{""}}
}
\examples{
## Base de datos generada
DF_TOTAL <- data.frame(CAMPANA = rep(201901:201908, times=200),
                       ZONA = rep(1:200, each=8, len=1600),
                       VENTA = ceiling(rep(runif(200, 100000, 300000),
                                           each=8,
                                           len=1600) *
                                       rnorm(1600,0.8,0.2)))

## realizar analisis
SankeyDiagram(DF_TOTAL,
              quantile(DF_TOTAL$VENTA, c(0.2,0.5,0.8)),
              "Evolución de venta acumulada por zona",
              1,
              ruta)
}
\keyword{accuracy}
}

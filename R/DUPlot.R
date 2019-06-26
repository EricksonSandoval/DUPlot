#' Create high impact visualizations for direct selling companies
#'
#' This package gather several visualization designs which representation lets make quick analysis and consequently take better decisions in business.
#'
#' @param DF_TOTAL
#' @param CAMPANA_ANALISIS
#' @param PAIS
#' @param EXPORTAR
#' @param RUTA
#'
#'
#' @export
#' @examples
#' AccyAnalysis()


calc_performance <- function(DF_TOTAL, PAIS, EXPORTAR, RUTA){

  n_prod_total <- nrow(DF_TOTAL)
  DF_DLT <- DF_TOTAL[(complete.cases(DF_TOTAL)==F | DF_TOTAL$REAL==0), ]
  DF_TOTAL <- DF_TOTAL[(complete.cases(DF_TOTAL)==F | DF_TOTAL$REAL==0)==F, ]
  n_prod_compl <- nrow(DF_TOTAL)

  n_models <- ncol(DF_TOTAL)-5

  INT_LAB <- data.frame(INT = levels(cut(0, breaks = c(0,
                                                       0.5,
                                                       0.8,
                                                       1.2,
                                                       1.8,
                                                       2.5,
                                                       Inf))),
                        LAB = c("a) 0 - 50%",
                                "b) 50 - 80%",
                                "c) 80 - 120%",
                                "d) 120 - 180%",
                                "e) 180 - 250%",
                                "f) > 250%"))

  DF1 <- data.frame(DF_TOTAL,
                    ASERT = DF_TOTAL$REAL / DF_TOTAL[, 6:(5+n_models)])

  DF1 <- data.frame(DF1,
                    INTERV = apply(DF1[, (6+n_models):(5 + 2*n_models)],
                                   2,
                                   function(x){INT_LAB$LAB[match(cut(x, breaks=c(0,
                                                                                 0.5,
                                                                                 0.8,
                                                                                 1.2,
                                                                                 1.8,
                                                                                 2.5,
                                                                                 Inf)),
                                                                 INT_LAB$INT)]}))

  DF1 <- data.frame(DF1,
                    DIF_CANT = ceiling((DF1[, 6:(5 + n_models)] - DF1$REAL)/10)*10)

  DF1X <- DF1

  DF1X$DIF_CANT.RG3 <- DF1X$DIF_CANT.RG3
  DF1X$DIF_CANT.MOD <- DF1X$DIF_CANT.MOD

  DFA <- DF1[, c(1:2, (6 + 2 * n_models):(5 + 3 * n_models))]

  DFA <- reshape(DFA,
                 v.name=c("INTERV.ASERT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5+n_models)],
                 direction = "long",sep = ".")[,-(5)]
  colnames(DFA) <- c("CAMPANA",
                     "LINEA",
                     "MODELO",
                     "ASERTIVIDAD")

  DFAN <- aggregate((1:nrow(DFA)) ~  CAMPANA + LINEA + ASERTIVIDAD + MODELO ,
                    DFA,
                    FUN = length)

  colnames(DFAN) <- c("CAMPANA",
                      "LINEA",
                      "ASERTIVIDAD",
                      "MODELO",
                      "CODI_VENT")

  library(dplyr)

  DFAP1 <- group_by(DFAN, paste(CAMPANA, LINEA, MODELO)) %>% mutate(PRODUCTOS=CODI_VENT/sum(CODI_VENT))

  DFAP1 <- data.frame(DFAP1)

  DFAP <- DFAP1[, c(1, 2, 4, 3, ncol(DFAP1))]

  INT_LAB <- data.frame(INT = levels(cut(0, breaks = c(0,
                                                       0.5,
                                                       0.8,
                                                       1,
                                                       1.2,
                                                       1.8,
                                                       2.5,
                                                       Inf))),
                        LAB = c("a) 0 - 50%",
                                "b) 50 - 80%",
                                "c) 80 - 100%",
                                "d) 100 - 120%",
                                "e) 120 - 180%",
                                "f) 180 - 250%",
                                "g) > 250%"))

  DF1 <- data.frame(DF_TOTAL,
                    ASERT = DF_TOTAL$REAL / DF_TOTAL[, 6:(5+n_models)])

  DF1 <- data.frame(DF1,
                    INTERV = apply(DF1[, (6+n_models):(5 + 2*n_models)],
                                   2,
                                   function(x){INT_LAB$LAB[match(cut(x,
                                                                     breaks=c(0,
                                                                              0.5,
                                                                              0.8,
                                                                              1,
                                                                              1.2,
                                                                              1.8,
                                                                              2.5,
                                                                              Inf)),
                                                                 INT_LAB$INT)]}))

  DF1 <- data.frame(DF1,
                    DIF_CANT = ceiling((DF1[, 6:(5 + n_models)] - DF1$REAL)/10)*10)


  DFQ <- DF1[,c(1, 2, (6 + 2 * n_models):(5 + 3 * n_models),
                (6 + 3 * n_models):(5 + 4 * n_models))]

  DFQ <- reshape(DFQ,
                 v.name = c("INTERV.ASERT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5 + n_models)],
                 direction = "long", sep = ".") ###REVISAR EN EL OTRO CODIGO

  DFQ <- DFQ[,-(ncol(DFQ))] ###REVISAR EN EL OTRO CODIGO

  colnames(DFQ) <- c("CAMPANA",
                     "LINEA",
                     paste("DIF_CANT",
                           colnames(DF1)[6:(5+n_models)],
                           sep="."),
                     "MODELO",
                     "ASERTIVIDAD")

  DFQ <- reshape(DFQ,
                 v.name = c("DIF_CANT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5 + n_models)],
                 direction = "long",
                 sep = ".") ###REVISAR EN EL OTRO CODIGO

  DFQ <- DFQ[,-(ncol(DFQ))]###REVISAR EN EL OTRO CODIGO

  colnames(DFQ) <- c("CAMPANA",
                     "LINEA",
                     "MODELO",
                     "ASERTIVIDAD",
                     "TEST",
                     "DIFERENCIA")

  DFQ <- DFQ[DFQ$MODELO == DFQ$TEST, c(1:4, 6)]

  DFQ <- aggregate(DIFERENCIA ~ ASERTIVIDAD+MODELO+LINEA+CAMPANA, DFQ, FUN=sum)

  X_RESULTADO <- list()
  X_RESULTADO[["DF_TOTAL"]] <- DF_TOTAL
  X_RESULTADO[["DF_DLT"]] <- DF_DLT
  X_RESULTADO[["ASERT_NOM"]] <- DFAN
  X_RESULTADO[["ASERT_PCT"]] <- DFAP
  X_RESULTADO[["CANT_SYF"]] <- DFQ
  X_RESULTADO[["CONS_ASERT"]] <- DF1X

  print(paste("Productos totales ingresados:", nrow(DF_TOTAL), sep=" "))
  print(paste("Productos incompletos eliminados:", nrow(DF_DLT), sep=" "))

  library(openxlsx)


  if(EXPORTAR==1){

    write.xlsx(x = DF1X, file = paste(RUTA, "/", PAIS, "_", "CONS_ASERT.xlsx", sep=""), sheetName = "DATOS", col.names = T, row.names = F)

  }

  print(DFAP)
  print(DFQ)

  if (nrow(DF_DLT)!=0){
    write.xlsx(x = DF_DLT, file = paste(RUTA, "/", PAIS, "_", "DATA_ELIMI.xlsx", sep=""), sheetName = "DATOS", col.names = T, row.names = F)
    print(DF_DLT)

  }

  return(X_RESULTADO)
}




plot_performance <- function(CONSOLIDADO, LA_CAMP, LINEA, PAIS, EXPORTAR, RUTA){

  DFAP <- CONSOLIDADO[["ASERT_PCT"]]
  LAB_1 <- sort(unique(DFAP$ASERTIVIDAD))
  DFAP <- DFAP[DFAP$LINEA == LINEA,]

  library(ggplot2)

  INT_COL_1 <- data.frame(LAB = c("a) 0 - 50%",
                                  "b) 50 - 80%",
                                  "c) 80 - 120%",
                                  "d) 120 - 180%",
                                  "e) 180 - 250%",
                                  "f) > 250%"),
                          COLOR = c("#E41A1C",
                                    "#377EB8",
                                    "#4DAF4A",
                                    "#984EA3",
                                    "#FF7F00",
                                    "#999999"))

  col_asertividad = as.character(INT_COL_1$COLOR[match(LAB_1, INT_COL_1$LAB)])

  plot_DFAP <- ggplot(DFAP[DFAP$CAMPANA == LA_CAMP,],
                      aes(x = ASERTIVIDAD,
                          y = PRODUCTOS,
                          group = MODELO,
                          label = paste0(format(PRODUCTOS * 100,
                                                digits = 0),"%"),
                          fill = ASERTIVIDAD)) +
    geom_bar(position = "dodge",
             stat = "identity") +
    scale_fill_manual(values = col_asertividad, drop=F) +
    scale_y_continuous(labels = scales::percent_format(scale = 100,
                                                       accuracy = 1)) +
    scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
                                    #,margin = margin(b = 12)
    ),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom") +
    ggtitle(paste("Distribución de productos según asertividad \n",
                  LINEA,
                  " ",
                  LA_CAMP,
                  " - ",
                  PAIS,
                  sep = "")) +
    xlab("Asertividad") +
    ylab("Productos") +
    geom_text(position = position_dodge(width = 0.7),
              color = "black",
              size = 4.2,
              fontface = "bold",
              vjust = -0.25) +
    facet_grid(~MODELO) +
    guides(fill=guide_legend(nrow=2, byrow=T))


  DFQ <- CONSOLIDADO[["CANT_SYF"]]
  LAB_2 <- sort(unique(DFQ$ASERTIVIDAD))
  DFQ <- DFQ[DFQ$LINEA==LINEA,]


  INT_COL_2 <- data.frame(LAB = c("a) 0 - 50%",
                                  "b) 50 - 80%",
                                  "c) 80 - 100%",
                                  "d) 100 - 120%",
                                  "e) 120 - 180%",
                                  "f) 180 - 250%",
                                  "g) > 250%"),
                          COLOR = c("#E41A1C",
                                    "#377EB8",
                                    "#4DAF4A",
                                    "#4DAF4A",
                                    "#984EA3",
                                    "#FF7F00",
                                    "#999999"))

  col_asertividad2 = as.character(INT_COL_2$COLOR[match(LAB_2, INT_COL_2$LAB)])

  options(scipen=100000)

  plot_DFQ <- ggplot(DFQ[DFQ$CAMPANA == LA_CAMP,], aes(x = ASERTIVIDAD,
                                                       y = DIFERENCIA,
                                                       fill = ASERTIVIDAD,
                                                       label = DIFERENCIA)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = col_asertividad2, drop=F) +
    scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
                                    #,margin = margin(b = 12)
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12,
                               face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom") +
    ggtitle(paste("Distribución de sobrantes / faltantes según asertividad \n",
                  LINEA,
                  " ",
                  LA_CAMP,
                  " - ",
                  PAIS,
                  sep = "")) +
    xlab("Asertividad") +
    ylab("Diferencia (unidades)") +
    geom_text(aes(label = format(abs(DIFERENCIA),
                                 digits=0)),
              size = 4.1,
              position = position_dodge(width = 0.95),
              vjust = -0.25,
              color = "black",
              fontface = "bold")+
    facet_grid(~ MODELO) +
    guides(fill=guide_legend(nrow=2, byrow=T))


  if(EXPORTAR==1){

    png(paste(RUTA, "/", PAIS, "_", substr(LINEA, 1,2), "_ASERT_", LA_CAMP, ".png", sep=""),
        width=1700, height=1200, res=220)
    plot(plot_DFAP)
    dev.off()

    png(paste(RUTA, "/", PAIS, "_", substr(LINEA, 1,2), "_DIFER_", LA_CAMP, ".png", sep=""),
        width=1700, height=1200, res=220)
    plot(plot_DFQ)
    dev.off()

  }

  print(plot_DFAP)
  print(plot_DFQ)

}



AccyAnalysis <- function(DF_TOTAL, CAMPANA_ANALISIS, PAIS, EXPORTAR, RUTA){

  DF_CAMP <- DF_TOTAL[DF_TOTAL$CODI_CAMP == CAMPANA_ANALISIS,]

  consolidado_res <- calc_performance(DF_CAMP, PAIS, EXPORTAR, RUTA)

  NOMB_LINES <- levels(unique(DF_CAMP$NOMB_LINE))

  for (i in NOMB_LINES){

    plot_performance(consolidado_res, CAMPANA_ANALISIS, i, PAIS, EXPORTAR, RUTA)

  }

  return(consolidado_res)

}
# DATA_ASERT <- RESULTS$CONS_ASERT
#
# if(nrow(RESULTS$DF_DLT)>0){
#
#   DATA_ELIMI <- RESULTS$DF_DLT
#
# }

#library(knitr)

#purl("1. Performance_Modelo_General.Rmd")

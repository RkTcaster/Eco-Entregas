#Procesamiento de columnas

rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

PARAM <- list()

PARAM$experimento <- "KA5240"

PARAM$input$dataset <- "./datasets/competencia_02_b6.csv.gz"

PARAM$meses <- c(202010, 202011, 202012, 
                 202101, 202102,202103, 202104, 202105, 202107)

#Fijo el directorio de trabajo
setwd("~/buckets/b1") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# genero var de prediccion binaria combinando baja+1 y baja+2
dataset[, clase01 := ifelse(clase_ternaria %in% c("baja+2", "baja+1","baja+3"), 1L, 0L)]


#Operamos sobre el drifting: 

#Operamos sobre el drifting: 
rank_norm <- function(data, columna) {
  for (i in columna){
    data[, nombre_col := round(rank(get(i))/.N, 6), by = as.character(foto_mes)]
    colnames(data)[which(names(data) == "nombre_col")] <- paste(i,"ranknorm",sep="_")
  }
  return(data)
}


col_drift <- c("Master_mlimitecompra",	"Master_mpagado",	"Master_mpagospesos",	"Master_mconsumototal",	"Master_mpagominimo",	"Visa_mconsumospesos",	
               "Visa_mlimitecompra",	"Visa_mpagado",	"Visa_mpagospesos",	"Visa_mconsumototal",	"Visa_mpagominimo",	"mcomisiones",
               "mactivos_margen",	"mpasivos_margen",	"mcuenta_corriente",	"mprestamos_personales",	"mprestamos_prendarios",	"mprestamos_hipotecarios",	"mplazo_fijo_pesos",	"minversion1_pesos",
               "minversion2",	"mpayroll",	"mpayroll2",	"mcuenta_debitos_automaticos",	"mttarjeta_visa_debitos_automaticos",	"mttarjeta_master_debitos_automaticos",	"mpagodeservicios",	"mpagomiscuentas",
               "mcajeros_propios_descuentos",	"mtarjeta_visa_descuentos",	"mtarjeta_master_descuentos",	"mcomisiones_mantenimiento",	"mcomisiones_otras",	"mtransferencias_recibidas",	"mtransferencias_emitidas",
               "mextraccion_autoservicio",	"mcheques_depositados",	"mcheques_emitidos",	"mcheques_depositados_rechazados",	"mcheques_emitidos_rechazados","Visa_Master_saldototal","Visa_Master_consumo","Visa_Master_finlim",
               "Visa_Master_saldopesos")

dataset <- rank_norm(dataset,col_drift)

dataset <- dataset[,-c(names(dataset) %in% col_drift),with=FALSE]


#-----------------------------------------------------------------------
# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA5240"


# meses donde se entrena el modelo
PARAM$input$training <- c(202010, 202011, 202012, 
                          202101, 202102,202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 100043

PARAM$finalmodel$num_iterations <- 4928
PARAM$finalmodel$learning_rate <- 0.0189943331895954
PARAM$finalmodel$feature_fraction <- 0.892623977897483
PARAM$finalmodel$min_data_in_leaf <- 785
PARAM$finalmodel$num_leaves <- 666


PARAM$finalmodel$max_bin <- 31

#-----------------------------------------------------------------------
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    seed = PARAM$finalmodel$semilla
  )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
       file = archivo_importancia,
       sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
       file = "prediccion.txt",
       sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 13000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
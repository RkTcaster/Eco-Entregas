# Intento base
require("data.table")
require("rpart")
require("rpart.plot")

# Info local de donde estan los parametros de trabajo
setwd("C:\\Users\\lrktl\\OneDrive\\Escritorio\\Maestria\\Primero\\Economia") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./dmeyf2023/src/monday/csv/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo


modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, 
        xval = 0,
        cp = -0.3, 
        minsplit = 800, 
        minbucket = 269, 
        maxdepth = 18
) 


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)



# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]


#Fijo la probabilidad para los estimulos a enviar

dapply[, Predicted := as.numeric(prob_baja2 > 0.032)]


# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./resultados")
dir.create("./resultados/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./resultados/exp/KA2001/K101_023.csv",
        sep = ","
)


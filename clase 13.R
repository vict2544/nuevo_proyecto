################################################
############### clase N°13       #############
########### POR: VICTOR AYOL
########## FECHA: 29/04/2025
########## HORA: 13H00  a 15H00
# 1. Ingresar un vector de 1 a 4 digitos 
# pasar a data frame
# Crear una columna en donde se almacenara la cantidad de digitos 
# Crear otra columna donde estar el mayor de los digitos
# Finalmente crear otra columna donde estar el menor de los digitos
# Crear dos columnas adicionales con la suma y el promedio de 

cat("Uso de estructuras de control \n")
cat("¿Cuántos números desea ingresar? \n")
N <- readLines(n = 1)
N <- as.numeric(N)
v1 <- c()

for (i in 1:N){
  cat("ingrese el elemento [",i,"]:\n")
  v1[i] <- readLines(n=1)
  v1[i] <- as.numeric(v1[i])
}

# Pasar a un dataframe
df1 <- data.frame(Numero = v1)
print(df1)

# Crear una columna en donde se almacenará la cantidad de dígitos
df1$dig1 <- nchar(as.character(df1$Numero))
print(df1)

# Forma 2 de cantidad de digitos
v2 <- c()
j <- 1
while (j<=N){
  if(as.numeric(df1[j,1])<10){
    v2[j]=1
  }else{
    if(as.numeric(df1[j,1])<100){
      v2[j]=2
    }else{
      if(as.numeric(df1[j,1])<1000){
        v2[j]=3
      }else{
        if(as.numeric(df1[j,1])<10000){
          v2[j]=4
        }
      }
    }
  }
  j=j+1
}

df1$dig2 <- v2

# Forma 3 de cantidad de digitos
v3 <- c()
k<-1
while (k<=N){
  Num <- as.numeric(df1[k,1])
  dig=0
  while(Num>0){
    Num=trunc(Num/10)
    dig=dig+1
  }
  v3[k]=dig
  k<-k+1
}
df1$dig3 <- v3

# Crear otra columna donde estar el mayor de los digitos
j <- 1
v4 <- c()
while (j <=N) {
  cdig=as.numeric(df1[j,2])
switch(cdig,
  "1"={
    v4[j]=1
    
  },
  "2"={
    num1 <- as.numeric(df1[j,1])
    x1 <- trunc(num1/10)
    x2 <- num1%%10
    if(x1>x2){
      v4[j] <- x1
    }else{
      v4[j] <- x2
    }
  },
  "3"={
    num1 <- as.numeric(df1[j,1])
    x1 <- trunc(num1/100)
    Aux <- num1%%100
    x2 <- trunc(Aux/100)
    x3 <- Aux%%10
    if(x1>x2){
      M1 <- x1
    }else{
      M1 <- x2
    }
    if(M1>x3){
      v4[j]=M1
    }else{
      v4[j] <- x3
      
    }
  }
)
 j=j+1 
}
df1$mayor=v4

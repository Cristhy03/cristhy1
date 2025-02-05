#Paquete que genera un menu para matrices
crear_matriz <- function(nf, nc) {
  M1 <- matrix(NA, nrow = nf, ncol = nc)  
  return(M1)
}

ingreso_de_param <- function() {
  cat("Ingrese el numero de filas:\n")
  nf <- readLines(n=1)
  nf <- as.numeric(nf)
  cat("Ingrese el numero de columnas:\n")
  nc <- readLines(n=1)
  nc <- as.numeric(nc)
  return(list(nf=nf, nc=nc))
}

val_matriz <- function(M1, nf, nc) {
  for (i in 1:nf) {  
    for (j in 1:nc) { 
      cat("Ingrese el elemento[", i,",", j,"]:\n")
      M1[i,j] <- readLines(n=1)
      M1[i,j] <- as.numeric(M1[i,j])
    }
  }
  return(M1)
}

cant_par_impar <- function(M1, nf, nc) {
  p <- 0
  imp <- 0
  for(i in 1:nf) {
    for(j in 1:nc) {
      if(M1[i,j] %% 2 == 0) {
        p <- p + 1
      } else {
        imp <- imp + 1
      }
    }
  }
  return(list(p=p, imp=imp))
}

val_par_vect <- function(M1, nf, nc) {
  V1 <- c()
  V2 <- c()
  k <- 1
  q <- 1
  for(i in 1:nf) {
    for(j in 1:nc) {
      if(M1[i,j] %% 2 == 0) {  # Changed condition to check for even numbers
        V1[k] <- M1[i,j]
        k <- k + 1
      } else {
        V2[q] <- M1[i,j]
        q <- q + 1
      }
    }
  }
  return(list(V1=V1, V2=V2))  # Named the list elements
}

Menu <- function() {
  cat("Menu principal\n")
  cat("1. Ingrese la matriz\n")
  cat("2. Cuantos pares e impares\n")
  cat("3. Valores pares o impares\n")
  cat("4. Salir\n")
  cat("Escoja una opcion:\n")
  op1 <- readLines(n=1)  # Fixed typo in readLines
  op1 <- as.numeric(op1)
  return(op1)
}

menu4 <- function() {  # Removed unnecessary parameter
  F <- ingreso_de_param()
  nf1 <- F$nf
  nc1 <- F$nc
  M <- crear_matriz(nf1, nc1)
  Mat1 <- val_matriz(M, nf1, nc1)
  Mat2 <- matrix(as.numeric(as.vector(Mat1)), nrow=nf1, ncol=nc1)  # Fixed matrix conversion
  
  ban <- 0
  while(ban == 0) {
    op1 <- Menu()  # Assign the return value from Menu()
    
    switch(as.character(op1),  # Convert op1 to character for switch
           "1" = {
             cat("Visualizar la matriz ingresada\n")
             print(Mat1)
             print(Mat2)
           },
           "2" = {
             cat("Existen en la matriz\n")
             c1 <- cant_par_impar(Mat2, nf1, nc1)
             cat("Pares=", c1$p, "\n")
             cat("Impares=", c1$imp, "\n")
           },
           "3" = {
             cat("Valores pares e impares de la matriz\n")
             L1vect <- val_par_vect(Mat2, nf1, nc1)
             cat("Vector 1 con pares\n")
             print(L1vect$V1)
             cat("Vector 2 con impares\n")
             print(L1vect$V2)
           },
           "4" = {
             cat("Saliendo del sistema\n")
             ban <- 1
           },
           {
             cat("Opción no válida\n")  # Default case for invalid options
           }
    )
  }
}


frepcomgen <- function(m, n) {
  x <- matrix(0, 2, n)
  y <- matrix(0, 2, m)
  val <- 1
  x[1, 1] <- sample(2:5, 1)
  x[2, 1] <- round(runif(1, 0, val), digits = 2)
  val <- val - x[2, 1]
  for (i in 2:(n - 1)) {
    x[1, i] <- sample(2:5, 1) + x[1, i - 1]
    x[2, i] <- round(runif(1, 0, val), digits = 2)
    val <- val - x[2, i]
    
  }
  x[2, n] = val
  x <- x[, order(x[1, ])]    #Ordonam matricile dupa prima linie
  
  val <- 1
  y[1, 1] <- sample(2:5, 1)
  y[2, 1] <- round(runif(1, 0, val), digits = 2)
  val <- val - y[2, 1]
  for (i in 2:(m - 1)) {
    y[1, i] <- sample(2:5, 1) + y[1, i - 1]
    y[2, i] <- round(runif(1, 0, val), digits = 2)
    val <- val - y[2, i]
  }
  y[2, m] = val
  matri <- list(x, y)
  
  
  matrice <- matrix(0, n, m)
  x <- x[, order(x[1, ])]
  y <- y[, order(y[1, ])]
  
  auxx <- x
  auxy <- y
  
  for (i in 1:n) {
    no <- sample(1:m, 1)
    for (j in 1:m)
    {
      if (j != no)
      {
        matrice[i, j] <-
          round(runif(1, 0, min(auxx[2, i], auxy[2, j])), digits = 2)
        auxx[2, i] <- auxx[2, i] - matrice[i, j]
        auxy[2, j] <- auxy[2, j] - matrice[i, j]
      }
    }
  }
  
  TheMatrix <- list(x, y, matrice)
  return(TheMatrix)
  
}




fcomplrepcom <- function(matri) {
  while (1) {
    okey <- 0
    n = length(matri[, 1])
    m = length(matri[1, ])
    
    
    i <- 1
    j <- 1
    auxi <- i
    auxj <- j
    while (auxi <= n &&
           auxj <= m)
      #caut doar 1 element lipsa ptr a putea completa rebusul, fie pe linie/col
      
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxi <- auxi + 1
      auxj <- auxj + 1
      
      
    }
    while (auxi <= n)
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      auxi <- auxi + 1
    }
    
    
    while (auxj <= m)
    {
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxj <- auxj + 1
      
      
    }
    
    if (okey == 0)
      
      return(matri)
  }
}


fcomplrepcom <- function(matri) {
  while (1) {
    okey <- 0
    n = length(matri[, 1])
    m = length(matri[1, ])
    
    
    i <- 1
    j <- 1
    auxi <- i
    auxj <- j
    while (auxi <= n && auxj <= m)
      #Complete the matrix
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxi <- auxi + 1
      auxj <- auxj + 1
      
      
    }
    while (auxi <= n)
    {
      ok <- 0
      poz <- 0
      for (count in 1:m)
        if (matri[auxi, count] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[auxi, poz] = x[2, auxi] - sum(matri[auxi, ])
        okey <- 1
      }
      auxi <- auxi + 1
    }
    
    
    while (auxj <= m)
    {
      ok <- 0
      poz <- 0
      for (count in 1:n)
        if (matri[count, auxj] == 0) {
          ok <- ok + 1
          poz <- count
          
        }
      if (ok == 1)
      {
        matri[poz, auxj] = y[2, auxj] - sum(matri[, auxj])
        okey <- 1
      }
      auxj <- auxj + 1
      
      
    }
    
    if (okey == 0)
      
      return(matri)
  }
}

fvernecor <- function(auxx, auxy) {
  #necorelate<=> Cov(X,Y)=0
  ex <- 0
  ey <- 0
  for (i in 1:length(auxx[1, ]))
    ex <- ex + auxx[1, i] * auxx[2, i]
  for (i in 1:length(y[1, ]))
    ey <- ey + auxy[1, i] * auxy[2, i]
  exy <- 0
  
  for (i in 1:length(auxx[1, ]))
    for (j in 1:length(auxy[1, ]))
      exy <- exy + ((auxx[1, i] * auxy[1, j]) * (auxx[2, i] * auxy[2, j]))
  covxy <- exy - ex * ey
  if (covxy == 0)
    print("necorelate")
  else
    print("corelate")
}

fverind <- function(x, y, matri) {
  ok <- 1
  for (i in 1:length(matri[, 1]))
    for (j in 1:length(matri[1, ]))
      if (x[2, i] * y[2, j] != matri[i, j])
        ok <- 0
      if (ok == 1)
        print("Sunt independente")
      else
        print("Nu sunt independente")
      
}

m=3
n=5
matri <- frepcomgen(m, n)

x <- matri[[1]]
print(x)

y <- matri[[2]]
print(y)

tabel <- matri[[3]]
print(tabel)
tabel <- fcomplrepcom(tabel)
print(tabel)


#Cov(5X,-3Y)-----------------------------------------
auxx <- 5 * x
auxx
auxy <- -3 * y
auxy
ex <- 0
ey <- 0
for (i in 1:length(auxx[1, ]))
  ex <- ex + auxx[1, i] * auxx[2, i]  #E[X]
for (i in 1:length(y[1, ]))
  ey <- ey + auxy[1, i] * auxy[2, i] #E[Y]

exy <- 0
for (i in 1:length(auxx[1, ]))
  for (j in 1:length(auxy[1, ]))
    exy <- exy + ((auxx[1, i] * auxy[1, j]) * (auxx[2, i] * auxy[2, j])) #E[XY]
covxy <- exy - ex * ey
covxy

#P(0<X<3/Y>2)-----------------------------------------
#P(0<X<3/Y>2)=P(0<X<3 n Y>2)/P(Y>2)=P(0<X<3)*P(Y>2)/P(Y>2)=P(0<X<3)

#P(Y>2)
sumy <- 0
for (i in 1:length(y[1, ]))
  if (y[1, i] > 2)
    #Elements with P[Y>2]
    sumy <- sumy + y[2, i]
P2 = sumy

#P(0<X<3)
sumx <- 0
for (i in 1:length(x[1, ]))
  if (x[1, i] > 0 && x[1, i] < 3)
    #Elements with P(0<X<3)
    sumx <- sumx + x[2, i]
raspuns2 = sumx

#P[X>6,Y<7]-----------------------------------------
sumx <- 0
sumy <- 0
for (i in 1:length(x[1, ]))
  #Elements with P[X>6]
  if (x[1, i] > 6)
    sumx <- sumx + x[2, i]

for (i in 1:length(y[1, ]))
  if (y[1, i] < 7)
    #Elements with P[Y<7]
    sumy <- sumy + y[2, i]

raspuns3 <- sumx * sumy   #P[X>6,Y<7]
raspuns3

#d-----------------------------------------
fverind(x, y, tabel)#indep
fvernecor(x, y)#cov
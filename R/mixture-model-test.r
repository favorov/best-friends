n   <- 400     
# число строк 
k   <- 50       
# число столбцов 
n0  <- 10       
# сколько строк «портим» 
p   <- 0.1      
# вероятность замены 
rate_exp    <- 1/2    
# rate для Exp 
lambda_pois <- 100   
# λ для Pois  
#-------------------------  # 
#stat <- c() # 
#false <- c() # 
for (i in 1:1){    
  # 1) Генерация матрицы 
  M <- matrix(rexp(n * k, rate = rate_exp), nrow = n, ncol = k)  
  # 2) Отметка строк и маскирование 
  selected_rows <- sort(sample.int(n, size = n0))
  mask <- matrix(FALSE, nrow = n, ncol = k) 
  mask[selected_rows, ] <- 
    matrix(runif(n0 * k) < p, nrow = n0, ncol = k, byrow=TRUE)  
  # 3) Замена отмеченных ячеек на Poisson 
  M[mask] <- rpois(sum(mask), lambda = lambda_pois)  
  # Задаём имена столбцов: "col_1", "col_2", …, "col_k" 
  colnames(M) <- paste0("col_", seq_len(k))  
  # Задаём имена строк: "row_1", "row_2", …, "row_n" 
  rownames(M) <- paste0("row_", seq_len(n))
}


  
#вот тут должно быть ок # 
#n   <- 400       # число строк # 
#k   <- 80       # число столбцов # 
#n0  <- 40       # сколько строк «портим»
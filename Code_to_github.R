
Extremogram_1 <- function (x, quant1, quant2, maxlag, type, ploting = 0, cutoff = 1, start = 0) 
{
  level1 = quantile(x, prob = quant1)
  level2 = quantile(x, prob = quant2)
  n = length(x)
  rhohat = rep(0, maxlag)
  if (type == 1) { # pos pos
    rhohat[1] = 1
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] > level1 & x[(i + 1):n] > level2])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] > level1])
    }
  }
  else if (type == 2) { #neg neg
    rhohat[1] = 1
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] < level1 & x[(i + 1):n] < level2])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] < level1])
    }
  }
  else if (type == 3) { #neg X0 pos X1
    rhohat[1] = 0
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] < level1 & x[(i + 1):n] > level2])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] < level1])
    }
  }
  else if (type == 4) { #pos neg
    rhohat[1] = 0
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] > level1 & x[(i + 1):n] < level2])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] > level1])
    }
  }
  else if (type == 5) { # pos abs
    rhohat[1] = 1
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] > level1 & x[(i + 1):n] < level2 | x[1:(n - i)] > level1 & x[(i + 1):n] > level1])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] > level1])
    }
  }
  else if (type == 6) { # neg abs
    rhohat[1] = 1
    for (i in 1:(maxlag - 1)) {
      rhohat[i + 1] = length((1:(n - i))[x[1:(n - i)] < level1 & x[(i + 1):n] > level2 | x[1:(n - i)] < level1 & x[(i + 1):n] < level1])
      rhohat[i + 1] = rhohat[i + 1]/length((1:(n - i))[x[1:(n - i)] < level1])
    }
  }
  if (ploting == 1) {
    plot((start:(maxlag - 1)), rhohat[(start + 1):maxlag], 
         type = "n", xlab = "lag", ylab = "extremogram", 
         ylim = c(0, cutoff))
    lines((start:(maxlag - 1)), rhohat[(start + 1):maxlag], 
          col = "grey", lwd = 1, type = "h")
    abline((0:(maxlag - 1)), 0, col = 1, lwd = 1)
  }
  return(rhohat)
}

Extremogram_2 <- function (a, quant1, quant2, maxlag, type, ploting = 0, cutoff = 1, start = 0, ...) 
{
  x = a[, 1]
  y = a[, 2]
  level1 = quantile(a[, 1], prob = quant1)
  level2 = quantile(a[, 2], prob = quant2)
  level2_b = quantile(a[, 2], prob = quant1)
  n = length(a[, 1])
  rhohat = rep(0, maxlag)
  if (type == 1) {
    for (i in 1:maxlag) {
      # amount of times both day of lag i is an extreme event and lag 0 is an extreme event
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] > level1 & y[i:n] > level2])
      # diveded by amount of times lag 0 is extreme in total, we get the sample extremogram
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] > level1])
    }
  }
  else if (type == 2) {
    for (i in 1:maxlag) {
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] < level1 & y[i:n] < level2])
      
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] < level1])
    }
  }
  else if (type == 3) {
    for (i in 1:maxlag) {
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] < level1 & y[i:n] > level2])
      
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] < level1])
    }
  }
  else if (type == 4) {
    for (i in 1:maxlag) {
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] > level1 & y[i:n] < level2])
    
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] > level1])
    }
  } 
  else if (type == 5) {#pos abs
    for (i in 1:maxlag) {
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] > level1 & y[i:n] < level2 |
                                       x[1:(n - i + 1)] > level1 & y[i:n] > level2_b ])
      
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] > level1])
    }
  }
  else if (type == 6) {#neg abs
    for (i in 1:maxlag) {
      rhohat[i] = length((1:(n - i))[x[1:(n - i + 1)] < level1 & y[i:n] > level2 |
                                       x[1:(n - i + 1)] < level1 & y[i:n] < level2_b ])
      
      rhohat[i] = rhohat[i]/length((1:(n - i))[x[1:(n - i + 1)] < level1])
    }
  }
  if (ploting == 1) {
    plot((start:(maxlag - 1)), rhohat[(start + 1):maxlag], 
         type = "n", xlab = "lag", ylab = "extremogram", 
         ylim = c(0, cutoff), ...)
    lines((start:(maxlag - 1)), rhohat[(start + 1):maxlag], 
          col = "grey", lwd = 1, type = "h")
    abline((0:(maxlag - 1)), 0, col = 1, lwd = 1)
  }
  return(rhohat)
}

Stationary_bootstrap_1 <- function (x, R, l, maxlag, quant1, quant2, type, start = 1, cutoff = 1, alpha = 0.05) 
{
    #set default extremogram til ploting = 0
    boot = boot::tsboot(x, Extremogram_1, R, l = l, sim = "geom", 
                        endcorr = TRUE, maxlag = maxlag, quant1 = quant1, 
                        quant2 = quant2, type = type)
    tmp = boot[[2]]
    mat = tmp[, (2:maxlag)]
    k = dim(mat)[2]
    pocket = matrix(0, ncol = 3, nrow = k)
    for (i in 1:k) {
      pocket[i, 1] = quantile(mat[, i], prob = (alpha/2)) #lower quantile
      pocket[i, 2] = mean(mat[, i]) #mean of extremogram on bootstraped samples
      pocket[i, 3] = quantile(mat[, i], prob = (1 - alpha/2)) #upper quantile
    }
    lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 2], lty = 1, lwd = 1, col = 1) #mean
    lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 3], lty = 4, lwd = 1, col = 1) #upper quantile
    lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 1], lty = 4, lwd = 1, col = 1) #lower quantile
}

#stationary bootstrap function. fuck parralel core fis. 
Stationary_bootstrap_2 <- function (x, R, l, maxlag, quant1, quant2, type, start = 1, cutoff = 1, alpha = 0.05) 
{
    boot = boot::tsboot(x, Extremogram_2, R, l = l, sim = "geom", #geom makes block sizes geometrically distributed
                        endcorr = TRUE, maxlag = maxlag, quant1 = quant1, #Extremogram2 is the statistic we wish to calculate
                        quant2 = quant2, type = type)
    tmp = boot[[2]]
    k = dim(tmp)[2]
    pocket = matrix(0, ncol = 3, nrow = k)
    for (i in 1:k) {
      pocket[i, 1] = quantile(tmp[, i], prob = (alpha/2))
      pocket[i, 2] = mean(tmp[, i])
      pocket[i, 3] = quantile(tmp[, i], prob = (1 - alpha/2))
    }
    lines((start-1):(k + (start-2)), pocket[start:(k -1 + start), 2], lty = 1, lwd = 1) #mean
    lines((start-1):(k + (start-2)), pocket[start:(k -1 + start), 3], lty = 4, lwd = 1) #upper quantile
    lines((start-1):(k + (start-2)), pocket[start:(k -1 + start), 1], lty = 4, lwd = 1) #lower quantile
    print(pocket)
}

Permutation_1 <- function (x, quant1, quant2, m, type, exttype, maxlag, start = 1, alpha = 0.05) 
{
  if (type == 1) {
    for (i in 1:m) {
      pBACC = sample(x)
      cc = Extremogram_1(pBACC, quant1 = quant1, quant2 = quant2, maxlag = maxlag, type = exttype, ploting = 0)
      lines((start:(maxlag - 1)), cc[(start + 1):maxlag], 
            col = 1, lwd = 1)
    }
  }
  if (type == 2) {
    cc = matrix(0, ncol = m, nrow = maxlag)
    for (i in 1:m) {
      pBACC = sample(x)
      cc[, i] = Extremogram_1(pBACC, quant1 = quant1, quant2 = quant2, maxlag = maxlag, type = exttype, ploting = 0) #i'th column
    }
    k = dim(cc)[1] #amount of observations
    pocket = matrix(0, ncol = 3, nrow = k)
    for (i in 1:k) {
      pocket[i, 1] = quantile(cc[i, ], prob = (alpha/2))
      pocket[i, 3] = quantile(cc[i, ], prob = (1 - alpha/2))
    }
    lines(start:(k - 1), pocket[(start+1):(k - 1 + start), 1], col = 1, lwd = 2, col = "yellow")
    lines(start:(k - 1), pocket[(start+1):(k - 1 + start), 3], col = 1, lwd = 2, col = "yellow")
  }
  if (type == 3) {
    cc = matrix(0, ncol = m, nrow = (start + 1))
    for (i in 1:m) {
      pBACC = sample(x)
      cc[, i] = Extremogram_1(pBACC, quant1 = quant1, quant2 = quant2, maxlag = start+1, type = exttype, ploting = 0) #i'th column
    }
    dde = as.numeric()
    gge = as.numeric()
    for (i in 1:maxlag) {
      dde[i] = quantile(cc[(start + 1), ], prob = (alpha/2))
      gge[i] = quantile(cc[(start + 1), ], prob = (1 - alpha/2))
    }
    lines((start:(maxlag - 1)), dde[(start + 1):maxlag],
          col = "black", lwd = 2)
    lines((start:(maxlag - 1)), gge[(start + 1):maxlag],
          col = "black", lwd = 2)
  }
}

Permutation_2 <- function (x, quant1, quant2, m, type, exttype, maxlag, start = 0, alpha = 0.05) 
{
  p1 = quant1
  p2 = quant2
  if (type == 1) {
    # plotting sample extremogram for each permutation. used as a test for seing if code is correct.
    for (i in 1:m) {
      perm1 = sample(x[,1])
      perm2 = sample(x[,2])
      pBACC = matrix(c(perm1, perm2), nrow = length(perm1), ncol = 2)
      #pBACC = permatrix(x)
      cc = Extremogram_2(pBACC, quant1 = p1, quant2 = p2, maxlag = maxlag, type = exttype, ploting = 0)
      #cc = permboot2(pBACC, p1, p2, maxlag, exttype)
      lines((start:(maxlag - 1)), cc[(start + 1):maxlag], col = 1, lwd = 1)
    }
  }
  if (type == 2) {
    #conf bond for all lags
    cc = matrix(0, ncol = m, nrow = maxlag)
    for (i in 1:m) {
      perm1 = sample(x[,1])
      perm2 = sample(x[,2])
      pBACC = matrix(c(perm1, perm2), nrow = length(perm1), ncol = 2)
      
      cc[, i] = Extremogram_2(pBACC, quant1 = p1, quant2 = p2, maxlag = maxlag, type = exttype, ploting = 0)
    }
    k = dim(cc)[1]
    pocket = matrix(0, ncol = 3, nrow = k)
    for (i in 1:k) {
      pocket[i, 1] = quantile(cc[i, ], prob = (alpha/2))
      pocket[i, 3] = quantile(cc[i, ], prob = (1 - alpha/2))
    }
    lines(start:(k-1), pocket[(start+1):k, 2], col = 1, lwd = 2)
    lines(start:(k-1), pocket[(start+1):k, 3], col = 1, lwd = 2)
  }
  if (type == 3) {
    #conf bond for first lag and apply to all lags because of theory
    cc = matrix(0, ncol = m, nrow = (start + 2))
    for (i in 1:m) {
      perm1 = sample(x[,1])
      perm2 = sample(x[,2])
      pBACC = matrix(c(perm1, perm2), nrow = length(perm1), ncol = 2)
      
      cc[, i] = Extremogram_2(pBACC, quant1 = p1, quant2 = p2, maxlag = start+2, type = exttype, ploting = 0)
    }
    dde = as.numeric()
    gge = as.numeric()
    for (i in 1:maxlag) {
      dde[i] = quantile(cc[(start + 1), ], prob = (alpha/2))
      gge[i] = quantile(cc[(start + 1), ], prob = (1 - 
                                                     alpha/2))
    }
    lines((start:(maxlag - 1)), dde[(start + 1):maxlag], col = 1, lwd = 2)
    lines((start:(maxlag - 1)), gge[(start + 1):maxlag],  col = 1, lwd = 2)
    print(dde[1])
    print(gge[1])
    
  }
}

### Return time extremogram functions ###


Extremogram_return <- function (x, type, maxlag, uplevel = 1, lowlevel = 0, histogram = 0, cutoff = 1, return_type = "abs") 
{
  n = length(x)
  mat = mat = rep(0, n)
  mat_up = rep(0, n)
  mat_low = rep(0, n)
  #indicator fct of time series which events are extreme. 
  if (type == 1) {
    uplevel = quantile(x, prob = uplevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] > uplevel, 1, 0)
    }
  }
  else if (type == 2) {
    lowlevel = quantile(x, prob = lowlevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] < lowlevel, 1, 0)
    }
  }
  else if (type == 3) {
    uplevel = quantile(x, prob = uplevel)
    lowlevel = quantile(x, prob = lowlevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] > uplevel || x[i] < lowlevel, 1, 0)
    }
  }
  else if (type == 4 | type == 5) {
    uplevel = quantile(x, prob = uplevel)
    lowlevel = quantile(x, prob = lowlevel)
    for (i in 1:n) {
      mat_up[i] = ifelse(x[i] > uplevel, 1, 0)
      mat_low[i] = ifelse(x[i] < lowlevel, 1, 0)
    }
  }
  sequence = seq(1, n)
  #numbering elements in indicator fct 
  if (type == 1 | type == 2 | type == 3) {
    gar = cbind(sequence, mat)
    junk = matrix(0, ncol = 2, nrow = n)
    for (i in 1:n) {
      if (mat[i] == 1) {
        junk[i, ] = gar[i, ]
      }
    }
    #filtering so only info about 1's in indicator fct are kept
    ind <- rowSums(junk == 0) != ncol(junk) #true false. true meaning it is an extreme event
    junk = junk[ind, ] #only extreme events
    n = dim(junk)[1]
    return_time = rep(0, n - 1)
    for (i in 1:n - 1) {
      return_time[i] = (junk[i + 1, 1] - junk[i, 1])
    }
    #vector with all return times created
    
  } else if (type == 4 | type == 5) {
    gar = cbind(sequence, mat_up, mat_low)
    junk = matrix(0, ncol = 3, nrow = n)
    for (i in 1:n) {
      if (mat_up[i] == 1) {
        junk[i, ] = gar[i, ]
      }
      if (mat_low[i] == 1) {
        junk[i, ] = gar[i, ]
      }
    }
    ind <- rowSums(junk == 0) != ncol(junk) #true false. true meaning it is an extreme event
    junk = junk[ind, ] #only extreme events
    n = dim(junk)[1]
    
    if (type == 4){
      return_time = matrix(0, ncol = 3, nrow = n-1)
      
      for (i in 0:(n - 2)) {
        #if we have positive extreme
        if (junk[i + 1, 2] == TRUE){      
          #[row,col]
          #return_time[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
          return_time[i+1,1] = (junk[i + 2, 1] - junk[i + 1, 1])
          return_time[i+1,2] = junk[i + 2, 2] #positive marker
          return_time[i+1,3] = junk[i + 2, 3] #negative marker
        } else {
          #if i'th extreme is not postive it is negative so we just type else
          #return_time_low_abs[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
      }
      
      # removing zeros because old code influenced this code. 
      #running time is no problem so whatever.
      return_time <- return_time[which(return_time[,1] != 0),]
      
      
    } else if (type == 5){
      return_time = matrix(0, ncol = 3, nrow = n-1)
      
      #note: last observation may be an extreme that we want to find the waiting time for. But we are not able to since we have 
      for (i in 0:(n - 2)) {
        #if we have positive extreme
        if (junk[i + 1, 3] == TRUE){      
          #[row,col]
          #return_time[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
          return_time[i+1,1] = (junk[i + 2, 1] - junk[i + 1, 1])
          return_time[i+1,2] = junk[i + 2, 2] #positive marker
          return_time[i+1,3] = junk[i + 2, 3] #negative marker
        } else {
          #if i'th extreme is not postive it is negative so we just type else
          #return_time_low_abs[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
      }
      
      # removing zeros because old code influenced this code. 
      #running time is no problem so whatever.
      
      return_time <- return_time[which(return_time[,1] != 0),]
      
    }
    
  } else if (type == 6) { #old stuff
    gar = cbind(sequence, mat_up, mat_low)
    junk = matrix(0, ncol = 3, nrow = n)
    for (i in 1:n) {
      if (mat_up[i] == 1) {
        junk[i, ] = gar[i, ]
      }
      if (mat_low[i] == 1) {
        junk[i, ] = gar[i, ]
      }
    }
    ind <- rowSums(junk == 0) != ncol(junk) #true false. true meaning it is an extreme event
    junk = junk[ind, ] #only extreme events
    n = dim(junk)[1]
    
    return_time_up_up = rep(0, n - 1)
    return_time_low_low = rep(0, n - 1)
    return_time_low_up = rep(0, n - 1)
    return_time_up_low = rep(0, n - 1)
    
    
    #let us roll through the observations of extremes 
    for (i in 0:(n - 2)) {
      #if we have positive extreme
      if (junk[i + 1, 2] == TRUE){      #[row,col]
        #if next extreme is positive
        if (as.logical(junk[i + 2, 2]) == TRUE){
          return_time_up_up[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        } else {
          #if next extreme is not positive then it is negative
          return_time_up_low[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
      } else {
        #if i'th extreme is not postive it is negative so we just type else
        #if next extreme is positive
        if (as.logical(junk[i + 2, 2]) == TRUE){
          return_time_low_up[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        } else {
          #if next extreme is not positive then it is negative
          return_time_low_low[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
        
      }
    }
    
    #cbind(return_time_up_up,return_time_low_low,return_time_low_up,return_time_up_low)
    
  }
  #cbind(return_time_up_up,return_time_low_low,return_time_low_up,return_time_up_low)
  
  return_time_pos <- return_time[which(return_time[,2] == 1), 1]
  return_time_neg <- return_time[which(return_time[,3] == 1), 1]
  return_time_abs <- return_time[,1]
  
  table_rt_pos = as.matrix(table(return_time_pos))
  for (j in 1:dim(table_rt_pos)[1]) {
    table_rt_pos[j, 1] = table_rt_pos[j, 1]/dim(return_time)[1]
  }
  
  table_rt_neg = as.matrix(table(return_time_neg))
  for (j in 1:dim(table_rt_neg)[1]) {
    table_rt_neg[j, 1] = table_rt_neg[j, 1]/dim(return_time)[1]
  }
  
  table_rt_abs = as.matrix(table(return_time_abs))
  for (j in 1:dim(table_rt_abs)[1]) {
    table_rt_abs[j, 1] = table_rt_abs[j, 1]/dim(return_time)[1]
  }
  
  table_rt_pos = as.double(table_rt_pos)[1:maxlag]
  table_rt_neg = as.double(table_rt_neg)[1:maxlag]
  table_rt_abs = as.double(table_rt_abs)[1:maxlag]
  
  table_rt_pos[is.na(table_rt_pos)] <- 0
  table_rt_neg[is.na(table_rt_neg)] <- 0
  table_rt_abs[is.na(table_rt_abs)] <- 0
  
  if (histogram == 1) {
    
    #plots
    plot((0:maxlag), table_rt_abs[1:(maxlag+1)], 
         type = "n", xlab = "lag", ylab = "return time extremogram", 
         ylim = c(0, cutoff))
    lines((1:maxlag), table_rt_abs[1:maxlag], 
          col = "black", lwd = 1, type = "h")
    points((1:maxlag), table_rt_pos[1:maxlag], 
           col = "red", lwd = 1)
    points((1:maxlag), table_rt_neg[1:maxlag], 
           col = "blue", lwd = 1)
    abline((0:(maxlag - 1)), 0, col = 1, lwd = 1)
    
    # k = 14
    # table_rt_pos[k] + table_rt_neg[k]
    # table_rt_abs[k]
  }
  
  #return(list(aa, return_time, mean(return_time)))
  if (return_type == "abs") {
    aa = table_rt_abs
  } else if (return_type == "pos"){
    aa = table_rt_pos
  } else if (return_type == "neg"){
    aa = table_rt_neg
  }
  return(aa)
}

Extremogram_return_2 <- function (datamatrix, type, maxlag, 
                                  uplevel = 1, lowlevel = 0, histogram = 0, cutoff = 1,
                                  return_type = "abs") 
{
  x = datamatrix[, 1]
  y = datamatrix[, 2]
  n = length(x)
  mat_x = rep(0, n)
  mat_up_x = rep(0, n)
  mat_low_x = rep(0, n)
  mat_y = rep(0, n)
  mat_up_y = rep(0, n)
  mat_low_y = rep(0, n)
  #indicator fct of time series which events are extreme. 
  if (type == 1) {
    uplevel = quantile(x, prob = uplevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] > uplevel, 1, 0)
    }
  }
  else if (type == 2) {
    lowlevel = quantile(x, prob = lowlevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] < lowlevel, 1, 0)
    }
  }
  else if (type == 3) {
    uplevel = quantile(x, prob = uplevel)
    lowlevel = quantile(x, prob = lowlevel)
    for (i in 1:n) {
      mat[i] = ifelse(x[i] > uplevel || x[i] < lowlevel, 1, 0)
    }
  }
  else if (type == 4 | type == 5) {
    uplevel_x = quantile(x, prob = uplevel)
    lowlevel_x = quantile(x, prob = lowlevel)
    uplevel_y = quantile(y, prob = uplevel)
    lowlevel_y = quantile(y, prob = lowlevel)
    for (i in 1:n) {
      mat_up_x[i] = ifelse(x[i] > uplevel_x, 1, 0)
      mat_low_x[i] = ifelse(x[i] < lowlevel_x, 1, 0)
      mat_up_y[i] = ifelse(y[i] > uplevel_y, 1, 0)
      mat_low_y[i] = ifelse(y[i] < lowlevel_y, 1, 0)
    }
  }
  sequence = seq(1, n)
  #numbering elements in indicator fct 
  if (type == 1 | type == 2 | type == 3) {
    gar = cbind(sequence, mat)
    junk = matrix(0, ncol = 2, nrow = n)
    for (i in 1:n) {
      if (mat[i] == 1) {
        junk[i, ] = gar[i, ]
      }
    }
    #filtering so only info about 1's in indicator fct are kept
    ind <- rowSums(junk == 0) != ncol(junk) #true false. true meaning it is an extreme event
    junk = junk[ind, ] #only extreme events
    n = dim(junk)[1]
    return_time = rep(0, n - 1)
    for (i in 1:n - 1) {
      return_time[i] = (junk[i + 1, 1] - junk[i, 1])
    }
    #vector with all return times created
    
  } 
  else if (type == 4 | type == 5) {
    gar = cbind(sequence, mat_up_x, mat_low_x, mat_up_y, mat_low_y)
    junk = matrix(0, ncol = 5, nrow = n)
    for (i in 1:n) {
      if (mat_up_x[i] == 1) {
        junk[i, ] = gar[i, ]
      }
      if (mat_low_x[i] == 1) {
        junk[i, ] = gar[i, ]
      }
      if (mat_up_y[i] == 1) {
        junk[i, ] = gar[i, ]
      }
      if (mat_low_y[i] == 1) {
        junk[i, ] = gar[i, ]
      }
    }
    ind <- rowSums(junk == 0) != ncol(junk) #true false. true meaning it is an extreme event
    junk = junk[ind, ] #only extreme events
    n = dim(junk)[1]
    
    if (type == 4){
      #return_time = rep(0, n - 1)
      
      return_time = matrix(0, ncol = 3, nrow = n)
      
      #we need to make this end for loop before possible errors. 
      if (max(which(junk[,2] == 1)) < max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(which(junk[,2] == 1)) 
      } else if (max(which(junk[,2] == 1)) > max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1))) - 1
      } else if (max(which(junk[,2] == 1)) == max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(which(junk[,2] == 1)) - 1
      }
      
      for (i in 0:(Loop_range-1)) {
        #if we have positive extreme
        if (junk[i + 1, 2] == TRUE){  
          # if (junk[i+1, 4] == TRUE){
          #   #then we have an extreme the same day. makes lags of interest very small. 
          # }
          #[row,col]
          next_extreme_obs_in_other_ts <- min(min(which(junk[((i+2):length(junk[,1])), 4] == 1)),
                                              min(which(junk[((i+2):length(junk[,1])), 5] == 1)), 
                                              10000) #just if gives infinity, if data is set up in a wrong way
          if (next_extreme_obs_in_other_ts == 10000){
            print("look into code, data is set up in an unfortunate way. change range i for return time loop.")
          }
    
          return_time[i+1,1] = (junk[i + 1 + next_extreme_obs_in_other_ts, 1] - junk[i + 1, 1])
          return_time[i+1,2] = junk[i + 1 + next_extreme_obs_in_other_ts, 4] #positive marker
          return_time[i+1,3] = junk[i + 1 + next_extreme_obs_in_other_ts, 5] #negative marker
        } else {
          #if i'th extreme is not postive it is negative so we just type else
          #return_time_low_abs[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
      }
      
      # removing zeros because old code influenced this code. 
      #running time is no problem so whatever.
      return_time <- return_time[which(return_time[,1] != 0),]
      
      
    } else if (type == 5){
      return_time = matrix(0, ncol = 3, nrow = n)
      
      #we need to make this end for loop before possible errors. 
      if (max(which(junk[,3] == 1)) < max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(which(junk[,3] == 1)) 
      } else if (max(which(junk[,3] == 1)) > max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1))) - 1
      } else if (max(which(junk[,3] == 1)) == max(max(which(junk[,4] == 1)), max(which(junk[,5] == 1)))){
        Loop_range = max(which(junk[,3] == 1)) - 1
      }
      
      for (i in 0:(Loop_range-1)) {
        #if we have positive extreme
        if (junk[i + 1, 3] == TRUE){      
          #[row,col]
          next_extreme_obs_in_other_ts <- min(min(which(junk[((i+2):length(junk[,1])), 4] == 1)),
                                              min(which(junk[((i+2):length(junk[,1])), 5] == 1)), 
                                              10000) #just if gives infinity, if data is set up in a wrong way
          if (next_extreme_obs_in_other_ts == 10000){
            print("look into code, data is set up in an unfortunate way. change range i for return time loop.")
          }
          
          return_time[i+1] = (junk[i + 1 + next_extreme_obs_in_other_ts, 1] - junk[i + 1, 1])
          return_time[i+1,2] = junk[i + 1 + next_extreme_obs_in_other_ts, 4] #positive marker
          return_time[i+1,3] = junk[i + 1 + next_extreme_obs_in_other_ts, 5] #negative marker
        } else {
          #return_time_low_abs[i+1] = (junk[i + 2, 1] - junk[i + 1, 1])
        }
      }
      
      # removing zeros because old code influenced this code. 
      #running time is no problem so whatever.
      
      return_time <- return_time[which(return_time[,1] != 0),]
      
    } 
  }  
  
  return_time_pos <- return_time[which(return_time[,2] == 1), 1]
  return_time_neg <- return_time[which(return_time[,3] == 1), 1]
  return_time_abs <- return_time[,1]
  
  table_rt_pos = as.matrix(table(return_time_pos))
  for (j in 1:dim(table_rt_pos)[1]) {
    table_rt_pos[j, 1] = table_rt_pos[j, 1]/dim(return_time)[1]
  }
  
  table_rt_neg = as.matrix(table(return_time_neg))
  for (j in 1:dim(table_rt_neg)[1]) {
    table_rt_neg[j, 1] = table_rt_neg[j, 1]/dim(return_time)[1]
  }
  
  table_rt_abs = as.matrix(table(return_time_abs))
  for (j in 1:dim(table_rt_abs)[1]) {
    table_rt_abs[j, 1] = table_rt_abs[j, 1]/dim(return_time)[1]
  }
  
  table_rt_pos = as.double(table_rt_pos)[1:maxlag]
  table_rt_neg = as.double(table_rt_neg)[1:maxlag]
  table_rt_abs = as.double(table_rt_abs)[1:maxlag]
  
  table_rt_pos[is.na(table_rt_pos)] <- 0
  table_rt_neg[is.na(table_rt_neg)] <- 0
  table_rt_abs[is.na(table_rt_abs)] <- 0
  
  if (histogram == 1) {
    
    #plots
    plot((0:maxlag), table_rt_abs[1:(maxlag+1)], 
         type = "n", xlab = "lag", ylab = "return time extremogram", 
         ylim = c(0, cutoff))
    lines((1:maxlag), table_rt_abs[1:maxlag], 
          col = "black", lwd = 1, type = "h")
    points((1:maxlag), table_rt_pos[1:maxlag], 
           col = "red", lwd = 1)
    points((1:maxlag), table_rt_neg[1:maxlag], 
           col = "blue", lwd = 1)
    abline((0:(maxlag - 1)), 0, col = 1, lwd = 1)
    
    # k = 14
    # table_rt_pos[k] + table_rt_neg[k]
    # table_rt_abs[k]
  }
  
  #return(list(aa, return_time, mean(return_time)))
  if (return_type == "abs") {
    aa = table_rt_abs
  } else if (return_type == "pos"){
    aa = table_rt_pos
  } else if (return_type == "neg"){
    aa = table_rt_neg
  }
  return(aa)
}

Stationary_bootstrap_return <- function (x, R, l, maxlag, uplevel, lowlevel, type, start = 1, cutoff = 1, alpha = 0.05) 
{
  #set default extremogram til ploting = 0
  boot = boot::tsboot(x, Extremogram_return, R, l = l, sim = "geom", 
                      endcorr = TRUE, maxlag = maxlag, uplevel = uplevel, 
                      lowlevel = lowlevel, type = type)
  tmp = boot[[2]]
  mat = tmp[, (1:maxlag)]
  k = dim(mat)[2]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(mat[, i], prob = (alpha/2)) #lower quantile
    pocket[i, 2] = mean(mat[, i]) #mean of extremogram on bootstraped samples
    pocket[i, 3] = quantile(mat[, i], prob = (1 - alpha/2)) #upper quantile
  }
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 2], lty = 1, lwd = 1, col = 1) #mean
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 3], lty = 4, lwd = 1, col = 1) #upper quantile
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 1], lty = 4, lwd = 1, col = 1) #lower quantile
}

Stationary_bootstrap_return_2 <- function (datamatrix, R, l, maxlag, uplevel, lowlevel, type, start = 1, cutoff = 1, alpha = 0.05) 
{
  #set default extremogram til ploting = 0
  boot = boot::tsboot(datamatrix, Extremogram_return_2, R, l = l, sim = "geom", 
                      endcorr = TRUE, maxlag = maxlag, uplevel = uplevel, 
                      lowlevel = lowlevel, type = type, return_type = "abs")
  tmp = boot[[2]]
  mat = tmp[, (1:maxlag)]
  k = dim(mat)[2]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(mat[, i], prob = (alpha/2)) #lower quantile
    pocket[i, 2] = mean(mat[, i]) #mean of extremogram on bootstraped samples
    pocket[i, 3] = quantile(mat[, i], prob = (1 - alpha/2)) #upper quantile
  }
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 2], lty = 1, lwd = 1, col = 1) #mean
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 3], lty = 4, lwd = 1, col = 1) #upper quantile
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 1], lty = 4, lwd = 1, col = 1) #lower quantile
}


Permutation_return_time <- function (x, m, exttype, maxlag, uplevel = 1, lowlevel = 0, start = 1, alpha = 0.05) 
{

  cc = matrix(0, ncol = m, nrow = maxlag)
  for (i in 1:m) {
    pBACC = sample(x)
    bb = Extremogram_return(pBACC, type = exttype, maxlag = maxlag, 
                            uplevel = uplevel, lowlevel = lowlevel)
    cc[, i] = bb[1:maxlag]
  }
  k = dim(cc)[1]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(cc[i, ], prob = (alpha/2))
    pocket[i, 3] = quantile(cc[i, ], prob = (1 - alpha/2))
  }
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 
                                      1], col = 1, lwd = 2)
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 
                                      3], col = 1, lwd = 2)
  return(pocket)
}

Permutation_return_time_2 <- function (datamatrix, m, exttype, maxlag, uplevel = 1, lowlevel = 0, start = 1, alpha = 0.05) 
{
  
  cc = matrix(0, ncol = m, nrow = maxlag)
  for (i in 1:m) {
    perm1 = sample(datamatrix[,1])
    perm2 = sample(datamatrix[,2])
    pBACC = matrix(c(perm1, perm2), nrow = length(perm1), ncol = 2)
    bb = Extremogram_return_2(pBACC, type = exttype, maxlag = maxlag, 
                            uplevel = uplevel, lowlevel = lowlevel, return_type = "abs")
    cc[, i] = bb[1:maxlag]
  }
  k = dim(cc)[1]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(cc[i, ], prob = (alpha/2))
    pocket[i, 3] = quantile(cc[i, ], prob = (1 - alpha/2))
  }
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 
                                      1], col = 1, lwd = 2)
  lines(start:(k - 1 + start), pocket[start:(k - 1 + start), 
                                      3], col = 1, lwd = 2)
}

iid_function_return_time <- function(x, p){
  return(p*(1-p)^(x-1))
}

### Spectral density of extremesfunctions 

scaling_function = function(datamatrix, uplevel, lowlevel){
  uplevel_value_x = quantile(datamatrix[,1], prob = uplevel)
  lowlevel_value_x = quantile(datamatrix[,1], prob = lowlevel)
  uplevel_value_y = quantile(datamatrix[,2], prob = uplevel)
  lowlevel_value_y = quantile(datamatrix[,2], prob = lowlevel)
  
  datamatrix_v1_scaled = rep(0, dim(datamatrix)[1])
  datamatrix_v2_scaled = rep(0, dim(datamatrix)[1])
  
  for (i in 1:dim(data_scatter)[1]){
    if (datamatrix[i,1] < 0){
      datamatrix_v1_scaled[i] = datamatrix[i,1]/abs(lowlevel_value_x)
    } else if (datamatrix[i,1] >= 0){
      datamatrix_v1_scaled[i] = datamatrix[i,1]/abs(uplevel_value_x)
    }
  }
  
  for (i in 1:dim(data_scatter)[1]){
    if (datamatrix[i,2] < 0){
      datamatrix_v2_scaled[i] = datamatrix[i,2]/abs(lowlevel_value_y)
    } else if (datamatrix[i,2] >= 0){
      datamatrix_v2_scaled[i] = datamatrix[i,2]/abs(uplevel_value_y)
    }
  }

  datamatrix_new = cbind(datamatrix_v1_scaled,datamatrix_v2_scaled)
  
  return(datamatrix_new)
}

Spectral_density_fct <- function(datamatrix, uplevel, lowlevel, histogram = 0, no_of_bins, bootstrap = TRUE, lag = 0, extreme_type = "one")
{
  data_scatter <- cbind(datamatrix[1:(dim(datamatrix)[1]- lag), 1], 
                        datamatrix[(1+lag):dim(datamatrix)[1], 2])
  
  #scaling with quantiles (for true picture of dependency)
  #data_scatter <- scaling_function(datamatrix = data_scatter, uplevel = uplevel, lowlevel = lowlevel)
  uplevel_value_x = quantile(data_scatter[,1], prob = uplevel)
  lowlevel_value_x = quantile(data_scatter[,1], prob = lowlevel)
  uplevel_value_y = quantile(data_scatter[,2], prob = uplevel)
  lowlevel_value_y = quantile(data_scatter[,2], prob = lowlevel)
  
  datamatrix_v1_scaled = rep(0, dim(data_scatter)[1])
  datamatrix_v2_scaled = rep(0, dim(data_scatter)[1])
  
  for (i in 1:dim(data_scatter)[1]){
    if (datamatrix[i,1] < 0){
      datamatrix_v1_scaled[i] = data_scatter[i,1]/abs(lowlevel_value_x)
    } else if (datamatrix[i,1] >= 0){
      datamatrix_v1_scaled[i] = data_scatter[i,1]/abs(uplevel_value_x)
    }
  }
  
  for (i in 1:dim(data_scatter)[1]){
    if (datamatrix[i,2] < 0){
      datamatrix_v2_scaled[i] = data_scatter[i,2]/abs(lowlevel_value_y)
    } else if (datamatrix[i,2] >= 0){
      datamatrix_v2_scaled[i] = data_scatter[i,2]/abs(uplevel_value_y)
    }
  }
  
  data_scatter = cbind(datamatrix_v1_scaled,datamatrix_v2_scaled)
  
  
  
  #extreme values only
  if (extreme_type == "one"){
    data_scatter <- data_scatter[which(data_scatter[,1] > 1 | data_scatter[,1] < -1
                                       | data_scatter[,2] > 1 | data_scatter[,2] < -1),]
  } else if (extreme_type == "both"){
    data_scatter <- data_scatter[which(data_scatter[,1] > 1 & data_scatter[,2] > 1
                                       | data_scatter[,1] < -1 & data_scatter[,2] > 1
                                       | data_scatter[,1] > 1 & data_scatter[,2] < -1
                                       | data_scatter[,1] < -1 & data_scatter[,2] < -1),]
  } else if (extreme_type == "upper_upper"){
    data_scatter <- data_scatter[which(data_scatter[,1] > 1 & data_scatter[,2] > 1),]
  } else if (extreme_type == "lower_lower"){
    data_scatter <- data_scatter[which(data_scatter[,1] < -1 & data_scatter[,2] < -1),]
  } else if (extreme_type == "upper_lower"){
    data_scatter <- data_scatter[which(data_scatter[,1] > 1 & data_scatter[,2] < -1),]
  } else if (extreme_type == "lower_upper"){
    data_scatter <- data_scatter[which(data_scatter[,1] < -1 & data_scatter[,2] > 1),]
  }
    
  angle_of_triangle_in_radians <- atan(data_scatter[,2]/data_scatter[,1]) 
  
  
  angle_in_circle <- ifelse(data_scatter[,1] < 0 & data_scatter[,2] > 0, #if in second quadrant
                            pi +  angle_of_triangle_in_radians, 
                            ifelse(data_scatter[,1] < 0 & data_scatter[,2] < 0, #if in third quadrant
                                   - pi +  angle_of_triangle_in_radians, 
                                   ifelse(data_scatter[,1] > 0 & data_scatter[,2] < 0, #if in fourth quadrant
                                          angle_of_triangle_in_radians, 
                                          ifelse(data_scatter[,1] > 0 & data_scatter[,2] > 0, #if in first quadrant
                                                 angle_of_triangle_in_radians, 
                                                 0
                                          ))))
  
  bins <- seq(-pi, pi, by = pi/no_of_bins)
  Count <- cut(angle_in_circle, bins)
  df <- data.frame(table(Count),
                   num_value_median = c(seq(-pi, pi, by = pi/no_of_bins)[1:(2*no_of_bins)] + pi/(no_of_bins*2)))
  
  rownames(df) <- NULL
  names(df) <- c('dir', 'mag', 'num_value_median')
  df$mag <- df$mag/length(angle_in_circle)
  
  
    return(if (bootstrap == TRUE) {df$mag } else {df})
}

Spectral_density_stationary_bootstrap_conf_band <- function(datamatrix, R, l, uplevel, lowlevel, no_of_bins, alpha = 0.05, lag, extreme_type) 
{
  #ts <- cbind(x,y)
  boot = boot::tsboot(tseries = datamatrix, statistic = Spectral_density_fct, R = R, l = l, sim = "geom", #geom makes block sizes geometrically distributed
                      endcorr = TRUE, uplevel = uplevel, lowlevel = lowlevel, no_of_bins = no_of_bins, lag = lag, extreme_type = extreme_type)
  tmp = boot[[2]]
  k = dim(tmp)[2]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(tmp[, i], prob = (alpha/2))
    pocket[i, 2] = mean(tmp[, i])
    pocket[i, 3] = quantile(tmp[, i], prob = (1 - alpha/2))
  }
  
  conf_band_df <- data.frame(pocket)
  rownames(conf_band_df) <- NULL
  names(conf_band_df) <- c('low', 'mean', 'high')
  return(conf_band_df)
}

Spectral_density_permutation <- function (datamatrix, m, uplevel, lowlevel, no_of_bins, alpha = 0.05, lag, extreme_type) 
{
  
  cc = matrix(0, ncol = m, nrow = (2*no_of_bins))
  for (i in 1:m) {
    perm1 = sample(datamatrix[,1])
    perm2 = sample(datamatrix[,2])
    pBACC = matrix(c(perm1, perm2), nrow = length(perm1), ncol = 2)
    bb = Spectral_density_fct(pBACC, extreme_type = extreme_type, 
                              uplevel = uplevel, lowlevel = lowlevel, 
                              no_of_bins = no_of_bins, lag = lag)
    cc[, i] = bb
  }
  k = dim(cc)[1]
  pocket = matrix(0, ncol = 3, nrow = k)
  for (i in 1:k) {
    pocket[i, 1] = quantile(cc[i, ], prob = (alpha/2))
    pocket[i, 2] = quantile(cc[i, ], prob = 0.5)
    pocket[i, 3] = quantile(cc[i, ], prob = (1 - alpha/2))
  }
  perm_df <- data.frame(pocket)
  rownames(perm_df) <- NULL
  names(perm_df) <- c('low', 'mean', 'high')
  return(perm_df)
}

### Spectral density plot example

library(circular)

no_of_bins <- 50
uplevel  = 0.95
lowlevel  = 1 - uplevel
rep = 1000 #number of bootstrap simulations
mean_block_size = 320*24 #from sample extremogram #mean block size
lag = 0

extreme_type = "one" #one, both, upper_upper, lower_lower , upper_lower, lower_upper

### ETH x, BTC y, h = 0
df <- Spectral_density_fct(datamatrix = DataTimeSeries_LogReturn_matrix,
                           uplevel = uplevel, lowlevel = lowlevel, histogram = 0,
                           no_of_bins = no_of_bins, bootstrap = FALSE, lag = lag, extreme_type = extreme_type)

conf_band_df <- Spectral_density_stationary_bootstrap_conf_band(datamatrix = DataTimeSeries_LogReturn_matrix,
                                                                R = rep, l = mean_block_size, uplevel = uplevel, lowlevel = lowlevel,
                                                                no_of_bins = no_of_bins, alpha = 0.05, lag = lag, extreme_type = extreme_type)
df = data.frame(df, conf_band_df)

# plot

spectral_density_plot_h0_one <- ggplot(df) +
  geom_bar(aes(x = num_value_median, y = mag), width = pi/no_of_bins, stat="identity",
           colour = "black", fill="lightblue") +
  coord_polar(theta = "x", start=pi/2, direction = -1) + 
  scale_x_continuous(breaks  = c(seq(-pi/2, pi, pi/2)),
                     labels = c("- \u03c0/2", "0", "\u03c0/2", "\u03c0")) +
  ggtitle(paste("Spectral density between the two exchange rates at lag",lag)) +
  labs(y = "Magnitude", x = "Angular value") +
  geom_col(aes(x = num_value_median, y = mag), position = position_dodge(width=0.2), stat = "identity", colour = "black", fill="lightblue") +
  geom_errorbar(aes(x = num_value_median, y = mag, ymin = low, ymax = high), width = 0.03)

extreme_type = "both"

df <- Spectral_density_fct(datamatrix = DataTimeSeries_LogReturn_matrix,
                           uplevel = uplevel, lowlevel = lowlevel, histogram = 0,
                           no_of_bins = no_of_bins, bootstrap = FALSE, lag = lag, extreme_type = extreme_type)

conf_band_df <- Spectral_density_stationary_bootstrap_conf_band(datamatrix = DataTimeSeries_LogReturn_matrix,
                                                                R = rep, l = mean_block_size, uplevel = uplevel, lowlevel = lowlevel,
                                                                no_of_bins = no_of_bins, alpha = 0.05, lag = lag, extreme_type = extreme_type)
df = data.frame(df, conf_band_df)

# plot

spectral_density_plot_h0_both <- ggplot(df) +
  geom_bar(aes(x = num_value_median, y = mag), width = pi/no_of_bins, stat="identity",
           colour = "black", fill="lightblue") +
  coord_polar(theta = "x", start=pi/2, direction = -1) + 
  scale_x_continuous(breaks  = c(seq(-pi/2, pi, pi/2)),
                     labels = c("- \u03c0/2", "0", "\u03c0/2", "\u03c0")) +
  ggtitle(paste("Spectral density between the two exchange rates at lag",lag)) +
  labs(y = "Magnitude", x = "Angular value") +
  geom_col(aes(x = num_value_median, y = mag), position = position_dodge(width=0.2), stat = "identity", colour = "black", fill="lightblue") +
  geom_errorbar(aes(x = num_value_median, y = mag, ymin = low, ymax = high), width = 0.03)

grid.arrange(spectral_density_plot_h0_one, spectral_density_plot_h0_both,
             #labels = c("A", "B", "C"),
             ncol = 2, nrow = 1)

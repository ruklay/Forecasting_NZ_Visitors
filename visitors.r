#--------------------------------------------------------
# BEGIN FUNCTION visitors
#--------------------------------------------------------
visitors <- function(Mt=6, year_start=1922, year_end=2015, d=1) {
  
  if(Mt%%2 != 0) {
    stop("Mt needs to be an even interger (e.g. 6, 12).")
  }
  
  visitors <- read.csv("visitors.csv", colClasses = "numeric")
  n <- nrow(visitors)
  
  # Subset the dataset
  # "start" is the begining of time index, Total = 1141 months
  start=(as.numeric(year_start)-1922)*12+1
  end = (as.numeric(year_end)-1922+1)*12
  visitors <- visitors[start:end,]
  n <- nrow(visitors)
  
  New.Time.Index <- c(1:n)
  visitors$New.Time.Index <- unlist(New.Time.Index)
  
  # Set Year/Month for plotting graph
  YearMonth <- rep(0,n)
  
  for(i in 1:n) {
    YearMonth[i] <- as.numeric(paste(visitors$Year[i],sprintf("%02d",visitors$Month[i]),sep=""))
  }
  
  visitors$YearMonth <- unlist(YearMonth)
  
  # Set moving average (need to be even integer)
  mv <- Mt
  m <- mv/2
  
  # Calculate and add Mt --> Moving Average
  Mt <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      Mt[i] <- NA
    } else if (i>n-m) {
      Mt[i] <- NA
    } else {
      tmp.Mt <- 0
      begin <- i-m+1
      end <- i+m-1
      for (j in begin:end) {
        tmp.Mt <- tmp.Mt + visitors$Counts[j]
      }
      Mt[i] <- (visitors$Counts[i-m]
                + 2*tmp.Mt
                + visitors$Counts[i+m])/(2*mv)
    }
  }
  
  visitors$Mt <- unlist(Mt)
  
  # Calculate and add Bt (Yt/Mt)
  Bt <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      Bt[i] <- NA
    } else if (i>n-m) {
      Bt[i] <- NA
    } else {
      Bt[i] <- visitors$Counts[i]/visitors$Mt[i]
    }
  }
  
  visitors$Bt <- unlist(Bt)
  
  # Calculate and add Qt (average of Bt on each period)
  Qt <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      Qt[i] <- NA
    } else if (i>n-m) {
      Qt[i] <- with(visitors, mean(visitors$Bt[visitors$Month==visitors$Month[i]], na.rm=TRUE))
    } else {
      Qt[i] <- with(visitors, mean(visitors$Bt[visitors$Month==visitors$Month[i]], na.rm=TRUE))
    }
  }
  
  visitors$Qt <- unlist(Qt)
  
  # Calculate and add Ct (Yt/Qt)
  Ct <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      Ct[i] <- NA
    } else if (i>n-m) {
      Ct[i] <- NA
    } else {
      Ct[i] <- visitors$Counts[i]/visitors$Qt[i]
    }
  }
  
  visitors$Ct <- unlist(Ct)
  
  # Calculate the linear regression equation
  lm.r <- findRegression(visitors, d)
  
  # Calculate Linear R of Ct * Qt
  LCt.Qt <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      LCt.Qt[i] <- NA
    } else if (i>n-m) {
      LCt.Qt[i] <- (lm.r$b0 + lm.r$b1 * visitors$New.Time.Index[i] + lm.r$b2 * (visitors$New.Time.Index[i]^2)) * visitors$Qt[i]
    } else {
      LCt.Qt[i] <- (lm.r$b0 + lm.r$b1 * visitors$New.Time.Index[i] + lm.r$b2 * (visitors$New.Time.Index[i]^2)) * visitors$Qt[i]
    }
  }
  
  visitors$LCt.Qt <- unlist(LCt.Qt)
  
  # Calculate and add Et (Yt/(Mt*Qt))
  # Calculate and add et (Yt-LCt.Qt)
  # Calculate and add et100 (et*100/Yt)
  Et <- rep(0,n)
  et <- rep(0,n)
  et100 <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      Et[i] <- NA
      et[i] <- NA
      et100[i] <- NA
    } else if (i>n-m) {
      Et[i] <- NA
      et[i] <- visitors$Counts[i]-visitors$LCt.Qt[i]
      et100[i] <- (et[i]*100)/visitors$Counts[i]
    } else  {
      Et[i] <- visitors$Counts[i]/(visitors$Mt[i]*visitors$Qt[i])
      et[i] <- visitors$Counts[i]-visitors$LCt.Qt[i]
      et100[i] <- (et[i]*100)/visitors$Counts[i]
    }
  }
  
  visitors$Et <- unlist(Et)
  visitors$et <- unlist(et)
  visitors$et100 <- unlist(et100)
  
  # Calcuate and add et.p2 --> et^2
  # Calcuate and add et100.abs --> abs(et100)
  et.p2 <- rep(0,n)
  et100.abs <- rep(0,n)
  
  for(i in 1:n) {
    if (i < m+1) {
      et.p2[i] <- NA
      et100.abs[i] <- NA
    } else if (i>n-m) {
      et.p2[i] <- visitors$et[i]^2
      et100.abs[i] <- abs(et100[i])
    } else  {
      et.p2[i] <- visitors$et[i]^2
      et100.abs[i] <- abs(et100[i])
    }
  }
  
  visitors$et.p2 <- unlist(et.p2)
  visitors$et100.abs <- unlist(et100.abs)
  
  # Write output file
  write.csv(visitors, file = "visitors_output.csv")
  
  return(visitors)
}

#--------------------------------------------------------
# BEGIN FUNCTION findError
#--------------------------------------------------------
findError <- function(visitors) {
  # Calculate and add MSE --> avg(et.p2) 
  # Calculate and add SDE --> sqrt(MSE)
  # Calculate and add MAPE --> avg(et100.abs)
  MSE <- mean(visitors$et.p2, na.rm=TRUE)
  SDE <- sqrt(MSE)
  MAPE <- mean(visitors$et100.abs, na.rm=TRUE)
  
  e = data.frame(MSE=numeric(), SDE=numeric(), MAPE=numeric())
  e <- rbind(e, data.frame(MSE = MSE, SDE = SDE, MAPE = MAPE))
  
  return(e)
}

#--------------------------------------------------------
# BEGIN FUNCTION findRegression
#--------------------------------------------------------
findRegression <- function(visitors, d=1) {
  if (d==2) {
    lm.r <- lm(visitors$Ct~visitors$New.Time.Index+I(visitors$New.Time.Index^2))
    
    b0 <- coef(lm.r)[1]
    b1 <- coef(lm.r)[2]
    b2 <- coef(lm.r)[3]
  } else {
    lm.r <- lm(visitors$Ct~visitors$New.Time.Index)
    
    b0 <- coef(lm.r)[1]
    b1 <- coef(lm.r)[2]
    b2 <- 0
  }
  
  r2 <- summary(lm.r)$r.squared
  adj.r2 <- summary(lm.r)$adj.r.squared
  
  lm = data.frame(b0=numeric(), b1=numeric(), b2=numeric(), r2=numeric(), adj.r2=numeric())
  lm <- rbind(lm, data.frame(b0 = b0, b1 = b1, b2 = b2, r2 = r2, adj.r2 = adj.r2))
  
  return(lm)
}

#--------------------------------------------------------
# BEGIN FUNCTION predictionYt
#--------------------------------------------------------
predictionYt <- function(visitors, year=2016, d=1) {
  
  year_start <- min(visitors$Year)
  
  start <- (as.numeric(year)-year_start)*12+1
  end <- start+11 
  
  Time.Index <- c(start:end)
  n <- length(Time.Index)
  Year <- rep(year,n)
  Month <- c(1:n)
  Prediction <- rep(0,n)
  Qt <- visitors[13:24,]$Qt
  
  lm.r <- findRegression(visitors, d)
  
  for(i in 1:n) {
    tmp <- (lm.r$b0 + lm.r$b1 * Time.Index[i] + lm.r$b2 * (Time.Index[i]^2)) * Qt[i]
    #Prediction[i] <- formatC(tmp, format="d", big.mark=',')
    Prediction[i] <- as.integer(tmp)
  }
  
  df = data.frame(Year,Month,Prediction)
  #df$Prediction<-levels(df$Prediction)
  
  return(df)
  
}

#--------------------------------------------------------
# BEGIN FUNCTION cornerText
#--------------------------------------------------------
cornerText <- function(text, location="topleft"){
  legend(location,legend=text, bty ="n", pch=NA) 
}

#--------------------------------------------------------
# BEGIN FUNCTION plotCtRegress
#--------------------------------------------------------
plotCtRegress <- function(visitors, d=1) {
  # Calculate the linear regression equation
  lm.r <- findRegression(visitors, d)
  
  # Plot the graph Ct and regression line
  plot(visitors$New.Time.Index,visitors$Ct,
       main="Deseasonalised Ct and Regression", 
       xlab="Time Index", ylab="No of visitors", col="blue")
  
  if (d==1) {
    text1 <- sprintf("Y = %f + %f * X",lm.r$b0, lm.r$b1)
  } else {
    text1 <- sprintf("Y = %f + %f * X + %f * X^2",lm.r$b0, lm.r$b1, lm.r$b2)
  }
  text2 <- sprintf("R-squared = %f",lm.r$r2)
  cornerText(text=paste(text1, text2, sep="\n"))
  
  linear.line <- lm.r$b0 + lm.r$b1 * visitors$New.Time.Index + lm.r$b2 * (visitors$New.Time.Index^2)
  lines(visitors$New.Time.Index, linear.line, col="red", lwd=2)
}

#--------------------------------------------------------
# BEGIN FUNCTION plotActualPrediction
#--------------------------------------------------------
plotActualPrediction <- function(visitors) {
  # Plot Actual (Yt) vs Prediction (LCt.Qt)
  plot(visitors$New.Time.Index, visitors$Counts, 
       main="Actual (Yt) vs Prediction (LCt.Qt)", 
       xlab="Time Index", ylab="No of visitors", 
       type="o", col="blue", lwd=2)
  lines(visitors$New.Time.Index, visitors$LCt.Qt, col="red", lwd=2)
}

#--------------------------------------------------------
# BEGIN FUNCTION plotMt
#--------------------------------------------------------
plotMt <- function(visitors) {
  # Plot Trend (Mt)
  plot(visitors$New.Time.Index, visitors$Mt, 
       main="Trend (Mt)", 
       xlab="Time Index", ylab="No of visitors", 
       type="o", col="blue")
}

#--------------------------------------------------------
# BEGIN FUNCTION plotQt
#--------------------------------------------------------
plotQt <- function(visitors) {
  # Plot Seasonality (Qt)
  plot(visitors$New.Time.Index, visitors$Qt, 
       main="Seasonality (Qt)", 
       xlab="Time Index", ylab="Seasonality", 
       type="o", col="blue")
}

#--------------------------------------------------------
# BEGIN FUNCTION plotEt
#--------------------------------------------------------
plotEt <- function(visitors) {
  # Plot Random Noise (Et)
  plot(visitors$New.Time.Index, visitors$Et, 
       main="Random Noise (Et)", 
       xlab="Time Index", ylab="Noise", 
       type="o", col="blue")
}

#--------------------------------------------------------
# BEGIN FUNCTION plotYt
#--------------------------------------------------------
plotYt <- function(visitors) {
  # Plot Actual Data (Yt)
  plot(visitors$New.Time.Index, visitors$Counts, 
       main="Actual Data (Yt)", 
       xlab="Time Index", ylab="No of visitors", 
       type="o", col="blue")
}

#--------------------------------------------------------
# BEGIN FUNCTION plotPrediction
#--------------------------------------------------------
plotPrediction <- function(prediction, year) {
  # Plot Prediction
  plot(prediction$Month, prediction$Prediction,
       main=paste("Prediction for year: ",year), 
       xlab="Time Index", ylab="No of visitors", 
       type="o", col="blue")
}

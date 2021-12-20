# wd <- setwd("C:/Users/jtshu/Desktop/FREEDOX/TestData/ScanType")
# wd.fls <- list.files(wd)
# path <- paste0(wd, "/", wd.fls[3])
# out <- parseCV2(path, span = .05)
# out
#
# out <- Mod_SD(out)
# 
# plot(out$potential, out$ avg)
# lines(out$potential, out$smth, col = "blue")
# lines(out$potential, out$base, col = "red")

# Parse an AIS CVB.json
parseCVB <- function(path, span = 0.05) {
  
  foo<- jsonlite::fromJSON(path)
  foo.df <- data.frame(foo$data)
  
  exps <- c()
  for(i in 1:(ncol(foo.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo.df) <- c("potential", exps)
  
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- apply(foo.df[,-c(1:8)], 1, mean)
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = span))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}

# Parse an AIS CV.XML
parseCV <- function(path, span = 0.05) {
  
  foo <- xmlParse(path)
  foo.list <- xmlToList(foo)
  df <- foo.list$experiment$results$series
  foo.df <- data.frame(matrix(unlist(df),
                              nrow=length(df), byrow=T), stringsAsFactors=FALSE)
  foo.df <- foo.df[-length(df),]
  foo.df <- data.frame(apply(foo.df, 2, as.numeric))
  
  exps <- c()
  for(i in 1:(ncol(foo.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo.df) <- c("potential", exps)
  
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- foo.df[,2]
  plot(foo.df$potential, foo.df$avg)
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = 0.05))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}

# Parse an AIS CV.csv
parseCV2 <- function(path, span = 0.05) {
  
  foo<- read_csv(path, skip = 29)
  foo.df <- data.frame(foo)
  
  exps <- c()
  for(i in 1:(ncol(foo.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo.df) <- c("potential", exps)
  
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- foo.df[,2]
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = span))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}


# Parse an AIS SWB.json
parseSWB <- function(path, span = 0.1) {
  
  foo<- jsonlite::fromJSON(path)
  foo.df <- data.frame(foo$data)
  foo2.df <- data.frame(foo.df[,1])
  rah <- seq(1, 30, by = 3)
  for (i in rah) {
    foo2.df <- cbind(foo2.df, foo.df[, i+1] + foo.df[, i+2])
  }
  
  exps <- c()
  for(i in 1:(ncol(foo2.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo2.df) <- c("potential", exps)
  
  foo.df <- foo2.df
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- apply(foo.df[,-c(1:8)], 1, mean)
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = span))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}

# Parse an AIS SW.XML
parseSW <- function(path, span = 0.1) {
  
  foo <- xmlParse(path)
  foo.list <- xmlToList(foo)
  df <- foo.list$experiment$results$series
  foo.df <- data.frame(matrix(unlist(df),
                              nrow=length(df), byrow=T), stringsAsFactors=FALSE)
  foo.df <- foo.df[-length(df),c(1,4)]
  foo.df <- data.frame(apply(foo.df, 2, as.numeric))
  
  exps <- c()
  for(i in 1:(ncol(foo.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo.df) <- c("potential", exps)
  
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- foo.df[,2]
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = span))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}

# Parse an AIS CV.csv
parseSW2 <- function(path, span = 0.05) {
  
  foo<- read_csv(path, skip = 29)
  foo.df <- data.frame(foo)
  
  exps <- c()
  for(i in 1:(ncol(foo.df)-1)) {exps[i] <- paste0("scan.", i)}
  names(foo.df) <- c("potential", exps)
  
  foo.df <- foo.df[1:which(foo.df$potential == min(foo.df$potential)),]
  foo.df$avg <- foo.df[,2]
  foo.df$smth <- predict(loess(foo.df$avg ~ foo.df$potential, span = span))
  foo.df$chull <- chullBL(y = foo.df$smth, envelope = "lower")
  foo.df$base <- foo.df$chull
  foo.df$blr <- foo.df$smth-foo.df$base
  
  return(foo.df)
}

chullBL <- function (y, envelope = "lower") {
  
  x <- 1:length(y)
  ch <- chull(x, y)
  ch.1 <- which(ch == 1)
  ch.ord <- c(ch[ch.1:length(ch)], ch[1:(ch.1 - 1)])
  ch.upper <- ch.ord[1:which(ch.ord == max(ch))]
  ch.lower <- c(ch.ord[which(ch.ord == max(ch)):length(ch.ord)],1)
  
  if(envelope == "lower") {
    
    lower<- 1:length(x)
    for (i in 1:(length(ch.lower)-1)) {
      y2 <- y[ch.lower[i:(i+1)]]
      x2 <- x[ch.lower[i:(i+1)]]
      if(diff(x2) == 1 | diff(x2) == -1) {
        
        lower[ch.lower[i]:ch.lower[i+1]] <- y2
        
      } else {
        
        mod <- lm(y~x, data = data.frame(y = y2, x = x2))
        lower[ch.lower[i]:ch.lower[i+1]] <- predict(mod, newdata = data.frame(x = x[ch.lower[i]:ch.lower[i+1]]))
        
      }
    }
    
    return(lower)
    
  } else if(envelope == "upper") {
    
    upper<- 1:length(x)
    for (i in 1:(length(ch.upper)-1)) {
      y2 <- y[ch.upper[i:(i+1)]]
      x2 <- x[ch.upper[i:(i+1)]]
      if(diff(x2) == 1 | diff(x2) == -1) {
        
        upper[ch.upper[i]:ch.upper[i+1]] <- y2
        
      } else {
        
        mod<- lm(y~x, data = data.frame(y = y2, x = x2))
        upper[ch.upper[i]:ch.upper[i+1]] <- predict(mod, newdata = data.frame( x = x[ch.upper[i]:ch.upper[i+1]]))
        
      }
    }
    return(upper)
    
  } else {
    
    upper<- 1:length(x)
    for (i in 1:(length(ch.upper)-1)) {
      y2 <- y[ch.upper[i:(i+1)]]
      x2 <- x[ch.upper[i:(i+1)]]
      if(diff(x2) == 1 | diff(x2) == -1) {
        
        upper[ch.upper[i]:ch.upper[i+1]] <- y2
        
      } else {
        
        mod<- lm(y~x, data = data.frame(y = y2, x = x2))
        upper[ch.upper[i]:ch.upper[i+1]] <- predict(mod, newdata = data.frame( x = x[ch.upper[i]:ch.upper[i+1]]))
        
      }
    }
    
    lower<- 1:length(x)
    for (i in 1:(length(ch.lower)-1)) {
      y2 <- y[ch.lower[i:(i+1)]]
      x2 <- x[ch.lower[i:(i+1)]]
      if(diff(x2) == 1 | diff(x2) == -1) {
        
        lower[ch.lower[i]:ch.lower[i+1]] <- y2
        
      } else {
        
        mod <- lm(y~x, data = data.frame(y = y2, x = x2))
        lower[ch.lower[i]:ch.lower[i+1]] <- predict(mod, newdata = data.frame(x = x[ch.lower[i]:ch.lower[i+1]]))
        
      }
    }
    
    return(c(upper, lower))
    
  }
  
}

# Calculates a baseline based on method of Slowey and Depasquale
Mod_SD <- function (x, pars = c(20, 40, 3, 20, 40, .005)) {
  
  #x <- out
  
  data <- data.frame(X = x$potential, Y = x$chull)
  
  lm.u <- lm(Y ~ X, data = data[nrow(data):(nrow(data)-10),])
  lm.l <- lm(Y ~ X, data = data[1:100,])
  
  cm <- rbind(coef(lm.u),coef(lm.l)) # Coefficient matrix
  int <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
  
  pts <- abs(data$X-int[1])
  p <- which(pts == min(pts))
  
  out1 <- predict(lm.u, newdata = data.frame(X = data$X[p:nrow(data)]))
  out1.df <- data$Y[p:nrow(data)]-out1
  
  o1 <- nrow(data) - (length(out1.df) - min(which(out1.df < 0)))
  if(o1 == Inf) {o1 <- length(data$X)-20}
  o2 <- o1 - pars[1] # alter for better fit
  o3 <- o1 + pars[4] # alter for better fit
  if(o3 > length(data$X)) {o3 <- length(data$X)}
  
  out2 <- predict(lm.l, newdata = data.frame(X = data$X[1:p]))
  out2.df <- data$Y[1:p]-out2
  out2.df.sd <- sd(out2.df[1:700])
  if (out2.df.sd == 0) {out2.df.sd <- 1e-24}
  
  o4 <- min(which(out2.df >= pars[3] * out2.df.sd)) # alter std. dev. multiplier for better fit
  if(o4 == 1) {o4 <- which(out2.df >= 1*out2.df.sd)[2]}
  o5 <- o4 + pars[2] # alter for better fit
  o6 <- o4 - pars[5] # alter for better fit
  
  dat <- data.frame(x = data$X[c(o1, o2, o3, o4, o5, o6)], y = data$Y[c(o1, o2, o3, o4, o5, o6)])
  #dat[1,2] <- dat[1,2] - (dat[1,2] - int[2])/4
  #dat[1,1] <- dat[1,1] - (dat[1,1] - dat[5,1])/3
  dat <- rbind(dat, int)
  
  weights <- c(1,1,1,1,1,1,pars[6]) # alter for better fit
  
  foo <- lm( y ~ poly(x, 4), data = dat, weights = weights )
  foo.p <- predict(foo, newdata = data.frame (x = data$X[o1:o5]))
  
  farad <- data$Y[c(o1:o5)]
  for (i in 1: length(farad)) { if (foo.p[i] <= farad[i]) {farad[i] <- foo.p[i]} }
  
  low <- predict(lm.l, newdata = data.frame(X = data$X[o1:o5]))
  for (i in 1: length(farad)) { if (farad[i] <= low[i]) {farad[i] <- low[i]} }
  
  x$base[o1:o5] <- farad
  x$blr <- x$smth-x$base
  
  plotOut <- plot(data[o3:o6,1], x$smth[o3:o6], type = 'l', xlab = "E(V vs Ag/AgCl)", ylab = "I",
                  xlim = c(data$X[o3],data$X[o6]), ylim = c(x$chull[o6-300],x$chull[o3]))
  lines(x  = data[o3:o6,1], y = data[o3:o6,2], col = 'green')
  abline(lm.u, lty = 2, col = 'gray50')
  abline(lm.l, lty = 2, col = 'gray50')
  points(x = int[1], y = int[2], col = 'blue', pch = 19, cex = 2)
  points(dat, col = 'blue', pch = 19)
  text(dat[1:3,], labels = c("A", "B", "C"), pos = 1, cex = 1)
  text(dat[4:7,], labels = c("D", "E", "F", "Int"), pos = 3, cex = 1)
  lines(x = data$X[o1:o5], y = foo.p[o1:o5], col = 'red', cex = 2)
  lines(x$potential[o3:o6], x$base[o3:o6], col = "blue")
  legend("topright", legend = c("Loess", "Convex", "Poly"), lwd = 1, col = c(1, "green", "blue"))
  
  out<-(list(x, plotOut))
  
  return(out)
}

Mod_CH <- function (x, pars = c(-0.2)) {
  
  # browser()
  
  st <- min(which(x$potential <= pars[1])) # adjust this to change where the hull starts
  ed <- length(x$potential)

  # Calculates a baseline based on a convex hull
  ch <- chullBL(x$blr[st:ed], envelope = "lower")
  ch <- c(x$blr[1:(st-1)], ch)
  
  plotOut <- plot(x$potential, x$blr, type = "l")
  lines(x$potential, ch, col = "blue")
  abline(v = pars[1], lty = 2, col = "gray50")
  legend("topleft", legend = c("Loess", "Convex"), lwd = 1, col = c(1, "blue"))
  
  x$base <- x$base + ch
  x$blr <- x$smth-x$base
  
  out<-(list(x, plotOut))
  
  return(out)
}


# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# func
# https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script("$('.dropdown-menu').click(function(e) {e.stopPropagation();});")
  )
}

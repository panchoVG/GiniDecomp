#############################################################################
#                                                                           #  
# GROUP- AND INDIVIDUAL-BASED APPROACHES TO HEALTH INEQUALITY:              #
#  TOWARDS AN INTEGRATION                                                   #
#                                                                           #
# Inaki PERMANYER, Isaac SASSON, Francisco VILLAVICENCIO                    #
#                                                                           #
# SET OF FUNCTIONS TO REPRODUCE RESULTS AND FIGURES FROM THE PAPER          #
# Sourced by: GiniDecomp.R                                                  #
#                                                                           #
#                                                                 July 2022 #
#############################################################################


# FUNCTION TO CALCULATE AVERAGE AGE AT DEATH WITHIN AGE GROUP (ax column)
Add_ax <- function(lt) {
  k <- dim(lt)[1] - 1
  lt$ax <- c(sapply(1:k, function(i) { (lt$Lx[i] - lt$lx[i+1]) / lt$dx[i] }), 
             lt$Lx[k+1] / lt$dx[k+1])
  return(lt)
}


# FUNCTION TO CALCULATE TWO-GROUP THEIL/GINI DECOMPOSITION
CalcDecomp <- function(lt1, lt2, p1 = .5,
                       abridged = FALSE, relative = TRUE) {
  
  ## lt1        Life table group 1
  ## lt2        Life table group 2
  ## p1         Proportion/share group 1
  ## abridged   Abridged life table (T/F)
  ## relative   Relative Gini (T/F)
  
  # Starting age of the last age group
  maxAge <- max(sort(unique(lt1$Age)))
  
  # Age groups by life table format
  if  (abridged) {
    x <- c(0, 1, seq(5, maxAge, by = 5))
  } else {
    x <- seq(0, maxAge, by = 1)
  }
  
  # Average age at death in each interval
  lt1 <- Add_ax(lt1)
  lt2 <- Add_ax(lt2)
  Ax1 <- x + lt1$ax
  Ax2 <- x + lt2$ax
  
  # (Normalized) Population shares
  p1 <- round(p1, 3)
  p2 <- round(1 - p1, 3)
  
  
  #-------------#
  # THEIL INDEX #
  #-------------#
  
  # THEIL INDEX OF THE LIFE TABLE
  T1 <- sum(lt1$dx * (Ax1 / lt1$ex[1]) * log(Ax1 / lt1$ex[1])) / lt1$lx[1]
  T2 <- sum(lt2$dx * (Ax2 / lt2$ex[1]) * log(Ax2 / lt2$ex[1])) / lt2$lx[1]
  
  # BETWEEN-GROUP COMPONENT
  ex <- lt1$ex[1] * p1 + lt2$ex[1] * p2
  Tb <- round(p1 * (lt1$ex[1] / ex) * log(lt1$ex[1] / ex) +
                p2 * (lt2$ex[1] / ex) * log(lt2$ex[1] / ex), 5)
  
  # WITHIN-GROUP COMPONENT
  Tw <- round((p1 * lt1$ex[1] * T1 / ex) +
                (p2 * lt2$ex[1] * T2 / ex), 5)
  
  # TOTAL THEIL
  Ttot <- round(Tb + Tw, 5)
  
  
  #--------------------#
  # GINI DECOMPOSITION #
  #--------------------#
  
  # Weigh deaths and radix by population shares
  lt1$dx <- lt1$dx * p1
  lt2$dx <- lt2$dx * p2
  N1 <- lt1$lx[1] * p1
  N2 <- lt2$lx[1] * p2

  # GINI WITHIN GROUP 1
  Gmat1 <- lt1$dx %*% t(lt1$dx) * sapply(1:length(Ax1), function(i) { abs(Ax1 - Ax1[i]) })
  G1 <- sum(Gmat1) / (2*N1^2)
  if (relative) {
    G1 <- round(G1 / lt1$ex[1], 5)
  } else G1 <- round(G1, 3)

  # GINI WITHIN GROUP 2
  Gmat2 <- lt2$dx %*% t(lt2$dx) * sapply(1:length(Ax2), function(i) { abs(Ax2 - Ax2[i]) })
  G2 <- sum(Gmat2) / (2*N2^2)
  if (relative) {
    G2 <- round(G2 / lt2$ex[1], 5)
  } else G2 <- round(G2, 3)
  
  # BETWEEN-GROUP MATRIX
  Gmat12 <- lt1$dx %*% t(lt2$dx) * sapply(1:length(Ax1), function(i) { Ax1 - Ax2[i] })
  
  # BETWEEN GROUP GINI
  Gb12 <- sum(abs(Gmat12)) / (2*N1*N2)
  if (relative) {
    Gb12 <-  round(2*Gb12 / (lt1$ex[1] + lt2$ex[1]), 5)
  } else Gb12 <- round(Gb12, 3)
  
  # ADVANTAGE COMPONENTS
  Ga12 <- sum(replace(Gmat12, Gmat12 < 0, 0)) / (2*N1*N2)
  Ga21 <- sum(abs(replace(Gmat12, Gmat12 > 0, 0))) / (2*N1*N2)
  if (relative) {
    Ga12 <- round(2*Ga12 / (lt1$ex[1] + lt2$ex[1]), 5)
    Ga21 <- round(2*Ga21 / (lt1$ex[1] + lt2$ex[1]), 5)
  } else {
    Ga12 <- round(Ga12, 3)
    Ga21 <- round(Ga21, 3)
  }
  
  # TOTAL GINI
  Gtot <- (sum(Gmat1) + sum(Gmat2) + 
             2*sum(abs(Gmat12))) / (2*(N1 + N2)^2)
  if (relative) {
    # Overall life expectancy
    e0 <- (p1 * lt1$ex[1] + p2 * lt2$ex[1])
    # Relative Gini coefficient
    Gtot <- round(Gtot / e0, 5)
  } else Gtot <- round(Gtot, 3)
    
  # Return results as data frame
  out <- data.frame(Ttot, Tb, Tw,
                    Gtot, G1, G2, Gb12, Ga12, Ga21, 
                    p1, p2)
  if (relative) {
    # Add s vector
    out$s1 <- round(p1*lt1$ex[1] / e0, 3)
    out$s2 <- round(p2*lt2$ex[1] / e0, 3)
  }
  out <- cbind(Year = unique(lt1$Year), Sex = unique(lt1$Sex), out)
  
  # Output
  return(out)
  
}


# FUNCTION TO CALCULATE UNDER/OVER PERFORMANCE PROBABILITIES
CalcPerform <- function(lt1, lt2, lt3 = NULL, abridged = FALSE) {
  
  ## lt1        Life table group 1
  ## lt2        Life table group 2
  ## lt3        Life table group 3
  ## abridged   Abridged life table (T/F)
  
  # Starting age of the last age group
  maxAge <- max(sort(unique(lt1$Age)))
  
  # Age groups by life table format
  if  (abridged) {
    x <- c(0, 1, seq(5, maxAge, by = 5))
  } else {
    x <- seq(0, maxAge, by = 1)
  }
  
  
  #--------------------------------------#
  # OVER/UNDER PERFORMANCE PROBABILITIES #
  #--------------------------------------#
  
  # TWO GROUPS
  n <- length(lt1$dx)
  P12 <- sum(lt2$dx[-n] * rev(cumsum(rev(lt1$dx[-1]))))
  P21 <- sum(lt1$dx[-n] * rev(cumsum(rev(lt2$dx[-1]))))
  P1x2 <- sum(lt1$dx * lt2$dx)
  
  # THREE GROUPS
  if (!is.null(lt3)) {
    P13 <- sum(lt3$dx[-n] * rev(cumsum(rev(lt1$dx[-1]))))
    P31 <- sum(lt1$dx[-n] * rev(cumsum(rev(lt3$dx[-1]))))
    P1x3 <- sum(lt1$dx * lt3$dx)
    P23 <- sum(lt3$dx[-n] * rev(cumsum(rev(lt2$dx[-1]))))
    P32<- sum(lt2$dx[-n] * rev(cumsum(rev(lt3$dx[-1]))))
    P2x3 <- sum(lt2$dx * lt3$dx)
  }
  
  # Return results as data frame
  if (!is.null(lt3)) {
    out <- round(data.frame(P12, P21, P1x2, P13, P31, P1x3, P23, P32, P2x3), 3)  
  } else out <- round(data.frame(P12, P21, P1x2), 3)  
  out <- cbind(Year = unique(lt1$Year), Sex = unique(lt1$Sex), out)
  
  # Output
  return(out)
  
}


# FUNCTION FOR PLOTTING THE DIFFERENT DECOMPOSITIONS
plotDecomp <- function(dat, varPlot, index, sex, colPalette, ltyPalette, 
                       lwdPalette, yLab = T, xLab = T) {
  
  ## dat          Data frame with decomposition estimates
  ## varPlot      Variables to plot
  ## index        Index to plot: Theil, Gini or Probs
  ## sex          Females/Males
  ## colPalette   Color palette to be used
  ## ltyPalette   Line type to be used
  ## lwdPalette   Line width to be used
  ## yLab         Include y-axis labels (T/F)
  ## xLab         Include x-axis labels (T/F)
  
  # Select years
  years <- sort(unique(dat$Year))
  
  # Y-axis limit
  if (index == 'Gini') {
    yLim <- c(3, 12)
    yTick1 <- 1
    yTick2 <- 1
    ymin <- 3
  }
  if (index == 'Theil') {
    yLim <- c(0, 0.055)
    yTick1 <- 0.01
    yTick2 <- 0.01 
    ymin <- 0
  }
  if (index == 'Probs') {
    yLim <- c(0.35, 0.63)
    yTick1 <- 0.01
    yTick2 <- 0.05
    ymin <- 0.35
  }
  if (yLab) {
    yLab <- seq(ymin, max(yLim), yTick2)
  } else yLab <- NA
  
  # X-axis limits
  if (min(years) == 1970) {
    yearAxis1 <- c(1985, 1990, 1995, years[years >= 2000])
    yearAxis2 <- c(seq(1985, 2010, 5), max(years))  
    if (xLab) {
      xLab <- c(1970, 1980, 1990, yearAxis2[yearAxis2 >= 2000])
    } else xLab <- NA
  } else {
    yearAxis1 <- years
    yearAxis2 <- c(2006, 2010, 2014, max(years))
    if (xLab) {
      xLab <- yearAxis2[yearAxis2 >= 2000]
    } else xLab <- NA
  }
  
  # Select data
  datAux <- dat[dat$Sex == sex, c(names(dat)[1], varPlot)]
  
  # Margins
  marPlot <- c(1.5, 2, 1.8, 1)
  if (index != 'Probs') {
    if (sex == 'Males') marPlot[c(1, 3)] <- marPlot[c(1, 3)] + c(.5, -.5)  
  } else {
    if (sex == 'Females') marPlot[4] <- marPlot[4] - .5
    if (sex == 'Males') marPlot[2] <- marPlot[2] - .5
  }
  
  # MAIN PLOT
  par(las = 1, mar = marPlot, mgp = c(3, .75, 0))
  btyPlot <- ifelse(min(years) == 1970, 'n', 'L') 
  plot(NA, xaxt = 'n', yaxt = 'n', bty = btyPlot, 
       xlim = range(yearAxis1), ylim = yLim, xlab = '', ylab = '', 
       panel.first = { 
         points(0, 0, pch = 16, cex = 1e6, col = grey(.95))
         grid(col = 'white', lty = 1) 
         })
  if (index != 'Probs' | min(years) == 1970) abline(v = max(datAux$Year), col = 'white')
  
  # TITLE
  title(bquote(italic(.(sex))), adj = 0.02, line = .8, cex.main = 1.1)
  
  # Y-AXIS
  axis(2, at = seq(min(yLim), max(yLim), yTick1), labels = NA, 
       tck = -.017, col = NA, col.ticks = 1)
  axis(2, at = seq(ymin, max(yLim), yTick2), labels = yLab, tck = -.03,
       col = NA, col.ticks = 1, cex.axis = .9)
  
  # X-AXIS
  axis(1, yearAxis1[yearAxis1 > 2000], labels = NA, tck = -.02,
       col = NA, col.ticks = 1)
  axis(1, at = yearAxis2, labels = xLab, tck = -.035, cex.axis = .9,
       col = NA, col.ticks = 1)
  
  # Customized axis when starting in 1970
  if (min(years) == 1970) {
    
    # Y-axis line
    abline(v = min(yearAxis1) - 1.25)
    
    # X-axis
    axis(1, at = c(yearAxis1[1] - 1.249, yearAxis1[1] + 2.3), labels = NA, tck = 0)
    axis(1, at = c(yearAxis1[2] - 2.3, yearAxis1[2] + 2.3), labels = NA, tck = 0)
    axis(1, at = c(yearAxis1[3] - 2.3, yearAxis1[3] + 2.3), labels = NA, tck = 0)
    axis(1, at = c(yearAxis1[4] - 2.3, max(yearAxis2) + 1.25), labels = NA, tck = 0)
    
    # Edges and marks
    if (index == 'Gini') {
      y0 <- 2.4
      y1 <- 2.85
    }
    if (index == 'Theil') {
      y0 <- -0.0036
      y1 <- -0.001
    }
    if (index == 'Probs') {
      y0 <- 0.3332
      y1 <- 0.3435
    }
    
    # Marks
    segments(x0 = yearAxis1[1:3] + 1.9, y0 = y0, 
             x1 = yearAxis1[1:3] + 2.7, y1 = y1, xpd = T)
    segments(x0 = yearAxis1[2:4] - 2.7, y0 = y0, 
             x1 = yearAxis1[2:4] - 1.9, y1 = y1, xpd = T) 
  
  } 
  
  # Horizontal line
  if (index == 'Probs') abline(h = 0.5, lty = 2)
  
  # Add LINES
  for (i in 1:length(varPlot)){
    lines(yearAxis1, datAux[, paste(varPlot[i])], 
          col = colPalette[i], lty = ltyPalette[i], lwd = lwdPalette[i])  
  }
  
}


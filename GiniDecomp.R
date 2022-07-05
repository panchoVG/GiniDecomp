#############################################################################
#                                                                           #  
# GROUP- AND INDIVIDUAL-BASED APPROACHES TO HEALTH INEQUALITY:              #
#  TOWARDS AN INTEGRATION                                                   #
#                                                                           #
# Inaki PERMANYER, Isaac SASSON, Francisco VILLAVICENCIO                    #
#                                                                           #
# CODE TO REPRODUCE RESULTS AND FIGURES FROM THE PAPER                      #
# Requires: Functions.R                                                     #
#                                                                           #
#                                                                 July 2022 #
#############################################################################


############################################################
### WORKING DIRECTORY AND PACKAGES                       ###  
############################################################

# Clear working directory
rm(list = ls())

# Libraries
# install.packages('RColorBewer')
library(RColorBrewer)
# install.packages('viridis')
library(viridis)

# Avoid scientific notation
options(scipen = 999)

# FUNCTIONS
source('Functions.R')


############################################################
### DATA                                                 ###  
############################################################

# LIFE TABLES
dat <- read.csv('Data/USlifeTables1970-2018.csv')
head(dat)

# POPULATION SHARES
popShares <- read.csv('Data/USpopShares1970-2017.csv')
popShares$Race[popShares$Race == 'Black or African American'] <- 'BlackAll'
popShares$Race[popShares$Race == 'White'] <- 'WhiteAll'
popShares <- droplevels(popShares[, c('Year', 'Race', 'Sex', 'Share')])
head(popShares)


############################################################
### FIGURE 1: MOTIVATION EXAMPLE                         ###  
############################################################

# Palette
colPalette <- viridis(n = 2, option = 'viridis', begin = 0.3, end = 0.7)[c(2, 1)]

# Life expectancy
ex <- c(0.5, 0.75)

# Y-axis limit
yLim <- c(0, 14)

# X-axis limit
xLim <- c(-0.03, 1.03)

# OUTPUT FILE
PNG <- F
if (PNG) png(filename = 'Fig1-Example.png', 
             width = 6, height = 3.2, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 4), 
                   c(0, 2, 2)), 
       widths = c(.07, 1, 1), heights = c(1, .1))

# Y-AIXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.7, 0.5, 'Distribution of deaths', cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.5, 'Age', cex = 1.4)

# PLOT DIFFERENT SCENARIOS
for (sce in c('Scenario 1', 'Scenario 2')) {
  
  # Main plot
  par(mar = c(0, 0.5, .8, 0))
  plot(x = xLim, y = yLim, xlab = '', ylab = '', col = NA, axes = F)
  
  # Distribution of deaths and life expectancy
  for (i in 1:2) {
    
    # Standard deviation
    std <- 0.03
    if (sce == 'Scenario 2' & i == 1) std <- 0.16
    
    # X-axis
    x <- seq(ex[i] - 0.15, ex[i] + 0.15, 0.005)
    if (sce == 'Scenario 2' & i == 1) x <- seq(ex[i] - 0.46, ex[i] + 0.46, 0.001)
    
    # Density
    polygon(c(x, rev(x)), 
            c(dnorm(x, mean = ex[i], sd = std), rep(0, length(x))),
            col = adjustcolor(colPalette[i], alpha.f = 0.5), border = NA)
    
    # Life expectancy
    segments(x0 = ex[i], y0 = yLim[1], x1 = ex[i], y1 = yLim[2], col = colPalette[i])
    
  }
  
  # Axes
  segments(x0 = xLim[1], y0 = 0, x1 = xLim[2], y1 = 0)
  segments(x0 = xLim[1], y0 = yLim[1], x1 = xLim[1], y1 = yLim[2])
  
  # Ticks
  axis(1, at = c(0, .25, .5, .75, 1), labels = NA, pos = 0)
  
  # Title
  title(bquote(italic(.(sce))), adj = 0.07, line = .05, cex.main = 1.1)  
  
  # Labels: Life expectancy
  text(x = ex[1] - 0.05, y = yLim[2] - 0.4, labels = expression(e[o]^A), col = colPalette[1])
  text(x = ex[2] + 0.05, y = yLim[2] - 0.4, labels = expression(e[o]^B), col = colPalette[2])
  
  # Labels: distribution of deaths
  if (sce == 'Scenario 1') {
    text(x = ex[1] - 0.1, y = yLim[2]*.45, labels = expression(d[x]^A), col = colPalette[1])
  } else text(x = ex[1] - 0.18, y = yLim[2]*.18, labels = expression(d[x]^A), col = colPalette[1])
  text(x = ex[2] + 0.1, y = yLim[2]*.45, labels = expression(d[x]^B), col = colPalette[2])
  
}

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, ex, i, PNG, sce, std, x, xLim, yLim)


############################################################
### FIGURE 2: DISTRIBUTION OF DEATHS                     ###  
############################################################

# Palette
colPalette <- viridis(n = 2, option = 'viridis', begin = 0.3, end = 0.7)[c(2, 1)]

# Life expectancy
exg <- c(50, 45, 40, 25)
exh <- c(50, 55, 60, 75)

# Ages
dt <- 0.05
x <- seq(0, 100, dt)

# Standard deviation
std <- 6

# Y-axis limit
yLim <- c(0, 0.075*dt)

# X-axis limit
xLim <- c(-0.03*max(x), 1.03*max(x))

# OUTPUT FILE
PNG <- F
if (PNG) png(filename = 'Fig2-DistrScenarios.png',
             width = 6, height = 5.5, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 4), 
                   c(1, 5, 6), 
                   c(0, 2, 2)),
       widths = c(.12, 1, 1), heights = c(1, 1, .12))

# Y-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.5, 'Distribution of deaths', cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.45, 'Age', cex = 1.4)

# PLOT DIFFERENT SCENARIOS
for (i in 1:4) {
  
  # Main plot
  par(mar = c(0, 0, 1, 0.8))
  plot(x = xLim, y = yLim, xlab = '', ylab = '', col = NA, axes = F)
  
  # Axes
  segments(x0 = xLim[1], y0 = 0, x1 = xLim[2]*1.1, y1 = 0)
  segments(x0 = xLim[1], y0 = yLim[1], x1 = xLim[1], y1 = yLim[2]*1.05)
  
  # Density 1
  dg <- dnorm(x, mean = exg[i], sd = std) * dt
  polygon(x = c(x, rev(x)), 
          y = c(dg, rep(0, length(dg))),
          col = adjustcolor(colPalette[1], alpha.f = 0.5), border = NA)
  
  # Density 2
  dh <- dnorm(x, mean = exh[i], sd = std) * dt
  polygon(x = c(x, rev(x)),
          c(dh, rep(0, length(dh))),
          col = adjustcolor(colPalette[2], alpha.f = 0.5), border = NA)
  
  # Labels
  if (i == 1) {
    text(x = (xLim[2] - xLim[1]) / 3.6, y = yLim[2] / 2,
         labels = expression(d[g]==d[h]), adj = 0.5, cex = 1.35)
  } else {
    text(x = exg[i] - 13, y = yLim[2] / 2,
         labels = expression(d[g]), col = colPalette[1], adj = 0.5, cex = 1.35)
    text(x = exh[i] + 13, y = yLim[2] / 2,
         labels = expression(d[h]), col = colPalette[2], adj = 0.5, cex = 1.35)
  }
  
  # Decomposition
  mat <- dg %*% t(dh) * sapply(1:length(x), function(i) {x - x[i]})
  Igh <- sum(abs(mat)) / 2
  Agh <- sum(replace(mat, mat < 0, 0)) / 2
  Ahg <- sum(abs(replace(mat, mat > 0, 0))) / 2
  
  # Out-performance probabilities: quasi-continuous framework
  dg <- dnorm(seq(0, 100, 0.001), mean = exg[i], sd = std) * 0.001
  dh <- dnorm(seq(0, 100, 0.001), mean = exh[i], sd = std) * 0.001
  Pgh <- sum(dh[-length(dh)] * rev(cumsum(rev(dg[-1]))))
  Phg <- sum(dg[-length(dg)] * rev(cumsum(rev(dh[-1]))))
  
  # Panel names
  if (i == 1) panelLab <- 'A'
  if (i == 2) panelLab <- 'B'
  if (i == 3) panelLab <- 'C'
  if (i == 4) panelLab <- 'D'
  text(x = 3.5, y = max(yLim), labels = panelLab, font = 2, cex = 1.8)

  # Legend
  n1 <- ifelse(i == 1, 1, ifelse(i == 4, 0, 2))
  if (i == 3) {
    Igh <- floor(100*Igh)/100
  } else Igh <- round(Igh, n1)
  legend('topright', 
         c(as.expression(bquote(I[B]^gh == .(format(Igh, nsmall = n1)))),
           as.expression(bquote(A[gh] == .(format(round(Agh, n1), nsmall = n1)))),
           as.expression(bquote(A[hg] == .(format(round(Ahg, n1), nsmall = n1)))),
           as.expression(bquote(P[gh] == .(format(round(Pgh, 2), nsmall = 2)))),
           as.expression(bquote(P[hg] == .(format(round(Phg, 2), nsmall = 2))))),
         bty = 'n')
  
  # Remove objects
  rm(Agh, Ahg, dg, dh, Igh, mat, n1, panelLab, Pgh, Phg)
  
}

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, dt, exg, exh, i, PNG, std, x, xLim, yLim)

  
############################################################
### FIGURE 3: DISTRIBUTION OF DEATHS                     ###  
############################################################

# Years of interest
Years <- range(dat$Year[dat$Race %in% c('BlackAll', 'WhiteAll')])

# Select data
dat1 <- dat[dat$Year %in% Years, c('Year', 'Race', 'Sex', 'Age', 'dx', 'ex')]
dat1 <- dat1[dat1$Race %in% c('BlackAll', 'WhiteAll'), ]

# Races
races <- c('Blacks', 'Whites')
dat1$Race[dat1$Race == 'BlackAll'] <- races[1]
dat1$Race[dat1$Race == 'WhiteAll'] <- races[2]

# Palette
colPalette <- viridis(n = 3, option = 'plasma', begin = 0.1, end = .9)[c(1, 3)]
names(colPalette) <- races

# Y-axis limit
yLim <- c(0, .04)

# OUTPUT FILE
PNG <- F
if (PNG) png(filename = 'Fig3-DistrDeaths.png',
             width = 6, height = 5.5, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 5), 
                   c(1, 4, 6), 
                   c(0, 2, 2),
                   c(0, 7, 7)),
       widths = c(.12, 1, 1), heights = c(1, 1, .12, .12))

# Y-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.35, 0.5, 'Distribution of deaths', cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.35, 'Age (years)', cex = 1.4)

# PLOT
par(las = 1)
for (year in Years) {
  
  # Select data from current year
  datAux <- dat1[dat1$Year == year, ]
  
  # Margins
  if (year == Years[1]) {
    marPlot <- c(1.3, 2, 1.8, 0)  
  } else marPlot <- c(1.3, 1.5, 1.8, .5)
  
  for (sex in c('Females', 'Males')) {
    
    # Margins
    if (sex == 'Males') marPlot[c(1, 3)] <- marPlot[c(1, 3)] + c(.5, -.5)
    
    # Main plot
    par(mar = marPlot)
    plot(NA, xlab = '', ylab = '', xlim = c(0, 110), bty = 'L', ylim = yLim,
         xaxt = 'n', yaxt = 'n')
    
    # Age at death distribution
    for (race in races) {
      # Color
      col1 <- adjustcolor(colPalette[paste(race)], alpha.f = 0.5)
      # Polygon
      polygon(x = c(sort(unique(datAux$Age)),
                    rev(sort(unique(datAux$Age)))),
              y = c(rep(0, length(unique(datAux$Age))),
                    rev(datAux$dx[datAux$Year == year & datAux$Sex == sex &
                                    datAux$Race == race])),
              border = NA, col = col1)
    }
    
    # Life expectancy at birth
    for (race in races) {
      # Add line
      abline(v = datAux$ex[datAux$Year == year & datAux$Sex == sex & 
                             datAux$Race == race][1],
             col = colPalette[paste(race)], lwd = 1.5)
    }
    
    # X-Axis
    xTick <- seq(0, 110, 10)
    if (sex == 'Males') {
      xLab <- xTick
      xLab[xLab == 100] <- NA
      axis(1, at = 100, cex.axis = .95)
    } else xLab <- NA
    axis(1, at = xTick, labels = xLab, cex.axis = .95)
    
    # Y-Axis
    yTick <- seq(0, yLim[2], 0.01)
    if (year == Years[1]) {
      yLab <- c(yTick[1], paste0(100*yTick[-1], '%'))
    } else yLab <- NA
    axis(2, at = yTick, labels = yLab, cex.axis = 1)
    
    # Title
    title(bquote(italic(.(paste(sex, year)))), 
          adj = 0.02, line = .8, cex.main = 1.1)

  }
  
  # Remove objects
  rm(col1, datAux, marPlot, race, sex, xLab, xTick, year, yLab, yTick)
  
}

# LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
legend(x = .45, y = .5, xjust = 1, yjust = .5, legend = races,
       col = adjustcolor(colPalette, alpha.f = 0.7),
       pch = 15, cex = 1.2, pt.cex = 2, horiz = T, bty = 'n')
legend(x = .48, y = .5, xjust = 0, yjust = .5, legend = 'Life expectancy at birth',
       col = 1, lwd = 1.5, cex = 1.2, seg.len = 2.5, horiz = T, bty = 'n')

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, dat1, PNG, races, Years, yLim)


############################################################
### FIGURE 4. GINI AND THEIL DECOMPOISITION              ###  
############################################################

# Years of interest
Years <- sort(unique(dat$Year[dat$Race %in% c('BalckAll', 'WhiteAll')]))


#-------------------------#
# TWO-GROUP DECOMPOSITION #
#-------------------------#

# Empty data frame to store results
datDecomp <- data.frame()

for (sex in c('Females', 'Males')) {
  
  # Pop shares in 2-group years
  shares <- popShares[popShares$Sex == sex & popShares$Year %in% Years, ]
  p1 <- shares$Share[shares$Race == 'WhiteAll']
  p2 <- shares$Share[shares$Race == 'BlackAll']
  
  # Normalize
  p1 <- p1 / (p1 + p2)
  
  # Two group decomposition: 1970-2017
  for (i in 1:length(Years)) {
    datDecomp <- 
      rbind(datDecomp,
            CalcDecomp(lt1 = dat[dat$Year == Years[i] & dat$Race == 'WhiteAll' & dat$Sex == sex, ],
                       lt2 = dat[dat$Year == Years[i] & dat$Race == 'BlackAll' & dat$Sex == sex, ],
                       p1 = p1[i], relative = F))
  }
  
}

# Decomposition results
head(datDecomp)

# Save output
SAVE <- F
if (SAVE) {
  # Decomposition results
  write.csv(datDecomp, row.names = F,
            file = 'Decomposition2Groups.csv')
}

# Remove objects
rm(i, p1, p2, SAVE, sex, shares, Years)


#----------#
# FIGURE 4 #
#----------#

# ABSOLUTE GINI 1970-2017, 2 GROUPS
PNG <- F
if (PNG) png(filename = 'Fig4-TheilGini2Groups.png',
             width = 6, height = 5.5, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 4, 2, 7), 
                   c(1, 5, 2, 8), 
                   c(0, 3, 3, 3),
                   c(0, 6, 0, 9)),
       widths = c(.11, 1, .11, 1), heights = c(1, 1, .1, .25))

# Y-AXIS 1
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.4, 0.5, 'Theil index', cex = 1.3, srt = 90)

# Y-AXIS 2
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.4, 0.5, 'Absolute Gini coefficient', cex = 1.3, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.55, 'Year', cex = 1.4)

# THEIL: Colors, line type and width
varPlot <- c('Ttot', 'Tb', 'Tw')
colPalette <- viridis(n = length(varPlot), 
                      option = 'viridis', begin = 0.2, end = .9)
ltyPalette <- c(rep(1, length(colPalette) -2), 4, 1)
lwdPalette <- rep(2, length(varPlot))
if ('Ttot' %in% varPlot) lwdPalette[which(varPlot == 'Ttot')] <- 3

# THEIL INDEX: FEMALES
plotDecomp(dat = datDecomp, varPlot = varPlot, index = 'Theil', 
           sex = 'Females', colPalette, ltyPalette, lwdPalette, xLab = F)

# THEIL INDEX: MALES
plotDecomp(dat = datDecomp, varPlot = varPlot, index = 'Theil', 
           sex = 'Males', colPalette, ltyPalette, lwdPalette)

# THEIL: LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
legend(x = 0.07, y = .9,
       legend = c('Total', NA, 'Between group', 'Within group'),
       col = c(colPalette[1], NA, colPalette[2:3]), 
       lty = c(ltyPalette[1], NA, ltyPalette[2:3]), 
       lwd = c(lwdPalette[1], NA, lwdPalette[2:3]), 
       seg.len = 2.6, ncol = 2, bty = 'n')
text(0.5, .95, 'Theil decomposition', font = 2)

# GINI: Colors, line type and width
varPlot <- c('Gtot', 'G2', 'G1', 'Ga21', 'Ga12')
colPalette <- viridis(n = 3, option = 'plasma', begin = 0.1, end = .9)[c(1, 3)]
colPalette <- c('black', rep(colPalette, 2))
ltyPalette <- c(rep(1, length(colPalette) - 2), 3, 3)
lwdPalette <- rep(2, length(varPlot))
if ('Gtot' %in% varPlot) lwdPalette[which(varPlot == 'Gtot')] <- 3

# GINI: FEMALES
plotDecomp(dat = datDecomp, varPlot = varPlot, index = 'Gini', 
           sex = 'Females', colPalette, ltyPalette, lwdPalette, xLab = F)

# GINI: MALES
plotDecomp(dat = datDecomp, varPlot = varPlot, index = 'Gini', 
           sex = 'Males', colPalette, ltyPalette, lwdPalette)

# GINI: LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
legend(x = 0.06, y = .9, 
       legend = c('Total', 'Blacks', 'Whites', NA,
                  'Black advantage', 'White advantage'),
       col = c(colPalette[1:3], NA, colPalette[4:5]),
       lty = c(ltyPalette[1:3], NA, ltyPalette[4:5]),
       lwd = c(lwdPalette[1:3], NA, lwdPalette[4:5]), 
       seg.len = 2.6, 
       ncol = 2, text.width = c(rep(.12, 3), rep(.15, 3)), bty = 'n')
text(0.5, .95, 'Gini decomposition', font = 2)

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, datDecomp, ltyPalette, lwdPalette, PNG, varPlot)


############################################################
### FIGURE 5. PERFORMANCE PROBABILITIES - 2 GROUPS       ###  
############################################################

#-------------------------------------#
# PERFORMANCE PROBABILITIES: 2 GROUPS #
#-------------------------------------#
# Blacks and whites irrespective of Hispanic origin, 1970-2017

# Years of interest
Years <- sort(unique(dat$Year[dat$Race == 'BlackAll']))

# Empty data frame to store results
datPerform2 <- data.frame()

# Calculate over/under performance probabilities
for (sex in c('Females', 'Males')) {
  
  for (i in 1:length(Years)) {
    datPerform2 <- 
      rbind(datPerform2,
            CalcPerform(lt1 = dat[dat$Year == Years[i] & dat$Race == 'WhiteAll' & dat$Sex == sex, ],
                        lt2 = dat[dat$Year == Years[i] & dat$Race == 'BlackAll' & dat$Sex == sex, ]))
  }
  
}

# Over/under performance probabilities
head(datPerform2)

# Save output
SAVE <- F
if (SAVE) {
  # Decomposition results
  write.csv(datPerform2, row.names = F,
            file = 'PerfProbs2Groups.csv')
}

# Remove objects
rm(i, SAVE, sex, Years)


#----------#
# FIGURE 5 #
#----------#

# PERFORMANCE PROBABILITIES
PNG <- F
if (PNG) png(filename = 'Fig5-PerfProbs2Groups.png',
             width = 6, height = 4, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 4), 
                   c(0, 2, 2), 
                   c(5, 5, 5)),
       widths = c(.12, 1, 1), heights = c(1, .1, .1))

# Y-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.4, 0.5, 'Performance probabilities', cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.4, 'Year', cex = 1.4)

# Colors, line type and width
colPalette <- viridis(n = 3, option = 'plasma', begin = 0.1, end = .9)[c(1, 3)]
ltyPalette <- c(3, 3)
lwdPalette <- rep(2, length(ltyPalette))

# PROBABILITIES: FEMALES
plotDecomp(dat = datPerform2, varPlot = c('P21', 'P12'),
           index = 'Probs', sex = 'Females', colPalette, ltyPalette, lwdPalette)

# PROBABILITIES: MALES
plotDecomp(dat = datPerform2, varPlot = c('P21', 'P12'),
           index = 'Probs', sex = 'Males', colPalette, ltyPalette, lwdPalette, yLab = F)

# LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
legend(x = .55, y = .5, xjust = .5, yjust = .5,
       legend = c('Blacks over whites', 'Whites over blacks'),
       col = colPalette, lty = ltyPalette, lwd = lwdPalette, text.width = .25,
       seg.len = 3, bty = 'n', horiz = T) 

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, datPerform2, ltyPalette, lwdPalette, PNG)


############################################################
### FIGURE 6. PERFORMANCE PROBABILITIES - 3 GROUPS       ###  
############################################################

#-------------------------------------#
# PERFORMANCE PROBABILITIES: 3 GROUPS #
#-------------------------------------#
# Non-Hispanic blacks, non-Hispanic whites, and Hispanics, 2006-2018

# Years of interest
Years <- sort(unique(dat$Year[dat$Race == 'Hispanic']))

# Empty data frame to store results
datPerform3 <- data.frame()

# Calculate over/under performance probabilities
for (sex in c('Females', 'Males')) {
  
  for (i in 1:length(Years)) {
    datPerform3 <- 
      rbind(datPerform3,
            CalcPerform(lt1 = dat[dat$Year == Years[i] & dat$Race == 'White' & dat$Sex == sex, ],
                        lt2 = dat[dat$Year == Years[i] & dat$Race == 'Black' & dat$Sex == sex, ],
                        lt3 = dat[dat$Year == Years[i] & dat$Race == 'Hispanic' & dat$Sex == sex, ]))
  }
  
}

# Over/under performance probabilities
head(datPerform3)

# Save output
SAVE <- F
if (SAVE) {
  # Decomposition results
  write.csv(datPerform3, row.names = F,
            file = 'PerfProbs3Groups.csv')
}

# Remove objects
rm(i, SAVE, sex, Years)


#----------#
# FIGURE 6 #
#----------#

# PERFORMANCE PROBABILITIES
PNG <- F
if (PNG) png(filename = 'Fig6-PerfProbs3Groups.png',
             width = 6, height = 4, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 4), 
                   c(0, 2, 2), 
                   c(5, 5, 5)),
       widths = c(.12, 1, 1), heights = c(1, .1, .1))

# Y-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.4, 0.5, 'Performance probabilities', cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
text(0.5, 0.4, 'Year', cex = 1.4)

# Colors, line type and width
colPalette <- rep(brewer.pal(3, 'Set2'), each = 2)
ltyPalette <- rep(c(1, 3), 3)
lwdPalette <- rep(1.8, length(ltyPalette))

# PROBABILITIES: FEMALES
plotDecomp(dat = datPerform3, varPlot = c('P12', 'P21', 'P13', 'P31', 'P23', 'P32'),
           index = 'Probs', sex = 'Females', colPalette, ltyPalette, lwdPalette)

# PROBABILITIES: MALES
plotDecomp(dat = datPerform3, varPlot = c('P12', 'P21', 'P13', 'P31', 'P23', 'P32'),
           index = 'Probs', sex = 'Males', colPalette, ltyPalette, lwdPalette, yLab = F)

# LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = '', ylab = '', col = NA, axes = F)
legend(x = .55, y = .5, xjust = .5, yjust = .5,
       legend = c('Non-Hisp whites over non-Hisp blacks', 
                  'Non-Hisp blacks over non-Hisp whites',
                  'Non-Hisp whites over Hispanic', 
                  'Hispanic over non-Hisp whites',
                  'Non-Hisp blacks over Hispanic', 
                  'Hispanic over non-Hisp blacks'),
       col = colPalette, lty = ltyPalette, lwd = lwdPalette, cex = .8, 
       text.width = .29, seg.len = 3, bty = 'n', ncol = 3) 

# CLOSE DEVICE
if (PNG) dev.off()

# Remove objects
rm(colPalette, datPerform3, ltyPalette, lwdPalette, PNG)


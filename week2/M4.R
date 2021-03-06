# 13/77
#####
# 執行前需先安裝套件
# vegan: Community Ecology Package
install.packages("vegan")
#####
library(vegan)
#####
# 從老師的網站下載 http://www.hmwu.idv.tw/web/R/data/DoubsFishData.zip
# 解壓縮後包含所需要的資料和程式庫
# 程式來源可以參考 Numerical Ecology with R 一書
#####
source("data/DoubsFishData/panelutils.R")
spe <- read.csv("data/DoubsFishData/NEwR data/DoubsSpe.csv", row.names=1)
env <- read.csv("data/DoubsFishData/NEwR data/DoubsEnv.csv", row.names=1)
spa <- read.csv("data/DoubsFishData/NEwR data/DoubsSpa.csv", row.names=1)

library(ade4)
data(doubs)
?doubs


# 14/77
spe
spe[1:5,1:10]
head(spe)
nrow(spe)	
ncol(spe)		
dim(spe)		
colnames(spe)		
rownames(spe)	
summary(spe)	


# 15/77
range(spe)
(ab <- table(unlist(spe)))
windows(title="Distribution of abundance classes")
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=gray(5:0/5))
sum(spe==0)
sum(spe==0)/(nrow(spe)*ncol(spe))


# 16/77
windows(title="Site Locations")
# 移除錯誤字元 "+"
plot(spa, asp=1, type="n", main="Site Locations", xlab="x coordinate (km)", ylab="y coordinate (km)")
lines(spa, col="light blue")
text(spa, row.names(spa), cex=0.8, col="red")
text(50, 10, "Upstream", cex=1.2, col="red")
text(30, 120, "Downstream", cex=1.2, col="red")


# 18/77
windows(title="Species Locations", 9, 9)
par(mfrow=c(1,4))
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"
plot(spa, asp=1, col="brown", cex=spe$TRU, main="Brown trout", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$OMB, main="Grayling", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BAR, main="Barbel", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BCO, main="Common bream", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)


# 19/77
spe.pres <- apply(spe > 0, 2, sum)
sort(spe.pres)
spe.relf <- 100*spe.pres/nrow(spe)
round(sort(spe.relf), 1)


# 20/77
windows(title="Frequency Histograms",8,5)
par(mfrow=c(1,2))
# 移除錯誤字元 "+"
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1, 
     xlab="Number of occurrences", ylab="Number of species", breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="Species Relative Frequencies", right=FALSE, 
     las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
     breaks=seq(0, 100, by=10), col="bisque")


# 21/77
sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)


# 22/77
windows(title="Species Richness", 10, 5)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(spe), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white", 
     bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)", 
     ylab="y coordinate (km)")
lines(spa, col="light blue")


# 23/77
?diversity
N0 <- rowSums(spe > 0)         
H <- diversity(spe)           
N1 <- exp(H)                   
N2 <- diversity(spe, "inv")  
J <- H/log(N0)                
E10 <- N1/N0                 
E20 <- N2/N0                 
(div <- data.frame(N0, H, N1, N2, E10, E20, J))


# 24/77
?decostand
spe[1:5, 2:4]
spe.pa <- decostand(spe, method="pa")
spe.pa[1:5, 2:4]


# 25/77
spe.scal <- decostand(spe, "max")
spe.scal[1:5,2:4]
#####
# add: missing assignment for spe.relsp
# from Numerical Ecology with R book
spe.relsp <- decostand(spe, "total", MARGIN = 2)
spe.relsp[1:5,2:4]
#####
apply(spe.scal, 2, max)
apply(spe.relsp, 2, sum)


# 26/77
spe.rel <- decostand(spe, "total")
spe.rel[1:5,2:4]
apply(spe.rel, 1, sum)
spe.norm <- decostand(spe, "normalize")
spe.norm[1:5,2:4]
norm <- function(x) sqrt(x%*%x)
apply(spe.norm, 1, norm)


# 27/77
spe.hel <- decostand(spe, "hellinger")
spe.hel[1:5,2:4]
apply(spe.hel, 1, norm)


# 28/77
spe.chi <- decostand(spe, "chi.square")
spe.chi[1:5,2:4]
spe.chi[7:9,]
spe.wis <- wisconsin(spe)
spe.wis[1:5,2:4]


# 29/77
windows(title="Loach")
par(mfrow=c(1,4))
boxplot(spe$LOC, sqrt(spe$LOC), log1p(spe$LOC), las=1, main="Simple transformation", names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scal$LOC, spe.relsp$LOC, las=1, main="Standardization by species",
        names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$LOC, spe.rel$LOC, spe.norm$LOC, las=1, main="Standardization by sites", names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$LOC, spe.wis$LOC, las=1, main="Double standardization",
        names=c("Chi-square", "Wisconsin"), col="orange")


# 31/77
windows(title="Species profiles", 9, 9)
plot(env$das, spe$TRU, type="l", col=4, main="Raw data",
     xlab="Distance from the source [km]", ylab="Raw abundance code")
lines(env$das, spe$OMB, col=3); lines(env$das, spe$BAR, col="orange")
lines(env$das, spe$BCO, col=2); lines(env$das, spe$LOC, col=1, lty="dotted")

plot(env$das, spe.scal$TRU, type="l", col=4, main="Species profiles (max)",
     xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.scal$OMB, col=3); lines(env$das, spe.scal$BAR, col="orange")
lines(env$das, spe.scal$BCO, col=2); lines(env$das, spe.scal$LOC, col=1, lty="dotted")

plot(env$das, spe.hel$TRU, type="l", col=4, main="Site profiles (Hellinger)",
     xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.hel$OMB, col=3); lines(env$das, spe.hel$BAR, col="orange")
lines(env$das, spe.hel$BCO, col=2); lines(env$das, spe.hel$LOC, col=1, lty="dotted")

plot(env$das, spe.chi$TRU, type="l", col=4, main="Double profiles (Chi-square)",
     xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.chi$OMB, col=3); lines(env$das, spe.chi$BAR, col="orange")
lines(env$das, spe.chi$BCO, col=2); lines(env$das, spe.chi$LOC, col=1, lty="dotted")
legend("topright", c("Brown trout", "Grayling", "Barbel", "Common bream", "Stone loach"), + col=c(4,3,"orange",2,1), lty=c(rep(1,4),3))


# 32/77
windows(title="Bubble maps", 9, 9)
par(mfrow=c(1,4))
# 移除錯誤字元 "+"
plot(spa, asp=1, main="Altitude", pch=21, col="white", 
     bg="red", cex=5*env$alt/max(env$alt), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Discharge", pch=21, col="white", 
     bg="blue", cex=5*env$deb/max(env$deb), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Oxygen", pch=21, col="white", 
     bg="green3", cex=5*env$oxy/max(env$oxy), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Nitrate", pch=21, col="white", 
     bg="brown", cex=5*env$nit/max(env$nit), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)


# 33/77
windows(title="Descriptor line plots")
par(mfrow=c(1,4))
plot(env$das, env$alt, type="l", xlab="Distance from the source (km)", 
     ylab="Altitude (m)", col="red", main="Altitude")
plot(env$das, env$deb, type="l", xlab="Distance from the source (km)", 
     ylab="Discharge (m3/s)", col="blue", main="Discharge")
plot(env$das, env$oxy, type="l", xlab="Distance from the source (km)", 
     ylab="Oxygen (mg/L)", col="green3", main="Oxygen")
plot(env$das, env$nit, type="l", xlab="Distance from the source (km)", 
     ylab="Nitrate (mg/L)", col="brown", main="Nitrate")


# 34/77
windows(title="Bivariate descriptor plots")
source("data/DoubsFishData/panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(env, panel=panel.smooth, 
      diag.panel=panel.hist, 
      main="Bivariate Plots with 
      Histograms and Smooth Curves")
par(op)


# 35/77
range(env$pen)
windows(title="Transformation and standardization of variable slope")
par(mfrow=c(1,4))
hist(env$pen, col="bisque", right=FALSE)
hist(log(env$pen), col="light green", right=F, main="Histogram of ln(env$pen)")
boxplot(env$pen, col="bisque", main="Boxplot of env$pen", ylab="env$pen")
boxplot(log(env$pen), col="light green", main="Boxplot of ln(env$pen)",
        ylab="log(env$pen)")


# 36/77
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean)
apply(env.z, 2, sd)
env.z <- as.data.frame(scale(env))
env.z

# 49/77
head(anscombe, 3)
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)
mapply(cor, anscombe[,1:4], anscombe[,5:8])
mapply(function(x, y) lm(y~x)$coefficients, anscombe[, 1:4], anscombe[, 5:8])

par(mfrow=c(2, 2))
regplot <- function(x, y){ 
    plot(y~x) 
    abline(lm(y~x), col="red")
}
mapply(regplot, anscombe[, 1:4], anscombe[, 5:8])


# 59/77
library("rgl")
open3d()
comet <- readOBJ("data/ESA_Rosetta_OSIRIS_67P_SHAP2P.obj")
class(comet)
str(comet)
shade3d(comet, col="gray")


# 60/77
source("https://bioconductor.org/biocLite.R")
biocLite("ALL")
library(ALL)
data(ALL)
ALL
str(ALL)
dim(exprs(ALL))
exprs(ALL)[1:3, 1:5]
table(ALL$mol.biol)
eset <- ALL[, ALL$mol.biol %in% 
                c("BCR/ABL", "ALL1/AF4")]
dim(exprs(eset))
f <- factor(as.character(eset$mol.biol))
eset.p <- apply(exprs(eset), 1, function(x) t.test(x ~ f)$p.value)
selected.eset <- eset[eset.p < 0.00001, ]
dim(selected.eset)
ma.col <- colorRampPalette(c("green", "black", "red"))(200)
var.col <- ifelse(f=="ALL1/AF4", "blue", "red")
heatmap(exprs(selected.eset), col=ma.col, ColSideColors=var.col,  
        scale="row")


# 61/77
source("https://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
Heatmap(exprs(selected.eset))


# 62/77
install.packages(c("tiff", "jpeg", "png", "fftwtools"), repos="http://cran.csie.ntu.edu.tw")
#####
# 須載入程式庫
source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
#####
#####
# 修正為載入 EBImage 才有 readImage function
# 老師的投影片中用的 library(jpeg) 需改為使用 readJPEG function
#####
library(EBImage) # (Repositories: BioC Software)
Transformers <- readImage("data/Transformers07.jpg")
(dims <- dim(Transformers))
Transformers
plot(c(0, dims[1]), c(0, dims[2]), type='n', 
     xlab="", ylab="")
rasterImage(Transformers, 0, 0, dims[1], dims[2])

# 63/77
#####
# 執行前需先安裝套件並載入
# RgoogleMaps
install.packages("RgoogleMaps")
library("RgoogleMaps")
#####
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "output/Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "output/Taiwan2.png", maptype = "terrain")

# 64/77
my.lat <- c(25.177339, 25.082288, 25.042185, 25.046254)
my.lon <- c(121.450003, 121.565481, 121.614548, 121.517732)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "output/my.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile = "output/my.png", cex=2.5, pch=20, col=1:4, add=F)


# 65/77
library(vcd)


# 67/77
n <- 1e+02
y <- as.factor(sample(LETTERS[1:4], n, replace=T, prob=c(0.1, 0.1, 0.5, 0.3)))
x1  <- rnorm(n)
x2  <- rbeta(n, 0.5, 0.5)
xydata <- data.frame(y, x1, x2)
par(mfrow=c(1,4))
boxplot(x1~y, data=xydata, ylab="x1", main="boxplot")
hist(x2, xlab="x2", main="hist")
barplot(table(y), xlab="y", col = 2:5, main="barplot")
plot(x1, x2, main="plot", col=as.integer(y)+1)


# 69/77
x <- rnorm(mean=1.5, 10000)
y <- rnorm(mean=1.6, 10000)
my.data <- data.frame(x, y)

pk <- c("RColorBrewer", "hexbin", "gplots")
install.packages(pk, repos="http://cran.csie.ntu.edu.tw")
library(RColorBrewer)
col_rb <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
plot(my.data, pch=16, col='black', cex=0.5)
library(hexbin)
h <- hexbin(my.data) # create a hexbin object
h

plot(h) # in grey level
plot(h, colramp=col_rb) # rainbow color


# 71/77
install.packages("tabplot")
library(tabplot)
tableplot(iris, nBins=150, sortCol=5)


# 72/77
tableplot(iris, nBins=50, sortCol=4)


# 73/77
require(ggplot2)
data(diamonds)
dim(diamonds)
head(diamonds)
tableplot(diamonds)




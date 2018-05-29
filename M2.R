#####
# reset plot 避免在 RStudio 中顯示比例異常
#####
dev.off()

# 21/41
x <- iris$Sepal.Length
y <- iris$Petal.Length
alpha <- 0.05
(vt <- (var.test(x, y)$p.value <= alpha))
t.test(x, y, var.equal = !vt ) 


# 27/41
source("https://bioconductor.org/biocLite.R")
biocLite("made4")
library(made4)
data(khan)
Anova.pvalues <- function(x){
  x <- unlist(x)
  SRBCT.aov.obj <- aov(x ~ khan$train.classes)
  SRBCT.aov.info <- unlist(summary(SRBCT.aov.obj))
  SRBCT.aov.info["Pr(>F)1"]
}
SRBCT.aov.p <- apply(khan$train, 1, Anova.pvalues)


# 28/41
order.p <- order(SRBCT.aov.p)
ranked.genes <- data.frame(pvalues=SRBCT.aov.p[order.p], ann=khan$annotation[order.p, ])
top5.gene.row.loc <- rownames(ranked.genes[1:5,  ])
summary(t(khan$train[top5.gene.row.loc, ]))

par(mfrow=c(1, 5), mai=c(0.3, 0.4, 0.3, 0.3))
usr <- par("usr")
myplot <- function(gene){ 
  boxplot(unlist(khan$train[gene, ]) ~ khan$train.classes, ylim=c(0, 6), main=ranked.genes[gene, 4])
  text(2, usr[4]-1, labels=paste("p=", ranked.genes[gene, 1], sep=""), col="blue")
  ranked.genes[gene,]
}


# 29/41
do.call(rbind, lapply(top5.gene.row.loc, myplot))

dev.off()

# 34/41
pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 
          +  4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug <- c(rep("A", 9), rep("B", 9), rep("C", 9))
migraine <- data.frame(pain, drug)
plot(pain ~ drug, data=migraine)
migraine.aov <- aov(pain ~ drug, data=migraine)
summary(migraine.aov)

kruskal.test(pain ~ drug, data=migraine)


# 35/41
pairwise.t.test(pain, drug, p.adjust="bonferroni")

TukeyHSD(migraine.aov)


# 36/41
hist(iris$Sepal.Width)

qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width, col="red")


# 37/41
#####
# 執行前需先安裝 nortest 套件
# nortest: Tests for Normality
install.packages("nortest")
#####

x <- iris$Sepal.Width
ks.test(x, 'pnorm', mean(x), sd(x))

library(nortest)
ad.test(iris$Sepal.Width)

shapiro.test(iris$Sepal.Width)


# 41/41
M <- as.table(rbind(c(762, 327, 468), 
                    c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat",
                              "Independent", 
                              "Republican"))
M
(res <- chisq.test(M))



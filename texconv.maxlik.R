# Converting tex file to useable data frame 

table <- readLines("lttable.tex")
table <- paste(table,collapse="")
g <- gregexpr("\\\\{2}",table)[[1]]
numr <- length(g)-1
c <- gregexpr("[A-Z]",table,ignore.case=FALSE)[[1]]
numc <- length(c)
d <- matrix(numeric(numc),nrow=1,ncol=(numc-1))
for(i in 1:numr){ 
  s <- substr(table,g[i],g[i+1])
  slist <- strsplit(s, "&")
  svector <- slist[[1]][-1]
  svector <- sub("\\\\","",svector)
  nvect <- numeric()
  for(j in 1:(numc-1)){
    nvect[j] <- as.numeric(svector[j])
  }
  d <- rbind(d,nvect)
}
d <- d[-1,]
firstcol <- c(1:numr)
d <- cbind(firstcol,d)

cnames <- numeric()
for(h in 1:numc){
  cnames[h] <- letters[h]
}
colnames(d) <- cnames

### Maximum Likelihood 

library(maxLik)

data <- read.csv("count.data.txt", header=TRUE)
y <- as.vector(data)
h <- c(1)
ll <- function(h,y){
  p <- (exp(-h)*(h^y))/(factorial(y))
  ll <- sum(log(p))
  return(-ll)
}

result <- optim(h, ll, method = "BFGS", y=y)

yo <- y
bmatrix <- matrix(ncol=1)
for(i in 1:1000){
  ind = sample(1:200,replace=TRUE)
  y <- vector()
  for (j in ind){
    y <- c(y,yo[j,])
  }
  bresults <- optim(h, ll, method = "BFGS", y=y)
  bmatrix <- rbind(bmatrix,bresults$par)
}
bmatrix <- bmatrix[-1,]

sderror <- sd(bmatrix)

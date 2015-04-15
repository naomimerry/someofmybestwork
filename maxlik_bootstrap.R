d <- read.csv("riskexperimentdata.csv")
d$aandloss = (2-d$choice)*as.integer(d$loss>0)
d$bandloss = (d$choice-1)*as.integer(d$loss>0)
d$bandnoloss = (d$choice-1)*as.integer(d$loss==0)
d$ch = d$choice-1

d <- d[order(d$round,d$id),]

aandlosslag = lag(ts(d$aandloss),-58)
bandlosslag = lag(ts(d$bandloss),-58)
bandnolosslag = lag(ts(d$bandnoloss),-58)
earnlag = lag(ts(d$earn),-58)
d2 <- data.frame(ts.union(ts(d),aandlosslag,bandlosslag,bandnolosslag,earnlag))
names(d2) <- c(names(d),"aandlosslag","bandlosslag","bandnolosslag","earnlag")

result <- glm(ch ~ aandlosslag + bandlosslag + bandnolosslag + earnlag, family=binomial(link="probit"),data=d2)
summary(result)

d2 <- d2[-which(is.na(d2$ch)),]
d2 <- d2[-which(is.na(d2$aandlosslag)),]


d2 <- d2[order(d2$id,d2$round),]
x <- as.matrix(d2[,26:29])
x <- cbind(integer(length(d2$aandlosslag))+1,x)
y <- as.vector(d2[,25])
b <- numeric(5)

xo<-x
yo<-y

ll <- function(b,x,y){
  p <- pnorm(x %*% b,log.p=TRUE)
  p2 <- pnorm(- x %*% b,log.p=TRUE)
  return(sum(y*p+(1-y)*p2))
}


llmin <- function(b){
  p <- pnorm(x %*% b,log.p=TRUE)
  p2 <- pnorm(- x %*% b,log.p=TRUE)
  r <- -sum(y*p+(1-y)*p2)
  #attr(r, "gradient") <- gr(b)
  #attr(r, "hessian") <- h(b)
  return(r)
}

gr <- function(b){
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  return(as.vector(t(x) %*% (y*d/p + (1-y)*(-d)/(1-p))))
}

h <- function(b) {
  p <- pnorm(x %*% b)
  d <- dnorm(x %*% b)
  e <- matrix(numeric(25),nrow=5)
  for(i in 1:length(y)){
    if(y[i]==1){
      e <- e+ as.numeric(d[i]/p[i]*(d[i]/p[i]+x[i,] %*% b))*(x[i,]%*%t(x[i,]))
    } else{
      f <- -d[i]/(1-p[i])
      g <- x[i,] %*% b
      g<- f*(f+g)
      e <- e+as.numeric(g)*(x[i,]%*%t(x[i,]))
    }
  }
  return (-e)
}




library(maxLik)
system.time(result <- maxLik(ll,start=numeric(5),method="NR",grad=gr,hess=h,x=x,y=y))
summary(result)

system.time(result <- maxLik(ll,start=numeric(5),method="NR",x=x,y=y))
summary(result)

(result <- nlm(llmin,b,hessian=TRUE))
sqrt(diag(solve(result$hessian)))
      
(result <- nlm(llmin,b,hessian=TRUE,gradtol=1e-8,steptol=1e-8))
sqrt(diag(solve(result$hessian)))
            

bmatrix=matrix(ncol=5)
for(i in 1:100){
  ind = sample(1:58,replace=TRUE)
  x=matrix(ncol=5)
  y=vector()
  for (j in ind){
    st = (j-1)*99+1
    x = rbind(x,xo[st:(st+98),])
    y = c(y,yo[st:(st+98)])
  }
  x=x[-1,]
  bresults =  maxLik(ll,start=numeric(5),method="NR",x=x,y=y)
  bmatrix=rbind(bmatrix,bresults$estimate)
}
bmatrix=bmatrix[-1,]
sderror=vector()
for(i in 1:5){
  sderror[i] = sd(bmatrix[,i])
}
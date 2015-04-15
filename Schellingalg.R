# N blue individuals
# N red
# 2N total

# Initially everyone lives at random location in unit square (x,y) in [0,1]x[0,1] 
# Equilibrium occurs when everyone is "ok" with current location - "ok" means no one wants to move
# A person is "ok" if at least t of their 10 closest neighbors are on the same color (focusing on t<5 - i.e. don't want to be an extreme minority)
# If a person is not "ok", choose a new random location, Repeat this until this is ok.

# Algorithm
  # Repeat until no one moves
  # For each individual
    #check if ok -> calc distance beteen that individual and everyone else (calc (N-1 distances) then sort by distance) take 
    # 10 closest & count how many are of same color. Compare to t. 
    # while not ok
      # move to random location
      #check if ok
    
# Simulate 500x for N=100 and T=4
# Use index of dissimilarity to calc how integrated/segregated area is -> formula 1 is seg, 0 is int


set.seed(1000)
t <- 4
num <- 50
iter <- 500

calcindex <- function(x,y){
  even=integer(num*2)
  odd=integer(num*2)
  for(i in 1:(num*2)){
    even[i]=1-i%%2
    odd[i]=i%%2
  }
  
  q1e=sum(even*as.integer((x<0.5)&(y<0.5)))
  q2e=sum(even*as.integer((x>0.5)&(y<0.5)))
  q3e=sum(even*as.integer((x<0.5)&(y>0.5)))
  q4e=sum(even*as.integer((x>0.5)&(y>0.5)))
  
  q1o=sum(odd*as.integer((x<0.5)&(y<0.5)))
  q2o=sum(odd*as.integer((x>0.5)&(y<0.5)))
  q3o=sum(odd*as.integer((x<0.5)&(y>0.5)))
  q4o=sum(odd*as.integer((x>0.5)&(y>0.5)))
  
  r = 0.5*(abs(q1e/num-q1o/num)+abs(q2e/num-q2o/num)+abs(q3e/num-q3o/num)+abs(q4e/num-q4o/num))
}

# blue <- x
# red <- y

initial.index <- numeric(iter)
final.index <- numeric(iter)

for(j in 1:iter){
  
  x <- runif(2*num)
  y <- runif(2*num)
  
  initial.index[j] <- calcindex(x,y)
  
  distance <- numeric(2*num)
  
 repeat{ 
  count <- 0
  for(k in 1:(2*num)){
    OK <- FALSE
    while(OK == FALSE){
      count <- count+1
      for(l in 1:(2*num)){
     
        distance[l] <- sqrt(((x[k]-x[l])^2)+((y[k]-y[l])^2))
        
        for(m in 1:(num*2)){
            red[m]=1-m%%2
            blue[m]=m%%2
          }
        
        colors <- cbind(distance,red,blue)
        colors <- as.data.frame(colors)
        colnames(colors) <- c("distance","red","blue")
        
        colors <- colors[-k,]
        
      colors <- colors[order(colors$distance),]
    
      indicator <- sum(colors$red)
    
      neighbors <- colors[(1:10),]
      bn <- sum(neighbors$blue)
      rn <- sum(neighbors$red)
      
      
      if(indicator == 99 & rn<t){
        x[k] <- runif(1)
        y[k] <- runif(1)
      } else if(indicator == 100 &bn<t){
        x[k] <- runif(1)
        y[k] <- runif(1)
      } else {
        OK <- TRUE
      }
    
      }
    }
  }
  
    if(count == 0){
      break
    }
  }
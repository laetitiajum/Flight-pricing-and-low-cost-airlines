library(foreach)
library(iterators)
library(GA)
library(stargazer)


rm(list=ls())

comments<-"\n Both airlines are FSCs wih identical characteristics and different initial market share (0.6 vs 0.4) \n
Initial demand number 2 : More demand on week-ends than on weekdays"

##################################### Parameters
players=2

#Costs parameters
p0=1000 #IATA price
k=c(100000, 100000) #fixed costs
c=c(60,60) #variable costs

#Demand parameters
a=c(-2.3,-2.3) #elasticity of airfare ratio to market share

b=c(1.0, 1.0) #elasticity of frequency to market share
lambda=-2.5 #elasticity of average relative airfare to market size
M0=c(0.6, 0.4) #initial market shares

#Other parameters
s=c(200,200) #seats
f=c(7,7) #maximum flight frequency
gamma=c(0.95, 0.95) #share of people waiting from i to i+1 if no flight at i

#Model parameters
I=30 #number of intervals
chsize=I+10 #length of the chromosome (one per interval of time + 10 for the price)
popsize=100 #size of the initial population for the genetic algorithm
iter=80 #number of iterations during the genetic algorithm

games=100  #number of repeated games

#Function for conversion of bits to integer
BinToDec <- function(x) {
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
}

############################################## Initialization ###############################################################

#Initial demand, two functions were built
demand <- function() {
  d0=vector(mode="numeric", length=I)
  indice=1
  while(indice<I/2) {
    d0[indice+1]<-5*indice
    d0[indice]<-5*indice
    d0[I-(indice+1)]<-5*indice
    d0[I-(indice)]<-5*indice
    indice<-indice+2
  }
  if((I/2)%%1==0) {d0[I/2]<-d0[I/2-1]}
  d0[I]=d0[I-1]
  d0*1.
}

demand2 <- function() {
  d0=vector(mode="numeric", length=I)
  for(indice in 1:I) {
    d0[indice]<-30
  }
  d0[6]<-70
  d0[7]<-70
  d0[13]<-45
  d0[14]<-45
  d0[20]<-70
  d0[21]<-70
  d0[27]<-45
  d0[28]<-45
  d0
}

d0=vector(mode="numeric", length=I)
d0=demand2()

#Generation of chromosomes for each airlines, two functions were built
gen<-function(){
  out<-vector(mode="numeric", length=chsize)
  random<-sample(1:I, sample(3:f[1]), replace=F)
  for (j in random) out[j]<-1
  
  #Random initial price
  price<-intToBits(sample(500:1000, 1))
  for(i in 1:10) {
    var=as.integer(price[i])
    out[chsize+1-i]<-var
  }
  out
}


gen2<-function(price0){
  out<-vector(mode="numeric", length=chsize)
  random<-sample(1:I, sample(3:f[1]), replace=F)
  for (j in random) out[j]<-1
  
  #Given initial price
  price<-intToBits(price0)
  for(i in 1:10) {
    var=as.integer(price[i])
    out[chsize+1-i]<-var
  }
  out
}

#Generation of the first  chromosomes that will be airlines' answers for the first game
x<-matrix(ncol=chsize, nrow=players)
p<-vector(mode="numeric", length=players)

for(i in 1:players) {
  x[i,]<-gen()
  p[i]<-BinToDec(x[i,((I+1):chsize)])
}

##Where we will record the results
resultsprice=matrix(nrow=games, ncol=players)
resultsprofit=matrix(nrow=games, ncol=players)
resultscalendar=matrix(nrow=games*players, ncol=I)
resultsmarketshare=matrix(nrow=games, ncol=players)
finalresults<-matrix(nrow=players, ncol = 4)

############################## Generation of population for GA ##############################

#Function for generation of completely random population
genpop<-function(object){
  P<-matrix(nrow=object@popSize, ncol=object@nBits)
  for(j in 1:object@popSize) {
    P[j,]<-gen()
  }
  P
}

#We generate a population where the prices are uniformly in [500,999]
genpop2<-function(object){
  P<-matrix(nrow=object@popSize, ncol=object@nBits)
  vartemp<-sample(0:4, 1)
  for(j in 1:object@popSize) {
    P[j,]<-gen2(495+vartemp+5*j)
  }
  P
}

################################################################################################################################
#Fitness function (or profit function)
profit <- function(y, x2) {
  xtemp<-matrix(ncol=chsize, nrow=players)
  xtemp[]<-x2
  xtemp[airline,]<-y
  nflights<-sum(xtemp[airline, (1:I)])
  
  ptemp<-vector(mode="numeric", length=players)   #Prices
  vartemp=0
  vartemp2=0
  vartemp3=0
  for (j in 1:players) {
    ptemp[j]<-BinToDec(xtemp[j, ((I+1):chsize)])
    
    vartemp<-vartemp+M0[j]*(ptemp[j]/p0)^a[j]*(sum(xtemp[j,1:I]))^b[j]
    vartemp2<-vartemp2 + ptemp[j]*s[j]*sum(xtemp[j,1:I])
    vartemp3<-vartemp3 + s[j]*sum(xtemp[j,1:I])
  }
  
  #Market share
  Mtemp=(M0[airline]*((ptemp[airline]/p0)^a[airline])*(sum(xtemp[airline, (1:I)]))^b[airline])/vartemp
  dtemp=vector(mode="numeric", length=I)        #Total demand
  Dtemp=matrix(ncol=I, nrow=players)            #Demand for each airline
  for (i in 1:I){
    dtemp[i]<-d0[i]*((1/p0*vartemp2)/vartemp3)^lambda
    Dtemp[i]<-dtemp[i]*Mtemp
  }
  
  result<-(-k[airline]+(ptemp[airline]-c[airline])*min(Dtemp[1], s[airline]))*xtemp[airline,1]
  
  for (i in 2:I) {
    restdemand=vector(mode="numeric", length=I)
    for (j in 1:(i-1)) {
      vartemp = 1
      for (l in (i-j):(i-1)) {
        vartemp <- vartemp *(1-xtemp[airline,l])
      }
      if(vartemp==1) {
        restdemand[i] <- restdemand[i] + Dtemp[i-j]*(gamma[airline]^j)
      }
    }
    mintemp<-min(Dtemp[i]+restdemand[i],s[airline])
    result<-result+(-k[airline]+(ptemp[airline]-c[airline])*mintemp)*xtemp[airline,i]
  }
  result
}

##################################### Mutation function #####################################
# double crossover

dco <- function(object, par) {
  chsizetemp<-object@nBits
  parents=matrix(nrow=2, ncol=chsizetemp)
  parents[1,]<-object@population[par[1],]
  parents[2,]<-object@population[par[2],]
  pts <- sample(1:I, 2, replace=FALSE)
  if(pts[2]<pts[1]) {
    vtemp=pts[2]
    pts[2]<-pts[1]
    pts[1]<-vtemp
  }
  
  children=matrix(nrow=2, ncol=chsizetemp)
  
  children[1, 1:pts[1]]<- parents[1, 1:pts[1]]
  children[1, (pts[1]+1):pts[2]]<- parents[2, (pts[1]+1):pts[2]]
  children[1, pts[2]:chsizetemp] <- parents[1, pts[2]:chsizetemp]
  
  children[2, 1:pts[1]]<- parents[2, 1:pts[1]]
  children[2, (pts[1]+1):pts[2]]<- parents[1, (pts[1]+1):pts[2]]
  children[2, pts[2]:chsizetemp] <- parents[2, pts[2]:chsizetemp]
  
  #In one case out of two, each child gets the average price of the parents. In the other case, we keep the two prices, one for each child
  if(pts[1]%%2==0) {
    price<-intToBits((BinToDec(parents[1,((I+1):chsize)])+BinToDec(parents[2,((I+1):chsize)]))%/%2)
    for(i in 1:10) {
      var=as.integer(price[i])
      children[1, (chsize+1-i)]<-var
      children[2, (chsize+1-i)]<-var
    }
  }

  fitnessrecords <- c(profit(children[1,], x), profit(children[2,], x))
  out=list("children"=children, "fitness"=fitnessrecords)
}

#######################################################################################################
#######################################################################################################
start<-date()
GA=vector("list", length = players)
for (g in 1:games) {
  print(paste("game number",g))
  
  for (airline in 1:players) {
    if (g==1) {
      GA[[airline]]=ga(type="binary",
                       fitness=profit, x2=x, nBits = chsize, population=genpop2, popSize=popsize, pcrossover=0.8, pmutation=0.5, 
                       elitism = max(1, round(popsize * 0.6)),
                       crossover=dco,
                       maxiter=iter
      )
    } else {
      GA[[airline]]=ga(type="binary",
                       fitness=profit, x2=x, nBits = chsize, population=genpop2, popSize=popsize, pcrossover=0.8, pmutation=0.5, 
                       elitism = max(1, round(popsize * 0.6)),
                       crossover=dco,
                       suggestions = GA[[airline]]@solution,
                       maxiter=iter
      )
    }
    x[airline,]<-GA[[airline]]@solution[1,] #We keep the first solution only

    print(paste("Airline number", airline))
    print(GA[[airline]]@solution)
    
    resultsprice[g,airline]<-BinToDec(x[airline,((I+1):chsize)])
    resultsprofit[g, airline]<-GA[[airline]]@fitnessValue
    
    resultscalendar[(players*g-(players-airline)),]<-x[airline,1:I]
  }
  
  vartemp=0
  for (i in 1:players) {
    vartemp<-vartemp+M0[i]*(resultsprice[g,i]/p0)^a[i]*(sum(x[i,(1:I)]))^b[i]
  }
  for (i in 1:players) {
    resultsmarketshare[g, i]<-(M0[i]*((resultsprice[g,i]/p0)^a[i])*(sum(x[i,(1:I)]))^b[i])/vartemp
  }
}
end<-date()

##################################### Results treatment #####################################
#We record the last results in a matrix
for (i in 1:players) {
  cal<-vector("list")
  index=1
    for(j in 1:I) {
      if(resultscalendar[(2*(games-1)+i), j]==1) {
        cal[[index]]<-j
        index<-index+1
      }
    }
  
  finalresults[i,1]<-resultsprice[games,i]
  finalresults[i,2]<-resultsprofit[games,i]
  finalresults[i,3]<-resultsmarketshare[games,i]
  finalresults[i, 4]<-paste(cal, collapse = ", ")
}

#Display results
resultsprice
resultsmarketshare
resultsprofit
resultscalendar[((games-1)*players+1):(players*games),]

plot(resultsprice[,1], type="l", ylim=c(500,1000), main="Prices evolution during the repeated games", ylab="Price", xlab="Games")
for(i in 2:players) {
  lines(resultsprice[,i],col=4*i)
} 

plot(resultsprofit[,1], type="l", ylim=c(0,200000), main="Profit evolution during the repeated games", ylab="Price", xlab="Games")
for(i in 2:players) {
  lines(resultsprofit[,i],col=4*i)
} 

#We save the results in a txt document
recordresults <- function() {
  today<-format(Sys.time(), format="%Y%m%d %H%M%S")
  namefile<-paste("C:\\Users\\35982\\Documents\\Research project\\GA\\Results\\", today, ".txt", sep="")
  
  write(paste(start, end, sep= ", "), file=namefile)
  write(comments, file=namefile, append=TRUE)
  write("\n Evolution of prices with games", file=namefile, append = TRUE)
  write.table(resultsprice, file=namefile, sep = "\t", col.names = FALSE, append = TRUE)
  
  write("\n Evolution of profit with games", file=namefile, append = TRUE)
  write.table(resultsprofit, file=namefile, sep = "\t", col.names = FALSE, append = TRUE)
  
  write("\n Evolution of market shares with games", file=namefile, append = TRUE)
  write.table(resultsmarketshare, file=namefile, sep = "\t", col.names = FALSE, append = TRUE)
  
  write("\n Evolution of scheduled calendar with games", file=namefile, append = TRUE)
  write.table(resultscalendar, file=namefile, sep = "\t", col.names = TRUE, append = TRUE)

  write(stargazer(finalresults), file=namefile, append=TRUE)
} 

#recordresults()

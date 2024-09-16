  
  
#install.packages("rJava")
  rm(list=ls(all=TRUE))
  .libPaths( c( .libPaths(), "C:/Users/keneilwe.manda/Downloads/Master Research/R/win-library/3.5") )
  
  set.seed(123)
  library(nsga2R)
  library(FRAPO)
  library(xts)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(assertthat)
  library(nnet)
  library(OpenMx)
  library(EnvStats)
  #library(covmat)
  library(xlsx)
  
  print(getwd()) # Where does the code think it is?
  setwd('C:/Users/keneilwe.manda/Downloads') # Where do I want my code to be?
  StockData<-read.table(file='C:/Users/keneilwe.manda/Downloads/MSc_Research_Testing/data.txt.txt',header=TRUE);
  #StockData<-read.table(file="C:/Users/a0055987/Dropbox/Temp read/Complexity_progs/data.txt",header=TRUE);
  StockData=data.frame(StockData[1:80])
  ncol=ncol(StockData)                           #Number of stocks
  popSize=300;                   
  generations=500; 
  interest_rate=0.007
  
  thresh=0.02                                   #Threshhold below which weight is set to 0 i.e. the stock is not picked
  evalFunc1 <- function(y) {
    x9=seq(1,ncol)                               #Generate a sequnce of 1-80 representing stock position in StockData 
    #print(y)
    y[y<(1+thresh)]<-0                           #random number is from 1 to 1.9 if 1+thresh< 0, stock not picked so no weight, if 1+thresh>0 stock is picked and the weight is the number after the decimal (i.e.of 1.something)
    
    
    xx=floor(as.matrix(y))                       #stock picking indicator, floor gets rid of the decimals
    
    x=t(xx*(x9))                                 # x=position of stock if stock is picked, 
    
    xy<-y-1                                      #(y-1) calculate weight from y values. For a stock that is picked this number will be >1. For a stock that is not picked it will be <0 i.e -1.something
    w<-xy[xy>0]                                  #Calculate a weight vector as those with xy>0, get rid of weights for stocks that were not picked <0. This step may be redundant because of line y[y<(1+thresh)]<-0
    
    weights<-as.matrix(w/sum(w))                 #rebalance weights to sum to 1 and turn it into a matrix
    
    
    x<-x[x>0]                                    #Get rid of stocks that were not picked i.e value is zero                            
    
    if (length(x)<2) {                            #Check if there are at least 2 stocks in the portfolio. If not set mean and var to 1000000
      var=Inf                                
      mean=Inf
    }
    else{                                         #else continew with the calculation
      
      Mat1=StockData[,x]                           #Mat1 is the matrix of stocks that were picked. Here we are using position x calculated earlier
      Mat=as.matrix(Mat1)                          #Turn Mat1 to matrix Mat1
      
      assert_that(ncol(Mat)==nrow(weights))
      
      mean<-sum(colMeans(Mat)*weights)*-1          #Calculate the expected return of portfolio. Multiply by -1 to maximize since NSGA2 minimize by default
      
      
      var=(t(weights)%*%(cov(Mat1))%*%(weights))^(0.5)     #Calculate the variance of the portfolio
      
    }
    
    return(c(var,mean))
    
  }
  
  #set.seed(403)
  resultsMV<-nsga2R(fn=evalFunc1, varNo=ncol, objDim=2, lowerBounds=rep(1,ncol), upperBounds=rep(1.9,ncol),
                    popSize=popSize, tourSize=4, generations=generations, cprob=0.02, XoverDistIdx=5, mprob=0.2,MuDistIdx=5)
  
  
  Big=((as.matrix(resultsMV$parameters)))                   #Get result of weights
  Big1=((as.matrix(resultsMV$parameters)))                  #place holder1
  Big2=((as.matrix(resultsMV$parameters)))                  #place holder2
  Big1[Big<(1+thresh)]<-0                                   #Get rid of weights that are below the threshold
  Bigfloor=floor(Big1)                                      #floor removes the decimal,yeild  stocks picked
  Big2[Big2<(1+thresh)]<-1                                 #Set stocks that were not picked equal to 1 so that by substracting 1 from them they will be zero while others will be the weights of the stock picked
  
  PortfolioSizeVAR<-apply((Bigfloor), 1, sum)               #rowsum indicator of stock picked to get nuber of stocks per portfolio on the efficient frontier
  PortfolioweightVAR<-Big2 - 1                              #Get the weight of stocks in each portfolio on the frontier
  
  rebal<-apply(PortfolioweightVAR,1,sum)                    #Calculate rowsum of weight to be used to rebalance the weights
  SumRebalWeightsMV<-apply(PortfolioweightVAR,1,sum)/rebal          #Rebalanced weights should be all 1
  rebalancedNSG2Weights<-PortfolioweightVAR/rebal
  PortfolioSizeVAR  
  
  VAR_NSGA2<-abs(resultsMV$objectives)[,1]
  returns_NSGA2<-abs(resultsMV$objectives)[,2]
  sharp_NSGA2<- (returns_NSGA2-interest_rate)/(VAR_NSGA2)     #^0.5
  max_sharpNAG2<-which.is.max(sharp_NSGA2)
  MVP_NAGA2<-which.min(VAR_NSGA2)
  max_sharpNAG2
  MVP_NAGA2
  
  plot(VAR_NSGA2,returns_NSGA2)
  positionNSGA2=rep("",popSize)
  positionNSGA2[max_sharpNAG2]<-"OPTIMAL"
  positionNSGA2[MVP_NAGA2]<-"MVP"
  
  MVP_NSGA2_W<-rebalancedNSG2Weights[MVP_NAGA2,]
  max_sharpNAG2_W<-rebalancedNSG2Weights[max_sharpNAG2,]
  
  dat <- data.frame(x=VAR_NSGA2, y=returns_NSGA2, ID=positionNSGA2)
  pp<-ggplot(dat, aes(x= x, y= y)) +geom_text(aes(label=ID), fontface=1, colour="red") + geom_point() #+ geom_smooth(method = "loess")
  pp + labs(title = "NSGA2 MV Frontier", x = "SD", y = "Returns")
  
  ############################
  #QP
  ####################################
  r=data.frame(StockData[1:80])
  return_Data<-data.matrix(r, rownames.force = NA)
  
  library(quadprog)
  library(calibrate)
  
  er<-return_Data
  cov.mat<-cov(er)
  
  efficientFrontier <- function(mean,vcov,aver) {
    
    aversion=aver     #Risk aversion parameter
    n_asset <- ncol(mean)
    Dmat <- vcov
    dvec <- colMeans(mean)*aversion   #penalized returns
    #bvec <- c(1, rep(0, nrow(Dmat))) 
    meq <- 1
    
    bvec <- c(1, rep(0,n_asset))
    Amat <- cbind(1, diag(nrow(Dmat)))
    
    #bvec <- c(1, rep(0, nrow(Dmat)), rep(-0.2, nrow(Dmat)))  #the one with constraints
    #Amat <- cbind(1, diag(nrow(Dmat)), -1*diag(nrow(Dmat)))
    
    sol<-solve.QP(Dmat, dvec,Amat, bvec=bvec, meq=meq)
    w<-sol$solution
    return(w)
  }
  
  #wmatrix<-matrix(0,nrow=281,ncol=80)
  #mean_vector<-matrix(0,nrow=1, ncol=281)
  #var_vector<-matrix(0,nrow=1, ncol=281)
  #sharp_vector<-matrix(0,nrow=1, ncol=281)
  #comp_vector<-matrix(0,nrow=1, ncol=281)
  #num_stocks<-matrix(0,nrow=1, ncol=281)
  
  wmatrix<-matrix(0,nrow=300,ncol=80)
  mean_vector<-rep(0,300)
  var_vector<-rep(0,300)
  sharp_vector<-rep(03001)
  comp_vector<-rep(0,300)
  num_stocks<-rep(0,300)
  
  
  loop=1
  for(risk in seq(0.00, 2.99, 0.01))
  {
    
    
    w<-efficientFrontier(mean=er,vcov=cov.mat,aver=risk)
    wmatrix[loop,]=round(w,4)
    mean_vector[loop]=sum(round(w,4)*colMeans(er))
    var_vector[loop]=(t(round(w,4))%*%cov.mat%*%round(w,4))^(0.5)
    sharp_vector[loop]=(mean_vector[loop]-interest_rate)/var_vector[loop]     #sharpe Ratio
    num_stocks[loop]<-length(which(wmatrix[loop,]!=0))              #number of stocks
    
    
    #wei1<-w
    #wei1<-wei1[wei1>(0)]
    #wei1_id<-w
    #wei1_id<-replace(wei1_id, wei1_id>0, 1) #replace weight=1 if weight>0 otherwise weight==0
    #Matt1<-StockData[,wei1_id]
    #wei1<-as.matrix(wei1)
    #Matt1<-as.matrix(Matt1)
    #weightedReturns<-sweep(Mat,MARGIN=2,w,`*`)
    #d1<-eigen(weightedReturns)
    #comp_vector[loop]<-(ncol(weightedReturns)/2)*mean(d1$values)/(geoMean(d1$values))
    
    loop=loop+1
    print(risk)
  }
  
  num_stocks
  
  max_sharpMV<-which.is.max(sharp_vector)
  MVP_MV<-which.min(var_vector)
  max_sharpMV
  MVP_MV
  
  plot(var_vector,mean_vector)
  positionMV=(rep("",300))
  positionMV[max_sharpMV]<-"OPTIMAL"
  positionMV[MVP_MV]<-"MVP"
  
  MVP_MV_W<-wmatrix[MVP_MV,]
  max_sharpMV_W<-wmatrix[max_sharpMV,]
  
  dat <- data.frame(x=var_vector, y=mean_vector, ID=positionMV)
  pp<-ggplot(dat, aes(x= x, y= y)) +geom_text(aes(label=ID), fontface=1, colour="red") + geom_point() #+ geom_smooth(method = "loess")
  pp + labs(title = "QP MV Frontier", x = "SD", y = "Returns")
  
  
  dat <- data.frame(x=var_vector, y=mean_vector, ID=positionMV)
  dat1 <- data.frame(x=VAR_NSGA2, y=returns_NSGA2, ID=positionNSGA2)
  
  
  pf<-ggplot(dat) +
    geom_point(data=dat, aes(x=var_vector, y=mean_vector), colour="red") + geom_text(x=var_vector, y=mean_vector, aes(label=positionMV),colour = 'red', position = "identity") +
    geom_point(data = dat1, aes(x=VAR_NSGA2, y=returns_NSGA2), colour = 'blue', size = 1) + geom_text(x=VAR_NSGA2, y=returns_NSGA2, aes(label=positionNSGA2), colour = 'blue',position = "identity")
  
  pf + labs (x = "Standard Deviation", y = "Expected Returns")    # + labs(title = "QP (red) and  NSGAII (blue) MV Efficient Frontier", x = "Standard Deviation", y = "Expected Returns")

stocks<-seq(80)
MVP_weights<-rbind(MVP_MV_W,MVP_NSGA2_W)
mp <- barplot(MVP_weights, beside = TRUE,
               col=c("red","blue"), names.arg = stocks, main="Weight of MVP QP (red) and  NSGAII (blue)", xlab="stocks", ylab="weights")

OP_weights<-rbind(max_sharpMV_W,max_sharpNAG2_W)
mp <- barplot(OP_weights, beside = TRUE,
              col=c("red","blue"), names.arg = stocks, main="Weight of OP QP (red) and  NSGAII (blue)", xlab="stocks", ylab="weights")

stocks<-seq(80)
MVP_weights<-rbind(MVP_MV_W,MVP_NSGA2_W)
mp <- barplot(MVP_weights, beside = TRUE,
              col=c("red","blue"), names.arg = stocks, xlab="stocks", ylab="weights")

OP_weights<-rbind(max_sharpMV_W,max_sharpNAG2_W)
mp <- barplot(OP_weights, beside = TRUE,
              col=c("red","blue"), names.arg = stocks,  xlab="stocks", ylab="weights")

####OP Returns
returns_NSGA2[max_sharpNAG2]     #NSGAII optimal expected returns
mean_vector[max_sharpMV]        #QP optimal expected returns

VAR_NSGA2[max_sharpNAG2]         #NSGAII optimal Standard deviation
var_vector[max_sharpMV]          #QP optimal Standard deviation

sharp_vector[max_sharpMV]        #QP Sharpe ratio
sharp_NSGA2[max_sharpNAG2]       #NSGAII Sharpe ration

##Minimum risk
returns_NSGA2[MVP_NAGA2]     #NSGAII MVP expected returns
mean_vector[MVP_MV]        #QP MVP expected returns

VAR_NSGA2[MVP_NAGA2]         #NSGAII MVP Standard deviation
var_vector[MVP_MV]          #QP MVP Standard deviation

sharp_vector[MVP_MV]        #QP MVP Sharpe ratio
sharp_NSGA2[MVP_NAGA2]       #NSGAII MVP Sharpe ration

######################################
#Portfolio returns

MVP_QP_returns<-return_Data %*% MVP_MV_W              #Minimum variance portfolio QP returns
MVP_NSGA2_returns <- return_Data %*% MVP_NSGA2_W      #Minimum variance portfolio NSGA2 returns
OP_QP_returns<- return_Data %*% max_sharpMV_W         #Optimal portfolio QP returns
OP_NSGA2_returns<- return_Data %*% max_sharpNAG2_W    #Optimal portfolio NSGA2 return

Port_Returns<- cbind(MVP_QP_returns,MVP_NSGA2_returns,OP_QP_returns,OP_NSGA2_returns)
colnames(Port_Returns)<- c("MVP_QP_returns","MVP_NSGA2_returns","OP_QP_returns","OP_NSGA2_returns")

write.xlsx(Port_Returns, file="Master Research read/Complexity_progs/data.txt read/Complexity_Second/Code1/Returns.xlsx", 
           sheetName = "MV_Returns", col.names = TRUE, row.names = TRUE, append = TRUE)

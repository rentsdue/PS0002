##Lab tasks 
getwd()
##set your own working directory
setwd("C:/Users/lmxiang/Documents/Teaching/Analytics2011&DataScienceAI_PS0002/PS0002-Intro Data Science/Lab1")
################################################
##Task 1. 
################################################
##First, download "lab1test.txt","lab1fixed.txt" to the working directory
##a: Read data from an external file with fixed format
varnames<-c("id","gender","height","weight","siblings")
lab1<-read.fwf("lab1fixed.txt",header=F,col.names=varnames,width=c(3,1,3,2,1))
plot(lab1$height,lab1$weight)

##b: Read data from an external file with free format
lab1m<-lab1[lab1[,2]=="M",]
length(lab1m[,1]) #or use nrow(lab1m)
##c:
lab1test<-read.table("lab1test.txt",header=T)
lab1merge<-merge(lab1, lab1test, by="id")
#You may check how it is different from using cbind(lab1,lab1test)

attach(lab1merge)
lab1merge[height>182,]
lab1merge[height>182,6] #test scores of subjects with height>182

##d:
lab1remo<-lab1[id<211|id>211,] # same as lab1[id!=211,]
#Or use the command below if you know the row number (11) corresponding to id=211
#lab1[-11,] 

##e:
lab1new<-lab1
lab1new[id==211,4]#View the original value
lab1new[id==211,4]=80 #replace the original value by 80
lab1new

##f:
lab1f<-lab1merge[lab1merge[,2]=="F",]
lab1sort<-lab1f[rev(order(lab1f[,3])),]
lab1sort[2,c(3,4,6)]
# answer:   height weight test
#       73    174     64   57

################################################
##Task 2
################################################
x<-c(rep(1,4), seq(1,7,2))
y<-c(4,6,13,20)
X<-matrix(x,nrow=4)
hatb<-solve(t(X)%*%X)%*%t(X)%*%y
hatb

################################################
##Task 3: Function for computing 2 moments
################################################
first2mom<-function(x) 
{ m<-numeric(2)
m[1]<-mean(x)
m[2]<-sum((x-m[1])^2)/length(x) #or simply use: mean((x-m[1])^2)
m 
} #end of function
##For example: apply it to variable height in dataframe lab1
first2mom(lab1$height)
#answer:  165.86667    75.98222   

################################################
##Task 4: Bisection method to solve an equation
################################################
f1<-function(x) {(-2)*x^2-5*x+7}
xmin<- -4;xmax<-0
xmid<-(xmin+xmax)/2
while(abs(f1(xmid))>1e-6)
{ if(f1(xmin)*f1(xmid)<0) {xmax<-xmid}
  else {xmin<-xmid}
 xmid<-(xmin+xmax)/2
cat("xmin=",xmin,"xmax=",xmax, "xmid=",xmid, "\n")
}
cat("root=",xmid,"\n")
#answer: xmin= -4 xmax= -2 xmid= -3 
#        xmin= -4 xmax= -3 xmid= -3.5 
#	   root= -3.5 

#Or Write bisection method as a function
findroot=function(xmin,xmax, f){
  xmid=(xmin+xmax)/2
  while (abs(f(xmid))>1e-6){
    if (f(xmid)*f(xmin)<0){xmax=xmid}
    else {xmin=xmid}
    cat(xmin,",",xmax,",",xmid,"\n")
    xmid=(xmin+xmax)/2
 }
  xmid
}
f1=function(x) {(-2)*x^2-5*x+7}
findroot(-4,0,f1) #find a root within interval [-4, 0].
findroot(-2,3,f1) #find a root within interval [-2, 3].




 

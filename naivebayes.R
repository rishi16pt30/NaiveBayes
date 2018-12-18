data=read.csv('C:/Users/v3575g/Desktop/balance.csv')
tot_field=ncol(data)
t=sample(2,nrow(data),replace=TRUE,prob=c(0.67,0.33))
traindata=data[t==1,]
testdata=data[t==2,]
fea=c()
for(i in 1:ncol(traindata)){
  fea=c(fea,length(unique(traindata[,i])))
  
}
a=matrix(0,nrow=5,ncol=6)
for (i in 2:length(fea)){
  tempmat=matrix(0,nrow=fea[i],ncol=2*fea[1])
  a=abind(a,tempmat,along=3)
}
r=0
for (column in 2:ncol(traindata)){
  r=r+1
  templist=traindata[,column]
  var=unique(traindata[,column])
  clas=traindata[,1]
  for(i in var){
    lbrcount=c(0,0,0)
    for(j in 1:nrow(traindata)){
      if(templist[j]==i){
        if(clas[j]=='L')
          lbrcount[1]=lbrcount[1]+1
        if(clas[j]=='B')
          lbrcount[2]=lbrcount[2]+1
        if(clas[j]=='R')
          lbrcount[3]=lbrcount[3]+1
      }
    }
    for(h in 1:6){
      if(h<4){
        a[i,h,r]=lbrcount[h]
      }
      else{
        a[i,h,r]=lbrcount[h-3]/sum(lbrcount)
      }
    }
  }
}
p=as.data.frame(table(traindata[,1]))
probmat=c()
for(i in 1:(nrow(p))){
  u=p[i,2]/sum(p[,2])
  probmat=c(probmat,u)
}
#testing part
mainlis=c()
corcount=0
for(i in 1:nrow(testdata)){
  temp=testdata[i,]
  g=c(1,1,1)
  for(j in 2:length(temp)){
    g[1]=g[1]*a[temp[1,j],4,j-1]*probmat[2]
    g[2]=g[2]*a[temp[1,j],5,j-1]*probmat[1]
    g[3]=g[3]*a[temp[1,j],6,j-1]*probmat[3]
  }
  b=which(g==max(g))
  if(b==1)
    b='L'
  if(b==2)
    b='B'
  if(b==3)
    b='R'
  mainlis=c(mainlis,b)
  if(b==as.character(temp[1,1]))
    corcount=corcount+1}
accuracy=(corcount/nrow(testdata))*100
print(accuracy)


#ROC curve analysis
mt=matrix(nrow = 0,ncol=3)
f=c()
theval=seq(0,1,length.out = 20)
for(i in 1:nrow(testdata)){
  temp=testdata[i,]
  g=c(1,1,1)
  for(j in 2:length(temp)){
    g[1]=g[1]*a[temp[1,j],4,j-1]*probmat[2]
    g[2]=g[2]*a[temp[1,j],5,j-1]*probmat[1]
    g[3]=g[3]*a[temp[1,j],6,j-1]*probmat[3]
  }
  for(k in 1:3){
    f[k]=g[k]/sum(g)
  }
  mt=rbind(mt,f)
}
llis=c()
for(j in 1:nrow(mt)){
  if((which(mt[j,]==max(mt[j,])))==1){
    llis=c(llis,"L")
  }
  else{
    llis=c(llis,0)
  }
}
x=c()
y=c()
for(i in theval){
  x1=0
  y1=0
  conmat=matrix(0,nrow=2,ncol=2)
  ls=c()
  for(j in 1:nrow(mt)){
     k=mt[j,]
    if(k[1]>i)
       ls=c(ls,"L")
    else
       ls=c(ls,0)
  }
  for(k in 1:nrow(mt)){
    if(llis[k]=="L" && ls[k]=="L")
      conmat[1,1]=conmat[1,1]+1
    if(llis[k]=="L" && ls[k]==0)
      conmat[1,2]=conmat[1,2]+1
    if(llis[k]==0 && ls[k]=="L")
      conmat[2,1]=conmat[2,1]+1
    if(llis[k]==0 && ls[k]==0)
      conmat[2,2]=conmat[2,2]+1
  }
  x1=conmat[1,1]/(conmat[1,1]+conmat[1,2])
  y1=conmat[2,2]/(conmat[2,1]+conmat[2,2])
  x=c(x,1-x1)
  y=c(y,y1)
}
print(accuracy)
getwd()
setwd("D:/非结构化数据挖掘/期末作业--边聚类算法的社区发现")
data_62<-read.csv("data1.csv",header = F) # numbering from 0 to 61
data_62<-data_62+1 # adjust numbering from 1 to 62
data_20<-read.csv("data2.csv",header = F)

library("igraph")
library("Matrix")
# Convert data to adjacency matrix
# library(igraph)
# E_c2<-graph.data.frame(as.matrix(data))
# E_c<-get.adjacency(E_c2,sparse=FALSE)
Turm<-function(data_62)
{
  mtx_62<-matrix(0,nrow = max(data_62),ncol = max(data_62))
  for (i in 1:dim(data_62)[1])
  {
    mtx_62[data_62[i,1],data_62[i,2]]<-1
    mtx_62[data_62[i,2],data_62[i,1]]<-1
  }
  colnames(mtx_62)<-c(1:max(data_62))
  rownames(mtx_62)<-c(1:max(data_62))
  return(mtx_62)
}

# Calculate the degree of the matrix
fun.getDegree = function(myMatrix){
  degree=Matrix(data=NA, nrow=dim(myMatrix)[1], ncol=1)
  for(i in 1:dim(myMatrix)[1]){
    degree[i,1]=sum(myMatrix[,i])
  }
  return (degree)
}

#Calculate the number of triangles
fun.getZ_3 = function(myMatrix)
{
  matrixEdge3=Matrix(data=NA, nrow=dim(myMatrix)[1], ncol=dim(myMatrix)[1])
  for(i in 1:dim(myMatrix)[1]){
    for(j in 1:dim(myMatrix)[1]){
      matrixEdge3[i,j]=0
      if(i==j){
        next
      }
      if(myMatrix[i,j]==1){
        for(mm in 1:dim(myMatrix)[1]){
          
          if(myMatrix[i,mm]==1 && myMatrix[j,mm]==1 ){
            matrixEdge3[i,j]=matrixEdge3[i,j]+1
          }
          
          
        }
      }
      else{
        matrixEdge3[i,j]=0
      }
      
      
    }
  }
  return (matrixEdge3)
}


#Calculate the clustering coefficient values ​​of all remaining edges in the network
fun.getCij=function(matrixEdge3,degree)
{
  matrixCij=Matrix(data=NA, nrow=dim(matrixEdge3)[1], ncol=dim(matrixEdge3)[1])
  for(i in 1:dim(matrixEdge3)[1]){
    for(j in 1 : dim(matrixEdge3)[1] ){
      if(matrixEdge3[i,j]>0){
        
        aa=min(degree[i]-1,degree[j]-1)
        matrixCij[i,j]=matrixEdge3[i,j]/aa
      }
      else{
        matrixCij[i,j]=0
      }
    }
  }
  
  return (matrixCij)
}

#Compute community division module
fun.commun=function(myMatrix1){
  N=length(myMatrix1[1,]);
  flag=rep(-1,N);lis=rep(-1,N);
  c1=1;lis[1]=1;flag[1]=c1;f=1;e=1;
  while(e<(N+1))
  {
    if(f<=e)
    {
      for(i in 1:N)
      { 
        if(myMatrix1[lis[f],i]==1 & flag[i]==-1)
        {
          flag[i]=c1;e=e+1;lis[e]=i;
        }
      } 
      f=f+1;
    }else
    { 
      m=0
      for(i in 1:N){
        if(flag[i]==-1){
          m=i;break
        } 
      }
      if(m!=0)
      {
        e=e+1;lis[e]=i;
        c1=c1+1;flag[m]=c1;
      }else
        break;
    }
  }
  return(flag);
}


# Delete the edge (minimum clustering coefficient value) and change the adjacency matrix accordingly
fun.deleteEdge=function(myMatrix1,matrixCij){
  mincij=10000000
  for(i in 1:dim(myMatrix1)[1]){
    for(j in 1:dim(myMatrix1)[1]){
      if(myMatrix1[i,j]==1 && matrixCij[i,j]<mincij ){
        
        mincij=matrixCij[i,j]
      }
    }
  }
  flag=0
  for(i in 1:dim(myMatrix1)[1]){
    if(flag==1){
      break
    }
    for(j in 1:dim(myMatrix1)[1]){
      if(matrixCij[i,j]==mincij & myMatrix1[i,j]==1 ){
        myMatrix1[i,j]=myMatrix1[j,i]=0
        print(i)
        print(j)
        flag=1
        break
      }
    }
  }
  print('aa')
  return (myMatrix1)
}

fun_Q <- function(myMatrix1,flag){
  eij <- matrix(0,nrow = max(flag),ncol = max(flag) )
  N <- dim(myMatrix1)[1]
  all_line <- sum(myMatrix1)/2
  for (i in 1:N ){
    for (j in 1:N) {
      if(myMatrix1[i,j]!=0)
        eij[flag[i],flag[j]]=eij[flag[i],flag[j]]+1
    }
  }
  eij<-0.5*eij
  # remove duplicates
  
  if(max(flag)==1){
    eij <- eij
  }
  else
    eij <-eij*(1/all_line)
  
  ji <- sum(diag(eij))
  Esquare <- sum(eij%*%eij)
  Qvalue <- ji-Esquare
  return(Qvalue)
}


select_main<-function(myMatrix,mode ="fitness",result =list(),res = c(),t=1)
{
  myMatrix_first=as.matrix(myMatrix)
  myMatrix=as.matrix(myMatrix)
  
  # display
  g1=graph.adjacency( myMatrix,mode="undirected")
  plot(g1)
  
  while(sum(myMatrix)>0)
  {
    degree=fun.getDegree(myMatrix)#计算度
    # Calculate the clustering coefficient
    matrixEdge3=fun.getZ_3 (myMatrix) #Calculate the number of triangles
    matrixCij=fun.getCij(matrixEdge3,degree)#calculate cij
    
    myMatrix=fun.deleteEdge(myMatrix,matrixCij)
    
    flag=fun.commun(myMatrix)
    
    if(mode == "fitness"){
      mea=fun.fitness(myMatrix_first,flag) #fitness
    }
    else{
      mea<-fun_Q(myMatrix_first,flag) #Q
    }
    aa=flag
    aa[length(flag)+1]=mea
    result[[t]]=aa
    t=t+1
    res<-c(res,mea)
  }
  res
  max(res[which(res<1)])
  result[which(res==max(res[which(res<1)]))[1]]
}


mtx_62<-Turm(data_62) # Adjacency matrix
mtx_20<-Turm(data_20)
#select_main(mtx_20,mode = "fitness")
select_main(mtx_20,mode = "Q")
#select_main(mtx_62,mode = "fitness")
#select_main(mtx_62,mode = "Q")

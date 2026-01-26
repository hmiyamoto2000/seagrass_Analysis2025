# BayesLiNGAM for seagrass

getwd()
library('fastICA')
source('bayeslingam/main/loud.R')
loud()

df <- read.csv("input_file.csv",header=T,sep=",")


#Seagrass
x <- df$Seagrass
y <- df$B_Desulfobulbaceae
z <- df$B_bacterium_enrichment_culture_clone_22_2013
w <- df$B_Defluviitaleaceae
w1 <- df$B_Hyphomonadaceae
w2 <- df$E_Corallinophycidae
w3 <- df$B_uncultured_Aminicenantes_bacterium
w4 <- df$E_Diatomea
w5 <- df$E_Intramacronucleata
d <- data.frame(x1=x, x2=y,x3=z,x4=w, x5=w1, x6=w2, x7=w3, x8=w4, x9=w5)
result <- greedybayeslingam(d,model='GL')


sink('out_put.txt', append = TRUE)
print (result)
sink()

#グラフの作成
library(igraph)
par("mar"=c(1,1,1,1)) #こちらを入れて余白をとる
library(igraph)
par(mfrow=c(2,3))
prob <- round(result$prob,digits=4) * 100
n.node <- max(result$components$node)
case <- nrow(result$DAGs)
node <- result$DAGs[,1:n.node]
res <- as.matrix(result$DAGs[,-c(1:n.node)],nrow=case)
name <-paste("X",1:n.node,sep="")

for(i in order(prob,decreasing=T)[1:6]){
  amat <- matrix(0,n.node,n.node)
  index <- node[i,]
  amat[lower.tri(amat,diag=FALSE)] <- res[i,]
  amat <- t(amat[index,index])
  g <- graph.adjacency(amat,mode="directed")
  E(g)$label <- NA
  pos <- layout.circle(g)
  rot <- matrix(c(0,1,-1,0),nrow=2,byrow=T)
  la <- pos %*% rot
  if(n.node == 2)la <- matrix(c(-1,0.5,1,0.5),,nrow=2,byrow=T)
  plot(g,layout=la,edge.arrow.size = 0.5,vertex.size=30,vertex.color = "yellow",vertex.label=name)
  mtext(paste(prob[i],"%"),line=0)
}




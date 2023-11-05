# Install the required Packages 
install.packages("hash")
install.packages("stringr")
install.packages("igraph")
install.packages("powerplus")
library(hash)
library(stringr)
library(igraph)
library(powerplus)

# Using hash to generate lookup table
hash_map=hash()
freq=hash()

# Initializing a dataframe having start node, end node and their edge weights
df=data.frame(matrix(ncol = 3, nrow = 0))
colnames(df)=c("start","end","edge_weight")

# Defining a function named Construct_weightedGraph
Construct_weightedGraph = function(data){
for (sent in data){
words = str_split(sent," ")[[1]]
for (i in 1:(length(words)-1)){
pair = paste(words[i],words[i+1])

# Calculating frequency of occurrence of start node
if(has.key(words[i],freq))
{
freq[words[i]] = values(freq,words[i])+1
}
else
{
freq[words[i]] = 1
}

# Calculating frequency of occurrence of start and end nodes together
if(has.key(pair,hash_map))
{
hash_map[pair] = values(hash_map,pair)+1
}
else
{
hash_map[pair] = 1
}
}
}
store_keys=keys(hash_map)
len = length(store_keys)
for(i in 1:len){
edge = str_split(store_keys[i]," ")[[1]]
weight = values(hash_map,store_keys[i])[[1]]/values(freq,edge[1])[[1]]
df[nrow(df)+1,]=c(edge[[1]],edge[[2]],as.numeric(weight))
}
return(df)
}

# Reading data from the text file 
data = readLines(file.choose())
list=Construct_weightedGraph(data)
list[,"edge_weight"]= sapply(list[, "edge_weight"], as.numeric)

# Creating a graph from the dataframe
g=graph.data.frame(list[c("start","end")],directed=TRUE)
E(g)$weight=list$edge_weight
plot(g,vertex.size=125,vertex.label.cex=.7,edge.label.cex=.7,edge.label=round(E(g)$weight,3),edge.arrow.size=.3,rescale = FALSE, xlim=c(-20,20))
adj_mat=as_adjacency_matrix(g,attr= "weight")
transition=as.matrix(adj_mat)
words=rownames(transition)

# Initializing state matrix with zeroes
state=matrix(0,ncol=dim(transition)[1])
state=state%*%transition

# Generating next occurrence using Markov Chains
print("Input the word:")
input = readline()
state[1,input]=1
trans_updated=state%*%transition
index = which(trans_updated==max(trans_updated))
sprintf("Next possible word is: %s",words[index])
sprintf("Probability of Occurrence is: %f", max(trans_updated))

# Generating possible 2nd word from the start word using Markov Chains
state=matrix(0,ncol=dim(transition)[1])
state=state%*%transition
print("Input the word:")
input = readline()
state[1,input]=1
trans_updated=state%*%Matpow(transition,2)
index = which(trans_updated==max(trans_updated))
sprintf("2nd possible word is: %s",words[index])
sprintf("Probability of Occurrence is: %f", max(trans_updated))

# Generating a sentence with given start word using Markov chains
state=matrix(0,ncol=dim(transition)[1])
state=state%*%transition
temp=state
print("Input the word:")
input = readline()
sent=input
state[1,input]=1
while(1){
trans_updated=state%*%transition
if(max(trans_updated)!=0){
index = which(trans_updated==max(trans_updated))
sent=paste(sent,words[index][1])
state=temp
state[1, words[index][1]]=1
}
else{
break
}
}
sprintf("Generated Sentence is: %s",sent)

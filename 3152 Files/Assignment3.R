library(slam)
library(tm)
library(SnowballC)
rm(list = ls())

#Q2
setwd("D:/Desktop/R files")
cname = file.path(".", "A3_movie_reviews")
cname
print(dir(cname))
docs = Corpus(DirSource(cname))
docs

#Q3
#Tokenisation
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
#Case normalization
docs = tm_map(docs, content_transformer(tolower))

#Removing common characters that appear on almost all docs
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "-")
docs = tm_map(docs, toSpace, "character")
docs = tm_map(docs, toSpace, "film")
docs = tm_map(docs, toSpace, "movi")
docs = tm_map(docs, toSpace, "scene")
docs = tm_map(docs, toSpace, "’s")
docs = tm_map(docs, toSpace, "“")

#Removing more common characters from trial and error
docs = tm_map(docs, toSpace, "back")
docs = tm_map(docs, toSpace, "come")
docs = tm_map(docs, toSpace, "feel")
docs = tm_map(docs, toSpace, "get")
docs = tm_map(docs, toSpace, "like")
docs = tm_map(docs, toSpace, "make")
docs = tm_map(docs, toSpace, "much")
docs = tm_map(docs, toSpace, "will")
docs = tm_map(docs, toSpace, "back")
docs = tm_map(docs, toSpace, "also")

#Filter Words
# Remove stop words and white space
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stripWhitespace)

# Stem
docs = tm_map(docs, stemDocument, language = "english")

#Create document term matrix
dtm <- DocumentTermMatrix(docs)

dim(dtm)
dtm_df = as.data.frame(as.matrix(dtm))

#Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.4)
dtms = as.data.frame(as.matrix(dtms))
dtms
#> dtms = as.matrix(dtms)
#> write.csv(dtms,"dtms.csv")
dtms = cbind(dtms, aveng = dtm_df$aveng, endgam = dtm_df$endgam, batman = dtm_df$batman, 
             bruce = dtm_df$bruce, maverick = dtm_df$maverick, avatar = dtm_df$avatar, 
             water = dtm_df$water)
dim(dtms)

#Q4
cos_dist_matrix = dtms

#Getting the number of terms
num_terms = ncol(dtms)

#Creating columns representing documents
r_names = row.names(dtms)
for (i in 1:length(r_names)){
  cos_dist_matrix[r_names[i]] = 0
}
cos_dist_matrix
for (i in 1:nrow(dtms)){
  for (j in 1:nrow(dtms)){
    if (i != j){
      numerator = 0
      tot_doc1 = 0
      tot_doc2 = 0
      for (k in 1:num_terms){
        numerator = numerator + (dtms[i,k] * dtms[j,k])
        tot_doc1 = tot_doc1+dtms[i,k]
        tot_doc2 = tot_doc2+dtms[j,k]
      }
      cos_dist = numerator/sqrt(tot_doc1 * tot_doc2)
      cos_dist_matrix[i,r_names[j]] = cos_dist
    }
  }
}

cos_dist_matrix = cos_dist_matrix[,-c(1:num_terms)]
cos_dist_matrix = dist(scale(cos_dist_matrix))
cos_dist_matrix

#clustering
fit_cos = hclust(cos_dist_matrix, method = "ward.D")
plot(fit_cos, hang = -1)

#Cutting the tree to 4 clusters
cutfit = cutree(fit_cos, k = 4)
table(actual = r_names, fitted = cutfit)

#5
library(igraph)
library(igraphdata)

simil_dtms = dtms
#Creating columns representing documents
r_names = row.names(dtms)
for (i in 1:length(r_names)){
  simil_dtms[r_names[i]] = 0
}

simil_dtms

simil_dtms = as.matrix(dtms)
# convert to binary matrix
simil_dtms = as.matrix((simil_dtms > 0) + 0)
# multiply binary matrix by its transpose
simil_matrix = simil_dtms %*% t(simil_dtms)
# make leading diagonal zero
diag(simil_matrix) = 0
simil_matrix


#Creating the graph
simil_graph = graph_from_adjacency_matrix(simil_matrix, mode = "undirected", weighted = TRUE)

#Make edges look bigger based on weight and change weight color
E(simil_graph)$width = E(simil_graph)$weight/6.75

E(simil_graph)$color = "orange"

#Generate color based on their topics
clr = c('red','blue','purple','green')
simil_matrix
simil_graph
V(simil_graph)$color = c(clr[1],clr[2],clr[3],clr[3],clr[3],clr[3],clr[3],clr[4],clr[4],clr[4],clr[4],clr[1],clr[4],
                         clr[1],clr[1],clr[1],clr[2],clr[2],clr[2],clr[2])

#Change text color to white
V(simil_graph)$label.color = "white"

#Node size based on the total weight connected to the node
simil_matrix_dup = as.data.frame(simil_matrix)
simil_matrix_dup['weight_sum'] = 0
for (i in 1: nrow(simil_matrix)){
  simil_matrix_dup[i,'weight_sum'] = sum(simil_matrix[i,c(1:nrow(simil_matrix))])
}
simil_matrix
V(simil_graph)$size = simil_matrix_dup$weight_sum*0.16

#Simplifying the graph
simil_graph <- simplify(simil_graph, remove.multiple = F, remove.loops = T)


plot(main = "Single-mode network",simil_graph,  layout =  layout.circle)

betweenness = as.table(betweenness(simil_graph))
closeness = as.table(closeness(simil_graph))
eig = as.table(evcent(simil_graph)$vector)
t.simil_graph = as.data.frame(rbind(betweenness, closeness, eig))
t.simil_graph = t(t.simil_graph)
t.simil_graph

cfb = cluster_fast_greedy(simil_graph)
cle = cluster_leading_eigen(simil_graph)
clp = cluster_label_prop(simil_graph)
plot(cfb, simil_graph,vertex.label=V(simil_graph)$role,main="Fast Greedy")
plot(cle, simil_graph,vertex.label=V(simil_graph)$role,main="Leading EigenVector")
plot(clp, simil_graph,vertex.label=V(simil_graph)$role,main="Label Propogation")

#6
term_dtms = t(simil_dtms)
term_dtms
# multiply binary matrix by its transpose
term_matrix = term_dtms %*% t(term_dtms)
# make leading diagonal zero
diag(term_matrix) = 0
term_matrix

#Creating the graph
term_graph = graph_from_adjacency_matrix(term_matrix, mode = "undirected", weighted = TRUE)

#Make edges look bigger based on weight and change edge color
E(term_graph)$width = E(term_graph)$weight/5.75

E(term_graph)$color = "darkgreen"

#Change the color of the nodes
V(term_graph)$color = "lavender"

#Node size based on the total weight connected to the node
term_matrix_dup = as.data.frame(term_matrix)
term_matrix_dup['weight_sum'] = 0
for (i in 1: nrow(term_matrix)){
  term_matrix_dup[i,'weight_sum'] = sum(term_matrix[i,c(1:nrow(term_matrix))])
}
term_matrix
V(term_graph)$size = term_matrix_dup$weight_sum*0.125

#Simplifying the graph
simil_graph = simplify(term_graph, remove.multiple = F, remove.loops = T)

plot(main = "Single-mode network for terms",term_graph,  layout =  layout.circle)

betweenness = as.table(betweenness(term_graph))
closeness = as.table(closeness(term_graph))
eig = as.table(evcent(term_graph)$vector)
t.term_graph = as.data.frame(rbind(betweenness, closeness, eig))
t.term_graph = t(t.term_graph)
t.term_graph

cfb = cluster_fast_greedy(term_graph)
cle = cluster_leading_eigen(term_graph)
clp = cluster_label_prop(term_graph)
plot(cfb, term_graph,vertex.label=V(term_graph)$role,main="Fast Greedy")
plot(cle, term_graph,vertex.label=V(term_graph)$role,main="Leading EigenVector")
plot(clp, term_graph,vertex.label=V(term_graph)$role,main="Label Propogation")

#7
dtmsa = as.data.frame(dtms)
dtmsa$ABS = rownames(dtmsa)
dtmsb = data.frame()
for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)],colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb, touse ) } }
colnames(dtmsb) = c("weight", "abs", "token")
dtmsc = dtmsb[dtmsb$weight != 0,]
dtmsc
dtmsc = dtmsc[,c(2,3,1)]
dtmsc

bi_graph = graph.data.frame(dtmsc, directed=FALSE)
bipartite.mapping(bi_graph)
V(bi_graph)$type = bipartite_mapping(bi_graph)$type
V(bi_graph)$color = ifelse(V(bi_graph)$type, "magenta", "gold")
V(bi_graph)$shape = ifelse(V(bi_graph)$type, "circle", "square")
E(bi_graph)$color = "lightblue"

#Adding the weight to the edge
dtmsc$weight = as.numeric(dtmsc$weight)
E(bi_graph)$width = (dtmsc$weight)/5

plot(bi_graph, main="Bipartite Graph", layout = layout.fruchterman.reingold)

betweenness = as.table(betweenness(bi_graph))
closeness = as.table(closeness(bi_graph))
eig = as.table(evcent(bi_graph)$vector)
t.bi_graph = as.data.frame(rbind(betweenness, closeness, eig))
t.bi_graph = t(t.bi_graph)
t.bi_graph

cfb = cluster_fast_greedy(bi_graph)
cle = cluster_leading_eigen(bi_graph)
clp = cluster_label_prop(bi_graph)
plot(cfb, bi_graph,vertex.label=V(bi_graph)$role,main="Fast Greedy")
plot(cle, bi_graph,vertex.label=V(bi_graph)$role,main="Leading EigenVector")
plot(clp, bi_graph,vertex.label=V(bi_graph)$role,main="Label Propogation")










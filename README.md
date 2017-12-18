---
title: "Assignment 3"
author: "Tal Barami & Yair Ganon"
date: "December 2017"
output: html_document
---

---
title: "Assignment 3 - Part 1"
author: "Tal Barami & Yair Ganon"
date: "December, 2017"
output: html_document
---

# Getting started 

### Setting a working directory:
```{r}
folder = 'C:\\Users\\Tal\\Documents\\GitHub\\Network-analysis\\Network-analysis\\part1'
setwd(folder)

knitr::opts_knit$set(root.dir = folder)
```
# Part 1:
### Install & load igraph:
```{r}
#install.packages("igraph")
library(igraph)
```

### Load the data files & create the graph:
```{r}
g.data <- read.csv('resources\\ga_edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
g.vrtx <- read.csv('resources\\ga_actors.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(g.data, vertices=g.vrtx, directed=FALSE)

plot(g, vertex.size = 4, vertex.label.dist = 2, asp = F)
```
# Q1a:

### Find & plot the largest component:
```{r}
g.components <- components(g)
largest_component <- which(g.components$membership == which.max(g.components$csize))
g.largest_component <- induced_subgraph(g, largest_component)

plot(g.largest_component, vertex.size = 4, vertex.label.dist = 2, asp = F)
```

### Calculate the betweenness centrality using the igraph method:
### Graph is not directed, without weights on the edges, no need to normalize the result.
```{r}
bc <- betweenness(g.largest_component, directed = FALSE, weights = NULL, normalized = FALSE)
sort(bc, decreasing = T)[1:1]
```

### Calculate the closeness centrality using the igraph method:
### There are no weights on the edges, no need to normalize the result.
```{r}
cc <- closeness(g.largest_component, weights = NULL, normalized = FALSE)
sort(cc, decreasing = T)[1:1]
```

### Finally, calculate the eigen centrality using the igraph method:
### Graph is not directed, without weights on the edges, we were asked for the real value, so no need to scale the result.
```{r}
ec <- eigen_centrality(g.largest_component, directed = FALSE, weights = NULL, scale = FALSE, options = arpack_defaults)$vector
sort(ec, decreasing = T)[1:1]
```

## Results:
### The character with the highest betweenness centrality is Sloan with 115.36667
### The character with the highest closeness centrality is Torres with 0.01754386
### The character with the highest eigen vector centrality is Karev with 0.5027688



# Q1b:

## 1. We'll use Girvan-Newman community detection:
```{r}
girvan_newman <-  edge.betweenness.community(g)
girvan_newman
```

### Define a modularity measure that measures the quality of a network partition:
```{r}
#modularity for each phase of the previous algorithm
girvan_newman$modularity
#best modularity score
max(girvan_newman$modularity)
#index (phase, i.e. partitioning) with the best score
which.max(girvan_newman$modularity)
```

### Grant each node a color defined by partitions, using the membership function that returns community ids for each vertex, according to our clustering model object:
```{r}
#Store cluster ids for each vertex
gn_color <- membership(girvan_newman)
head(gn_color)
```

### Then, set the node's color (vertex.color) accordingly:
```{r}
plot(g, vertex.size = 4, vertex.label.dist = 2, asp = F, vertex.color = gn_color)
```

## Results:
### There are 7 communities: 1[8], 2[5], 3[5], 4[4], 5[3], 6[3], 7[4]
### This algotithm yields modularity of: 0.5774221

## 2. We'll use the fastgreedy community detection:
```{r}
g.simple <- simplify(g)
fastgreedy <- fastgreedy.community(g.simple)
```


### Define a modularity measure that measures the quality of a network partition:
```{r}
fastgreedy$modularity
max(fastgreedy$modularity)
which.max(fastgreedy$modularity)
```

### Grant each node a color defined by partitions, using the membership function that returns community ids for each vertex, according to our clustering model object:
```{r}
fg_color <- membership(fastgreedy)
head(fg_color)
```

### Then, set the node's color (vertex.color) accordingly:
```{r}
plot(g, vertex.size = 4, vertex.label.dist = 2, asp = F, vertex.color = fg_color)
```

## Results:
### There are 6 communities: 1[10], 2[5], 3[4], 4[5], 5[5], 6[3]
### This algotithm yields modularity of: 0.5947232


# Part 2:

# Twitter API

### Install and load the relevant packages:

```{r}
#install.packages("twitteR")
#install.packages("httr")
#install.packages("base64enc")
#install.packages("jsonlite")
#install.packages("wordcloud")
#install.packages("tm")
# install.packages("igraph")

library(twitteR)
library(httr)
library(jsonlite)
library(wordcloud)
library(tm)
library(igraph)
```

### Access using your twitterAuth.R file.
```{r}
source("resources\\twitterAuth.R")
user <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
```

### Warcraft is a popular PC game developed by Blizzard.
### We'll examine the connection between the twits about this game and the users.
```{r}
search <- searchTwitter("#warcraft", 50)
df_twits <- twListToDF(search)
search <- df_twits$screenName
df_users <- twListToDF(lookupUsers(search))

#df <- rbind(df_twits_warcraft, df_twits_starcraft)
```

### Warcraft is known as an addictive game. We'll check how many times each user has twitted about this game.
### Therefore, the vertices group will be the twits and the usernames. There will be an edge between a user and a twit if the user has twitted about this game.
```{r}
vertices <- c()
usernames <- c()
edge1 <- c()
edge2 <- c()
for(i in 1:nrow(df_users)){
  user <- df_users[i,]
  vertices <- c(vertices, user$screenName)
  usernames <- c(usernames, user$screenName)
}
for(i in 1:nrow(df_twits)){
  twit <- df_twits[i,]
  vertices <- c(vertices, twit$id)
}

for(i in 1:nrow(df_users)) {
  user <- df_users[i,]
  for(j in 1:nrow(df_twits)){
    twit <- df_twits[j,]
    if(user$screenName == twit$screenName){
      edge1 <- c(edge1, user$screenName)
      edge2 <- c(edge2, twit$id)
    }
  }
}
```

### Save the result into edgelist.csv file:
```{r}
edgelist <- cbind(from=edge1, to=edge2)
write.csv(edgelist, file="resources\\edgelist.csv", row.names = F)
write.csv(vertices, file="resources\\data.csv", row.names = F)
```

# Repeat the steps from part 1:

### Load the data files & create the graph:
```{r}
g.data <- read.csv('resources\\edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
g.vrtx <- read.csv('resources\\data.csv', header=TRUE, stringsAsFactors=FALSE)
#g <- graph.data.frame(g.data, vertices = g.vrtx, directed=FALSE)
g <- graph.data.frame(g.data, directed=FALSE)

  
plot(g, vertex.size = 4, vertex.label = "", asp = F)
```
# Q1a:

### Find & plot the largest component:
```{r}
g.components <- components(g)
largest_component <- which(g.components$membership == which.max(g.components$csize))
g.largest_component <- induced_subgraph(g, largest_component)

plot(g.largest_component, vertex.label = '', vertex.size = 4, asp = F)
```

### Calculate the betweenness centrality using the igraph method:
### Graph is not directed, without weights on the edges, no need to normalize the result.
```{r}
bc <- betweenness(g.largest_component, directed = FALSE, weights = NULL, normalized = FALSE)
sort(bc, decreasing = T)[1:1]
```

### Calculate the closeness centrality using the igraph method:
### There are no weights on the edges, no need to normalize the result.
```{r}
cc <- closeness(g.largest_component, weights = NULL, normalized = FALSE)
sort(cc, decreasing = T)[1:1]
```

### Finally, calculate the eigen centrality using the igraph method:
### Graph is not directed, without weights on the edges, we were asked for the real value, so no need to scale the result.
```{r}
ec <- eigen_centrality(g.largest_component, directed = FALSE, weights = NULL, scale = FALSE, options = arpack_defaults)$vector
sort(ec, decreasing = T)[1:1]
```

## Results:
### The user with the highest betweenness centrality is Kellryth with 6
### The user with the highest closeness centrality is Kellryth with 0.25
### The user with the highest eigen vector centrality is Kellryth with 0.7071068



# Q1b:

## 1. We'll use Girvan-Newman community detection:
```{r}
girvan_newman <-  edge.betweenness.community(g)
girvan_newman
length(membership(girvan_newman))
```

### Define a modularity measure that measures the quality of a network partition:
```{r}
#modularity for each phase of the previous algorithm
girvan_newman$modularity
#best modularity score
max(girvan_newman$modularity)
#index (phase, i.e. partitioning) with the best score
which.max(girvan_newman$modularity)
```

### Grant each node a color defined by partitions, using the membership function that returns community ids for each vertex, according to our clustering model object:
```{r}
#Store cluster ids for each vertex
gn_color <- membership(girvan_newman)
head(gn_color)
```

### Then, set the node's color (vertex.color) accordingly:
```{r}
plot(g, vertex.size = 4, vertex.label = '', asp = F, vertex.color = gn_color)
```

## Results:
### Each vertex is a community.
### This algotithm yields modularity of: 0.9664

## 2. We'll use the fastgreedy community detection:
```{r}
g.simple <- simplify(g)
fastgreedy <- fastgreedy.community(g.simple)
length(membership(fastgreedy))
```


### Define a modularity measure that measures the quality of a network partition:
```{r}
fastgreedy$modularity
max(fastgreedy$modularity)
which.max(fastgreedy$modularity)
```

### Grant each node a color defined by partitions, using the membership function that returns community ids for each vertex, according to our clustering model object:
```{r}
fg_color <- membership(fastgreedy)
head(fg_color)
```

### Then, set the node's color (vertex.color) accordingly:
```{r}
plot(g, vertex.size = 4, vertex.label = '', asp = F, vertex.color = fg_color)
```

## Results:
### Each vertex is a community.
### This algotithm yields modularity of: 0.9664
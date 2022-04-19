# Loading package to work with vk API
install.packages("vkR")
library(vkR)

# Getting app token from the url
# https://oauth.vk.com/blank.html#access_token=***&expires_in=86400&user_id=22722179
access_token <- setAccessToken("***")

# Saving token for future
# ***

# Getting data
d <- GET(url=str_c("https://api.vk.com/method/wall.get?domain=", "sociopat_ru", "&access_token=", access_token ))
data <- content(d, as="parsed")

# Getting list of users by membership
members <- getGroupsForUsers(c(me(), 123456), extended = 1, fields='city', progress_bar = TRUE)

# Creating data frame with users from the first group
all_members<- unlist(members)
first_group <- data.frame(group_id = 123456, all_members=all_members)

# Getting list of users by membership
members <- getGroupsForUsers(c(me(), 654321), extended = 1, fields='city', progress_bar = TRUE)

# Creating data frame with users from the first group
all_members<- unlist(members)
second_group <- data.frame(group_id = 654321, all_members=all_members)

# Loading packages for network analysis
library(dplyr)
library(stringr)
library(igraph)
library(tnet)
library(reshape2)

# Creating edges lists
edges_first <- select(first_group, group_id, all_members, Name)
edges_second <- select(second_group, group_id, all_members, Name)

# Binding the edges
edges <- rbind(edges_first, edges_second)

# Creating edge matrix
group_first = first_group$group_id
group_second = second_group$group_id
m = matrix(0, nrow = 1000, ncol = 50)
rownames(m) = group_first
colnames(m) = group_second
for (i in group_first) {
  a = subset(edges, group_id == i)
  a = a$all_members
  for (j in group_second) {
    b = subset(edges, group_id == j)
    b = b$all_members
    m[c(as.character(i)),c(as.character(j))] = length(intersect(a,b))/length(unique(c(a,b)))
  }
}

# Creating edgelist
library(reshape2)
edges <- melt(m)
edges <- dplyr::filter(edges, value >= 0.03)
quantile(edges$value, probs=0.9)

# Creating unweighted projection
g=graph.data.frame(edges[,1:2], directed=F)
V(g)$type <- bipartite.mapping(g)$type
proj1 <- bipartite.projection(g)$proj1

# Creating community
edges_comm <- fastgreedy.community(proj1)
length(edges_comm) #смотрим, сколько communities получается
sizes(edges_comm)
table <- cbind(edges_comm$membership, edges_comm$names)
plot(proj1.test,layout=layout.fruchterman.reingold,vertex.label = NA, edge.arrow.size=0, vertex.size=5, vertex.color=membership(edges_comm))

# Creating weighted projection
library(tnet)
edges <- unique(edges)
my_net <- projecting_tm(unique(edges[,1:2]), method="Newman")
my_net[,1]=as.character(my_net[,1])
my_net[,2]=as.character(my_net[,2])
my_net=as.matrix(my_net)
my_graph=graph.edgelist(my_net[,1:2],directed=FALSE)
E(my_graph)$weight=as.numeric(my_net[,3])
my_graph <- simplify(my_graph)

# Creating community
edges_comm <- fastgreedy.community(my_graph)
length(edges_comm)
sizes(edges_comm)
plot(my_graph,layout=layout.fruchterman.reingold,vertex.label = NA, edge.arrow.size=0, vertex.size=2, vertex.color=membership(edges_comm))
table <- data.frame(edges_comm$names, edges_comm$membership)
table <- as.data.frame(table)
table <- rename(table, group_id=edges_comm.names)
table$group_id <- as.character(table$group_id)
edges$group_id <- as.character(edges$group_id)
newman_table <- left_join(table, edges, by="group_id")
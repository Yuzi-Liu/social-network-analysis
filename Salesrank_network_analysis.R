library(igraph)
library(dplyr)
setwd("/Users/yuziliu/Desktop/Bana277")
products <- read.csv("products.csv")
copurchase <- read.csv("copurchase.csv")

# Step1: delete products that are not books from "products" and "copurchase" files. 
products1 <- products[products$group == "Book", ]
Book <- subset(products1, salesrank <= 150000 & salesrank != -1)
Book$downloads <- NULL
copurchase1 <- subset(copurchase, Source %in% Book$id & Target %in% Book$id)

# Step2: Create a variable named in-degree, to show how many "Source" products people who buy "Target" products buy; 
# i.e. how many edges are to the focal product in "co-purchase" network.
network <- graph.data.frame(copurchase1, directed = T)
indegree <- degree(network, mode = 'in')

# Step3: Create a variable named out-degree, to show how many "Target" products people who buy "Source" product also buy;
#i.e., how many edges are from the focal product in "co-purchase" network.
outdegree <- degree(network, mode = 'out')

# Step4: Pick up one of the products (in case there are multiple) with highest degree (in-degree + out-degree), 
# and find its subcomponent, i.e., all the products that are connected to this focal product.
all_degrees <- merge(indegree,outdegree,by="id")
all_degrees$total_degree <- all_degrees$indegree+all_degrees$out_degree
View(all_degrees)
unique_id <- all_degrees$id[which.max(all_degrees$total_degree)]
max_degree <- all_degrees$total_degree[which.max(all_degrees$total_degree)]
unique_id #Vertex with higest total degree
max_degree #Value of highest total degree 
graph1<-graph.data.frame(copurchase,directed = TRUE)
subcomp <- subcomponent(g1, "33", mode="all")

# Step5: Visualize the subcomponent using iGraph
graph2<-subgraph(graph1,subcomp)
V(graph2)$degree<- degree(graph2)
plot(graph2,
     vertex.size=V(graph2)$degree*0.4,
     edge.arrow.size=0.05,
     edge.color="black",
     vertex.label=NA,
     vertex.label.cex=0.4,
     vertex.color=rainbow(5),
     layout=layout.kamada.kawai)
diameter(graph2,direct=T, weights=NA)
diameter_of_vertices <- get_diameter(graph2, directed=T)

# Step6: Various statistics about this network (i.e., subcomponent), including degree distribution, density, and 
# centrality (degree centrality, closeness centrality and between centrality), hub/authority scores
deg_graph_all <- degree(graph2, mode='all')
deg_graph_in <- degree(graph2, mode='in')
deg_graph_out <- degree(graph2, mode='out')
deg <- cbind.data.frame(deg_graph_all,deg_graph_in,deg_graph_out)
View(deg)
edge_density(graph2, loops=F)
ecount(graph2)/(vcount(graph2)*(vcount(graph2)-1))
reciprocity(graph2)
closeness(graph2, mode='all', weights=NA)
betweenness(graph2, directed='T', weights=NA)
hub_score(graph2, scale = TRUE, weights = NULL, options = arpack_defaults)
authority_score(graph2, scale = TRUE, weights = NULL, options = arpack_defaults)

# Step7: Create a group of variables containing the information of neighbors that "point to" focal products. The variables include:
#a.	Neighbors' mean rating (nghb_mn_rating), 
#b.	Neighbors' mean salesrank (nghb_mn_salesrank), 
#c.	Neighbors' mean number of reviews (nghb_mn_review_cnt)

subcomp1 <-as_ids(subcomp)
Book$id <- as.character(Book$id)
filtered <- Book[Book$id %in% subcomp1,]
copurchase$Target <- as.character(copurchase$Target)
mean_values <- inner_join(copurchase, filtered, by = c("Target"="id")) %>%
  group_by(Target) %>%
  summarise(nghb_mn_rating = mean(rating),
            nghb_mn_salesrank = mean(salesrank),
            nghb_mn_review_cnt = mean(review_cnt))

# Step8: Fit a Poisson regression to predict salesrank of all the books in this subcomponent using products' own information and their neighbor's information.
new_product <- merge(products, all_degrees, by = "id", all.x = TRUE)

summary(salesrank_prediction <- glm(formula = new_product$salesrank ~ new_product$indegree + new_product$outdegree + new_product$total_degree +
                    new_product$review_cnt + new_product$downloads + new_product$rating, family="poisson", data=products))

exp(coef(salesrank_prediction))
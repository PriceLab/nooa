# data = {
#   "nodes": [
#     {"id": "Myriel", "group": 1},
#     {"id": "Napoleon", "group": 1},
#     {"id": "Mlle.Baptistine", "group": 1},
#     {"id": "Mme.Hucheloup", "group": 8}
#   ],
#   "links": [
#     {"source": "Napoleon", "target": "Myriel", "value": 1},
#     {"source": "Mlle.Baptistine", "target": "Myriel", "value": 8},
#     {"source": "Mme.Magloire", "target": "Myriel", "value": 10}
#     ]
#    };
library(jsonlite)
print(load("01may2020-curated.01-1k.nodes-1k.edges.RData"))
head(tbl.nodes)
dim(tbl.nodes)
dim(tbl.edges)

head(tbl.edges)
tbl.e <- tbl.edges[, 1:2]
tbl.e$value <- sample(1:10, nrow(tbl.e), replace=TRUE)
nodes <- with(tbl.e, sort(unique(c(source, target))))

nodeList <- lapply(nodes, function(node) list(id=node, group=sample(1:4,1, replace=TRUE)))
edgeList <- tbl.e

x <- list(nodes=nodeList, links=edgeList)
s <- sprintf("data = %s", toJSON(x, auto_unbox=TRUE))
f.out <- file("network.json")
writeLines(s, f.out)
close(f.out)



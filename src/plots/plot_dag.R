suppressMessages(library(Rgraphviz))

plot_dag <- function(){
  dag <- new("graphNEL",
    nodes=c("X", "Y", "W", "Uw", "Ux", "U"),
    edgemode="directed"
    )
  
  dag <- addEdge("X", "Y", dag, 1)
  dag <- addEdge("W", "Y", dag, 1)
  dag <- addEdge("X", "W", dag, 1)
  dag <- addEdge("Uw", "W", dag, 1)
  dag <- addEdge("Ux", "X", dag, 1)
  dag <- addEdge("U", "Y", dag, 1)
  
  attrs <- list(graph=list(rankdir="LR"), node=list(fontsize="8"))
  
  nodeAttrs <- list(
    shape=c("U"="box", "Uw"="box", "Ux"="box"),
    fillcolor=c("U"="black", "Uw"="black", "Ux"="black"),
    fontcolor=c("U"="white", "Uw"="white", "Ux"="white")
    )
  
  plot(dag, attrs=attrs, nodeAttrs=nodeAttrs)
}

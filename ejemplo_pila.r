Stack <- setRefClass("Stack",
                     fields = list(
                       vStack = "vector",
                       nTop = "numeric"
                     ),
                     
                     methods = list(
                       initialize = function() {
                         nTop <<- 1
                       },
                       push = function(nItem) {
                         vStack <<- c(vStack, nItem)
                         nTop <<- nTop + 1
                         vStack[nTop-1]
                       },
                       pop = function() {
                         if (nTop == 1) return(NULL)
                         nItem <- vStack[nTop-1]
                         nTop <<- nTop - 1
                         vStack <<- vStack[1:nTop-1]
                         nItem
                       },
                       top = function() {
                         vStack[nTop-1]
                       },
                       listar =function(){
                         vStack
                       }
                     )
)
s <- Stack()
s$push('cris')
s$push('mario')
s$push('pepe')
s$top()
s$pop()
s$listar()

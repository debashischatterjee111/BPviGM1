#'Triangles in 2D (raw)
#'
#' 5 Simulated coordinate for Triangles in 2D (raw), Data  built by : Debashis Chatterjee
#'
#' @docType data
#'
#' @usage data(fivetr)
#'
#'
#' @keywords datasets
#'
#' @references chatterjee et al. (2020) (\href{https://github.com/debashischatterjee111/BPviGM1})
#'
#' @source \href{https://github.com/debashischatterjee111/BPviGM1/data}
#'
#' @examples
#' data(fivetr)
#' plot(c(-5, 10), c(-2,8), type = "n", main="Raw Data-plot of  Simulated 5 Random Triangles", xlab="x",ylab="y")
#'polygon(fivetr[,1,1],fivetr[,2,1],density=20, col="pink", border="red")
#'polygon(fivetr[,1,2],fivetr[,2,2],density=20, col="green", border="forestgreen")
#'polygon(fivetr[,1,3],fivetr[,2,3],density=20, col="skyblue", border="blue")
#'polygon(fivetr[,1,4],fivetr[,2,4],density=20, col="orange", border="darkorange")
#'polygon(fivetr[,1,5],fivetr[,2,5],density=20, col="yellow", border="gold3", lty = 2)
#'
#'
#'
"fivetr"

#'1000 Concave Quadrilaterals  in 2D (raw)
#'
#' 1000 Simulated coordinate for 1000 Concave Quadrilaterals  in 2D (raw), Data  built by : Debashis Chatterjee
#'
#' @docType data
#'
#' @usage data(ccq)
#'
#'
#' @keywords datasets
#'
#' @references chatterjee et al. (2020) (\href{https://github.com/debashischatterjee111/BPviGM1})
#'
#' @source \href{https://github.com/debashischatterjee111/BPviGM1/data}
#'
#' @examples
#' data(ccq)
#' plot(c(-10, 30), c(-12,10), type = "n", main=" Simulated RawData: 1000 Random convex & concave Quadrilaterals",cex.main=0.98, xlab="x",ylab="y")

#'polygon(ccq[,1,1],ccq[,2,1],density=0, col="blue", border="blue", lwd=2)
#'for(k in 1:1000){
 #' polygon(ccq[,1,k],ccq[,2,k],density=0, col="green", border=alpha(rgb(0,0,1), 0.018), lty=2, lwd=1)}
#'
#'legend("bottomright", legend=c("1000 convex Quadrilaterals, true sigma=1.5", "1000 concave quadrilaterals, true sigma=0.8"),col=c("red", "blue"), lty=c(1,1), cex=0.65,title="Object shape types", text.font=4, bg='white')
#'
"ccq"

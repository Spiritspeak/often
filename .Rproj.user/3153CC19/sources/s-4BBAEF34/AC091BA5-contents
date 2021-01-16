#' Make a periodically re-evaluated value-returning function.
#'
#' @param fun Function that generates a value.
#' @param period Period, inseconds, after which to refresh the value.
#' @param ... Other arguments to be passed to the value-generating function.
#'
#' @return A function that returns a value, provided by the value-generating function,
#' and refreshes it after the provided refresh period.
#' @export
makePeriodicValue<-function(fun,period,...){
  value<-NULL
  ts<-NULL
  args<-list(...)
  returnfun<-function(){
    #refresh value if period is passed or if timestamp was not set
    if(any(is.null(ts), ts <= (as.numeric(Sys.time())-period))){
      ts <<- as.numeric(Sys.time())
      value <<- do.call(fun,args)
    }
    return(value)
  }
}


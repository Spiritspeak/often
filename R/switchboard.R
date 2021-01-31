#switchboard base code


#' @describeIn Switchboard add an action to a switchboard.
#' @param condition A logical statement, to be evaluated when the entire switchboard is evaluated.
#' If true, the associated action is executed.
#' The content of this argument remains unevaluated and is converted to a language object,
#' to be evaluated when the \code{evaluate} method is called.
#' However, content that's placed between \code{.()} is evaluated when running this function.
#' @param action A piece of code to be run when the associated if statement is evaluated and found to be true.
#' Content placed within \code{.()} is evaluated when running this function,
#' the rest remains unevaluated until the \code{evaluateAll} method is run.
#' @param name A name for this particular if-then statement. Can be left empty.
#' @param endChain Logical. Should this statement, if true, end the evaluation of any further statements?
#' @examples
addAction<-function(.self,condition,action,name=NULL,endChain=F,...){
  "Add an if-then statement to a switchboard."
  condition<-bquote.arg(condition,up=2)
  action<-bquote.arg(action,up=2)

  stopifnot(is.character(name) || is.null(name),is.logical(endChain))
  .self$actions<-c(.self$actions,list(list(
                                           name=name,
                                           condition=condition,
                                           action=action,
                                           endChain=endChain)))
}

#' @describeIn Switchboard Delete an action from a switchboard.
#' @param idx Character or numeric. The to-be-deleted if-then statement. You can provide either its name or its number.
#' @examples
delAction<-function(.self,idx){
  "Remove an if-then statement from a switchboard."
  if(is.character(idx)){
    idx<-which(sapply(.self$actions,FUN=function(x){x$name}) == idx)
  }
  .self$actions<-.self$actions[-idx]
}

#' @describeIn Switchboard Evaluate all actions in a switchboard.
#' Evaluate all if-then statements in a switchboard.
#' @examples
evaluateAll<-function(.self){
  "Evaluate all if-then statements in a switchboard."
  callenv<-new.env()

  for(act in .self$actions){
    message("Evaluating action ",act$name)
    try({
      if(eval(act$condition,envir=callenv)){
        eval(act$action,envir=callenv)
        if(act$endChain){
          break
        }
      }
    })
  }
}



#' @title Switchboard objects
#' @description switchboards are chains of if-then statements,
#' which can be very useful when long chains of such statements are needed,
#' such as when programming bots. Using switchboards allows users to divide parts of their chain
#' into different files or modules.
#'
#' @field actions A list of if-then statements.
#' @exportClass Switchboard
#' @export
Switchboard<-setRefClass(Class="Switchboard",
                         fields=list(actions="list"),
                         methods=list(addAction=addAction,
                                      delAction=delAction,
                                      evaluateAll=evaluateAll))

#' @describeIn Switchboard Make a switchboard object
#' @export
makeSwitchboard<-function(){
  Switchboard()
}



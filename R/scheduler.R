#library(later)
#library(magrittr)

#.jobtable "anatomy":
#  loopid = name of the loop that will call the function
#  jobid  = schedule time + random numbers, serves as identifier
#  status = ready, running, completed, missed, errored?
#  code   = the call to run
#  st     = scheduled time -> when it was scheduled
#  rt     = run time -> when the function should be run

#' @import later
#' @import magrittr
#' @import methods
#' @importFrom methods is new
#' @importFrom stats setNames

pkg.env<-globalenv()
.onLoad <- function(libname,pkgname){
  checkTables()
  #assign("pkg.env",value=pkg.env,envir=parent.frame())
  utils::globalVariables("pkg.env","often")
}

checkTables<-function(){
  if(!exists("looptable",envir=pkg.env)){
    resetLoopTable()
  }
  if(!exists("jobtable",envir=pkg.env)){
    resetJobTable()
  }
  if(nrow(pkg.env$looptable)==0){
    setLoop()
  }
}
resetLoopTable<-function(){
  pkg.env$looptable<-data.frame(loopid=NA,stateid=randomStateId(),status=NA,rate=1,
                                tolerance=0,on.error=NA,on.miss=NA,on.complete=NA,
                                starttime=NA)[F,]
}
resetJobTable<-function(){
  pkg.env$jobtable <- data.frame(loopid=NA,jobid=NA,status=NA,code=NA,st=NA,rt=NA)[F,]
}

randomStateId<-function(){
  Sys.time() %>% as.numeric() %>% as.character() %>% paste0(".",sample(0:99999,1))
}

#' @describeIn scheduler Returns the environment containing the
#' looptable and the jobtable, allowing for direct modification
#' rather than through helper functions.
#' @export
tableEnv<-function(){
  return(pkg.env)
}

#' @name scheduler
#' @title Schedule tasks
#' @description The following functions allow users to schedule functions in a virtual agenda,
#' and initiate a loop that periodically checks whether any particular function is due.
#'
#' @details
#' A job can be any line(s) of code, and it will be run at the designated timstamp if the associated loop is running.
#' @return
#'
#' @examples
#' addJob(code=print("Hello World"),runtime=now()+5)
#' delme<-addJob(code=print("Goodbye World"),runtime=now()+6)
#' delJob(jobid=delme)
#' addJob(code=stopLoop(),runtime=now()+7)
#' setLoop(rate=0.5)
#' print(jobList())
#' startLoop()
#'
#'
NULL

#' @describeIn scheduler Add a job to the schedule.
#'
#' @param code Code of any length. It will remain unevaluated until the job is run -
#' save for the parts of the code wrapped in \code{.()} brackets.
#' @param runtime a future timestamp (use in conjunction with \code{now()} for convenience)
#' @param loopid A handle to an event loop; optional.
#'
#' @export
addJob<-function(code,runtime,loopid="main"){
  st<-now()
  jobid<-randomStateId()
  if(!is.numeric(ncol(pkg.env$jobtable))){ checkTables() }
  temptable<-do.call(data.frame,setNames(as.list(rep(NA,ncol(pkg.env$jobtable))),colnames(pkg.env$jobtable)))
  temptable[1,]$code<-deparse(bquote.arg(code))
  temptable[1,]$jobid<-jobid
  temptable[1,]$st<-st
  temptable[1,]$rt<-runtime
  temptable[1,]$loopid<-loopid
  temptable[1,]$status<-"ready"
  pkg.env$jobtable<-rbind(pkg.env$jobtable,temptable)
  return(jobid)
}

#' @describeIn scheduler Delete a job. Use in conjunction with \code{jobList()}.
#' Deletes job(s) based on their number in the job queue/schedule, their IDs, or their status.
#'
#' @param jobid one or multiple jobids, which serve as identifiers for the scheduled jobs, to be deleted
#' @param jobnum Number of job(s) to be deleted
#' @param status status of job(s) to be deleted
#' @param delete Logical. If true, deletes jobs from schedule.
#' If false, sets their status to 'disabled' instead. Default is TRUE.
#'
#' @return \code{delJob()} returns TRUE if successful, FALSE if failed.
#' @export
#'
delJob<-function(jobid,jobnum,status,delete=TRUE){
  if(missing(jobnum)){
    if(missing(jobid)){
      if(missing(status)){
        stop("No arguments provided!")
        return(F)
      }else{
        doable<- jobnum <= nrow(pkg.env$jobtable)
        if(doable){
          if(delete){
            pkg.env$jobtable<-pkg.env$jobtable[-jobnum,]
          }else{
            pkg.env$jobtable[jobnum,]$status<-"disabled"
          }
        }else{
          stop("Line number exceeds number of jobs.")
        }
        return(doable)
      }
    }else{
      doable<- which(pkg.env$jobtable$jobid==jobid)
      if(length(doable)>0){
        if(delete){
          pkg.env$jobtable<-pkg.env$jobtable[-doable,]
        }else{
          pkg.env$jobtable[doable,]$status<-"disabled"
        }
      }else{
        stop("No jobs with the provided jobid found.")
      }
      return(length(doable)>0)
    }
  }else{
    doable<- which(pkg.env$jobtable$status==status)
    if(length(doable)>0){
      if(delete){
        pkg.env$jobtable<-pkg.env$jobtable[-doable,]
      }else{
        pkg.env$jobtable[doable,]$status<-"disabled"
      }
    }else{
      stop("No jobs with the provided status found.")
    }
    return(length(doable)>0)
  }
}

#' @describeIn scheduler Returns a data.frame containing all jobs, finished and unfinished.
#'
#' @export
jobList<-function(){
  return(pkg.env$jobtable)
}

#' @describeIn scheduler Configure an existing loop or create a new loop
#' with custom settings. When started using \code{startLoop()}, this loop
#' will periodically check for jobs to execute.
#' When an unknown \code{loopid} is given, a new loop is created.
#'
#' @param loopid A handle to an event loop.
#' @param rate numeric, in seconds. How often should the loop check for a new task to be run?
#' @param tolerance numeric, in seconds; jobs that were scheduled this many seconds
#' in the past will still be run. Jobs beyond this point will be considered missed.
#' @param on.error character; what is to be done when a job results in an error?
#' Defaults to "continue".
#' @param on.miss character; what is to be done when a job is missed?
#' Defaults to "continue".
#' @param on.complete character; what is to be done to the loop when
#' there are no more jobs to be run? Defaults to "terminate".
#'
#' @export
setLoop<-function(loopid="main",rate=1,tolerance=60,on.error="continue",
                  on.miss="continue",on.complete="terminate"){
  newtable<-data.frame(loopid=loopid,stateid=randomStateId(),status="inactive",
                       rate=rate,tolerance=tolerance,on.error=on.error,
                       on.miss=on.miss,on.complete=on.complete,starttime=NA)
  if(any(loopid %in% pkg.env$looptable$loopid)){
    newtable$status<-pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status
    pkg.env$looptable[pkg.env$looptable$loopid==loopid,]<-newtable
  }else{
    pkg.env$looptable<-rbind(pkg.env$looptable,newtable)
  }
}

#' @describeIn scheduler Start a loop with given loopid.
#'
#' @details \code{startLoop()} starts a loop.
#' @export
startLoop<-function(loopid="main"){
  checkTables()
  if(pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status=="active"){
    warning("Loop '",loopid,"' is already active!")
    return()
  }
  pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$stateid<-randomStateId()
  pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$starttime<-now()
  pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"active"
  carryLoop(loopid,pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$stateid)
}

#' @describeIn scheduler Stop a running loop.
#'
#' @details \code{stopLoop()} stops a loop.
#' @export
stopLoop<-function(loopid="main"){
  if(pkg.env$looptable$status[pkg.env$looptable$loopid == loopid] != "active"){
    warning("Loop '",loopid,"' is not running!")
    return()
  }
  pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$stateid<-randomStateId()
}

carryLoop<-function(loopid,stateid){
  #stop if stateid was scrambled
  if(stateid != pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$stateid){
    pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"aborted"
    message("Loop '",loopid,"' terminated by command.")
    return()
  }
  #Stop if there are no remaining jobs while the looper was set to terminate
  # when reaching this situation
  if(pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$on.complete=="terminate" &
     sum(pkg.env$jobtable$loopid==loopid & pkg.env$jobtable$status=="ready") == 0){
    pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"completed"
    message("Loop '",loopid,"' terminated because no more to-be-executed tasks remain.")
    return()
  }

  #find missed jobs and stop if the loop was set to terminate on this condition
  missed<-  pkg.env$jobtable[pkg.env$jobtable$loopid==loopid,]$status=="ready" &
    pkg.env$jobtable[pkg.env$jobtable$loopid==loopid,]$rt < now() -
    pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$tolerance
  #mark missed as missed
  if(sum(missed)>0){
    pkg.env$jobtable[pkg.env$jobtable$loopid==loopid,][missed,]$status<-"missed"
  }
  #stop if missed means terminate
  if(sum(missed)>0 & pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$on.miss=="terminate"){
    pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"missed"
    message("Loop '",loopid,"' terminated after scheduled task was missed.")
    return()
  }

  #pick most overdue jobs that have not been missed
  torun<-which(pkg.env$jobtable$loopid==loopid & pkg.env$jobtable$status=="ready" &
            pkg.env$jobtable$rt > (now() -
            pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$tolerance) &
            pkg.env$jobtable$rt <= now())

  #run overdue tasks one by one
  for(runline in torun){
    #run currently picked task
    out<-try(eval(parse(text=pkg.env$jobtable[runline,]$code)))

    #log errors and stop if required
    if(!is.null(attr(out,"condition"))){
      pkg.env$jobtable[runline,]$status<-"errored"
      if(pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$on.error=="terminate"){
        pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"errored"
        message("Loop '",loopid,"' terminated after scheduled task returned an error.")
        return()
      }
      #if not failed, set as successfully run
    }else{
      pkg.env$jobtable[runline,]$status<-"completed"
    }
  }

  #run next loop phase
  pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$status<-"active"
  delay <- quantize(now() + pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$rate,
                    init=pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$starttime,
                    step=pkg.env$looptable[pkg.env$looptable$loopid==loopid,]$rate,
                    bias="floor") - now()
  later::later(func=~carryLoop(loopid,stateid),delay=delay)
}

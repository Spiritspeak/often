library(often)

#impatient loop
setLoop(loopid="impatient_loop",tolerance=1,on.miss="terminate")
addJob(func=Sys.sleep(5),loopid="impatient_loop",runtime=now()+1)
addJob(func=print("noooo"),loopid="impatient_loop",runtime=now()+2)
startLoop("impatient_loop")

#errorshy loop
setLoop(loopid="errored_loop",tolerance=1,on.error="terminate")
addJob(func=stop("Error!!!"),loopid="errored_loop",runtime=now()+1)
addJob(func=print("oh hey it works"),loopid="errored_loop",runtime=now()+2)
startLoop("errored_loop")

#simultaneous loops
setLoop("saysMarco",rate=.5)
setLoop("saysPolo",rate=.5)
for(i in 1:5){ addJob(func=print("Marco"),runtime=now()+i, loopid="saysMarco") }
for(i in 1:5){ addJob(func=print("Polo"),runtime=now()+i+.5, loopid="saysPolo") }
startLoop("saysMarco")
startLoop("saysPolo")




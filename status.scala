class status(icp: Int, idura: Int){
	var cpmax: Int = icp
	var duramax: Int = idura
	var cp = cpmax - 22 //assume innerquiet on first step
	var dura = duramax
	var state = 1.0
	var manipCnt = 0
	var steadyCnt = 0
	var gsCnt = 0

	def stateChange(state: Double): Double = state match{
		case 1.5 => 1.0		//good
		case 4.0 => 0.5		//excellent
		case 0.5 => 1.0		//poor
		case 1.0 =>	{ def rand(x: Double): Double = x match { case x if (x > 0.95) => 4.0
															  case x if (x > 0.75) => 1.5
															  case _ => 1.0 }
					  rand(scala.util.Random.nextDouble())
					}
	}
	
	def nextState(spendDura: Boolean){
		if(spendDura && manipCnt < 1)		//if we are on a step that spends durability and we dont have manip up
			dura -= 10 
		if(!spendDura && manipCnt>0)		//if we are on a step that does not spend durability and we have manip up
			dura += 10
		if(manipCnt > 0)
			manipCnt -= 1
		if(steadyCnt > 0)
			steadyCnt -= 1
		if(gsCnt > 0)
			gsCnt -= 1
		state = stateChange(state)
	}
	
	def richStateHeader(){
		println("State\tDura\tCP\tProg")
	}
	def richStateInfo(){
		print(state+"\t"+dura+"\t"+cp)
	}
	def resetState(){
		cp = cpmax - 22
		dura = duramax
		state = 1.0
		manipCnt = 0
		steadyCnt = 0
		gsCnt = 0
		state = stateChange(state)
	}
	
	def getState(): Double = {
		state
	}
	
	def getDura(): Int = {
		dura
	}
	
	def getManipCnt(): Int = {
		manipCnt
	}
	
	def getSteadyCnt(): Int = {
		steadyCnt
	}
	
	def getGsCnt(): Int = {
		gsCnt
	}
	
	def setGsCnt(nv: Int){
		gsCnt = nv
	}
	
	def setSteadyCnt(nv: Int){
		steadyCnt = nv
	}
	
	def setManipCnt(nv: Int){
		manipCnt = nv
	}
	
	def getCp(): Int = {
		cp
	}
	
	def changeCp(delta: Int){
		cp += delta
	}
	
	def stateName(state: Double): String = state match{
		case 1.0 => "Normal"
		case 1.5 => "Good"
		case 4.0 => "Excel"
		case 0.5 => "Poor"
	}


}
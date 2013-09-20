class touch(ic: Int, is: status){
	var cnt: Int = ic
	var state: status = is
	def qualMap(cnt: Double): Int = { ((0.36*cnt+34.0)*state.getState).toInt }
	
	def innerQuietMap(cnt: Double): Double = {
		//println("iqmap: "+cnt+" -> "+cnt*(1+scala.math.min(iqStacks/5.0,2)))
		cnt*(1+scala.math.min(iqStacks/5.0,2))
	}
	
	var qual = 0
	var iqStacks = 0
	
	def resetTouch(){
		qual = 0
		iqStacks = 0
	}
	
	//generic touch function, call with the specific parameters for a given touch
	def touch(eff: Double, chance: Double){
		var x = scala.util.Random.nextDouble()
		if( state.getSteadyCnt > 0 ) x = x - 0.2
		var stridesMulti = 1
		if( state.getGsCnt > 0 ){
			stridesMulti = 2
			state.setGsCnt(0)
		}
		if( x < chance){ //success
			qual += (eff*stridesMulti*qualMap(innerQuietMap(cnt))).toInt
			//println((eff*stridesMulti*qualMap(innerQuietMap(cnt))).toInt)
			iqStacks += 1
		}
	}

	def getQual(): Int = {
		qual
	}

	/****************************************************
	*													*
	*	Touch implementations below						*
	*													*
	****************************************************/
	def hastyTouch(): Boolean = {
		touch(1.0, 0.5)
		state.nextState(true)
		true
	}
	def byregotsBlessing(): Boolean = {		//unimplemented yet, due to not being able to use generic touch
		false
	}
	def basicTouch(): Boolean = {
		if(state.getCp < 18){
			false
		}else{
			touch(1.0, 0.7)
			state.changeCp(-18)
			state.nextState(true)
			true
		}
	}
	def standardTouch(): Boolean = {
		if(state.getCp < 32){
			false
		}else{
			touch(1.25, 0.8)
			state.changeCp(-32)
			state.nextState(true)
			true
		}
	}
	def advTouch(): Boolean = {
		if(state.getCp < 48){
			false
		}else{
			touch(1.5, 0.9)
			state.changeCp(-48)
			state.nextState(true)
			true
		}
	}
}
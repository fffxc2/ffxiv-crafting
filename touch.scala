class touch(ic: Int, ip: craft){
	var cnt: Int = ic
	var parent: craft = ip
	def qualMap(cnt: Double): Int = { ((0.36*cnt+34.0)*parent.getState).toInt }
	
	var qual = 0
	var iqStacks = 0
	
	//generic touch function, call with the specific parameters for a given touch
	def touch(eff: Double, chance: Double){
		var x = scala.util.Random.nextDouble()
		if( parent.getSteadyCnt > 0 ) x = x + 0.2
		var stridesMulti = 1
		if( parent.getGsCnt > 0 ){
			stridesMulti = 2
			parent.zeroGsCnt
		}
		if( x > chance){ //success
			qual += eff*stridesMulti*qualMap(innerQuietMap(cnt))
			iqStacks += 1
		}
	}

	def getQual(): Int = {
		//qual
		0
	}

	/****************************************************
	*													*
	*	Touch implementations below						*
	*													*
	****************************************************/
	def hastyTouch(): Boolean = {
		touch(1.0, 0.5)
		true
	}
	def byregotsBlessing(): Boolean = {		//unimplemented yet, due to not being able to use generic touch
		false
	}
	def basicTouch(): Boolean = {
		if(parent.getCp < 18){
			false
		}
		touch(1.0, 0.7)
		parent.changeCp(-18)
		true
	}
	def standardTouch(): Boolean = {
		if(parent.getCp < 32){
			false
		}
		touch(1.25, 0.8)
		parent.changeCp(-32)
		true
	}
	def advTouch(): Boolean = {
		if(parent.getCp < 48){
			false
		}
		touch(1.5, 0.9)
		parent.changeCp(-48)
		true
	}
}
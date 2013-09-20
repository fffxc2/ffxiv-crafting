class synth(ic: Int, is: status){
	var crf: Int = ic
	var state: status = is
	def progMap(crf: Double): Int = { (0.21*crf+1.6).toInt }
	
	var prog = 0
	
	//generic synth function, call with the specific parameters for given synth
	def synth(eff: Double, chance: Double){
		var x = scala.util.Random.nextDouble()
		if( state.getSteadyCnt > 0 ) x = x - 0.2
		if( x <= chance ){
			prog += (eff*progMap(crf)).toInt
			//println("prog += "+(eff*progMap(crf)).toInt)
		}
	}
	
	def resetSynth(){
		prog = 0
	}

	def getProg(): Int = {
		prog
	}
	
	/****************************************************
	*													*
	*	Synth implementations below						*
	*													*
	****************************************************/
	def brandOfWater(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}
	def brandOfLightning(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}
	def brandOfEarth(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}
	def brandOfIce(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}
	def brandOfFire(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}
	def brandOfWind(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.0, 0.9)
			state.nextState(true)
			true
		}
	}	
	def carefulSynth(): Boolean = {
		synth(0.9, 1.0)
		state.nextState(true)
		true
	}
	def carefulSynth2(): Boolean = {
		synth(1.2, 1.0)
		state.nextState(true)
		true
	}
	def rapidSynth(): Boolean = {
		synth(2.5, 0.5)
		true
	}
	def basicSynth(): Boolean = {
		synth(1.0, 0.9)
		state.nextState(true)
		true
	}
	def standardSynth(): Boolean = {
		if(state.getCp < 15){
			false
		}else{
			state.changeCp(-15)
			synth(1.5, 0.9)
			state.nextState(true)
			true
		}
	}
	def flawlessSynth(): Boolean = {	//unimplemented yet, due to not being able to use generic synth
		false
	}
	def pieceByPiece(): Boolean = {		//unimplemented yet, due to not being able to use generic synth
		false
	}
}
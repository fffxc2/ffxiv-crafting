class synth(ic: Int, ip: craft){
	var crf: Int = ic
	var parent: craft = ip
	def progMap(crf: Double): Int = { (0.21*crf+1.6).toInt }
	
	var prog = 0
	
	//generic synth function, call with the specific parameters for given synth
	def synth(eff: double, chance: double){
		var x = scala.util.Random.nextDouble()
		if( parent.getSteadyCnt > 0 ) x = x + 0.2
		if( x > chance ){
			prog += eff*progMap(crf)
		}
	}

	def getProg(): Int = {
		//prog
		0
	}
	
	/****************************************************
	*													*
	*	Synth implementations below						*
	*													*
	****************************************************/
	def brandOfWater(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}
	def brandOfLightning(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}
	def brandOfEarth(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}
	def brandOfIce(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}
	def brandOfFire(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}
	def brandOfWind(): Boolean = {		//only partially implemented, the affinity double doesn't work yet
		if(parent.getCp < 15)
			false
		parent.changeCp(-15)
		synth(1.0, 0.9)
		true
	}	
	def carefulSynth(): Boolean = {
		synth(0.9, 1.0)
		true
	}
	def carefulSynth2(): Boolean = {
		synth(1.2, 1.0)
		true
	}
	def rapidSynth(): Boolean = {
		synth(2.5, 0.5)
		true
	}
	def basicSynth(): Boolean = {
		synth(1.0, 0.9)
		true
	}
	def standardSynth(): Boolean = {
		if(parent.getCp < 15){
			false
		}
		parent.changeCp(-15)
		synth(1.5, 0.9)
		true
	}
	def flawlessSynth(): Boolean = {	//unimplemented yet, due to not being able to use generic synth
		false
	}
	def pieceByPiece(): Boolean = {		//unimplemented yet, due to not being able to use generic synth
		false
	}
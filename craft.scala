object craft{
	var crf = 300
	var cnt = 300
	var cpmax = 300
	var duramax = 40
	var a1 = 0
	var a2 = 0
	var a3 = 0
	var a4 = 0
	var myStatus = new status(cpmax, duramax)
	var myTouch = new touch(cnt, myStatus)
	var mySynth = new synth(crf, myStatus)
	
	def reset(){
		if(a1 > 0 && a2 > 0 && a3 > 0 && a4 > 0){	//if we passed it valid values use them
			crf = a1
			cnt = a2
			cpmax = a3
			duramax = a4
		}else{	//otherwise use hardcoded state
			crf = 300
			cnt = 300
			cpmax = 300
			duramax = 40
		}
		myTouch.resetTouch
		mySynth.resetSynth
		myStatus.resetState
	}
			
	def innerQuiet(): Boolean = {
		if(myStatus.getCp < 18){
			false
		}else{
			myStatus.changeCp(-18)
			myStatus.nextState(false)
			true
		}
	}
	
	def greatStrides(): Boolean = {
		if(myStatus.getCp < 32){
			false
		}else{
			myStatus.changeCp(-32)
			myStatus.nextState(false)
			myStatus.setGsCnt(3)
			true
		}
	}
	
	def steadyHand(): Boolean = {
		if(myStatus.getCp < 22){
			false
		}else{
			myStatus.changeCp(-22)
			myStatus.nextState(false)
			myStatus.setSteadyCnt(5)
			true
		}
	}
	
	def TotT(): Boolean = {
		if(myStatus.getState != 1.5){
			false
		}else{
			myStatus.changeCp(20)
			myStatus.nextState(false)
			true
		}
	}
	
	def manipulation(): Boolean = {
		if(myStatus.getCp < 88){
			false
		}else{
			myStatus.changeCp(-88)
			myStatus.nextState(false)
			myStatus.setManipCnt(3)
			true
		}
	}
			
	def action(f: () => Unit){
		var actionTaken = f()
		if( actionTaken.isInstanceOf[Boolean] && !(actionTaken.asInstanceOf[Boolean]) ){ //actionTaken will be false if the action fails due to lack of cp, in which case don't advance state information
			println("Tried to take action while unable")
		}//state advancement occurs when the action is properly taken
	}
	
	def chainCraft() = {
		var skip = false
		if(myStatus.getState == 1.5){	//tott logic
			if(myStatus.getCp < cpmax && myStatus.getManipCnt == 0 && myStatus.getSteadyCnt == 0){
				//println(" - TotT")
				skip = true
				TotT
			}else if(myStatus.getCp + 20 < cpmax  && myStatus.getManipCnt == 0){
				//println(" - TotT")
				skip = true
				TotT
			}else if(myStatus.getCp + 20 < cpmax && myStatus.getDura < 40){
				//println(" - TotT")
				skip = true
				TotT
			}
		}
		if(myStatus.getState == 4.0){
			if((myStatus.getDura > 20 || (myStatus.getDura == 20 && myStatus.getManipCnt > 0)) && myStatus.getCp > 47){
				//print(" - Adv: ")
				skip = true
				myTouch.advTouch
			}
		}
		
		if(skip == false){
			if(myStatus.getDura == 20 && myStatus.getManipCnt < 1 && myStatus.getCp > 87){
				//println(" - Manip")
				manipulation
			}else if(myStatus.getCp > 21 && myStatus.getSteadyCnt < 1 && (myStatus.getDura < 40 || myStatus.getManipCnt == 0)){
				//println(" - Steady")
				steadyHand
			}else if(myStatus.getDura > 20 || (myStatus.getDura == 20 && myStatus.getManipCnt > 0) || (myStatus.getDura == 20 && myStatus.getCp + 20 < 88)){
				//print(" - Hasty: ")
				myTouch.hastyTouch
			}else{
				//println(" - Careful")
				mySynth.carefulSynth
			}
		}
	}
	
	def simpCraft() = {
		if(mySynth.progMap(crf) + mySynth.getProg > 50 && myStatus.getDura > 20 || (myStatus.getDura == 20 && myStatus.getManipCnt > 0)){
			//print(" - Hasty: ")
			myTouch.hastyTouch
		}else if(mySynth.progMap(crf) + mySynth.getProg < 50 && myStatus.getDura > 20 || (myStatus.getDura == 20 && myStatus.getManipCnt > 0)){
			//println(" - Careful")
			mySynth.carefulSynth
		}else if(myStatus.getCp > 87){
			//println(" - Manip")
			manipulation
		}else{
			//println(" - Careful")
			mySynth.carefulSynth
		}
	}
	
	def quickHq() = {
		if(myStatus.getGsCnt < 1 && myStatus.getCp > 31){
			//println(" - Great")
			greatStrides
		}else if(myStatus.getSteadyCnt < 1 && myStatus.getCp > 21){
			//println(" - Steady")
			steadyHand
		}else if(myStatus.getDura > 10 && myStatus.getCp > 47){
			//print(" - Adv: ")
			myTouch.advTouch
		}else if(myStatus.getDura > 10){
			//print(" - Hasty: ")
			myTouch.hastyTouch
		}else{
			//println(" - Careful")
			mySynth.carefulSynth
		}
	}
	
	def avg(f: () => Unit, count: Int): Double = {
		var avgProg = 0
		for(i <- 1 to count){
			reset
			//myStatus.richStateHeader
			while(mySynth.getProg < 50 && myStatus.getDura > 0){
				//myStatus.richStateInfo
				//print("\t"+mySynth.getProg+"\t")
				action(f)
			}
			//println("")
			//println("craft complete")
			//println("")
			//println("Final qual = "+myTouch.getQual)
			if(myStatus.getDura <= 0 && mySynth.getProg < 50){
				//println("Broke it, final prog: "+mySynth.getProg)
				//don't need to output
			}else{
				avgProg += myTouch.getQual // just add up the qual
				//println("Craft successful: "+mySynth.getProg) 
			}
		}
		avgProg/count
	}
	def main(args: Array[String]){		
		println("DOES NOT ACCOUNT FOR LEVEL VARIANCE BETWEEN CRAFTING LEVEL AND RECIPE LEVEL CURRENTLY")
		if(args.size > 3){
			a1 = args(0).toInt
			a2 = args(1).toInt
			a3 = args(2).toInt
			a4 = args(3).toInt
		}
		println("fastAvg - " + avg(quickHq,10000))
		println("simpAvg - " + avg(simpCraft,10000))
		println("chainAvg - " + avg(chainCraft,10000))
	}
}
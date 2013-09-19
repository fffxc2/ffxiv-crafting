object craft{
	var iqStacks = 0
	var crf = 300
	var cnt = 327
	var cp = 280 - 22 //iq on first step always
	var cpmax = 280
	var dura = 40
	var duramax = 40
	var prog = 0
	var qual = 0
	var state = 1.0
	var steadyCnt = 0
	var manipCnt = 0
	var gsCnt = 0
	
	def reset(){
		iqStacks = 0
		crf = 300
		cnt = 327
		cp = 280 - 22 //iq on first step always
		cpmax = 280
		duramax = 80
		dura = duramax
		prog = 0
		qual = 0
		state = 1.0
		steadyCnt = 0
		manipCnt = 0
		gsCnt = 0
		state = stateChange(state) //since we are assuming iq first always, advance to 2nd state
	}
	
	def qualMap(cnt: Double): Int = { ((0.36*cnt+34.0)*state).toInt }
	def progMap(crf: Double): Int = { (0.21*crf+1.6).toInt }
	
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
	
	def stateName(state: Double): String = state match{
		case 1.0 => "Normal"
		case 1.5 => "Good"
		case 4.0 => "Excel"
		case 0.5 => "Poor"
	}
	
	def hastyTouch(){
		var x = scala.util.Random.nextDouble()
		if(steadyCnt > 0) x = x + 0.2
		var qualChange = 0
		var stridesMulti = 1
		if(gsCnt > 0){
			stridesMulti = 2
			gsCnt = 0
		}
		if( x > 0.5 ){	//success
			var temp = stridesMulti*qualMap(innerQuietMap(cnt))
			qual += temp
			iqStacks += 1
			//println("success +"+temp)
		}else{				//failure
			//println("failure")
		}
		//stateChange(state)
	}
	
	def advTouch(){
		var x = scala.util.Random.nextDouble()
		if(steadyCnt > 0) x = x + 0.2
		var qualChange = 0
		var stridesMulti = 1
		if(gsCnt > 0){
			stridesMulti = 2
			gsCnt = 0
		}
		if( x > 0.9 ){ //success
			var temp = stridesMulti*qualMap(innerQuietMap(cnt))
			qual += temp
			iqStacks += 1
			//println("success +"+temp)
		}else{	//failure
			//println("failure")
		}
	}
	
	
	
	def carefulSynth(){
		prog += progMap(crf)
		//stateChange(state)
	}
	
	def innerQuietMap(cnt: Double): Double = {
		//println("iqmap: "+cnt+" -> "+cnt*(1+scala.math.min(iqStacks/5.0,2)))
		cnt*(1+scala.math.min(iqStacks/5.0,2))
	}
	
	def innerQuiet(){
		cp = cp - 18
		//stateChange(state)
		//if(dura+20 <= duramax)
		dura += 10 //refund dura now, since action always spends it
	}
	
	def greatStrides(){
		cp = cp - 32
		dura += 10
		gsCnt = 4
	}
	
	def steadyHand(){
		cp = cp - 22
		steadyCnt = 6
		//stateChange(state)
		//if(dura+20 <= duramax)
		dura += 10 //refund dura now, since action always spends it
	}
	
	def TotT(){
		if(state == 1.5) cp = cp + 20
		//stateChange(state)
		//if(dura+20 <= duramax)
		dura += 10 //refund dura now, since action always spends it
	}
	
	def manipulation(){
		cp = cp - 88
		manipCnt = 4
		//stateChange(state)
		//if(dura+20 <= duramax)
		dura += 10 //refund dura now, since action always spends it
	}
			
	def action(f: () => Unit){
		f()
		if(!(manipCnt > 0 && manipCnt < 4)){
			dura -= 10 
		}
		if(manipCnt > 0)
			manipCnt -= 1
		if(steadyCnt > 0)
			steadyCnt -= 1
		if(gsCnt > 0)
			gsCnt -= 1
		//print("State "+stateName(state)+" -> ")
		state = stateChange(state)
		//print(stateName(state))
	}
	
	def chainCraft() = {
		var skip = false
		if(state == 1.5){	//tott logic
			if(cp < cpmax && manipCnt == 0 && steadyCnt == 0){
				//println(" - TotT")
				skip = true
				TotT
			}else if(cp + 20 < cpmax  && manipCnt == 0){
				//println(" - TotT")
				skip = true
				TotT
			}else if(cp + 20 < cpmax && dura < 40){
				//println(" - TotT")
				skip = true
				TotT
			}
		}
		if(state == 4.0){
			if((dura > 20 || (dura == 20 && manipCnt > 0)) && cp > 47){
				//print(" - Adv: ")
				skip = true
				advTouch
			}
		}
		
		if(skip == false){
			if(dura == 20 && manipCnt < 1 && cp > 87){
				//println(" - Manip")
				manipulation
			}else if(cp > 21 && steadyCnt < 1 && (dura < 40 || manipCnt == 0)){
				//println(" - Hand")
				steadyHand
			}else if(dura > 20 || (dura == 20 && manipCnt > 0) || (dura == 20 && cp + 20 < 88)){
				//print(" - Hasty: ")
				hastyTouch
				//println(cp+" "+steadyCnt+" "+dura+" "+manipCnt)
			}else{
				//println(" - Final Careful")
				carefulSynth
			}
		}
	}
	
	def simpCraft() = {
		if(progMap(crf) + prog > 50 && dura > 20 || (dura == 20 && manipCnt > 0)){
			//print(" - Hasty: ")
			hastyTouch
		}else if(progMap(crf) + prog < 50 && dura > 20 || (dura == 20 && manipCnt > 0)){
			//println(" - Careful")
			carefulSynth
		}else if(cp > 87){
			//println(" - Manip")
			manipulation
		}else{
			//println(" - final Careful")
			carefulSynth
		}
	}
	
	def quickHq() = {
		if(gsCnt < 1 && cp > 31){
			greatStrides
		}else if(steadyCnt < 1){
			steadyHand
		}else if(dura > 10 && cp > 48){
			advTouch
		}else if(dura > 10){
			hastyTouch
		}else{
			carefulSynth
		}
	}
	
	def avg(f: () => Unit, count: Int): Double = {
		var avgProg = 0
		for(i <- 1 to count){
			reset
			while(prog < 50 && dura > 0){
				//print(stateName(state))
				//print("\t "+dura+" d, "+cp+" cp, "+manipCnt+" m, "+steadyCnt+" s")
				//no output, just loop through
				action(f)
			}
			if(dura <= 0 && prog < 50){
				//println("Broke it") 
				//don't need to output
			}else{
				avgProg += qual // just add up the qual
				//println("Craft successful: "+qual+"/maxqual") 
			}
		}
		avgProg/count
	}
	def main(args: Array[String]){
		//println("Test")
		//println(qualMap(100.0))
		
		println("DOES NOT ACCOUNT FOR LEVEL VARIANCE BETWEEN CRAFTING LEVEL AND RECIPE LEVEL CURRENTLY")
		/*println("simpCraft")
		println("Difficulty == 50")
		reset
		while(prog < 50 && dura > 0){
			print(stateName(state))
			print("\t "+dura+" d, "+cp+" cp, "+manipCnt+" m, "+steadyCnt+" s")
			action(simpCraft)
		}
		if(dura <= 0 && prog < 50){
			println("Broke it")
		}else{
			println("Craft successful: "+qual+"/maxqual")
		}
		
		println("chainCraft")
		println("Difficulty == 50")
		reset
		while(prog < 50 && dura > 0){
			print(stateName(state))
			//print("\t "+prog/50.0*100+"%, "+dura+" dura, "+cp+" cp ")
			print("\t "+dura+" d, "+cp+" cp, "+manipCnt+" m, "+steadyCnt+" s")
			action(chainCraft)
		}
		if(dura <= 0 && prog < 50){
			println("Broke it")
		}else{
			println("Craft successful: "+qual+"/maxqual")
		}*/
		
		println("fastAvg - " + avg(quickHq,10000))
		println("simpAvg - " + avg(simpCraft,10000))
		println("chainAvg - " + avg(chainCraft,10000))
	}
}
#### This is the interpreter escape game package. 
#### Created by Mike Higgins

initInterpreter = function(){
	
	#Initialize gameState
	gameState = list()
	gameState$Location = "FirstRoom"
	gameState$Inventory = NULL
	
	#Initialize Status
	gameState = updateInterpreter(gameState)

	#Output gameState
	gameState	
}

updateInterpreter = function(gameState){
	
	#Update First Room
	if(is.null(gameState$FirstRoom$Init)){
		gameState$FirstRoom$DoorUnlocked = FALSE
		gameState$FirstRoom$SafeUnlocked = FALSE
		gameState$FirstRoom$Init = TRUE
	}
	gameState
}

getLook = function(gameState, obj){

	if(obj == "FirstRoom"){
		myMap = list()
		myMap$x = c(0,3,17,20,0,NA,
				    0,0,3,3,NA,
				    20,20,17,17,NA,
				    3,3,17,17,NA,
				    5,5,9,9,NA,
				    11,11,16,16,11,NA,
				    12,12,15.5,15.5)
		myMap$y = c(0,4,4,0,0,NA,
				    0,20,20,4,NA,
				    0,20,20,4,NA,
				    4,20,20,4,NA,
				    4,15,15,4,NA,
				    13,18,18,13,13,NA,
				    4,8,8,4)
		myMap$range = c(0,20,0,20)
		myMap$names = c("floor","leftWall","rightWall","backWall",
			            "door","picture","safeFront")
		myCols =      c("tan", "lightblue","lightblue","lightblue",
					    "white", "white","olivedrab")
		map(myMap, col = myCols, fill = TRUE)
		points(c(8,8,14.75,14.75),c(9,9,5.75,5.75), pch = c(1,19,15,15),
			cex = c(1.75,1.7,1.5,1), col = c("black","gold","white","black"))
		points(c(12,13.5,15),c(15.5,14.5,17),pch = c("x","x","?"))
		segments(11,14.5,16,16.5)
		text(c(7,13.5,13.75),c(16,19,9),c("Door", "Picture","Safe"))
		
		cat("In front of you, you see a door, a picture, and a safe.")
	}
	
	else if(obj == "pictureFirstRoom"){
		x = c(1,3,4,5,7)
		y = c(6,2,10,8,14)
		myLM = lm(y~x)
		plot(x,y,xlab = "", ylab = "", xlim = c(0,8), ylim = c(1,15),
			main = "Regression Line:\ny = 1.5x + 2\nSolve for ?", xaxt = "n", yaxt = "n")
		abline(myLM)
		text(x,y+.5,c("(1,6)","(3,2)","(4,10)","(5,8)","(7,?)"))
	}

	else if(obj == "safeFirstRoom"){
		cat("It is a safe.\nThere is a screen that allows you to input a double digit number.\nInputting the correct number will unlock the safe\nIt is not the most secure safe in the world.")
	}
	
	else if(obj == "doorFirstRoom"){
		
		if(gameState$FirstRoom$DoorUnlocked){
			cat("This is the door to the outside world.  It is currently unlocked.")
		}
		else{
			cat("This is the door to the outside world.  It is currently locked.")
		}
	}	
	else{
		print(paste("The object ", obj, " cannot be viewed while in this room."))
	}
}

getObjName = function(gameState,obj){
	name = NULL	
	if(is.null(obj)){
		name = gameState$Location
	}
	else{	
		#Check if object is a character
		if(!is.character(obj)){
			stop("Object in function must be in quotes.\nSee instructions() for details.")
		}	
		else{
			print(obj)
			obj = tolower(obj)
			name = paste(c(obj,gameState$Location),collapse = "")
		}
	}
}

useInterpreter = function(gameState,obj){
	myObj = getObjName(gameState,obj)
	if(is.null(obj)){
		
	}
	
	else{
		print(paste("You cannot use the object ", obj))
	}
}

lookInterpreter = function(gameState,obj){
	myObj = getObjName(gameState,obj)
	getLook(gameState, myObj)
}

plotInterpreter = function(){}

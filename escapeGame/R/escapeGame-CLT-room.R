#### CLT Room for escape game
#### Modified by Brad Luen 5/26/14

# initialize room
# need ExponentialRod (get in first room) to get the MeanJewel and defeat the wizard

# Functions:
# update: Check if initialized; if not, initialize
# look
# getObj ("get" is a problem)
# use: things you can use here are ExponentialRod and MeanJewel
# movement
# inventory (global)
# talk

init.CLT = function(gameState){
	sampleMeans = c()
	print("A Wizard approaches and starts yelling at you.")
}

talk.CLT = function(obj){
	if(obj == "Wizard"){
		# if you don't have the ExponentialRod
		# print("")
		print("'I am the AbNormal Wizard! I can only be defeated by normality!'")
	}
}

enter.CLT = function(x){ # Name? Save enter for movement?
	if(abs(x-mean(gameState$Data)) < 0.01){
		sampleMeans = c(sampleMeans, mean(gameState$Data))
		hist(sampleMeans)
		if(length(sampleMeans = 3){
			print("Correct! Congratulations! You earned the MeanJewel! The MeanJewel can generate nine samples at once, AND find the sample mean of each.")
		}
		print("Correct! Now use the ExponentialRod again.")
	}
	else{
		print("Nope! Try again.")
	}
}

look.CLT = function(obj){
	if(obj == "Wizard"){print("You stare at the AbNormal Wizard. It's hard to describe how he looks, but it's definitely not bell-shaped.")
	} else if (obj == "Door"){print("You see two doors. One door leads back to the lobby. The other is blocked by a wizard who looks more than a little skewed.")
	} else if (obj == ""){print("You see two doors. One door leads back to the lobby. The other is blocked by a wizard who looks more than a little skewed.")
	} else{print("You don't see one of those.")}
}

use.CLT = function(obj){
	if(obj == "Rod"){obj = "ExponentialRod"}
	if(obj == "Exponential"){obj = "ExponentialRod"}
	if(obj == "ExponentialRod"){
		gameState$Data = rexp(100, 0.1)
		print("You use the ExponentialRod to sample 100 observations from an exponential distribution.")
		hist(data)
	}
	if(obj == "MeanJewel"){
		data = matrix(rexp(1000,0.1),100,10)
		sampleMeans = c(sampleMeans, apply(data, 2, mean))
		print("You use the MeanJewel to find ten sample means for sets of 100 observations from an exponential distribution.")
		hist(sampleMeans)
		if(sampleMeans-10,"pnorm")$stat < 0.1){
			print("The Wizard shouts, 'This looks like a normal distribution! Nooooo!'")
			# unlock exit 
		}
	}
	else{
		print(paste("You cannot use the", obj))
	}
}




# Copy-pasted from escapeGameInterpreter



updateInterpreter = function(gameState){
	
	#Update First Room
	#The init is helpful to determine if we have initialized a given location
	if(is.null(gameState$FirstRoom$Init)){
		
		#Need to check if door is locked or unlocked
		gameState$FirstRoom$DoorUnlocked = FALSE
		
		#Need to check to see if the safe is locked or unlocked
		gameState$FirstRoom$SafeUnlocked = FALSE
		
		#Some intro words:
		cat("Welcome!  This is a beta version of the\nStatistical Testing and Proficiency System (STAPS).\nPlease enter your student ID and press enter.\n")
		gameState$SID = readline(prompt = "Student ID: ")

		cat("\nGreat! Let's get started.\nYou are in a room.  To view the room, type look(). To look at individual objects, type look(\"OBJECT NAME\")\nwhere \"OBJECT NAME\" is replaced with the name of the object of interest.\nTo take objects, type take(\"OBJECT NAME\").\nTo download available data, type gameData()\n")
		
		#We initialized the first room.  
		#So we set this equal to true
		gameState$FirstRoom$Init = TRUE
	}
	gameState
}

#Initialize the game state upon startGame
initInterpreter = function(){
	
	#Initialize gameState
	gameState = list()
	gameState$Location = "CLTRoom"
	gameState$Inventory = NULL
	gameState$Data = c(1,2,3,4,5)
	gameState$Password = mean(gameState$Data)
	
	#Initialize Status
	gameState = updateInterpreter(gameState)

	#Output gameState
	gameState	
}

getLook = function(gameState, obj){

	if(obj == "CLTRoom"){
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
		
		cat("In front of you, you see a door, a picture, and a safe.\n")
	}
	
	else if(obj == "pictureFirstRoom"){
        x = c(1,2,3,4,5)
        plot(x,rep(0,length(x)),type="n",xlab="",ylab="",axes=F)
        text(x,rep(0,length(x)),x,cex=4)
	}

	else if(obj == "safeFirstRoom"){
		cat("It is a safe.\nThere is a screen that allows you to input a number.\nNext to the screen, an inscription says 'ENTER THE MEAN.'\nInputting the correct number will unlock the safe\nIt is not the most secure safe in the world.\nType enter(NUMBER) to enter a number.\n")
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
	
	if(obj == "Second Room"){
		print("How did you get here? We haven't even made this room yet.")
	}
}

unlock = function(gameState,number){
    if(gameState$Location == "FirstRoom" && gameState$Password == number){
	    gameState$Location == "Second Room"
		# Generate second room
    	print("The door opens, and you walk through. Oh great, it's another locked room.")
    }
    if(gameState$Location== "LastRoom" && gameState$Password == number){
    	print("The door opens, and you walk through. You are free. You're blinded by tears of joy, which prevent you from seeing the bus that crashes into you.")
    }
    else{
    	print("A sad trombone plays. Try again.")
    }
}




#Used to standardize object names.
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

enterInterpreter = function(gameState,number){
	unlock(gameState, number)
}

lookInterpreter = function(gameState,obj){
	myObj = getObjName(gameState,obj)
	getLook(gameState, myObj)
}


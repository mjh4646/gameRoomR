#### Interpreter escape game package: The LLN room
#### Mike Higgins & Brad Luen
#### Last modified 7/31/14

#### This function will eventually be used to 
#### Update which location we are in.
updateInterpreter = function(gameState){
	
	#Update First Room
	#The init is helpful to determine if we have initialized a given location
	if(is.null(gameState$FirstRoom$Init)){
		
		#Need to check if door is locked or unlocked
		gameState$FirstRoom$DoorUnlocked = FALSE
		
		#Need to check to see if the safe is locked or unlocked
		gameState$FirstRoom$SafeUnlocked = FALSE
		
		#Some intro words:
		cat("Welcome to the\nStatistical Testing and Proficiency System (STAPS).\nYou are in a small room. Your task is to escape.\nTo view the room, type look().\nTo look at an object, type look(\"OBJECT NAME\")\nwhere \"OBJECT NAME\" is replaced with the name of the object.\nTo use an object, type use(\"OBJECT NAME\").\n")
		
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
	gameState$Location = "FirstRoom"
	gameState$Inventory = NULL
	gameState$Data = c()
	gameState$Means = c()
    gameState$Coins = 1
	gameState$Switch = FALSE
	
	#Initialize Status
	gameState = updateInterpreter(gameState)

	#Output gameState
	gameState	
}


getLook = function(gameState, obj){

	if(obj == "FirstRoom"){
        par(mfrow=c(1,1))
		myMap = list()
		myMap$x = c(0,3,17,20,0,NA,
				    0,0,3,3,NA,
				    20,20,17,17,NA,
				    3,3,17,17,NA,
				    5,5,9,9,NA,
				    11,11,16,16,11,NA,
					13,13,13.75,13.75,13,NA,
					13.75,13.75,14.5,14.5,13,NA,
				    12,12,15.5,15.5)
		myMap$y = c(0,4,4,0,0,NA,
				    0,20,20,4,NA,
				    0,20,20,4,NA,
				    4,20,20,4,NA,
				    4,15,15,4,NA,
				    15,18,18,15,15,NA,
					11.5,12,12,11.5,11.5,NA,
					11.5,12,12,11.5,11.5,NA,
				    4,8,8,4)
		myMap$range = c(0,20,0,20)
		myMap$names = c("floor","leftWall","rightWall","backWall",
			            "door","screen","leftSwitch","rightSwitch","safeFront")
		myCols =      c("tan", "lightblue","lightblue","lightblue",
					    "white", "white","grey","black","olivedrab")
		map(myMap, col = myCols, fill = TRUE)
		points(c(8,8,14.75,14.75),c(9,9,5.75,5.75), pch = c(1,19,15,15),
			cex = c(1.75,1.7,1.5,1), col = c("black","gold","white","black"))
            #points(c(12,13.5,15),c(15.5,14.5,17),pch = c("x","x","?"))
            #segments(11,14.5,16,16.5)
		text(c(7,13.5,13.75,13.75),c(16,19,13,9),c("Door", "Screen", "Switch", "Button"))
		
		cat("In front of you, you see a Door, a Screen, a Switch, and a Button.\n")
	}
	
	else if(obj == "screenFirstRoom"){
		if(length(gameState$Data) == 0){
			print("The screen is blank. I wonder what the button does?")
        } else if(gameState$Switch==FALSE){
            plot(gameState$Data, main="One set of coins", xlab="",ylab="Heads",ylim=c(0,1))
            abline(h=0.47, lty=3)
            abline(h=0.53, lty=3)
            print(paste("The screen shows the results of one set of", length(gameState$Data),"tosses."))
        } else {
            plot(gameState$Means*100, main="1,000 sets of coins", xlab="Set number", ylab="Percentage of heads",ylim=c(0,100),cex=0.7)
            abline(h=47, lty=3)
            abline(h=53, lty=3)
            print(paste("The screen shows the percentage of heads for 1,000 sets of tosses."))
        }
	}

	else if(obj == "buttonFirstRoom"){
		cat("There is a panel with one button on it.\nAn inscription reads: 'To toss 5 coins, type toss(5).\n")
	}
	
	else if(obj == "doorFirstRoom"){
		
		if(gameState$FirstRoom$DoorUnlocked){
			cat("This is the door to the outside world.  It is currently unlocked.\n")
		}
		else{
			cat("This is the door to the outside world.  It is currently locked.\nA panel reads 'TO OPEN, GET (ABOUT) 95% OF THE DOTS WITHIN THE DASHED LINES.'\n")
		}
	}	
	else if(obj == "switchFirstRoom"){
        tempState = get("gameState", myE)
        if(tempState$Switch==FALSE){
            cat("There is a switch with two settings: '1' and '1000'.\nIt is currently set to '1'.\nTo flick the switch, type use(\"Switch\").\n")
        }
        if(tempState$Switch==TRUE){
            cat("There is a switch with two settings: '1' and '1000'.\nIt is currently set to '1000'.\nTo flick the switch, type use(\"Switch\").\n")
        }
	}
	else{
		print(paste("The object ", obj, " cannot be viewed while in this room."))
	}
	
	if(obj == "SecondRoom"){
		print("How did you get here? We haven't even made this room yet. Are you a ghost? If so, that is cool.")
	}
}

unlock = function(gameState,number){
    if(gameState$Password == number){
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
			#print(obj)
			obj = tolower(obj)
			name = paste(c(obj,gameState$Location),collapse = "")
		}
	}
}

toss = function(coins){
    tempState = get("gameState", myE)
    if(tempState$Switch == FALSE){
        tempState$Coins = coins
        tempState$Data = rbinom(coins,1,0.5)
		tempState$Means = mean(tempState$Data)
        print(paste("You push the button.",coins,"coins are tossed, of which",sum(tempState$Data),"are heads."))
    } else if(tempState$Switch == TRUE){
        tempState$Coins = coins
		tempState$Data = rbinom(1,coins,0.5)
        tempState$Means = rbinom(1000,coins,0.5)/coins
        cat(paste("You push the button. One thousand sets of",coins,"coins are tossed.\n The number of heads ranges from",min(tempState$Means*coins),"to",max(tempState$Means*coins),".\n"))
    }
    assign("gameState",tempState,myE)
    look("Screen")
}

useInterpreter = function(gameState,obj){
	myObj = getObjName(gameState,obj)
	if(obj == "Button"){
        coins = readline("How many coins would you like to toss at a time?\n")
        toss(coins)
	} else if(obj == "Switch"){
        tempState = get("gameState", myE)
        if(tempState$Switch == FALSE){
            tempState$Switch = TRUE
            print("You flick the switch to the setting '1000'.")
            assign("gameState",tempState,myE)
        } else if(tempState$Switch == TRUE){
            tempState$Switch = FALSE
            print("You flick the switch to the setting '1'.")
            assign("gameState",tempState,myE)
        }
        #look("Screen")
    } else if(obj == "Door"){
        tempState = get("gameState", myE)
        if(tempState$Switch==FALSE){
            cat("None of the dots are within the dotted lines.\n Perhaps you should use the switch?\n")
        } else {
            prop = mean(tempState$Means <= 0.53) - mean(tempState$Means <= 0.47)
            if(prop >= 0.935 && prop <= 0.965){
                cat(paste(100*prop,"percent of the dots are within the dotted lines.\n The door opens and you escape into the outside world.\n You win! (trumpets)\n"))
                plot(0:1,0:1,type="n",xlab="",ylab="",xaxt="n",yaxt="n")
                text(0.5,0.5,"You win!",cex=5)
            } else {
                cat(paste(100*prop,"percent of the dots are within the dotted lines.\n You need 95%. Keep trying!\n"))
            }
        }
	} else if(is.null(obj)){
		
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

open = function(obj = "Door"){
    if(obj == "Door"){
        use("Door")
    } else {
        print("You can't open that.")
    }
}

endGame = function(){
    rm(myE)
    dev.off()
}




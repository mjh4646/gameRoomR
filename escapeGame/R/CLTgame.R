#### Interpreter escape game package: The CLT room
#### Mike Higgins & Brad Luen
#### Last modified 8/3/14

#### This function will eventually be used to 
#### Update which location we are in.
updateInterpreter = function(gameState){
	
	#Update First Room
	#The init is helpful to determine if we have initialized a given location
	if(is.null(gameState$FirstRoom$Init)){
		
		# ID entry
		cat("Welcome to the Statistical Teaching and Proficiency System (STAPS).\nPlease enter your student ID and press enter.\n")
		gameState$SID = as.numeric(readline(prompt = "Student ID: "))
		cat("You are in a small room. Your task is to escape.\nTo view the room, type look().\nTo look at an object, type look(\"OBJECT NAME\")\nwhere \"OBJECT NAME\" is replaced with the name of the object.\nTo use an object, type use(\"OBJECT NAME\").\n")
		
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
            hist(gameState$Data, main=paste("Sample number", length(gameState$Means)), xlab="")
            print(paste("The screen shows your current sample."))
        } else {
            hist(gameState$Means, main=paste("Means of", length(gameState$Means), "samples"), xlab="Sample mean")
            print(paste("The screen shows the distribution of sample means."))
        }
	}

	else if(obj == "buttonFirstRoom"){
		cat("There is a panel with one button on it.\nAn inscription reads: 'Press to generate a sample from an exponential distribution.'\nTo press the button, type use(\"Button\").\n")
	}
	
	else if(obj == "doorFirstRoom"){
		cat("This is the door to the outside world.  It is currently closed.\nA panel reads:\n 'TO OPEN, DISPLAY AN (APPROXIMATELY) NORMAL DISTRIBUTION ON THE SCREEN.'\n")
	}	
	else if(obj == "switchFirstRoom"){
        tempState = get("gameState", myE)
        if(tempState$Switch==FALSE){
            cat("There is a switch with two settings: Data and Means.\nIt is currently set to Data.\nTo flick the switch, type use(\"Switch\").\n")
        }
        if(tempState$Switch==TRUE){
            cat("There is a switch with two settings: Data and Means.\nIt is currently set to Means.\nTo flick the switch, type use(\"Switch\").\n")
        }
	}
	else{
		print(paste("The object ", obj, " cannot be viewed while in this room."))
	}
	
	if(obj == "SecondRoom"){
		print("How did you get here? We haven't even made this room yet. Are you a ghost? If so, that is cool.")
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

useInterpreter = function(gameState,obj,rep=1){
	myObj = getObjName(gameState,obj)
    tempState = get("gameState", myE)
	if(obj == "Button"){
 		if(rep < 2){
			tempState$Data = rexp(100)
			tempState$Means = c(tempState$Means, mean(tempState$Data))
			cat(paste("You pushed the button. \nOne hundred data points fly across the room and into the screen.\n Their mean is", round(mean(tempState$Data),3),".\n"))
		} else {
			rep = floor(rep)
			temp_results = matrix(rexp(100*rep),100,rep)
			temp_means = apply(temp_results,2,"mean")
			tempState$Data = temp_results[,rep]
			tempState$Means = c(tempState$Means, temp_means)
			cat(paste("You pushed the button",rep,"times. \nEach time, one hundred data points fly across the room and into the screen.\n The sample means are:", round(mean(temp_means),3),".\n"))
		}
		assign("gameState",tempState,myE)
		look("Screen")			
	} else if(obj == "Switch"){
        if(tempState$Switch == FALSE){
            tempState$Switch = TRUE
            print("You flick the switch to the setting 'Means'.")
        } else if(tempState$Switch == TRUE){
            tempState$Switch = FALSE
            print("You flick the switch to the setting 'Data'.")
        }
		assign("gameState",tempState,myE)
        look("Screen")
    } else if(obj == "Door"){
        if(tempState$Switch==FALSE){
            print("Looks like an exponential distribution to me.")
        } else if(length(tempState$Means) < 5){
            print("The door doesn't open. You're going to need a lot more samples.")
        } else if(shapiro.test(tempState$Means)$stat > 0.95){
            cat(paste("The distribution of sample means is approximately normal.\n The door opens and you escape into the outside world. You win! (trumpets)\n"))
            plot(0:1,0:1,type="n",xlab="",ylab="",xaxt="n",yaxt="n")
            text(0.5,0.5,"You win!",cex=4)
			code = tempState$SID %% 23456
			cat(paste("Your completion code is:",code,"\n"))
        } else {
            print("Hmmm, can't be sure it's normal yet. Try taking a few more samples.")
			if(length(tempState$Means) >= 10){
				cat(paste("Psst... Here's a secret. Type use(\"Button\", 10) to use the button ten times.\n"))
			}
        }
	} else if(is.null(obj)){
		cat(paste("To use an object, type use(\"NAME\"), where NAME is the name of the object.\n"))
	}
	else{
		print(paste("You cannot use the object ", obj))
	}
}

#enterInterpreter = function(gameState,number){
#	unlock(gameState, number)
#}

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




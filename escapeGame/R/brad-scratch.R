### Scratch 7/30/14

useInterpreter = function(gameState,obj){
	myObj = getObjName(gameState,obj)
	if(obj == "Button"){
        tempState = get("gameState", myE)
        tempState$Data = rexp(100)
        print(paste("You pushed the button. One hundred data points come flying out. Their mean is", round(mean(tempState$Data),3)))
        tempState$Means = c(tempState$Means, mean(tempState$Data))
        assign("gameState",tempState,myE)
	} else if(obj == "Switch"){
        tempState = get("gameState", myE)
        if(tempState$Switch == FALSE){
            tempState$Switch = TRUE
            print("You flick the switch to the setting 'Means'.")
            assign("gameState",tempState,myE)
        } else {
            tempState$Switch = FALSE
            print("You flick the switch to the setting 'Data'.")
            assign("gameState",tempState,myE)
        }
	} else if(is.null(obj)){
		
	}
	else{
		print(paste("You cannot use the object ", obj))
	}
}


### Old scratch



gameState = list()
gameState$Location = "Lobby"
gameState$BackDoorLocked = TRUE

look = function(){
	print(paste("You are in the", gameState$Location, "."))
}

# go function: gets correct movement function based on location
go = function(Place, gameState=gameState){
	if(gameState$Location == "Lobby"){
		goLobby(Place, gameState)
	} else if(gameState$Location == "CLTRoom"){
		goCLTRoom(Place, gameState)
	} else if(gameState$Location == "Outside"){
		goOutside(Place, gameState)
	}
}

# Lobby functions

goLobby = function(Place, gameState=gameState){
	if(Place == "CLTRoom"){
		gameState$Location = "CLTRoom"
	} else{print("You can't go there right now.")}
}

# CLT Room functions

goCLTRoom = function(Place, gameState=gameState){
	if(Place == "Lobby"){
		gameState$Location = "Lobby"
	} else if(Place == "Outside"){
		if(gameState$BackDoorLocked == FALSE){
			gameState$Location = gameState$Outside
		} else {print("The door is locked!")}
	} else{print("You can't go there right now.")}
}

# Outside functions

goOutside = function(Place, gameState=gameState){
	if(Place == "CLTRoom"){
		gameState$Location = "CLTRoom"
	} else{print("You can't go there right now.")}
}





### Room structure

### room.names = c("Lobby", "101", "Outside")

### This is hard to edit
### Better to set up a list of exits for each room?
# door.matrix = matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow=T)
# open.door.matrix = matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), 3, 3, byrow=T)


initInterpreter = function(){
	
	myE = new.env()
	Lobby = new.env()
	CLTRoom = new.env()
	Outside = new.env()
	
	#for(i in 1:length(roomNames)){
	#	tempEnv = new.env()
	#	sys.source(paste(c(roomNames[i],".R"),collapse = ""),envir = tempEnv)
	#	assign(tempEnv,roomNames[i],roomEnv)		
	#}
	
	#Initialize gameState
	gameState = list()
	gameState$roomEnv = myE
	gameState$Location = "Lobby"
	gameState$Inventory = NULL
	#gameState$Data = c(1,2,3,4,5)
	#gameState$Password = mean(gameState$Data)
	
	#Initialize Status
	# gameState = updateInterpreter(gameState)

	#Output gameState
	gameState	

look.Lobby = function(obj){
	print("You're in the Lobby. An open door leads to the next room. There's a sign above the door.")
}
assign("look.local",look.Lobby,Lobby)
move.Lobby = function(obj){
	if(obj == "Door"){
		roomID = "CLTRoom"
		print("You go through the door and enter the next room. You see a weird dude with a pointy hat.")
	}
}
assign("move.local",move.Lobby,Lobby)


look.CLT = function(obj){
	if(obj == "Wizard"){print("You stare at the AbNormal Wizard. It's hard to describe how he looks, but it's definitely not bell-shaped.")
	} else if (obj == "Door"){print("You see two doors. One door leads back to the lobby. The other is blocked by a wizard who looks more than a little skewed.")
	} else{print("You see two doors. One door leads back to the lobby. The other is blocked by a wizard who looks more than a little skewed.")}
}
assign("look.local",look.CLT,CLTRoom)
move.CLT = function(obj){
	if(obj == "Outside"){
		if(unlocked == T){
			roomID = "Outside"
    			print("The door opens, and you walk through. You are free. You're blinded by tears of joy, which prevent you from seeing the bus that crashes into you.")
		}
	}
}
assign("move.local",move.CLT,CLTRoom)


look.Outside = function(obj){
	print("You are outside. You've won the game. Go play Minecraft or something.")
}
assign("look.local",look.Outside,Outside)
move.Outside = function(obj){
	if(obj == "Inside"){
		roomID = "CLTRoom"
    		print("You go back inside for some reason. The NormalWizard stares at you grumpily.")
	}
}
assign("move.local",move.Outside,Outside)

}

lookInterpreter = function(obj){
	getRoom = get(roomID, envir = myE)
	aRoom = get("look.local", getRoom)
	aRoom(obj)
}
moveInterpreter = function(obj){
	getRoom = get(roomID, envir = myE)
	aRoom = get("move.local", getRoom)
	aRoom(obj)
}

































look.local = function(){
	print("They're shooting... Made you look.")
}



assign("look.local",look.local,Lobby)
assign("Lobby",Lobby,myE)
look.local = function(){
	print("You're in the Lobby.")
}
assign("tempBaby",tempBaby,CLTRoom)
assign("CLTRoom",CLTRoom,myE)
rm(look.local)

look = function(roomID){
	getRoom = get(roomID,envir = myE)
	aRoom = get("look.local",getRoom)
	aRoom()
}
go = function(roomID){
	getRoom = get(roomID,envir = myE)
	aRoom = get("move.local",getRoom)
	aRoom()
}


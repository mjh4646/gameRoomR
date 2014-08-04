library(maps)

#### This is the game interface for R
#### All games created will use this interface
#### Created by Mike Higgins
#### Modified by Brad Luen 8/3/14
####  
#### Game interface needs the following:
#### A gameState list object.
####     The gameState list object has the following categories:
####     Inventory
####     Location
####
#### The following are the functions:
#### startGame: Starts the game.  Calls initInterpreter to initialize gameState.
#### saveGame: Stores the gameState into a .dat file (really a .txt)
#### loadGame: Loads a gameState file
#### instructions: loads the instructions help file!
#### inventory: views the inventory
#### checkGame: Checks to see if the gameState is loaded.  May return an error.
####
#### goUp: "You go up, then you go right back down.  Gravity is fascinating, isn't it?"
####       "for a list of directions, type ?goForward"
####
#### The following functions call the corresponding interpreter function
####    gameStart: Starts the game.  Calls initGameInterpreter
#### 	takeObj: Takes an object. Calls takeInterpreter
####    getObj:  Gets an object.  Same as takeObj
####    openObj: Opens an object. Calls openInterpreter
####    gameDat: Obtains data. Calls plotInterpreter
####    look: Looks.  Calls lookInterpreter.  
####		  If a null parameter, makes a plot or types a description.
####          If an object parameter, interpreted by lookObjInterpreter
####    lookObj, lookAt, lookAtObj: See look.
####    useObj: Uses an object. Calls useInterpreter
####    talk: Talks to a person.  Calls talkInterpreter

#Create an environment.  
#This will hide the gameState variable.
myE = new.env() #Clunky?

#Start game functions
startGame = function(roomID = 0){
	print("Welcome to the Statistical Teaching and Proficiency System (STAPS).")
	if(roomID == 0){
		cat(paste("You are in a hallway with two doors. They are labeled LLNgame and CLTgame.\n Which of the two doors do you enter?\n"))
		roomID = readline(prompt = "Door: ")
	}	
	#Ignore this
	# myGS =  mget("gameState", envir = myE, ifnotfound = "NotFound")
	# if(myGS$gameState == "NotFound"){
		# gameState = initInterpreter()
		# assign("gameState",gameState, myE)
	# }
	
	#Use InitInterpreter to initialize the gameState
	gameState = initInterpreter(roomID)
	assign("gameState",gameState, myE)
}

startgame = function(roomID = 0){
	startGame(roomID)
}

#Checks to see if a game is loaded or not.
#Ignore for now
# checkGame = function(){
	# myGS =  mget("gameState", envir = myE, ifnotfound = "NotFound")
	# if(myGS == "NotFound"){
		# stop("You need to either start a game or load a game\nbefore 
		  # you are able to use this command.\nType instructions() for more details.")
	# }
# }

#Ignore for now
#loadGame = function(filename){
#	print(filename)
#}

endGame = function(){
    #rm(myE)
    dev.off()
}

endgame = function(){
	endGame()
}

instructions = function(){
	?instructions
}

inventory = function(){
	gameState = get("gameState", myE)
	print(gameState$inventory)
}

#Use functions
use = function(obj = NULL, rep = 1){
	gameState = get("gameState", myE)
	useInterpreter(gameState, obj, rep)
}

useobj = function(obj = NULL, rep = 1){
	use(obj)
}

useObj = function(obj = NULL, rep = 1){
	use(obj)
}

enter = function(number = NULL){
	gameState = get("gameState", myE)
	enterInterpreter(gameState, number)
}

#Take functions
take = function(obj = NULL){
	gameState = get("gameState", myE)
	takeInterpreter(gameState, obj)
}

takeObj = function(obj = NULL){
	take(obj)	
}

takeobj = function(obj = NULL){
	take(obj)	
}

getobj = function(obj = NULL){
	take(obj)	
}

getObj = function(obj = NULL){
	take(obj)	
}

#Look functions
look = function(obj = NULL){
	gameState = get("gameState", myE)
	lookInterpreter(gameState, obj)
}

lookat = function(obj = NULL){
	look(obj)
}

lookAt = function(obj = NULL){
	look(obj)
}

lookobj = function(obj = NULL){
	look(obj)
}

lookatObj = function(obj = NULL){
	look(obj)
}

lookAtobj = function(obj = NULL){
	look(obj)
}

lookatobj = function(obj = NULL){
	look(obj)
}

lookObj = function(obj = NULL){
	look(obj)
}

lookAtObj = function(obj = NULL){
	look(obj)
}

#data functions
gameData = function(obj = NULL){
	gameState = get("gameState", myE)
	dataInterpreter(gameState, obj)
}

gamedata = function(obj = NULL){
	gameData(obj)
}

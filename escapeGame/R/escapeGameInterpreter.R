#### EACH ROOM NEEDS TO HAVE THE FOLLOWING FUNCTIONS:
#### update
#### look
#### getObj
#### use
#### move

#### This is the interpreter escape game package. 
#### Created by Mike Higgins
#### Modified by Brad Luen 5/14/14

#### This function will eventually be used to 
#### Update which location we are in.

#### Linear rooms or...?

#Initialize the game state upon startGame
#roomNames is a set of room names, with the initial room listed first.
initInterpreter = function(){
	
	roomEnv = new.env()
	roomNames = c("FirstRoom","escapeGame-CLT-room","ThirdRoom")
	
	for(i in 1:roomNames()){
		tempEnv = new.env()
		sys.source(paste(c(roomNames[i],".R"),collapse = ""),envir = tempEnv)
		assign(tempEnv,roomNames[i],roomEnv)		
	}
	
	#Initialize gameState
	gameState = list()
	gameState$roomEnv = roomEnv
	gameState$Location = roomNames[1]
	gameState$Inventory = NULL
	#gameState$Data = c(1,2,3,4,5)
	#gameState$Password = mean(gameState$Data)
	
	#Initialize Status
	gameState = updateInterpreter(gameState)

	#Output gameState
	gameState	
}

updateInterpreter = function(gameState){
	
	#Get the update function from the state space that corresponds
	#to the current location
	tempEnv = get(gameState$Location,gameState$roomEnv)
	updateFun = get("update",tempEnv)
	
	#Update status
	gameState = updateFun(gameState)
	gameState
}

lookInterpreter = function(gameState, obj){
	
	#Get the update function from the state space that corresponds
	#to the current location
	tempEnv = get(gameState$Location,gameState$roomEnv)
	lookFun = get("look", tempEnv)
	
	#Look
	lookFun(gameState, obj)
}

getInterpreter = function(gameState,obj){
	
	#Get the update function from the state space that corresponds
	#to the current location
	tempEnv = get(gameState$Location,gameState$roomEnv)
	getFun = get("getObj", tempEnv)
	
	#Get
	gameState = getFun(gameState, obj)
	gameState = updateInterpreter(gameState)
	gameState 
}

useInterpreter = function(gameState,obj){
	
	#Get the update function from the state space that corresponds
	#to the current location
	tempEnv = get(gameState$Location,gameState$roomEnv)
	useFun = get("use", tempEnv)
	
	#Use
	gameState = useFun(gameState, obj)
	gameState = updateInterpreter(gameState)
	gameState 
}

moveInterpreter = function(gameState, direction){
	#Get the update function from the state space that corresponds
	#to the current location
	tempEnv = get(gameState$Location,gameState$roomEnv)
	moveFun = get("move", tempEnv)
	
	#Move
	gameState = moveFun(gameState, obj)
	gameState = updateInterpreter(gameState)
	gameState
}

# getLook = function(gameState, obj){

	# if(obj == "FirstRoom"){
		# myMap = list()
		# myMap$x = c(0,3,17,20,0,NA,
				    # 0,0,3,3,NA,
				    # 20,20,17,17,NA,
				    # 3,3,17,17,NA,
				    # 5,5,9,9,NA,
				    # 11,11,16,16,11,NA,
				    # 12,12,15.5,15.5)
		# myMap$y = c(0,4,4,0,0,NA,
				    # 0,20,20,4,NA,
				    # 0,20,20,4,NA,
				    # 4,20,20,4,NA,
				    # 4,15,15,4,NA,
				    # 13,18,18,13,13,NA,
				    # 4,8,8,4)
		# myMap$range = c(0,20,0,20)
		# myMap$names = c("floor","leftWall","rightWall","backWall",
			            # "door","picture","safeFront")
		# myCols =      c("tan", "lightblue","lightblue","lightblue",
					    # "white", "white","olivedrab")
		# map(myMap, col = myCols, fill = TRUE)
		# points(c(8,8,14.75,14.75),c(9,9,5.75,5.75), pch = c(1,19,15,15),
			# cex = c(1.75,1.7,1.5,1), col = c("black","gold","white","black"))
		# points(c(12,13.5,15),c(15.5,14.5,17),pch = c("x","x","?"))
		# segments(11,14.5,16,16.5)
		# text(c(7,13.5,13.75),c(16,19,9),c("Door", "Picture","Safe"))
		
		# cat("In front of you, you see a door, a picture, and a safe.\n")
	# }
	
	# else if(obj == "pictureFirstRoom"){
        # x = c(1,2,3,4,5)
        # plot(x,rep(0,length(x)),type="n",xlab="",ylab="",axes=F)
        # text(x,rep(0,length(x)),x,cex=4)
	# }

	# else if(obj == "safeFirstRoom"){
		# cat("It is a safe.\nThere is a screen that allows you to input a number.\nNext to the screen, an inscription says 'ENTER THE MEAN.'\nInputting the correct number will unlock the safe\nIt is not the most secure safe in the world.\nType enter(NUMBER) to enter a number.\n")
	# }
	
	# else if(obj == "doorFirstRoom"){
		
		# if(gameState$FirstRoom$DoorUnlocked){
			# cat("This is the door to the outside world.  It is currently unlocked.")
		# }
		# else{
			# cat("This is the door to the outside world.  It is currently locked.")
		# }
	# }	
	# else{
		# print(paste("The object ", obj, " cannot be viewed while in this room."))
	# }
	
	# if(obj == "Second Room"){
		# print("How did you get here? We haven't even made this room yet.")
	# }
# }

# lookInterpreter = function(gameState,obj){
	# myObj = getObjName(gameState,obj)
	# getLook(gameState, myObj)
# }

# #Initialize the game state upon startGame
# initInterpreter = function(){
	
	# #Initialize gameState
	# gameState = list()
	# gameState$Location = "FirstRoom"
	# gameState$Inventory = NULL
	# gameState$Data = c(1,2,3,4,5)
	# gameState$Password = mean(gameState$Data)
	
	# #Initialize Status
	# gameState = updateInterpreter(gameState)

	# #Output gameState
	# gameState	
# }

# getLook = function(gameState, obj){

	# if(obj == "FirstRoom"){
		# myMap = list()
		# myMap$x = c(0,3,17,20,0,NA,
				    # 0,0,3,3,NA,
				    # 20,20,17,17,NA,
				    # 3,3,17,17,NA,
				    # 5,5,9,9,NA,
				    # 11,11,16,16,11,NA,
				    # 12,12,15.5,15.5)
		# myMap$y = c(0,4,4,0,0,NA,
				    # 0,20,20,4,NA,
				    # 0,20,20,4,NA,
				    # 4,20,20,4,NA,
				    # 4,15,15,4,NA,
				    # 13,18,18,13,13,NA,
				    # 4,8,8,4)
		# myMap$range = c(0,20,0,20)
		# myMap$names = c("floor","leftWall","rightWall","backWall",
			            # "door","picture","safeFront")
		# myCols =      c("tan", "lightblue","lightblue","lightblue",
					    # "white", "white","olivedrab")
		# map(myMap, col = myCols, fill = TRUE)
		# points(c(8,8,14.75,14.75),c(9,9,5.75,5.75), pch = c(1,19,15,15),
			# cex = c(1.75,1.7,1.5,1), col = c("black","gold","white","black"))
		# points(c(12,13.5,15),c(15.5,14.5,17),pch = c("x","x","?"))
		# segments(11,14.5,16,16.5)
		# text(c(7,13.5,13.75),c(16,19,9),c("Door", "Picture","Safe"))
		
		# cat("In front of you, you see a door, a picture, and a safe.\n")
	# }
	
	# else if(obj == "pictureFirstRoom"){
        # x = c(1,2,3,4,5)
        # plot(x,rep(0,length(x)),type="n",xlab="",ylab="",axes=F)
        # text(x,rep(0,length(x)),x,cex=4)
	# }

	# else if(obj == "safeFirstRoom"){
		# cat("It is a safe.\nThere is a screen that allows you to input a number.\nNext to the screen, an inscription says 'ENTER THE MEAN.'\nInputting the correct number will unlock the safe\nIt is not the most secure safe in the world.\nType enter(NUMBER) to enter a number.\n")
	# }
	
	# else if(obj == "doorFirstRoom"){
		
		# if(gameState$FirstRoom$DoorUnlocked){
			# cat("This is the door to the outside world.  It is currently unlocked.")
		# }
		# else{
			# cat("This is the door to the outside world.  It is currently locked.")
		# }
	# }	
	# else{
		# print(paste("The object ", obj, " cannot be viewed while in this room."))
	# }
	
	# if(obj == "Second Room"){
		# print("How did you get here? We haven't even made this room yet.")
	# }
# }

# unlock = function(gameState,number){
    # if(gameState$Location == "FirstRoom" && gameState$Password == number){
	    # gameState$Location == "Second Room"
		# # Generate second room
    	# print("The door opens, and you walk through. Oh great, it's another locked room.")
    # }
    # if(gameState$Location== "LastRoom" && gameState$Password == number){
    	# print("The door opens, and you walk through. You are free. You're blinded by tears of joy, which prevent you from seeing the bus that crashes into you.")
    # }
    # else{
    	# print("A sad trombone plays. Try again.")
    # }
# }




# #Used to standardize object names.
# getObjName = function(gameState,obj){
	# name = NULL	
	# if(is.null(obj)){
		# name = gameState$Location
	# }
	# else{	
		# #Check if object is a character
		# if(!is.character(obj)){
			# stop("Object in function must be in quotes.\nSee instructions() for details.")
		# }	
		# else{
			# print(obj)
			# obj = tolower(obj)
			# name = paste(c(obj,gameState$Location),collapse = "")
		# }
	# }
# }

# useInterpreter = function(gameState,obj){
	# myObj = getObjName(gameState,obj)
	# if(is.null(obj)){
		
	# }
	
	# else{
		# print(paste("You cannot use the object ", obj))
	# }
# }

# enterInterpreter = function(gameState,number){
	# unlock(gameState, number)
# }



# #Update for first room
# updateInterpreter = function(gameState){
	
	# #Update First Room
	# #The init is helpful to determine if we have initialized a given location
	# if(is.null(gameState$FirstRoom$Init)){
		
		# #Need to check if door is locked or unlocked
		# gameState$FirstRoom$DoorUnlocked = FALSE
		
		# #Need to check to see if the safe is locked or unlocked
		# gameState$FirstRoom$SafeUnlocked = FALSE
		
		# #Some intro words:
		# cat("Welcome!  This is a beta version of the\nStatistical Testing and Proficiency System (STAPS).\nPlease enter your student ID and press enter.\n")
		# gameState$SID = readline(prompt = "Student ID: ")

		# cat("\nGreat! Let's get started.\nYou are in a room.  To view the room, type look(). To look at individual objects, type look(\"OBJECT NAME\")\nwhere \"OBJECT NAME\" is replaced with the name of the object of interest.\nTo take objects, type take(\"OBJECT NAME\").\nTo download available data, type gameData()\n")
		
		# #We initialized the first room.  
		# #So we set this equal to true
		# gameState$FirstRoom$Init = TRUE
	# }
	# gameState
# }

# getLook = function(gameState, obj){

	# if(obj == "FirstRoom"){
		# myMap = list()
		# myMap$x = c(0,3,17,20,0,NA,
				    # 0,0,3,3,NA,
				    # 20,20,17,17,NA,
				    # 3,3,17,17,NA,
				    # 5,5,9,9,NA,
				    # 11,11,16,16,11,NA,
				    # 12,12,15.5,15.5)
		# myMap$y = c(0,4,4,0,0,NA,
				    # 0,20,20,4,NA,
				    # 0,20,20,4,NA,
				    # 4,20,20,4,NA,
				    # 4,15,15,4,NA,
				    # 13,18,18,13,13,NA,
				    # 4,8,8,4)
		# myMap$range = c(0,20,0,20)
		# myMap$names = c("floor","leftWall","rightWall","backWall",
			            # "door","picture","safeFront")
		# myCols =      c("tan", "lightblue","lightblue","lightblue",
					    # "white", "white","olivedrab")
		# map(myMap, col = myCols, fill = TRUE)
		# points(c(8,8,14.75,14.75),c(9,9,5.75,5.75), pch = c(1,19,15,15),
			# cex = c(1.75,1.7,1.5,1), col = c("black","gold","white","black"))
		# points(c(12,13.5,15),c(15.5,14.5,17),pch = c("x","x","?"))
		# segments(11,14.5,16,16.5)
		# text(c(7,13.5,13.75),c(16,19,9),c("Door", "Picture","Safe"))
		
		# cat("In front of you, you see a door, a picture, and a safe.\n")
	# }
	
	# else if(obj == "pictureFirstRoom"){
        # x = c(1,2,3,4,5)
        # plot(x,rep(0,length(x)),type="n",xlab="",ylab="",axes=F)
        # text(x,rep(0,length(x)),x,cex=4)
	# }

	# else if(obj == "safeFirstRoom"){
		# cat("It is a safe.\nThere is a screen that allows you to input a number.\nNext to the screen, an inscription says 'ENTER THE MEAN.'\nInputting the correct number will unlock the safe\nIt is not the most secure safe in the world.\nType enter(NUMBER) to enter a number.\n")
	# }
	
	# else if(obj == "doorFirstRoom"){
		
		# if(gameState$FirstRoom$DoorUnlocked){
			# cat("This is the door to the outside world.  It is currently unlocked.")
		# }
		# else{
			# cat("This is the door to the outside world.  It is currently locked.")
		# }
	# }	
	# else{
		# print(paste("The object ", obj, " cannot be viewed while in this room."))
	# }
	
	# if(obj == "Second Room"){
		# print("How did you get here? We haven't even made this room yet.")
	# }
# }
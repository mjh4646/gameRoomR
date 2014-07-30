myE = new.env()
FirstRoom = new.env()
SecondRoom = new.env()
tempBaby = function(){
	print("You've entered tempBaby")
}
assign("tempBaby",tempBaby,FirstRoom)
assign("FirstRoom",FirstRoom,myE)
tempBaby = function(){
	print("You've entered pimpBaby")
}
assign("tempBaby",tempBaby,SecondRoom)
assign("SecondRoom",SecondRoom,myE)
rm(tempBaby)

permBaby = function(roomID){
	getRoom = get(roomID,envir = myE)
	aRoom = get("tempBaby",getRoom)
	aRoom()
}

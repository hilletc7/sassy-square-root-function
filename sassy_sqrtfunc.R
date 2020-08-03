sassysqrt <- function(x, tol=.000001, verbose=FALSE) {
  sass <- 
  myguess=10
	x <- if(x < 0){return("Stop! You have violated the law. This function will not allow you to take the square root of a negative number")} else if(x==0) {return(0)} else{x} 
	newguess <- if(myguess < 0){return("Stop! That's illegal. The square root of a number cannot be negative")} else{myguess}  
	approxy <- x/newguess 
	newguess <- (newguess + approxy)/2
	error <- newguess-approxy
	error <- if(error < 0) {error*-1} else if (error == 0){return(myguess)} else{error} 
	error < tol
	if(verbose==TRUE) {print(paste("error:", error))} 
	if(verbose==TRUE) {print(paste("approximate y:", approxy))} 
	if(verbose==TRUE) {print(paste("guess Y:", newguess))} 

	while(error > tol) {
		approxy <- x/newguess
		newguess <- (newguess + approxy)/2 
		error <- newguess-approxy
		if(verbose==TRUE) {print(paste("error:", error))}
		if(verbose==TRUE) {print(paste("approximate y:", approxy))}
		if(verbose==TRUE) {print(paste("guess Y:", newguess))} 
}
	if(verbose==TRUE) {return(paste("final answer:", round(newguess, 3)))} else{return(newguess)} 
}

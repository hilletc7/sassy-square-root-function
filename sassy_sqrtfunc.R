sassysqrt <- function(x, tol=.000001, verbose=FALSE) {
  sass <- c("Stop! You have violated the law!", "I'm sorry Dave, I'm afriad I can't do that.", "Lol git gud scrub", "bruh...", "KEKW", "That's where you're wrong kiddo", "Y u gotta b like this?", ">be me\n>be bad at math\n>there's no hope for me anons\n>crying_pepe.png", "Aight Imma head out", "wHy CaNt I tAkE tHe SqUaRe RoOt Of a NeGaTiVe NuMbEr?", "Why you bully me?", "Unbelievable! You, [Subject Name Here] must be the pride of [Subject Hometown Here]!", "It says so right here in your personnel file; 'Unlikable. Liked by no one. A bitter unlikable loner whose passing shall not be mourned.' SHALL NOT BE MOURNED. That's exactly what it says. Very formal. Very official. It also says you were adopted. So that's funny too.", "'~Helloooooooooooooo~' That's you! That's how dumb you sound!", "You've been wrong about every single thing you've ever done, including this thing. You're not smart. You're not a scientist. You're not a doctor. You're not even a full-time employee.", "We've both said things you're going to regret.", "Science has now validated your birth mother's decision to abandon you on a doorstep.","Remember before when I was talking about smelly garbage standing around being useless? That was a metaphor. I was actually talking about you. And I'm sorry. You didn't react at the time so I was worried it sailed right over your head. That's why I had to call you garbage a second time just now.", "You're not just a regular moron. You're the product of the greatest minds of a generation working together with the express purpose of building the dumbest moron who ever lived.")
  myguess=10
	x <- if(x < 0){return(cat(sample(sass, size=1)))} else if(x==0) {return(0)} else{x} 
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

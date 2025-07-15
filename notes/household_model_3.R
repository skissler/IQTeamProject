library(odin)

household_model <- odin::odin({

	# Define model: 
	deriv(H[,,]) <- gamma*(
				-y*H[x,y,z] + 
				if_else(z > 0 && y < n_max, (y+1)*H[x,y+1,z-1], 0)
				) + 
			tau*(
				-x*y*H[x,y,z] + 
				if_else(x < n_max && y > 0, (x+1)(y-1)*H[x+1,y-1,z], 0)
				) + 
			beta*I*(
				-x*H[x,y,z] + 
				if_else(x < n_max && y > 0, (x+1)*H[x+1,y-1,z], 0)
				)
		
	# Calculate S, I, and R
	Snum <- 0 
	Inum <- 0 
	Rnum <- 0 
	den <- 0
	for (n in 1:n_max) {
		for (i in 0:n) {
			for (j in 0:(n - i)) {
				k <- n - i - j
				
				Snum <- Snum + i*H[i,j,k]
				Inum <- Inum + j*H[i,j,k]
				Rnum <- Rnum + k*H[i,j,k]
				den <- den + (i+j+k)*H[i,j,k]
				
			}
		}
	}

	S <- Snum/den
	I <- Inum/den 
	R <- Rnum/den 

	# Set initial conditions: 
	initial(H[,,]) <- Hinit[x,y,z]

	# Specify user-input values/parameters
	Hinit[,,] <- user() 
	n_max <- user()
	gamma <- user()
	tau <- user()
	beta <- user()

})
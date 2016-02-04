
# list exponential sequences up to iteration i
exp_seq <- function( x , i )
{
	r <- x
	for( j in 1:i )
	{
		 r <- c( r , list( c(r[[j]] , sum(r[[j]]) ) ) ) 
	}
	return(r)
}

# list fibonachi sequences up to iteration i
fibo_seq <- function( x , i )
{
	r <- list(x)
	for( j in 1:i )
	{
		 r <- c( r , list( c( r[[j]] , r[[j]][j] + r[[j]][j+1] ) ) ) 
	}
	return(r)
}

# combine results of two affine operations to single vector
# R1 - linear operation 1
# C1 - traslation 1
# R2 - linear operation 2
# C2 - translation 2
fractal_op <- function( x , R1 , C1 , R2 , C2 )
{
	r <- c( R1 * x + C1 , R2 * x + C2 )
	return( r )
}

# iterate two operation process described above
fractal_gen <- function( x , R1 , C1 , R2 , C2 , n )
{
	r <- x
	for( i in 1:n )
	{
		r <- fractal_op( r , R1 , C1 , R2 , C2 )
	}

	return( r )
}

# constants for generating symmetric dragon fractal
dragon_R <- (sqrt(2) / 2) * exp( complex( r = 0 , i = pi/4 ) )
dragon_C1 = complex( r = 0.5 , i = -0.5 )
dragon_C2 = complex( r = -0.5 , i = 0.5 )

dragon_fractal <- function( n )
{
	return( fractal_gen( complex(r = 0,i = 0) , dragon_R , dragon_C1 , dragon_R , dragon_C2 , n ) )
}
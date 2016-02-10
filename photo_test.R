library(shiny)
library(png)

# load the image
cheetah <- readPNG( "cheetah.png" , native = FALSE , info = FALSE )

# make it monochrome matrix
cheetah_mono <- t( cheetah[ , ,1] + cheetah[ , ,2] + cheetah[ , ,3] )

# blur the image by taking mean over neighbouring pixels
# s_image - source monochrome image as a matrix
# s - size of the blurring square
mean_blur <- function( s_image , s )
{
	n <- nrow( s_image )
	m <- ncol( s_image )
	d_image <- matrix( 0 , n - s , m - s )

	for( i in 1:(n-s) )
	{
		for( j in 1:(m-s) )
		{
			d_image[i,m-s-j] <- mean( s_image[ i:(i+s) , j:(j+s) ] )
		}
	}

	return( d_image )
} 

# blur the image by taking median over neighbouring pixels
# s_image - source monochrome image as a matrix
# s - size of the blurring square
median_blur <- function( s_image , s )
{
	n <- nrow( s_image )
	m <- ncol( s_image )
	d_image <- matrix( 0 , n - s , m - s )

	for( i in 1:(n-s) )
	{
		for( j in 1:(m-s) )
		{
			d_image[i,m-s-j] <- median( s_image[ i:(i+s) , j:(j+s) ] )
		}
	}

	return( d_image )
} 

ui <- fluidPage( sliderInput( inputId = "mean_range" ,
			label = "Size of the sub image", 
			value = 1 , min = 1, max = 10 ) ,
			plotOutput( outputId = "output_mean" , height="512" , width="512" ) , 
			plotOutput( outputId = "output_median" , height="512" , width="512" )
 )


server <- function( input , output )
{
	output$output_median <- renderPlot( 
		{
			image( median_blur( cheetah_mono , input$mean_range ) )
		} )
	output$output_mean <- renderPlot( 
		{
			image( mean_blur( cheetah_mono , input$mean_range ) )
		} )
}

shinyApp( ui = ui , server = server )
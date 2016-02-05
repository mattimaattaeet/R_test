library(shiny)

source( "test2.R" )

rotation <- function( angle )
{
	return( exp( complex( r = 0 , i = pi * angle / 180 ) ) )
}

ui <- fluidPage( sliderInput( inputId = "angle" ,
			label = "Angle of rotation", 
			value = 45 , min = 0, max = 180 ) ,
			sliderInput( inputId = "iterations" ,
			label = "Number of iterations", 
			value = 10 , min = 1, max = 20 ) ,
			plotOutput( outputId = "fractal" , height="512" , width="512" ) );

server <- function( input , output )
{
	output$fractal <- renderPlot( 
		{
			R = (sqrt(2) / 2) * rotation( input$angle )
			C1 = Conj( R )
			C2 = -C1
			plot( fractal_gen( complex( r = 0, i = 0 ) , R , C1 , R , C2 , input$iterations ) )
		} )
}

shinyApp( ui = ui , server = server )

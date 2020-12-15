\name{NutrienTrackeRapp}
\alias{NutrienTrackeRapp}
\title{Starts shiny app for NutrienTrackeR}
\description{
This function starts the shiny app for NutrienTrackeR running locally.
Personal data and the database of choice should be input in the side panel.
Diet should be input in the "Diet input" tab, with one food per line for each
day and separating with a semicolon (;) the food name and the eaten amount (in 
grams). Food names should match those found in the chosen database.
A quick assessment of the diet for a 24-h period can be performed from the
"One-day diet fast assessment" tab.
}
\usage{
NutrienTrackeRapp()
}
\examples{
## Start the NutrienTrackeR shiny app
if(interactive()) {
NutrienTrackeRapp()
}
}

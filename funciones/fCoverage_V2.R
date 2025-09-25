## funcion fCoverage

#' Autor: Rodrigo Lagos
# Fecha: 29-06-27

#Determina el estimador del coverage 

#Inputs:
#  vQ : vector con valores de Q
# U   : cantidad total del features en la muestra 
# nT  : canrtidad de participantes (trampas)

#Outputs:
#  list con dos valores
# CV : estimador del coverage de la muestra



fCoverage_V2 <- function(vQ,nT)
{
  
  nK <- as.numeric(gsub("Q_", "",names(vQ)))
  
  U <- sum(vQ * nK)
  
  
  CV <- 1 - vQ[1]/U * ((vQ[1]*(nT-1)) / (vQ[1]*(nT-1) + 2 *vQ[2]) )
  
  CV <-unname(CV)
  
  return(CV)
  
}
calculatePossibleHalfLives <- function(minPumpFlow=0.4,
                                   maxPumpFlow=23.5,
                                   stepPumpFlow=0.1,
                                   Vcentral=700,
                                   Vcartridge=50,
                                   roundingDigits=2)
{
  possiblePumpFlow <- seq(minPumpFlow,maxPumpFlow,stepPumpFlow)
  possibleHalfLife <- ((log(2)*(Vcentral+Vcartridge))/possiblePumpFlow)/60
  return(unique(round(possibleHalfLife,roundingDigits)))
}
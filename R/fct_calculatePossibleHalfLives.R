calculatePossibleHalfLives <- function(minPumpFlow=0.4,
                                   maxPumpFlow=23.5,
                                   stepPumpFlow=0.1,
                                   Vcentral=700,
                                   Vcartridge=50,
                                   roundingDigits=2)
{
  step <- ifelse(stepPumpFlow <= 0, 0.1, stepPumpFlow)
  possiblePumpFlow <- seq(minPumpFlow,maxPumpFlow,step)
  possibleHalfLife <- ((log(2)*(Vcentral+Vcartridge))/possiblePumpFlow)/60
  return(unique(round(possibleHalfLife,roundingDigits)))
}
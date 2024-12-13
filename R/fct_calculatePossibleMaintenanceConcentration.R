calculatePossibleMaintenanceConcentration <- function(minPumpFlow=0.4,
                                       maxPumpFlow=23.5,
                                       stepPumpFlow=0.1,
                                       Vcentral=700,
                                       Vcartridge=50,
                                       Css = 1,
                                       halfLifeHours=7.12,
                                       roundingDigits=2)
{
  possiblePumpFlow <- seq(minPumpFlow,maxPumpFlow,stepPumpFlow)

  keHours <- log(2) / halfLifeHours
  rate_infuse <- keHours * Css * (Vcentral + Vcartridge)

  possibleMaintenanceConcentration <- rate_infuse/possiblePumpFlow

  return(unique(round(possibleMaintenanceConcentration,roundingDigits)))
}
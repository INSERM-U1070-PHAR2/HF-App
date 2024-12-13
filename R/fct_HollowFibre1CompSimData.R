HollowFibre1CompSimData  <- function(model,
                                     halfLifeHours,
                                  Vcentral,
                                  Vcartridge,
                                  initial_concentration,
                                  lastTimePointHours,
                                  drugName,
                                  VinjectBolus =  NA,
                                  dosingIntervalHoursBolus = NA,
                                  numberOfDosesBolus = NA,
                                  tinfuseHours = NA,
                                  VinjectInf =  NA,
                                  dosingIntervalHoursInf = NA,
                                  numberOfDosesInf = NA,
                                  adm.type,
                                  constantVolume = T,
                                  debit_central_cartridge,
                                  Css = NA,
                                  Cinfusemaintenance=NA)
{
  library(mrgsolve)
  library(dplyr)
  library(ggplot2)
  halfLifeMin <-   halfLifeHours * 60
  lastTimePointMin <- lastTimePointHours * 60

  clearance <-
    log(2) * (Vcentral + Vcartridge) / halfLifeMin #ml/min

  debit_pompe_central_waste <- clearance #ml/min

  parameterValues <-
    c(Q1 = debit_pompe_central_waste ,
      Q2 = debit_central_cartridge ,
      V1 = Vcentral,
      V2 = Vcartridge)



  if (adm.type == "Bolus")
  {
    dosingIntervalMinBolus <- dosingIntervalHoursBolus * 60
    dose_bolus <- initial_concentration * (Vcentral + Vcartridge) #µg

    #Dose events for each compartments
    e1_bolus <-
      ev(
        amt = dose_bolus,
        cmt = 1,
        ii = dosingIntervalMinBolus,
        addl = numberOfDosesBolus-1
      )
    dose <- e1_bolus
    dose <- realize_addl(dose)
    if (constantVolume == FALSE)
    {
      Vadd_bolus <-
        ev(
          amt = VinjectBolus,
          cmt = 3,
          ii = dosingIntervalMinBolus,
          addl = numberOfDosesBolus-1
        )
      dose <- e1_bolus + Vadd_bolus
      dose <- realize_addl(dose)
    }
    simulated_data <- model %>%
      ev(dose) %>%
      param(parameterValues) %>%
      mrgsim(end = lastTimePointMin) %>%
      as_tibble

  }
  if (adm.type == "Infusion")
  {
    tinfuseMin <- tinfuseHours * 60
    halfLifeMin <-   halfLifeHours * 60
    dosingIntervalMinInf <- dosingIntervalHoursInf * 60

    # IV infusion
    keHours <- log(2) / halfLifeHours
    dose_infuse <-
      (
        initial_concentration * (Vcentral + Vcartridge) * keHours * tinfuseHours
      ) / (1 - exp(-keHours *
                     tinfuseHours))#µg
    #Dose events for each compartments
    e1_infusion <-
      ev(
        amt = dose_infuse,
        rate = dose_infuse / tinfuseMin,
        cmt = 1,
        ii = dosingIntervalMinInf,
        addl = numberOfDosesInf-1
      )
    dose <- e1_infusion
    dose <- realize_addl(dose)
    if (constantVolume == FALSE)
    {
      Vadd_infusion <-
        ev(
          amt = VinjectInf,
          rate = VinjectInf / tinfuseMin,
          cmt = 3,
          ii = dosingIntervalMinInf,
          addl = numberOfDosesInf-1
        )
      dose <- e1_infusion + Vadd_infusion
      dose <- realize_addl(dose)
    }


    simulated_data <- model %>%
      ev(dose) %>%
      param(parameterValues) %>%
      mrgsim(end = lastTimePointMin) %>%
      as_tibble

  }

  if (adm.type == "Loading dose + Infusion")
  {
    halfLifeMin <-   halfLifeHours * 60
    keHours <- log(2) / halfLifeHours
    keMin <- log(2) / halfLifeMin
    # IV Loading dose
    dose_loading <- Css * (Vcentral + Vcartridge)
    # IV infusion
    rate_infuse <- keMin * Css * (Vcentral + Vcartridge)

    #Dose events for each compartments
    e1_loading <- ev(amt = dose_loading,
      cmt = 1,
      ii = NA,
      addl = NA)
    e1_infusion <-
      ev(
        amt = rate_infuse*lastTimePointMin,
        rate = rate_infuse,
        cmt = 1
      )
    dose <- e1_loading + e1_infusion
    if (constantVolume == FALSE)
    {

      Vadd_infusion <-
        ev(
          amt = (rate_infuse/Cinfusemaintenance)*lastTimePointMin,
          rate = rate_infuse/Cinfusemaintenance,
          cmt = 3
        ) %>% realize_addl()
      dose <- e1_loading + e1_infusion + Vadd_infusion

    }


    simulated_data <- model %>%
      ev(dose) %>%
      param(parameterValues) %>%
      mrgsim(end = lastTimePointMin) %>%
      as_tibble

  }

  return(simulated_data)
}
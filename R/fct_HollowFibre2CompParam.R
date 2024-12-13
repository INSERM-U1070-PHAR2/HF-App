#' HollowFibre2CompParam
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
HollowFibre2CompParam <- function (halfLifeHoursA = 10,
                                   halfLifeHoursB = 16,
                                   Vcentral = 200,
                                   Vcartridge = 50,
                                   drugNameA = NA,
                                   drugNameB = NA,
                                   initialConcentrationA = 2,
                                   initialConcentrationB = 10,
                                   lastTimePointHours = 24,
                                   admType="Bolus",
                                   VinjectBolusA =  NA,
                                   dosingIntervalHoursBolusA = NA,
                                   numberOfDosesBolusA = NA,
                                   VinjectBolusB = NA,
                                   VinjectBolusExtraB = NA,
                                   dosingIntervalHoursBolusB = NA,
                                   numberOfDosesBolusB = NA,
                                   tinfuseHoursA=NA,
                                   VinjectInfA =  NA,
                                   dosingIntervalHoursInfA = NA,
                                   numberOfDosesInfA = NA,
                                   tinfuseHoursB=NA,
                                   VinjectInfB = NA,
                                   VinjectInfExtraB = NA,
                                   dosingIntervalHoursInfB = NA,
                                   numberOfDosesInfB = NA,
                                   debitCentralCartridge = 120)
{
  library(plyr)
  library(dplyr)
  halfLifeAmin <- halfLifeHoursA * 60 #hours * minutes/hours
  halfLifeBmin <- halfLifeHoursB * 60 #hours * minutes/hours

  clearanceA <-
    log(2) * (Vcentral + Vcartridge) / halfLifeAmin #ml/min
  clearanceB <-
    log(2) * (Vcentral + Vcartridge) / halfLifeBmin #ml/min
  Vextra <- ifelse(clearanceA-clearanceB != 0, ((clearanceA - clearanceB) / clearanceB) * (Vcentral + Vcartridge) ,0)

  debit_pompe_extra_central <- clearanceA - clearanceB #ml/min
  debit_pompe_central_waste <- clearanceA #ml/min
  debit_pompe_dil_extra <- clearanceA - clearanceB #ml/min
  debit_pompe_dil_central <- clearanceB #ml/min

  #Declare last time point to be simulated
  lastTimePointmin <-
    lastTimePointHours * 60 #in minutes = durée de la manip

  volume_waste_produced <-
    debit_pompe_central_waste * lastTimePointmin #mL
  volume_diluant_to_extra <-
    debit_pompe_extra_central * lastTimePointmin #mL
  volume_diluant_to_central <-
    clearanceB * lastTimePointmin #mL

  if(admType=="Bolus")
  {
    total_injected_volume_A <- VinjectBolusA * numberOfDosesBolusA
    total_injected_volume_B_central <-
      VinjectBolusB * numberOfDosesBolusB
    total_injected_volume_B_extra <- VinjectBolusExtraB * numberOfDosesBolusB
    # IV bolus
    doseA_central <-
      initialConcentrationA * (Vcentral + Vcartridge) #µg
    conc_bolus_A <- doseA_central / VinjectBolusA
    doseB_central <-
      initialConcentrationB * (Vcentral + Vcartridge) #µg
    conc_bolus_B_central <- doseB_central / VinjectBolusB
    doseB_extra <-
      doseB_central * ((clearanceA - clearanceB) / clearanceB) #µg
    conc_bolus_B_extra <- doseB_extra / VinjectBolusExtraB
    #Create a table that summarizes everything
    Parameters <- tibble(
      Group = c(rep("Drugs",9),
                rep("Volume",6),
                rep("Bolus",10),
                rep("Experiment",6)),
      Parameter = c(
        "Drug A name",
        "Drug B name",
        "Cmax central after 1 dose for drug A (µg/mL)",
        "Bolus solution concentration for drug A (µg/mL)",
        "Half life central for drug A (h)",
        "Cmax central after 1 dose for drug B (µg/mL)",
        "Bolus solution concentration for drug B in central (µg/mL)",
        "Bolus solution concentration for drug B in extra (µg/mL)",
        "Half life central for drug B (h)",
        "Central volume (mL)",
        "Cartridge volume (mL)",
        "Extra compartment volume (mL)",
        "Spent volume diluant to central (mL)",
        "Spent volume diluant to extra (mL)",
        "Waste volume (mL)",
        "Bolus volume for 1 dose of drug A (mL)",
        "Bolus volume for 1 dose of drug B in central (mL)",
        "Bolus volume for 1 dose of drug B in extra (mL)",
        "Total number of doses of drug A",
        "Total number of doses of drug B",
        "Dosing interval for drug A (h)",
        "Dosing interval for drug B (h)",
        "Total injected volume of drug A (mL)",
        "Total injected volume of drug B in central (mL)",
        "Total injected volume of drug B in extra (mL)",
        "Duration of experiment (h)",
        "Flow pump diluant extra to extra (mL/min)",
        "Flow pump extra to central (mL/min)",
        "Flow pump diluant central to central (mL/min)",
        "Flow pump central to waste (mL/min)",
        "Flow pump central to cartidge (mL/min)"
      ),
      Value = c(drugNameA,
                drugNameB,
                round(
                  c(
                    initialConcentrationA,
                    conc_bolus_A,
                    halfLifeHoursA,
                    initialConcentrationB,
                    conc_bolus_B_central,
                    conc_bolus_B_extra,
                    halfLifeHoursB,
                    Vcentral,
                    Vcartridge,
                    Vextra,
                    volume_diluant_to_central,
                    volume_diluant_to_extra,
                    volume_waste_produced,
                    VinjectBolusA,
                    VinjectBolusB,
                    VinjectBolusExtraB,
                    numberOfDosesBolusA,
                    numberOfDosesBolusB,
                    dosingIntervalHoursBolusA,
                    dosingIntervalHoursBolusB,
                    total_injected_volume_A,
                    total_injected_volume_B_central,
                    total_injected_volume_B_extra,
                    lastTimePointHours,
                    debit_pompe_dil_extra,
                    debit_pompe_extra_central,
                    debit_pompe_dil_central,
                    debit_pompe_central_waste,
                    debitCentralCartridge
                  ),
                  3
                )),
      ID =        c(
        "drugNameA",
        "drugNameB",
        "initialConcentrationA",
        "conc_bolus_A",
        "halfLifeHoursA",
        "initialConcentrationB",
        "conc_bolus_B_central",
        "conc_bolus_B_extra",
        "halfLifeHoursB",
        "Vcentral",
        "Vcartridge",
        "Vextra",
        "volume_diluant_to_central",
        "volume_diluant_to_extra",
        "volume_waste_produced",
        "Vinject_A",
        "Vinject_B_central",
        "Vinject_B_extra",
        "numberOfDosesA",
        "numberOfDosesB",
        "dosingIntervalHours_A",
        "dosingIntervalHours_B",
        "total_injected_volume_A",
        "total_injected_volume_B_central",
        "total_injected_volume_B_extra",
        "lastTimePointHours",
        "debit_pompe_dil_extra",
        "debit_pompe_extra_central",
        "debit_pompe_dil_central",
        "debit_pompe_central_waste",
        "debitCentralCartridge"
      )
    )}
  if(admType=="Infusion")
  {
    total_injected_volume_A <- VinjectInfA * numberOfDosesInfA
    total_injected_volume_B_central <-
      VinjectInfB * numberOfDosesInfB
    total_injected_volume_B_extra <- VinjectInfExtraB * numberOfDosesInfB
    tinfuseMin_A <- tinfuseHoursA * 60
    debit_infuse_A <- VinjectInfA / tinfuseMin_A
    keHours_A <- log(2) / halfLifeHoursA
    dose_infuse_A <-
      (
        initialConcentrationA * (Vcentral + Vcartridge) * keHours_A * tinfuseHoursA
      ) / (1 - exp(-keHours_A *
                     tinfuseHoursA))#µg
    conc_infuse_A <- dose_infuse_A / VinjectInfA


    tinfuseMin_B <- tinfuseHoursB * 60
    debit_infuse_B_central <- VinjectInfB / tinfuseMin_B
    keHours_B <- log(2) / halfLifeHoursB
    dose_infuse_B_central <-
      (
        initialConcentrationB * (Vcentral + Vcartridge) * keHours_B * tinfuseHoursB
      ) / (1 - exp(-keHours_B *
                     tinfuseHoursB))#µg
    conc_infuse_B_central <- dose_infuse_B_central / VinjectInfB

    debit_infuse_B_extra <- VinjectInfExtraB / tinfuseMin_B
    dose_infuse_B_extra <-
      dose_infuse_B_central * ((clearanceA - clearanceB) / clearanceB) #µg
    conc_infuse_B_extra <- dose_infuse_B_extra / VinjectInfExtraB
    # IV bolus
    # IV bolus
    #Create a table that summarizes everything
    Parameters <- tibble(
      Group = c(rep("Drugs",9),
                rep("Volume",6),
                rep("Infusion",10),
                rep("Experiment",9)),
      Parameter = c(
        "Drug A name",
        "Drug B name",
        "Cmax central after 1 dose for drug A (µg/mL)",
        "Infusion solution concentration for drug A (µg/mL)",
        "Half life central for drug A (h)",
        "Cmax central after 1 dose for drug B (µg/mL)",
        "Infusion solution concentration for drug B in central (µg/mL)",
        "Infusion solution concentration for drug B in extra (µg/mL)",
        "Half life central for drug B (h)",
        "Central volume (mL)",
        "Cartridge volume (mL)",
        "Extra compartment volume (mL)",
        "Spent volume diluant to central (mL)",
        "Spent volume diluant to extra (mL)",
        "Waste volume (mL)",
        "Infusion volume for 1 dose of drug A (mL)",
        "Infusion volume for 1 dose of drug B in central (mL)",
        "Infusion volume for 1 dose of drug B in extra (mL)",
        "Total number of doses of drug A",
        "Total number of doses of drug B",
        "Dosing interval for drug A (h)",
        "Dosing interval for drug B (h)",
        "Total injected volume of drug A (mL)",
        "Total injected volume of drug B in central (mL)",
        "Total injected volume of drug B in extra (mL)",
        "Duration of experiment (h)",
        "Flow pump diluant extra to extra (mL/min)",
        "Flow pump extra to central (mL/min)",
        "Flow pump diluant central to central (mL/min)",
        "Flow pump central to waste (mL/min)",
        "Flow pump central to cartridge (mL/min)",
        "Flow infusion A to central (mL/min)",
        "Flow infusion B to central (mL/min)",
        "Flow infusion B to extra (mL/min)"
      ),
      Value = c(drugNameA,
                drugNameB,
                round(
                  c(
                    initialConcentrationA,
                    conc_infuse_A,
                    halfLifeHoursA,
                    initialConcentrationB,
                    conc_infuse_B_central,
                    conc_infuse_B_extra,
                    halfLifeHoursB,
                    Vcentral,
                    Vcartridge,
                    Vextra,
                    volume_diluant_to_central,
                    volume_diluant_to_extra,
                    volume_waste_produced,
                    VinjectInfA,
                    VinjectInfB,
                    VinjectInfExtraB,
                    numberOfDosesInfA,
                    numberOfDosesInfB,
                    dosingIntervalHoursInfA,
                    dosingIntervalHoursInfB,
                    total_injected_volume_A,
                    total_injected_volume_B_central,
                    total_injected_volume_B_extra,
                    lastTimePointHours,
                    debit_pompe_dil_extra,
                    debit_pompe_extra_central,
                    debit_pompe_dil_central,
                    debit_pompe_central_waste,
                    debitCentralCartridge,
                    debit_infuse_A,
                    debit_infuse_B_central,
                    debit_infuse_B_extra
                  ),
                  3
                )),
      ID =        c(
        "drugNameA",
        "drugNameB",
        "initialConcentrationA",
        "conc_infuse_A",
        "halfLifeHoursA",
        "initialConcentrationB",
        "conc_infuse_B_central",
        "conc_infuse_B_extra",
        "halfLifeHoursB",
        "Vcentral",
        "Vcartridge",
        "Vextra",
        "volume_diluant_to_central",
        "volume_diluant_to_extra",
        "volume_waste_produced",
        "Vinject_A",
        "Vinject_B_central",
        "Vinject_B_extra",
        "numberOfDosesA",
        "numberOfDosesB",
        "dosingIntervalHours_A",
        "dosingIntervalHours_B",
        "total_injected_volume_A",
        "total_injected_volume_B_central",
        "total_injected_volume_B_extra",
        "lastTimePointHours",
        "debit_pompe_dil_extra",
        "debit_pompe_extra_central",
        "debit_pompe_dil_central",
        "debit_pompe_central_waste",
        "debitCentralCartridge",
        "debit_infuse_A",
        "debit_infuse_B_central",
        "debit_infuse_B_extra"
      )
    )}

  return(Parameters)
}

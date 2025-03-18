#' HollowFibre1CompAbsParam
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
HollowFibre1CompAbsParam <- function(halfLifeHours = 7.22,
                                     Vcentral = 200,
                                     Vcartridge = 50,
                                     initial_concentration = 2,
                                     lastTimePointHours = 24,
                                     drugName = "Drug",
                                     debit_central_cartridge = 60,
                                     nDoses = 2,
                                     ka = 0.1,
                                     f_avail = 1,
                                     nInfusions = 12,
                                     dosingIntervalHoursAbs = 12,
                                     CinfusionMin = 1,
                                     CinfusionMax = 1000,
                                     CinfusionStep = 1,
                                     minInfusionVolume = 2) {
  library(mrgsolve)
  library(dplyr)
  library(ggplot2)

  calc_ke <- function(half_life){
    log(2)/half_life
  }

  calc_t_peak <- function(ka,ke){
    1/(ka-ke) * log(ka/ke)
  }

  calc_dose <- function(c_peak,f_avail,ka,v,ke,t_peak){
    c_peak * (1/((f_avail*ka/(v*(ka-ke))) * (exp(-ke*t_peak)-exp(-ka*t_peak))))
  }

  calc_f_dose <- function(ka,t_n){
    1-exp(-ka*t_n)
  }

  calc_a_i <- function(dose,ka,t_i){
    dose * exp(-ka*t_i)
  }

  calc_t_i_rounded <- function(t_i){
    round(t_i*60,0)
  }

  calc_t_i_interval_rounded <- function(t_i_minus_1,t_i){
    round((t_i*60-t_i_minus_1*60),0)
  }

  calc_s_i_rounded <- function(a_i_minus_1,a_i,t_i_interval_rounded){
    (a_i_minus_1-a_i)/(t_i_interval_rounded/60)
  }

  calc_v_inf_rounded <- function(s_i_rounded,c_j,t_i_rounded){
    (s_i_rounded/60/c_j)*t_i_rounded*1000
  }

  calc_v_inf_decimal_rounded <- function(v_inf_rounded){
    v_inf_rounded %% 1
  }

  calc_max_v_inf_decimal_rounded <- function(n,ka,f_dose,dose,c)
  {
    max_v_inf_decimal_rounded <- tibble(t_i = c(0,calc_ti(1:n,n,ka,f_dose)),
                                        a_i = calc_a_i(dose,ka,t_i),
                                        s_i = calc_s_i(lag(a_i),a_i,lag(t_i),t_i),
                                        th_amt_transferred = dose * exp(-ka*lag(t_i))-dose * exp(-ka*t_i),
                                        amt_transferred_s_i = s_i*(t_i-lag(t_i)),
                                        t_i_interval_rounded = calc_t_i_interval_rounded(lag(t_i),t_i),
                                        s_i_rounded = calc_s_i_rounded(lag(a_i),a_i,t_i_interval_rounded),
                                        v_inf_rounded = calc_v_inf_rounded(s_i_rounded,c,t_i_interval_rounded),
                                        v_inf_decimal_rounded = calc_v_inf_decimal_rounded(v_inf_rounded)) %>%
      slice(-1)

    max_v_inf_decimal_rounded |>
      dplyr::mutate(c) |>
      dplyr::select(c,v_inf_rounded,v_inf_decimal_rounded) |>
      dplyr::slice(1)

  }

  calc_ti <- function(i,n,ka,f_dose){
    -(log(1-f_dose*(i/n)))/ka
  }

  calc_s_i <- function(a_i_minus_1,
                       a_i,
                       t_i_minus_1,
                       t_i){
    (a_i_minus_1-a_i)/(t_i-t_i_minus_1)
  }
  if(is.na(halfLifeHours)){halfLifeHours <- 7.22} #to enable calculation at app launch
  halfLifeMin <-   halfLifeHours * 60
  lastTimePointMin <- lastTimePointHours * 60

  clearance <-
    log(2) * (Vcentral + Vcartridge) / halfLifeMin #ml/min

  debit_pompe_central_waste <- clearance #ml/min
  debit_pompe_dil_central <- clearance #ml/min

  parameterValues <-
    c(Q1 = debit_pompe_central_waste ,
      Q2 = debit_central_cartridge ,
      V1 = Vcentral,
      V2 = Vcartridge)

  ke <- calc_ke(halfLifeHours)
  t_peak <- calc_t_peak(ka,ke)
  dose <- calc_dose(initial_concentration,f_avail,ka,(Vcentral + Vcartridge)/1000,ke,t_peak)
  f_dose <- calc_f_dose(ka,dosingIntervalHoursAbs)

  c_list = seq(CinfusionMin,CinfusionMax,CinfusionStep)


  optimal_v_inf_decimal_rounded <- purrr::map_dfr(c_list,~calc_max_v_inf_decimal_rounded(nInfusions,ka,f_dose,dose,.x)) %>%
    dplyr::filter(v_inf_rounded >= minInfusionVolume) |>
    dplyr::filter(v_inf_decimal_rounded==min(v_inf_decimal_rounded))

  c_optimal <- optimal_v_inf_decimal_rounded %>% pull(c)

  final_table_optimal <- tibble(t_i = c(0,calc_ti(1:nInfusions,nInfusions,ka,f_dose)),
                                a_i = calc_a_i(dose,ka,t_i),
                                s_i = calc_s_i(lag(a_i),a_i,lag(t_i),t_i),
                                th_amt_transferred = dose * exp(-ka*lag(t_i))-dose * exp(-ka*t_i),
                                amt_transferred_s_i = s_i*(t_i-lag(t_i)),
                                t_i_rounded = calc_t_i_rounded(t_i),
                                t_i_interval_rounded = calc_t_i_interval_rounded(lag(t_i),t_i),
                                s_i_rounded = calc_s_i_rounded(lag(a_i),a_i,t_i_interval_rounded),
                                v_inf_rounded = calc_v_inf_rounded(s_i_rounded,c_optimal,t_i_interval_rounded),
                                v_inf_decimal_rounded = calc_v_inf_decimal_rounded(v_inf_rounded))

  VinjectInf <- sum(final_table_optimal$v_inf_rounded,na.rm = T) |> round(0)
  conc_infuse <- c_optimal
  lastTimePointMin <- lastTimePointHours * 60
  volume_waste_produced <-
    debit_pompe_central_waste * lastTimePointMin #mL
  volume_diluant_to_central <-
    debit_pompe_dil_central * lastTimePointMin #mL
  numberOfDoses <- nDoses
  total_infused_volume <- VinjectInf * numberOfDoses
  #Create a table that summarizes everything
  Parameters <- tibble(
    Group = c(rep("Drug",7),
              rep("Volume",4),
              rep("Infusion",4),
              rep("Experiment",4)),
    Parameter = c(
      "Drug Name",
      "Cmax central after 1 dose (µg/mL)",
      "Infusion solution concentration (µg/mL)",
      "Time to reach Cmax,1 (tmax,1 in h)",
      "Dose administered by the end of the last sub-interval (Dose in mg)",
      "Fraction of the target dose administered by the end of the last sub-interval (fdose)",
      "Half life central (h)",
      "Central volume (mL)",
      "Cartridge volume (mL)",
      "Spent volume diluant to central (mL)",
      "Waste volume (mL)",
      "Infusion volume for 1 dose (mL)",
      "Total number of doses",
      "Dosing interval (h)",
      "Total infused volume (mL)",
      "Duration of experiment (h)",
      "Flow pump diluant to central (mL/min)",
      "Flow pump central to waste (mL/min)",
      "Flow pump central to cartridge (mL/min)"
    ),
    Value = c(drugName,
              round(
                c(
                  initial_concentration,
                  conc_infuse,
                  t_peak,
                  dose,
                  f_dose,
                  halfLifeHours,
                  Vcentral,
                  Vcartridge,
                  volume_diluant_to_central,
                  volume_waste_produced,
                  VinjectInf,
                  numberOfDoses,
                  dosingIntervalHoursAbs,
                  total_infused_volume,
                  lastTimePointHours,
                  debit_pompe_dil_central,
                  debit_pompe_central_waste,
                  debit_central_cartridge
                ),
                3
              )),
    ID =        c(
      "drugName",
      "initial_concentration",
      "conc_infuse",
      "t_peak",
      "dose",
      "f_dose",
      "halfLifeHours",
      "Vcentral",
      "Vcartridge",
      "volume_diluant_to_central",
      "volume_waste_produced",
      "Vinject",
      "numberOfDoses",
      "dosingIntervalHours",
      "total_infused_volume",
      "lastTimePointHours",
      "debit_pompe_dil_central",
      "debit_pompe_central_waste",
      "debit_central_cartridge"
    )
  )
}

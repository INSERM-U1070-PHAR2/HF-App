$PROB
- Author: Vincent Aranzana-Climent
- Date: 04/02/2019 - 15/02/18 - 02/05/19
- Structure: Hollow fiber 2 bottles bolus
- Implementation: ODE
- Error model: None
- Random effects: None

$PARAM @annotated
Q1 : 1 : Flow rate of transfer from extra drug bottle to central (mL/min)
V1 : 100: Volume in that extra drug bottle (mL)
Q2 : 0.5 : Flowrate of transfer from central bottle to waste (mL/min)
V2 : 100 : Volume in central bottle (mL)
Q3 : 150 : Flowrate of transfer from central bottle to cartridge (mL/min)
V3 : 50 : Initial volume in cartridge (mL)

$CMT @annotated
DRUG_A_CENTRAL : Amount of drug A in central bottle (mcg)
DRUG_A_CARTRIDGE : Amount of drug A in cartridge (mcg)
DRUG_B_EXTRA : Amount of drug B in extra drug bottle (mcg)
DRUG_B_CENTRAL : Amount of drug B in central bottle (mcg)
DRUG_B_CARTRIDGE : Amount of drug B in cartridge (mcg)
Vadd_A_Central : Volume added by dosing drug A in central bottle (mL)
Vadd_B_Central : Volume added by dosing drug B in central bottle (mL)
Vadd_B_Extra : Volume added by dosing drug B in extra bottle (mL)

$ODE
double Ve = V1 + Vadd_B_Extra;
double Vc = V2 + Vadd_A_Central + Vadd_B_Central;
dxdt_DRUG_A_CENTRAL = -(Q2/Vc+Q3/Vc) * DRUG_A_CENTRAL + Q3/V3 * DRUG_A_CARTRIDGE;
dxdt_DRUG_A_CARTRIDGE = Q3/Vc * DRUG_A_CENTRAL - Q3/V3 * DRUG_A_CARTRIDGE;
dxdt_DRUG_B_EXTRA = -(Q1/Ve) * DRUG_B_EXTRA;
dxdt_DRUG_B_CENTRAL = (Q1/Ve) * DRUG_B_EXTRA  -(Q2/Vc+Q3/Vc) * DRUG_B_CENTRAL + Q3/V3 * DRUG_B_CARTRIDGE;
dxdt_DRUG_B_CARTRIDGE = Q3/Vc * DRUG_B_CENTRAL - Q3/V3 * DRUG_B_CARTRIDGE;
dxdt_Vadd_A_Central = 0;
dxdt_Vadd_B_Central = 0;
dxdt_Vadd_B_Extra = 0;


$TABLE
double C_DRUG_A_CENTRAL = (DRUG_A_CENTRAL/Vc);
double C_DRUG_A_CARTRIDGE = (DRUG_A_CARTRIDGE/V3);
double C_DRUG_B_EXTRA = (DRUG_B_EXTRA/Ve);
double C_DRUG_B_CENTRAL = (DRUG_B_CENTRAL/Vc);
double C_DRUG_B_CARTRIDGE = (DRUG_B_CARTRIDGE/V3);

$CAPTURE @annotated
C_DRUG_A_CENTRAL : Concentration of drug A in central bottle (mcg/mL)
C_DRUG_A_CARTRIDGE : Concentration of drug A in cartridge (mcg/mL)
C_DRUG_B_EXTRA : Concentration of drug B in extra drug bottle (mcg/mL)
C_DRUG_B_CENTRAL : Concentration of drug B in central bottle (mcg/mL)
C_DRUG_B_CARTRIDGE : Concentration of drug B in cartridge (mcg/mL)
Ve : Volume extra (mL)
Vc : Volume central (mL)
V3 : Volume cartridge (mL)

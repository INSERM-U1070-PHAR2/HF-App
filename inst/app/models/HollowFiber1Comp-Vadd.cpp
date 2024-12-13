$PROB
- Author: Vincent Aranzana-Climent
- Date: 02/05/19
- Structure: Hollow fiber 1 bottle bolus
- Implementation: ODE
- Error model: None
- Random effects: None

$PARAM @annotated
Q1 : 0.5 : Flowrate of transfer from central bottle to waste (mL/min)
Q2 : 150 : Flowrate of transfer from central bottle to cartridge (mL/min)
V1 : 100 : Initial volume in central bottle (mL)
V2 : 50 : Initial volume in cartridge (mL)

$CMT @annotated
DRUG_CENTRAL : Amount of drug in central bottle (mcg)
DRUG_CARTRIDGE : Amount of drug in cartridge (mcg)
Vadd : Cumulative added volume in central (mL)

$MAIN
DRUG_CENTRAL_0 = 0 ;

$ODE
double V = V1 + Vadd;
dxdt_DRUG_CENTRAL = -(Q1/V+Q2/V) * DRUG_CENTRAL + Q2/V2 * DRUG_CARTRIDGE;
dxdt_DRUG_CARTRIDGE = Q2/V * DRUG_CENTRAL - Q2/V2 * DRUG_CARTRIDGE;
dxdt_Vadd = 0;

$TABLE
double C_DRUG_CENTRAL = (DRUG_CENTRAL/V);
double C_DRUG_CARTRIDGE = DRUG_CARTRIDGE/V2;

$CAPTURE @annotated
C_DRUG_CENTRAL : Concentration of drug in central bottle (mcg/mL)
C_DRUG_CARTRIDGE : Concentration of drug in cartridge (mcg/mL)

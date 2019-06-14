//
//
//  UNIT      : Contains the class TStomsaDLLManager.
//  AUTHOR    : Grant Nyland
//  DATE      : 2019/05/31
//  COPYRIGHT : Copyright © 2019 DWS.
//
//
unit UStomsaDLLManager;


interface


uses
  UDLLManager;


//
// A class that manages the Fortran DLLs for the Stomsa model.
//
type
  TStomsaDLLManager = class(TDLLManager)
  protected

    // Introduced in this class.
    function CreateDLLAgent(I: Integer): TDLLAgent; override;
  public

    // Construction, destruction.
    constructor Create;

    // Introduced in this class.
    procedure FITMARG(N: PInteger;      // INPUT  - Number of element that was read in from file, now in Y().
                      ISTART: PInteger; // INPUT  - The start year from the INC file (eg. 1920).
                      NZF: PInteger;    // OUTPUT - Number of zero (or neer zero) flows in dataset.
                      ICURV: PInteger;  // OUTPUT - The models selected curve (1 of 4).
                      YIN: PDouble;     // INPUT  - Contains the array of calculated annual totals from INC file.
                      X: PDouble;       // OUTPUT - Natural annual flows sorted (non zero).
                      W: PDouble;       // OUTPUT - Normalized flows - transformed and sorted. This is for the default curve.
                      GA: PDouble;      // OUTPUT - Gamma for the four models.
                      DE: PDouble;      // OUTPUT - Delta for the four models.
                      XL: PDouble;      // OUTPUT - X-lamda for the four models.
                      XXX: PDouble;     // OUTPUT -
                      CR: PDouble;      // OUTPUT - Calculated criteria (model with the smallest value is default choice).
                      A1: PDouble;      // OUTPUT - Mean of annual totals.
                      S1: PDouble);     // OUTPUT - Standard deviation of annual totals.

    procedure MDSTATS(NY: PInteger;
                      NNP: PInteger;
                      M1: PInteger;
                      M2: PInteger;
                      Y: PDouble;
                      X: PDouble;
                      W: PDouble;
                      XA1: PDouble;
                      XSD1: PDouble;
                      XS31: PDouble;
                      XS41: PDouble;
                      WA1: PDouble;
                      WSD1: PDouble;
                      WS31: PDouble;
                      WS411: PDouble;
                      CL11: PDouble;
                      CL21: PDouble;
                      XX11: PDouble;
                      QCH11: PDouble;
                      XX21: PDouble;
                      QCH21: PDouble);

    procedure SERCOR(NNPD: PInteger;
                     NYD: PInteger;
                     NFRD: PInteger;
                     NLAG: PInteger;
                     ITP: PInteger;
                     W: PDouble;
                     YIN: PDouble;
                     G: PDouble;
                     D: PDouble;
                     XL: PDouble;
                     XXI: PDouble;
                     NCORR: PDouble;
                     NPORT: PDouble;
                     NCONF: PDouble;
                     NEXCD: PDouble;
                     PCORR: PDouble;
                     PPORT: PDouble;
                     PFRD: PInteger;
                     PLAG: PInteger;
                     PCONF: PDouble;
                     PEXCD: PDouble;
                     NN: PInteger;
                     IT: PInteger;
                     IER: PInteger;
                     FFF: PDouble;
                     FF: PDouble;
                     FFI: PDouble;
                     TTH: PDouble;
                     AIC: PDouble;
                     DIST: PDouble;
                     CRIT: PDouble;
                     QI: PDouble;
                     CRNX: PInteger;
                     CRJX: PInteger;
                     CRSX: PDouble;
                     CRP: PDouble;
                     AS1: PDouble);

    procedure CROSS(NG: PInteger;      // INPUT  - Number of records to process.
                    ND: PInteger;      // INPUT  - Number of years in each data record.
                    ISTRTD: PInteger;  // INPUT  - Start year of each data record > 1920.
                    FI1: PDouble;      // INPUT  - FI(first) variable for each data record.
                    FI2: PDouble;      // INPUT  - FI(second) variable for each data record.
                    TH1: PDouble;      // INPUT  - Theta(first) variable for each data record.
                    TH2: PDouble;      // INPUT  - Theta(second) variable for each data record.
                    YD: PDouble;       // INPUT  - Yearly flows for each record.
                    S0D: PDouble;      // OUTPUT - The lag zero dispersion matrix [ANS file].
                    EG0D: PDouble;     // OUTPUT - The Eigenvalues of G0 [ANS file].
                    EH1D: PDouble;     // OUTPUT - The Eigenvalues of H1 [ANS file].
                    EH0D: PDouble;     // OUTPUT - The Eigenvalues of H0 [ANS file].
                    BD: PDouble;       // OUTPUT - The square-root of lag-zero dispersion matrix G0 [PARAM file].
                    B0D: PDouble;      // OUTPUT - The square-root of lag-zero starting matrix H0 [PARAM file].
                    B1D: PDouble;      // OUTPUT - The square-root of lag-one starting matrix H1 [PARAM file].
                    AD: PDouble;       // OUTPUT - The coefficient matrix of Z(-1) calculationg Z(0) [PARAM file].
                    CD: PDouble;       // OUTPUT - The coefficient matrix of A(-1) calculationg Z(0) [PARAM file].
                    BBD: PDouble;      // OUTPUT - As a check.
                    DD: PDouble);      // OUTPUT - The matrix of differences.

    procedure FLOWGEN(var NR: Integer;      // INPUT  - Number of sequences to generate.
                      var NGG: Integer;     // INPUT  - Max number of gauges that can be processed per flow generation.
                      var NSYRD: Integer;   // INPUT  - Number of stochastic years to generate.
                      var IFLAGD: Integer;  // INPUT  - Stochastic generation method ( 0 = rigourous, 1 = bootstrap ).
                      var NKEYGD: Integer;  // INPUT  - Number of key gauges.
                      var NFGD: Integer;    // INPUT  - Number of gauges in PARAM.DAT.
                      var NGPD: Integer;    // INPUT  - Number of gauges to process.
                      NGSTORD: PInteger; // INPUT  - List of gauge numbers (NPGD size).
                      ITYPED: PInteger;  // INPUT  - Marginal distribution type per gauge.
                      NZFD: PInteger;    // INPUT  - Number of zeros per gauge.
                      NYRSD: PInteger;   // INPUT  - Number of years per gauge.
                      ISTRYD: PInteger;  // INPUT  - Start year for each gauge, base = 1900.
                      IKEYGD: PInteger;  // INPUT  - List of key gauges.
                      XSDel: PDouble;    // INPUT  - Historical data.
                      PHID: PDouble;     // INPUT  - Pi variable, two per gauge.
                      THETAD: PDouble;   // INPUT  - Theta variable, two per gauge.
                      PZEROD: PDouble;   // INPUT  - Pzero.
                      GAMMAD: PDouble;   // INPUT  - The following variables are for marginal distribution generation.
                      DELTAD: PDouble;   // INPUT  -
                      XLAMD: PDouble;    // INPUT  -
                      XID: PDouble;      // INPUT  -
                      ZMEAND: PDouble;   // INPUT  -
                      ZSTDEVD: PDouble;  // INPUT  -
                      BD: PDouble;       // INPUT  - The following variables and the cross-correlation matrixes.
                      AA1D: PDouble;     // INPUT  -
                      AA2D: PDouble;     // INPUT  -
                      ZZ1D: PDouble;     // INPUT  -
                      ZZ2D: PDouble;     // INPUT  -
                      B0D: PDouble;      // INPUT  -
                      B1D: PDouble;      // INPUT  -
                      AD: PDouble;       // INPUT  -
                      CD: PDouble;       // INPUT  -
                      FLOW: PDouble);    // OUTPUT - Stochastically generated output flows.

    procedure FLOWSTAT(NRSEQ: PInteger;
                       LWD: PInteger;
                       NYRS: PInteger;
                       LPD: PInteger;
                       RILHD: PDouble;
                       RILSD: PDouble;
                       RJLHD: PDouble;
                       RJLSD: PDouble;
                       XH: PDouble;
                       FLOWD: PDouble;
                       APAD: PDouble;
                       GPAD: PDouble;
                       HGPAD: PDouble;
                       TSAMD: PDouble;
                       TSTMD: PDouble;
                       TSKMD: PDouble;
                       TSASD: PDouble;
                       TSTSD: PDouble;
                       TSKSD: PDouble;
                       TSCVD: PDouble;
                       TSTVD: PDouble;
                       TSKVD: PDouble;
                       GJAD: PDouble;
                       PGD: PDouble;
                       HSAD: PDouble;
                       PSD: PDouble;
                       X1D: PDouble;
                       PMAD: PDouble;
                       X2D: PDouble;
                       PMSD: PDouble;
                       HSKD: PDouble;
                       HJAKD: PDouble;
                       RELD: PDouble;
                       AMIND: PDouble;
                       WMIND: PDouble;
                       QRHD: PDouble;
                       QRSD: PDouble;
                       RSHD: PDouble;
                       RSSD: PDouble;
                       MNGPAD: PDouble;
                       MDXJD: PDouble);

    procedure MTHRNK(NRR: PInteger;   // INPUT  - Number of sequences to process.
                     NSYRI: PInteger; // INPUT  - Number of years of generated flows.
                     NYRS: PInteger;  // INPUT  - Number of years of historical data for two gauges to be correlated.
                     ISTRY: PInteger; // INPUT  - Start years of historical data for two gauges.
                     XH1: PDouble;    // INPUT  - Historical data for gauge 1, the 13th value is the annual total.
                     XH2: PDouble;    // INPUT  - Historical data for gauge 2, the 13th value is the annual total.
                     FLOWD1: PDouble; // INPUT  - Stochastically generated flows for gauge 1 - from FLOWGEN, the 13th value is the annual total.
                     FLOWD2: PDouble; // INPUT  - Stochastically generated flows for gauge 1 - from FLOWGEN, the 13th value is the annual total.
                     RSHO: PDouble;   // OUTPUT - Historical correlation coeficients, 1-12 monthly, 13 annual.
                     RMEANO: PDouble; // OUTPUT - Mean of simulated, 1-12 monthly, 13 annual.
                     PO: PDouble);    // OUTPUT - Distribution of correlation coeficients of simulated flows, 1-12 monthly, 13 annual.
  end;


implementation


//
// Implementation dependencies.
//
uses

  // Delphi
  SysUtils, Windows, Forms,

  // DWS
  UErrorHandlingOperations;


{ T_anlmk6 }


//
//      SUBROUTINE  FITMARG( N, ISTART, NZF, ICURV, YIN, X, W, GA, DE, XL, XXX, CR, A1, S1) bind(C, name="FITMARG")
//
//      REAL (KIND=8) YIN,X,GA,DE,XL,CR
//      DIMENSION X(1000),W(1000)
//      DIMENSION YIN(1000)
//C     Variables from NORMALIZ
//      DIMENSION GA(4), DE(4), XL(4), XXX(4), CR(4)
//      DIMENSION A1(1),S1(1)
//
type
  FITMARG_Function = procedure (
    N: PInteger;      // INPUT  - Number of element that was read in from file, now in Y().
    ISTART: PInteger; // INPUT  - The start year from the INC file (eg. 1920).
    NZF: PInteger;    // OUTPUT - Number of zero (or neer zero) flows in dataset.
    ICURV: PInteger;  // OUTPUT - The models selected curve (1 of 4).
    YIN: PDouble;     // INPUT  - Contains the array of calculated annual totals from INC file.
    X: PDouble;       // OUTPUT - Natural annual flows sorted (non zero).
    W: PDouble;       // OUTPUT - Normalized flows - transformed and sorted. This is for the default curve.
    GA: PDouble;      // OUTPUT - Gamma for the four models.
    DE: PDouble;      // OUTPUT - Delta for the four models.
    XL: PDouble;      // OUTPUT - X-lamda for the four models.
    XXX: PDouble;     // OUTPUT -
    CR: PDouble;      // OUTPUT - Calculated criteria (model with the smallest value is default choice).
    A1: PDouble;      // OUTPUT - Mean of annual totals.
    S1: PDouble       // OUTPUT - Standard deviation of annual totals.
  ); stdcall;
  T_anlmk6 = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    FITMARG: FITMARG_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;

//
// Constructor.
//
constructor T_anlmk6.Create;
const OPNAME = 'T_anlmk6.Create';
begin
  try
    inherited Create;
    FITMARG := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_anlmk6.GetFunctionPointers: Boolean;
const OPNAME = 'T_anlmk6.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'FITMARG') then
  begin
    FITMARG := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_anlmk6.ClearFunctionPointers;
const OPNAME = 'T_anlmk6.ClearFunctionPointers';
begin
  try
    FITMARG := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_mdstats }


//
//  procedure MDSTATS(var NY,NNP,M1,M2: ArrayT1I; Y,X,W : array1000F; XA1,XSD1,XS31,XS41,WA1,WSD1,
//                        WS31,WS411,CL11,CL21,XX11,
//                        QCH11,XX21,QCH21 : Array1F);
//                        stdcall; external 'MDSTATS.dll';
//
type
  MDSTATS_Function = procedure (
    NY: PInteger;
    NNP: PInteger;
    M1: PInteger;
    M2: PInteger;
    Y: PDouble;
    X: PDouble;
    W: PDouble;
    XA1: PDouble;
    XSD1: PDouble;
    XS31: PDouble;
    XS41: PDouble;
    WA1: PDouble;
    WSD1: PDouble;
    WS31: PDouble;
    WS411: PDouble;
    CL11: PDouble;
    CL21: PDouble;
    XX11: PDouble;
    QCH11: PDouble;
    XX21: PDouble;
    QCH21: PDouble
  ); stdcall;
  T_mdstats = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    MDSTATS: MDSTATS_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_mdstats.Create;
const OPNAME = 'T_mdstats.Create';
begin
  try
    inherited Create;
    MDSTATS := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_mdstats.GetFunctionPointers: Boolean;
const OPNAME = 'T_mdstats.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'MDSTATS') then
  begin
    MDSTATS := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_mdstats.ClearFunctionPointers;
const OPNAME = 'T_mdstats.ClearFunctionPointers';
begin
  try
    MDSTATS := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_sercor }


//
//      SUBROUTINE SERCOR(NNPD,NYD,NFRD,NLAG,ITP,
//     *            W,YIN,G,D,XL,XXI,
//     *            NCORR,NPORT,NCONF,NEXCD,
//     *            PCORR,PPORT,PFRD,PLAG,PCONF,PEXCD,
//     *            NN,IT,IER,FFF,FF,FFI,TTH,AIC,
//     *            DIST,CRIT,QI,CRNX,CRJX,CRSX,CRP,AS)
//     *            bind(C, name="SERCOR")
//
//      DIMENSION NNPD(1),NYD(1),NFRD(1),NLAG(1),ITP(1)
//      INTEGER NLAG,ITP
//      DIMENSION W(1000),YIN(1000),G(1),D(1),XL(1),XXI(1)
//      DIMENSION NCORR(30,2),NPORT(1),NCONF(1),NEXCD(1)
//      REAL*8 NCORR,NPORT,NCONF,NEXCD
//      DIMENSION PCORR(30,2,9),PPORT(9),PFRD(9),PLAG(9),PCONF(9),PEXCD(9)
//      INTEGER PFRD,PLAG
//      DIMENSION NN(9),IT(9),IER(9),FFF(9),FF(9),FFI(9,2),TTH(9,2),AIC(9)
//      DIMENSION DIST(9),CRIT(9),QI(12),CRNX(13),CRJX(13),CRSX(13),CRP(6)
//      INTEGER CRNX,CRJX
//      DIMENSION AS1(9,2)
//
type
  SERCOR_Function = procedure (
    NNPD: PInteger;
    NYD: PInteger;
    NFRD: PInteger;
    NLAG: PInteger;
    ITP: PInteger;
    W: PDouble;
    YIN: PDouble;
    G: PDouble;
    D: PDouble;
    XL: PDouble;
    XXI: PDouble;
    NCORR: PDouble;
    NPORT: PDouble;
    NCONF: PDouble;
    NEXCD: PDouble;
    PCORR: PDouble;
    PPORT: PDouble;
    PFRD: PInteger;
    PLAG: PInteger;
    PCONF: PDouble;
    PEXCD: PDouble;
    NN: PInteger;
    IT: PInteger;
    IER: PInteger;
    FFF: PDouble;
    FF: PDouble;
    FFI: PDouble;
    TTH: PDouble;
    AIC: PDouble;
    DIST: PDouble;
    CRIT: PDouble;
    QI: PDouble;
    CRNX: PInteger;
    CRJX: PInteger;
    CRSX: PDouble;
    CRP: PDouble;
    AS1: PDouble
  ); stdcall;
  T_sercor = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    SERCOR: SERCOR_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_sercor.Create;
const OPNAME = 'T_sercor.Create';
begin
  try
    inherited Create;
    SERCOR := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_sercor.GetFunctionPointers: Boolean;
const OPNAME = 'T_sercor.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'SERCOR') then
  begin
    SERCOR := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_sercor.ClearFunctionPointers;
const OPNAME = 'T_sercor.ClearFunctionPointers';
begin
  try
    SERCOR := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_cross }


//
//      SUBROUTINE  CROSS( NG, ND , ISTRTD, FI1, FI2, TH1, TH2, YD, S0D,
//     *                   EG0D, EH1D, EH0D, BD, B0D, B1D, AD, CD, BBD,
//     *                   DD ) bind(C, name="CROSS")
//
//        DIMENSION ND(500),ISTRTD(500),FI1(500),FI2(500),TH1(500)
//        DIMENSION TH2(500),YD(1000,500)
//        DIMENSION S0D(500,500),EG0D(500),EH1D(500),EH0D(500),BD(500,500)
//        DIMENSION B0D(500,500),B1D(500,500),AD(500,500),CD(500,500)
//        DIMENSION BBD(500,500),DD(500,500)
//
type
  CROSS_Function = procedure (
    NG: PInteger;      // INPUT  - Number of records to process.
    ND: PInteger;      // INPUT  - Number of years in each data record.
    ISTRTD: PInteger;  // INPUT  - Start year of each data record > 1920.
    FI1: PDouble;      // INPUT  - FI(first) variable for each data record.
    FI2: PDouble;      // INPUT  - FI(second) variable for each data record.
    TH1: PDouble;      // INPUT  - Theta(first) variable for each data record.
    TH2: PDouble;      // INPUT  - Theta(second) variable for each data record.
    YD: PDouble;       // INPUT  - Yearly flows for each record.
    S0D: PDouble;      // OUTPUT - The lag zero dispersion matrix [ANS file].
    EG0D: PDouble;     // OUTPUT - The Eigenvalues of G0 [ANS file].
    EH1D: PDouble;     // OUTPUT - The Eigenvalues of H1 [ANS file].
    EH0D: PDouble;     // OUTPUT - The Eigenvalues of H0 [ANS file].
    BD: PDouble;       // OUTPUT - The square-root of lag-zero dispersion matrix G0 [PARAM file].
    B0D: PDouble;      // OUTPUT - The square-root of lag-zero starting matrix H0 [PARAM file].
    B1D: PDouble;      // OUTPUT - The square-root of lag-one starting matrix H1 [PARAM file].
    AD: PDouble;       // OUTPUT - The coefficient matrix of Z(-1) calculationg Z(0) [PARAM file].
    CD: PDouble;       // OUTPUT - The coefficient matrix of A(-1) calculationg Z(0) [PARAM file].
    BBD: PDouble;      // OUTPUT - As a check.
    DD: PDouble        // OUTPUT - The matrix of differences.
  ); stdcall;
  T_cross = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    CROSS: CROSS_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_cross.Create;
const OPNAME = 'T_cross.Create';
begin
  try
    inherited Create;
    CROSS := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_cross.GetFunctionPointers: Boolean;
const OPNAME = 'T_cross.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'CROSS') then
  begin
    CROSS := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_cross.ClearFunctionPointers;
const OPNAME = 'T_cross.ClearFunctionPointers';
begin
  try
    CROSS := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_flowgen }


//
//      SUBROUTINE FLOWGEN(NR, NGG, NSYRD, IFLAGD, NKEYGD, NFGD, NGPD,
//     *                   NGSTORD,
//     *                    ITYPED,   NZFD,  NYRSD,  ISTRYD,  IKEYGD,
//     *                     XSDel,   PHID, THETAD,  PZEROD,  GAMMAD,
//     *                    DELTAD,  XLAMD,    XID,  ZMEAND, ZSTDEVD,
//     *                        BD,   AA1D,   AA2D,    ZZ1D,    ZZ2D,
//     *                       B0D,    B1D,     AD,      CD,   FLOW)
//     *   bind(C, name="FLOWGEN")
//
//      PARAMETER ( NGG = 10 )
//      PARAMETER ( NGM = 200 )
//      PARAMETER ( NR  = 500 )
//      PARAMETER ( MNY = 100 )
//
//      INTEGER NSYRD(1), IFLAGD(1), NKEYGD(1), NFGD(1), NGPD(1)
//      DIMENSION NGSTORD(NGG)
//      DIMENSION ITYPED(NGM), NZFD(NGM),  NYRSD(NGM), ISTRYD(NGM), IKEYGD(NGM)
//      DIMENSION XSDel(12,MNY,NGM), PHID(2,NGM), THETAD(2,NGM), PZEROD(NGM), GAMMAD(NGM)
//      DIMENSION DELTAD(NGM), XLAMD(NGM), XID(NGM), ZMEAND(NGM), ZSTDEVD(NGM)
//      DIMENSION BD(NGM,NGM), AA1D(NGM), AA2D(NGM), ZZ1D(NGM), ZZ2D(NGM)
//      DIMENSION B0D(NGM,NGM), B1D(NGM,NGM), AD(NGM,NGM), CD(NGM,NGM)
//      REAL(KIND=4) FLOW(12,MNY,NR,NGG)
//
type
  FLOWGEN_Function = procedure (
    var NR: Integer;      // INPUT  - Number of sequences to generate.
    var NGG: Integer;     // INPUT  - Max number of gauges that can be processed per flow generation.
    var NSYRD: Integer;   // INPUT  - Number of stochastic years to generate.
    var IFLAGD: Integer;  // INPUT  - Stochastic generation method ( 0 = rigourous, 1 = bootstrap ).
    var NKEYGD: Integer;  // INPUT  - Number of key gauges.
    var NFGD: Integer;    // INPUT  - Number of gauges in PARAM.DAT.
    var NGPD: Integer;    // INPUT  - Number of gauges to process.
    NGSTORD: PInteger;    // INPUT  - List of gauge numbers (NPGD size).
    ITYPED: PInteger;     // INPUT  - Marginal distribution type per gauge.
    NZFD: PInteger;       // INPUT  - Number of zeros per gauge.
    NYRSD: PInteger;      // INPUT  - Number of years per gauge.
    ISTRYD: PInteger;     // INPUT  - Start year for each gauge, base = 1900.
    IKEYGD: PInteger;     // INPUT  - List of key gauges.
    XSDel: PDouble;       // INPUT  - Historical data.
    PHID: PDouble;        // INPUT  - Pi variable, two per gauge.
    THETAD: PDouble;      // INPUT  - Theta variable, two per gauge.
    PZEROD: PDouble;      // INPUT  - Pzero.
    GAMMAD: PDouble;      // INPUT  - The following variables are for marginal distribution generation.
    DELTAD: PDouble;      // INPUT  -
    XLAMD: PDouble;       // INPUT  -
    XID: PDouble;         // INPUT  -
    ZMEAND: PDouble;      // INPUT  -
    ZSTDEVD: PDouble;     // INPUT  -
    BD: PDouble;          // INPUT  - The following variables and the cross-correlation matrixes.
    AA1D: PDouble;        // INPUT  -
    AA2D: PDouble;        // INPUT  -
    ZZ1D: PDouble;        // INPUT  -
    ZZ2D: PDouble;        // INPUT  -
    B0D: PDouble;         // INPUT  -
    B1D: PDouble;         // INPUT  -
    AD: PDouble;          // INPUT  -
    CD: PDouble;          // INPUT  -
    FLOW: PDouble         // OUTPUT - Stochastically generated output flows.
  ); stdcall;
  T_flowgen = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    FLOWGEN: FLOWGEN_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_flowgen.Create;
const OPNAME = 'T_flowgen.Create';
begin
  try
    inherited Create;
    FLOWGEN := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_flowgen.GetFunctionPointers: Boolean;
const OPNAME = 'T_flowgen.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'FLOWGEN') then
  begin
    FLOWGEN := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_flowgen.ClearFunctionPointers;
const OPNAME = 'T_flowgen.ClearFunctionPointers';
begin
  try
    FLOWGEN := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_flowstat }


{
      SUBROUTINE FLOWSTAT(  NRSEQ,    LWD,  NYRS ,    LPD,
     *     RILHD,  RILSD,   RJLHD,  RJLSD,      XH,
     *     FLOWD,   APAD,   GPAD ,
     *     HGPAD,  TSAMD,   TSTMD, TSKMD,   TSASD,    TSTSD,
     *     TSKSD,  TSCVD,   TSTVD, TSKVD,    GJAD,      PGD,
     *      HSAD,    PSD,     X1D,  PMAD,     X2D,     PMSD,
     *      HSKD,  HJAKD,    RELD,
     *     AMIND,  WMIND,    QRHD,  QRSD,
     *      RSHD,   RSSD,  MNGPAD, MDXJD)
     *  bind(C, name="FLOWSTAT")

      PARAMETER ( NR  = 101 )
      PARAMETER ( MNY = 100 )

      DIMENSION NRSEQ(1), LWD(1), NYRS(2), LPD(10)
      DIMENSION RILHD(5), RILSD(5,7), RJLHD(5), RJLSD(5,7), XH(MNY,12)
      DIMENSION FLOWD(12,MNY,NR), APAD(20,NR), GPAD(20,NR)
      DIMENSION HGPAD(20), TSAMD(1), TSTMD(1), TSKMD(1), TSASD(1), TSTSD(1)
      DIMENSION TSKSD(1), TSCVD(1), TSTVD(1), TSKVD(1), GJAD(1), PGD(7)
      DIMENSION HSAD(1), PSD(7), X1D(12), PMAD(7,12), X2D(12), PMSD(7,12)
      DIMENSION HSKD(1), HJAKD(1), RELD(5)
      DIMENSION AMIND(10), WMIND(10,7), QRHD(5), QRSD(5,7)
      DIMENSION RSHD(5), RSSD(5,7), MNGPAD(20), MDXJD(20)
}
type
  FLOWSTAT_Function = procedure (
    NRSEQ: PInteger;
    LWD: PInteger;
    NYRS: PInteger;
    LPD: PInteger;
    RILHD: PDouble;
    RILSD: PDouble;
    RJLHD: PDouble;
    RJLSD: PDouble;
    XH: PDouble;
    FLOWD: PDouble;
    APAD: PDouble;
    GPAD: PDouble;
    HGPAD: PDouble;
    TSAMD: PDouble;
    TSTMD: PDouble;
    TSKMD: PDouble;
    TSASD: PDouble;
    TSTSD: PDouble;
    TSKSD: PDouble;
    TSCVD: PDouble;
    TSTVD: PDouble;
    TSKVD: PDouble;
    GJAD: PDouble;
    PGD: PDouble;
    HSAD: PDouble;
    PSD: PDouble;
    X1D: PDouble;
    PMAD: PDouble;
    X2D: PDouble;
    PMSD: PDouble;
    HSKD: PDouble;
    HJAKD: PDouble;
    RELD: PDouble;
    AMIND: PDouble;
    WMIND: PDouble;
    QRHD: PDouble;
    QRSD: PDouble;
    RSHD: PDouble;
    RSSD: PDouble;
    MNGPAD: PDouble;
    MDXJD: PDouble); stdcall;
  T_flowstat = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    FLOWSTAT: FLOWSTAT_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_flowstat.Create;
const OPNAME = 'T_flowstat.Create';
begin
  try
    inherited Create;
    FLOWSTAT := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_flowstat.GetFunctionPointers: Boolean;
const OPNAME = 'T_flowstat.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'FLOWSTAT') then
  begin
    FLOWSTAT := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_flowstat.ClearFunctionPointers;
const OPNAME = 'T_flowstat.ClearFunctionPointers';
begin
  try
    FLOWSTAT := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ T_flowcor }


{
      SUBROUTINE MTHRNK(NRR, NSYRI, NYRS, ISTRY,  XH1, XH2, FLOWD1, FLOWD2, RSHO, RMEANO, PO)
     $        bind(C, name="MTHRNK")

      PARAMETER (NR  = 101)
      PARAMETER (MNY = 100)

      DIMENSION NRR(1), NSYRI(1), NYRS(2), ISTRY(2), XH1(MNY,13), XH2(MNY,13)
      DIMENSION FLOWD1(13,MNY,NR), FLOWD2(13,MNY,NR)
      DIMENSION RSHO(13), RMEANO(13), PO(13,7)

      REAL(KIND=8) RSHO,RMEANO,PO
}
type
  MTHRNK_Function = procedure (
    NRR: PInteger;         // INPUT  - Number of sequences to process.
    NSYRI: PInteger;       // INPUT  - Number of years of generated flows.
    NYRS: PInteger;        // INPUT  - Number of years of historical data for two gauges to be correlated.
    ISTRY: PInteger;       // INPUT  - Start years of historical data for two gauges.
    XH1: PDouble;          // INPUT  - Historical data for gauge 1, the 13th value is the annual total.
    XH2: PDouble;          // INPUT  - Historical data for gauge 2, the 13th value is the annual total.
    FLOWD1: PDouble;       // INPUT  - Stochastically generated flows for gauge 1 - from FLOWGEN, the 13th value is the annual total.
    FLOWD2: PDouble;       // INPUT  - Stochastically generated flows for gauge 1 - from FLOWGEN, the 13th value is the annual total
    RSHO: PDouble;         // OUTPUT - Historical correlation coeficients, 1-12 monthly, 13 annual.
    RMEANO: PDouble;       // OUTPUT - Mean of simulated, 1-12 monthly, 13 annual.
    PO: PDouble); stdcall; // OUTPUT - Distribution of correlation coeficients of simulated flows, 1-12 monthly, 13 annual.
  T_flowcor = class(TDLLAgent)
  protected

    // Overriden from TDLLAgent.
    function GetFunctionPointers: Boolean; override;
    procedure ClearFunctionPointers; override;
  public

    // Members
    MTHRNK: MTHRNK_Function; // Stores the function pointer exported from the DLL's.

    // Construction, destruction.
    constructor Create; override;
  end;


//
// Constructor.
//
constructor T_flowcor.Create;
const OPNAME = 'T_flowcor.Create';
begin
  try
    inherited Create;
    MTHRNK := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function T_flowcor.GetFunctionPointers: Boolean;
const OPNAME = 'T_flowcor.GetFunctionPointers';
var LPointer: Pointer;
begin
  Result := False;
  try
  if GetDLLFunction(LPointer, 'MTHRNK') then
  begin
    MTHRNK := LPointer;
    Result := True;
  end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Clears all the function pointers.
//
procedure T_flowcor.ClearFunctionPointers;
const OPNAME = 'T_flowcor.ClearFunctionPointers';
begin
  try
    MTHRNK := nil;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TStomsaDLLManager }


//
// Constructor.
//
constructor TStomsaDLLManager.Create;
const OPNAME = 'TStomsaDLLManager.Create';
begin
  try

    // Call the ancestor.
    inherited Create;

    // The Stomsa DLL's are in a Stomsa sub folder.
    FBinFolder := 'Stomsa\';

    // Add all the stomsa DLL's
    FDLL.Add('anlmk6.dll');
    FDLL.Add('mdstats.dll');
    FDLL.Add('sercor.dll');
    FDLL.Add('cross.dll');
    FDLL.Add('flowgen.dll');
    FDLL.Add('flowstat.dll');
    FDLL.Add('flowcor.dll');
    CreateDLLAgents;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Loads the function pointers from the DLL.
//
function TStomsaDLLManager.CreateDLLAgent(I: Integer): TDLLAgent;
const OPNAME = 'TStomsaDLLManager.CreateDLLAgent';
begin
  Result := nil;
  try
    case I of
      0 : Result := T_anlmk6.Create;
      1 : Result := T_mdstats.Create;
      2 : Result := T_sercor.Create;
      3 : Result := T_cross.Create;
      4 : Result := T_flowgen.Create;
      5 : Result := T_flowstat.Create;
      6 : Result := T_flowcor.Create;
    else
      Result := TDLLAgent.Create;
    end;

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//
// Calls the function.
//
procedure TStomsaDLLManager.FITMARG(N, ISTART, NZF, ICURV: PInteger; YIN, X, W, GA, DE, XL, XXX, CR, A1, S1: PDouble);
const OPNAME = 'TStomsaDLLManager.FITMARG';
begin
  try
    T_Anlmk6(FDLL.Objects[0]).FITMARG(N, ISTART, NZF, ICURV, YIN, X, W, GA, DE, XL, XXX, CR, A1, S1);

{
        AParams.N,
        AParams.ISTART,
        AParams.NZF,
        AParams.ICURV,
        AParams.YIN,
        AParams.X,
        AParams.W,
        AParams.GA,
        AParams.DE,
        AParams.XL,
        AParams.XXX,
        AParams.CR,
        AParams.A1,
        AParams.S1);
}

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.MDSTATS(NY, NNP, M1, M2: PInteger; Y, X, W, XA1, XSD1, XS31, XS41, WA1,
  WSD1, WS31, WS411, CL11, CL21, XX11, QCH11, XX21, QCH21: PDouble);
const OPNAME = 'TStomsaDLLManager.MDSTATS';
begin
  try
    T_mdstats(FDLL.Objects[1]).MDSTATS(NY, NNP, M1, M2, Y, X, W, XA1, XSD1, XS31, XS41, WA1, WSD1, WS31,
                                       WS411, CL11, CL21, XX11, QCH11, XX21, QCH21);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.SERCOR(NNPD, NYD, NFRD, NLAG, ITP: PInteger; W, YIN, G, D, XL, XXI, NCORR, NPORT, NCONF,
  NEXCD, PCORR, PPORT: PDouble; PFRD, PLAG: PInteger; PCONF, PEXCD: PDouble; NN, IT, IER: PInteger; FFF, FF, FFI, TTH,
  AIC, DIST, CRIT, QI: PDouble; CRNX, CRJX: PInteger; CRSX, CRP, AS1: PDouble);
const OPNAME = 'TStomsaDLLManager.SERCOR';
begin
  try
    T_sercor(FDLL.Objects[2]).SERCOR(NNPD, NYD, NFRD, NLAG, ITP, W, YIN, G, D, XL, XXI, NCORR, NPORT, NCONF, NEXCD,
                                      PCORR, PPORT, PFRD, PLAG, PCONF, PEXCD, NN, IT, IER, FFF, FF, FFI, TTH, AIC,
                                      DIST, CRIT, QI, CRNX, CRJX, CRSX, CRP, AS1);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.CROSS(NG, ND, ISTRTD: PInteger; FI1, FI2, TH1, TH2, YD, S0D, EG0D, EH1D, EH0D, BD, B0D, B1D,
  AD, CD, BBD, DD: PDouble);
const OPNAME = 'TStomsaDLLManager.CROSS';
begin
  try
    T_cross(FDLL.Objects[3]).CROSS(NG, ND, ISTRTD, FI1, FI2, TH1, TH2, YD, S0D,
                                   EG0D, EH1D, EH0D, BD, B0D, B1D, AD, CD, BBD, DD);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.FLOWGEN(var NR, NGG, NSYRD, IFLAGD, NKEYGD, NFGD, NGPD: Integer; NGSTORD, ITYPED, NZFD, NYRSD,
  ISTRYD, IKEYGD: PInteger; XSDel, PHID, THETAD, PZEROD, GAMMAD, DELTAD, XLAMD, XID, ZMEAND, ZSTDEVD, BD, AA1D, AA2D,
  ZZ1D, ZZ2D, B0D, B1D, AD, CD, FLOW: PDouble);
const OPNAME = 'TStomsaDLLManager.FLOWGEN';
begin
  try
    T_flowgen(FDLL.Objects[4]).FLOWGEN(NR, NGG, NSYRD, IFLAGD, NKEYGD, NFGD, NGPD, NGSTORD, ITYPED, NZFD, NYRSD, ISTRYD,
                                       IKEYGD, XSDel, PHID, THETAD, PZEROD, GAMMAD, DELTAD, XLAMD, XID, ZMEAND,
                                       ZSTDEVD, BD, AA1D, AA2D, ZZ1D, ZZ2D, B0D, B1D, AD, CD, FLOW);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.FLOWSTAT(NRSEQ, LWD, NYRS, LPD: PInteger; RILHD, RILSD, RJLHD, RJLSD, XH,
  FLOWD, APAD, GPAD, HGPAD, TSAMD, TSTMD, TSKMD, TSASD, TSTSD, TSKSD, TSCVD, TSTVD, TSKVD, GJAD, PGD, HSAD, PSD, X1D,
  PMAD, X2D, PMSD, HSKD, HJAKD, RELD, AMIND, WMIND, QRHD, QRSD, RSHD, RSSD, MNGPAD, MDXJD: PDouble);
const OPNAME = 'TStomsaDLLManager.FLOWSTAT';
begin
  try
    T_flowstat(FDLL.Objects[5]).FLOWSTAT(NRSEQ, LWD, NYRS, LPD, RILHD, RILSD, RJLHD, RJLSD, XH, FLOWD, APAD, GPAD,
                                         HGPAD, TSAMD, TSTMD, TSKMD, TSASD, TSTSD, TSKSD, TSCVD, TSTVD, TSKVD, GJAD,
                                         PGD, HSAD, PSD, X1D, PMAD, X2D, PMSD, HSKD, HJAKD, RELD, AMIND, WMIND, QRHD,
                                         QRSD, RSHD, RSSD, MNGPAD, MDXJD);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TStomsaDLLManager.MTHRNK(NRR, NSYRI, NYRS, ISTRY: PInteger; XH1, XH2, FLOWD1, FLOWD2, RSHO, RMEANO, PO: PDouble);
const OPNAME = 'TStomsaDLLManager.MTHRNK';
begin
  try
    T_flowcor(FDLL.Objects[6]).MTHRNK(NRR, NSYRI, NYRS, ISTRY, XH1, XH2, FLOWD1, FLOWD2, RSHO, RMEANO, PO);

  // Report exception.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

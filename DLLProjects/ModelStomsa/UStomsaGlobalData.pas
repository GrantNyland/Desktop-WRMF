unit UStomsaGlobalData;

interface

uses UStomsaData;

type
  //This object is used to store temporary data for the FLOWGEN dll
  TFlowGenData = class(TObject)
  public
    XSDEL : array_Hist_400;
    PHID, THETAD : array200_2F;
    PZEROD, GAMMAD, DELTAD, XLAMD, XID, ZMEAND, ZSTDEVD : array200F;
    BD : array200_200F;
    AA1D, AA2D, ZZ1D, ZZ2D : array200F;
    B0D, B1D, AD, CD : array200_200F;
    StochasticFlows : Array[1..10] of Array_Flows_400;
  end;

  //DECLARATION of external dll calls
//  procedure FITMARG(var N,ISTART,NZF,ICURV:integer; Yin,X,W:array1000F;
//                        GA,DE,XL,XX,CR:arrayCoefs; A1, S1 : Array1F);
//                        stdcall; external 'anlmk6.dll';
//  procedure MDSTATS(var NY,NNP,M1,M2: ArrayT1I; Y,X,W : array1000F; XA1,XSD1,XS31,XS41,WA1,WSD1,
//                        WS31,WS411,CL11,CL21,XX11,
//                        QCH11,XX21,QCH21 : Array1F);
//                        stdcall; external 'MDSTATS.dll';
//  procedure SERCOR(var NNP,NY,NFRD,NLAG,ITP:arrayT1I; W,Y:array1000F; G,D,XL,XXI:array1F;
//                       NCORR:arrayT2_30;
//                       NPORT,NCONF,NEXCD:array1F;
//                       PCORR:arrayT9_2_30; PPORT:array9F;
//                       PFRD,PLAG:arrayT9I; PCONF,PEXCD:array9F; NN,IT,IER:arrayT9I;
//                       FFF,FF:array9F; FFI,TTH:arrayT2_9;
//                       AIC,DIST,CRIT:array9F;
//                       QI:array12F;
//                       CRNX,CRJX: array13I;
//                       CRSX:array13F;
//                       CRP:array6F;
//                       HistoricalStart : arrayT2_9);
//                       stdcall; external 'sercor.dll';
//  procedure CROSS(var NG: integer; N, ISTRT : array500I;
//                      FI1, FI2, TH1, TH2 : array500F;
//                      YD : array500_1000F;
//                      S0D : array500_500F;
//                      EG0D, EH1D, EH0D : array500F;
//                      BD, B0D, B1D, AD, CD, BBD, DD : array500_500F);
//                      stdcall; external 'cross.dll';
//  procedure FLOWGEN(var NSYRD, IFLAGD, NKEYGD, NFGD, NGPD   :arrayT1I        ;
//                    var NGSTORD                             :array10I        ;
//                    var ITYPED, NZFD, NYRSD, ISTRYD, IKEYGD :array200I       ;
//                    var XSDEL                               :array_Hist_400  ;
//                    var PHID, THETAD                        :array200_2F     ;
//                    var PZEROD, GAMMAD, DELTAD, XLAMD, XID,
//                        ZMEAND, ZSTDEVD                     :array200F       ;
//                    var BD                                  :array200_200F   ;
//                    var AA1D, AA2D, ZZ1D, ZZ2D              :array200F       ;
//                    var B0D, B1D, AD, CD                    :array200_200F   ;
//                        //ACTUAL return variables from the DLL
//                    var FLOW1, FLOW2, FLOW3, FLOW4, FLOW5,
//                        FLOW6, FLOW7, FLOW8, FLOW9, FLOW10  : array_flows_400); stdcall; external 'FLOWGEN.dll';
//  procedure FLOWSTAT(var NRSEQ:arrayT1I;
//                         LWD:arrayT1I;
//                         NYRS:arrayT2I; LPD:array10I;
//                         RILHD:array5F; RILSD:array7_5F;
//                         RJLHD:array5F; RJLSD:array7_5F;
//                         XH:array12_100F;
//                         FLOWD:array101_400_12F;
//                         APAD, GPAD:array101_20F;
//                         HGPAD:array20F;
//                         TSAMD, TSTMD, TSKMD, TSASD, TSTSD,
//                         TSKSD, TSCVD, TSTVD, TSKVD, GJAD: array1F;
//                         PGD: array7F; HSAD: array1F;
//                         PSD: array7F; X1D:array12F;
//                         PMAD:array12_7F; X2D:array12F;
//                         PMSD:array12_7F; HSKD:array1F;
//                         HJAKD:array1F; RELD:array5F;
//                         AMIND:array10F;
//                         WMIND: array7_10F;
//                         QRHD:array5F; QRSD:array7_5F;
//                         RSHD:array5F; RSSD:array7_5F;
//                         MNGPAD, MDXJD:array20F);
//                         stdcall; external 'FLOWSTAT.dll';
//  procedure MTHRNK(var   NRR, NSYRI:arrayT1I;
//                         NYRS, ISTRY:arrayT2I;
//                         XH1, XH2:array13_400F; //Y2K
//                         FLOWD1, FLOWD2:array101_400_13F; //Y2K
//                         RSHO, RMEANO:array13F;
//                         PO:array7_13F);
//                         stdcall; external 'FLOWCOR.dll';

var
  ProjectName : WideString;

implementation

//uses
//  SysUtils,
//  UErrorHandlingOperations;


//  function  XNORM(var p : double ): double;
//                      stdcall; external 'xnorm.dll';
//  function  AJV(var ITYPE,IFAULT :integer; SNVR,GAMMA,DELTA,XLAM,XI:arrayT): double;
//                    stdcall; external 'ajv.dll';
//  function  SNV(var ITYPE,IFAULT :integer; AJV,GAMMA,DELTA,XLAM,XI:arrayT): double;
//                    stdcall; external 'snv.dll';


  {procedure FLOWGEN(var NSYRD, IFLAGD, NKEYGD, NFGD, NGPD : arrayT1I;
                        NGSTORD : array10I;
                        ITYPED, NZFD, NYRSD, ISTRYD, IKEYGD: array200I;
                        XSDEL : array_Hist_400;
                        PHID, THETAD : array200_2F;
                        PZEROD, GAMMAD, DELTAD, XLAMD, XID, ZMEAND, ZSTDEVD : array200F;
                        BD : array200_200F;
                        AA1D, AA2D, ZZ1D, ZZ2D : array200F;
                        B0D, B1D, AD, CD : array200_200F;
                        //ACTUAL return variables from the DLL
                        FLOW1, FLOW2, FLOW3, FLOW4, FLOW5, FLOW6, FLOW7,
                        FLOW8, FLOW9, FLOW10 : array_flows_400);
                        stdcall; external 'FLOWGEN.dll';}





end.

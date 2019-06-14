unit RainfallCom_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 13/08/2009 10:33:23 from Type Library described below.

// ************************************************************************  //
// Type Lib: R:\WRMF\Source\DLLProjects\ModelRainfallData\RainfallCom\RainfallCom.tlb (1)
// LIBID: {C2EB46E4-88B9-4485-A9B3-89ACEF8A60CF}
// LCID: 0
// Helpfile: 
// HelpString: RainfallCom Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\STDOLE2.TLB)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Vcl.Graphics, System.Win.StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  RainfallComMajorVersion = 1;
  RainfallComMinorVersion = 0;

  LIBID_RainfallCom: TGUID = '{C2EB46E4-88B9-4485-A9B3-89ACEF8A60CF}';

  IID_IRainfallComObject: TGUID = '{5B0D324F-7F4A-417A-8F8A-5413B9B10241}';
  CLASS_RainfallComObject: TGUID = '{803AAF27-D5D2-440B-BB17-3AFE7041E9C3}';
  IID_IRainGauge: TGUID = '{88D5FFF2-18CD-4F27-94EE-500881DE5F63}';
  IID_IRainGaugeList: TGUID = '{6A35D8B2-EBB6-4998-BE14-CF913F327E4A}';
  IID_IYearlyData: TGUID = '{FCD2A3B4-3438-4803-B350-FD4A6707099A}';
  IID_IRainfallData: TGUID = '{31933C98-E20E-471C-94EC-292ECB8E633E}';
  IID_IPatchData: TGUID = '{B8B45193-F069-46BC-B233-590E95E1B1E1}';
  IID_IRainfallDataSplit: TGUID = '{78B959E4-6155-413A-B07B-37584161CBC8}';
  IID_IStationData: TGUID = '{AD7FA5A8-B76D-48FE-82C8-B46707030330}';
  IID_IRainfallModelData: TGUID = '{ADBAB5AE-3263-4B38-A713-4116D3912A07}';
  IID_IRainfallModel: TGUID = '{989135AE-A86D-46EB-8874-BBD7A51116FC}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IRainfallComObject = interface;
  IRainGauge = interface;
  IRainGaugeDisp = dispinterface;
  IRainGaugeList = interface;
  IRainGaugeListDisp = dispinterface;
  IYearlyData = interface;
  IYearlyDataDisp = dispinterface;
  IRainfallData = interface;
  IRainfallDataDisp = dispinterface;
  IPatchData = interface;
  IPatchDataDisp = dispinterface;
  IRainfallDataSplit = interface;
  IRainfallDataSplitDisp = dispinterface;
  IStationData = interface;
  IStationDataDisp = dispinterface;
  IRainfallModelData = interface;
  IRainfallModelDataDisp = dispinterface;
  IRainfallModel = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RainfallComObject = IRainfallComObject;


// *********************************************************************//
// Interface: IRainfallComObject
// Flags:     (256) OleAutomation
// GUID:      {5B0D324F-7F4A-417A-8F8A-5413B9B10241}
// *********************************************************************//
  IRainfallComObject = interface(IUnknown)
    ['{5B0D324F-7F4A-417A-8F8A-5413B9B10241}']
  end;

// *********************************************************************//
// Interface: IRainGauge
// Flags:     (320) Dual OleAutomation
// GUID:      {88D5FFF2-18CD-4F27-94EE-500881DE5F63}
// *********************************************************************//
  IRainGauge = interface(IUnknown)
    ['{88D5FFF2-18CD-4F27-94EE-500881DE5F63}']
    function Get_Group: WideString; safecall;
    function Get_GaugeNumber: WideString; safecall;
    function Get_GaugeName: WideString; safecall;
    function Get_GaugeID: Integer; safecall;
    function Get_Latitude: Integer; safecall;
    function Get_Longitude: Integer; safecall;
    function Get_Selected: WordBool; safecall;
    procedure Set_Selected(Value: WordBool); safecall;
    function Get_IsInWR90: WordBool; safecall;
    function Get_ListIndex: Integer; safecall;
    property Group: WideString read Get_Group;
    property GaugeNumber: WideString read Get_GaugeNumber;
    property GaugeName: WideString read Get_GaugeName;
    property GaugeID: Integer read Get_GaugeID;
    property Latitude: Integer read Get_Latitude;
    property Longitude: Integer read Get_Longitude;
    property Selected: WordBool read Get_Selected write Set_Selected;
    property IsInWR90: WordBool read Get_IsInWR90;
    property ListIndex: Integer read Get_ListIndex;
  end;

// *********************************************************************//
// DispIntf:  IRainGaugeDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {88D5FFF2-18CD-4F27-94EE-500881DE5F63}
// *********************************************************************//
  IRainGaugeDisp = dispinterface
    ['{88D5FFF2-18CD-4F27-94EE-500881DE5F63}']
    property Group: WideString readonly dispid 101;
    property GaugeNumber: WideString readonly dispid 102;
    property GaugeName: WideString readonly dispid 103;
    property GaugeID: Integer readonly dispid 104;
    property Latitude: Integer readonly dispid 105;
    property Longitude: Integer readonly dispid 106;
    property Selected: WordBool dispid 107;
    property IsInWR90: WordBool readonly dispid 108;
    property ListIndex: Integer readonly dispid 109;
  end;

// *********************************************************************//
// Interface: IRainGaugeList
// Flags:     (320) Dual OleAutomation
// GUID:      {6A35D8B2-EBB6-4998-BE14-CF913F327E4A}
// *********************************************************************//
  IRainGaugeList = interface(IUnknown)
    ['{6A35D8B2-EBB6-4998-BE14-CF913F327E4A}']
    procedure DeSelectAll; safecall;
    procedure SelectByDistance(ALatitude: Double; ALongitude: Double; ADistance: Double; 
                               AReplaceSelection: WordBool); safecall;
    procedure SelectByRectangle(ALeft: Integer; ATop: Integer; ABottom: Integer; ARight: Integer; 
                                AReplaceSelection: WordBool); safecall;
    procedure SelectByName(const AName: WideString; AReplaceSelection: WordBool); safecall;
    procedure SelectByNumber(const ANumber: WideString; AReplaceSelection: WordBool); safecall;
    function TotalCount: Integer; safecall;
    function GetSelectedGauges: WideString; safecall;
    function SelectedCount: Integer; safecall;
    procedure SaveToDB; safecall;
    procedure SelectByStationID(AGaugeID: Integer; AReplaceSelection: WordBool); safecall;
    function GetGaugeByIndex(AIndex: Integer): IRainGauge; safecall;
    function GetGaugeByNumber(const ANumber: WideString): IRainGauge; safecall;
  end;

// *********************************************************************//
// DispIntf:  IRainGaugeListDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6A35D8B2-EBB6-4998-BE14-CF913F327E4A}
// *********************************************************************//
  IRainGaugeListDisp = dispinterface
    ['{6A35D8B2-EBB6-4998-BE14-CF913F327E4A}']
    procedure DeSelectAll; dispid 101;
    procedure SelectByDistance(ALatitude: Double; ALongitude: Double; ADistance: Double; 
                               AReplaceSelection: WordBool); dispid 104;
    procedure SelectByRectangle(ALeft: Integer; ATop: Integer; ABottom: Integer; ARight: Integer; 
                                AReplaceSelection: WordBool); dispid 105;
    procedure SelectByName(const AName: WideString; AReplaceSelection: WordBool); dispid 106;
    procedure SelectByNumber(const ANumber: WideString; AReplaceSelection: WordBool); dispid 107;
    function TotalCount: Integer; dispid 108;
    function GetSelectedGauges: WideString; dispid 109;
    function SelectedCount: Integer; dispid 110;
    procedure SaveToDB; dispid 112;
    procedure SelectByStationID(AGaugeID: Integer; AReplaceSelection: WordBool); dispid 113;
    function GetGaugeByIndex(AIndex: Integer): IRainGauge; dispid 111;
    function GetGaugeByNumber(const ANumber: WideString): IRainGauge; dispid 102;
  end;

// *********************************************************************//
// Interface: IYearlyData
// Flags:     (320) Dual OleAutomation
// GUID:      {FCD2A3B4-3438-4803-B350-FD4A6707099A}
// *********************************************************************//
  IYearlyData = interface(IUnknown)
    ['{FCD2A3B4-3438-4803-B350-FD4A6707099A}']
    function Get_Year: Integer; safecall;
    procedure Set_Year(Value: Integer); safecall;
    function Get_HydroYear: WideString; safecall;
    procedure Set_HydroYear(const Value: WideString); safecall;
    function Get_Total: Double; safecall;
    procedure Set_Total(Value: Double); safecall;
    function Get_MonthlyRainfall(AMonth: Integer): Double; safecall;
    procedure Set_MonthlyRainfall(AMonth: Integer; Value: Double); safecall;
    function Get_MonthlyPatchSign(AMonth: Integer): WideString; safecall;
    procedure Set_MonthlyPatchSign(AMonth: Integer; const Value: WideString); safecall;
    function Get_MissingMonths: Integer; safecall;
    procedure Set_MissingMonths(Value: Integer); safecall;
    function Get_HasUnreliableData: WordBool; safecall;
    function Get_UnreliableMonths: Integer; safecall;
    procedure Set_UnreliableMonths(Value: Integer); safecall;
    function Get_HasMissingData: WordBool; safecall;
    function Get_MonthlyScaledDown(AMonth: Integer): Double; safecall;
    procedure Set_MonthlyScaledDown(AMonth: Integer; Value: Double); safecall;
    property Year: Integer read Get_Year write Set_Year;
    property HydroYear: WideString read Get_HydroYear write Set_HydroYear;
    property Total: Double read Get_Total write Set_Total;
    property MonthlyRainfall[AMonth: Integer]: Double read Get_MonthlyRainfall write Set_MonthlyRainfall;
    property MonthlyPatchSign[AMonth: Integer]: WideString read Get_MonthlyPatchSign write Set_MonthlyPatchSign;
    property MissingMonths: Integer read Get_MissingMonths write Set_MissingMonths;
    property HasUnreliableData: WordBool read Get_HasUnreliableData;
    property UnreliableMonths: Integer read Get_UnreliableMonths write Set_UnreliableMonths;
    property HasMissingData: WordBool read Get_HasMissingData;
    property MonthlyScaledDown[AMonth: Integer]: Double read Get_MonthlyScaledDown write Set_MonthlyScaledDown;
  end;

// *********************************************************************//
// DispIntf:  IYearlyDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {FCD2A3B4-3438-4803-B350-FD4A6707099A}
// *********************************************************************//
  IYearlyDataDisp = dispinterface
    ['{FCD2A3B4-3438-4803-B350-FD4A6707099A}']
    property Year: Integer dispid 101;
    property HydroYear: WideString dispid 102;
    property Total: Double dispid 103;
    property MonthlyRainfall[AMonth: Integer]: Double dispid 104;
    property MonthlyPatchSign[AMonth: Integer]: WideString dispid 105;
    property MissingMonths: Integer dispid 106;
    property HasUnreliableData: WordBool readonly dispid 107;
    property UnreliableMonths: Integer dispid 108;
    property HasMissingData: WordBool readonly dispid 109;
    property MonthlyScaledDown[AMonth: Integer]: Double dispid 110;
  end;

// *********************************************************************//
// Interface: IRainfallData
// Flags:     (320) Dual OleAutomation
// GUID:      {31933C98-E20E-471C-94EC-292ECB8E633E}
// *********************************************************************//
  IRainfallData = interface(IUnknown)
    ['{31933C98-E20E-471C-94EC-292ECB8E633E}']
    function Get_StationID: Integer; safecall;
    procedure Set_StationID(Value: Integer); safecall;
    function Get_StationNumber: WideString; safecall;
    procedure Set_StationNumber(const Value: WideString); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_EndYear: Integer; safecall;
    procedure Set_EndYear(Value: Integer); safecall;
    function Get_HydroStartYear: Integer; safecall;
    procedure Set_HydroStartYear(Value: Integer); safecall;
    function Get_HydroEndYear: Integer; safecall;
    procedure Set_HydroEndYear(Value: Integer); safecall;
    function Get_GrandTotal: Double; safecall;
    procedure Set_GrandTotal(Value: Double); safecall;
    function Get_StdDeviation: Double; safecall;
    procedure Set_StdDeviation(Value: Double); safecall;
    function Get_CV: Double; safecall;
    procedure Set_CV(Value: Double); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_NrOfMissingMonths: Integer; safecall;
    procedure Set_NrOfMissingMonths(Value: Integer); safecall;
    function HydroYearsCount: Integer; safecall;
    function HydroStartDate: TDateTime; safecall;
    function HydroEndDate: TDateTime; safecall;
    procedure GetBaseDataForYearAndMonth(AYear: Integer; AMonth: Integer; var ARainfall: Double; 
                                         var APatchSign: WideString); safecall;
    function Get_XGrandTotal: Double; safecall;
    procedure Set_XGrandTotal(Value: Double); safecall;
    function Get_XStdDeviation: Double; safecall;
    procedure Set_XStdDeviation(Value: Double); safecall;
    function Get_XMAP: Double; safecall;
    procedure Set_XMAP(Value: Double); safecall;
    function Get_XCV: Double; safecall;
    procedure Set_XCV(Value: Double); safecall;
    function Get_YearMin: Double; safecall;
    procedure Set_YearMin(Value: Double); safecall;
    function Get_YearMax: Double; safecall;
    procedure Set_YearMax(Value: Double); safecall;
    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_XYearMin: Double; safecall;
    procedure Set_XYearMin(Value: Double); safecall;
    function Get_XYearMax: Double; safecall;
    procedure Set_XYearMax(Value: Double); safecall;
    function Get_XYearCount: Integer; safecall;
    procedure Set_XYearCount(Value: Integer); safecall;
    function Get_MonthMax(AMonth: Integer): Double; safecall;
    procedure Set_MonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMin(AMonth: Integer): Double; safecall;
    procedure Set_MonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_MonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_MonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_MonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_MonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_MonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_MonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_MonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_MonthCV(AMonth: Integer): Double; safecall;
    procedure Set_MonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMin(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMax(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_XMonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_XMonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_XMonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_XMonthCV(AMonth: Integer): Double; safecall;
    procedure Set_XMonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_HighlightMonths: WideString; safecall;
    function Get_HighlightYears: WideString; safecall;
    function GetHydroYearDataByIndex(AIndex: Integer): IYearlyData; safecall;
    function GetHydroYearDataByYear(AYear: Integer): IYearlyData; safecall;
    property StationID: Integer read Get_StationID write Set_StationID;
    property StationNumber: WideString read Get_StationNumber write Set_StationNumber;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property EndYear: Integer read Get_EndYear write Set_EndYear;
    property HydroStartYear: Integer read Get_HydroStartYear write Set_HydroStartYear;
    property HydroEndYear: Integer read Get_HydroEndYear write Set_HydroEndYear;
    property GrandTotal: Double read Get_GrandTotal write Set_GrandTotal;
    property StdDeviation: Double read Get_StdDeviation write Set_StdDeviation;
    property CV: Double read Get_CV write Set_CV;
    property MAP: Double read Get_MAP write Set_MAP;
    property NrOfMissingMonths: Integer read Get_NrOfMissingMonths write Set_NrOfMissingMonths;
    property XGrandTotal: Double read Get_XGrandTotal write Set_XGrandTotal;
    property XStdDeviation: Double read Get_XStdDeviation write Set_XStdDeviation;
    property XMAP: Double read Get_XMAP write Set_XMAP;
    property XCV: Double read Get_XCV write Set_XCV;
    property YearMin: Double read Get_YearMin write Set_YearMin;
    property YearMax: Double read Get_YearMax write Set_YearMax;
    property YearCount: Integer read Get_YearCount write Set_YearCount;
    property XYearMin: Double read Get_XYearMin write Set_XYearMin;
    property XYearMax: Double read Get_XYearMax write Set_XYearMax;
    property XYearCount: Integer read Get_XYearCount write Set_XYearCount;
    property MonthMax[AMonth: Integer]: Double read Get_MonthMax write Set_MonthMax;
    property MonthMin[AMonth: Integer]: Double read Get_MonthMin write Set_MonthMin;
    property MonthCount[AMonth: Integer]: Integer read Get_MonthCount write Set_MonthCount;
    property MonthTotal[AMonth: Integer]: Double read Get_MonthTotal write Set_MonthTotal;
    property MonthMAP[AMonth: Integer]: Double read Get_MonthMAP write Set_MonthMAP;
    property MonthStdDev[AMonth: Integer]: Double read Get_MonthStdDev write Set_MonthStdDev;
    property MonthCV[AMonth: Integer]: Double read Get_MonthCV write Set_MonthCV;
    property XMonthMin[AMonth: Integer]: Double read Get_XMonthMin write Set_XMonthMin;
    property XMonthMax[AMonth: Integer]: Double read Get_XMonthMax write Set_XMonthMax;
    property XMonthTotal[AMonth: Integer]: Double read Get_XMonthTotal write Set_XMonthTotal;
    property XMonthMAP[AMonth: Integer]: Double read Get_XMonthMAP write Set_XMonthMAP;
    property XMonthStdDev[AMonth: Integer]: Double read Get_XMonthStdDev write Set_XMonthStdDev;
    property XMonthCount[AMonth: Integer]: Integer read Get_XMonthCount write Set_XMonthCount;
    property XMonthCV[AMonth: Integer]: Double read Get_XMonthCV write Set_XMonthCV;
    property HighlightMonths: WideString read Get_HighlightMonths;
    property HighlightYears: WideString read Get_HighlightYears;
  end;

// *********************************************************************//
// DispIntf:  IRainfallDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {31933C98-E20E-471C-94EC-292ECB8E633E}
// *********************************************************************//
  IRainfallDataDisp = dispinterface
    ['{31933C98-E20E-471C-94EC-292ECB8E633E}']
    property StationID: Integer dispid 101;
    property StationNumber: WideString dispid 102;
    property StartYear: Integer dispid 103;
    property EndYear: Integer dispid 104;
    property HydroStartYear: Integer dispid 105;
    property HydroEndYear: Integer dispid 106;
    property GrandTotal: Double dispid 107;
    property StdDeviation: Double dispid 108;
    property CV: Double dispid 109;
    property MAP: Double dispid 110;
    property NrOfMissingMonths: Integer dispid 111;
    function HydroYearsCount: Integer; dispid 114;
    function HydroStartDate: TDateTime; dispid 115;
    function HydroEndDate: TDateTime; dispid 116;
    procedure GetBaseDataForYearAndMonth(AYear: Integer; AMonth: Integer; var ARainfall: Double; 
                                         var APatchSign: WideString); dispid 118;
    property XGrandTotal: Double dispid 119;
    property XStdDeviation: Double dispid 120;
    property XMAP: Double dispid 121;
    property XCV: Double dispid 122;
    property YearMin: Double dispid 123;
    property YearMax: Double dispid 124;
    property YearCount: Integer dispid 125;
    property XYearMin: Double dispid 126;
    property XYearMax: Double dispid 127;
    property XYearCount: Integer dispid 128;
    property MonthMax[AMonth: Integer]: Double dispid 129;
    property MonthMin[AMonth: Integer]: Double dispid 130;
    property MonthCount[AMonth: Integer]: Integer dispid 131;
    property MonthTotal[AMonth: Integer]: Double dispid 132;
    property MonthMAP[AMonth: Integer]: Double dispid 133;
    property MonthStdDev[AMonth: Integer]: Double dispid 134;
    property MonthCV[AMonth: Integer]: Double dispid 135;
    property XMonthMin[AMonth: Integer]: Double dispid 136;
    property XMonthMax[AMonth: Integer]: Double dispid 137;
    property XMonthTotal[AMonth: Integer]: Double dispid 138;
    property XMonthMAP[AMonth: Integer]: Double dispid 139;
    property XMonthStdDev[AMonth: Integer]: Double dispid 140;
    property XMonthCount[AMonth: Integer]: Integer dispid 141;
    property XMonthCV[AMonth: Integer]: Double dispid 142;
    property HighlightMonths: WideString readonly dispid 112;
    property HighlightYears: WideString readonly dispid 113;
    function GetHydroYearDataByIndex(AIndex: Integer): IYearlyData; dispid 144;
    function GetHydroYearDataByYear(AYear: Integer): IYearlyData; dispid 117;
  end;

// *********************************************************************//
// Interface: IPatchData
// Flags:     (320) Dual OleAutomation
// GUID:      {B8B45193-F069-46BC-B233-590E95E1B1E1}
// *********************************************************************//
  IPatchData = interface(IUnknown)
    ['{B8B45193-F069-46BC-B233-590E95E1B1E1}']
    function Get_PatchID: Integer; safecall;
    procedure Set_PatchID(Value: Integer); safecall;
    function Get_PatchTypeID: Integer; safecall;
    procedure Set_PatchTypeID(Value: Integer); safecall;
    function Get_PatchName: WideString; safecall;
    procedure Set_PatchName(const Value: WideString); safecall;
    function Get_PatchMultiple: WordBool; safecall;
    procedure Set_PatchMultiple(Value: WordBool); safecall;
    function Get_ClassRInputFileName: WideString; safecall;
    procedure Set_ClassRInputFileName(const Value: WideString); safecall;
    function Get_ClassROutputFileName: WideString; safecall;
    procedure Set_ClassROutputFileName(const Value: WideString); safecall;
    function Get_PatchRInputFileName: WideString; safecall;
    procedure Set_PatchRInputFileName(const Value: WideString); safecall;
    function Get_PatchRPrintFileName: WideString; safecall;
    procedure Set_PatchRPrintFileName(const Value: WideString); safecall;
    function Get_PatchRPlotFileName: WideString; safecall;
    procedure Set_PatchRPlotFileName(const Value: WideString); safecall;
    function Get_PatchStartYear: Integer; safecall;
    procedure Set_PatchStartYear(Value: Integer); safecall;
    function Get_PatchEndYear: Integer; safecall;
    procedure Set_PatchEndYear(Value: Integer); safecall;
    function Get_ClassRInputData: WideString; safecall;
    procedure Set_ClassRInputData(const Value: WideString); safecall;
    function Get_ClassROutputData: WideString; safecall;
    procedure Set_ClassROutputData(const Value: WideString); safecall;
    function Get_PatchRPrintData: WideString; safecall;
    procedure Set_PatchRPrintData(const Value: WideString); safecall;
    function Get_PatchRPlotData: WideString; safecall;
    procedure Set_PatchRPlotData(const Value: WideString); safecall;
    function Get_ChangeDate: TDateTime; safecall;
    procedure Set_ChangeDate(Value: TDateTime); safecall;
    function Get_ClassRDate: TDateTime; safecall;
    procedure Set_ClassRDate(Value: TDateTime); safecall;
    function Get_PatchRInputDate: TDateTime; safecall;
    procedure Set_PatchRInputDate(Value: TDateTime); safecall;
    function Get_PatchROutputDate: TDateTime; safecall;
    procedure Set_PatchROutputDate(Value: TDateTime); safecall;
    function Get_PatchRPrintDate: TDateTime; safecall;
    procedure Set_PatchRPrintDate(Value: TDateTime); safecall;
    function Get_PatchRPlotDate: TDateTime; safecall;
    procedure Set_PatchRPlotDate(Value: TDateTime); safecall;
    function Get_PatchRInputData: WideString; safecall;
    procedure Set_PatchRInputData(const Value: WideString); safecall;
    function Get_PatchROutputData: WideString; safecall;
    function SourcesCount: Integer; safecall;
    procedure GetSourceInfoByIndex(AIndex: Integer; out AStationID: Integer; out APatchID: Integer; 
                                   out ATarget: WideString; out AStartYear: Integer; 
                                   out AEndYear: Integer); safecall;
    procedure DeleteMonthlyPatchData; safecall;
    procedure SavePatchROutputData(AStationID: Integer; const AValue: WideString); safecall;
    procedure SavePATFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); safecall;
    function Get_SourceInfo: WideString; safecall;
    procedure Set_SourceInfo(const Value: WideString); safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    procedure GetSourceInfoByStationID(AStationID: Integer; out APatchID: Integer; 
                                       out ATarget: WideString; out AStartYear: Integer; 
                                       out AEndYear: Integer); safecall;
    procedure SaveRAWFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); safecall;
    procedure SaveMPFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); safecall;
    function Get_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                       AMonth: Integer): WordBool; safecall;
    function Set_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                       AMonth: Integer; AStatus: WordBool): WordBool; safecall;
    function Save_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                        AMonth: Integer; AIdentifier: Integer; AStatus: WordBool): WordBool; safecall;
    function RainfallData: IRainfallData; safecall;
    function PopulateCommonBlockRAWData(AStartYear: Integer; AEndYear: Integer; 
                                        AGaugeIndex: Integer; AAddIndex: Integer; 
                                        var ABEGY: Integer; var AENDY: Integer): WordBool; safecall;
    property PatchID: Integer read Get_PatchID write Set_PatchID;
    property PatchTypeID: Integer read Get_PatchTypeID write Set_PatchTypeID;
    property PatchName: WideString read Get_PatchName write Set_PatchName;
    property PatchMultiple: WordBool read Get_PatchMultiple write Set_PatchMultiple;
    property ClassRInputFileName: WideString read Get_ClassRInputFileName write Set_ClassRInputFileName;
    property ClassROutputFileName: WideString read Get_ClassROutputFileName write Set_ClassROutputFileName;
    property PatchRInputFileName: WideString read Get_PatchRInputFileName write Set_PatchRInputFileName;
    property PatchRPrintFileName: WideString read Get_PatchRPrintFileName write Set_PatchRPrintFileName;
    property PatchRPlotFileName: WideString read Get_PatchRPlotFileName write Set_PatchRPlotFileName;
    property PatchStartYear: Integer read Get_PatchStartYear write Set_PatchStartYear;
    property PatchEndYear: Integer read Get_PatchEndYear write Set_PatchEndYear;
    property ClassRInputData: WideString read Get_ClassRInputData write Set_ClassRInputData;
    property ClassROutputData: WideString read Get_ClassROutputData write Set_ClassROutputData;
    property PatchRPrintData: WideString read Get_PatchRPrintData write Set_PatchRPrintData;
    property PatchRPlotData: WideString read Get_PatchRPlotData write Set_PatchRPlotData;
    property ChangeDate: TDateTime read Get_ChangeDate write Set_ChangeDate;
    property ClassRDate: TDateTime read Get_ClassRDate write Set_ClassRDate;
    property PatchRInputDate: TDateTime read Get_PatchRInputDate write Set_PatchRInputDate;
    property PatchROutputDate: TDateTime read Get_PatchROutputDate write Set_PatchROutputDate;
    property PatchRPrintDate: TDateTime read Get_PatchRPrintDate write Set_PatchRPrintDate;
    property PatchRPlotDate: TDateTime read Get_PatchRPlotDate write Set_PatchRPlotDate;
    property PatchRInputData: WideString read Get_PatchRInputData write Set_PatchRInputData;
    property PatchROutputData: WideString read Get_PatchROutputData;
    property SourceInfo: WideString read Get_SourceInfo write Set_SourceInfo;
  end;

// *********************************************************************//
// DispIntf:  IPatchDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {B8B45193-F069-46BC-B233-590E95E1B1E1}
// *********************************************************************//
  IPatchDataDisp = dispinterface
    ['{B8B45193-F069-46BC-B233-590E95E1B1E1}']
    property PatchID: Integer dispid 131;
    property PatchTypeID: Integer dispid 132;
    property PatchName: WideString dispid 133;
    property PatchMultiple: WordBool dispid 134;
    property ClassRInputFileName: WideString dispid 135;
    property ClassROutputFileName: WideString dispid 136;
    property PatchRInputFileName: WideString dispid 137;
    property PatchRPrintFileName: WideString dispid 138;
    property PatchRPlotFileName: WideString dispid 139;
    property PatchStartYear: Integer dispid 140;
    property PatchEndYear: Integer dispid 141;
    property ClassRInputData: WideString dispid 142;
    property ClassROutputData: WideString dispid 143;
    property PatchRPrintData: WideString dispid 144;
    property PatchRPlotData: WideString dispid 145;
    property ChangeDate: TDateTime dispid 146;
    property ClassRDate: TDateTime dispid 147;
    property PatchRInputDate: TDateTime dispid 148;
    property PatchROutputDate: TDateTime dispid 149;
    property PatchRPrintDate: TDateTime dispid 150;
    property PatchRPlotDate: TDateTime dispid 151;
    property PatchRInputData: WideString dispid 152;
    property PatchROutputData: WideString readonly dispid 153;
    function SourcesCount: Integer; dispid 201;
    procedure GetSourceInfoByIndex(AIndex: Integer; out AStationID: Integer; out APatchID: Integer; 
                                   out ATarget: WideString; out AStartYear: Integer; 
                                   out AEndYear: Integer); dispid 202;
    procedure DeleteMonthlyPatchData; dispid 203;
    procedure SavePatchROutputData(AStationID: Integer; const AValue: WideString); dispid 204;
    procedure SavePATFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); dispid 205;
    property SourceInfo: WideString dispid 206;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 102;
    procedure GetSourceInfoByStationID(AStationID: Integer; out APatchID: Integer; 
                                       out ATarget: WideString; out AStartYear: Integer; 
                                       out AEndYear: Integer); dispid 103;
    procedure SaveRAWFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); dispid 104;
    procedure SaveMPFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); dispid 105;
    function Get_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                       AMonth: Integer): WordBool; dispid 106;
    function Set_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                       AMonth: Integer; AStatus: WordBool): WordBool; dispid 107;
    function Save_PatchScaledDownStatus(AStationID: Integer; APatchID: Integer; AYear: Integer; 
                                        AMonth: Integer; AIdentifier: Integer; AStatus: WordBool): WordBool; dispid 108;
    function RainfallData: IRainfallData; dispid 101;
    function PopulateCommonBlockRAWData(AStartYear: Integer; AEndYear: Integer; 
                                        AGaugeIndex: Integer; AAddIndex: Integer; 
                                        var ABEGY: Integer; var AENDY: Integer): WordBool; dispid 109;
  end;

// *********************************************************************//
// Interface: IRainfallDataSplit
// Flags:     (320) Dual OleAutomation
// GUID:      {78B959E4-6155-413A-B07B-37584161CBC8}
// *********************************************************************//
  IRainfallDataSplit = interface(IUnknown)
    ['{78B959E4-6155-413A-B07B-37584161CBC8}']
    function Get_HydroStartYear: Integer; safecall;
    procedure Set_HydroStartYear(Value: Integer); safecall;
    function Get_HydroEndYear: Integer; safecall;
    procedure Set_HydroEndYear(Value: Integer); safecall;
    function Get_NrOfMissingMonths: Integer; safecall;
    procedure Set_NrOfMissingMonths(Value: Integer); safecall;
    function Get_GrandTotal: Double; safecall;
    procedure Set_GrandTotal(Value: Double); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_StdDeviation: Double; safecall;
    procedure Set_StdDeviation(Value: Double); safecall;
    function Get_CV: Double; safecall;
    procedure Set_CV(Value: Double); safecall;
    function Get_XGrandTotal: Double; safecall;
    procedure Set_XGrandTotal(Value: Double); safecall;
    function Get_XMAP: Double; safecall;
    procedure Set_XMAP(Value: Double); safecall;
    function Get_XStdDeviation: Double; safecall;
    procedure Set_XStdDeviation(Value: Double); safecall;
    function Get_XCV: Double; safecall;
    procedure Set_XCV(Value: Double); safecall;
    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_YearMin: Double; safecall;
    procedure Set_YearMin(Value: Double); safecall;
    function Get_YearMax: Double; safecall;
    procedure Set_YearMax(Value: Double); safecall;
    function Get_XYearCount: Integer; safecall;
    procedure Set_XYearCount(Value: Integer); safecall;
    function Get_XYearMin: Double; safecall;
    procedure Set_XYearMin(Value: Double); safecall;
    function Get_XYearMax: Double; safecall;
    procedure Set_XYearMax(Value: Double); safecall;
    function Get_MonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_MonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_MonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_MonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMin(AMonth: Integer): Double; safecall;
    procedure Set_MonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMax(AMonth: Integer): Double; safecall;
    procedure Set_MonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_MonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_MonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_MonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_MonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_MonthCV(AMonth: Integer): Double; safecall;
    procedure Set_MonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCount(AMonth: Integer): Integer; safecall;
    procedure Set_XMonthCount(AMonth: Integer; Value: Integer); safecall;
    function Get_XMonthTotal(AMonth: Integer): Double; safecall;
    procedure Set_XMonthTotal(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMin(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMin(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMax(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMax(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthMAP(AMonth: Integer): Double; safecall;
    procedure Set_XMonthMAP(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthStdDev(AMonth: Integer): Double; safecall;
    procedure Set_XMonthStdDev(AMonth: Integer; Value: Double); safecall;
    function Get_XMonthCV(AMonth: Integer): Double; safecall;
    procedure Set_XMonthCV(AMonth: Integer; Value: Double); safecall;
    function Get_HighlightMonths: WideString; safecall;
    function Get_HighlightYears: WideString; safecall;
    property HydroStartYear: Integer read Get_HydroStartYear write Set_HydroStartYear;
    property HydroEndYear: Integer read Get_HydroEndYear write Set_HydroEndYear;
    property NrOfMissingMonths: Integer read Get_NrOfMissingMonths write Set_NrOfMissingMonths;
    property GrandTotal: Double read Get_GrandTotal write Set_GrandTotal;
    property MAP: Double read Get_MAP write Set_MAP;
    property StdDeviation: Double read Get_StdDeviation write Set_StdDeviation;
    property CV: Double read Get_CV write Set_CV;
    property XGrandTotal: Double read Get_XGrandTotal write Set_XGrandTotal;
    property XMAP: Double read Get_XMAP write Set_XMAP;
    property XStdDeviation: Double read Get_XStdDeviation write Set_XStdDeviation;
    property XCV: Double read Get_XCV write Set_XCV;
    property YearCount: Integer read Get_YearCount write Set_YearCount;
    property YearMin: Double read Get_YearMin write Set_YearMin;
    property YearMax: Double read Get_YearMax write Set_YearMax;
    property XYearCount: Integer read Get_XYearCount write Set_XYearCount;
    property XYearMin: Double read Get_XYearMin write Set_XYearMin;
    property XYearMax: Double read Get_XYearMax write Set_XYearMax;
    property MonthCount[AMonth: Integer]: Integer read Get_MonthCount write Set_MonthCount;
    property MonthTotal[AMonth: Integer]: Double read Get_MonthTotal write Set_MonthTotal;
    property MonthMin[AMonth: Integer]: Double read Get_MonthMin write Set_MonthMin;
    property MonthMax[AMonth: Integer]: Double read Get_MonthMax write Set_MonthMax;
    property MonthMAP[AMonth: Integer]: Double read Get_MonthMAP write Set_MonthMAP;
    property MonthStdDev[AMonth: Integer]: Double read Get_MonthStdDev write Set_MonthStdDev;
    property MonthCV[AMonth: Integer]: Double read Get_MonthCV write Set_MonthCV;
    property XMonthCount[AMonth: Integer]: Integer read Get_XMonthCount write Set_XMonthCount;
    property XMonthTotal[AMonth: Integer]: Double read Get_XMonthTotal write Set_XMonthTotal;
    property XMonthMin[AMonth: Integer]: Double read Get_XMonthMin write Set_XMonthMin;
    property XMonthMax[AMonth: Integer]: Double read Get_XMonthMax write Set_XMonthMax;
    property XMonthMAP[AMonth: Integer]: Double read Get_XMonthMAP write Set_XMonthMAP;
    property XMonthStdDev[AMonth: Integer]: Double read Get_XMonthStdDev write Set_XMonthStdDev;
    property XMonthCV[AMonth: Integer]: Double read Get_XMonthCV write Set_XMonthCV;
    property HighlightMonths: WideString read Get_HighlightMonths;
    property HighlightYears: WideString read Get_HighlightYears;
  end;

// *********************************************************************//
// DispIntf:  IRainfallDataSplitDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {78B959E4-6155-413A-B07B-37584161CBC8}
// *********************************************************************//
  IRainfallDataSplitDisp = dispinterface
    ['{78B959E4-6155-413A-B07B-37584161CBC8}']
    property HydroStartYear: Integer dispid 101;
    property HydroEndYear: Integer dispid 102;
    property NrOfMissingMonths: Integer dispid 103;
    property GrandTotal: Double dispid 104;
    property MAP: Double dispid 105;
    property StdDeviation: Double dispid 106;
    property CV: Double dispid 107;
    property XGrandTotal: Double dispid 108;
    property XMAP: Double dispid 109;
    property XStdDeviation: Double dispid 110;
    property XCV: Double dispid 111;
    property YearCount: Integer dispid 112;
    property YearMin: Double dispid 113;
    property YearMax: Double dispid 114;
    property XYearCount: Integer dispid 115;
    property XYearMin: Double dispid 116;
    property XYearMax: Double dispid 117;
    property MonthCount[AMonth: Integer]: Integer dispid 118;
    property MonthTotal[AMonth: Integer]: Double dispid 119;
    property MonthMin[AMonth: Integer]: Double dispid 120;
    property MonthMax[AMonth: Integer]: Double dispid 121;
    property MonthMAP[AMonth: Integer]: Double dispid 122;
    property MonthStdDev[AMonth: Integer]: Double dispid 123;
    property MonthCV[AMonth: Integer]: Double dispid 124;
    property XMonthCount[AMonth: Integer]: Integer dispid 125;
    property XMonthTotal[AMonth: Integer]: Double dispid 126;
    property XMonthMin[AMonth: Integer]: Double dispid 127;
    property XMonthMax[AMonth: Integer]: Double dispid 128;
    property XMonthMAP[AMonth: Integer]: Double dispid 129;
    property XMonthStdDev[AMonth: Integer]: Double dispid 130;
    property XMonthCV[AMonth: Integer]: Double dispid 131;
    property HighlightMonths: WideString readonly dispid 132;
    property HighlightYears: WideString readonly dispid 133;
  end;

// *********************************************************************//
// Interface: IStationData
// Flags:     (320) Dual OleAutomation
// GUID:      {AD7FA5A8-B76D-48FE-82C8-B46707030330}
// *********************************************************************//
  IStationData = interface(IUnknown)
    ['{AD7FA5A8-B76D-48FE-82C8-B46707030330}']
    function Get_StationName: WideString; safecall;
    procedure Set_StationName(const Value: WideString); safecall;
    function Get_Latitude: Integer; safecall;
    procedure Set_Latitude(Value: Integer); safecall;
    function Get_Longitude: Integer; safecall;
    procedure Set_Longitude(Value: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(Value: Integer); safecall;
    function Get_StationType: WideString; safecall;
    procedure Set_StationType(const Value: WideString); safecall;
    function Get_IsInWR90: WordBool; safecall;
    procedure Set_IsInWR90(Value: WordBool); safecall;
    function DeletePatchWithID(APatchID: Integer): WordBool; safecall;
    procedure LoadMonthlyData; safecall;
    function PatchCount: Integer; safecall;
    procedure LatLong(var ALat: WideString; var ALong: WideString); safecall;
    function IsPartOfZone: WordBool; safecall;
    function HasACreatedPatch: WordBool; safecall;
    function IsAPatchSource: WordBool; safecall;
    function MayBeDeleted: WordBool; safecall;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; safecall;
    function SplitCount: Integer; safecall;
    procedure SaveRAWFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); safecall;
    procedure SaveMPFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); safecall;
    function MayDeleteSplit(AStartYear: Integer; AEndYear: Integer): WordBool; safecall;
    procedure RecalculateStatistics; safecall;
    procedure RepopulateHighlights; safecall;
    function GetPatchWithID(APatchID: Integer): IPatchData; safecall;
    function GetPatchWithIndex(AIndex: Integer): IPatchData; safecall;
    function GetSplitWithIndex(AIndex: Integer): IRainfallDataSplit; safecall;
    function GetSplitForYears(AStartYear: Integer; AEndYear: Integer): IRainfallDataSplit; safecall;
    function RainfallData: IRainfallData; safecall;
    function PopulateCommonBlockRAWData(AStartYear: Integer; AEndYear: Integer; 
                                        AGaugeIndex: Integer; AAddIndex: Integer; 
                                        var ABEGY: Integer; var AENDY: Integer): WordBool; safecall;
    property StationName: WideString read Get_StationName write Set_StationName;
    property Latitude: Integer read Get_Latitude write Set_Latitude;
    property Longitude: Integer read Get_Longitude write Set_Longitude;
    property Height: Integer read Get_Height write Set_Height;
    property StationType: WideString read Get_StationType write Set_StationType;
    property IsInWR90: WordBool read Get_IsInWR90 write Set_IsInWR90;
  end;

// *********************************************************************//
// DispIntf:  IStationDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {AD7FA5A8-B76D-48FE-82C8-B46707030330}
// *********************************************************************//
  IStationDataDisp = dispinterface
    ['{AD7FA5A8-B76D-48FE-82C8-B46707030330}']
    property StationName: WideString dispid 131;
    property Latitude: Integer dispid 132;
    property Longitude: Integer dispid 133;
    property Height: Integer dispid 134;
    property StationType: WideString dispid 135;
    property IsInWR90: WordBool dispid 136;
    function DeletePatchWithID(APatchID: Integer): WordBool; dispid 138;
    procedure LoadMonthlyData; dispid 201;
    function PatchCount: Integer; dispid 202;
    procedure LatLong(var ALat: WideString; var ALong: WideString); dispid 204;
    function IsPartOfZone: WordBool; dispid 205;
    function HasACreatedPatch: WordBool; dispid 206;
    function IsAPatchSource: WordBool; dispid 207;
    function MayBeDeleted: WordBool; dispid 208;
    function GetKeyValues(const AParamField: WideString; const AFieldIndex: WideString): WideString; dispid 102;
    function SplitCount: Integer; dispid 105;
    procedure SaveRAWFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); dispid 106;
    procedure SaveMPFile(AStartYear: Integer; AEndYear: Integer; const ADirectory: WideString); dispid 107;
    function MayDeleteSplit(AStartYear: Integer; AEndYear: Integer): WordBool; dispid 108;
    procedure RecalculateStatistics; dispid 109;
    procedure RepopulateHighlights; dispid 110;
    function GetPatchWithID(APatchID: Integer): IPatchData; dispid 111;
    function GetPatchWithIndex(AIndex: Integer): IPatchData; dispid 112;
    function GetSplitWithIndex(AIndex: Integer): IRainfallDataSplit; dispid 103;
    function GetSplitForYears(AStartYear: Integer; AEndYear: Integer): IRainfallDataSplit; dispid 104;
    function RainfallData: IRainfallData; dispid 101;
    function PopulateCommonBlockRAWData(AStartYear: Integer; AEndYear: Integer; 
                                        AGaugeIndex: Integer; AAddIndex: Integer; 
                                        var ABEGY: Integer; var AENDY: Integer): WordBool; dispid 113;
  end;

// *********************************************************************//
// Interface: IRainfallModelData
// Flags:     (320) Dual OleAutomation
// GUID:      {ADBAB5AE-3263-4B38-A713-4116D3912A07}
// *********************************************************************//
  IRainfallModelData = interface(IUnknown)
    ['{ADBAB5AE-3263-4B38-A713-4116D3912A07}']
    function Get_DefaultDir: WideString; safecall;
    procedure Set_DefaultDir(const Value: WideString); safecall;
    function Get_CurrentStationID: Integer; safecall;
    procedure Set_CurrentStationID(Value: Integer); safecall;
    function Get_CurrentPatchID: Integer; safecall;
    procedure Set_CurrentPatchID(Value: Integer); safecall;
    function SaveRAWDataFiles: WordBool; safecall;
    function SaveMPDataFiles: WordBool; safecall;
    function LoadData: WordBool; safecall;
    function CreateAPatch(APatchTypeID: Integer; AStationID: Integer; 
                          const ADescription: WideString; AStartYear: Integer; AEndYear: Integer): Integer; safecall;
    function DeleteAPatch(APatchID: Integer): WordBool; safecall;
    function ModifyPatchDescription(AStationID: Integer; APatchID: Integer; 
                                    const ANewDescr: WideString): WordBool; safecall;
    function AddToPatch(AStationID: Integer; APatchID: Integer; ASourceStationID: Integer; 
                        ASourcePatchID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; safecall;
    function RemoveFromPatch(APatchID: Integer; ASourceStationID: Integer; ASourcePatchID: Integer): WordBool; safecall;
    function LoadMonthlyData: WordBool; safecall;
    function CreateReport: WideString; safecall;
    function Get_StationCount: Integer; safecall;
    function GetDailyDataByMonthAndYear(AStationID: Integer; APatchID: Integer; AMonth: Integer; 
                                        AYear: Integer): WideString; safecall;
    function Get_HydroStartMonth: Integer; safecall;
    procedure Set_HydroStartMonth(Value: Integer); safecall;
    procedure LatLong(const AStationNumber: WideString; var ALat: Double; var ALong: Double); safecall;
    function GetZoneStations: WideString; safecall;
    function GetZoneCount: Integer; safecall;
    function AddToRainfallZone(AStationID: Integer; APatchID: Integer; AStartYear: Integer; 
                               AEndYear: Integer): WordBool; safecall;
    function RemoveFromRainfallZone(AStationID: Integer; APatchID: Integer): WordBool; safecall;
    function Get_CatchmentFileName: WideString; safecall;
    procedure Set_CatchmentFileName(const Value: WideString); safecall;
    function Get_ZoneChangeDate: TDateTime; safecall;
    procedure Set_ZoneChangeDate(Value: TDateTime); safecall;
    function Get_ZoneRunDate: TDateTime; safecall;
    procedure Set_ZoneRunDate(Value: TDateTime); safecall;
    function Get_OutputFileName: WideString; safecall;
    procedure Set_OutputFileName(const Value: WideString); safecall;
    function Get_CatchmentFileData: WideString; safecall;
    procedure Set_CatchmentFileData(const Value: WideString); safecall;
    function Get_OutputFileData: WideString; safecall;
    procedure Set_OutputFileData(const Value: WideString); safecall;
    function Get_RAWFlags: WideString; safecall;
    procedure Set_RAWFlags(const Value: WideString); safecall;
    function CreateASplit(AStationID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; safecall;
    function DeleteASplit(AStationID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; safecall;
    function Get_CurrentSplitIndex: Integer; safecall;
    procedure Set_CurrentSplitIndex(Value: Integer); safecall;
    function Get_PatchRStationID: Integer; safecall;
    procedure Set_PatchRStationID(Value: Integer); safecall;
    function Get_PatchRSplitIndex: Integer; safecall;
    procedure Set_PatchRSplitIndex(Value: Integer); safecall;
    function Get_PatchRPatchID: Integer; safecall;
    procedure Set_PatchRPatchID(Value: Integer); safecall;
    function Get_HighlightWetSeasonZeros: WordBool; safecall;
    procedure Set_HighlightWetSeasonZeros(Value: WordBool); safecall;
    function Get_WetSeasonMonths: WideString; safecall;
    procedure Set_WetSeasonMonths(const Value: WideString); safecall;
    function Get_HighlightMonthlyGreaterThanProportion: WordBool; safecall;
    procedure Set_HighlightMonthlyGreaterThanProportion(Value: WordBool); safecall;
    function Get_MonthlyGreaterThanProportionValue: Double; safecall;
    procedure Set_MonthlyGreaterThanProportionValue(Value: Double); safecall;
    function Get_HighlightAnnualGreaterThanAverage: WordBool; safecall;
    procedure Set_HighlightAnnualGreaterThanAverage(Value: WordBool); safecall;
    function Get_HighlightAnnualLessThanAverage: WordBool; safecall;
    procedure Set_HighlightAnnualLessThanAverage(Value: WordBool); safecall;
    function Get_HighlightMonthlyGreaterThanAbsolute: WordBool; safecall;
    procedure Set_HighlightMonthlyGreaterThanAbsolute(Value: WordBool); safecall;
    function Get_MonthlyGreaterThanAbsoluteValue: Double; safecall;
    procedure Set_MonthlyGreaterThanAbsoluteValue(Value: Double); safecall;
    function Get_HighlightRoundedValues: WordBool; safecall;
    procedure Set_HighlightRoundedValues(Value: WordBool); safecall;
    function Get_HighlightRepeatingValues: WordBool; safecall;
    procedure Set_HighlightRepeatingValues(Value: WordBool); safecall;
    function Get_IncludeUnreliableData: WordBool; safecall;
    procedure Set_IncludeUnreliableData(Value: WordBool); safecall;
    function SaveProjectGauges: WordBool; safecall;
    function UpdateSplit(AStationID: Integer; AOldStartYear: Integer; AStartYear: Integer; 
                         AOldEndYear: Integer; AEndYear: Integer): WordBool; safecall;
    function GetStationDataByNumber(const ANumber: WideString): IStationData; safecall;
    function GetStationDataByID(AStationID: Integer): IStationData; safecall;
    function GetStationDataByIndex(AIndex: Integer): IStationData; safecall;
    function GaugeList: IRainGaugeList; safecall;
    property DefaultDir: WideString read Get_DefaultDir write Set_DefaultDir;
    property CurrentStationID: Integer read Get_CurrentStationID write Set_CurrentStationID;
    property CurrentPatchID: Integer read Get_CurrentPatchID write Set_CurrentPatchID;
    property StationCount: Integer read Get_StationCount;
    property HydroStartMonth: Integer read Get_HydroStartMonth write Set_HydroStartMonth;
    property CatchmentFileName: WideString read Get_CatchmentFileName write Set_CatchmentFileName;
    property ZoneChangeDate: TDateTime read Get_ZoneChangeDate write Set_ZoneChangeDate;
    property ZoneRunDate: TDateTime read Get_ZoneRunDate write Set_ZoneRunDate;
    property OutputFileName: WideString read Get_OutputFileName write Set_OutputFileName;
    property CatchmentFileData: WideString read Get_CatchmentFileData write Set_CatchmentFileData;
    property OutputFileData: WideString read Get_OutputFileData write Set_OutputFileData;
    property RAWFlags: WideString read Get_RAWFlags write Set_RAWFlags;
    property CurrentSplitIndex: Integer read Get_CurrentSplitIndex write Set_CurrentSplitIndex;
    property PatchRStationID: Integer read Get_PatchRStationID write Set_PatchRStationID;
    property PatchRSplitIndex: Integer read Get_PatchRSplitIndex write Set_PatchRSplitIndex;
    property PatchRPatchID: Integer read Get_PatchRPatchID write Set_PatchRPatchID;
    property HighlightWetSeasonZeros: WordBool read Get_HighlightWetSeasonZeros write Set_HighlightWetSeasonZeros;
    property WetSeasonMonths: WideString read Get_WetSeasonMonths write Set_WetSeasonMonths;
    property HighlightMonthlyGreaterThanProportion: WordBool read Get_HighlightMonthlyGreaterThanProportion write Set_HighlightMonthlyGreaterThanProportion;
    property MonthlyGreaterThanProportionValue: Double read Get_MonthlyGreaterThanProportionValue write Set_MonthlyGreaterThanProportionValue;
    property HighlightAnnualGreaterThanAverage: WordBool read Get_HighlightAnnualGreaterThanAverage write Set_HighlightAnnualGreaterThanAverage;
    property HighlightAnnualLessThanAverage: WordBool read Get_HighlightAnnualLessThanAverage write Set_HighlightAnnualLessThanAverage;
    property HighlightMonthlyGreaterThanAbsolute: WordBool read Get_HighlightMonthlyGreaterThanAbsolute write Set_HighlightMonthlyGreaterThanAbsolute;
    property MonthlyGreaterThanAbsoluteValue: Double read Get_MonthlyGreaterThanAbsoluteValue write Set_MonthlyGreaterThanAbsoluteValue;
    property HighlightRoundedValues: WordBool read Get_HighlightRoundedValues write Set_HighlightRoundedValues;
    property HighlightRepeatingValues: WordBool read Get_HighlightRepeatingValues write Set_HighlightRepeatingValues;
    property IncludeUnreliableData: WordBool read Get_IncludeUnreliableData write Set_IncludeUnreliableData;
  end;

// *********************************************************************//
// DispIntf:  IRainfallModelDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {ADBAB5AE-3263-4B38-A713-4116D3912A07}
// *********************************************************************//
  IRainfallModelDataDisp = dispinterface
    ['{ADBAB5AE-3263-4B38-A713-4116D3912A07}']
    property DefaultDir: WideString dispid 201;
    property CurrentStationID: Integer dispid 202;
    property CurrentPatchID: Integer dispid 206;
    function SaveRAWDataFiles: WordBool; dispid 207;
    function SaveMPDataFiles: WordBool; dispid 208;
    function LoadData: WordBool; dispid 209;
    function CreateAPatch(APatchTypeID: Integer; AStationID: Integer; 
                          const ADescription: WideString; AStartYear: Integer; AEndYear: Integer): Integer; dispid 210;
    function DeleteAPatch(APatchID: Integer): WordBool; dispid 211;
    function ModifyPatchDescription(AStationID: Integer; APatchID: Integer; 
                                    const ANewDescr: WideString): WordBool; dispid 212;
    function AddToPatch(AStationID: Integer; APatchID: Integer; ASourceStationID: Integer; 
                        ASourcePatchID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; dispid 213;
    function RemoveFromPatch(APatchID: Integer; ASourceStationID: Integer; ASourcePatchID: Integer): WordBool; dispid 214;
    function LoadMonthlyData: WordBool; dispid 101;
    function CreateReport: WideString; dispid 102;
    property StationCount: Integer readonly dispid 103;
    function GetDailyDataByMonthAndYear(AStationID: Integer; APatchID: Integer; AMonth: Integer; 
                                        AYear: Integer): WideString; dispid 104;
    property HydroStartMonth: Integer dispid 105;
    procedure LatLong(const AStationNumber: WideString; var ALat: Double; var ALong: Double); dispid 106;
    function GetZoneStations: WideString; dispid 107;
    function GetZoneCount: Integer; dispid 108;
    function AddToRainfallZone(AStationID: Integer; APatchID: Integer; AStartYear: Integer; 
                               AEndYear: Integer): WordBool; dispid 109;
    function RemoveFromRainfallZone(AStationID: Integer; APatchID: Integer): WordBool; dispid 110;
    property CatchmentFileName: WideString dispid 111;
    property ZoneChangeDate: TDateTime dispid 112;
    property ZoneRunDate: TDateTime dispid 113;
    property OutputFileName: WideString dispid 114;
    property CatchmentFileData: WideString dispid 115;
    property OutputFileData: WideString dispid 116;
    property RAWFlags: WideString dispid 117;
    function CreateASplit(AStationID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; dispid 118;
    function DeleteASplit(AStationID: Integer; AStartYear: Integer; AEndYear: Integer): WordBool; dispid 119;
    property CurrentSplitIndex: Integer dispid 120;
    property PatchRStationID: Integer dispid 121;
    property PatchRSplitIndex: Integer dispid 122;
    property PatchRPatchID: Integer dispid 123;
    property HighlightWetSeasonZeros: WordBool dispid 124;
    property WetSeasonMonths: WideString dispid 125;
    property HighlightMonthlyGreaterThanProportion: WordBool dispid 126;
    property MonthlyGreaterThanProportionValue: Double dispid 127;
    property HighlightAnnualGreaterThanAverage: WordBool dispid 128;
    property HighlightAnnualLessThanAverage: WordBool dispid 129;
    property HighlightMonthlyGreaterThanAbsolute: WordBool dispid 130;
    property MonthlyGreaterThanAbsoluteValue: Double dispid 131;
    property HighlightRoundedValues: WordBool dispid 132;
    property HighlightRepeatingValues: WordBool dispid 133;
    property IncludeUnreliableData: WordBool dispid 134;
    function SaveProjectGauges: WordBool; dispid 135;
    function UpdateSplit(AStationID: Integer; AOldStartYear: Integer; AStartYear: Integer; 
                         AOldEndYear: Integer; AEndYear: Integer): WordBool; dispid 136;
    function GetStationDataByNumber(const ANumber: WideString): IStationData; dispid 137;
    function GetStationDataByID(AStationID: Integer): IStationData; dispid 138;
    function GetStationDataByIndex(AIndex: Integer): IStationData; dispid 139;
    function GaugeList: IRainGaugeList; dispid 140;
  end;

// *********************************************************************//
// Interface: IRainfallModel
// Flags:     (256) OleAutomation
// GUID:      {989135AE-A86D-46EB-8874-BBD7A51116FC}
// *********************************************************************//
  IRainfallModel = interface(IUnknown)
    ['{989135AE-A86D-46EB-8874-BBD7A51116FC}']
    procedure ViewDataGraph; safecall;
    procedure ViewGaugeStats; safecall;
    procedure ViewGaugeSelection; safecall;
    procedure ViewPatchAdmin; safecall;
    function GetMonthlyData(const AStationNumber: WideString; const APatchName: WideString): WideString; safecall;
    function GetDailyData(const AStationNumber: WideString; const APatchName: WideString): WideString; safecall;
  end;

// *********************************************************************//
// The Class CoRainfallComObject provides a Create and CreateRemote method to          
// create instances of the default interface IRainfallComObject exposed by              
// the CoClass RainfallComObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRainfallComObject = class
    class function Create: IRainfallComObject;
    class function CreateRemote(const MachineName: string): IRainfallComObject;
  end;

implementation

uses ComObj;

class function CoRainfallComObject.Create: IRainfallComObject;
begin
  Result := CreateComObject(CLASS_RainfallComObject) as IRainfallComObject;
end;

class function CoRainfallComObject.CreateRemote(const MachineName: string): IRainfallComObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RainfallComObject) as IRainfallComObject;
end;

end.

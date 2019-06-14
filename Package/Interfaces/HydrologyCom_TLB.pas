unit HydrologyCom_TLB;

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
// File generated on 29/03/2010 10:45:43 from Type Library described below.

// ************************************************************************  //
// Type Lib: R:\WRMF\Source\DLLProjects\HydrologyModel\HydroCom\HydrologyCom.tlb (1)
// LIBID: {F1230AF5-EC8F-4D5E-A4B0-42FFFE958D78}
// LCID: 0
// Helpfile: 
// HelpString: HydrologyCom Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, VCL.Graphics, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  HydrologyComMajorVersion = 1;
  HydrologyComMinorVersion = 0;

  LIBID_HydrologyCom: TGUID = '{F1230AF5-EC8F-4D5E-A4B0-42FFFE958D78}';

  IID_INetwork: TGUID = '{DCFA8996-C27D-4546-9854-1FF9EAFC0A9F}';
  IID_IModule: TGUID = '{D30022F5-EBF5-4B39-AB74-8126868845AB}';
  IID_INetworkModule: TGUID = '{B8505A79-7D05-4BAD-A875-2948CEE64ECF}';
  IID_IReservoirModule: TGUID = '{3055327F-DA7E-4497-8FFF-47428AF83CBF}';
  IID_IReservoirModuleAgent: TGUID = '{6D0C7D0C-938C-4172-8C0A-A996CB3C2F0E}';
  IID_IChannelModule: TGUID = '{C0E3A02F-7C36-4AF4-9CCB-E091CC072B79}';
  IID_IChannelModuleAgent: TGUID = '{75E83A87-E32E-4696-9AF4-5CB84544847C}';
  IID_IRunOffModule: TGUID = '{098FBB48-D94A-4E3E-9543-762A9A821CE8}';
  IID_IRunOffAfforestation: TGUID = '{0351D8BC-4EF9-417B-A184-CB44E6D784E1}';
  IID_IRunOffPitmanModel: TGUID = '{AC16446D-DEF4-41C5-A264-C9CE78F928B3}';
  IID_IRunOffModuleAgent: TGUID = '{40325093-8CE0-43FA-8524-868CE0098212}';
  IID_IRunOffAlienVegetation: TGUID = '{8D65C35B-D37D-480F-BEB7-11DA13913E76}';
  IID_IRunOffPavedArea: TGUID = '{976D0007-9641-4DF5-9C16-06D70F2D01D9}';
  IID_IRunOffOutflowRoute: TGUID = '{8B4E1D7C-1E73-47B6-A6FF-E1D034E53353}';
  IID_IRunOffHughesModel: TGUID = '{DB8C7D39-907B-44FD-B374-D1E2BA2D80E5}';
  IID_IRunOffSamiModel: TGUID = '{A14D1F19-F686-4785-A920-24F640781267}';
  IID_IRunOffGroundWaterAbstraction: TGUID = '{31499947-6F2B-4B45-9E6B-875D2DC5FB06}';
  IID_INetworkRoute: TGUID = '{C0BA59E3-2AE2-43B9-883D-202697729CE6}';
  IID_INetworkRouteAgent: TGUID = '{E5D1BBCA-FA4E-4B0C-B0EF-3F481C9DF016}';
  IID_IObservationPoint: TGUID = '{E5B8B1B1-6E73-4F89-89E0-DEB194D74782}';
  IID_IObservationPointAgent: TGUID = '{E3F12CBE-9C3A-41E5-BF26-636022F9A88D}';
  IID_IMineModule: TGUID = '{99461795-DE79-4C22-A51C-0B36CBA4A39F}';
  IID_IMineModuleAgent: TGUID = '{F523665A-1EEF-427F-8F87-9F00AB70791C}';
  IID_IPan: TGUID = '{4A387CF7-3CB5-4FE6-B936-10DE6259C503}';
  IID_IInflowRoute: TGUID = '{1C711808-065A-4BC0-AAAF-CE634002C2CC}';
  IID_IOutflowRoute: TGUID = '{2E2B6217-A7A8-4CBD-A2C6-4095C3D0917C}';
  IID_IOpencastPit: TGUID = '{FCF3540C-CCFD-4D1F-AFDE-EB7485345061}';
  IID_IUndergroundSection: TGUID = '{BEA1B616-53D1-4F24-9040-51693A8EF2E2}';
  IID_ISlurryDump: TGUID = '{9B59804B-CA54-4926-A610-12DAC993AA07}';
  IID_IIrrigationModule: TGUID = '{41F2BF67-70E7-4263-9085-65AA62A78C24}';
  IID_IIrrigationModuleAgent: TGUID = '{15CE7C42-F633-4E76-B01B-CBF4A5EDF683}';
  IID_IIrrigationCrop: TGUID = '{390CF9D3-7291-4C73-B119-48D9D6F8D21D}';
  IID_IHydroNVDrawing: TGUID = '{99C287E1-02A5-40E8-9884-F256C553AD2A}';
  IID_IHydroNVDrawingAgent: TGUID = '{70781048-F72F-4312-BFB0-2C3535D90BAE}';
  IID_IHydrologyModel: TGUID = '{D40CF196-D458-48F7-A13F-794A8C5D482E}';
  IID_IYearVolumeAreaData: TGUID = '{7FDC9172-8066-4100-9299-4E4556BE44EC}';
  IID_IHydroOutputAgent: TGUID = '{CCF80B02-225B-4CE1-BDA7-DD8B9DB57D09}';
  IID_IHydroOutput: TGUID = '{9504DE8B-38B1-46B5-BB07-4D010FD75245}';
  IID_ITimeSeries: TGUID = '{5EBFB19D-23E2-4A5D-80E1-CA2125725169}';
  IID_IHydroResultType: TGUID = '{6154CD50-12B5-4C64-896B-26E8D3836BEA}';
  IID_IHydrologyComObject: TGUID = '{99658A58-442A-45A9-8EA1-80A449E0E923}';
  CLASS_HydrologyComObject: TGUID = '{E020ECF0-703B-4F86-AAEE-5D24098B5D55}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  INetwork = interface;
  INetworkDisp = dispinterface;
  IModule = interface;
  IModuleDisp = dispinterface;
  INetworkModule = interface;
  INetworkModuleDisp = dispinterface;
  IReservoirModule = interface;
  IReservoirModuleDisp = dispinterface;
  IReservoirModuleAgent = interface;
  IReservoirModuleAgentDisp = dispinterface;
  IChannelModule = interface;
  IChannelModuleDisp = dispinterface;
  IChannelModuleAgent = interface;
  IChannelModuleAgentDisp = dispinterface;
  IRunOffModule = interface;
  IRunOffModuleDisp = dispinterface;
  IRunOffAfforestation = interface;
  IRunOffAfforestationDisp = dispinterface;
  IRunOffPitmanModel = interface;
  IRunOffPitmanModelDisp = dispinterface;
  IRunOffModuleAgent = interface;
  IRunOffModuleAgentDisp = dispinterface;
  IRunOffAlienVegetation = interface;
  IRunOffAlienVegetationDisp = dispinterface;
  IRunOffPavedArea = interface;
  IRunOffPavedAreaDisp = dispinterface;
  IRunOffOutflowRoute = interface;
  IRunOffOutflowRouteDisp = dispinterface;
  IRunOffHughesModel = interface;
  IRunOffHughesModelDisp = dispinterface;
  IRunOffSamiModel = interface;
  IRunOffSamiModelDisp = dispinterface;
  IRunOffGroundWaterAbstraction = interface;
  IRunOffGroundWaterAbstractionDisp = dispinterface;
  INetworkRoute = interface;
  INetworkRouteDisp = dispinterface;
  INetworkRouteAgent = interface;
  INetworkRouteAgentDisp = dispinterface;
  IObservationPoint = interface;
  IObservationPointDisp = dispinterface;
  IObservationPointAgent = interface;
  IObservationPointAgentDisp = dispinterface;
  IMineModule = interface;
  IMineModuleDisp = dispinterface;
  IMineModuleAgent = interface;
  IMineModuleAgentDisp = dispinterface;
  IPan = interface;
  IPanDisp = dispinterface;
  IInflowRoute = interface;
  IInflowRouteDisp = dispinterface;
  IOutflowRoute = interface;
  IOutflowRouteDisp = dispinterface;
  IOpencastPit = interface;
  IOpencastPitDisp = dispinterface;
  IUndergroundSection = interface;
  IUndergroundSectionDisp = dispinterface;
  ISlurryDump = interface;
  ISlurryDumpDisp = dispinterface;
  IIrrigationModule = interface;
  IIrrigationModuleDisp = dispinterface;
  IIrrigationModuleAgent = interface;
  IIrrigationModuleAgentDisp = dispinterface;
  IIrrigationCrop = interface;
  IIrrigationCropDisp = dispinterface;
  IHydroNVDrawing = interface;
  IHydroNVDrawingDisp = dispinterface;
  IHydroNVDrawingAgent = interface;
  IHydroNVDrawingAgentDisp = dispinterface;
  IHydrologyModel = interface;
  IHydrologyModelDisp = dispinterface;
  IYearVolumeAreaData = interface;
  IYearVolumeAreaDataDisp = dispinterface;
  IHydroOutputAgent = interface;
  IHydroOutputAgentDisp = dispinterface;
  IHydroOutput = interface;
  IHydroOutputDisp = dispinterface;
  ITimeSeries = interface;
  ITimeSeriesDisp = dispinterface;
  IHydroResultType = interface;
  IHydroResultTypeDisp = dispinterface;
  IHydrologyComObject = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  HydrologyComObject = IHydrologyComObject;


// *********************************************************************//
// Interface: INetwork
// Flags:     (320) Dual OleAutomation
// GUID:      {DCFA8996-C27D-4546-9854-1FF9EAFC0A9F}
// *********************************************************************//
  INetwork = interface(IUnknown)
    ['{DCFA8996-C27D-4546-9854-1FF9EAFC0A9F}']
    function Get_NetworkID: Integer; safecall;
    function Get_NetworkCode: WideString; safecall;
    procedure Set_NetworkCode(const Value: WideString); safecall;
    function Get_VersionNumber: Integer; safecall;
    procedure Set_VersionNumber(Value: Integer); safecall;
    function Get_InputDirectory: WideString; safecall;
    procedure Set_InputDirectory(const Value: WideString); safecall;
    function Get_OutputDirectory: WideString; safecall;
    procedure Set_OutputDirectory(const Value: WideString); safecall;
    function Get_DebugRequired: WideString; safecall;
    procedure Set_DebugRequired(const Value: WideString); safecall;
    function Get_DebugStartPeriod: Integer; safecall;
    procedure Set_DebugStartPeriod(Value: Integer); safecall;
    function Get_DebugEndPeriod: Integer; safecall;
    procedure Set_DebugEndPeriod(Value: Integer); safecall;
    function Get_SummaryRequired: WideString; safecall;
    procedure Set_SummaryRequired(const Value: WideString); safecall;
    function Get_SimulationStartYear: Integer; safecall;
    procedure Set_SimulationStartYear(Value: Integer); safecall;
    function Get_SimulationEndYear: Integer; safecall;
    procedure Set_SimulationEndYear(Value: Integer); safecall;
    function Get_IsReadOnly: Integer; safecall;
    procedure Set_IsReadOnly(Value: Integer); safecall;
    function Get_NetworkRouteAgent: INetworkRouteAgent; safecall;
    function Get_ReservoirModuleAgent: IReservoirModuleAgent; safecall;
    function Get_ChannelModuleAgent: IChannelModuleAgent; safecall;
    function Get_RunOffModuleAgent: IRunOffModuleAgent; safecall;
    function Populate(ANetworkID: Integer; const ANetworkCode: WideString; AVersionNumber: Integer; 
                      const AInputDirectory: WideString; const AOutputDirectory: WideString; 
                      const ADebugRequired: WideString; ADebugStartPeriod: Integer; 
                      ADebugEndPeriod: Integer; const ASummaryRequired: WideString; 
                      ASimulationStartYear: Integer; ASimulationEndYear: Integer; 
                      AIsReadOnly: Integer; AMinLongitude: Double; AMaxLongitude: Double; 
                      AMinLatitude: Double; AMaxLatitude: Double): WordBool; safecall;
    function Get_ObservationPointAgent: IObservationPointAgent; safecall;
    function Get_MineModuleAgent: IMineModuleAgent; safecall;
    function Get_IrrigationModuleAgent: IIrrigationModuleAgent; safecall;
    function Get_ModuleBySequenceNumber(ASequence: Integer): INetworkModule; safecall;
    function Get_HydroNVDrawingAgent: IHydroNVDrawingAgent; safecall;
    function Get_MinLongitude: Double; safecall;
    procedure Set_MinLongitude(Value: Double); safecall;
    function Get_MaxLongitude: Double; safecall;
    procedure Set_MaxLongitude(Value: Double); safecall;
    function Get_MinLatitude: Double; safecall;
    procedure Set_MinLatitude(Value: Double); safecall;
    function Get_MaxLatitude: Double; safecall;
    procedure Set_MaxLatitude(Value: Double); safecall;
    function Get_ModuleByID(AModuleID: Integer): INetworkModule; safecall;
    function Get_ModuleTextByID(AModuleID: Integer): WideString; safecall;
    procedure SetRouteSource(ARouteNo: Integer; ASourceModuleID: Integer); safecall;
    procedure SetRouteSink(ARouteNo: Integer; ASinkModuleID: Integer); safecall;
    function DetermineNetworkSequence: WordBool; safecall;
    function Get_HydroOutputAgent: IHydroOutputAgent; safecall;
    function Get_NoOfIntervals: Integer; safecall;
    function Get_IntervalText(AInterval: Integer): WideString; safecall;
    property NetworkID: Integer read Get_NetworkID;
    property NetworkCode: WideString read Get_NetworkCode write Set_NetworkCode;
    property VersionNumber: Integer read Get_VersionNumber write Set_VersionNumber;
    property InputDirectory: WideString read Get_InputDirectory write Set_InputDirectory;
    property OutputDirectory: WideString read Get_OutputDirectory write Set_OutputDirectory;
    property DebugRequired: WideString read Get_DebugRequired write Set_DebugRequired;
    property DebugStartPeriod: Integer read Get_DebugStartPeriod write Set_DebugStartPeriod;
    property DebugEndPeriod: Integer read Get_DebugEndPeriod write Set_DebugEndPeriod;
    property SummaryRequired: WideString read Get_SummaryRequired write Set_SummaryRequired;
    property SimulationStartYear: Integer read Get_SimulationStartYear write Set_SimulationStartYear;
    property SimulationEndYear: Integer read Get_SimulationEndYear write Set_SimulationEndYear;
    property IsReadOnly: Integer read Get_IsReadOnly write Set_IsReadOnly;
    property NetworkRouteAgent: INetworkRouteAgent read Get_NetworkRouteAgent;
    property ReservoirModuleAgent: IReservoirModuleAgent read Get_ReservoirModuleAgent;
    property ChannelModuleAgent: IChannelModuleAgent read Get_ChannelModuleAgent;
    property RunOffModuleAgent: IRunOffModuleAgent read Get_RunOffModuleAgent;
    property ObservationPointAgent: IObservationPointAgent read Get_ObservationPointAgent;
    property MineModuleAgent: IMineModuleAgent read Get_MineModuleAgent;
    property IrrigationModuleAgent: IIrrigationModuleAgent read Get_IrrigationModuleAgent;
    property ModuleBySequenceNumber[ASequence: Integer]: INetworkModule read Get_ModuleBySequenceNumber;
    property HydroNVDrawingAgent: IHydroNVDrawingAgent read Get_HydroNVDrawingAgent;
    property MinLongitude: Double read Get_MinLongitude write Set_MinLongitude;
    property MaxLongitude: Double read Get_MaxLongitude write Set_MaxLongitude;
    property MinLatitude: Double read Get_MinLatitude write Set_MinLatitude;
    property MaxLatitude: Double read Get_MaxLatitude write Set_MaxLatitude;
    property ModuleByID[AModuleID: Integer]: INetworkModule read Get_ModuleByID;
    property ModuleTextByID[AModuleID: Integer]: WideString read Get_ModuleTextByID;
    property HydroOutputAgent: IHydroOutputAgent read Get_HydroOutputAgent;
    property NoOfIntervals: Integer read Get_NoOfIntervals;
    property IntervalText[AInterval: Integer]: WideString read Get_IntervalText;
  end;

// *********************************************************************//
// DispIntf:  INetworkDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DCFA8996-C27D-4546-9854-1FF9EAFC0A9F}
// *********************************************************************//
  INetworkDisp = dispinterface
    ['{DCFA8996-C27D-4546-9854-1FF9EAFC0A9F}']
    property NetworkID: Integer readonly dispid 1610678272;
    property NetworkCode: WideString dispid 201;
    property VersionNumber: Integer dispid 203;
    property InputDirectory: WideString dispid 204;
    property OutputDirectory: WideString dispid 205;
    property DebugRequired: WideString dispid 206;
    property DebugStartPeriod: Integer dispid 207;
    property DebugEndPeriod: Integer dispid 208;
    property SummaryRequired: WideString dispid 209;
    property SimulationStartYear: Integer dispid 210;
    property SimulationEndYear: Integer dispid 211;
    property IsReadOnly: Integer dispid 212;
    property NetworkRouteAgent: INetworkRouteAgent readonly dispid 101;
    property ReservoirModuleAgent: IReservoirModuleAgent readonly dispid 102;
    property ChannelModuleAgent: IChannelModuleAgent readonly dispid 103;
    property RunOffModuleAgent: IRunOffModuleAgent readonly dispid 104;
    function Populate(ANetworkID: Integer; const ANetworkCode: WideString; AVersionNumber: Integer; 
                      const AInputDirectory: WideString; const AOutputDirectory: WideString; 
                      const ADebugRequired: WideString; ADebugStartPeriod: Integer; 
                      ADebugEndPeriod: Integer; const ASummaryRequired: WideString; 
                      ASimulationStartYear: Integer; ASimulationEndYear: Integer; 
                      AIsReadOnly: Integer; AMinLongitude: Double; AMaxLongitude: Double; 
                      AMinLatitude: Double; AMaxLatitude: Double): WordBool; dispid 105;
    property ObservationPointAgent: IObservationPointAgent readonly dispid 106;
    property MineModuleAgent: IMineModuleAgent readonly dispid 107;
    property IrrigationModuleAgent: IIrrigationModuleAgent readonly dispid 108;
    property ModuleBySequenceNumber[ASequence: Integer]: INetworkModule readonly dispid 110;
    property HydroNVDrawingAgent: IHydroNVDrawingAgent readonly dispid 111;
    property MinLongitude: Double dispid 112;
    property MaxLongitude: Double dispid 113;
    property MinLatitude: Double dispid 114;
    property MaxLatitude: Double dispid 115;
    property ModuleByID[AModuleID: Integer]: INetworkModule readonly dispid 116;
    property ModuleTextByID[AModuleID: Integer]: WideString readonly dispid 117;
    procedure SetRouteSource(ARouteNo: Integer; ASourceModuleID: Integer); dispid 118;
    procedure SetRouteSink(ARouteNo: Integer; ASinkModuleID: Integer); dispid 119;
    function DetermineNetworkSequence: WordBool; dispid 120;
    property HydroOutputAgent: IHydroOutputAgent readonly dispid 121;
    property NoOfIntervals: Integer readonly dispid 122;
    property IntervalText[AInterval: Integer]: WideString readonly dispid 123;
  end;

// *********************************************************************//
// Interface: IModule
// Flags:     (320) Dual OleAutomation
// GUID:      {D30022F5-EBF5-4B39-AB74-8126868845AB}
// *********************************************************************//
  IModule = interface(IUnknown)
    ['{D30022F5-EBF5-4B39-AB74-8126868845AB}']
    function Get_ModuleID: Integer; safecall;
    function Get_ModuleType: WideString; safecall;
    procedure Set_ModuleType(const Value: WideString); safecall;
    function Get_PanByMonth(AMonth: Integer): IPan; safecall;
    function Get_PanByIndex(AIndex: Integer): IPan; safecall;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; safecall;
    function Get_Longitude: Double; safecall;
    procedure Set_Longitude(Value: Double); safecall;
    function Get_Latitude: Double; safecall;
    procedure Set_Latitude(Value: Double); safecall;
    function Get_PanCount: Integer; safecall;
    property ModuleID: Integer read Get_ModuleID;
    property ModuleType: WideString read Get_ModuleType write Set_ModuleType;
    property PanByMonth[AMonth: Integer]: IPan read Get_PanByMonth;
    property PanByIndex[AIndex: Integer]: IPan read Get_PanByIndex;
    property Longitude: Double read Get_Longitude write Set_Longitude;
    property Latitude: Double read Get_Latitude write Set_Latitude;
    property PanCount: Integer read Get_PanCount;
  end;

// *********************************************************************//
// DispIntf:  IModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D30022F5-EBF5-4B39-AB74-8126868845AB}
// *********************************************************************//
  IModuleDisp = dispinterface
    ['{D30022F5-EBF5-4B39-AB74-8126868845AB}']
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: INetworkModule
// Flags:     (320) Dual OleAutomation
// GUID:      {B8505A79-7D05-4BAD-A875-2948CEE64ECF}
// *********************************************************************//
  INetworkModule = interface(IModule)
    ['{B8505A79-7D05-4BAD-A875-2948CEE64ECF}']
    function Get_NetworkID: Integer; safecall;
    function Get_ModuleNumber: Integer; safecall;
    procedure Set_ModuleNumber(Value: Integer); safecall;
    function Get_NetworkSequence: Integer; safecall;
    procedure Set_NetworkSequence(Value: Integer); safecall;
    function Get_Active: WideString; safecall;
    procedure Set_Active(const Value: WideString); safecall;
    property NetworkID: Integer read Get_NetworkID;
    property ModuleNumber: Integer read Get_ModuleNumber write Set_ModuleNumber;
    property NetworkSequence: Integer read Get_NetworkSequence write Set_NetworkSequence;
    property Active: WideString read Get_Active write Set_Active;
  end;

// *********************************************************************//
// DispIntf:  INetworkModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {B8505A79-7D05-4BAD-A875-2948CEE64ECF}
// *********************************************************************//
  INetworkModuleDisp = dispinterface
    ['{B8505A79-7D05-4BAD-A875-2948CEE64ECF}']
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IReservoirModule
// Flags:     (320) Dual OleAutomation
// GUID:      {3055327F-DA7E-4497-8FFF-47428AF83CBF}
// *********************************************************************//
  IReservoirModule = interface(INetworkModule)
    ['{3055327F-DA7E-4497-8FFF-47428AF83CBF}']
    function Get_ReservoirName: WideString; safecall;
    procedure Set_ReservoirName(const Value: WideString); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_AreaPower: Double; safecall;
    procedure Set_AreaPower(Value: Double); safecall;
    function Get_SpillageRouteNo: Integer; safecall;
    procedure Set_SpillageRouteNo(Value: Integer); safecall;
    function Get_InitialStorageState: Double; safecall;
    procedure Set_InitialStorageState(Value: Double); safecall;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AReservoirName: WideString; AMAP: Double; 
                      const ARainfallFileName: WideString; AAreaPower: Double; 
                      ASpillageRouteNo: Integer; AInitialStorageState: Double; ALongitude: Double; 
                      ALatitude: Double): WordBool; safecall;
    function AddInflowRoute(ARouteNo: Integer; const AFileName: WideString): IInflowRoute; safecall;
    function AddOutflowRoute(ARouteNo: Integer; const AFileName: WideString; 
                             const AAbstractions: WideString; AStorageState: Double; 
                             AReductionFactor: Double): IOutflowRoute; safecall;
    function Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute; safecall;
    function Get_NoOfInFlowRoutes: Integer; safecall;
    function Get_InflowRouteByIndex(AIndex: Integer): IInflowRoute; safecall;
    function Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute; safecall;
    function Get_NoOfOutFlowRoutes: Integer; safecall;
    function Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute; safecall;
    function AddVolumeAreaData(AYear: Integer; AVolume: Double; AArea: Double): IYearVolumeAreaData; safecall;
    function Get_VolumeAreaDataByYear(AYear: Integer): IYearVolumeAreaData; safecall;
    function Get_VolumeAreaDataByIndex(AIndex: Integer): IYearVolumeAreaData; safecall;
    function Get_VolumeAreaDataCount: Integer; safecall;
    property ReservoirName: WideString read Get_ReservoirName write Set_ReservoirName;
    property MAP: Double read Get_MAP write Set_MAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property AreaPower: Double read Get_AreaPower write Set_AreaPower;
    property SpillageRouteNo: Integer read Get_SpillageRouteNo write Set_SpillageRouteNo;
    property InitialStorageState: Double read Get_InitialStorageState write Set_InitialStorageState;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute read Get_InFlowRouteByRouteNo;
    property NoOfInFlowRoutes: Integer read Get_NoOfInFlowRoutes;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute read Get_InflowRouteByIndex;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute read Get_OutFlowRouteByRouteNo;
    property NoOfOutFlowRoutes: Integer read Get_NoOfOutFlowRoutes;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute read Get_OutFlowRouteByIndex;
    property VolumeAreaDataByYear[AYear: Integer]: IYearVolumeAreaData read Get_VolumeAreaDataByYear;
    property VolumeAreaDataByIndex[AIndex: Integer]: IYearVolumeAreaData read Get_VolumeAreaDataByIndex;
    property VolumeAreaDataCount: Integer read Get_VolumeAreaDataCount;
  end;

// *********************************************************************//
// DispIntf:  IReservoirModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {3055327F-DA7E-4497-8FFF-47428AF83CBF}
// *********************************************************************//
  IReservoirModuleDisp = dispinterface
    ['{3055327F-DA7E-4497-8FFF-47428AF83CBF}']
    property ReservoirName: WideString dispid 202;
    property MAP: Double dispid 203;
    property RainfallFileName: WideString dispid 204;
    property AreaPower: Double dispid 305;
    property SpillageRouteNo: Integer dispid 306;
    property InitialStorageState: Double dispid 307;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AReservoirName: WideString; AMAP: Double; 
                      const ARainfallFileName: WideString; AAreaPower: Double; 
                      ASpillageRouteNo: Integer; AInitialStorageState: Double; ALongitude: Double; 
                      ALatitude: Double): WordBool; dispid 308;
    function AddInflowRoute(ARouteNo: Integer; const AFileName: WideString): IInflowRoute; dispid 301;
    function AddOutflowRoute(ARouteNo: Integer; const AFileName: WideString; 
                             const AAbstractions: WideString; AStorageState: Double; 
                             AReductionFactor: Double): IOutflowRoute; dispid 302;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute readonly dispid 303;
    property NoOfInFlowRoutes: Integer readonly dispid 304;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute readonly dispid 309;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute readonly dispid 310;
    property NoOfOutFlowRoutes: Integer readonly dispid 311;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute readonly dispid 312;
    function AddVolumeAreaData(AYear: Integer; AVolume: Double; AArea: Double): IYearVolumeAreaData; dispid 313;
    property VolumeAreaDataByYear[AYear: Integer]: IYearVolumeAreaData readonly dispid 314;
    property VolumeAreaDataByIndex[AIndex: Integer]: IYearVolumeAreaData readonly dispid 315;
    property VolumeAreaDataCount: Integer readonly dispid 317;
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IReservoirModuleAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {6D0C7D0C-938C-4172-8C0A-A996CB3C2F0E}
// *********************************************************************//
  IReservoirModuleAgent = interface(IUnknown)
    ['{6D0C7D0C-938C-4172-8C0A-A996CB3C2F0E}']
    function Get_ReservoirModuleCount: Integer; safecall;
    function Get_ReservoirModuleByID(AModuleID: Integer): IReservoirModule; safecall;
    function Get_ReservoirModuleByIndex(AIndex: Integer): IReservoirModule; safecall;
    function Get_ReservoirModuleByNumber(AModuleNumber: Integer): IReservoirModule; safecall;
    function CreateNewReservoirModule(ANetworkID: Integer): IReservoirModule; safecall;
    function RemoveReservoirModule(AModuleNumber: Integer): WordBool; safecall;
    property ReservoirModuleCount: Integer read Get_ReservoirModuleCount;
    property ReservoirModuleByID[AModuleID: Integer]: IReservoirModule read Get_ReservoirModuleByID;
    property ReservoirModuleByIndex[AIndex: Integer]: IReservoirModule read Get_ReservoirModuleByIndex;
    property ReservoirModuleByNumber[AModuleNumber: Integer]: IReservoirModule read Get_ReservoirModuleByNumber;
  end;

// *********************************************************************//
// DispIntf:  IReservoirModuleAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6D0C7D0C-938C-4172-8C0A-A996CB3C2F0E}
// *********************************************************************//
  IReservoirModuleAgentDisp = dispinterface
    ['{6D0C7D0C-938C-4172-8C0A-A996CB3C2F0E}']
    property ReservoirModuleCount: Integer readonly dispid 101;
    property ReservoirModuleByID[AModuleID: Integer]: IReservoirModule readonly dispid 102;
    property ReservoirModuleByIndex[AIndex: Integer]: IReservoirModule readonly dispid 103;
    property ReservoirModuleByNumber[AModuleNumber: Integer]: IReservoirModule readonly dispid 106;
    function CreateNewReservoirModule(ANetworkID: Integer): IReservoirModule; dispid 107;
    function RemoveReservoirModule(AModuleNumber: Integer): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IChannelModule
// Flags:     (320) Dual OleAutomation
// GUID:      {C0E3A02F-7C36-4AF4-9CCB-E091CC072B79}
// *********************************************************************//
  IChannelModule = interface(INetworkModule)
    ['{C0E3A02F-7C36-4AF4-9CCB-E091CC072B79}']
    function Get_ChannelName: WideString; safecall;
    procedure Set_ChannelName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_WetlandMAP: Double; safecall;
    procedure Set_WetlandMAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MonthlyBedLoss: Double; safecall;
    procedure Set_MonthlyBedLoss(Value: Double); safecall;
    function Get_WetlandStorage: Double; safecall;
    procedure Set_WetlandStorage(Value: Double); safecall;
    function Get_WetlandArea: Double; safecall;
    procedure Set_WetlandArea(Value: Double); safecall;
    function Get_WetlandRechargeCoefficient: Double; safecall;
    procedure Set_WetlandRechargeCoefficient(Value: Double); safecall;
    function Get_PrincipalOutflowRouteNo: Integer; safecall;
    procedure Set_PrincipalOutflowRouteNo(Value: Integer); safecall;
    function Get_FutureUse: Integer; safecall;
    procedure Set_FutureUse(Value: Integer); safecall;
    function Get_WetlandType: Integer; safecall;
    procedure Set_WetlandType(Value: Integer); safecall;
    function Get_QDiv: Double; safecall;
    procedure Set_QDiv(Value: Double); safecall;
    function Get_WetlandsInflowRouteNo: Integer; safecall;
    procedure Set_WetlandsInflowRouteNo(Value: Integer); safecall;
    function Get_WetlandsOutflowRouteNo: Integer; safecall;
    procedure Set_WetlandsOutflowRouteNo(Value: Integer); safecall;
    function Get_DiversionRouteNo: Integer; safecall;
    procedure Set_DiversionRouteNo(Value: Integer); safecall;
    function Get_BankfillCapacity: Double; safecall;
    procedure Set_BankfillCapacity(Value: Double); safecall;
    function Get_DiversionEfficiency: Double; safecall;
    procedure Set_DiversionEfficiency(Value: Double); safecall;
    function Get_MaxMonthlyDiversionCapacity: Double; safecall;
    procedure Set_MaxMonthlyDiversionCapacity(Value: Double); safecall;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AChannelName: WideString; AVersionNo: Integer; AWetlandMAP: Double; 
                      const ARainfallFileName: WideString; AMonthlyBedLoss: Double; 
                      AWetlandStorage: Double; AWetlandArea: Double; 
                      AWetlandRechargeCoefficient: Double; APrincipalOutflowRouteNo: Integer; 
                      AWetlandType: Integer; AQDiv: Double; AWetlandsInflowRouteNo: Integer; 
                      AWetlandsOutflowRouteNo: Integer; ADiversionRouteNo: Integer; 
                      ABankfillCapacity: Double; ADiversionEfficiency: Double; 
                      AMaxMonthlyDiversionCapacity: Double; ALongitude: Double; ALatitude: Double): WordBool; safecall;
    function PopulateComprehensiveWetlandParams(ABankfillArea: Double; ABankfillVolume: Double; 
                                                APowerOfAreaCapCurve: Double; 
                                                ABankfillCapacity: Double; 
                                                AWetlandInflowProportion: Double; 
                                                AChannelInflowProportion: Double): WordBool; safecall;
    function AddInflowRoute(ARouteNo: Integer; const AFileName: WideString): IInflowRoute; safecall;
    function AddOutflowRoute(ARouteNo: Integer; const AFileName: WideString; 
                             const AAbstractions: WideString; AStorageState: Double; 
                             AReductionFactor: Double): IOutflowRoute; safecall;
    function Get_NoOfInFlowRoutes: Integer; safecall;
    function Get_InFlowRouteByRouteNo(ARouteNo: Integer): IInflowRoute; safecall;
    function Get_InflowRouteByIndex(AIndex: Integer): IInflowRoute; safecall;
    function Get_NoOfOutFlowRoutes: Integer; safecall;
    function Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IOutflowRoute; safecall;
    function Get_OutFlowRouteByIndex(AIndex: Integer): IOutflowRoute; safecall;
    function Get_BankfillArea: Double; safecall;
    procedure Set_BankfillArea(Value: Double); safecall;
    function Get_BankfillVolume: Double; safecall;
    procedure Set_BankfillVolume(Value: Double); safecall;
    function Get_PowerOfAreaCapCurve: Double; safecall;
    procedure Set_PowerOfAreaCapCurve(Value: Double); safecall;
    function Get_BankfillCapacityComprehensive: Double; safecall;
    procedure Set_BankfillCapacityComprehensive(Value: Double); safecall;
    function Get_WetlandInflowProportion: Double; safecall;
    procedure Set_WetlandInflowProportion(Value: Double); safecall;
    function Get_ChannelInflowProportion: Double; safecall;
    procedure Set_ChannelInflowProportion(Value: Double); safecall;
    property ChannelName: WideString read Get_ChannelName write Set_ChannelName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property WetlandMAP: Double read Get_WetlandMAP write Set_WetlandMAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MonthlyBedLoss: Double read Get_MonthlyBedLoss write Set_MonthlyBedLoss;
    property WetlandStorage: Double read Get_WetlandStorage write Set_WetlandStorage;
    property WetlandArea: Double read Get_WetlandArea write Set_WetlandArea;
    property WetlandRechargeCoefficient: Double read Get_WetlandRechargeCoefficient write Set_WetlandRechargeCoefficient;
    property PrincipalOutflowRouteNo: Integer read Get_PrincipalOutflowRouteNo write Set_PrincipalOutflowRouteNo;
    property FutureUse: Integer read Get_FutureUse write Set_FutureUse;
    property WetlandType: Integer read Get_WetlandType write Set_WetlandType;
    property QDiv: Double read Get_QDiv write Set_QDiv;
    property WetlandsInflowRouteNo: Integer read Get_WetlandsInflowRouteNo write Set_WetlandsInflowRouteNo;
    property WetlandsOutflowRouteNo: Integer read Get_WetlandsOutflowRouteNo write Set_WetlandsOutflowRouteNo;
    property DiversionRouteNo: Integer read Get_DiversionRouteNo write Set_DiversionRouteNo;
    property BankfillCapacity: Double read Get_BankfillCapacity write Set_BankfillCapacity;
    property DiversionEfficiency: Double read Get_DiversionEfficiency write Set_DiversionEfficiency;
    property MaxMonthlyDiversionCapacity: Double read Get_MaxMonthlyDiversionCapacity write Set_MaxMonthlyDiversionCapacity;
    property NoOfInFlowRoutes: Integer read Get_NoOfInFlowRoutes;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute read Get_InFlowRouteByRouteNo;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute read Get_InflowRouteByIndex;
    property NoOfOutFlowRoutes: Integer read Get_NoOfOutFlowRoutes;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute read Get_OutFlowRouteByRouteNo;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute read Get_OutFlowRouteByIndex;
    property BankfillArea: Double read Get_BankfillArea write Set_BankfillArea;
    property BankfillVolume: Double read Get_BankfillVolume write Set_BankfillVolume;
    property PowerOfAreaCapCurve: Double read Get_PowerOfAreaCapCurve write Set_PowerOfAreaCapCurve;
    property BankfillCapacityComprehensive: Double read Get_BankfillCapacityComprehensive write Set_BankfillCapacityComprehensive;
    property WetlandInflowProportion: Double read Get_WetlandInflowProportion write Set_WetlandInflowProportion;
    property ChannelInflowProportion: Double read Get_ChannelInflowProportion write Set_ChannelInflowProportion;
  end;

// *********************************************************************//
// DispIntf:  IChannelModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C0E3A02F-7C36-4AF4-9CCB-E091CC072B79}
// *********************************************************************//
  IChannelModuleDisp = dispinterface
    ['{C0E3A02F-7C36-4AF4-9CCB-E091CC072B79}']
    property ChannelName: WideString dispid 102;
    property VersionNo: Integer dispid 103;
    property WetlandMAP: Double dispid 104;
    property RainfallFileName: WideString dispid 105;
    property MonthlyBedLoss: Double dispid 106;
    property WetlandStorage: Double dispid 107;
    property WetlandArea: Double dispid 108;
    property WetlandRechargeCoefficient: Double dispid 109;
    property PrincipalOutflowRouteNo: Integer dispid 110;
    property FutureUse: Integer dispid 111;
    property WetlandType: Integer dispid 112;
    property QDiv: Double dispid 113;
    property WetlandsInflowRouteNo: Integer dispid 114;
    property WetlandsOutflowRouteNo: Integer dispid 115;
    property DiversionRouteNo: Integer dispid 116;
    property BankfillCapacity: Double dispid 117;
    property DiversionEfficiency: Double dispid 118;
    property MaxMonthlyDiversionCapacity: Double dispid 119;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AChannelName: WideString; AVersionNo: Integer; AWetlandMAP: Double; 
                      const ARainfallFileName: WideString; AMonthlyBedLoss: Double; 
                      AWetlandStorage: Double; AWetlandArea: Double; 
                      AWetlandRechargeCoefficient: Double; APrincipalOutflowRouteNo: Integer; 
                      AWetlandType: Integer; AQDiv: Double; AWetlandsInflowRouteNo: Integer; 
                      AWetlandsOutflowRouteNo: Integer; ADiversionRouteNo: Integer; 
                      ABankfillCapacity: Double; ADiversionEfficiency: Double; 
                      AMaxMonthlyDiversionCapacity: Double; ALongitude: Double; ALatitude: Double): WordBool; dispid 120;
    function PopulateComprehensiveWetlandParams(ABankfillArea: Double; ABankfillVolume: Double; 
                                                APowerOfAreaCapCurve: Double; 
                                                ABankfillCapacity: Double; 
                                                AWetlandInflowProportion: Double; 
                                                AChannelInflowProportion: Double): WordBool; dispid 121;
    function AddInflowRoute(ARouteNo: Integer; const AFileName: WideString): IInflowRoute; dispid 301;
    function AddOutflowRoute(ARouteNo: Integer; const AFileName: WideString; 
                             const AAbstractions: WideString; AStorageState: Double; 
                             AReductionFactor: Double): IOutflowRoute; dispid 302;
    property NoOfInFlowRoutes: Integer readonly dispid 303;
    property InFlowRouteByRouteNo[ARouteNo: Integer]: IInflowRoute readonly dispid 304;
    property InflowRouteByIndex[AIndex: Integer]: IInflowRoute readonly dispid 305;
    property NoOfOutFlowRoutes: Integer readonly dispid 306;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IOutflowRoute readonly dispid 307;
    property OutFlowRouteByIndex[AIndex: Integer]: IOutflowRoute readonly dispid 308;
    property BankfillArea: Double dispid 309;
    property BankfillVolume: Double dispid 310;
    property PowerOfAreaCapCurve: Double dispid 311;
    property BankfillCapacityComprehensive: Double dispid 312;
    property WetlandInflowProportion: Double dispid 313;
    property ChannelInflowProportion: Double dispid 314;
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IChannelModuleAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {75E83A87-E32E-4696-9AF4-5CB84544847C}
// *********************************************************************//
  IChannelModuleAgent = interface(IUnknown)
    ['{75E83A87-E32E-4696-9AF4-5CB84544847C}']
    function Get_ChannelModuleCount: Integer; safecall;
    function Get_ChannelModuleByID(AModuleID: Integer): IChannelModule; safecall;
    function Get_ChannelModuleByIndex(AIndex: Integer): IChannelModule; safecall;
    function Get_ChannelModuleByNumber(AModuleNumber: Integer): IChannelModule; safecall;
    function CreateNewChannelModule(ANetworkID: Integer): IChannelModule; safecall;
    function RemoveChannelModule(AModuleNumber: Integer): WordBool; safecall;
    property ChannelModuleCount: Integer read Get_ChannelModuleCount;
    property ChannelModuleByID[AModuleID: Integer]: IChannelModule read Get_ChannelModuleByID;
    property ChannelModuleByIndex[AIndex: Integer]: IChannelModule read Get_ChannelModuleByIndex;
    property ChannelModuleByNumber[AModuleNumber: Integer]: IChannelModule read Get_ChannelModuleByNumber;
  end;

// *********************************************************************//
// DispIntf:  IChannelModuleAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {75E83A87-E32E-4696-9AF4-5CB84544847C}
// *********************************************************************//
  IChannelModuleAgentDisp = dispinterface
    ['{75E83A87-E32E-4696-9AF4-5CB84544847C}']
    property ChannelModuleCount: Integer readonly dispid 101;
    property ChannelModuleByID[AModuleID: Integer]: IChannelModule readonly dispid 102;
    property ChannelModuleByIndex[AIndex: Integer]: IChannelModule readonly dispid 103;
    property ChannelModuleByNumber[AModuleNumber: Integer]: IChannelModule readonly dispid 106;
    function CreateNewChannelModule(ANetworkID: Integer): IChannelModule; dispid 107;
    function RemoveChannelModule(AModuleNumber: Integer): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IRunOffModule
// Flags:     (320) Dual OleAutomation
// GUID:      {098FBB48-D94A-4E3E-9543-762A9A821CE8}
// *********************************************************************//
  IRunOffModule = interface(INetworkModule)
    ['{098FBB48-D94A-4E3E-9543-762A9A821CE8}']
    function Get_RunOffName: WideString; safecall;
    procedure Set_RunOffName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_CatchmentArea: Double; safecall;
    procedure Set_CatchmentArea(Value: Double); safecall;
    function Get_CatchmentMAP: Double; safecall;
    procedure Set_CatchmentMAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_ProduceNaturalisedFlows: Integer; safecall;
    procedure Set_ProduceNaturalisedFlows(Value: Integer); safecall;
    function Get_APanFactor(AMonthIndex: Integer): Double; safecall;
    procedure Set_APanFactor(AMonthIndex: Integer; Value: Double); safecall;
    function Get_PitmanModel: IRunOffPitmanModel; safecall;
    function Get_Afforestation: IRunOffAfforestation; safecall;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const ARunOffName: WideString; AVersionNo: Integer; ACatchmentArea: Double; 
                      ACatchmentMAP: Double; const ARainfallFileName: WideString; 
                      AProduceNaturalisedInflows: Integer; ALongitude: Double; ALatitude: Double; 
                      const AAPanFactors: WideString): WordBool; safecall;
    function Get_AlienVegetation: IRunOffAlienVegetation; safecall;
    function Get_PavedArea: IRunOffPavedArea; safecall;
    procedure AddOutflowRoute(ARouteNo: Integer; AOutflowPercentage: Double); safecall;
    function Get_NoOfOutFlowRoutes: Integer; safecall;
    function Get_HughesModel: IRunOffHughesModel; safecall;
    function Get_SamiModel: IRunOffSamiModel; safecall;
    function Get_NoOfSlaves: Integer; safecall;
    procedure AddSlaveModuleNo(ASlaveModuleNo: Integer); safecall;
    function Get_GroundWaterAbstraction: IRunOffGroundWaterAbstraction; safecall;
    function SlaveModuleNoByIndex(AIndex: Integer): Integer; safecall;
    function Get_OutFlowRouteByRouteNo(ARouteNo: Integer): IRunOffOutflowRoute; safecall;
    function Get_OutFlowRouteByIndex(AIndex: Integer): IRunOffOutflowRoute; safecall;
    property RunOffName: WideString read Get_RunOffName write Set_RunOffName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property CatchmentArea: Double read Get_CatchmentArea write Set_CatchmentArea;
    property CatchmentMAP: Double read Get_CatchmentMAP write Set_CatchmentMAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property ProduceNaturalisedFlows: Integer read Get_ProduceNaturalisedFlows write Set_ProduceNaturalisedFlows;
    property APanFactor[AMonthIndex: Integer]: Double read Get_APanFactor write Set_APanFactor;
    property PitmanModel: IRunOffPitmanModel read Get_PitmanModel;
    property Afforestation: IRunOffAfforestation read Get_Afforestation;
    property AlienVegetation: IRunOffAlienVegetation read Get_AlienVegetation;
    property PavedArea: IRunOffPavedArea read Get_PavedArea;
    property NoOfOutFlowRoutes: Integer read Get_NoOfOutFlowRoutes;
    property HughesModel: IRunOffHughesModel read Get_HughesModel;
    property SamiModel: IRunOffSamiModel read Get_SamiModel;
    property NoOfSlaves: Integer read Get_NoOfSlaves;
    property GroundWaterAbstraction: IRunOffGroundWaterAbstraction read Get_GroundWaterAbstraction;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IRunOffOutflowRoute read Get_OutFlowRouteByRouteNo;
    property OutFlowRouteByIndex[AIndex: Integer]: IRunOffOutflowRoute read Get_OutFlowRouteByIndex;
  end;

// *********************************************************************//
// DispIntf:  IRunOffModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {098FBB48-D94A-4E3E-9543-762A9A821CE8}
// *********************************************************************//
  IRunOffModuleDisp = dispinterface
    ['{098FBB48-D94A-4E3E-9543-762A9A821CE8}']
    property RunOffName: WideString dispid 102;
    property VersionNo: Integer dispid 103;
    property CatchmentArea: Double dispid 104;
    property CatchmentMAP: Double dispid 105;
    property RainfallFileName: WideString dispid 106;
    property ProduceNaturalisedFlows: Integer dispid 119;
    property APanFactor[AMonthIndex: Integer]: Double dispid 122;
    property PitmanModel: IRunOffPitmanModel readonly dispid 123;
    property Afforestation: IRunOffAfforestation readonly dispid 107;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const ARunOffName: WideString; AVersionNo: Integer; ACatchmentArea: Double; 
                      ACatchmentMAP: Double; const ARainfallFileName: WideString; 
                      AProduceNaturalisedInflows: Integer; ALongitude: Double; ALatitude: Double; 
                      const AAPanFactors: WideString): WordBool; dispid 108;
    property AlienVegetation: IRunOffAlienVegetation readonly dispid 109;
    property PavedArea: IRunOffPavedArea readonly dispid 112;
    procedure AddOutflowRoute(ARouteNo: Integer; AOutflowPercentage: Double); dispid 114;
    property NoOfOutFlowRoutes: Integer readonly dispid 115;
    property HughesModel: IRunOffHughesModel readonly dispid 118;
    property SamiModel: IRunOffSamiModel readonly dispid 125;
    property NoOfSlaves: Integer readonly dispid 127;
    procedure AddSlaveModuleNo(ASlaveModuleNo: Integer); dispid 129;
    property GroundWaterAbstraction: IRunOffGroundWaterAbstraction readonly dispid 130;
    function SlaveModuleNoByIndex(AIndex: Integer): Integer; dispid 128;
    property OutFlowRouteByRouteNo[ARouteNo: Integer]: IRunOffOutflowRoute readonly dispid 302;
    property OutFlowRouteByIndex[AIndex: Integer]: IRunOffOutflowRoute readonly dispid 303;
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IRunOffAfforestation
// Flags:     (320) Dual OleAutomation
// GUID:      {0351D8BC-4EF9-417B-A184-CB44E6D784E1}
// *********************************************************************//
  IRunOffAfforestation = interface(IUnknown)
    ['{0351D8BC-4EF9-417B-A184-CB44E6D784E1}']
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_PineAreaPercentage: Double; safecall;
    procedure Set_PineAreaPercentage(Value: Double); safecall;
    function Get_PineRotationPeriod: Integer; safecall;
    procedure Set_PineRotationPeriod(Value: Integer); safecall;
    function Get_EucalyptusAreaPercentage: Double; safecall;
    procedure Set_EucalyptusAreaPercentage(Value: Double); safecall;
    function Get_EucalyptusRotationPeriod: Integer; safecall;
    procedure Set_EucalyptusRotationPeriod(Value: Integer); safecall;
    function Get_WattleAreaPercentage: Double; safecall;
    procedure Set_WattleAreaPercentage(Value: Double); safecall;
    function Get_WattleRotationPeriod: Integer; safecall;
    procedure Set_WattleRotationPeriod(Value: Integer); safecall;
    function Get_OptimalAreaPercentage: Double; safecall;
    procedure Set_OptimalAreaPercentage(Value: Double); safecall;
    function Get_SFRReductionMAR: Double; safecall;
    procedure Set_SFRReductionMAR(Value: Double); safecall;
    function Get_SFRReductionLowFlows: Double; safecall;
    procedure Set_SFRReductionLowFlows(Value: Double); safecall;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Area(AYearIndex: Integer): Double; safecall;
    procedure Set_Area(AYearIndex: Integer; Value: Double); safecall;
    function Populate(AAlgorithm: Integer; APineAreaPercentage: Double; 
                      APineRotationPeriod: Integer; AEucalyptusAreaPercentage: Double; 
                      AEucalyptusRotationPeriod: Integer; AWattleAreaPercentage: Double; 
                      AWattleRotationPeriod: Integer; AOptimalAreaPercentage: Double; 
                      ASFRReductionMAR: Double; ASFRReductionLowFlows: Double): WordBool; safecall;
    procedure AddAreaData(AYear: Integer; AArea: Double); safecall;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property PineAreaPercentage: Double read Get_PineAreaPercentage write Set_PineAreaPercentage;
    property PineRotationPeriod: Integer read Get_PineRotationPeriod write Set_PineRotationPeriod;
    property EucalyptusAreaPercentage: Double read Get_EucalyptusAreaPercentage write Set_EucalyptusAreaPercentage;
    property EucalyptusRotationPeriod: Integer read Get_EucalyptusRotationPeriod write Set_EucalyptusRotationPeriod;
    property WattleAreaPercentage: Double read Get_WattleAreaPercentage write Set_WattleAreaPercentage;
    property WattleRotationPeriod: Integer read Get_WattleRotationPeriod write Set_WattleRotationPeriod;
    property OptimalAreaPercentage: Double read Get_OptimalAreaPercentage write Set_OptimalAreaPercentage;
    property SFRReductionMAR: Double read Get_SFRReductionMAR write Set_SFRReductionMAR;
    property SFRReductionLowFlows: Double read Get_SFRReductionLowFlows write Set_SFRReductionLowFlows;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Area[AYearIndex: Integer]: Double read Get_Area write Set_Area;
  end;

// *********************************************************************//
// DispIntf:  IRunOffAfforestationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0351D8BC-4EF9-417B-A184-CB44E6D784E1}
// *********************************************************************//
  IRunOffAfforestationDisp = dispinterface
    ['{0351D8BC-4EF9-417B-A184-CB44E6D784E1}']
    property Algorithm: Integer dispid 201;
    property PineAreaPercentage: Double dispid 202;
    property PineRotationPeriod: Integer dispid 203;
    property EucalyptusAreaPercentage: Double dispid 204;
    property EucalyptusRotationPeriod: Integer dispid 205;
    property WattleAreaPercentage: Double dispid 206;
    property WattleRotationPeriod: Integer dispid 207;
    property OptimalAreaPercentage: Double dispid 208;
    property SFRReductionMAR: Double dispid 209;
    property SFRReductionLowFlows: Double dispid 210;
    property NumberOfYears: Integer readonly dispid 101;
    property Year[AYearIndex: Integer]: Integer dispid 102;
    property Area[AYearIndex: Integer]: Double dispid 103;
    function Populate(AAlgorithm: Integer; APineAreaPercentage: Double; 
                      APineRotationPeriod: Integer; AEucalyptusAreaPercentage: Double; 
                      AEucalyptusRotationPeriod: Integer; AWattleAreaPercentage: Double; 
                      AWattleRotationPeriod: Integer; AOptimalAreaPercentage: Double; 
                      ASFRReductionMAR: Double; ASFRReductionLowFlows: Double): WordBool; dispid 104;
    procedure AddAreaData(AYear: Integer; AArea: Double); dispid 105;
  end;

// *********************************************************************//
// Interface: IRunOffPitmanModel
// Flags:     (320) Dual OleAutomation
// GUID:      {AC16446D-DEF4-41C5-A264-C9CE78F928B3}
// *********************************************************************//
  IRunOffPitmanModel = interface(IUnknown)
    ['{AC16446D-DEF4-41C5-A264-C9CE78F928B3}']
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    function Populate(APOW: Double; ASL: Integer; AST: Integer; AFT: Double; AGW: Double; 
                      AZMIN: Integer; AZMAX: Integer; API: Double; ATL: Double; AGL: Double; 
                      AR: Double; AFF: Double): WordBool; safecall;
    property POW: Double read Get_POW write Set_POW;
    property SL: Integer read Get_SL write Set_SL;
    property ST: Integer read Get_ST write Set_ST;
    property FT: Double read Get_FT write Set_FT;
    property GW: Double read Get_GW write Set_GW;
    property ZMIN: Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX: Integer read Get_ZMAX write Set_ZMAX;
    property PI: Double read Get_PI write Set_PI;
    property TL: Double read Get_TL write Set_TL;
    property GL: Double read Get_GL write Set_GL;
    property R: Double read Get_R write Set_R;
    property FF: Double read Get_FF write Set_FF;
  end;

// *********************************************************************//
// DispIntf:  IRunOffPitmanModelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {AC16446D-DEF4-41C5-A264-C9CE78F928B3}
// *********************************************************************//
  IRunOffPitmanModelDisp = dispinterface
    ['{AC16446D-DEF4-41C5-A264-C9CE78F928B3}']
    property POW: Double dispid 107;
    property SL: Integer dispid 108;
    property ST: Integer dispid 109;
    property FT: Double dispid 110;
    property GW: Double dispid 201;
    property ZMIN: Integer dispid 202;
    property ZMAX: Integer dispid 203;
    property PI: Double dispid 204;
    property TL: Double dispid 205;
    property GL: Double dispid 206;
    property R: Double dispid 207;
    property FF: Double dispid 208;
    function Populate(APOW: Double; ASL: Integer; AST: Integer; AFT: Double; AGW: Double; 
                      AZMIN: Integer; AZMAX: Integer; API: Double; ATL: Double; AGL: Double; 
                      AR: Double; AFF: Double): WordBool; dispid 101;
  end;

// *********************************************************************//
// Interface: IRunOffModuleAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {40325093-8CE0-43FA-8524-868CE0098212}
// *********************************************************************//
  IRunOffModuleAgent = interface(IUnknown)
    ['{40325093-8CE0-43FA-8524-868CE0098212}']
    function Get_RunOffModuleCount: Integer; safecall;
    function Get_RunOffModuleByID(AModuleID: Integer): IRunOffModule; safecall;
    function Get_RunOffModuleByIndex(AIndex: Integer): IRunOffModule; safecall;
    function Get_RunOffModuleByNumber(AModuleNumber: Integer): IRunOffModule; safecall;
    function CreateNewRunOffModule(ANetworkID: Integer): IRunOffModule; safecall;
    function RemoveRunOffModule(AModuleNumber: Integer): WordBool; safecall;
    property RunOffModuleCount: Integer read Get_RunOffModuleCount;
    property RunOffModuleByID[AModuleID: Integer]: IRunOffModule read Get_RunOffModuleByID;
    property RunOffModuleByIndex[AIndex: Integer]: IRunOffModule read Get_RunOffModuleByIndex;
    property RunOffModuleByNumber[AModuleNumber: Integer]: IRunOffModule read Get_RunOffModuleByNumber;
  end;

// *********************************************************************//
// DispIntf:  IRunOffModuleAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {40325093-8CE0-43FA-8524-868CE0098212}
// *********************************************************************//
  IRunOffModuleAgentDisp = dispinterface
    ['{40325093-8CE0-43FA-8524-868CE0098212}']
    property RunOffModuleCount: Integer readonly dispid 201;
    property RunOffModuleByID[AModuleID: Integer]: IRunOffModule readonly dispid 202;
    property RunOffModuleByIndex[AIndex: Integer]: IRunOffModule readonly dispid 203;
    property RunOffModuleByNumber[AModuleNumber: Integer]: IRunOffModule readonly dispid 101;
    function CreateNewRunOffModule(ANetworkID: Integer): IRunOffModule; dispid 102;
    function RemoveRunOffModule(AModuleNumber: Integer): WordBool; dispid 103;
  end;

// *********************************************************************//
// Interface: IRunOffAlienVegetation
// Flags:     (320) Dual OleAutomation
// GUID:      {8D65C35B-D37D-480F-BEB7-11DA13913E76}
// *********************************************************************//
  IRunOffAlienVegetation = interface(IUnknown)
    ['{8D65C35B-D37D-480F-BEB7-11DA13913E76}']
    function Get_Algorithm: Integer; safecall;
    procedure Set_Algorithm(Value: Integer); safecall;
    function Get_RiparianVegetationArea: Double; safecall;
    procedure Set_RiparianVegetationArea(Value: Double); safecall;
    function Get_TallTreeAreaPercentage: Double; safecall;
    procedure Set_TallTreeAreaPercentage(Value: Double); safecall;
    function Get_TallTreeAge: Double; safecall;
    procedure Set_TallTreeAge(Value: Double); safecall;
    function Get_MediumTreeAreaPercentage: Double; safecall;
    procedure Set_MediumTreeAreaPercentage(Value: Double); safecall;
    function Get_MediumTreeAge: Double; safecall;
    procedure Set_MediumTreeAge(Value: Double); safecall;
    function Get_TallSchrubAreaPercentage: Double; safecall;
    procedure Set_TallSchrubAreaPercentage(Value: Double); safecall;
    function Get_TallSchrubAge: Double; safecall;
    procedure Set_TallSchrubAge(Value: Double); safecall;
    function Get_OptimalAreaPercentage: Double; safecall;
    procedure Set_OptimalAreaPercentage(Value: Double); safecall;
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Area(AYearIndex: Integer): Double; safecall;
    procedure Set_Area(AYearIndex: Integer; Value: Double); safecall;
    function Populate(AAlgorithm: Integer; ARiparianVegetationArea: Double; 
                      ATallTreeAreaPercentage: Double; ATallTreeAge: Double; 
                      AMediumTreeAreaPercentage: Double; AMediumTreeAge: Double; 
                      ATallSchrubAreaPercentage: Double; ATallSchrubAge: Double; 
                      AOptimalAreaPercentage: Double): WordBool; safecall;
    procedure AddAreaData(AYear: Integer; AArea: Double); safecall;
    property Algorithm: Integer read Get_Algorithm write Set_Algorithm;
    property RiparianVegetationArea: Double read Get_RiparianVegetationArea write Set_RiparianVegetationArea;
    property TallTreeAreaPercentage: Double read Get_TallTreeAreaPercentage write Set_TallTreeAreaPercentage;
    property TallTreeAge: Double read Get_TallTreeAge write Set_TallTreeAge;
    property MediumTreeAreaPercentage: Double read Get_MediumTreeAreaPercentage write Set_MediumTreeAreaPercentage;
    property MediumTreeAge: Double read Get_MediumTreeAge write Set_MediumTreeAge;
    property TallSchrubAreaPercentage: Double read Get_TallSchrubAreaPercentage write Set_TallSchrubAreaPercentage;
    property TallSchrubAge: Double read Get_TallSchrubAge write Set_TallSchrubAge;
    property OptimalAreaPercentage: Double read Get_OptimalAreaPercentage write Set_OptimalAreaPercentage;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Area[AYearIndex: Integer]: Double read Get_Area write Set_Area;
  end;

// *********************************************************************//
// DispIntf:  IRunOffAlienVegetationDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {8D65C35B-D37D-480F-BEB7-11DA13913E76}
// *********************************************************************//
  IRunOffAlienVegetationDisp = dispinterface
    ['{8D65C35B-D37D-480F-BEB7-11DA13913E76}']
    property Algorithm: Integer dispid 101;
    property RiparianVegetationArea: Double dispid 102;
    property TallTreeAreaPercentage: Double dispid 103;
    property TallTreeAge: Double dispid 104;
    property MediumTreeAreaPercentage: Double dispid 105;
    property MediumTreeAge: Double dispid 106;
    property TallSchrubAreaPercentage: Double dispid 107;
    property TallSchrubAge: Double dispid 108;
    property OptimalAreaPercentage: Double dispid 109;
    property NumberOfYears: Integer readonly dispid 110;
    property Year[AYearIndex: Integer]: Integer dispid 111;
    property Area[AYearIndex: Integer]: Double dispid 112;
    function Populate(AAlgorithm: Integer; ARiparianVegetationArea: Double; 
                      ATallTreeAreaPercentage: Double; ATallTreeAge: Double; 
                      AMediumTreeAreaPercentage: Double; AMediumTreeAge: Double; 
                      ATallSchrubAreaPercentage: Double; ATallSchrubAge: Double; 
                      AOptimalAreaPercentage: Double): WordBool; dispid 113;
    procedure AddAreaData(AYear: Integer; AArea: Double); dispid 114;
  end;

// *********************************************************************//
// Interface: IRunOffPavedArea
// Flags:     (320) Dual OleAutomation
// GUID:      {976D0007-9641-4DF5-9C16-06D70F2D01D9}
// *********************************************************************//
  IRunOffPavedArea = interface(IUnknown)
    ['{976D0007-9641-4DF5-9C16-06D70F2D01D9}']
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_Proportion(AYearIndex: Integer): Double; safecall;
    procedure Set_Proportion(AYearIndex: Integer; Value: Double); safecall;
    procedure AddAreaData(AYear: Integer; AProportion: Double); safecall;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property Proportion[AYearIndex: Integer]: Double read Get_Proportion write Set_Proportion;
  end;

// *********************************************************************//
// DispIntf:  IRunOffPavedAreaDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {976D0007-9641-4DF5-9C16-06D70F2D01D9}
// *********************************************************************//
  IRunOffPavedAreaDisp = dispinterface
    ['{976D0007-9641-4DF5-9C16-06D70F2D01D9}']
    property NumberOfYears: Integer readonly dispid 101;
    property Year[AYearIndex: Integer]: Integer dispid 102;
    property Proportion[AYearIndex: Integer]: Double dispid 103;
    procedure AddAreaData(AYear: Integer; AProportion: Double); dispid 104;
  end;

// *********************************************************************//
// Interface: IRunOffOutflowRoute
// Flags:     (320) Dual OleAutomation
// GUID:      {8B4E1D7C-1E73-47B6-A6FF-E1D034E53353}
// *********************************************************************//
  IRunOffOutflowRoute = interface(IUnknown)
    ['{8B4E1D7C-1E73-47B6-A6FF-E1D034E53353}']
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_OutflowPercentage: Double; safecall;
    procedure Set_OutflowPercentage(Value: Double); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property OutflowPercentage: Double read Get_OutflowPercentage write Set_OutflowPercentage;
  end;

// *********************************************************************//
// DispIntf:  IRunOffOutflowRouteDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {8B4E1D7C-1E73-47B6-A6FF-E1D034E53353}
// *********************************************************************//
  IRunOffOutflowRouteDisp = dispinterface
    ['{8B4E1D7C-1E73-47B6-A6FF-E1D034E53353}']
    property RouteNo: Integer dispid 101;
    property OutflowPercentage: Double dispid 102;
  end;

// *********************************************************************//
// Interface: IRunOffHughesModel
// Flags:     (320) Dual OleAutomation
// GUID:      {DB8C7D39-907B-44FD-B374-D1E2BA2D80E5}
// *********************************************************************//
  IRunOffHughesModel = interface(IUnknown)
    ['{DB8C7D39-907B-44FD-B374-D1E2BA2D80E5}']
    function Get_InflowRouteNo: Integer; safecall;
    procedure Set_InflowRouteNo(Value: Integer); safecall;
    function Get_InfluenceROMNo: Integer; safecall;
    procedure Set_InfluenceROMNo(Value: Integer); safecall;
    function Get_GroundWaterModel: Integer; safecall;
    procedure Set_GroundWaterModel(Value: Integer); safecall;
    function Get_HGSL: Double; safecall;
    procedure Set_HGSL(Value: Double); safecall;
    function Get_GPOW: Double; safecall;
    procedure Set_GPOW(Value: Double); safecall;
    function Get_TLGMax: Double; safecall;
    procedure Set_TLGMax(Value: Double); safecall;
    function Get_HGGW: Double; safecall;
    procedure Set_HGGW(Value: Double); safecall;
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    function Get_UseNoOfReaches: Integer; safecall;
    procedure Set_UseNoOfReaches(Value: Integer); safecall;
    function Get_DrainageDensity: Double; safecall;
    procedure Set_DrainageDensity(Value: Double); safecall;
    function Get_NoOfReaches: Integer; safecall;
    procedure Set_NoOfReaches(Value: Integer); safecall;
    function Get_RiparianAreaWidthPercentage: Double; safecall;
    procedure Set_RiparianAreaWidthPercentage(Value: Double); safecall;
    function Get_RiparianStripFactor: Double; safecall;
    procedure Set_RiparianStripFactor(Value: Double); safecall;
    function Get_RestWaterLevel: Double; safecall;
    procedure Set_RestWaterLevel(Value: Double); safecall;
    function Get_Transmissivity: Double; safecall;
    procedure Set_Transmissivity(Value: Double); safecall;
    function Get_Storativity: Double; safecall;
    procedure Set_Storativity(Value: Double); safecall;
    function Get_GroundWaterSlope: Double; safecall;
    procedure Set_GroundWaterSlope(Value: Double); safecall;
    function Get_AnnualUpperZoneAbstraction: Double; safecall;
    procedure Set_AnnualUpperZoneAbstraction(Value: Double); safecall;
    function Get_MonthlyUpperZoneAbstraction(AMonthIndex: Integer): Double; safecall;
    procedure Set_MonthlyUpperZoneAbstraction(AMonthIndex: Integer; Value: Double); safecall;
    function Get_AnnualRiparianZoneAbstraction: Double; safecall;
    procedure Set_AnnualRiparianZoneAbstraction(Value: Double); safecall;
    function Get_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer): Double; safecall;
    procedure Set_MonthlyRiparianZoneAbstraction(AMonthIndex: Integer; Value: Double); safecall;
    function Populate(AInflowRouteNo: Integer; AInfluenceROMNo: Integer; 
                      AGroundWaterModel: Integer; AHGSL: Double; AGPOW: Double; ATLGMax: Double; 
                      AHGGW: Double; APOW: Double; ASL: Integer; AST: Integer; AFT: Double; 
                      AGW: Double; AZMIN: Integer; AZMAX: Integer; API: Double; ATL: Double; 
                      AGL: Double; AR: Double; AFF: Double; AUseNoOfReaches: Integer; 
                      ADrainageDensity: Double; ANoOfReaches: Integer; 
                      ARiparianAreaWidthPercentage: Double; ARiparianStripFactor: Double; 
                      ARestWaterLevel: Double; ATransmissivity: Double; AStorativity: Double; 
                      AGroundWaterSlope: Double; AAnnualUpperZoneAbstraction: Double; 
                      const AMonthlyUpperZoneAbstraction: WideString; 
                      AAnnualRiparianZoneAbstraction: Double; 
                      const AMonthlyRiparionZoneAbstraction: WideString): WordBool; safecall;
    property InflowRouteNo: Integer read Get_InflowRouteNo write Set_InflowRouteNo;
    property InfluenceROMNo: Integer read Get_InfluenceROMNo write Set_InfluenceROMNo;
    property GroundWaterModel: Integer read Get_GroundWaterModel write Set_GroundWaterModel;
    property HGSL: Double read Get_HGSL write Set_HGSL;
    property GPOW: Double read Get_GPOW write Set_GPOW;
    property TLGMax: Double read Get_TLGMax write Set_TLGMax;
    property HGGW: Double read Get_HGGW write Set_HGGW;
    property POW: Double read Get_POW write Set_POW;
    property SL: Integer read Get_SL write Set_SL;
    property ST: Integer read Get_ST write Set_ST;
    property FT: Double read Get_FT write Set_FT;
    property GW: Double read Get_GW write Set_GW;
    property ZMIN: Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX: Integer read Get_ZMAX write Set_ZMAX;
    property PI: Double read Get_PI write Set_PI;
    property TL: Double read Get_TL write Set_TL;
    property GL: Double read Get_GL write Set_GL;
    property R: Double read Get_R write Set_R;
    property FF: Double read Get_FF write Set_FF;
    property UseNoOfReaches: Integer read Get_UseNoOfReaches write Set_UseNoOfReaches;
    property DrainageDensity: Double read Get_DrainageDensity write Set_DrainageDensity;
    property NoOfReaches: Integer read Get_NoOfReaches write Set_NoOfReaches;
    property RiparianAreaWidthPercentage: Double read Get_RiparianAreaWidthPercentage write Set_RiparianAreaWidthPercentage;
    property RiparianStripFactor: Double read Get_RiparianStripFactor write Set_RiparianStripFactor;
    property RestWaterLevel: Double read Get_RestWaterLevel write Set_RestWaterLevel;
    property Transmissivity: Double read Get_Transmissivity write Set_Transmissivity;
    property Storativity: Double read Get_Storativity write Set_Storativity;
    property GroundWaterSlope: Double read Get_GroundWaterSlope write Set_GroundWaterSlope;
    property AnnualUpperZoneAbstraction: Double read Get_AnnualUpperZoneAbstraction write Set_AnnualUpperZoneAbstraction;
    property MonthlyUpperZoneAbstraction[AMonthIndex: Integer]: Double read Get_MonthlyUpperZoneAbstraction write Set_MonthlyUpperZoneAbstraction;
    property AnnualRiparianZoneAbstraction: Double read Get_AnnualRiparianZoneAbstraction write Set_AnnualRiparianZoneAbstraction;
    property MonthlyRiparianZoneAbstraction[AMonthIndex: Integer]: Double read Get_MonthlyRiparianZoneAbstraction write Set_MonthlyRiparianZoneAbstraction;
  end;

// *********************************************************************//
// DispIntf:  IRunOffHughesModelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {DB8C7D39-907B-44FD-B374-D1E2BA2D80E5}
// *********************************************************************//
  IRunOffHughesModelDisp = dispinterface
    ['{DB8C7D39-907B-44FD-B374-D1E2BA2D80E5}']
    property InflowRouteNo: Integer dispid 101;
    property InfluenceROMNo: Integer dispid 102;
    property GroundWaterModel: Integer dispid 103;
    property HGSL: Double dispid 104;
    property GPOW: Double dispid 105;
    property TLGMax: Double dispid 106;
    property HGGW: Double dispid 107;
    property POW: Double dispid 108;
    property SL: Integer dispid 109;
    property ST: Integer dispid 110;
    property FT: Double dispid 111;
    property GW: Double dispid 112;
    property ZMIN: Integer dispid 113;
    property ZMAX: Integer dispid 114;
    property PI: Double dispid 115;
    property TL: Double dispid 116;
    property GL: Double dispid 117;
    property R: Double dispid 118;
    property FF: Double dispid 119;
    property UseNoOfReaches: Integer dispid 120;
    property DrainageDensity: Double dispid 121;
    property NoOfReaches: Integer dispid 122;
    property RiparianAreaWidthPercentage: Double dispid 123;
    property RiparianStripFactor: Double dispid 124;
    property RestWaterLevel: Double dispid 125;
    property Transmissivity: Double dispid 126;
    property Storativity: Double dispid 127;
    property GroundWaterSlope: Double dispid 128;
    property AnnualUpperZoneAbstraction: Double dispid 129;
    property MonthlyUpperZoneAbstraction[AMonthIndex: Integer]: Double dispid 130;
    property AnnualRiparianZoneAbstraction: Double dispid 131;
    property MonthlyRiparianZoneAbstraction[AMonthIndex: Integer]: Double dispid 132;
    function Populate(AInflowRouteNo: Integer; AInfluenceROMNo: Integer; 
                      AGroundWaterModel: Integer; AHGSL: Double; AGPOW: Double; ATLGMax: Double; 
                      AHGGW: Double; APOW: Double; ASL: Integer; AST: Integer; AFT: Double; 
                      AGW: Double; AZMIN: Integer; AZMAX: Integer; API: Double; ATL: Double; 
                      AGL: Double; AR: Double; AFF: Double; AUseNoOfReaches: Integer; 
                      ADrainageDensity: Double; ANoOfReaches: Integer; 
                      ARiparianAreaWidthPercentage: Double; ARiparianStripFactor: Double; 
                      ARestWaterLevel: Double; ATransmissivity: Double; AStorativity: Double; 
                      AGroundWaterSlope: Double; AAnnualUpperZoneAbstraction: Double; 
                      const AMonthlyUpperZoneAbstraction: WideString; 
                      AAnnualRiparianZoneAbstraction: Double; 
                      const AMonthlyRiparionZoneAbstraction: WideString): WordBool; dispid 133;
  end;

// *********************************************************************//
// Interface: IRunOffSamiModel
// Flags:     (320) Dual OleAutomation
// GUID:      {A14D1F19-F686-4785-A920-24F640781267}
// *********************************************************************//
  IRunOffSamiModel = interface(IUnknown)
    ['{A14D1F19-F686-4785-A920-24F640781267}']
    function Get_AquiferThickness: Double; safecall;
    procedure Set_AquiferThickness(Value: Double); safecall;
    function Get_Storativity: Double; safecall;
    procedure Set_Storativity(Value: Double); safecall;
    function Get_InitialAquiferStorage: Double; safecall;
    procedure Set_InitialAquiferStorage(Value: Double); safecall;
    function Get_StaticWaterLevel: Double; safecall;
    procedure Set_StaticWaterLevel(Value: Double); safecall;
    function Get_UnsaturatedStorage: Double; safecall;
    procedure Set_UnsaturatedStorage(Value: Double); safecall;
    function Get_InitialUnsaturatedZoneStorage: Double; safecall;
    procedure Set_InitialUnsaturatedZoneStorage(Value: Double); safecall;
    function Get_PerculationPower: Double; safecall;
    procedure Set_PerculationPower(Value: Double); safecall;
    function Get_GPOW: Double; safecall;
    procedure Set_GPOW(Value: Double); safecall;
    function Get_MaxDischarge: Double; safecall;
    procedure Set_MaxDischarge(Value: Double); safecall;
    function Get_InteractionCurvePower: Double; safecall;
    procedure Set_InteractionCurvePower(Value: Double); safecall;
    function Get_HGSL: Double; safecall;
    procedure Set_HGSL(Value: Double); safecall;
    function Get_HGGW: Double; safecall;
    procedure Set_HGGW(Value: Double); safecall;
    function Get_MaxHydrologicalGradient: Double; safecall;
    procedure Set_MaxHydrologicalGradient(Value: Double); safecall;
    function Get_Transmissivity: Double; safecall;
    procedure Set_Transmissivity(Value: Double); safecall;
    function Get_BoreholeDistanceToRiver: Double; safecall;
    procedure Set_BoreholeDistanceToRiver(Value: Double); safecall;
    function Get_GroundWaterEvaporationArea: Double; safecall;
    procedure Set_GroundWaterEvaporationArea(Value: Double); safecall;
    function Get_K2: Double; safecall;
    procedure Set_K2(Value: Double); safecall;
    function Get_K3: Double; safecall;
    procedure Set_K3(Value: Double); safecall;
    function Get_InterflowLag: Double; safecall;
    procedure Set_InterflowLag(Value: Double); safecall;
    function Get_RechargeAveragedNoMonths: Double; safecall;
    procedure Set_RechargeAveragedNoMonths(Value: Double); safecall;
    function Get_UseAbstractions: Integer; safecall;
    procedure Set_UseAbstractions(Value: Integer); safecall;
    function Get_POW: Double; safecall;
    procedure Set_POW(Value: Double); safecall;
    function Get_SL: Integer; safecall;
    procedure Set_SL(Value: Integer); safecall;
    function Get_ST: Integer; safecall;
    procedure Set_ST(Value: Integer); safecall;
    function Get_FT: Double; safecall;
    procedure Set_FT(Value: Double); safecall;
    function Get_GW: Double; safecall;
    procedure Set_GW(Value: Double); safecall;
    function Get_ZMIN: Integer; safecall;
    procedure Set_ZMIN(Value: Integer); safecall;
    function Get_ZMAX: Integer; safecall;
    procedure Set_ZMAX(Value: Integer); safecall;
    function Get_PI: Double; safecall;
    procedure Set_PI(Value: Double); safecall;
    function Get_TL: Double; safecall;
    procedure Set_TL(Value: Double); safecall;
    function Get_GL: Double; safecall;
    procedure Set_GL(Value: Double); safecall;
    function Get_R: Double; safecall;
    procedure Set_R(Value: Double); safecall;
    function Get_FF: Double; safecall;
    procedure Set_FF(Value: Double); safecall;
    procedure Populate(AAquiferThickness: Double; AStorativity: Double; 
                       AInitialAquiferStorage: Double; AStaticWaterLevel: Double; 
                       AUnsaturatedStorage: Double; AInitialUnsaturatedZoneStorage: Double; 
                       APerculationPower: Double; AGPOW: Double; AMaxDischarge: Double; 
                       AInteractionCurvePower: Double; AHGSL: Double; AHGGW: Double; 
                       AMaxHydrologicalGradient: Double; ATransmissivity: Double; 
                       ABoreholeDistanceToRiver: Double; AGroundWaterEvaporationArea: Double; 
                       AK2: Double; AK3: Double; AInterflowLag: Double; 
                       ARechargeAveragedNoMonths: Double; AUseAbstractions: Integer; APOW: Double; 
                       ASL: Integer; AST: Integer; AFT: Double; AGW: Double; AZMIN: Integer; 
                       AZMAX: Integer; API: Double; ATL: Double; AGL: Double; AR: Double; 
                       AFF: Double); safecall;
    property AquiferThickness: Double read Get_AquiferThickness write Set_AquiferThickness;
    property Storativity: Double read Get_Storativity write Set_Storativity;
    property InitialAquiferStorage: Double read Get_InitialAquiferStorage write Set_InitialAquiferStorage;
    property StaticWaterLevel: Double read Get_StaticWaterLevel write Set_StaticWaterLevel;
    property UnsaturatedStorage: Double read Get_UnsaturatedStorage write Set_UnsaturatedStorage;
    property InitialUnsaturatedZoneStorage: Double read Get_InitialUnsaturatedZoneStorage write Set_InitialUnsaturatedZoneStorage;
    property PerculationPower: Double read Get_PerculationPower write Set_PerculationPower;
    property GPOW: Double read Get_GPOW write Set_GPOW;
    property MaxDischarge: Double read Get_MaxDischarge write Set_MaxDischarge;
    property InteractionCurvePower: Double read Get_InteractionCurvePower write Set_InteractionCurvePower;
    property HGSL: Double read Get_HGSL write Set_HGSL;
    property HGGW: Double read Get_HGGW write Set_HGGW;
    property MaxHydrologicalGradient: Double read Get_MaxHydrologicalGradient write Set_MaxHydrologicalGradient;
    property Transmissivity: Double read Get_Transmissivity write Set_Transmissivity;
    property BoreholeDistanceToRiver: Double read Get_BoreholeDistanceToRiver write Set_BoreholeDistanceToRiver;
    property GroundWaterEvaporationArea: Double read Get_GroundWaterEvaporationArea write Set_GroundWaterEvaporationArea;
    property K2: Double read Get_K2 write Set_K2;
    property K3: Double read Get_K3 write Set_K3;
    property InterflowLag: Double read Get_InterflowLag write Set_InterflowLag;
    property RechargeAveragedNoMonths: Double read Get_RechargeAveragedNoMonths write Set_RechargeAveragedNoMonths;
    property UseAbstractions: Integer read Get_UseAbstractions write Set_UseAbstractions;
    property POW: Double read Get_POW write Set_POW;
    property SL: Integer read Get_SL write Set_SL;
    property ST: Integer read Get_ST write Set_ST;
    property FT: Double read Get_FT write Set_FT;
    property GW: Double read Get_GW write Set_GW;
    property ZMIN: Integer read Get_ZMIN write Set_ZMIN;
    property ZMAX: Integer read Get_ZMAX write Set_ZMAX;
    property PI: Double read Get_PI write Set_PI;
    property TL: Double read Get_TL write Set_TL;
    property GL: Double read Get_GL write Set_GL;
    property R: Double read Get_R write Set_R;
    property FF: Double read Get_FF write Set_FF;
  end;

// *********************************************************************//
// DispIntf:  IRunOffSamiModelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {A14D1F19-F686-4785-A920-24F640781267}
// *********************************************************************//
  IRunOffSamiModelDisp = dispinterface
    ['{A14D1F19-F686-4785-A920-24F640781267}']
    property AquiferThickness: Double dispid 101;
    property Storativity: Double dispid 102;
    property InitialAquiferStorage: Double dispid 103;
    property StaticWaterLevel: Double dispid 104;
    property UnsaturatedStorage: Double dispid 105;
    property InitialUnsaturatedZoneStorage: Double dispid 106;
    property PerculationPower: Double dispid 107;
    property GPOW: Double dispid 108;
    property MaxDischarge: Double dispid 109;
    property InteractionCurvePower: Double dispid 110;
    property HGSL: Double dispid 111;
    property HGGW: Double dispid 112;
    property MaxHydrologicalGradient: Double dispid 113;
    property Transmissivity: Double dispid 114;
    property BoreholeDistanceToRiver: Double dispid 115;
    property GroundWaterEvaporationArea: Double dispid 116;
    property K2: Double dispid 117;
    property K3: Double dispid 118;
    property InterflowLag: Double dispid 119;
    property RechargeAveragedNoMonths: Double dispid 120;
    property UseAbstractions: Integer dispid 121;
    property POW: Double dispid 122;
    property SL: Integer dispid 123;
    property ST: Integer dispid 124;
    property FT: Double dispid 125;
    property GW: Double dispid 126;
    property ZMIN: Integer dispid 127;
    property ZMAX: Integer dispid 128;
    property PI: Double dispid 129;
    property TL: Double dispid 130;
    property GL: Double dispid 131;
    property R: Double dispid 132;
    property FF: Double dispid 133;
    procedure Populate(AAquiferThickness: Double; AStorativity: Double; 
                       AInitialAquiferStorage: Double; AStaticWaterLevel: Double; 
                       AUnsaturatedStorage: Double; AInitialUnsaturatedZoneStorage: Double; 
                       APerculationPower: Double; AGPOW: Double; AMaxDischarge: Double; 
                       AInteractionCurvePower: Double; AHGSL: Double; AHGGW: Double; 
                       AMaxHydrologicalGradient: Double; ATransmissivity: Double; 
                       ABoreholeDistanceToRiver: Double; AGroundWaterEvaporationArea: Double; 
                       AK2: Double; AK3: Double; AInterflowLag: Double; 
                       ARechargeAveragedNoMonths: Double; AUseAbstractions: Integer; APOW: Double; 
                       ASL: Integer; AST: Integer; AFT: Double; AGW: Double; AZMIN: Integer; 
                       AZMAX: Integer; API: Double; ATL: Double; AGL: Double; AR: Double; 
                       AFF: Double); dispid 134;
  end;

// *********************************************************************//
// Interface: IRunOffGroundWaterAbstraction
// Flags:     (320) Dual OleAutomation
// GUID:      {31499947-6F2B-4B45-9E6B-875D2DC5FB06}
// *********************************************************************//
  IRunOffGroundWaterAbstraction = interface(IUnknown)
    ['{31499947-6F2B-4B45-9E6B-875D2DC5FB06}']
    function Get_NumberOfYears: Integer; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    procedure Set_Year(AYearIndex: Integer; Value: Integer); safecall;
    function Get_AbstractionValue(AYearIndex: Integer): Double; safecall;
    procedure Set_AbstractionValue(AYearIndex: Integer; Value: Double); safecall;
    procedure AddAbstractionData(AYear: Integer; AAbstractionValue: Double); safecall;
    property NumberOfYears: Integer read Get_NumberOfYears;
    property Year[AYearIndex: Integer]: Integer read Get_Year write Set_Year;
    property AbstractionValue[AYearIndex: Integer]: Double read Get_AbstractionValue write Set_AbstractionValue;
  end;

// *********************************************************************//
// DispIntf:  IRunOffGroundWaterAbstractionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {31499947-6F2B-4B45-9E6B-875D2DC5FB06}
// *********************************************************************//
  IRunOffGroundWaterAbstractionDisp = dispinterface
    ['{31499947-6F2B-4B45-9E6B-875D2DC5FB06}']
    property NumberOfYears: Integer readonly dispid 101;
    property Year[AYearIndex: Integer]: Integer dispid 102;
    property AbstractionValue[AYearIndex: Integer]: Double dispid 103;
    procedure AddAbstractionData(AYear: Integer; AAbstractionValue: Double); dispid 104;
  end;

// *********************************************************************//
// Interface: INetworkRoute
// Flags:     (320) Dual OleAutomation
// GUID:      {C0BA59E3-2AE2-43B9-883D-202697729CE6}
// *********************************************************************//
  INetworkRoute = interface(IUnknown)
    ['{C0BA59E3-2AE2-43B9-883D-202697729CE6}']
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_SourceModuleID: Integer; safecall;
    procedure Set_SourceModuleID(Value: Integer); safecall;
    function Get_SinkModuleID: Integer; safecall;
    procedure Set_SinkModuleID(Value: Integer); safecall;
    function Get_RouteCost: Integer; safecall;
    procedure Set_RouteCost(Value: Integer); safecall;
    function Populate(ANetworkID: Integer; ARouteID: Integer; ARouteNo: Integer; 
                      ASourceModuleID: Integer; ASinkModuleID: Integer; ARouteCost: Integer): WordBool; safecall;
    function Get_RouteID: Integer; safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property SourceModuleID: Integer read Get_SourceModuleID write Set_SourceModuleID;
    property SinkModuleID: Integer read Get_SinkModuleID write Set_SinkModuleID;
    property RouteCost: Integer read Get_RouteCost write Set_RouteCost;
    property RouteID: Integer read Get_RouteID;
  end;

// *********************************************************************//
// DispIntf:  INetworkRouteDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {C0BA59E3-2AE2-43B9-883D-202697729CE6}
// *********************************************************************//
  INetworkRouteDisp = dispinterface
    ['{C0BA59E3-2AE2-43B9-883D-202697729CE6}']
    property RouteNo: Integer dispid 101;
    property SourceModuleID: Integer dispid 102;
    property SinkModuleID: Integer dispid 103;
    property RouteCost: Integer dispid 104;
    function Populate(ANetworkID: Integer; ARouteID: Integer; ARouteNo: Integer; 
                      ASourceModuleID: Integer; ASinkModuleID: Integer; ARouteCost: Integer): WordBool; dispid 105;
    property RouteID: Integer readonly dispid 106;
  end;

// *********************************************************************//
// Interface: INetworkRouteAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {E5D1BBCA-FA4E-4B0C-B0EF-3F481C9DF016}
// *********************************************************************//
  INetworkRouteAgent = interface(IUnknown)
    ['{E5D1BBCA-FA4E-4B0C-B0EF-3F481C9DF016}']
    function Get_NetworkRouteCount: Integer; safecall;
    function Get_NetworkRouteByRouteNo(ARouteNo: Integer): INetworkRoute; safecall;
    function Get_NetworkRouteByIndex(AIndex: Integer): INetworkRoute; safecall;
    function CreateNewNetworkRoute(ANetworkID: Integer): INetworkRoute; safecall;
    function RemoveNetworkRoute(ANetworkID: Integer; ARouteNo: Integer): WordBool; safecall;
    property NetworkRouteCount: Integer read Get_NetworkRouteCount;
    property NetworkRouteByRouteNo[ARouteNo: Integer]: INetworkRoute read Get_NetworkRouteByRouteNo;
    property NetworkRouteByIndex[AIndex: Integer]: INetworkRoute read Get_NetworkRouteByIndex;
  end;

// *********************************************************************//
// DispIntf:  INetworkRouteAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E5D1BBCA-FA4E-4B0C-B0EF-3F481C9DF016}
// *********************************************************************//
  INetworkRouteAgentDisp = dispinterface
    ['{E5D1BBCA-FA4E-4B0C-B0EF-3F481C9DF016}']
    property NetworkRouteCount: Integer readonly dispid 101;
    property NetworkRouteByRouteNo[ARouteNo: Integer]: INetworkRoute readonly dispid 102;
    property NetworkRouteByIndex[AIndex: Integer]: INetworkRoute readonly dispid 103;
    function CreateNewNetworkRoute(ANetworkID: Integer): INetworkRoute; dispid 106;
    function RemoveNetworkRoute(ANetworkID: Integer; ARouteNo: Integer): WordBool; dispid 107;
  end;

// *********************************************************************//
// Interface: IObservationPoint
// Flags:     (320) Dual OleAutomation
// GUID:      {E5B8B1B1-6E73-4F89-89E0-DEB194D74782}
// *********************************************************************//
  IObservationPoint = interface(IUnknown)
    ['{E5B8B1B1-6E73-4F89-89E0-DEB194D74782}']
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_FlowDataFileName: WideString; safecall;
    procedure Set_FlowDataFileName(const Value: WideString); safecall;
    function Populate(ANetworkID: Integer; ARouteNo: Integer; const AName: WideString; 
                      const AFlowDataFileName: WideString): WordBool; safecall;
    function Get_FlowData: ITimeSeries; safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property Name: WideString read Get_Name write Set_Name;
    property FlowDataFileName: WideString read Get_FlowDataFileName write Set_FlowDataFileName;
    property FlowData: ITimeSeries read Get_FlowData;
  end;

// *********************************************************************//
// DispIntf:  IObservationPointDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E5B8B1B1-6E73-4F89-89E0-DEB194D74782}
// *********************************************************************//
  IObservationPointDisp = dispinterface
    ['{E5B8B1B1-6E73-4F89-89E0-DEB194D74782}']
    property RouteNo: Integer dispid 101;
    property Name: WideString dispid 102;
    property FlowDataFileName: WideString dispid 103;
    function Populate(ANetworkID: Integer; ARouteNo: Integer; const AName: WideString; 
                      const AFlowDataFileName: WideString): WordBool; dispid 104;
    property FlowData: ITimeSeries readonly dispid 106;
  end;

// *********************************************************************//
// Interface: IObservationPointAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {E3F12CBE-9C3A-41E5-BF26-636022F9A88D}
// *********************************************************************//
  IObservationPointAgent = interface(IUnknown)
    ['{E3F12CBE-9C3A-41E5-BF26-636022F9A88D}']
    function Get_ObservationPointCount: Integer; safecall;
    function Get_ObservationPointByRouteNo(ARouteNo: Integer): IObservationPoint; safecall;
    function Get_ObservationPointByIndex(AIndex: Integer): IObservationPoint; safecall;
    function CreateNewObservationPoint(ANetworkID: Integer; ARouteNo: Integer): IObservationPoint; safecall;
    function RemoveObservationPoint(ANetworkID: Integer; ARouteNo: Integer): WordBool; safecall;
    property ObservationPointCount: Integer read Get_ObservationPointCount;
    property ObservationPointByRouteNo[ARouteNo: Integer]: IObservationPoint read Get_ObservationPointByRouteNo;
    property ObservationPointByIndex[AIndex: Integer]: IObservationPoint read Get_ObservationPointByIndex;
  end;

// *********************************************************************//
// DispIntf:  IObservationPointAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E3F12CBE-9C3A-41E5-BF26-636022F9A88D}
// *********************************************************************//
  IObservationPointAgentDisp = dispinterface
    ['{E3F12CBE-9C3A-41E5-BF26-636022F9A88D}']
    property ObservationPointCount: Integer readonly dispid 101;
    property ObservationPointByRouteNo[ARouteNo: Integer]: IObservationPoint readonly dispid 102;
    property ObservationPointByIndex[AIndex: Integer]: IObservationPoint readonly dispid 103;
    function CreateNewObservationPoint(ANetworkID: Integer; ARouteNo: Integer): IObservationPoint; dispid 106;
    function RemoveObservationPoint(ANetworkID: Integer; ARouteNo: Integer): WordBool; dispid 107;
  end;

// *********************************************************************//
// Interface: IMineModule
// Flags:     (320) Dual OleAutomation
// GUID:      {99461795-DE79-4C22-A51C-0B36CBA4A39F}
// *********************************************************************//
  IMineModule = interface(INetworkModule)
    ['{99461795-DE79-4C22-A51C-0B36CBA4A39F}']
    function Get_MineName: WideString; safecall;
    procedure Set_MineName(const Value: WideString); safecall;
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_RunOffModuleNo: Integer; safecall;
    procedure Set_RunOffModuleNo(Value: Integer); safecall;
    function Get_OutflowRouteNoToRiver: Integer; safecall;
    procedure Set_OutflowRouteNoToRiver(Value: Integer); safecall;
    function Get_OutflowRouteNoToPCD: Integer; safecall;
    procedure Set_OutflowRouteNoToPCD(Value: Integer); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_PlantArea: Double; safecall;
    procedure Set_PlantArea(Value: Double); safecall;
    function Get_PlantAreaRunOffFactor: Double; safecall;
    procedure Set_PlantAreaRunOffFactor(Value: Double); safecall;
    function Get_SaltBuildUpRate: Double; safecall;
    procedure Set_SaltBuildUpRate(Value: Double); safecall;
    function Get_SaltWashOffFactor: Double; safecall;
    procedure Set_SaltWashOffFactor(Value: Double); safecall;
    function Get_InitialSaltStore: Double; safecall;
    procedure Set_InitialSaltStore(Value: Double); safecall;
    function Get_PlantAreaGrowthInterpolationType: Integer; safecall;
    procedure Set_PlantAreaGrowthInterpolationType(Value: Integer); safecall;
    function Get_NoOfPlantAreaGrowthPoints: Integer; safecall;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AMineName: WideString; AVersionNo: Integer; ARunOffModuleNo: Integer; 
                      AOutflowRouteNoToRiver: Integer; AOutflowRouteNoToPCD: Integer; 
                      const ARainfallFileName: WideString; AMAP: Double; APlantArea: Double; 
                      APlantAreaRunOffFactor: Double; ASaltBuildUpRate: Double; 
                      ASaltWashOffFactor: Double; AInitialSaltStore: Double; 
                      APlantAreaGrowthInterpolationType: Integer; ALongitude: Double; 
                      ALatitude: Double): WordBool; safecall;
    function Get_PlantAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_PlantAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_PlantAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PlantAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function AddPlantAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_NoOfOpencastPits: Integer; safecall;
    function Get_OpencastPitBySectionNo(ASectionNo: Integer): IOpencastPit; safecall;
    function Get_OpencastPitByIndex(AIndex: Integer): IOpencastPit; safecall;
    function Get_NoOfUndergroundSections: Integer; safecall;
    function Get_UndergroundSectionBySectionNo(ASectionNo: Integer): IUndergroundSection; safecall;
    function Get_UndergroundSectionByIndex(AIndex: Integer): IUndergroundSection; safecall;
    function Get_NoOfSlurryDumps: Integer; safecall;
    function Get_SlurryDumpBySectionNo(ASectionNo: Integer): ISlurryDump; safecall;
    function Get_SlurryDumpByIndex(AIndex: Integer): ISlurryDump; safecall;
    property MineName: WideString read Get_MineName write Set_MineName;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property RunOffModuleNo: Integer read Get_RunOffModuleNo write Set_RunOffModuleNo;
    property OutflowRouteNoToRiver: Integer read Get_OutflowRouteNoToRiver write Set_OutflowRouteNoToRiver;
    property OutflowRouteNoToPCD: Integer read Get_OutflowRouteNoToPCD write Set_OutflowRouteNoToPCD;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MAP: Double read Get_MAP write Set_MAP;
    property PlantArea: Double read Get_PlantArea write Set_PlantArea;
    property PlantAreaRunOffFactor: Double read Get_PlantAreaRunOffFactor write Set_PlantAreaRunOffFactor;
    property SaltBuildUpRate: Double read Get_SaltBuildUpRate write Set_SaltBuildUpRate;
    property SaltWashOffFactor: Double read Get_SaltWashOffFactor write Set_SaltWashOffFactor;
    property InitialSaltStore: Double read Get_InitialSaltStore write Set_InitialSaltStore;
    property PlantAreaGrowthInterpolationType: Integer read Get_PlantAreaGrowthInterpolationType write Set_PlantAreaGrowthInterpolationType;
    property NoOfPlantAreaGrowthPoints: Integer read Get_NoOfPlantAreaGrowthPoints;
    property PlantAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_PlantAreaGrowthYearByIndex write Set_PlantAreaGrowthYearByIndex;
    property PlantAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_PlantAreaGrowthFactorByIndex write Set_PlantAreaGrowthFactorByIndex;
    property NoOfOpencastPits: Integer read Get_NoOfOpencastPits;
    property OpencastPitBySectionNo[ASectionNo: Integer]: IOpencastPit read Get_OpencastPitBySectionNo;
    property OpencastPitByIndex[AIndex: Integer]: IOpencastPit read Get_OpencastPitByIndex;
    property NoOfUndergroundSections: Integer read Get_NoOfUndergroundSections;
    property UndergroundSectionBySectionNo[ASectionNo: Integer]: IUndergroundSection read Get_UndergroundSectionBySectionNo;
    property UndergroundSectionByIndex[AIndex: Integer]: IUndergroundSection read Get_UndergroundSectionByIndex;
    property NoOfSlurryDumps: Integer read Get_NoOfSlurryDumps;
    property SlurryDumpBySectionNo[ASectionNo: Integer]: ISlurryDump read Get_SlurryDumpBySectionNo;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump read Get_SlurryDumpByIndex;
  end;

// *********************************************************************//
// DispIntf:  IMineModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {99461795-DE79-4C22-A51C-0B36CBA4A39F}
// *********************************************************************//
  IMineModuleDisp = dispinterface
    ['{99461795-DE79-4C22-A51C-0B36CBA4A39F}']
    property MineName: WideString dispid 102;
    property VersionNo: Integer dispid 103;
    property RunOffModuleNo: Integer dispid 104;
    property OutflowRouteNoToRiver: Integer dispid 105;
    property OutflowRouteNoToPCD: Integer dispid 106;
    property RainfallFileName: WideString dispid 107;
    property MAP: Double dispid 108;
    property PlantArea: Double dispid 109;
    property PlantAreaRunOffFactor: Double dispid 110;
    property SaltBuildUpRate: Double dispid 111;
    property SaltWashOffFactor: Double dispid 112;
    property InitialSaltStore: Double dispid 113;
    property PlantAreaGrowthInterpolationType: Integer dispid 114;
    property NoOfPlantAreaGrowthPoints: Integer readonly dispid 115;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      const AMineName: WideString; AVersionNo: Integer; ARunOffModuleNo: Integer; 
                      AOutflowRouteNoToRiver: Integer; AOutflowRouteNoToPCD: Integer; 
                      const ARainfallFileName: WideString; AMAP: Double; APlantArea: Double; 
                      APlantAreaRunOffFactor: Double; ASaltBuildUpRate: Double; 
                      ASaltWashOffFactor: Double; AInitialSaltStore: Double; 
                      APlantAreaGrowthInterpolationType: Integer; ALongitude: Double; 
                      ALatitude: Double): WordBool; dispid 116;
    property PlantAreaGrowthYearByIndex[AIndex: Integer]: Integer dispid 301;
    property PlantAreaGrowthFactorByIndex[AIndex: Integer]: Double dispid 302;
    function AddPlantAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 303;
    property NoOfOpencastPits: Integer readonly dispid 305;
    property OpencastPitBySectionNo[ASectionNo: Integer]: IOpencastPit readonly dispid 306;
    property OpencastPitByIndex[AIndex: Integer]: IOpencastPit readonly dispid 307;
    property NoOfUndergroundSections: Integer readonly dispid 309;
    property UndergroundSectionBySectionNo[ASectionNo: Integer]: IUndergroundSection readonly dispid 310;
    property UndergroundSectionByIndex[AIndex: Integer]: IUndergroundSection readonly dispid 311;
    property NoOfSlurryDumps: Integer readonly dispid 313;
    property SlurryDumpBySectionNo[ASectionNo: Integer]: ISlurryDump readonly dispid 314;
    property SlurryDumpByIndex[AIndex: Integer]: ISlurryDump readonly dispid 315;
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IMineModuleAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {F523665A-1EEF-427F-8F87-9F00AB70791C}
// *********************************************************************//
  IMineModuleAgent = interface(IUnknown)
    ['{F523665A-1EEF-427F-8F87-9F00AB70791C}']
    function Get_MineModuleCount: Integer; safecall;
    function Get_MineModuleByID(AModuleID: Integer): IMineModule; safecall;
    function Get_MineModuleByIndex(AIndex: Integer): IMineModule; safecall;
    function Get_MineModuleByNumber(AModuleNumber: Integer): IMineModule; safecall;
    function CreateNewMineModule(ANetworkID: Integer): IMineModule; safecall;
    function RemoveMineModule(AModuleNumber: Integer): WordBool; safecall;
    property MineModuleCount: Integer read Get_MineModuleCount;
    property MineModuleByID[AModuleID: Integer]: IMineModule read Get_MineModuleByID;
    property MineModuleByIndex[AIndex: Integer]: IMineModule read Get_MineModuleByIndex;
    property MineModuleByNumber[AModuleNumber: Integer]: IMineModule read Get_MineModuleByNumber;
  end;

// *********************************************************************//
// DispIntf:  IMineModuleAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {F523665A-1EEF-427F-8F87-9F00AB70791C}
// *********************************************************************//
  IMineModuleAgentDisp = dispinterface
    ['{F523665A-1EEF-427F-8F87-9F00AB70791C}']
    property MineModuleCount: Integer readonly dispid 101;
    property MineModuleByID[AModuleID: Integer]: IMineModule readonly dispid 102;
    property MineModuleByIndex[AIndex: Integer]: IMineModule readonly dispid 103;
    property MineModuleByNumber[AModuleNumber: Integer]: IMineModule readonly dispid 106;
    function CreateNewMineModule(ANetworkID: Integer): IMineModule; dispid 107;
    function RemoveMineModule(AModuleNumber: Integer): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IPan
// Flags:     (320) Dual OleAutomation
// GUID:      {4A387CF7-3CB5-4FE6-B936-10DE6259C503}
// *********************************************************************//
  IPan = interface(IUnknown)
    ['{4A387CF7-3CB5-4FE6-B936-10DE6259C503}']
    function Get_Month: Integer; safecall;
    procedure Set_Month(Value: Integer); safecall;
    function Get_Evaporation: Double; safecall;
    procedure Set_Evaporation(Value: Double); safecall;
    function Get_Factor: Double; safecall;
    procedure Set_Factor(Value: Double); safecall;
    property Month: Integer read Get_Month write Set_Month;
    property Evaporation: Double read Get_Evaporation write Set_Evaporation;
    property Factor: Double read Get_Factor write Set_Factor;
  end;

// *********************************************************************//
// DispIntf:  IPanDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {4A387CF7-3CB5-4FE6-B936-10DE6259C503}
// *********************************************************************//
  IPanDisp = dispinterface
    ['{4A387CF7-3CB5-4FE6-B936-10DE6259C503}']
    property Month: Integer dispid 101;
    property Evaporation: Double dispid 102;
    property Factor: Double dispid 103;
  end;

// *********************************************************************//
// Interface: IInflowRoute
// Flags:     (320) Dual OleAutomation
// GUID:      {1C711808-065A-4BC0-AAAF-CE634002C2CC}
// *********************************************************************//
  IInflowRoute = interface(IUnknown)
    ['{1C711808-065A-4BC0-AAAF-CE634002C2CC}']
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property FileName: WideString read Get_FileName write Set_FileName;
  end;

// *********************************************************************//
// DispIntf:  IInflowRouteDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {1C711808-065A-4BC0-AAAF-CE634002C2CC}
// *********************************************************************//
  IInflowRouteDisp = dispinterface
    ['{1C711808-065A-4BC0-AAAF-CE634002C2CC}']
    property RouteNo: Integer dispid 101;
    property FileName: WideString dispid 102;
  end;

// *********************************************************************//
// Interface: IOutflowRoute
// Flags:     (320) Dual OleAutomation
// GUID:      {2E2B6217-A7A8-4CBD-A2C6-4095C3D0917C}
// *********************************************************************//
  IOutflowRoute = interface(IUnknown)
    ['{2E2B6217-A7A8-4CBD-A2C6-4095C3D0917C}']
    function Get_RouteNo: Integer; safecall;
    procedure Set_RouteNo(Value: Integer); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_AbstractionVolumeByMonth(AMonth: Integer): Double; safecall;
    procedure Set_AbstractionVolumeByMonth(AMonth: Integer; Value: Double); safecall;
    function Get_StorageState: Double; safecall;
    procedure Set_StorageState(Value: Double); safecall;
    function Get_ReductionFactor: Double; safecall;
    procedure Set_ReductionFactor(Value: Double); safecall;
    property RouteNo: Integer read Get_RouteNo write Set_RouteNo;
    property FileName: WideString read Get_FileName write Set_FileName;
    property AbstractionVolumeByMonth[AMonth: Integer]: Double read Get_AbstractionVolumeByMonth write Set_AbstractionVolumeByMonth;
    property StorageState: Double read Get_StorageState write Set_StorageState;
    property ReductionFactor: Double read Get_ReductionFactor write Set_ReductionFactor;
  end;

// *********************************************************************//
// DispIntf:  IOutflowRouteDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {2E2B6217-A7A8-4CBD-A2C6-4095C3D0917C}
// *********************************************************************//
  IOutflowRouteDisp = dispinterface
    ['{2E2B6217-A7A8-4CBD-A2C6-4095C3D0917C}']
    property RouteNo: Integer dispid 101;
    property FileName: WideString dispid 102;
    property AbstractionVolumeByMonth[AMonth: Integer]: Double dispid 103;
    property StorageState: Double dispid 104;
    property ReductionFactor: Double dispid 105;
  end;

// *********************************************************************//
// Interface: IOpencastPit
// Flags:     (320) Dual OleAutomation
// GUID:      {FCF3540C-CCFD-4D1F-AFDE-EB7485345061}
// *********************************************************************//
  IOpencastPit = interface(IUnknown)
    ['{FCF3540C-CCFD-4D1F-AFDE-EB7485345061}']
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_CoalReserveArea: Double; safecall;
    procedure Set_CoalReserveArea(Value: Double); safecall;
    function Get_WorkingsArea: Double; safecall;
    procedure Set_WorkingsArea(Value: Double); safecall;
    function Get_CommissionYear: Integer; safecall;
    procedure Set_CommissionYear(Value: Integer); safecall;
    function Get_CommissionMonth: Integer; safecall;
    procedure Set_CommissionMonth(Value: Integer); safecall;
    function Get_DecommissionYear: Integer; safecall;
    procedure Set_DecommissionYear(Value: Integer); safecall;
    function Get_DecommissionMonth: Integer; safecall;
    procedure Set_DecommissionMonth(Value: Integer); safecall;
    function Get_DisturbedArea: Double; safecall;
    procedure Set_DisturbedArea(Value: Double); safecall;
    function Get_RehabilitatedArea: Double; safecall;
    procedure Set_RehabilitatedArea(Value: Double); safecall;
    function Get_PitEvaporationArea: Double; safecall;
    procedure Set_PitEvaporationArea(Value: Double); safecall;
    function Get_WorkingsAreaInterpolationOption: Integer; safecall;
    procedure Set_WorkingsAreaInterpolationOption(Value: Integer); safecall;
    function Get_DisturbedAreaInterpolationOption: Integer; safecall;
    procedure Set_DisturbedAreaInterpolationOption(Value: Integer); safecall;
    function Get_RehabilitatedAreaInterpolationOption: Integer; safecall;
    procedure Set_RehabilitatedAreaInterpolationOption(Value: Integer); safecall;
    function Get_PitEvaporationAreaInterpolationOption: Integer; safecall;
    procedure Set_PitEvaporationAreaInterpolationOption(Value: Integer); safecall;
    function Get_DisturbedAreaRunOffFactor: Double; safecall;
    procedure Set_DisturbedAreaRunOffFactor(Value: Double); safecall;
    function Get_DisturbedWorkingsAreaRunOffFactor: Double; safecall;
    procedure Set_DisturbedWorkingsAreaRunOffFactor(Value: Double); safecall;
    function Get_WashOffParameter: Double; safecall;
    procedure Set_WashOffParameter(Value: Double); safecall;
    function Get_SulphateBuildUpRate: Double; safecall;
    procedure Set_SulphateBuildUpRate(Value: Double); safecall;
    function Get_InitialSaltMass: Double; safecall;
    procedure Set_InitialSaltMass(Value: Double); safecall;
    function Get_InspoilsStorageDecantVolume: Double; safecall;
    procedure Set_InspoilsStorageDecantVolume(Value: Double); safecall;
    function Get_InspoilsStorageSeepageVolume: Double; safecall;
    procedure Set_InspoilsStorageSeepageVolume(Value: Double); safecall;
    function Get_InspoilsStorageInitialVolume: Double; safecall;
    procedure Set_InspoilsStorageInitialVolume(Value: Double); safecall;
    function Get_InspoilsDecantInterpolationOption: Integer; safecall;
    procedure Set_InspoilsDecantInterpolationOption(Value: Integer); safecall;
    function Get_InspoilsSeepageInterpolationOption: Integer; safecall;
    procedure Set_InspoilsSeepageInterpolationOption(Value: Integer); safecall;
    function Get_MaxSeepageRate: Double; safecall;
    procedure Set_MaxSeepageRate(Value: Double); safecall;
    function Get_SeepageEquationExponent: Double; safecall;
    procedure Set_SeepageEquationExponent(Value: Double); safecall;
    function Get_PCDFullSurfaceArea: Double; safecall;
    procedure Set_PCDFullSurfaceArea(Value: Double); safecall;
    function Get_PCDCapacity: Double; safecall;
    procedure Set_PCDCapacity(Value: Double); safecall;
    function Get_PCDInitialVolume: Double; safecall;
    procedure Set_PCDInitialVolume(Value: Double); safecall;
    function Get_InspoilsDamConcentration: Double; safecall;
    procedure Set_InspoilsDamConcentration(Value: Double); safecall;
    function Get_StdDevWorkingsArea: Double; safecall;
    procedure Set_StdDevWorkingsArea(Value: Double); safecall;
    function Get_StdDevSeepageDecant: Double; safecall;
    procedure Set_StdDevSeepageDecant(Value: Double); safecall;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      ACoalReserveArea: Double; AWorkingsArea: Double; ACommissionYear: Integer; 
                      ACommissionMonth: Integer; ADecommissionYear: Integer; 
                      ADecommissionMonth: Integer; ADisturbedArea: Double; 
                      ARehabilitatedArea: Double; APitEvaporationArea: Double; 
                      AWorkingsAreaInterpolationOption: Integer; 
                      ADisturbedAreaInterpolationOption: Integer; 
                      ARehabilitatedAreaInterpolationOption: Integer; 
                      AOpenSurfaceAreaInterpolationOption: Integer; 
                      ADisturbedAreaRunOffFactor: Double; 
                      ADisturbedWorkingsAreaRunOffFactor: Double; AWashOffParameter: Double; 
                      ASulphateBuildUpRate: Double; AInitialSaltMass: Double; 
                      AInspoilsStorageDecantVolume: Double; AInspoilsStorageSeepageVolume: Double; 
                      AInspoilsStorageInitialVolume: Double; 
                      AInspoilsDecantInterpolationOption: Integer; 
                      AInspoilsSeepageInterpolationOption: Integer; AMaxSeepageRate: Double; 
                      ASeepageEquationExponent: Double; APCDFullSurfaceArea: Double; 
                      APCDCapacity: Double; APCDInitialVolume: Double; 
                      AInspoilsDamConcentration: Double; AStdDevWorkingsArea: Double; 
                      AStdDevSeepageDecant: Double): WordBool; safecall;
    function AddWorkingsAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddDisturbedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddRehabilitatedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddPitEvaporationGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddInspoilsDecantGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddInspoilsSeepageGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_WorkingsAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_WorkingsAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_WorkingsAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfWorkingsAreaGrowthPoints: Integer; safecall;
    function Get_DisturbedAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_DisturbedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_DisturbedAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_DisturbedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfDisturbedAreaGrowthPoints: Integer; safecall;
    function Get_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_RehabilitatedAreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_RehabilitatedAreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfRehabilitatedAreaGrowthPoints: Integer; safecall;
    function Get_PitEvaporationGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_PitEvaporationGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_PitEvaporationGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_PitEvaporationGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfPitEvaporationGrowthPoints: Integer; safecall;
    function Get_InspoilsDecantGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_InspoilsDecantGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_InspoilsDecantGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_InspoilsDecantGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfInspoilsDecantGrowthPoints: Integer; safecall;
    function Get_InspoilsSeepageGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_InspoilsSeepageGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_InspoilsSeepageGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_NoOfInspoilsSeepageGrowthPoints: Integer; safecall;
    function Get_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_DisturbedAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_DisturbedWorkingsAreaRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_WorkingsAreaQvsSLDNoOfPoints: Integer; safecall;
    function Get_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaQvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_WorkingsAreaQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_SeepageDecantQvsSLDNoOfPoints: Integer; safecall;
    function Get_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SeepageDecantQvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_SeepageDecantQvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function AddWorkingsAreaLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function AddSeepageDecantLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property CoalReserveArea: Double read Get_CoalReserveArea write Set_CoalReserveArea;
    property WorkingsArea: Double read Get_WorkingsArea write Set_WorkingsArea;
    property CommissionYear: Integer read Get_CommissionYear write Set_CommissionYear;
    property CommissionMonth: Integer read Get_CommissionMonth write Set_CommissionMonth;
    property DecommissionYear: Integer read Get_DecommissionYear write Set_DecommissionYear;
    property DecommissionMonth: Integer read Get_DecommissionMonth write Set_DecommissionMonth;
    property DisturbedArea: Double read Get_DisturbedArea write Set_DisturbedArea;
    property RehabilitatedArea: Double read Get_RehabilitatedArea write Set_RehabilitatedArea;
    property PitEvaporationArea: Double read Get_PitEvaporationArea write Set_PitEvaporationArea;
    property WorkingsAreaInterpolationOption: Integer read Get_WorkingsAreaInterpolationOption write Set_WorkingsAreaInterpolationOption;
    property DisturbedAreaInterpolationOption: Integer read Get_DisturbedAreaInterpolationOption write Set_DisturbedAreaInterpolationOption;
    property RehabilitatedAreaInterpolationOption: Integer read Get_RehabilitatedAreaInterpolationOption write Set_RehabilitatedAreaInterpolationOption;
    property PitEvaporationAreaInterpolationOption: Integer read Get_PitEvaporationAreaInterpolationOption write Set_PitEvaporationAreaInterpolationOption;
    property DisturbedAreaRunOffFactor: Double read Get_DisturbedAreaRunOffFactor write Set_DisturbedAreaRunOffFactor;
    property DisturbedWorkingsAreaRunOffFactor: Double read Get_DisturbedWorkingsAreaRunOffFactor write Set_DisturbedWorkingsAreaRunOffFactor;
    property WashOffParameter: Double read Get_WashOffParameter write Set_WashOffParameter;
    property SulphateBuildUpRate: Double read Get_SulphateBuildUpRate write Set_SulphateBuildUpRate;
    property InitialSaltMass: Double read Get_InitialSaltMass write Set_InitialSaltMass;
    property InspoilsStorageDecantVolume: Double read Get_InspoilsStorageDecantVolume write Set_InspoilsStorageDecantVolume;
    property InspoilsStorageSeepageVolume: Double read Get_InspoilsStorageSeepageVolume write Set_InspoilsStorageSeepageVolume;
    property InspoilsStorageInitialVolume: Double read Get_InspoilsStorageInitialVolume write Set_InspoilsStorageInitialVolume;
    property InspoilsDecantInterpolationOption: Integer read Get_InspoilsDecantInterpolationOption write Set_InspoilsDecantInterpolationOption;
    property InspoilsSeepageInterpolationOption: Integer read Get_InspoilsSeepageInterpolationOption write Set_InspoilsSeepageInterpolationOption;
    property MaxSeepageRate: Double read Get_MaxSeepageRate write Set_MaxSeepageRate;
    property SeepageEquationExponent: Double read Get_SeepageEquationExponent write Set_SeepageEquationExponent;
    property PCDFullSurfaceArea: Double read Get_PCDFullSurfaceArea write Set_PCDFullSurfaceArea;
    property PCDCapacity: Double read Get_PCDCapacity write Set_PCDCapacity;
    property PCDInitialVolume: Double read Get_PCDInitialVolume write Set_PCDInitialVolume;
    property InspoilsDamConcentration: Double read Get_InspoilsDamConcentration write Set_InspoilsDamConcentration;
    property StdDevWorkingsArea: Double read Get_StdDevWorkingsArea write Set_StdDevWorkingsArea;
    property StdDevSeepageDecant: Double read Get_StdDevSeepageDecant write Set_StdDevSeepageDecant;
    property WorkingsAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_WorkingsAreaGrowthYearByIndex write Set_WorkingsAreaGrowthYearByIndex;
    property WorkingsAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_WorkingsAreaGrowthFactorByIndex write Set_WorkingsAreaGrowthFactorByIndex;
    property NoOfWorkingsAreaGrowthPoints: Integer read Get_NoOfWorkingsAreaGrowthPoints;
    property DisturbedAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_DisturbedAreaGrowthYearByIndex write Set_DisturbedAreaGrowthYearByIndex;
    property DisturbedAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_DisturbedAreaGrowthFactorByIndex write Set_DisturbedAreaGrowthFactorByIndex;
    property NoOfDisturbedAreaGrowthPoints: Integer read Get_NoOfDisturbedAreaGrowthPoints;
    property RehabilitatedAreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_RehabilitatedAreaGrowthYearByIndex write Set_RehabilitatedAreaGrowthYearByIndex;
    property RehabilitatedAreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_RehabilitatedAreaGrowthFactorByIndex write Set_RehabilitatedAreaGrowthFactorByIndex;
    property NoOfRehabilitatedAreaGrowthPoints: Integer read Get_NoOfRehabilitatedAreaGrowthPoints;
    property PitEvaporationGrowthYearByIndex[AIndex: Integer]: Integer read Get_PitEvaporationGrowthYearByIndex write Set_PitEvaporationGrowthYearByIndex;
    property PitEvaporationGrowthFactorByIndex[AIndex: Integer]: Double read Get_PitEvaporationGrowthFactorByIndex write Set_PitEvaporationGrowthFactorByIndex;
    property NoOfPitEvaporationGrowthPoints: Integer read Get_NoOfPitEvaporationGrowthPoints;
    property InspoilsDecantGrowthYearByIndex[AIndex: Integer]: Integer read Get_InspoilsDecantGrowthYearByIndex write Set_InspoilsDecantGrowthYearByIndex;
    property InspoilsDecantGrowthFactorByIndex[AIndex: Integer]: Double read Get_InspoilsDecantGrowthFactorByIndex write Set_InspoilsDecantGrowthFactorByIndex;
    property NoOfInspoilsDecantGrowthPoints: Integer read Get_NoOfInspoilsDecantGrowthPoints;
    property InspoilsSeepageGrowthYearByIndex[AIndex: Integer]: Integer read Get_InspoilsSeepageGrowthYearByIndex write Set_InspoilsSeepageGrowthYearByIndex;
    property InspoilsSeepageGrowthFactorByIndex[AIndex: Integer]: Double read Get_InspoilsSeepageGrowthFactorByIndex write Set_InspoilsSeepageGrowthFactorByIndex;
    property NoOfInspoilsSeepageGrowthPoints: Integer read Get_NoOfInspoilsSeepageGrowthPoints;
    property DisturbedAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_DisturbedAreaRechargeFactorByMonth write Set_DisturbedAreaRechargeFactorByMonth;
    property DisturbedWorkingsAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_DisturbedWorkingsAreaRechargeFactorByMonth write Set_DisturbedWorkingsAreaRechargeFactorByMonth;
    property WorkingsAreaQvsSLDNoOfPoints: Integer read Get_WorkingsAreaQvsSLDNoOfPoints;
    property WorkingsAreaQvsSLDLoadByIndex[AIndex: Integer]: Double read Get_WorkingsAreaQvsSLDLoadByIndex write Set_WorkingsAreaQvsSLDLoadByIndex;
    property WorkingsAreaQvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_WorkingsAreaQvsSLDFlowRefByIndex write Set_WorkingsAreaQvsSLDFlowRefByIndex;
    property SeepageDecantQvsSLDNoOfPoints: Integer read Get_SeepageDecantQvsSLDNoOfPoints;
    property SeepageDecantQvsSLDLoadByIndex[AIndex: Integer]: Double read Get_SeepageDecantQvsSLDLoadByIndex write Set_SeepageDecantQvsSLDLoadByIndex;
    property SeepageDecantQvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_SeepageDecantQvsSLDFlowRefByIndex write Set_SeepageDecantQvsSLDFlowRefByIndex;
    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
  end;

// *********************************************************************//
// DispIntf:  IOpencastPitDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {FCF3540C-CCFD-4D1F-AFDE-EB7485345061}
// *********************************************************************//
  IOpencastPitDisp = dispinterface
    ['{FCF3540C-CCFD-4D1F-AFDE-EB7485345061}']
    property SectionNo: Integer dispid 101;
    property SectionName: WideString dispid 102;
    property CoalReserveArea: Double dispid 103;
    property WorkingsArea: Double dispid 104;
    property CommissionYear: Integer dispid 105;
    property CommissionMonth: Integer dispid 106;
    property DecommissionYear: Integer dispid 107;
    property DecommissionMonth: Integer dispid 108;
    property DisturbedArea: Double dispid 109;
    property RehabilitatedArea: Double dispid 110;
    property PitEvaporationArea: Double dispid 111;
    property WorkingsAreaInterpolationOption: Integer dispid 112;
    property DisturbedAreaInterpolationOption: Integer dispid 113;
    property RehabilitatedAreaInterpolationOption: Integer dispid 114;
    property PitEvaporationAreaInterpolationOption: Integer dispid 115;
    property DisturbedAreaRunOffFactor: Double dispid 116;
    property DisturbedWorkingsAreaRunOffFactor: Double dispid 117;
    property WashOffParameter: Double dispid 118;
    property SulphateBuildUpRate: Double dispid 119;
    property InitialSaltMass: Double dispid 120;
    property InspoilsStorageDecantVolume: Double dispid 121;
    property InspoilsStorageSeepageVolume: Double dispid 122;
    property InspoilsStorageInitialVolume: Double dispid 123;
    property InspoilsDecantInterpolationOption: Integer dispid 124;
    property InspoilsSeepageInterpolationOption: Integer dispid 125;
    property MaxSeepageRate: Double dispid 126;
    property SeepageEquationExponent: Double dispid 127;
    property PCDFullSurfaceArea: Double dispid 128;
    property PCDCapacity: Double dispid 129;
    property PCDInitialVolume: Double dispid 130;
    property InspoilsDamConcentration: Double dispid 131;
    property StdDevWorkingsArea: Double dispid 132;
    property StdDevSeepageDecant: Double dispid 133;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      ACoalReserveArea: Double; AWorkingsArea: Double; ACommissionYear: Integer; 
                      ACommissionMonth: Integer; ADecommissionYear: Integer; 
                      ADecommissionMonth: Integer; ADisturbedArea: Double; 
                      ARehabilitatedArea: Double; APitEvaporationArea: Double; 
                      AWorkingsAreaInterpolationOption: Integer; 
                      ADisturbedAreaInterpolationOption: Integer; 
                      ARehabilitatedAreaInterpolationOption: Integer; 
                      AOpenSurfaceAreaInterpolationOption: Integer; 
                      ADisturbedAreaRunOffFactor: Double; 
                      ADisturbedWorkingsAreaRunOffFactor: Double; AWashOffParameter: Double; 
                      ASulphateBuildUpRate: Double; AInitialSaltMass: Double; 
                      AInspoilsStorageDecantVolume: Double; AInspoilsStorageSeepageVolume: Double; 
                      AInspoilsStorageInitialVolume: Double; 
                      AInspoilsDecantInterpolationOption: Integer; 
                      AInspoilsSeepageInterpolationOption: Integer; AMaxSeepageRate: Double; 
                      ASeepageEquationExponent: Double; APCDFullSurfaceArea: Double; 
                      APCDCapacity: Double; APCDInitialVolume: Double; 
                      AInspoilsDamConcentration: Double; AStdDevWorkingsArea: Double; 
                      AStdDevSeepageDecant: Double): WordBool; dispid 134;
    function AddWorkingsAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 135;
    function AddDisturbedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 136;
    function AddRehabilitatedAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 137;
    function AddPitEvaporationGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 138;
    function AddInspoilsDecantGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 139;
    function AddInspoilsSeepageGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 140;
    property WorkingsAreaGrowthYearByIndex[AIndex: Integer]: Integer dispid 141;
    property WorkingsAreaGrowthFactorByIndex[AIndex: Integer]: Double dispid 142;
    property NoOfWorkingsAreaGrowthPoints: Integer readonly dispid 143;
    property DisturbedAreaGrowthYearByIndex[AIndex: Integer]: Integer dispid 144;
    property DisturbedAreaGrowthFactorByIndex[AIndex: Integer]: Double dispid 145;
    property NoOfDisturbedAreaGrowthPoints: Integer readonly dispid 146;
    property RehabilitatedAreaGrowthYearByIndex[AIndex: Integer]: Integer dispid 147;
    property RehabilitatedAreaGrowthFactorByIndex[AIndex: Integer]: Double dispid 148;
    property NoOfRehabilitatedAreaGrowthPoints: Integer readonly dispid 149;
    property PitEvaporationGrowthYearByIndex[AIndex: Integer]: Integer dispid 150;
    property PitEvaporationGrowthFactorByIndex[AIndex: Integer]: Double dispid 151;
    property NoOfPitEvaporationGrowthPoints: Integer readonly dispid 152;
    property InspoilsDecantGrowthYearByIndex[AIndex: Integer]: Integer dispid 153;
    property InspoilsDecantGrowthFactorByIndex[AIndex: Integer]: Double dispid 154;
    property NoOfInspoilsDecantGrowthPoints: Integer readonly dispid 155;
    property InspoilsSeepageGrowthYearByIndex[AIndex: Integer]: Integer dispid 156;
    property InspoilsSeepageGrowthFactorByIndex[AIndex: Integer]: Double dispid 157;
    property NoOfInspoilsSeepageGrowthPoints: Integer readonly dispid 158;
    property DisturbedAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double dispid 159;
    property DisturbedWorkingsAreaRechargeFactorByMonth[AMonthIndex: Integer]: Double dispid 160;
    property WorkingsAreaQvsSLDNoOfPoints: Integer readonly dispid 161;
    property WorkingsAreaQvsSLDLoadByIndex[AIndex: Integer]: Double dispid 162;
    property WorkingsAreaQvsSLDFlowRefByIndex[AIndex: Integer]: Double dispid 163;
    property SeepageDecantQvsSLDNoOfPoints: Integer readonly dispid 164;
    property SeepageDecantQvsSLDLoadByIndex[AIndex: Integer]: Double dispid 165;
    property SeepageDecantQvsSLDFlowRefByIndex[AIndex: Integer]: Double dispid 166;
    function AddWorkingsAreaLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; dispid 167;
    function AddSeepageDecantLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; dispid 168;
    property ModuleID: Integer dispid 170;
  end;

// *********************************************************************//
// Interface: IUndergroundSection
// Flags:     (320) Dual OleAutomation
// GUID:      {BEA1B616-53D1-4F24-9040-51693A8EF2E2}
// *********************************************************************//
  IUndergroundSection = interface(IUnknown)
    ['{BEA1B616-53D1-4F24-9040-51693A8EF2E2}']
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_OutFlowRouteNo: Integer; safecall;
    procedure Set_OutFlowRouteNo(Value: Integer); safecall;
    function Get_UpstreamCatchmentArea: Double; safecall;
    procedure Set_UpstreamCatchmentArea(Value: Double); safecall;
    function Get_BoardAndPillarArea: Double; safecall;
    procedure Set_BoardAndPillarArea(Value: Double); safecall;
    function Get_HighExtractionArea: Double; safecall;
    procedure Set_HighExtractionArea(Value: Double); safecall;
    function Get_SurfaceRunOffFactor: Double; safecall;
    procedure Set_SurfaceRunOffFactor(Value: Double); safecall;
    function Get_BoardAndPillarInterpolationOption: Integer; safecall;
    procedure Set_BoardAndPillarInterpolationOption(Value: Integer); safecall;
    function Get_HighExtractionInterpolationOption: Integer; safecall;
    procedure Set_HighExtractionInterpolationOption(Value: Integer); safecall;
    function Get_UndergroundStdDev: Double; safecall;
    procedure Set_UndergroundStdDev(Value: Double); safecall;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      AOutFlowRouteNo: Integer; AUpstreamCatchmentArea: Double; 
                      ABoardAndPillarArea: Double; AHighExtractionArea: Double; 
                      ASurfaceRunOffFactor: Double; ABoardAndPillarInterpolationOption: Integer; 
                      AHighExtractionInterpolationOption: Integer; AUndergroundStdDev: Double): WordBool; safecall;
    function AddBoardAndPillarGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function AddHighExtractionGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_NoOfBoardAndPillarGrowthPoints: Integer; safecall;
    function Get_NoOfHighExtractionGrowthPoints: Integer; safecall;
    function Get_BoardAndPillarGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_BoardAndPillarGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_BoardAndPillarGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_BoardAndPillarGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_HighExtractionGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_HighExtractionGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_HighExtractionGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_HighExtractionGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_UndergroundWaterRechargePortionByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_BoardAndPillarRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_HighExtractionRechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function Get_QvsSLDNoOfPoints: Integer; safecall;
    function Get_QvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property OutFlowRouteNo: Integer read Get_OutFlowRouteNo write Set_OutFlowRouteNo;
    property UpstreamCatchmentArea: Double read Get_UpstreamCatchmentArea write Set_UpstreamCatchmentArea;
    property BoardAndPillarArea: Double read Get_BoardAndPillarArea write Set_BoardAndPillarArea;
    property HighExtractionArea: Double read Get_HighExtractionArea write Set_HighExtractionArea;
    property SurfaceRunOffFactor: Double read Get_SurfaceRunOffFactor write Set_SurfaceRunOffFactor;
    property BoardAndPillarInterpolationOption: Integer read Get_BoardAndPillarInterpolationOption write Set_BoardAndPillarInterpolationOption;
    property HighExtractionInterpolationOption: Integer read Get_HighExtractionInterpolationOption write Set_HighExtractionInterpolationOption;
    property UndergroundStdDev: Double read Get_UndergroundStdDev write Set_UndergroundStdDev;
    property NoOfBoardAndPillarGrowthPoints: Integer read Get_NoOfBoardAndPillarGrowthPoints;
    property NoOfHighExtractionGrowthPoints: Integer read Get_NoOfHighExtractionGrowthPoints;
    property BoardAndPillarGrowthYearByIndex[AIndex: Integer]: Integer read Get_BoardAndPillarGrowthYearByIndex write Set_BoardAndPillarGrowthYearByIndex;
    property BoardAndPillarGrowthFactorByIndex[AIndex: Integer]: Double read Get_BoardAndPillarGrowthFactorByIndex write Set_BoardAndPillarGrowthFactorByIndex;
    property HighExtractionGrowthYearByIndex[AIndex: Integer]: Integer read Get_HighExtractionGrowthYearByIndex write Set_HighExtractionGrowthYearByIndex;
    property HighExtractionGrowthFactorByIndex[AIndex: Integer]: Double read Get_HighExtractionGrowthFactorByIndex write Set_HighExtractionGrowthFactorByIndex;
    property UndergroundWaterRechargePortionByMonth[AMonthIndex: Integer]: Double read Get_UndergroundWaterRechargePortionByMonth write Set_UndergroundWaterRechargePortionByMonth;
    property BoardAndPillarRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_BoardAndPillarRechargeFactorByMonth write Set_BoardAndPillarRechargeFactorByMonth;
    property HighExtractionRechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_HighExtractionRechargeFactorByMonth write Set_HighExtractionRechargeFactorByMonth;
    property QvsSLDNoOfPoints: Integer read Get_QvsSLDNoOfPoints;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double read Get_QvsSLDLoadByIndex write Set_QvsSLDLoadByIndex;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_QvsSLDFlowRefByIndex write Set_QvsSLDFlowRefByIndex;
    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
  end;

// *********************************************************************//
// DispIntf:  IUndergroundSectionDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {BEA1B616-53D1-4F24-9040-51693A8EF2E2}
// *********************************************************************//
  IUndergroundSectionDisp = dispinterface
    ['{BEA1B616-53D1-4F24-9040-51693A8EF2E2}']
    property SectionNo: Integer dispid 101;
    property SectionName: WideString dispid 102;
    property OutFlowRouteNo: Integer dispid 103;
    property UpstreamCatchmentArea: Double dispid 104;
    property BoardAndPillarArea: Double dispid 105;
    property HighExtractionArea: Double dispid 106;
    property SurfaceRunOffFactor: Double dispid 107;
    property BoardAndPillarInterpolationOption: Integer dispid 108;
    property HighExtractionInterpolationOption: Integer dispid 109;
    property UndergroundStdDev: Double dispid 110;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      AOutFlowRouteNo: Integer; AUpstreamCatchmentArea: Double; 
                      ABoardAndPillarArea: Double; AHighExtractionArea: Double; 
                      ASurfaceRunOffFactor: Double; ABoardAndPillarInterpolationOption: Integer; 
                      AHighExtractionInterpolationOption: Integer; AUndergroundStdDev: Double): WordBool; dispid 111;
    function AddBoardAndPillarGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 112;
    function AddHighExtractionGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 113;
    property NoOfBoardAndPillarGrowthPoints: Integer readonly dispid 114;
    property NoOfHighExtractionGrowthPoints: Integer readonly dispid 115;
    property BoardAndPillarGrowthYearByIndex[AIndex: Integer]: Integer dispid 116;
    property BoardAndPillarGrowthFactorByIndex[AIndex: Integer]: Double dispid 117;
    property HighExtractionGrowthYearByIndex[AIndex: Integer]: Integer dispid 118;
    property HighExtractionGrowthFactorByIndex[AIndex: Integer]: Double dispid 119;
    property UndergroundWaterRechargePortionByMonth[AMonthIndex: Integer]: Double dispid 120;
    property BoardAndPillarRechargeFactorByMonth[AMonthIndex: Integer]: Double dispid 121;
    property HighExtractionRechargeFactorByMonth[AMonthIndex: Integer]: Double dispid 122;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; dispid 123;
    property QvsSLDNoOfPoints: Integer readonly dispid 124;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double dispid 125;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double dispid 126;
    property ModuleID: Integer dispid 127;
  end;

// *********************************************************************//
// Interface: ISlurryDump
// Flags:     (320) Dual OleAutomation
// GUID:      {9B59804B-CA54-4926-A610-12DAC993AA07}
// *********************************************************************//
  ISlurryDump = interface(IUnknown)
    ['{9B59804B-CA54-4926-A610-12DAC993AA07}']
    function Get_SectionNo: Integer; safecall;
    procedure Set_SectionNo(Value: Integer); safecall;
    function Get_SectionName: WideString; safecall;
    procedure Set_SectionName(const Value: WideString); safecall;
    function Get_Area: Double; safecall;
    procedure Set_Area(Value: Double); safecall;
    function Get_RunOffFactor: Double; safecall;
    procedure Set_RunOffFactor(Value: Double); safecall;
    function Get_SeepProportion: Double; safecall;
    procedure Set_SeepProportion(Value: Double); safecall;
    function Get_PCDFullSupplyVolume: Double; safecall;
    procedure Set_PCDFullSupplyVolume(Value: Double); safecall;
    function Get_PCDFullSupplyArea: Double; safecall;
    procedure Set_PCDFullSupplyArea(Value: Double); safecall;
    function Get_PCDInitialVolume: Double; safecall;
    procedure Set_PCDInitialVolume(Value: Double); safecall;
    function Get_InterpolationOption: Integer; safecall;
    procedure Set_InterpolationOption(Value: Integer); safecall;
    function Get_StdDev: Double; safecall;
    procedure Set_StdDev(Value: Double); safecall;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      AArea: Double; ARunOffFactor: Double; ASeepProportion: Double; 
                      APCDFullSupplyVolume: Double; APCDFullSupplyArea: Double; 
                      APCDInitialVolume: Double; AInterpolatrionOption: Integer; AStdDev: Double): WordBool; safecall;
    function AddAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; safecall;
    function Get_NoOfAreaGrowthPoints: Integer; safecall;
    function Get_AreaGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AreaGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AreaGrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AreaGrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_RechargeFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_RechargeFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDFlowRefByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDFlowRefByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_QvsSLDLoadByIndex(AIndex: Integer): Double; safecall;
    procedure Set_QvsSLDLoadByIndex(AIndex: Integer; Value: Double); safecall;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; safecall;
    function Get_QvsSLDNoOfPoints: Integer; safecall;
    function Get_ModuleID: Integer; safecall;
    procedure Set_ModuleID(Value: Integer); safecall;
    property SectionNo: Integer read Get_SectionNo write Set_SectionNo;
    property SectionName: WideString read Get_SectionName write Set_SectionName;
    property Area: Double read Get_Area write Set_Area;
    property RunOffFactor: Double read Get_RunOffFactor write Set_RunOffFactor;
    property SeepProportion: Double read Get_SeepProportion write Set_SeepProportion;
    property PCDFullSupplyVolume: Double read Get_PCDFullSupplyVolume write Set_PCDFullSupplyVolume;
    property PCDFullSupplyArea: Double read Get_PCDFullSupplyArea write Set_PCDFullSupplyArea;
    property PCDInitialVolume: Double read Get_PCDInitialVolume write Set_PCDInitialVolume;
    property InterpolationOption: Integer read Get_InterpolationOption write Set_InterpolationOption;
    property StdDev: Double read Get_StdDev write Set_StdDev;
    property NoOfAreaGrowthPoints: Integer read Get_NoOfAreaGrowthPoints;
    property AreaGrowthYearByIndex[AIndex: Integer]: Integer read Get_AreaGrowthYearByIndex write Set_AreaGrowthYearByIndex;
    property AreaGrowthFactorByIndex[AIndex: Integer]: Double read Get_AreaGrowthFactorByIndex write Set_AreaGrowthFactorByIndex;
    property RechargeFactorByMonth[AMonthIndex: Integer]: Double read Get_RechargeFactorByMonth write Set_RechargeFactorByMonth;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double read Get_QvsSLDFlowRefByIndex write Set_QvsSLDFlowRefByIndex;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double read Get_QvsSLDLoadByIndex write Set_QvsSLDLoadByIndex;
    property QvsSLDNoOfPoints: Integer read Get_QvsSLDNoOfPoints;
    property ModuleID: Integer read Get_ModuleID write Set_ModuleID;
  end;

// *********************************************************************//
// DispIntf:  ISlurryDumpDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9B59804B-CA54-4926-A610-12DAC993AA07}
// *********************************************************************//
  ISlurryDumpDisp = dispinterface
    ['{9B59804B-CA54-4926-A610-12DAC993AA07}']
    property SectionNo: Integer dispid 101;
    property SectionName: WideString dispid 102;
    property Area: Double dispid 103;
    property RunOffFactor: Double dispid 104;
    property SeepProportion: Double dispid 105;
    property PCDFullSupplyVolume: Double dispid 106;
    property PCDFullSupplyArea: Double dispid 107;
    property PCDInitialVolume: Double dispid 108;
    property InterpolationOption: Integer dispid 109;
    property StdDev: Double dispid 110;
    function Populate(AModuleID: Integer; ASectionNo: Integer; const ASectionName: WideString; 
                      AArea: Double; ARunOffFactor: Double; ASeepProportion: Double; 
                      APCDFullSupplyVolume: Double; APCDFullSupplyArea: Double; 
                      APCDInitialVolume: Double; AInterpolatrionOption: Integer; AStdDev: Double): WordBool; dispid 111;
    function AddAreaGrowthData(AYear: Integer; AGrowthFactor: Double): WordBool; dispid 112;
    property NoOfAreaGrowthPoints: Integer readonly dispid 113;
    property AreaGrowthYearByIndex[AIndex: Integer]: Integer dispid 114;
    property AreaGrowthFactorByIndex[AIndex: Integer]: Double dispid 115;
    property RechargeFactorByMonth[AMonthIndex: Integer]: Double dispid 116;
    property QvsSLDFlowRefByIndex[AIndex: Integer]: Double dispid 117;
    property QvsSLDLoadByIndex[AIndex: Integer]: Double dispid 118;
    function AddLoadFlowRefPair(ALoad: Double; AFlowRef: Double): WordBool; dispid 119;
    property QvsSLDNoOfPoints: Integer readonly dispid 120;
    property ModuleID: Integer dispid 121;
  end;

// *********************************************************************//
// Interface: IIrrigationModule
// Flags:     (320) Dual OleAutomation
// GUID:      {41F2BF67-70E7-4263-9085-65AA62A78C24}
// *********************************************************************//
  IIrrigationModule = interface(INetworkModule)
    ['{41F2BF67-70E7-4263-9085-65AA62A78C24}']
    function Get_VersionNo: Integer; safecall;
    procedure Set_VersionNo(Value: Integer); safecall;
    function Get_ModelType: Integer; safecall;
    procedure Set_ModelType(Value: Integer); safecall;
    function Get_LastUsedModelType: Integer; safecall;
    procedure Set_LastUsedModelType(Value: Integer); safecall;
    function Get_MAP: Double; safecall;
    procedure Set_MAP(Value: Double); safecall;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function Get_MaxAnnualIrrigationAllocation: Double; safecall;
    procedure Set_MaxAnnualIrrigationAllocation(Value: Double); safecall;
    function Get_AbstractionRouteNo: Integer; safecall;
    procedure Set_AbstractionRouteNo(Value: Integer); safecall;
    function Get_ReturnFlowRouteNo: Integer; safecall;
    procedure Set_ReturnFlowRouteNo(Value: Integer); safecall;
    function Get_ReturnFlowPercentage: Double; safecall;
    procedure Set_ReturnFlowPercentage(Value: Double); safecall;
    function Get_AreaInterpolationType: Integer; safecall;
    procedure Set_AreaInterpolationType(Value: Integer); safecall;
    function Get_MaxWaterAllocation: Double; safecall;
    procedure Set_MaxWaterAllocation(Value: Double); safecall;
    function Get_WaterAllocationNoOfPoints: Integer; safecall;
    procedure Set_WaterAllocationNoOfPoints(Value: Integer); safecall;
    function Get_WaterAllocationInterpolationType: Integer; safecall;
    procedure Set_WaterAllocationInterpolationType(Value: Integer); safecall;
    function Get_RunOffModuleNo: Integer; safecall;
    procedure Set_RunOffModuleNo(Value: Integer); safecall;
    function Get_TransferCanalSeepage: Double; safecall;
    procedure Set_TransferCanalSeepage(Value: Double); safecall;
    function Get_ProduceNetReturnFlows: Integer; safecall;
    procedure Set_ProduceNetReturnFlows(Value: Integer); safecall;
    function Get_TransferCanalFlowLossProportion: Double; safecall;
    procedure Set_TransferCanalFlowLossProportion(Value: Double); safecall;
    function Get_TransferCanalSaltLossProportion: Double; safecall;
    procedure Set_TransferCanalSaltLossProportion(Value: Double); safecall;
    function Get_IrrigationEfficiencyFactor: Double; safecall;
    procedure Set_IrrigationEfficiencyFactor(Value: Double); safecall;
    function Get_ReturnFlowFactor: Double; safecall;
    procedure Set_ReturnFlowFactor(Value: Double); safecall;
    function Get_LowerZoneReturnFlowProportion: Double; safecall;
    procedure Set_LowerZoneReturnFlowProportion(Value: Double); safecall;
    function Get_UpperZoneReturnFlowProportion: Double; safecall;
    procedure Set_UpperZoneReturnFlowProportion(Value: Double); safecall;
    function Get_SaltConcentrationFactor: Double; safecall;
    procedure Set_SaltConcentrationFactor(Value: Double); safecall;
    function Get_SaltLossProportion: Double; safecall;
    procedure Set_SaltLossProportion(Value: Double); safecall;
    function Get_SaltLoad1: Double; safecall;
    procedure Set_SaltLoad1(Value: Double); safecall;
    function Get_SaltLoad2: Double; safecall;
    procedure Set_SaltLoad2(Value: Double); safecall;
    function Get_InitialSaltLoadUpperZone: Double; safecall;
    procedure Set_InitialSaltLoadUpperZone(Value: Double); safecall;
    function Get_InitialSaltLoadLowerZone: Double; safecall;
    procedure Set_InitialSaltLoadLowerZone(Value: Double); safecall;
    function Get_SoilMoistureCapacityUpperZone: Double; safecall;
    procedure Set_SoilMoistureCapacityUpperZone(Value: Double); safecall;
    function Get_SoilMoistureCapacityLowerZone: Double; safecall;
    procedure Set_SoilMoistureCapacityLowerZone(Value: Double); safecall;
    function Get_TargetSoilMoistureStorage: Double; safecall;
    procedure Set_TargetSoilMoistureStorage(Value: Double); safecall;
    function Get_InitialSoilMoistureStorage: Double; safecall;
    procedure Set_InitialSoilMoistureStorage(Value: Double); safecall;
    function Get_EffectiveRainfallFactor1: Double; safecall;
    procedure Set_EffectiveRainfallFactor1(Value: Double); safecall;
    function Get_EffectiveRainfallFactor2: Double; safecall;
    procedure Set_EffectiveRainfallFactor2(Value: Double); safecall;
    function Get_GrowthInterpolationType: Integer; safecall;
    procedure Set_GrowthInterpolationType(Value: Integer); safecall;
    function Get_ReturnFlowInterpolationType: Integer; safecall;
    procedure Set_ReturnFlowInterpolationType(Value: Integer); safecall;
    function Get_EfficiencyInterpolationType: Integer; safecall;
    procedure Set_EfficiencyInterpolationType(Value: Integer); safecall;
    function Get_IrrigationName: WideString; safecall;
    procedure Set_IrrigationName(const Value: WideString); safecall;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      AVersionNo: Integer; const AIrrigationName: WideString; AModelType: Integer; 
                      ALastUsedModelType: Integer; AMAP: Double; 
                      const ARainfallFileName: WideString; AMaxAnnualIrrigationAllocation: Double; 
                      AAbstractionRouteNo: Integer; AReturnFlowRouteNo: Integer; 
                      AReturnFlowPercentage: Double; AAreaInterpolationType: Integer; 
                      AMaxWaterAllocation: Double; AWaterAllocationNoOfPoints: Integer; 
                      AWaterAllocationInterpolationType: Integer; ARunOffModuleNo: Integer; 
                      ATransferCanalSeepage: Double; AProduceNetReturnFlows: Integer; 
                      ATransferCanalFlowLossProportion: Double; 
                      ATransferCanalSaltLossProportion: Double; 
                      AIrrigationEfficiencyFactor: Double; AReturnFlowFactor: Double; 
                      AUpperZoneReturnFlowProportion: Double; 
                      ALowerZoneReturnFlowProportion: Double; ASaltConcentrationFactor: Double; 
                      ASaltLossProportion: Double; ASaltLoad1: Double; ASaltLoad2: Double; 
                      AInitialSaltLoadUpperZone: Double; AInitialSaltLoadLowerZone: Double; 
                      ASoilMoistureCapacityUpperZone: Double; 
                      ASoilMoistureCapacityLowerZone: Double; ATargetSoilMoistureStorage: Double; 
                      AInitialSoilMoistureStorage: Double; AEffectiveRainfallFactor1: Double; 
                      AEffectiveRainfallFactor2: Double; AGrowthInterpolationType: Integer; 
                      AReturnFlowInterpolationType: Integer; AEfficiencyInterpolationType: Integer; 
                      ALongitude: Double; ALatitude: Double): WordBool; safecall;
    function Get_PIndexFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_PIndexFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_RainfallFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_RainfallFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_CropFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_APanFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_APanFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Get_NoOfIrrigationCrops: Integer; safecall;
    function Get_IrrigationCropByCropNo(ACropNo: Integer): IIrrigationCrop; safecall;
    function Get_IrrigationCropByIndex(AIndex: Integer): IIrrigationCrop; safecall;
    function AddAreaData(AYear: Integer; AArea: Double): WordBool; safecall;
    function Get_NoOfAreaDataPoints: Integer; safecall;
    function Get_AreaYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AreaYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AreaValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AreaValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddAllocationGrowthData(AYear: Integer; AGrowth: Double): WordBool; safecall;
    function Get_NoOfAllocationGrowthPoints: Integer; safecall;
    function Get_AllocationGrowthYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_AllocationGrowthYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_AllocationGrowthValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_AllocationGrowthValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddEfficiencyData(AYear: Integer; AEfficiency: Double): WordBool; safecall;
    function Get_NoOfEfficiencyDataPoints: Integer; safecall;
    function Get_EfficiencyYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_EfficiencyYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_EfficiencyValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_EfficiencyValueByIndex(AIndex: Integer; Value: Double); safecall;
    function AddReturnFlowData(AYear: Integer; AValue: Double): WordBool; safecall;
    function Get_NoOfReturnFlowDataPoints: Integer; safecall;
    function Get_ReturnFlowYearByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_ReturnFlowYearByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_ReturnFlowValueByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ReturnFlowValueByIndex(AIndex: Integer; Value: Double); safecall;
    property VersionNo: Integer read Get_VersionNo write Set_VersionNo;
    property ModelType: Integer read Get_ModelType write Set_ModelType;
    property LastUsedModelType: Integer read Get_LastUsedModelType write Set_LastUsedModelType;
    property MAP: Double read Get_MAP write Set_MAP;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property MaxAnnualIrrigationAllocation: Double read Get_MaxAnnualIrrigationAllocation write Set_MaxAnnualIrrigationAllocation;
    property AbstractionRouteNo: Integer read Get_AbstractionRouteNo write Set_AbstractionRouteNo;
    property ReturnFlowRouteNo: Integer read Get_ReturnFlowRouteNo write Set_ReturnFlowRouteNo;
    property ReturnFlowPercentage: Double read Get_ReturnFlowPercentage write Set_ReturnFlowPercentage;
    property AreaInterpolationType: Integer read Get_AreaInterpolationType write Set_AreaInterpolationType;
    property MaxWaterAllocation: Double read Get_MaxWaterAllocation write Set_MaxWaterAllocation;
    property WaterAllocationNoOfPoints: Integer read Get_WaterAllocationNoOfPoints write Set_WaterAllocationNoOfPoints;
    property WaterAllocationInterpolationType: Integer read Get_WaterAllocationInterpolationType write Set_WaterAllocationInterpolationType;
    property RunOffModuleNo: Integer read Get_RunOffModuleNo write Set_RunOffModuleNo;
    property TransferCanalSeepage: Double read Get_TransferCanalSeepage write Set_TransferCanalSeepage;
    property ProduceNetReturnFlows: Integer read Get_ProduceNetReturnFlows write Set_ProduceNetReturnFlows;
    property TransferCanalFlowLossProportion: Double read Get_TransferCanalFlowLossProportion write Set_TransferCanalFlowLossProportion;
    property TransferCanalSaltLossProportion: Double read Get_TransferCanalSaltLossProportion write Set_TransferCanalSaltLossProportion;
    property IrrigationEfficiencyFactor: Double read Get_IrrigationEfficiencyFactor write Set_IrrigationEfficiencyFactor;
    property ReturnFlowFactor: Double read Get_ReturnFlowFactor write Set_ReturnFlowFactor;
    property LowerZoneReturnFlowProportion: Double read Get_LowerZoneReturnFlowProportion write Set_LowerZoneReturnFlowProportion;
    property UpperZoneReturnFlowProportion: Double read Get_UpperZoneReturnFlowProportion write Set_UpperZoneReturnFlowProportion;
    property SaltConcentrationFactor: Double read Get_SaltConcentrationFactor write Set_SaltConcentrationFactor;
    property SaltLossProportion: Double read Get_SaltLossProportion write Set_SaltLossProportion;
    property SaltLoad1: Double read Get_SaltLoad1 write Set_SaltLoad1;
    property SaltLoad2: Double read Get_SaltLoad2 write Set_SaltLoad2;
    property InitialSaltLoadUpperZone: Double read Get_InitialSaltLoadUpperZone write Set_InitialSaltLoadUpperZone;
    property InitialSaltLoadLowerZone: Double read Get_InitialSaltLoadLowerZone write Set_InitialSaltLoadLowerZone;
    property SoilMoistureCapacityUpperZone: Double read Get_SoilMoistureCapacityUpperZone write Set_SoilMoistureCapacityUpperZone;
    property SoilMoistureCapacityLowerZone: Double read Get_SoilMoistureCapacityLowerZone write Set_SoilMoistureCapacityLowerZone;
    property TargetSoilMoistureStorage: Double read Get_TargetSoilMoistureStorage write Set_TargetSoilMoistureStorage;
    property InitialSoilMoistureStorage: Double read Get_InitialSoilMoistureStorage write Set_InitialSoilMoistureStorage;
    property EffectiveRainfallFactor1: Double read Get_EffectiveRainfallFactor1 write Set_EffectiveRainfallFactor1;
    property EffectiveRainfallFactor2: Double read Get_EffectiveRainfallFactor2 write Set_EffectiveRainfallFactor2;
    property GrowthInterpolationType: Integer read Get_GrowthInterpolationType write Set_GrowthInterpolationType;
    property ReturnFlowInterpolationType: Integer read Get_ReturnFlowInterpolationType write Set_ReturnFlowInterpolationType;
    property EfficiencyInterpolationType: Integer read Get_EfficiencyInterpolationType write Set_EfficiencyInterpolationType;
    property IrrigationName: WideString read Get_IrrigationName write Set_IrrigationName;
    property PIndexFactorByMonth[AMonthIndex: Integer]: Double read Get_PIndexFactorByMonth write Set_PIndexFactorByMonth;
    property RainfallFactorByMonth[AMonthIndex: Integer]: Double read Get_RainfallFactorByMonth write Set_RainfallFactorByMonth;
    property CropFactorByMonth[AMonthIndex: Integer]: Double read Get_CropFactorByMonth write Set_CropFactorByMonth;
    property APanFactorByMonth[AMonthIndex: Integer]: Double read Get_APanFactorByMonth write Set_APanFactorByMonth;
    property NoOfIrrigationCrops: Integer read Get_NoOfIrrigationCrops;
    property IrrigationCropByCropNo[ACropNo: Integer]: IIrrigationCrop read Get_IrrigationCropByCropNo;
    property IrrigationCropByIndex[AIndex: Integer]: IIrrigationCrop read Get_IrrigationCropByIndex;
    property NoOfAreaDataPoints: Integer read Get_NoOfAreaDataPoints;
    property AreaYearByIndex[AIndex: Integer]: Integer read Get_AreaYearByIndex write Set_AreaYearByIndex;
    property AreaValueByIndex[AIndex: Integer]: Double read Get_AreaValueByIndex write Set_AreaValueByIndex;
    property NoOfAllocationGrowthPoints: Integer read Get_NoOfAllocationGrowthPoints;
    property AllocationGrowthYearByIndex[AIndex: Integer]: Integer read Get_AllocationGrowthYearByIndex write Set_AllocationGrowthYearByIndex;
    property AllocationGrowthValueByIndex[AIndex: Integer]: Double read Get_AllocationGrowthValueByIndex write Set_AllocationGrowthValueByIndex;
    property NoOfEfficiencyDataPoints: Integer read Get_NoOfEfficiencyDataPoints;
    property EfficiencyYearByIndex[AIndex: Integer]: Integer read Get_EfficiencyYearByIndex write Set_EfficiencyYearByIndex;
    property EfficiencyValueByIndex[AIndex: Integer]: Double read Get_EfficiencyValueByIndex write Set_EfficiencyValueByIndex;
    property NoOfReturnFlowDataPoints: Integer read Get_NoOfReturnFlowDataPoints;
    property ReturnFlowYearByIndex[AIndex: Integer]: Integer read Get_ReturnFlowYearByIndex write Set_ReturnFlowYearByIndex;
    property ReturnFlowValueByIndex[AIndex: Integer]: Double read Get_ReturnFlowValueByIndex write Set_ReturnFlowValueByIndex;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationModuleDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {41F2BF67-70E7-4263-9085-65AA62A78C24}
// *********************************************************************//
  IIrrigationModuleDisp = dispinterface
    ['{41F2BF67-70E7-4263-9085-65AA62A78C24}']
    property VersionNo: Integer dispid 301;
    property ModelType: Integer dispid 302;
    property LastUsedModelType: Integer dispid 303;
    property MAP: Double dispid 304;
    property RainfallFileName: WideString dispid 305;
    property MaxAnnualIrrigationAllocation: Double dispid 306;
    property AbstractionRouteNo: Integer dispid 307;
    property ReturnFlowRouteNo: Integer dispid 308;
    property ReturnFlowPercentage: Double dispid 309;
    property AreaInterpolationType: Integer dispid 310;
    property MaxWaterAllocation: Double dispid 311;
    property WaterAllocationNoOfPoints: Integer dispid 312;
    property WaterAllocationInterpolationType: Integer dispid 313;
    property RunOffModuleNo: Integer dispid 314;
    property TransferCanalSeepage: Double dispid 315;
    property ProduceNetReturnFlows: Integer dispid 316;
    property TransferCanalFlowLossProportion: Double dispid 317;
    property TransferCanalSaltLossProportion: Double dispid 318;
    property IrrigationEfficiencyFactor: Double dispid 319;
    property ReturnFlowFactor: Double dispid 320;
    property LowerZoneReturnFlowProportion: Double dispid 322;
    property UpperZoneReturnFlowProportion: Double dispid 321;
    property SaltConcentrationFactor: Double dispid 323;
    property SaltLossProportion: Double dispid 324;
    property SaltLoad1: Double dispid 325;
    property SaltLoad2: Double dispid 326;
    property InitialSaltLoadUpperZone: Double dispid 327;
    property InitialSaltLoadLowerZone: Double dispid 328;
    property SoilMoistureCapacityUpperZone: Double dispid 329;
    property SoilMoistureCapacityLowerZone: Double dispid 330;
    property TargetSoilMoistureStorage: Double dispid 331;
    property InitialSoilMoistureStorage: Double dispid 332;
    property EffectiveRainfallFactor1: Double dispid 333;
    property EffectiveRainfallFactor2: Double dispid 334;
    property GrowthInterpolationType: Integer dispid 335;
    property ReturnFlowInterpolationType: Integer dispid 336;
    property EfficiencyInterpolationType: Integer dispid 337;
    property IrrigationName: WideString dispid 338;
    function Populate(ANetworkID: Integer; AModuleID: Integer; const AModuleType: WideString; 
                      AModuleNumber: Integer; ANetworkSequence: Integer; const AActive: WideString; 
                      AVersionNo: Integer; const AIrrigationName: WideString; AModelType: Integer; 
                      ALastUsedModelType: Integer; AMAP: Double; 
                      const ARainfallFileName: WideString; AMaxAnnualIrrigationAllocation: Double; 
                      AAbstractionRouteNo: Integer; AReturnFlowRouteNo: Integer; 
                      AReturnFlowPercentage: Double; AAreaInterpolationType: Integer; 
                      AMaxWaterAllocation: Double; AWaterAllocationNoOfPoints: Integer; 
                      AWaterAllocationInterpolationType: Integer; ARunOffModuleNo: Integer; 
                      ATransferCanalSeepage: Double; AProduceNetReturnFlows: Integer; 
                      ATransferCanalFlowLossProportion: Double; 
                      ATransferCanalSaltLossProportion: Double; 
                      AIrrigationEfficiencyFactor: Double; AReturnFlowFactor: Double; 
                      AUpperZoneReturnFlowProportion: Double; 
                      ALowerZoneReturnFlowProportion: Double; ASaltConcentrationFactor: Double; 
                      ASaltLossProportion: Double; ASaltLoad1: Double; ASaltLoad2: Double; 
                      AInitialSaltLoadUpperZone: Double; AInitialSaltLoadLowerZone: Double; 
                      ASoilMoistureCapacityUpperZone: Double; 
                      ASoilMoistureCapacityLowerZone: Double; ATargetSoilMoistureStorage: Double; 
                      AInitialSoilMoistureStorage: Double; AEffectiveRainfallFactor1: Double; 
                      AEffectiveRainfallFactor2: Double; AGrowthInterpolationType: Integer; 
                      AReturnFlowInterpolationType: Integer; AEfficiencyInterpolationType: Integer; 
                      ALongitude: Double; ALatitude: Double): WordBool; dispid 339;
    property PIndexFactorByMonth[AMonthIndex: Integer]: Double dispid 340;
    property RainfallFactorByMonth[AMonthIndex: Integer]: Double dispid 341;
    property CropFactorByMonth[AMonthIndex: Integer]: Double dispid 342;
    property APanFactorByMonth[AMonthIndex: Integer]: Double dispid 343;
    property NoOfIrrigationCrops: Integer readonly dispid 345;
    property IrrigationCropByCropNo[ACropNo: Integer]: IIrrigationCrop readonly dispid 346;
    property IrrigationCropByIndex[AIndex: Integer]: IIrrigationCrop readonly dispid 347;
    function AddAreaData(AYear: Integer; AArea: Double): WordBool; dispid 348;
    property NoOfAreaDataPoints: Integer readonly dispid 349;
    property AreaYearByIndex[AIndex: Integer]: Integer dispid 350;
    property AreaValueByIndex[AIndex: Integer]: Double dispid 351;
    function AddAllocationGrowthData(AYear: Integer; AGrowth: Double): WordBool; dispid 352;
    property NoOfAllocationGrowthPoints: Integer readonly dispid 353;
    property AllocationGrowthYearByIndex[AIndex: Integer]: Integer dispid 354;
    property AllocationGrowthValueByIndex[AIndex: Integer]: Double dispid 355;
    function AddEfficiencyData(AYear: Integer; AEfficiency: Double): WordBool; dispid 356;
    property NoOfEfficiencyDataPoints: Integer readonly dispid 357;
    property EfficiencyYearByIndex[AIndex: Integer]: Integer dispid 358;
    property EfficiencyValueByIndex[AIndex: Integer]: Double dispid 359;
    function AddReturnFlowData(AYear: Integer; AValue: Double): WordBool; dispid 360;
    property NoOfReturnFlowDataPoints: Integer readonly dispid 361;
    property ReturnFlowYearByIndex[AIndex: Integer]: Integer dispid 362;
    property ReturnFlowValueByIndex[AIndex: Integer]: Double dispid 363;
    property NetworkID: Integer readonly dispid 92;
    property ModuleNumber: Integer dispid 93;
    property NetworkSequence: Integer dispid 94;
    property Active: WideString dispid 95;
    property ModuleID: Integer readonly dispid 80;
    property ModuleType: WideString dispid 81;
    property PanByMonth[AMonth: Integer]: IPan readonly dispid 82;
    property PanByIndex[AIndex: Integer]: IPan readonly dispid 86;
    function AddPan(AMonth: Integer; AEvaportaion: Double; AFactor: Double): IPan; dispid 85;
    property Longitude: Double dispid 87;
    property Latitude: Double dispid 88;
    property PanCount: Integer readonly dispid 101;
  end;

// *********************************************************************//
// Interface: IIrrigationModuleAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {15CE7C42-F633-4E76-B01B-CBF4A5EDF683}
// *********************************************************************//
  IIrrigationModuleAgent = interface(IUnknown)
    ['{15CE7C42-F633-4E76-B01B-CBF4A5EDF683}']
    function Get_IrrigationModuleCount: Integer; safecall;
    function Get_IrrigationModuleByID(AModuleID: Integer): IIrrigationModule; safecall;
    function Get_IrrigationModuleByIndex(AInde: Integer): IIrrigationModule; safecall;
    function Get_IrrigationModuleByNumber(AModuleNumber: Integer): IIrrigationModule; safecall;
    function CreateNewIrrigationModule(ANetworkID: Integer): IIrrigationModule; safecall;
    function RemoveIrrigationModule(AModuleNumber: Integer): WordBool; safecall;
    property IrrigationModuleCount: Integer read Get_IrrigationModuleCount;
    property IrrigationModuleByID[AModuleID: Integer]: IIrrigationModule read Get_IrrigationModuleByID;
    property IrrigationModuleByIndex[AInde: Integer]: IIrrigationModule read Get_IrrigationModuleByIndex;
    property IrrigationModuleByNumber[AModuleNumber: Integer]: IIrrigationModule read Get_IrrigationModuleByNumber;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationModuleAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {15CE7C42-F633-4E76-B01B-CBF4A5EDF683}
// *********************************************************************//
  IIrrigationModuleAgentDisp = dispinterface
    ['{15CE7C42-F633-4E76-B01B-CBF4A5EDF683}']
    property IrrigationModuleCount: Integer readonly dispid 101;
    property IrrigationModuleByID[AModuleID: Integer]: IIrrigationModule readonly dispid 102;
    property IrrigationModuleByIndex[AInde: Integer]: IIrrigationModule readonly dispid 103;
    property IrrigationModuleByNumber[AModuleNumber: Integer]: IIrrigationModule readonly dispid 106;
    function CreateNewIrrigationModule(ANetworkID: Integer): IIrrigationModule; dispid 107;
    function RemoveIrrigationModule(AModuleNumber: Integer): WordBool; dispid 108;
  end;

// *********************************************************************//
// Interface: IIrrigationCrop
// Flags:     (320) Dual OleAutomation
// GUID:      {390CF9D3-7291-4C73-B119-48D9D6F8D21D}
// *********************************************************************//
  IIrrigationCrop = interface(IUnknown)
    ['{390CF9D3-7291-4C73-B119-48D9D6F8D21D}']
    function Get_CropNo: Integer; safecall;
    procedure Set_CropNo(Value: Integer); safecall;
    function Get_CropName: WideString; safecall;
    procedure Set_CropName(const Value: WideString); safecall;
    function Get_CropPercentage: Double; safecall;
    procedure Set_CropPercentage(Value: Double); safecall;
    function Get_CropFactorByMonth(AMonthIndex: Integer): Double; safecall;
    procedure Set_CropFactorByMonth(AMonthIndex: Integer; Value: Double); safecall;
    function Populate(ACropNo: Integer; const ACropName: WideString; ACropPercentage: Double; 
                      const AMonthlyCropFactors: WideString): WordBool; safecall;
    property CropNo: Integer read Get_CropNo write Set_CropNo;
    property CropName: WideString read Get_CropName write Set_CropName;
    property CropPercentage: Double read Get_CropPercentage write Set_CropPercentage;
    property CropFactorByMonth[AMonthIndex: Integer]: Double read Get_CropFactorByMonth write Set_CropFactorByMonth;
  end;

// *********************************************************************//
// DispIntf:  IIrrigationCropDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {390CF9D3-7291-4C73-B119-48D9D6F8D21D}
// *********************************************************************//
  IIrrigationCropDisp = dispinterface
    ['{390CF9D3-7291-4C73-B119-48D9D6F8D21D}']
    property CropNo: Integer dispid 101;
    property CropName: WideString dispid 102;
    property CropPercentage: Double dispid 103;
    property CropFactorByMonth[AMonthIndex: Integer]: Double dispid 104;
    function Populate(ACropNo: Integer; const ACropName: WideString; ACropPercentage: Double; 
                      const AMonthlyCropFactors: WideString): WordBool; dispid 105;
  end;

// *********************************************************************//
// Interface: IHydroNVDrawing
// Flags:     (320) Dual OleAutomation
// GUID:      {99C287E1-02A5-40E8-9884-F256C553AD2A}
// *********************************************************************//
  IHydroNVDrawing = interface(IUnknown)
    ['{99C287E1-02A5-40E8-9884-F256C553AD2A}']
    function Get_NetworkID: Integer; safecall;
    function Get_DrawingID: Integer; safecall;
    function Get_DrawingName: WideString; safecall;
    procedure Set_DrawingName(const Value: WideString); safecall;
    function Get_GISDrawing: Integer; safecall;
    procedure Set_GISDrawing(Value: Integer); safecall;
    function Populate(ANetworkID: Integer; ADrawingID: Integer; const ADrawingName: WideString; 
                      AGISDrawing: Integer; AReadOnly: Integer): WordBool; safecall;
    function Get_ReadOnly: Integer; safecall;
    procedure Set_ReadOnly(Value: Integer); safecall;
    property NetworkID: Integer read Get_NetworkID;
    property DrawingID: Integer read Get_DrawingID;
    property DrawingName: WideString read Get_DrawingName write Set_DrawingName;
    property GISDrawing: Integer read Get_GISDrawing write Set_GISDrawing;
    property ReadOnly: Integer read Get_ReadOnly write Set_ReadOnly;
  end;

// *********************************************************************//
// DispIntf:  IHydroNVDrawingDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {99C287E1-02A5-40E8-9884-F256C553AD2A}
// *********************************************************************//
  IHydroNVDrawingDisp = dispinterface
    ['{99C287E1-02A5-40E8-9884-F256C553AD2A}']
    property NetworkID: Integer readonly dispid 201;
    property DrawingID: Integer readonly dispid 202;
    property DrawingName: WideString dispid 203;
    property GISDrawing: Integer dispid 204;
    function Populate(ANetworkID: Integer; ADrawingID: Integer; const ADrawingName: WideString; 
                      AGISDrawing: Integer; AReadOnly: Integer): WordBool; dispid 205;
    property ReadOnly: Integer dispid 101;
  end;

// *********************************************************************//
// Interface: IHydroNVDrawingAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {70781048-F72F-4312-BFB0-2C3535D90BAE}
// *********************************************************************//
  IHydroNVDrawingAgent = interface(IUnknown)
    ['{70781048-F72F-4312-BFB0-2C3535D90BAE}']
    function Get_HydroNVDrawingCount: Integer; safecall;
    function Get_HydroNVDrawingByID(ADrawingID: Integer): IHydroNVDrawing; safecall;
    function Get_HydroNVDrawingByIndex(AIndex: Integer): IHydroNVDrawing; safecall;
    function Get_HydroNVDrawingByName(const AName: WideString): IHydroNVDrawing; safecall;
    function Get_DrawingExists(const ADrawingName: WideString): WordBool; safecall;
    function CreateNewDrawing(ANetworkID: Integer; const ADrawingFileName: WideString; 
                              AGISDrawing: Integer; const ACopyFrom: WideString): IHydroNVDrawing; safecall;
    function DeleteHydroNVDrawing(ADrawingID: Integer; const ADrawingFileName: WideString): WordBool; safecall;
    property HydroNVDrawingCount: Integer read Get_HydroNVDrawingCount;
    property HydroNVDrawingByID[ADrawingID: Integer]: IHydroNVDrawing read Get_HydroNVDrawingByID;
    property HydroNVDrawingByIndex[AIndex: Integer]: IHydroNVDrawing read Get_HydroNVDrawingByIndex;
    property HydroNVDrawingByName[const AName: WideString]: IHydroNVDrawing read Get_HydroNVDrawingByName;
    property DrawingExists[const ADrawingName: WideString]: WordBool read Get_DrawingExists;
  end;

// *********************************************************************//
// DispIntf:  IHydroNVDrawingAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {70781048-F72F-4312-BFB0-2C3535D90BAE}
// *********************************************************************//
  IHydroNVDrawingAgentDisp = dispinterface
    ['{70781048-F72F-4312-BFB0-2C3535D90BAE}']
    property HydroNVDrawingCount: Integer readonly dispid 101;
    property HydroNVDrawingByID[ADrawingID: Integer]: IHydroNVDrawing readonly dispid 102;
    property HydroNVDrawingByIndex[AIndex: Integer]: IHydroNVDrawing readonly dispid 103;
    property HydroNVDrawingByName[const AName: WideString]: IHydroNVDrawing readonly dispid 106;
    property DrawingExists[const ADrawingName: WideString]: WordBool readonly dispid 107;
    function CreateNewDrawing(ANetworkID: Integer; const ADrawingFileName: WideString; 
                              AGISDrawing: Integer; const ACopyFrom: WideString): IHydroNVDrawing; dispid 108;
    function DeleteHydroNVDrawing(ADrawingID: Integer; const ADrawingFileName: WideString): WordBool; dispid 109;
  end;

// *********************************************************************//
// Interface: IHydrologyModel
// Flags:     (320) Dual OleAutomation
// GUID:      {D40CF196-D458-48F7-A13F-794A8C5D482E}
// *********************************************************************//
  IHydrologyModel = interface(IUnknown)
    ['{D40CF196-D458-48F7-A13F-794A8C5D482E}']
    function Get_Network: INetwork; safecall;
    function ShowNetworkRouteDialog(ARouteNo: Integer): WordBool; safecall;
    function ShowReservoirModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowRunOffModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowChannelModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowIrrigationModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function Get_MayChangeNetwork: WordBool; safecall;
    procedure Set_MayChangeNetwork(Value: WordBool); safecall;
    function ShowMineModuleDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowMineOpencastPitDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; safecall;
    function ShowMineUndergroundSectionDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; safecall;
    function ShowMineSlurryDumpDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; safecall;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown; 
                            AVisioEventCode: Integer; const ASourceObj: IUnknown; 
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown; 
                            AMoreInfo: OleVariant): WordBool; safecall;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; safecall;
    function UpdateNetworkData(const AXMLOut: WideString): WordBool; safecall;
    function ShowMineSectionsDialog(AModuleNo: Integer): WordBool; safecall;
    function ShowNetworkSequenceDialog(AModuleID: Integer): WordBool; safecall;
    function ShowObservationPointDialog(ARouteNo: Integer): WordBool; safecall;
    function CreateNewNetwork(const ANetworkCode: WideString; AVersionNo: Integer; 
                              const AInputDir: WideString; const AOutputDir: WideString; 
                              const ADebugRequired: WideString; ADebugStartPeriod: Integer; 
                              ADebudEndPeriod: Integer; const ASummaryRequired: WideString; 
                              ASimulationStartYear: Integer; ASimulationEndYear: Integer; 
                              AReadOnly: Integer; var ANetworkID: Integer; var AErrorMsg: WideString): WordBool; safecall;
    function ShowHydroOutputDialog(const AElementType: WideString; 
                                   const AElementSubType: WideString; AElementNo: Integer; 
                                   ASubElementID: Integer; AResultTypeID: Integer): WordBool; safecall;
    function RefreshOutputDlg(const AXMLOut: WideString): WordBool; safecall;
    procedure FreeOutputDlg; safecall;
    function DeleteNetwork(ANetworkID: Integer; var AErrorMsg: WideString): WordBool; safecall;
    function CopyNetwork(ANetworkID: Integer; const ANewNetworkCode: WideString; 
                         var AErrorMsg: WideString): WordBool; safecall;
    function ExportNetwork(ANetworkID: Integer; const ADirectory: WideString; 
                           var AErrorMsg: WideString): WordBool; safecall;
    function ImportNetwork(const ADirectory: WideString; ANetworkID: Integer; 
                           var AErrorMsg: WideString): WordBool; safecall;
    property Network: INetwork read Get_Network;
    property MayChangeNetwork: WordBool read Get_MayChangeNetwork write Set_MayChangeNetwork;
  end;

// *********************************************************************//
// DispIntf:  IHydrologyModelDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {D40CF196-D458-48F7-A13F-794A8C5D482E}
// *********************************************************************//
  IHydrologyModelDisp = dispinterface
    ['{D40CF196-D458-48F7-A13F-794A8C5D482E}']
    property Network: INetwork readonly dispid 101;
    function ShowNetworkRouteDialog(ARouteNo: Integer): WordBool; dispid 102;
    function ShowReservoirModuleDialog(AModuleNo: Integer): WordBool; dispid 103;
    function ShowRunOffModuleDialog(AModuleNo: Integer): WordBool; dispid 104;
    function ShowChannelModuleDialog(AModuleNo: Integer): WordBool; dispid 105;
    function ShowIrrigationModuleDialog(AModuleNo: Integer): WordBool; dispid 108;
    property MayChangeNetwork: WordBool dispid 109;
    function ShowMineModuleDialog(AModuleNo: Integer): WordBool; dispid 110;
    function ShowMineOpencastPitDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; dispid 111;
    function ShowMineUndergroundSectionDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; dispid 112;
    function ShowMineSlurryDumpDialog(AModuleNo: Integer; ASectionNo: Integer): WordBool; dispid 113;
    function HandleVNVEvent(const AVisioApp: IUnknown; const AVisioDoc: IUnknown; 
                            AVisioEventCode: Integer; const ASourceObj: IUnknown; 
                            AEventID: Integer; AEventSeqNum: Integer; const ASubjectObj: IUnknown; 
                            AMoreInfo: OleVariant): WordBool; dispid 148;
    function ProcessVNVSpecial(const AParameter: WideString): WordBool; dispid 150;
    function UpdateNetworkData(const AXMLOut: WideString): WordBool; dispid 114;
    function ShowMineSectionsDialog(AModuleNo: Integer): WordBool; dispid 106;
    function ShowNetworkSequenceDialog(AModuleID: Integer): WordBool; dispid 107;
    function ShowObservationPointDialog(ARouteNo: Integer): WordBool; dispid 115;
    function CreateNewNetwork(const ANetworkCode: WideString; AVersionNo: Integer; 
                              const AInputDir: WideString; const AOutputDir: WideString; 
                              const ADebugRequired: WideString; ADebugStartPeriod: Integer; 
                              ADebudEndPeriod: Integer; const ASummaryRequired: WideString; 
                              ASimulationStartYear: Integer; ASimulationEndYear: Integer; 
                              AReadOnly: Integer; var ANetworkID: Integer; var AErrorMsg: WideString): WordBool; dispid 116;
    function ShowHydroOutputDialog(const AElementType: WideString; 
                                   const AElementSubType: WideString; AElementNo: Integer; 
                                   ASubElementID: Integer; AResultTypeID: Integer): WordBool; dispid 117;
    function RefreshOutputDlg(const AXMLOut: WideString): WordBool; dispid 118;
    procedure FreeOutputDlg; dispid 119;
    function DeleteNetwork(ANetworkID: Integer; var AErrorMsg: WideString): WordBool; dispid 120;
    function CopyNetwork(ANetworkID: Integer; const ANewNetworkCode: WideString; 
                         var AErrorMsg: WideString): WordBool; dispid 121;
    function ExportNetwork(ANetworkID: Integer; const ADirectory: WideString; 
                           var AErrorMsg: WideString): WordBool; dispid 122;
    function ImportNetwork(const ADirectory: WideString; ANetworkID: Integer; 
                           var AErrorMsg: WideString): WordBool; dispid 123;
  end;

// *********************************************************************//
// Interface: IYearVolumeAreaData
// Flags:     (320) Dual OleAutomation
// GUID:      {7FDC9172-8066-4100-9299-4E4556BE44EC}
// *********************************************************************//
  IYearVolumeAreaData = interface(IUnknown)
    ['{7FDC9172-8066-4100-9299-4E4556BE44EC}']
    function Get_Year: Integer; safecall;
    procedure Set_Year(Value: Integer); safecall;
    function Get_Volume: Double; safecall;
    procedure Set_Volume(Value: Double); safecall;
    function Get_Area: Double; safecall;
    procedure Set_Area(Value: Double); safecall;
    property Year: Integer read Get_Year write Set_Year;
    property Volume: Double read Get_Volume write Set_Volume;
    property Area: Double read Get_Area write Set_Area;
  end;

// *********************************************************************//
// DispIntf:  IYearVolumeAreaDataDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {7FDC9172-8066-4100-9299-4E4556BE44EC}
// *********************************************************************//
  IYearVolumeAreaDataDisp = dispinterface
    ['{7FDC9172-8066-4100-9299-4E4556BE44EC}']
    property Year: Integer dispid 101;
    property Volume: Double dispid 102;
    property Area: Double dispid 103;
  end;

// *********************************************************************//
// Interface: IHydroOutputAgent
// Flags:     (320) Dual OleAutomation
// GUID:      {CCF80B02-225B-4CE1-BDA7-DD8B9DB57D09}
// *********************************************************************//
  IHydroOutputAgent = interface(IUnknown)
    ['{CCF80B02-225B-4CE1-BDA7-DD8B9DB57D09}']
    function Get_HydroOutput(const AElementType: WideString; const AElementSubType: WideString; 
                             AElementID: Integer; ASubElementID: Integer; AResultTypeID: Integer): IHydroOutput; safecall;
    function Get_ResultTypeCount: Integer; safecall;
    function Get_ResultTypeByIndex(AIndex: Integer): IHydroResultType; safecall;
    function Get_ResultTypeByID(AResultTypeID: Integer): IHydroResultType; safecall;
    property HydroOutput[const AElementType: WideString; const AElementSubType: WideString; 
                         AElementID: Integer; ASubElementID: Integer; AResultTypeID: Integer]: IHydroOutput read Get_HydroOutput;
    property ResultTypeCount: Integer read Get_ResultTypeCount;
    property ResultTypeByIndex[AIndex: Integer]: IHydroResultType read Get_ResultTypeByIndex;
    property ResultTypeByID[AResultTypeID: Integer]: IHydroResultType read Get_ResultTypeByID;
  end;

// *********************************************************************//
// DispIntf:  IHydroOutputAgentDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {CCF80B02-225B-4CE1-BDA7-DD8B9DB57D09}
// *********************************************************************//
  IHydroOutputAgentDisp = dispinterface
    ['{CCF80B02-225B-4CE1-BDA7-DD8B9DB57D09}']
    property HydroOutput[const AElementType: WideString; const AElementSubType: WideString; 
                         AElementID: Integer; ASubElementID: Integer; AResultTypeID: Integer]: IHydroOutput readonly dispid 101;
    property ResultTypeCount: Integer readonly dispid 102;
    property ResultTypeByIndex[AIndex: Integer]: IHydroResultType readonly dispid 103;
    property ResultTypeByID[AResultTypeID: Integer]: IHydroResultType readonly dispid 105;
  end;

// *********************************************************************//
// Interface: IHydroOutput
// Flags:     (320) Dual OleAutomation
// GUID:      {9504DE8B-38B1-46B5-BB07-4D010FD75245}
// *********************************************************************//
  IHydroOutput = interface(IUnknown)
    ['{9504DE8B-38B1-46B5-BB07-4D010FD75245}']
    function Get_ResultID: Integer; safecall;
    function Get_NetworkID: Integer; safecall;
    function Get_ElementType: WideString; safecall;
    procedure Set_ElementType(const Value: WideString); safecall;
    function Get_ElementID: Integer; safecall;
    procedure Set_ElementID(Value: Integer); safecall;
    function Get_SubElementID: Integer; safecall;
    procedure Set_SubElementID(Value: Integer); safecall;
    function Get_ResultTypeID: Integer; safecall;
    procedure Set_ResultTypeID(Value: Integer); safecall;
    function Get_PeriodsPerYear: Integer; safecall;
    function Get_IntervalCount: Integer; safecall;
    function Get_YearCount: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    function Get_EndYear: Integer; safecall;
    function Get_YearTotal(AYearIndex: Integer): Double; safecall;
    function Get_YearAverage(AYearIndex: Integer): Double; safecall;
    function Get_PeriodTotal(APeriodIndex: Integer): Double; safecall;
    function Get_PeriodAverage(APeriodIndex: Integer): Double; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    function Get_Data(AIntervalIndex: Integer): Double; safecall;
    function Get_DataByYearMonth(AYear: Integer; AMonth: Integer): Double; safecall;
    function Get_OverallAverage: Double; safecall;
    function Get_OverallTotal: Double; safecall;
    function Get_ElementSubType: WideString; safecall;
    procedure Set_ElementSubType(const Value: WideString); safecall;
    function Get_AllZero: WordBool; safecall;
    procedure Set_AllZero(Value: WordBool); safecall;
    property ResultID: Integer read Get_ResultID;
    property NetworkID: Integer read Get_NetworkID;
    property ElementType: WideString read Get_ElementType write Set_ElementType;
    property ElementID: Integer read Get_ElementID write Set_ElementID;
    property SubElementID: Integer read Get_SubElementID write Set_SubElementID;
    property ResultTypeID: Integer read Get_ResultTypeID write Set_ResultTypeID;
    property PeriodsPerYear: Integer read Get_PeriodsPerYear;
    property IntervalCount: Integer read Get_IntervalCount;
    property YearCount: Integer read Get_YearCount;
    property StartYear: Integer read Get_StartYear;
    property EndYear: Integer read Get_EndYear;
    property YearTotal[AYearIndex: Integer]: Double read Get_YearTotal;
    property YearAverage[AYearIndex: Integer]: Double read Get_YearAverage;
    property PeriodTotal[APeriodIndex: Integer]: Double read Get_PeriodTotal;
    property PeriodAverage[APeriodIndex: Integer]: Double read Get_PeriodAverage;
    property Year[AYearIndex: Integer]: Integer read Get_Year;
    property Data[AIntervalIndex: Integer]: Double read Get_Data;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double read Get_DataByYearMonth;
    property OverallAverage: Double read Get_OverallAverage;
    property OverallTotal: Double read Get_OverallTotal;
    property ElementSubType: WideString read Get_ElementSubType write Set_ElementSubType;
    property AllZero: WordBool read Get_AllZero write Set_AllZero;
  end;

// *********************************************************************//
// DispIntf:  IHydroOutputDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {9504DE8B-38B1-46B5-BB07-4D010FD75245}
// *********************************************************************//
  IHydroOutputDisp = dispinterface
    ['{9504DE8B-38B1-46B5-BB07-4D010FD75245}']
    property ResultID: Integer readonly dispid 101;
    property NetworkID: Integer readonly dispid 102;
    property ElementType: WideString dispid 103;
    property ElementID: Integer dispid 104;
    property SubElementID: Integer dispid 105;
    property ResultTypeID: Integer dispid 106;
    property PeriodsPerYear: Integer readonly dispid 107;
    property IntervalCount: Integer readonly dispid 108;
    property YearCount: Integer readonly dispid 109;
    property StartYear: Integer readonly dispid 110;
    property EndYear: Integer readonly dispid 111;
    property YearTotal[AYearIndex: Integer]: Double readonly dispid 112;
    property YearAverage[AYearIndex: Integer]: Double readonly dispid 113;
    property PeriodTotal[APeriodIndex: Integer]: Double readonly dispid 114;
    property PeriodAverage[APeriodIndex: Integer]: Double readonly dispid 115;
    property Year[AYearIndex: Integer]: Integer readonly dispid 116;
    property Data[AIntervalIndex: Integer]: Double readonly dispid 117;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double readonly dispid 118;
    property OverallAverage: Double readonly dispid 119;
    property OverallTotal: Double readonly dispid 120;
    property ElementSubType: WideString dispid 121;
    property AllZero: WordBool dispid 122;
  end;

// *********************************************************************//
// Interface: ITimeSeries
// Flags:     (320) Dual OleAutomation
// GUID:      {5EBFB19D-23E2-4A5D-80E1-CA2125725169}
// *********************************************************************//
  ITimeSeries = interface(IUnknown)
    ['{5EBFB19D-23E2-4A5D-80E1-CA2125725169}']
    function Get_PeriodsPerYear: Integer; safecall;
    function Get_IntervalCount: Integer; safecall;
    function Get_YearCount: Integer; safecall;
    function Get_StartYear: Integer; safecall;
    function Get_EndYear: Integer; safecall;
    function Get_YearTotal(AYearIndex: Integer): Double; safecall;
    function Get_YearAverage(AYearIndex: Integer): Double; safecall;
    function Get_PeriodTotal(APeriodIndex: Integer): Double; safecall;
    function Get_PeriodAverage(APeriodIndex: Integer): Double; safecall;
    function Get_Year(AYearIndex: Integer): Integer; safecall;
    function Get_Data(AIntervalIndex: Integer): Double; safecall;
    function Get_DataByYearMonth(AYear: Integer; AMonth: Integer): Double; safecall;
    function Get_OverallAverage: Double; safecall;
    function Get_OverallTotal: Double; safecall;
    property PeriodsPerYear: Integer read Get_PeriodsPerYear;
    property IntervalCount: Integer read Get_IntervalCount;
    property YearCount: Integer read Get_YearCount;
    property StartYear: Integer read Get_StartYear;
    property EndYear: Integer read Get_EndYear;
    property YearTotal[AYearIndex: Integer]: Double read Get_YearTotal;
    property YearAverage[AYearIndex: Integer]: Double read Get_YearAverage;
    property PeriodTotal[APeriodIndex: Integer]: Double read Get_PeriodTotal;
    property PeriodAverage[APeriodIndex: Integer]: Double read Get_PeriodAverage;
    property Year[AYearIndex: Integer]: Integer read Get_Year;
    property Data[AIntervalIndex: Integer]: Double read Get_Data;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double read Get_DataByYearMonth;
    property OverallAverage: Double read Get_OverallAverage;
    property OverallTotal: Double read Get_OverallTotal;
  end;

// *********************************************************************//
// DispIntf:  ITimeSeriesDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {5EBFB19D-23E2-4A5D-80E1-CA2125725169}
// *********************************************************************//
  ITimeSeriesDisp = dispinterface
    ['{5EBFB19D-23E2-4A5D-80E1-CA2125725169}']
    property PeriodsPerYear: Integer readonly dispid 101;
    property IntervalCount: Integer readonly dispid 102;
    property YearCount: Integer readonly dispid 103;
    property StartYear: Integer readonly dispid 104;
    property EndYear: Integer readonly dispid 105;
    property YearTotal[AYearIndex: Integer]: Double readonly dispid 106;
    property YearAverage[AYearIndex: Integer]: Double readonly dispid 107;
    property PeriodTotal[APeriodIndex: Integer]: Double readonly dispid 108;
    property PeriodAverage[APeriodIndex: Integer]: Double readonly dispid 109;
    property Year[AYearIndex: Integer]: Integer readonly dispid 110;
    property Data[AIntervalIndex: Integer]: Double readonly dispid 111;
    property DataByYearMonth[AYear: Integer; AMonth: Integer]: Double readonly dispid 112;
    property OverallAverage: Double readonly dispid 113;
    property OverallTotal: Double readonly dispid 114;
  end;

// *********************************************************************//
// Interface: IHydroResultType
// Flags:     (320) Dual OleAutomation
// GUID:      {6154CD50-12B5-4C64-896B-26E8D3836BEA}
// *********************************************************************//
  IHydroResultType = interface(IUnknown)
    ['{6154CD50-12B5-4C64-896B-26E8D3836BEA}']
    function Get_ResultTypeID: Integer; safecall;
    procedure Set_ResultTypeID(Value: Integer); safecall;
    function Get_ElementType: WideString; safecall;
    procedure Set_ElementType(const Value: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const Value: WideString); safecall;
    function Get_ElementSubType: WideString; safecall;
    procedure Set_ElementSubType(const Value: WideString); safecall;
    function Get_Units: WideString; safecall;
    procedure Set_Units(const Value: WideString); safecall;
    property ResultTypeID: Integer read Get_ResultTypeID write Set_ResultTypeID;
    property ElementType: WideString read Get_ElementType write Set_ElementType;
    property Description: WideString read Get_Description write Set_Description;
    property ElementSubType: WideString read Get_ElementSubType write Set_ElementSubType;
    property Units: WideString read Get_Units write Set_Units;
  end;

// *********************************************************************//
// DispIntf:  IHydroResultTypeDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6154CD50-12B5-4C64-896B-26E8D3836BEA}
// *********************************************************************//
  IHydroResultTypeDisp = dispinterface
    ['{6154CD50-12B5-4C64-896B-26E8D3836BEA}']
    property ResultTypeID: Integer dispid 201;
    property ElementType: WideString dispid 202;
    property Description: WideString dispid 203;
    property ElementSubType: WideString dispid 101;
    property Units: WideString dispid 102;
  end;

// *********************************************************************//
// Interface: IHydrologyComObject
// Flags:     (256) OleAutomation
// GUID:      {99658A58-442A-45A9-8EA1-80A449E0E923}
// *********************************************************************//
  IHydrologyComObject = interface(IUnknown)
    ['{99658A58-442A-45A9-8EA1-80A449E0E923}']
  end;

// *********************************************************************//
// The Class CoHydrologyComObject provides a Create and CreateRemote method to          
// create instances of the default interface IHydrologyComObject exposed by              
// the CoClass HydrologyComObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHydrologyComObject = class
    class function Create: IHydrologyComObject;
    class function CreateRemote(const MachineName: string): IHydrologyComObject;
  end;

implementation

uses ComObj;

class function CoHydrologyComObject.Create: IHydrologyComObject;
begin
  Result := CreateComObject(CLASS_HydrologyComObject) as IHydrologyComObject;
end;

class function CoHydrologyComObject.CreateRemote(const MachineName: string): IHydrologyComObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HydrologyComObject) as IHydrologyComObject;
end;

end.

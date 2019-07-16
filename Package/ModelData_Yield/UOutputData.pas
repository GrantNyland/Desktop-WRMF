//
//
//  UNIT      : Contains OutputData Classes
//  AUTHOR    : Dziedzi  Ramulondi (Arivia)
//  DATE      : 2003/07/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UOutputData;

interface

uses
  VCL.Controls,
  Contnrs,
  Classes,
  VCL.Graphics,
  VCL.Forms,
  VCL.Dialogs,
  VoaimsCom_TLB,
  UPopupDialogForm,
  USumOutDataObjects,
  UAbstractObject,
  ULongTermSupplyData,
  UYieldModelDataGUIForm,
  UPopupForm,
  UOutputDataSelectionValidator,
  UOutputDataSelectionDialog;

type
  Real8 = double;
  PReal8 = ^Real8;

  {TBlobElement = class(TObject)
  protected
    FElementNumber : integer;
    FElementName   : string;
    FPrintOutput   : Char;
    FNrOfPoints    : integer;
    FStatus        : integer;
    FElementIndex  : integer;
  public
    constructor Create;
    property ElementNumber  : integer read FElementNumber write FElementNumber;
    property ElementName    : string  read FElementName   write FElementName;
    property PrintOutput    : Char    read FPrintOutput   write FPrintOutput;
    property NrOfPoints     : integer read FNrOfPoints    write FNrOfPoints;
    property Status         : integer read FStatus        write FStatus;
    property ElementIndex   : integer read FElementIndex  write FElementIndex;
  end;}

  TOutputDataSelection = class(TAbstractAppObject, IOutputDataSelection)
  protected
    FLoadCase      : integer;
    FSequence      : integer;
    FMonth         : integer;
    FDisplayMonth  : integer;
    FDecisionMonth : integer;
    FYearsToSkip   : integer;
    FUnits         : TOutputUnits;
    FValueType     : TOutputValueType;
    FTimeStep      : TOutputTimeStep;
    FPlotOption    : TOutputPlotOptions;
    FHighlight     : boolean;
    FPopulated        : boolean;
    FAverageType      : TOutputAverageType;
    FAverageStartDate : TDateTime;
    FAverageEndDate   : TDateTime;
    FApplySensitivity : TSensitivity;
    FSensitivity      : double;
    FPercSensitivity  : double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; override;
    procedure Populate(ALoadCase,ASequence,AMonth,ADisplayMonth,ADecisionMonth : integer; AUnits : TOutputUnits;
              AValueType: TOutputValueType; ATimeStep: TOutputTimeStep; AHighlight : boolean;
              AAverageType : TOutputAverageType; AAverageStartDate,
              AAverageEndDate   : TDateTime;APlotOption:TOutputPlotOptions;AYearsToSkip : integer;
              AApplySensitivity : TSensitivity; APercSensitivity,ASensitivity : double);
    function Get_LoadCase: Integer; safecall;
    procedure Set_LoadCase(Value: Integer); safecall;
    function Get_Sequence: Integer; safecall;
    procedure Set_Sequence(Value: Integer); safecall;
    function Get_Month: Integer; safecall;
    function Get_Units: TOutputUnits; safecall;
    function Get_ValueType: TOutputValueType; safecall;
    function Get_TimeStep: TOutputTimeStep; safecall;
    procedure Set_TimeStep(Value: TOutputTimeStep); safecall;
    function Get_Highlight: WordBool; safecall;
    function Get_DisplayMonth: Integer; safecall;
    function Get_AverageType: TOutputAverageType; safecall;
    function Get_AverageStartDate: TDateTime; safecall;
    function Get_AverageEndDate: TDateTime; safecall;
    function Get_DecisionMonth: Integer; safecall;
    function Get_PlotOption: TOutputPlotOptions; safecall;
    function Get_YearsToSkip: Integer; safecall;
    function Get_ApplySensitivity: TSensitivity; safecall;
    procedure Set_ApplySensitivity(AValue: TSensitivity); safecall;
    function Get_Sensitivity: Double; safecall;
    procedure Set_Sensitivity(Value: Double); safecall;
    function Get_PercSensitivity: Double; safecall;
    procedure Set_PercSensitivity(Value: Double); safecall;

    property LoadCase      : Integer          read  Get_LoadCase  write Set_LoadCase;
    property Sequence      : integer          read  Get_Sequence write Set_Sequence;
    property Month         : integer          read  Get_Month;
    property Units         : TOutputUnits     read  Get_Units;
    property ValueType     : TOutputValueType read  Get_ValueType;
    property TimeStep      : TOutputTimeStep  read  Get_TimeStep write Set_TimeStep;
    property Highlight     : Wordbool         read  Get_Highlight;
    property DisplayMonth  : integer          read  Get_DisplayMonth;
    property Populated     : boolean          read  FPopulated;
    property AverageType      : TOutputAverageType read FAverageType;
    property AverageStartDate : TDateTime          read FAverageStartDate;
    property AverageEndDate   : TDateTime          read FAverageEndDate;
    property DecisionMonth    : Integer            read Get_DecisionMonth;
    property PlotOption       : TOutputPlotOptions read Get_PlotOption;
    property YearsToSkip      : Integer            read Get_YearsToSkip;
    property ApplySensitivity : TSensitivity       read Get_ApplySensitivity write Set_ApplySensitivity;
    property Sensitivity      : Double             read Get_Sensitivity      write Set_Sensitivity;
    property PercSensitivity: Double               read Get_PercSensitivity  write Set_PercSensitivity;

  end;


  TPlottingOutputElement = class(TAbstractAppObject)
  protected
    FElementID         : integer;
    FElementType       : TNetworkElementType;
    FElementDataType   : TOutputDataType;
    FElementName       : string;
  public
    function Initialise: boolean; override;
    procedure Populate(AElementType : TNetworkElementType;AElementDataTypeType: TOutputDataType; AElementID: integer; AElementName: string);
    property ElementID          : integer                read FElementID         write FElementID;
    property ElementType        : TNetworkElementType    read FElementType       write FElementType;
    property ElementDataType    : TOutputDataType        read FElementDataType   write FElementDataType;
    property ElementName        : string                 read FElementName       write FElementName;
  end;

  TPlottingOutputData = class(TAbstractAppObject)
  protected
    FDataSelection  : TOutputDataSelection;
    FDBItemsList    : TObjectList;
//    FFileItemsList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetElementByIndex(AIndex: integer):TPlottingOutputElement;
    function GetElementByNo(AElement: integer;var AElementInex : integer):TPlottingOutputElement;
  public
    function GetDataByElement(ADataType: TOutputDataType;AElement : integer;AData : TStrings;var AErrors : string):boolean;
    function GetBoxPlotDataByElement(ADataType: TOutputDataType;AElement : integer;AData : TStrings;var AErrors : string):boolean;
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function ElementsCount: integer;
    procedure PopulateDataSelection(ADataSelection:TOutputDataSelection);
    procedure Populate(AElementType : TNetworkElementType;AElementDataType: TOutputDataType; AElementID: integer;ElementName: string);
    function GetPlottingDataSet(ADataSet: TAbstractModelDataset;ExcludeMasterControl,ExcludeTotals: boolean;  var AErrors: string): boolean;
    property ElementByIndex[AIndex: integer]:TPlottingOutputElement read GetElementByIndex;
  end;

  TDataOutputData = class(TAbstractAppObject)
  protected
  public
  end;

  TDebuggingOutputData = class(TAbstractAppObject)
  protected
  public
  end;


  TSummaryOutputDataElement= class(TObject)
  protected
    FElementID         : integer;
    FBlockType         : TOutputDataType;
    FBlockNumber       : integer;
    FLoadCaseNumber    : integer;
    FSequenceNumber    : integer;
    FAnnualWaterDemand : double;
    FAnnualPowerDemand : double;
    FBlockHeading      : string;
    FBlockTitle        : string;
    FBlockIndex        : Int64;
    FValuesLines       : TObjectlist;// TStringList;

    procedure CopyValuesLines(AValuesLines : TObjectList);

  public
    function Initialise: boolean;

    procedure PopulateBlockIndex(ABlockType: TOutputDataType; AElementID,ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer;
              AAnnualWaterDemand,AAnnualPowerDemand: double;AHeading,ATitle : string;ABlockIndex : Int64);
    procedure Populate(ABlockType: TOutputDataType; AElementID,ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer;
              AAnnualWaterDemand,AAnnualPowerDemand: double;AHeading,ATitle : string ; AValuesLines:TObjectlist {TStringList});
    property ElementID          : integer                read FElementID         write FElementID;
    property BlockType          : TOutputDataType        read FBlockType         write FBlockType;
    property BlockNumber        : integer                read FBlockNumber       write FBlockNumber;
    property LoadCaseNumber     : integer                read FLoadCaseNumber    write FLoadCaseNumber;
    property SequenceNumber     : integer                read FSequenceNumber    write FSequenceNumber;
    property AnnualWaterDemand  : double                 read FAnnualWaterDemand write FAnnualWaterDemand;
    property AnnualPowerDemand  : double                 read FAnnualPowerDemand write FAnnualPowerDemand;
    property BlockHeading       : string                 read FBlockHeading      write FBlockHeading;
    property BlockTitle         : string                 read FBlockTitle        write FBlockTitle;
    property BlockIndex         : Int64                  read FBlockIndex ;      // write FBlockIndex;
    property ValuesLines        : TObjectlist{TStringList}             read FValuesLines      write FValuesLines;
  end;

  TSummaryOutputDataSource  = class(TAbstractAppObject)
  protected
    FCurrentSource    : TSummaryOutputDataSourceName;
    FAvailableSources : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_CurrentSource: TSummaryOutputDataSourceName;
    procedure Set_CurrentSource(ASource: TSummaryOutputDataSourceName);
    function Get_DataSourceAvailable(ASource: TSummaryOutputDataSourceName): boolean;
    function Get_DataSourceByIndex(AIndex : integer): TSummaryOutputDataSourceName;
    function Get_DataSourcesCount:integer;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;

    procedure AddSource(ASource:TSummaryOutputDataSourceName);
    procedure RemoveSource(ASource:TSummaryOutputDataSourceName);
    property CurrentSource : TSummaryOutputDataSourceName read Get_CurrentSource write Set_CurrentSource;
    property DataSourceAvailable[ASource: TSummaryOutputDataSourceName]: boolean read Get_DataSourceAvailable;
    property DataSourceByIndex[AIndex : integer]: TSummaryOutputDataSourceName read Get_DataSourceByIndex;
    property DataSourcesCount : integer read Get_DataSourcesCount;
  end;

  TDemandOutputData = class(TAbstractAppObject)
  protected
    function GetIrrigationDemandValues(AChannelDemandValues: TStrings;  AChannel: IGeneralFlowChannel): boolean;
    function GetMasterControlDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
    function GetMinMaxDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
    function GetSpecifiedDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
    function GetIFRDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
    function CalculateAverageDemandValues(AChannelDemandValues: TStrings): boolean;
  public
    function GetChannelDemandValues(ADataContainer: TStrings; AChannelNumber: integer; var AErrors: string): boolean;
  end;


  TSummaryOutputData = class(TAbstractAppObject,ISummaryOutputData)
  protected
    FDataSelection : TOutputDataSelection;
    FDBItemsList   : TObjectList;
    //FSumOutBlob    : TSumOutBlob;
    //FFileItemsList : TObjectList;
    FSummaryOutputDataSource : TSummaryOutputDataSource;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetBlockAvarageSequenceValue(AElement:TSummaryOutputDataElement): double;
    function GetBlockMonthlyValue(AMonthNumber: integer; AElement:TSummaryOutputDataElement): double;
    function GetBlockAveragePeriodValue(AStartDate,AEndDate: TDateTime; AElement:TSummaryOutputDataElement): double;
    function GetBlockAvarageStorageChange(AElement:TSummaryOutputDataElement): double;
    function GetDataElement(ABlockType:TOutputDataType;ALoadCaseNumber,ASequenceNumber,AElementID: integer):TSummaryOutputDataElement;

    procedure LoadDatasetValuesIntoAStringList(ADataList : TStrings;ADataSet: TAbstractModelDataset);
    procedure LoadValuesLinesValuesIntoAStringList(ADataList : TStrings;AElement:TSummaryOutputDataElement);
    procedure LoadValuesLinesWithDateValuesIntoAStringList(ADataList : TStrings;AElement:TSummaryOutputDataElement);
    //procedure LoadValuesBlobWithDateValuesIntoAStringList(ADataList : TStrings;AElement:TSummaryOutputDataElement);

    procedure LoadDatasetValuesWithAveragesIntoAStringList(ADataList : TStrings;ADataSet: TAbstractModelDataset);
    procedure LoadValuesLinesWithAveragesIntoAStringlist(ADataList : TStrings;AElement:TSummaryOutputDataElement);
    //procedure LoadBlobValuesWithAveragesIntoAStringlist(ADataList : TStrings;AElement:TSummaryOutputDataElement);

    function PopulateChannelMasterControlDeficits(ALoadCaseNumber: integer;ADataContainer, AComplyContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function PopulateChannelMinMaxDeficits(ALoadCaseNumber: integer;ADataContainer, AComplyContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function PopulateChannelIFRDeficits(ALoadCaseNumber: integer;ADataContainer, AComplyContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function PopulateChannelDemandDeficits(ALoadCaseNumber: integer;ADataContainer, AComplyContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function PopulateIrrigationChannelDeficits(ALoadCaseNumber: integer;ASupplyContainer, ADeficitOutputContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function GetSpecifiedDemandFeatureDataSet(ADemandFileData   : TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function GetScenarioWhereClause: string;
    //function PopulateElementsFromBlobObject: boolean;
    function CalcRankLongtermSupply(AAnualFirmSelectedDemand : TSumOutAnualFirmEnergyDemandBlockValues;ADataStringList : TStrings;var AActualDemand : double;AFixed : boolean): boolean;

  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function Get_SumOutBlob: ISumOutBlob; safecall;
    procedure PopulateDataSelection(ADataSelection: TOutputDataSelection);
    function GetSpecifiedDemandFeatureFileData(ADemandFileData   : TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
    function GetChannelAreaData(ADataSource: TStrings; ADataType:TOutputDataType;
                                ANetworkElementID: integer;var AErrors: string): boolean;

    function GetLongtermSupplyData(ADataType:TOutputDataType;ANetworkElementID: integer;ADataStringList: TStringList;  var AErrors: string;var AActualDemand : double;AFixed : boolean = True): boolean;
    function GetBlockData(ADataContainer: TStrings; ADataType:TOutputDataType;ANetworkElementID: integer;var AErrors: string): boolean;
    function GetBlockDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                     ANetworkElementID: Integer; var AErrors: WideString): WordBool; safecall;
    function GetBlockAverageDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                     ANetworkElementID: Integer; var AErrors: WideString): WordBool; safecall;
    function GetPeriodChangeDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                     ANetworkElementID: Integer; var AErrors: WideString): WordBool; safecall;
    function GetElementsByElementDataType(AElementDataType: TOutputDataType;
                                          var ADataContainer: WideString): WordBool; safecall;

    function GetBlockDataSet(ADataType:TOutputDataType;ANetworkElementID: integer;ADataStringList: TStringList;  var AErrors: string): boolean;

    function GetAnnualChannelWaterBalancelData(AChannelNumber,ALoadCaseNumber, ASequenceNumber: integer;AWaterBalanceData: TStrings;
             var AErrors: string): boolean;
    function GetMonthlyChannelWaterBalancelData(AChannelNumber,ALoadCaseNumber,ASequenceNumber ,AMonthNumber: integer;
             AWaterBalanceData: TStrings; var AErrors: string): boolean;

    function GetReservoirWaterBalancelData(AReservoirNumber, ALoadCaseNumber,ASequenceNumber: integer;
             ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings; var AErrors: string): boolean;

    function GetComplianceGridBlockData(ADataContainer, AComplyContainer: TStrings; ALoadCaseNumber, ASequenceNumber,
             ANetworkElementID: integer;var AErrors: string): boolean;

    function PopulateElement(ABlockType: TOutputDataType; AElementID,ABlockNumber,ALoadCaseNumber,
             ASequenceNumber: integer;AAnnualWaterDemand,AAnnualPowerDemand: double;AHeading,ATitle : string; AValuesLines:TObjectlist{TStringList{; ABlockIndex : Int64}): boolean;

    function GetBlockNextSignificantMonth(ABlockType: TOutputDataType;AElementID,ALoadCaseNumber, ASequenceNumber,
             ACurrentMonthNumber: integer;var AErrors: string): integer;
    function GetBlockPreviousSignificantMonth(ABlockType: TOutputDataType;AElementID,ALoadCaseNumber, ASequenceNumber,
             ACurrentMonthNumber: integer;var AErrors: string): integer;
    function GetBoxPlotData(ADataContainer: TStrings; ADataType:TOutputDataType;ANetworkElementID: integer;
             const ALoadCase: integer;var AErrors: string): boolean;
    function GetChannelAreaBoxPlotData(ADataContainer: TStrings;ADataType: TOutputDataType; ANetworkElementID : integer;
             const ALoadCase: integer; var AErrors: string): boolean;
    function GetChannelAreaComplianceGridData(ADataContainer, AComplyContainer: TStrings; ALoadCaseNumber,
             ASequenceNumber, ANetworkElementID: integer; var AErrors: string): boolean;
    function GetChannelAreaMonthlyWaterBalancelData(AChannelAreaNumber,ALoadCaseNumber,ASequenceNumber ,AMonthNumber: integer;
             AWaterBalanceData: TStrings; var AErrors: string): boolean;

    function GetChannelAreaAnnualWaterBalancelData(AChannelAreaNumber,ALoadCaseNumber, ASequenceNumber: integer;
             AWaterBalanceData: TStrings; var AErrors: string): boolean;
    //function PopulateFromFileBlob: boolean;
    //function PopulateFromDataBaseBlob: boolean;
    function ReCalculateElementSequenceNumbers: boolean;

    function GetReservoirAreaData(ADataSource: TStrings; ADataType:TOutputDataType;
                                ANetworkElementID: integer;var AErrors: string): boolean;

    function GetReservoirAreaBoxPlotData(ADataContainer: TStrings;ADataType: TOutputDataType; ANetworkElementID : integer;
             const ALoadCase: integer; var AErrors: string): boolean;
    property DataSources : TSummaryOutputDataSource  read FSummaryOutputDataSource;
    property SumOutBlob: ISumOutBlob read Get_SumOutBlob;
  end;

  TYieldOutputData = class(TAbstractAppObject)
  protected
  public
  end;

  THydrologOutputData = class(TAbstractAppObject)
  protected
  public
  end;
  TOutputData = class(TAbstractAppObject, IOutputData)
  protected
    FGUIForm              : TfrmPopupForm; //TYieldModelDataGUIForm;
    FPopupForm            : TfrmPopupDialog;
    FValidator            : TOutputDataSelectionValidator;
    FShowMore             : Boolean;
    FDataSelection        : TOutputDataSelection;
    FHydrologOutputData   : THydrologOutputData;
    FPlottingOutputData   : TPlottingOutputData;
    FDataOutputData       : TDataOutputData;
    FDebuggingOutputData  : TDebuggingOutputData;
    FSummaryOutputData    : TSummaryOutputData;
    FYieldOutputData      : TYieldOutputData;
    FDemandOutputData     : TDemandOutputData;
    FLongtermSupplyData   : TLongtermSupplyData;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnSelectionChange(Sender: TObject);
    procedure OnMoreLessClick (Sender: TObject);
    procedure ReadDataSelectionFromINI;
    procedure WriteDataSelectionToINI;
    procedure UpdateDataSelectionFromDialog;
    procedure OnCloseForm(Sender: TObject; var Action: TCloseAction);
    procedure DoShowdfmPopupForm(AIdentifier: Integer;ANetworkElementType: TNetworkElementType;
             AOutputSourceDialog: TOutputSourceDialog;AOutputDataType: TOutputDataType;AOutputValueType: TOutputValueType);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ShowDataSelectionDialog(AIdentifier: Integer;ANetworkElementType: TNetworkElementType;
             AOutputSourceDialog: TOutputSourceDialog;AOutputDataType: TOutputDataType;AOutputValueType: TOutputValueType): WordBool; safecall;
    function Get_GetSelection: IOutputDataSelection; safecall;
    function Get_SummaryOutputData: ISummaryOutputData; safecall;

    property SummaryOutputData        : ISummaryOutputData   read Get_SummaryOutputData;
    property CastHydrologOutputData   : THydrologOutputData  read FHydrologOutputData ;
    property CastPlottingOutputData   : TPlottingOutputData  read FPlottingOutputData ;
    property CastDataOutputData       : TDataOutputData      read FDataOutputData     ;
    property CastDebuggingOutputData  : TDebuggingOutputData read FDebuggingOutputData;
    property CastSummaryOutputData    : TSummaryOutputData   read FSummaryOutputData;
    property CastYieldOutputData      : TYieldOutputData     read FYieldOutputData    ;
    property CastDataSelection        : TOutputDataSelection read FDataSelection      ;
    property CastDemandOutputData     : TDemandOutputData    read FDemandOutputData;
    property CastLongtermSupplyData   : TLongtermSupplyData  read FLongtermSupplyData;
  end;

implementation

uses
  Windows,
  SysUtils,
  UFileNames,
  UConstants,
  UDataSetType,
  UUtilities,
  DateUtils,
  UIFRFeatures,
  UIrrigationBlock,
  UDemandFileAgent,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UOutputDataSQLAgent,
  UDemandOutputFileAgent,
  UErrorHandlingOperations,
  UPlotFileAgent,
  UDataComponent, UNetworkFeaturesData;


{ TBlobElement }
{constructor TBlobElement.Create;
const OPNAME = 'TBlobElement.Create';
begin
  inherited;
  try
    FElementNumber := 0;
    FElementName   := '';
    FPrintOutput   := 'N';
    FNrOfPoints    := 0;
    FStatus        := 0;
    FElementIndex  := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TOutputData }

function TOutputData._AddRef: Integer;
const OPNAME = 'TOutputData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputData._Release: Integer;
const OPNAME = 'TOutputData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputData.CreateMemberObjects;
const OPNAME = 'TOutputData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FValidator            := nil;
    FGUIForm              := nil;
    FShowMore             := False;
    FDataSelection        := TOutputDataSelection.Create(FAppModules);
    FHydrologOutputData   := THydrologOutputData.Create(FAppModules);
    FPlottingOutputData   := TPlottingOutputData.Create(FAppModules);
    FDataOutputData       := TDataOutputData.Create(FAppModules);
    FDebuggingOutputData  := TDebuggingOutputData.Create(FAppModules);
    FSummaryOutputData    := TSummaryOutputData.Create(FAppModules);
    FYieldOutputData      := TYieldOutputData.Create(FAppModules);
    FDemandOutputData     := TDemandOutputData.Create(FAppModules);
    FLongtermSupplyData   := TLongtermSupplyData.Create(FAppModules);

    FSummaryOutputData.PopulateDataSelection(FDataSelection);
    FPlottingOutputData.PopulateDataSelection(FDataSelection);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.DestroyMemberObjects;
const OPNAME = 'TOutputData.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FDataSelection);
    FreeAndNil(FHydrologOutputData);
    FreeAndNil(FPlottingOutputData);
    FreeAndNil(FDataOutputData);
    FreeAndNil(FDebuggingOutputData);
    FreeAndNil(FSummaryOutputData);
    FreeAndNil(FYieldOutputData);
    FreeAndNil(FDemandOutputData);
    FreeAndNil(FLongtermSupplyData);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.DoShowdfmPopupForm(AIdentifier: Integer;  ANetworkElementType: TNetworkElementType;
  AOutputSourceDialog: TOutputSourceDialog; AOutputDataType: TOutputDataType;  AOutputValueType: TOutputValueType);
const OPNAME = 'TOutputData.DoShowdfmPopupForm';
begin
  try
    FPopupForm  := TfrmPopupDialog.Create(Application);
    FValidator := TOutputDataSelectionValidator.Create(nil,FAppModules);
    try
      FPopupForm.Initialise;
      FPopupForm.AddValidator(FValidator);
      FValidator.SetDisplayOptions(AIdentifier,ANetworkElementType,AOutputSourceDialog, AOutputDataType, AOutputValueType);
      FValidator.Initialise;
      FValidator.PopulateDataViewer;
      FValidator.LanguageHasChanged;

      if (FPopupForm.CanShowValidators) then
      begin

        FPopupForm.Caption      := FValidator.TabShetCaption;
        FPopupForm.ClientHeight := 200;
        FPopupForm.ClientWidth  := 550;
        FPopupForm.OnClose := OnCloseForm;
        if (FShowMore) then
          OnMoreLessClick(Self);
        FPopupForm.FormStyle := fsStayOnTop;
        FPopupForm.ShowModal;
        if(FPopupForm.ModalResult = mrOk)then
          UpdateDataSelectionFromDialog;
      end;
    finally
      FreeAndNil(FValidator);
      FreeAndNil(FPopupForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.UpdateDataSelectionFromDialog();
const OPNAME = 'TOutputData.UpdateDataSelectionFromDialog';
var
  lLoadCase      : integer;
  lSequence      : integer;
  lMonth         : integer;
  LDisplayMonth  : integer;
  LDecisionMonth  : integer;
  lUnits         : TOutputUnits;
  lValueType     : TOutputValueType;
  lTimeStep      : TOutputTimeStep;
  lHighlight     : boolean;
  LPanel         : TOutputDataSelectionDialog;
  lAverageType      : TOutputAverageType;
  LDate             : TDateTime;
  lAverageStartDate : TDateTime;
  lAverageEndDate   : TDateTime;
  LPlotOption       : TOutputPlotOptions;
  LYearsToSkip      : integer;
  LApplySensitivity : TSensitivity;
  LPercSensitivity,
  LSensitivity      : double;

begin
  try
    if(FValidator <> nil) then
    begin
      LPanel     := FValidator.DataSelectionDialog;
      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        LLoadCase  := 1;
      end
      else
      begin
        LLoadCase  := LPanel.LoadCaseSelector.CurrentLoadCase;
      end;
      LSequence  := LPanel.SequenceSelector.CurrentSequence;
      LMonth     := LPanel.MonthSelector.CurrentMonth;
      LDisplayMonth := LPanel.SelectMonthCombobox.ItemIndex;
      LDecisionMonth := StrToIntDef(LPanel.DecisionMonthCombobox.Text,0);
      if (LPanel.MetersPerSecond.Checked) then
        LUnits := ouPerSecond
      else if (LPanel.Million.Checked) then
        LUnits := ouMcmPerMonthOrYear
      else if (LPanel.Percentage.Checked) then
        lUnits := ouPercentage
      else if (LPanel.LivePercentage.Checked) then
        lUnits := ouLivePercentage
      else if (LPanel.MCM.Checked) then
        lUnits := ouMCM
      else if (LPanel.Meters.Checked) then
        lUnits := ouMeters
      else if (LPanel.MegaLitersPerDay.Checked) then
        lUnits := ouMegaLitersPerDay
      else
        lUnits := ouMCM;

      if (LPanel.Supply.Checked) then
        lValueType := ovtSupply
      else if (LPanel.Demand.Checked) then
        lValueType := ovtDemand
      else if (LPanel.Deficit.Checked) then
        lValueType := ovtDeficits
      else if (LPanel.DemandAndSupply.Checked) then
        lValueType := ovtDemandAndSupply
      else if (LPanel.Allocated.Checked) then
        lValueType := ovtAllocated
      else
        lValueType := ovtSupply;

      if (LPanel.Monthly.Checked) then
        lTimeStep := otsMonthly
      else if (LPanel.Annually.Checked) then
        lTimeStep := otsAnnual
      else if (LPanel.MonthlyCummulative.Checked) then
        lTimeStep := otsMonthlyCumulative
      else if (LPanel.AnnualCummulative.Checked) then
        lTimeStep := otsAnnualCumulative
      else if (LPanel.Sequence.Checked) then
        lTimeStep := otsSequence
      else
        lTimeStep := otsMonthly;

      LHighlight := LPanel.ChkHighlight.Checked;
      if LPanel.PeriodSequence.Checked then
        lAverageType      := oatSequence
      else if LPanel.PeriodSelection.Checked then
        lAverageType      := oatPeriod
      else
        lAverageType      := oatSequence;

      LYearsToSkip := 0;
      LPlotOption  := poNone;
      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        if Trim(LPanel.edtYearsToSkip.Text) <> '' then
          LYearsToSkip := StrToInt(Trim(LPanel.edtYearsToSkip.Text));
        if LPanel.Condenced.Checked then
        begin
          LPlotOption  := poCondenced;
        end
        else
        if LPanel.NonCondenced.Checked then
          LPlotOption  := poNotCondenced
        else
        if LPanel.Cumulative.Checked then
          LPlotOption  := poCumulative
        else
        if LPanel.NonCumulative.Checked then
          LPlotOption  := poNotCumulative
        else
          LPlotOption  := poNotCumulative;

      end;

      LDate             := LPanel.DatePeriodStart.DateTime;
      lAverageStartDate := EncodeDate(YearOf(LDate), MonthOf(LDate), 1);
      LDate             := LPanel.DatePeriodEnd.DateTime;
      lAverageEndDate   := EncodeDate(YearOf(LDate), MonthOf(LDate), 1);
      LApplySensitivity := stvNone;

      if LPanel.ApplySensitivity.Checked then
        LApplySensitivity := stvNone;
      if LPanel.AbsoluteSensitivity.Checked then
        LApplySensitivity := stvAbsolute;
      if LPanel.PercSensitivity.Checked then
        LApplySensitivity := stvPercentage;

      LSensitivity := 0.001;
      LPercSensitivity := 0;
      if Trim(LPanel.edtSensitivity.Text) <> '' then
        LSensitivity := StrToFloat(LPanel.edtSensitivity.Text);
      if Trim(LPanel.edtPercSensitivity.Text) <> '' then
        LPercSensitivity := StrToFloat(LPanel.edtPercSensitivity.Text);

      FDataSelection.Populate(LLoadCase,LSequence,LMonth,LDisplayMonth,LDecisionMonth,LUnits,LValueType,LTimeStep,LHighlight,
                              lAverageType,lAverageStartDate,lAverageEndDate,LPlotOption,LYearsToSkip,LApplySensitivity,LPercSensitivity, LSensitivity);
      WriteDataSelectionToINI;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'OutputDataSelection','','');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.OnSelectionChange(Sender: TObject);
const OPNAME = 'TOutputData.OnSelectionChange';
begin
  try
    if (FValidator <> nil) then
    begin
      UpdateDataSelectionFromDialog;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.OnMoreLessClick (Sender: TObject);
const OPNAME = 'TOutputData.OnMoreLessClick';
begin
  try
    if (FValidator <> nil) AND (FGUIForm <> nil) then
    begin
      FShowMore := FValidator.DataSelectionDialog.ToggleMoreLess;
      if (FShowMore) then
      begin
        FGUIForm.ClientWidth  := 750;
        FGUIForm.ClientHeight := FGUIForm.ClientHeight + 290;
      end
      else
      begin
        FGUIForm.ClientWidth  := 550;
        FGUIForm.ClientHeight := FGUIForm.ClientHeight - 290;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.ReadDataSelectionFromINI;
const OPNAME = 'TOutputData.ReadDataSelectionFromINI';
var
  lModelCode,
  lStudyAreaCode,
  lSubAreaCode,
  lScenarioCode   :  string;

  lLoadCase,
  lSequence,
  lDecisionMonth,
  lMonth,
  lDisplayMonth,
  lUnits,
  lValueType,
  lTimeStep       :  Integer;
  lHighlight      :  WordBool;
  lAverageType      : TOutputAverageType;
  lAverageStartDate : Double;
  lAverageEndDate   : Double;
  lTimeStepperLabel : string;
  LPlotOption       : TOutputPlotOptions;
  LYearsToSkip      : integer;
  LPercSensitivity,
  LSensitivity      : double;
  LApplySensitivity : TSensitivity;
begin
  try
    (* get the settings from the viewini.ini file  jkw *)
    lTimeStepperLabel := FAppModules.Model.ModelName + '_TimeStepper';
    lModelCode := FAppModules.ViewIni.ReadString(lTimeStepperLabel,'Model','');
    lStudyAreaCode := FAppModules.ViewIni.ReadString(lTimeStepperLabel,'StudyAreaName','');
    lSubAreaCode := FAppModules.ViewIni.ReadString(lTimeStepperLabel,'SubArea','');
    lScenarioCode := FAppModules.ViewIni.ReadString(lTimeStepperLabel,'Scenario','');

    if ((lModelCode = FAppModules.StudyArea.ModelCode) and
        (lStudyAreaCode = FAppModules.StudyArea.StudyAreaCode) and
        (lSubAreaCode = FAppModules.StudyArea.SubAreaCode) and
        (lScenarioCode = FAppModules.StudyArea.ScenarioCode)) then
    begin
      lLoadCase := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'LoadCase', 1);
      if(lLoadCase < 1) then lLoadCase := 1;
      lDecisionMonth  := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'DecisionMonth', 0);
      if(lDecisionMonth < 0) then lDecisionMonth := 0;
      lSequence := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'Sequence', 1);
      if(lSequence < 1) then lSequence := 1;
      lMonth := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'MonthIndex', 1);
      if(lMonth < 1) then lMonth := 1;
      //lDisplayMonth := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'DisplayMonth', 0);
      lDisplayMonth := 0;
      if(lDisplayMonth < 0) then lMonth := 0;
      lUnits := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'Units', ouPerSecond);
      lValueType := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'ValueType', ovtSupply);
      lTimeStep := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'TimeStep', otsMonthly);
      lHighlight := WordBool(FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'Highlight', 1));
      lAverageType      := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'AverageType', oatSequence);
      lAverageStartDate := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'AverageStartDate', 0);
      lAverageEndDate   := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'AverageEndDate', 0);
      LPlotOption       := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'PlotOption', poNone);
      LYearsToSkip      := FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'YearsToSkip', 0);
      LApplySensitivity := TSensitivity(FAppModules.ViewIni.ReadInteger(lTimeStepperLabel,'ApplySensitivity', 0));
      LSensitivity      := StrToFloat(FAppModules.ViewIni.ReadString(lTimeStepperLabel,'Sensitivity', '0.000'));
      LPercSensitivity  := StrToFloat(FAppModules.ViewIni.ReadString(lTimeStepperLabel,'PercSensitivity', '0.000'));

      if(FAppModules.Model <> nil) and (FAppModules.Model.ModelData <> nil) then
      begin
        if(lLoadCase > TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NrOfActiveLoadCases) then
          lLoadCase := 1;
        if(lSequence > TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis) then
          lSequence := 1;
      end;
      FDataSelection.Populate(lLoadCase, lSequence, lMonth, lDisplayMonth, lDecisionMonth, lUnits, lValueType,
                              lTimeStep, lHighlight, lAverageType,lAverageStartDate,lAverageEndDate,LPlotOption,LYearsToSkip,LApplySensitivity,LPercSensitivity, LSensitivity);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.WriteDataSelectionToINI;
const OPNAME = 'TOutputData.WriteDataSelectionToINI';
var
  lTimeStepperLabel : string;
begin
  try
    (* write the settings to the viewini.ini file jkw *)
    lTimeStepperLabel := FAppModules.Model.ModelName + '_TimeStepper';
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'Model',         FAppModules.StudyArea.ModelCode);
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'StudyAreaName', FAppModules.StudyArea.StudyAreaCode);
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'SubArea',       FAppModules.StudyArea.SubAreaCode);
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'Scenario',      FAppModules.StudyArea.ScenarioCode);

    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'LoadCase',      FDataSelection.FLoadCase);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'DecisionMonth', FDataSelection.FDecisionMonth);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'Sequence',      FDataSelection.FSequence);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'MonthIndex',    FDataSelection.FMonth);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'DisplayMonth',    0);
    //FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'DisplayMonth',    FDataSelection.FDisplayMonth);

    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'Units',         FDataSelection.FUnits);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'ValueType',     FDataSelection.FValueType);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'TimeStep',      FDataSelection.FTimeStep);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'Highlight',     Integer(FDataSelection.FHighlight));

    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'AverageStartDate',     Trunc(FDataSelection.FAverageStartDate));
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'AverageEndDate',      Trunc(FDataSelection.FAverageEndDate));
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'AverageType',     Integer(FDataSelection.FAverageType));
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'PlotOption',FDataSelection.FPlotOption);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'YearsToSkip',FDataSelection.FYearsToSkip);
    FAppModules.ViewIni.WriteInteger(lTimeStepperLabel,'ApplySensitivity',Ord(FDataSelection.FApplySensitivity));
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'Sensitivity',FloatToStr(FDataSelection.FSensitivity));
    FAppModules.ViewIni.WriteString(lTimeStepperLabel,'PercSensitivity',FloatToStr(FDataSelection.FPercSensitivity));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputData.OnCloseForm(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TOutputData.OnCloseForm';
begin
  try
    WriteDataSelectionToINI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputData.ShowDataSelectionDialog(AIdentifier: Integer;ANetworkElementType: TNetworkElementType;
         AOutputSourceDialog: TOutputSourceDialog;AOutputDataType: TOutputDataType;AOutputValueType: TOutputValueType): WordBool;
const OPNAME = 'TOutputData.ShowDataSelectionDialog';
var
  LAuto : boolean;
begin
  Result := False;
  try
    LAuto := (AOutputSourceDialog = osdNetworkVisualiser);

    FGUIForm   := TfrmPopupForm.Create(nil);

    FValidator := TOutputDataSelectionValidator.Create(nil,FAppModules);
    try
      FGUIForm.Initialise;
      FValidator.Panel.Parent := FGUIForm;
      FValidator.Panel.Align := alClient;
      FValidator.Panel.Visible := True;

      FValidator.SetDisplayOptions(AIdentifier,ANetworkElementType,AOutputSourceDialog, AOutputDataType, AOutputValueType);
      FValidator.Initialise;
      FValidator.PopulateDataViewer;
      FValidator.LanguageHasChanged;

      FGUIForm.Caption      := FValidator.TabShetCaption;

      FGUIForm.ClientHeight := 200;
      FGUIForm.ClientWidth  := 550;

      FGUIForm.OnClose := OnCloseForm;
      if LAuto then
         FValidator.OnSelectionChange := OnSelectionChange;
      FValidator.OnMoreLessClick      := OnMoreLessClick;
      if (FShowMore) then
        OnMoreLessClick(Self);
      FGUIForm.FormStyle := fsStayOnTop;
      FGUIForm.ShowModal;
      Result := (FGUIForm.ModalResult = mrOk);
      if Result and (not LAuto)then
        UpdateDataSelectionFromDialog;
    finally
      FreeAndNil(FValidator);
      FreeAndNil(FGUIForm);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.Get_GetSelection: IOutputDataSelection;
const OPNAME = 'TOutputData.Get_GetSelection';
begin
  Result := nil;
  try
    Result := FDataSelection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.Get_SummaryOutputData: ISummaryOutputData;
const OPNAME = 'TOutputData.Get_SummaryOutputData';
begin
  Result := nil;
  try
    Result := FSummaryOutputData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.Initialise: boolean;
const OPNAME = 'TOutputData.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataSelection.Initialise;
    ReadDataSelectionFromINI;
    if Assigned(FValidator) then
      FValidator.Initialise;
    if Assigned(FGUIForm) then
      FGUIForm.Initialise;
    FHydrologOutputData.Initialise;
    FPlottingOutputData.Initialise;
    FDataOutputData.Initialise;
    FDebuggingOutputData.Initialise;
    FSummaryOutputData.Initialise;
    FYieldOutputData.Initialise;
    FDemandOutputData.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.LanguageHasChanged: boolean;
const OPNAME = 'TOutputData.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FValidator) then
      FValidator.LanguageHasChanged;
    //if Assigned(FGUIForm) then
      //FGUIForm.LanguageHasChanged;
    FDataSelection.LanguageHasChanged;
    FHydrologOutputData.LanguageHasChanged;
    FPlottingOutputData.LanguageHasChanged;
    FDataOutputData.LanguageHasChanged;
    FDebuggingOutputData.LanguageHasChanged;
    FSummaryOutputData.LanguageHasChanged;
    FYieldOutputData.LanguageHasChanged;
    FDemandOutputData.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.ResetState: boolean;
const OPNAME = 'TOutputData.ResetState';
begin
  Result := inherited ResetState;
  try
    if Assigned(FValidator) then
      FValidator.ResetState;
    //if Assigned(FGUIForm) then
      //FGUIForm.ResetState;
    FDataSelection.ResetState;
    FHydrologOutputData.ResetState;
    FPlottingOutputData.ResetState;
    FDataOutputData.ResetState;
    FDebuggingOutputData.ResetState;
    FSummaryOutputData.ResetState;
    FYieldOutputData.ResetState;
    FDemandOutputData.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.SaveState: boolean;
const OPNAME = 'TOutputData.SaveState';
begin
  Result := inherited SaveState;
  try
    if Assigned(FValidator) then
      FValidator.SaveState;
    //if Assigned(FGUIForm) then
     // FGUIForm.SaveState;
    FDataSelection.SaveState;
    FHydrologOutputData.SaveState;
    FPlottingOutputData.SaveState;
    FDataOutputData.SaveState;
    FDebuggingOutputData.SaveState;
    FSummaryOutputData.SaveState;
    FYieldOutputData.SaveState;
    FDemandOutputData.SaveState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TOutputData.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
  try
    if Assigned(FValidator) then
      FValidator.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    //if Assigned(FGUIForm) then
     // FGUIForm.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FDataSelection.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FHydrologOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FPlottingOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FDataOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FDebuggingOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FSummaryOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    FYieldOutputData.StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputData.StudyHasChanged: boolean;
const OPNAME = 'TOutputData.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Assigned(FValidator) then
      FValidator.StudyHasChanged;
    //if Assigned(FGUIForm) then
      //FGUIForm.StudyHasChanged;
    FDataSelection.StudyHasChanged;
    FHydrologOutputData.StudyHasChanged;
    FPlottingOutputData.StudyHasChanged;
    FDataOutputData.StudyHasChanged;
    FDebuggingOutputData.StudyHasChanged;
    FSummaryOutputData.StudyHasChanged;
    FYieldOutputData.StudyHasChanged;
    FDemandOutputData.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSummaryOutputData }

procedure TSummaryOutputData.CreateMemberObjects;
const OPNAME = 'TSummaryOutputData.CreateMemberObjects';
begin
  inherited;
  try
    FDBItemsList   := TObjectList.Create(False);
    //FFileItemsList  := TObjectList.Create(True);
    FDataSelection := nil;
    //FSumOutBlob    := TSumOutBlob.Create(FAppModules);
    FSummaryOutputDataSource := TSummaryOutputDataSource.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.DestroyMemberObjects;
const OPNAME = 'TSummaryOutputData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDBItemsList);
    //FreeAndNil(FSumOutBlob);
    FreeAndNil(FSummaryOutputDataSource);
    //FreeAndNil(FFileItemsList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.Initialise: boolean;
const OPNAME = 'TSummaryOutputData.Initialise';
begin
  Result := inherited Initialise;
  try
    FDBItemsList.Clear;
    FSummaryOutputDataSource.Initialise;
    //FFileItemsList.Clear;
    Result         := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.StudyHasChanged: boolean;
const OPNAME = 'TSummaryOutputData.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDBItemsList.Clear;
    FSummaryOutputDataSource.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.PopulateElement(ABlockType: TOutputDataType; AElementID,ABlockNumber,ALoadCaseNumber,
         ASequenceNumber: integer;AAnnualWaterDemand,AAnnualPowerDemand: double;AHeading,ATitle : string ;AValuesLines:TObjectlist{ TStringList{; ABlockIndex : Int64}): boolean;
const OPNAME = 'TSummaryOutputData.PopulateElement';
var
  LElement: TSummaryOutputDataElement;
begin
  Result := False;
  try
    LElement := TSummaryOutputDataElement.Create;
    LElement.Initialise;
    //    FDBItemsList.Add(LElement);

    //    LElement.PopulateBlockIndex(ABlockType,AElementID,ABlockNumber,ALoadCaseNumber,ASequenceNumber,
    //              AAnnualWaterDemand,AAnnualPowerDemand,AHeading,ATitle,ABlockIndex);
    LElement.Populate(ABlockType,AElementID,ABlockNumber,ALoadCaseNumber,ASequenceNumber,
              AAnnualWaterDemand,AAnnualPowerDemand,AHeading,ATitle,AValuesLines);

    FDBItemsList.Add(LElement);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockDataSet(ADataType:TOutputDataType;ANetworkElementID: integer;ADataStringList: TStringList;  var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetBlockDataSet';
var
  LElement  : TSummaryOutputDataElement;
begin
  Result := False;
  try
    AErrors := '';
    if(FDBItemsList.Count = 0) then
      AErrors := FAppModules.Language.GetString('ErrorString.ChannelSUMOutFileNotImported')
    else
    begin
      begin
        LElement := GetDataElement(ADataType,FDataSelection.LoadCase,FDataSelection.Sequence,ANetworkElementID);
        if (LElement = nil) then
         AErrors := FAppModules.Language.GetString('ErrorString.SelectedChannelNotIncluded')
        else if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
        begin
          LoadValuesLinesWithAveragesIntoAStringlist(ADataStringList,LElement);
          Result := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetAnnualChannelWaterBalancelData(AChannelNumber,ALoadCaseNumber, ASequenceNumber: integer;
         AWaterBalanceData: TStrings; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetAnnualChannelWaterBalancelData';
var
  LElement :TSummaryOutputDataElement;
  LChannelFlow: double;
  lChannel : IGeneralFlowChannel;
begin
  Result := False;
  try
    AErrors := '';
    if(FDBItemsList.Count = 0) then
      AErrors := FAppModules.Language.GetString('ErrorString.ChannelSUMOutFileNotImported')
    else
    begin
      if Assigned(AWaterBalanceData) then
      begin
        AWaterBalanceData.Clear;
        LElement := GetDataElement(btMonthlyAverageChannelFlow,ALoadCaseNumber,ASequenceNumber,AChannelNumber);
        if (LElement = nil) then
         AErrors := FAppModules.Language.GetString('ErrorString.SelectedChannelNotIncluded')
        else
        begin
          lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            LChannelFlow := GetBlockAvarageSequenceValue(LElement);
            if(LChannelFlow  = NullFloat) then
              AWaterBalanceData.Add('Average flow=No Data')
            else
              AWaterBalanceData.Add('Average flow='+FormatFloat('#####0.000',LChannelFlow));
            Result := True;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetMonthlyChannelWaterBalancelData(AChannelNumber,ALoadCaseNumber,ASequenceNumber ,
         AMonthNumber: integer;AWaterBalanceData: TStrings; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetMonthlyChannelWaterBalancelData';
var
  LElement :TSummaryOutputDataElement;
  LChannelFlow: double;
  lChannel : IGeneralFlowChannel;
begin
  Result := False;
  try
    AErrors := '';
    if(FDBItemsList.Count = 0) then
      AErrors := FAppModules.Language.GetString('ErrorString.ChannelSUMOutFileNotImported')
    else
    begin
      if Assigned(AWaterBalanceData) then
      begin
        AWaterBalanceData.Clear;
        LElement := GetDataElement(btMonthlyAverageChannelFlow,ALoadCaseNumber,ASequenceNumber,AChannelNumber);
        if (LElement = nil) then
         AErrors := FAppModules.Language.GetString('ErrorString.SelectedChannelNotIncluded')
        else
        begin
          lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
          if (lChannel <> nil) then
          begin
            LChannelFlow := GetBlockMonthlyValue(AMonthNumber,LElement);
            if(LChannelFlow  = NullFloat) then
              AWaterBalanceData.Add('Average flow=No Data')
            else
              AWaterBalanceData.Add('Average flow='+FormatFloat('#####0.000',LChannelFlow));
            Result := True;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetReservoirWaterBalancelData(AReservoirNumber, ALoadCaseNumber,ASequenceNumber: integer;
             ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetReservoirWaterBalancelData';
var
  //LSQLAgent: TOutputDataSQLAgent;
  LResevoirElement,
  LChannelElement :TSummaryOutputDataElement;
  LInflowName,
  LChannelName    : string;
  LNumberOfYears,
  LRefNumber,
  LIndex          : integer;
  lChannel        : IGeneralFlowChannel;
  lReservoir      : IReservoirData;
  LRainfall,
  LEvaporation,
  LStorageChange,
  LMassBalance,
  LInflow,
  LTotalInflow,
  LTotalOutflow,
  LInflowChannelTotal,
  LOutflowChannelTotal,
  LChannelFlow     : double;
  LParamReference  : IParamReference;
begin
  Result := False;
  try
    AErrors := '';
    if(FDBItemsList.Count = 0) then
      AErrors := FAppModules.Language.GetString('ErrorString.ReservoirSUMOutFileNotImported')
    else
    begin
      if Assigned(ASummaryValues) and Assigned(AInflowValues) and Assigned(AOutflowValues) and Assigned(ATotalsValues)then
      begin
        ASummaryValues.Clear;
        AInflowValues.Clear;
        AOutflowValues.Clear;
        ATotalsValues.Clear;

        LRainfall        := 0.0;
        LEvaporation     := 0.0;
        LStorageChange   := 0.0;
        LInflow          := 0.0;
        lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AReservoirNumber];
        if (lReservoir <> nil) then
        begin
          LInflowName  := 'Inflow';
          LRefNumber   := lReservoir.ReservoirConfigurationData.CatchmentRef;
          if(LRefNumber <> NullInteger) then
          begin
            LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceDataByCatchNumber[LRefNumber];
            if(LParamReference <> nil) then
              LInflowName  := ExtractFileName(LParamReference.FileReference);
          end;

          LResevoirElement := GetDataElement(btRainfallOnReservoirSurface,ALoadCaseNumber,ASequenceNumber,AReservoirNumber);
          if (LResevoirElement = nil) then
          begin
            ASummaryValues.Add('Rainfall (m3/s)=No Data');
            AInflowValues.Add('Rainfall (m3/s)=No Data');
          end
          else
          begin
            LRainfall := GetBlockAvarageSequenceValue(LResevoirElement);
            if(LRainfall = NullFloat) then
            begin
              ASummaryValues.Add('Rainfall (m3/s)=No Data');
              AInflowValues.Add('Rainfall (m3/s)=No Data');
            end
            else
            begin
              ASummaryValues.Add('Rainfall (m3/s)='+FormatFloat('#####0.000',LRainfall));
              AInflowValues.Add('Rainfall (m3/s)='+FormatFloat('#####0.000',LRainfall));
            end;
          end;
          LResevoirElement := GetDataElement(btGrossEvaporationLossFromReservoir,ALoadCaseNumber,ASequenceNumber,AReservoirNumber);
          if (LResevoirElement = nil) then
          begin
            ASummaryValues.Add('Evaporation (m3/s)=No Data');
            AOutflowValues.Add('Evaporation (m3/s)=No Data');
          end
          else
          begin
            LEvaporation := GetBlockAvarageSequenceValue(LResevoirElement);
            if(LEvaporation = NullFloat) then
            begin
              ASummaryValues.Add('Evaporation (m3/s)=No Data');
              AOutflowValues.Add('Evaporation (m3/s)=No Data');
            end
            else
            begin
              LEvaporation := -1 * LEvaporation;
              ASummaryValues.Add('Evaporation (m3/s)='+FormatFloat('#####0.000',LEvaporation));
              AOutflowValues.Add('Evaporation (m3/s)='+FormatFloat('#####0.000',LEvaporation));
            end;
          end;
          LResevoirElement := GetDataElement(btMonthEndReservoirVolume,ALoadCaseNumber,ASequenceNumber,AReservoirNumber);
          if (LResevoirElement = nil) then
          begin
            ASummaryValues.Add('Change in storage (m3/s)=No Data');
          end
          else
          begin
            LStorageChange := GetBlockAvarageStorageChange(LResevoirElement);
            if(LStorageChange = NullFloat) then
            begin
              ASummaryValues.Add('Change in storage (m3/s)=No Data');
            end
            else
            begin
              LNumberOfYears := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;

              LStorageChange := LStorageChange/LNumberOfYears/365.25/24/3600 * 1000000;
              ASummaryValues.Add('Change in storage (m3/s)='+FormatFloat('#####0.000',LStorageChange));
            end;
          end;

          LResevoirElement := GetDataElement(btNetBasinRunoffIntoResArea,ALoadCaseNumber,ASequenceNumber,AReservoirNumber);
          if (LResevoirElement = nil) then
          begin
            AInflowValues.Add(LInflowName+' (m3/s)=No Data');
          end
          else
          begin
            LInflow := GetBlockAvarageSequenceValue(LResevoirElement);
            if(LInflow = NullFloat) then
            begin
              AInflowValues.Add(LInflowName+' (m3/s)=No Data');
            end
            else
            begin
              AInflowValues.Add(LInflowName+' (m3/s)='+FormatFloat('#####0.000',LInflow));
            end;
          end;

          if(LRainfall = NullFloat) then LRainfall:= 0.0;
          if(LEvaporation = NullFloat) then LEvaporation:= 0.0;
          if(LStorageChange = NullFloat) then LStorageChange:= 0.0;
          if(LInflow = NullFloat) then LInflow:= 0.0;

          LMassBalance := LStorageChange + LRainfall + LEvaporation;
          ASummaryValues.Add('Net='+FormatFloat('#####0.000',LMassBalance));

          LInflowChannelTotal := 0.0;
          LOutflowChannelTotal:= 0.0;
          for LIndex := 0  to TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList.ChannelCount -1 do
          begin
            lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList.ChannelByIndex[LIndex];
            if (lChannel <> nil) then
            begin
              if (lChannel.DownStreamNodeNumber = AReservoirNumber)  or
                 (lChannel.UpStreamNodeNumber = AReservoirNumber)then
              begin
                LChannelName := LowerCase(lChannel.ChannelName);
                StringReplace(LChannelName,'=',' ',[rfReplaceAll]);
                LChannelElement := GetDataElement(btMonthlyAverageChannelFlow,ALoadCaseNumber,ASequenceNumber,
                                   lChannel.ChannelNumber);
                if (LChannelElement = nil) then
                begin
                  if (lChannel.DownStreamNodeNumber = AReservoirNumber) then
                    AInflowValues.Add(LChannelName+' (m3/s)=No Data')
                  else
                    AOutflowValues.Add(LChannelName+' (m3/s)=No Data');
                end
                else
                begin
                  LChannelFlow := GetBlockAvarageSequenceValue(LChannelElement);
                  if(LChannelFlow = NullFloat) then
                  begin
                    if (lChannel.DownStreamNodeNumber = AReservoirNumber) then
                      AInflowValues.Add(LChannelName+' (m3/s)=No Data')
                    else
                      AOutflowValues.Add(LChannelName+' (m3/s)=No Data');
                  end
                  else
                  begin
                    if (lChannel.DownStreamNodeNumber = AReservoirNumber) then
                    begin
                      LInflowChannelTotal := LInflowChannelTotal + LChannelFlow;
                      AInflowValues.Add(LChannelName+' (m3/s)='+FormatFloat('#####0.000',LChannelFlow));
                    end
                    else
                    begin
                      LOutflowChannelTotal := LOutflowChannelTotal + LChannelFlow;
                      AOutflowValues.Add(LChannelName+' (m3/s)='+FormatFloat('#####0.000',LChannelFlow));
                    end;
                  end;
                end;
              end;
            end;
          end;
          LTotalInflow := LRainfall + LInflow + LInflowChannelTotal;
          LTotalOutflow := LOutflowChannelTotal - LEvaporation;
          AInflowValues.Add('Total='+FormatFloat('#####0.000',LTotalInflow));
          AOutflowValues.Add('Total='+FormatFloat('#####0.000',LTotalOutflow));

          ATotalsValues.Add(' Inflow (m3/s) - Outflow (m3/s)='+FormatFloat('#####0.000',LTotalInflow-LTotalOutflow));
          ATotalsValues.Add(' Change in storage='+FormatFloat('#####0.000',LStorageChange));
          Result := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockAvarageSequenceValue(AElement:TSummaryOutputDataElement): double;
const OPNAME = 'TSummaryOutputData.GetBlockAvarageSequenceValue';
var
  LIndex      : integer;
  LTotal      : double;
  LObjectlist : TObjectlist;
  LValueLines : TValuesLine;
  //LAgent      : TSumOutBlobFileAgent;
begin
  Result := NullFloat;
  try
    if(AElement = nil) then Exit;
    {if (FSummaryOutputDataSource.CurrentSource in [sodsBlobFile,sodsBlobDatabase]) then
    begin
       LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        Result := LAgent.GetBlockAvarageSequenceValue(FSumOutBlob,AElement);
      finally
        LAgent.Free;
      end;
    end
    else}
    if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
    begin
      LObjectlist := AElement.FValuesLines;
      if LObjectlist <> nil then
      begin
        if(AElement.FValuesLines.Count > 0) then
        begin
          LTotal := 0;
          for Lindex := 0 to AElement.FValuesLines.Count -1 do
          begin
            LValueLines := TValuesLine(LObjectlist.Items[Lindex]);
            LTotal      := LTotal + LValueLines.FValues[14].FData;
          end;
          Result := LTotal/AElement.FValuesLines.Count;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockAveragePeriodValue(AStartDate,AEndDate: TDateTime; AElement: TSummaryOutputDataElement): double;
const OPNAME = 'TSummaryOutputData.GetBlockAveragePeriodValue';
var
  LIndex           : integer;
  LCount           : integer;
  LErrors          : string;
  LTempContainer   : TStringList;
  LSourceContainer : TStringList;
  LDestContainer   : TStringList;
  LStartMonth      : integer;
  LEndMonth        : integer;
  LMonthIndex      : integer;
  lMonthDays       : array [1..12] of double;
  LDaysTotal       : double;
  LValueTotal      : double;
begin
  Result := NullFloat;
  try
    if(AElement = nil) then Exit;
    LSourceContainer := TStringList.Create;
    try
      if GetBlockData(LSourceContainer,AElement.BlockType, AElement.ElementID,LErrors) then
      begin
        LTempContainer := TStringList.Create;
        LDestContainer := TStringList.Create;
        try
          if(LSourceContainer.Count > 0) then
          begin
            //put all values in a stringlist one value per row(not comma texted)
            for LCount := 0  to LSourceContainer.Count-1 do
            begin
              LTempContainer.CommaText := LSourceContainer[LCount];
              for LIndex := 1 to 12 do
              begin
                LDestContainer.Add(LTempContainer[LIndex]);
              end;
            end;
            LStartMonth := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.ModelMonthPeriodElapsed[AStartDate];
            LEndMonth   := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.ModelMonthPeriodElapsed[AEndDate];
            LMonthIndex := MonthOf(AStartDate);
            for LIndex := 1 to 12 do
            begin
              lMonthDays[LMonthIndex] := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData.MonthDaysByIndex[LIndex];
              LMonthIndex := LMonthIndex + 1;
              if(LMonthIndex > 12) then
                LMonthIndex := 1;
            end;

            Lindex       := LStartMonth;
            LDaysTotal   := 0.0;
            LValueTotal  := 0.0;
            LMonthIndex := MonthOf(AStartDate);
            while(Lindex < LDestContainer.Count) and (Lindex <= LEndMonth) do
            begin
              LValueTotal := LValueTotal + (StrToFloat(LDestContainer[Lindex]) * lMonthDays[LMonthIndex]);
              LDaysTotal  := LDaysTotal  + lMonthDays[LMonthIndex];
              LMonthIndex := LMonthIndex + 1;
              if(LMonthIndex > 12) then
                LMonthIndex := 1;
              Lindex := Lindex + 1;
            end;
            if(LDaysTotal > 0) then
              LValueTotal := LValueTotal/LDaysTotal;
            Result := LValueTotal;
          end;
        finally
          LTempContainer.Free;
          LDestContainer.Free;
        end;
      end
    finally
      LSourceContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockMonthlyValue(AMonthNumber: integer;AElement:TSummaryOutputDataElement): double;
const OPNAME = 'TSummaryOutputData.GetBlockMonthlyValue';
var
  LValues   : TStringList;
  //LAgent    : TSumOutBlobFileAgent;
begin
  Result := NullFloat;
  try
    if(AElement = nil) then Exit;
    if(AMonthNumber <= 0)  then Exit;

    {if (FSummaryOutputDataSource.CurrentSource in [sodsBlobFile,sodsBlobDatabase]) then
    begin
       LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        Result := LAgent.GetBlockMonthlyValue(FSumOutBlob,AMonthNumber,AElement);
      finally
        LAgent.Free;
      end;
    end
    else}
    if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
    begin
      LValues := TStringList.Create;
      try
        LoadValuesLinesValuesIntoAStringList(LValues,AElement);
        if(AMonthNumber <= LValues.Count) then
           Result := StrToFloat(LValues[AMonthNumber-1]);
      finally
        LValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetDataElement(ABlockType:TOutputDataType;ALoadCaseNumber,ASequenceNumber,AElementID: integer): TSummaryOutputDataElement;
const OPNAME = 'TSummaryOutputData.GetDataElement';
var
  LIndex   : integer;
  LElement :TSummaryOutputDataElement;
begin
  Result := nil;
  try
    for LIndex := 0  to FDBItemsList.Count -1 do
    begin
      LElement := TSummaryOutputDataElement(FDBItemsList.Items[LIndex]);
      if (LElement.FBlockType      = ABlockType) and
         (LElement.LoadCaseNumber  = ALoadCaseNumber) and
         (LElement.SequenceNumber  = ASequenceNumber) and
         (LElement.ElementID       = AElementID) then
      begin
        Result := LElement;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetElementsByElementDataType(AElementDataType: TOutputDataType;
                                      var ADataContainer: WideString): WordBool; safecall;
const OPNAME = 'TSummaryOutputData.GetElementsByElementDataType';
var
  LIndex : integer;
  LElement: TSummaryOutputDataElement;
  LDataContainer : TStringList;
begin
  Result := False;
  try
    ADataContainer := '';
    LDataContainer := TStringList.Create;
    try
      for LIndex := 0  to FDBItemsList.Count -1 do
      begin
        LElement := TSummaryOutputDataElement(FDBItemsList.Items[LIndex]);
        if (LElement <> nil) and (Ord(AElementDataType) = Ord(LElement.BlockType)) then
          LDataContainer.Add(LElement.BlockTitle+','+IntToStr(LElement.FElementID));
      end;
      ADataContainer := LDataContainer.CommaText;
    finally
      LDataContainer.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSummaryOutputData.GetBlockAvarageStorageChange(AElement:TSummaryOutputDataElement): double;
const OPNAME = 'TSummaryOutputData.GetBlockAvarageStorageChange';
var
  LStartStorage : double;
  LEndStorage   : double;
  LValues       : TStringList;
  //LAgent        : TSumOutBlobFileAgent;
  LErrors       : string;
begin
  Result := NullFloat;
  try
    if(AElement = nil) then Exit;

    {if (FSummaryOutputDataSource.CurrentSource in [sodsBlobFile,sodsBlobDatabase]) then
    begin
       LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        Result := LAgent.GetBlockAvarageStorageChange(FSumOutBlob,AElement);
      finally
        LAgent.Free;
      end;
    end
    else}
    if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
    begin
      LValues := TStringList.Create;
      try
        LoadValuesLinesValuesIntoAStringList(LValues,AElement);
        LStartStorage := StrToFloat(LValues[0]);
        LEndStorage   := StrToFloat(LValues[LValues.Count-1]);
        Result        := LEndStorage - LStartStorage;
      finally
        LValues.Free;
      end;
    end
    else if (FSummaryOutputDataSource.CurrentSource = sodsPltFile) then
    begin
      LValues := TStringList.Create;
      try
        GetBlockData(LValues,AElement.BlockType, AElement.ElementID,LErrors);
        LStartStorage := StrToFloat(LValues[0]);
        LEndStorage   := StrToFloat(LValues[LValues.Count-1]);
        Result        := LEndStorage - LStartStorage;
      finally
        LValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockData(ADataContainer: TStrings; ADataType:TOutputDataType;
         ANetworkElementID: integer;var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetBlockData';
var
  LElement     : TSummaryOutputDataElement;
  LMonthDataStr,
  LLineData    : string;
  LIndex       : Integer;
  LIndex1      : integer;
  LMonthData   : double;
  LObjectlist  : TObjectlist;
  LValueLines  : TValuesLine;
  //LAgent       : TSumOutBlobFileAgent;
begin
  Result := False;
  try
    AErrors := '';

    if(ADataContainer = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedDataContainer');
      Exit;
    end;
    ADataContainer.Clear;

    if(ADataType = btNone) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedDataType');
      Exit;
    end;

    LElement := GetDataElement(ADataType,FDataSelection.LoadCase,FDataSelection.Sequence,ANetworkElementID);
    if (LElement = nil) and (FSummaryOutputDataSource.CurrentSource = sodsPltFile) then
    begin
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastPlottingOutputData.GetDataByElement(ADataType,ANetworkElementID,ADataContainer,AErrors);
      Exit;
    end;

    if (LElement = nil) then
    begin
     AErrors := FAppModules.Language.GetString('ErrorString.SelectedNetworkElementNotIncluded');
     Exit;
    end;

    {if (FSummaryOutputDataSource.CurrentSource in [sodsBlobFile,sodsBlobDatabase]) then
    begin
      LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        Result := LAgent.GetBlockDataFromBlob(FSumOutBlob,ADataContainer,LElement);
        if(ADataContainer.Count = 0) then
         AErrors := FAppModules.Language.GetString('ErrorString.SelectedNetworkElementNotIncluded');
      finally
        LAgent.Free;
      end;
    end
    else}
    if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
    begin
      LObjectlist := LElement.FValuesLines;
      if LObjectlist <> nil then
      begin
        for Lindex := 0 to LElement.FValuesLines.Count -1 do
        begin
          LValueLines := TValuesLine(LObjectlist.Items[Lindex]);
          LLineData := '';
          for Lindex1 := 1 to 14 do
          begin
            LMonthData  := LValueLines.FValues[LIndex1].FData;
            if LIndex1 = 1 then
              LMonthDataStr := FormatFloat('###0000.###',LMonthData)
            else
              LMonthDataStr := FormatFloat('######0.000',LMonthData);

            if LIndex1 = 14 then
              LLineData := LLineData + LMonthDataStr
            else
              LLineData := LLineData + LMonthDataStr + ',';
          end;
          ADataContainer.Add(LLineData);
        end;
        Result := True;
      end;
    end
    else
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.ChannelSUMOutFileNotImported');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSummaryOutputData.GetLongtermSupplyData(ADataType:TOutputDataType;ANetworkElementID: integer;ADataStringList: TStringList;  var AErrors: string;var AActualDemand : double;AFixed : boolean = True): boolean;
const OPNAME = 'TSummaryOutputData.GetLongtermSupplyData';
var
  LFileNameObject : TAbstractModelFileName;
  LInputFile: TSumOutFile;
  LBLockData: TDataBlock;
  LBlockNumber: integer;
  LSumOutAnualFirmEnergyDemandBlockValues  : TSumOutAnualFirmEnergyDemandBlockValues;
  LChannel : IGeneralFlowChannel;
  LBlockSkipped      : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try
    LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.GetSumOutFile;
    if not FileExists(LFileNameObject.FileName) then Exit;
    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LChannel := (FAppModules.Model.ModelData as IYieldModelData).
                 NetworkElementData.ChannelList.ChannelByChannelNumber[ANetworkElementID];
    try
      if LInputFile.OpenTheFile(LFileNameObject.FileName,ofaRead) then
      begin
        LInputFile.GetCurrentLine;
        LBlockNumber := 0;
        while not LInputFile.EOF do
        begin
          if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then Break;
          if LBlockSkipped then Continue;
          LBlockNumber := LBlockNumber + 1;
          LBLockData.BlockNumber := LBlockNumber;
          Result := True;

          if (LBLockData.BlockType = btAnualFirmSelectedYieldDemands)or
             ((LChannel.MasterControlFeature<> nil) and (LBLockData.BlockType = btAnualFirmYieldDemands)) then
          begin
            LSumOutAnualFirmEnergyDemandBlockValues  := TSumOutAnualFirmEnergyDemandBlockValues.Create(FAppModules);
            try
              if LSumOutAnualFirmEnergyDemandBlockValues.ReadFromBlockValues(ANetworkElementID,LBLockData) then
              begin
                Result := Result and CalcRankLongtermSupply(LSumOutAnualFirmEnergyDemandBlockValues,ADataStringList,AActualDemand,AFixed);
                if Result then
                  Exit;
              end;
            finally
             FreeAndNil(LSumOutAnualFirmEnergyDemandBlockValues);
            end;
          end
        end;
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
    Result := (ADataStringList.Count>0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.CalcRankLongtermSupply(AAnualFirmSelectedDemand : TSumOutAnualFirmEnergyDemandBlockValues;ADataStringList : TStrings;var AActualDemand : double;AFixed : boolean): boolean;
const OPNAME = 'TSummaryOutputData.CalcRankLongtermSupply';
var
  LCount : integer;
  LLineData : TStringList;
  LSortedLineData : TStringList;
  function AddSequences : boolean;
  var
    LIndex : integer;
    LAnualSummaryValuesLine : TAnualSummaryValuesLine;
    LValue : double;
  begin
    for LIndex := 0 to AAnualFirmSelectedDemand.FAnualValues.Count-1 do
    begin
      LAnualSummaryValuesLine := TAnualSummaryValuesLine(AAnualFirmSelectedDemand.FAnualValues.AnualSummaryValuesLine[LIndex]);
      if LAnualSummaryValuesLine <> nil then
      begin

        if (AFixed) then
          LValue := (1-LAnualSummaryValuesLine.FAnualSummaryValuesLine[LCount].FData)*AActualDemand
        else
          LValue := 100*(1-LAnualSummaryValuesLine.FAnualSummaryValuesLine[LCount].FData);

        if (FDataSelection.FUnits = ouPercentage) and (AFixed) then
          LValue :=  LValue/AActualDemand * 100;

        if (FDataSelection.FUnits = ouMegaLitersPerDay) and (AFixed)  then
          LValue := LValue * (365.25 * 86400.0) / 1000.0;
        if (FDataSelection.FUnits = ouMcmPerMonthOrYear) and (AFixed) then
          LValue := LValue * (365.25 * 86400.0) /1000000.0;
        LLineData.Add(FormatFloat('00000000000000.0000',LValue));
      end;
    end;
    for LIndex := LLineData.Count-1 downto 0 do
      LSortedLineData.Add(LLineData[LIndex]);
    Result := True;
  end;
begin
  Result := false;
  try
    LLineData := TStringList.Create;
    LSortedLineData := TStringList.Create;
    LLineData.Sorted := True;
    LLineData.Duplicates := dupAccept;
    try
      for LCount := 0 to 9 do
      begin
        LLineData.Clear;
        LSortedLineData.Clear;
        if AAnualFirmSelectedDemand.FActualDemand[LCount].FInitalised then
        begin
          AActualDemand := AAnualFirmSelectedDemand.FActualDemand[LCount].FData;
          if AddSequences then
          begin
            LSortedLineData.Insert(0,FloatToStr(AActualDemand));
            ADataStringList.Add(LSortedLineData.CommaText);
          end;
        end;
      end;
    finally
      LLineData.Free;
      LSortedLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
                                                   ANetworkElementID: Integer; var AErrors: WideString): WordBool;
const OPNAME = 'TSummaryOutputData.GetBlockDataByElementID';
var
  LData : TStringList;
  LErrors : string;
begin
  Result := False;
  try
    LData := TStringList.Create;
    try
      ADataContainer := '';
      Result := GetBlockData(LData,ADataType,ANetworkElementID,LErrors);
      AErrors := LErrors;
      ADataContainer := LData.CommaText;
    finally
      FreeandNil(LData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockAverageDataByElementID( var ADataContainer: WideString; ADataType: TOutputDataType;
  ANetworkElementID: Integer; var AErrors: WideString): WordBool;
const OPNAME = 'TOutputData.GetBlockAverageDataByElementID';
var
  lElement : TSummaryOutputDataElement;
  lValue   : double;
begin
  Result := FALSE;
  try
    ADataContainer  := '';
    lElement := GetDataElement(ADataType, FDataSelection.LoadCase,FDataSelection.Sequence, ANetworkElementID);
    if (lElement <> nil) then
    begin
      if (FDataSelection.AverageType = oatSequence) then
        lValue := GetBlockAvarageSequenceValue(lElement)
      else
        lValue := GetBlockAveragePeriodValue(FDataSelection.FAverageStartDate,
                                                                FDataSelection.FAverageEndDate, lElement);

      ADataContainer := FormatOutputValue(ADataType,lValue);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetPeriodChangeDataByElementID(var ADataContainer: WideString; ADataType: TOutputDataType;
         ANetworkElementID: Integer; var AErrors: WideString): WordBool;
const OPNAME = 'TOutputData.GetPeriodChangeDataByElementID';
var
  lElement     : TSummaryOutputDataElement;
  lStartValue  : double;
  lEndValue    : double;
  LStartMonth  : integer;
  LEndMonth    : integer;
  LStartDate   : TDateTime;
  LConfigData  : IRunConfigurationData;
begin
  Result := FALSE;
  try
    ADataContainer := '';
    lElement := GetDataElement(ADataType, FDataSelection.LoadCase,
                                                  FDataSelection.Sequence, ANetworkElementID);
    if (lElement <> nil) then
    begin
      LConfigData    := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (FDataSelection.AverageType = oatSequence) then
      begin
        LStartMonth  := 1;
        LEndMonth    := LConfigData.PeriodsInAnalysis;
      end
      else
      begin
        LStartDate   := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.CalenderStartDate[0];
        LStartMonth  := NumberOfMonthsBetween(LStartDate,FDataSelection.AverageStartDate) + 1;
        LEndMonth    := NumberOfMonthsBetween(LStartDate,FDataSelection.AverageEndDate) + 1;
      end;

      lStartValue      := GetBlockMonthlyValue(LStartMonth,lElement);
      lEndValue        := GetBlockMonthlyValue(LEndMonth,lElement);
      ADataContainer   := FormatOutputValue(ADataType, Abs(lEndValue - lStartValue));
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSummaryOutputData.GetChannelAreaData(ADataSource: TStrings; ADataType:TOutputDataType;
                            ANetworkElementID: integer;var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetChannelAreaData';
var
  LIndex                : integer;
  LIndexA               : integer;
  LCount                : integer;
  LIdentifier           : integer;

  LSourceValue          : double;
  LDestinationValue     : double;

  LTempData          : TStringList;
  LDestination       : TStringList;
  LSource            : TStringList;
  LLineData          : TStringList;

  LChannel              : IGeneralFlowChannel;
  LChannelList          : IChannelList;
begin
  Result := False;
  try
    LTempData    := TStringList.Create;
    LSource      := TStringList.Create;
    LDestination := TStringList.Create;
    LLineData    := TStringList.Create;
    ADataSource.Clear;
    try
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                      .ChannelList;
      for LIndex := 0 to LChannelList.ChannelCount -1 do
      begin
        LChannel := LChannelList.ChannelByIndex[LIndex];
        if LChannel <> nil then
        begin
          if LChannel.ChannelArea = ANetworkElementID then
          begin
            LIdentifier := LChannel.ChannelNumber;
            if GetBlockData(LTempData,ADataType,LIdentifier,AErrors) then
            begin
              if(ADataSource.Count = 0) then
              begin
                 ADataSource.AddStrings(LTempData);
              end
              else
              begin
                for LCount := 0 to ADataSource.Count-1 do
                begin
                   LSource.CommaText := LTempData[LCount];
                   LDestination.CommaText := ADataSource[LCount];

                   LLineData.Clear;
                   LLineData.Add(LSource[0]);
                   for LIndexA := 1 to LSource.Count-1 do
                   begin
                     LSourceValue      := StrToFloat(LSource[LIndexA]);
                     LDestinationValue := StrToFloat(LDestination[LIndexA]);
                     LLineData.Add(FormatFloat('##0.000',LSourceValue+LDestinationValue));
                   end;
                   ADataSource[LCount] := LLineData.CommaText;
                end;
              end;
            end;
            Result := true;
          end;
        end;
      end;
    finally
      LTempData.Free;
      LSource.Free;
      LLineData.Free;
      LDestination.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBoxPlotData(ADataContainer: TStrings;ADataType: TOutputDataType;
         ANetworkElementID : integer;const ALoadCase: integer; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetBoxPlotData';
var
  LElement              : TSummaryOutputDataElement;
  LLineData             : string;
  LBlockNumber          : integer;
  LPos                  : integer;
  LYear                 : integer;
  LStartMonth           : integer;
  LElementIndex         : integer;
  LIndex                : integer;
  LMonthData            : string;
  LElementList          : TObjectList;
  LMonthNumbers         : array[0..11] of integer;
  LDate                 : TDateTime;
  LValues               : TStringList;
  LCount                : integer;
  LYieldModelData       : IYieldModelData;
  LChangeYear           : boolean;
begin
  Result := False;
  try
    AErrors := '';
    LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);

    if(FDBItemsList.Count = 0) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.ChannelSUMOutFileNotImported')
    end;

    if(ADataContainer = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedDataContainer');
      Exit;
    end;

    ADataContainer.Clear;

    if(ADataType = btNone) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedDataType');
      Exit;
    end;

    LElementList := TObjectList.Create(False);
    try
      LElement := nil;
      for LElementIndex := 0 to FDBItemsList.Count -1 do
      begin
        LElement := TSummaryOutputDataElement(FDBItemsList.Items[LElementIndex]);
        if (LElement.FBlockType      = ADataType) and
           (LElement.ElementID       = ANetworkElementID) and
           (LElement.LoadCaseNumber  = ALoadCase) then
        begin
          LElementList.Add(LElement);
        end;
      end;

      if (LElement = nil) and (FSummaryOutputDataSource.CurrentSource = sodsPltFile) then
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.
                  CastPlottingOutputData.GetBoxPlotDataByElement(ADataType,ANetworkElementID,ADataContainer,AErrors);
        Exit;
      end;

      if (LElementList.Count = 0) then
      begin
       AErrors := FAppModules.Language.GetString('ErrorString.SelectedNetworkElementNotIncluded');
       Exit;
      end;

      LStartMonth := LYieldModelData.RunConfigurationData.StartMonthNumber + FAppModules.StudyArea.CalendarStartMonth - 1;
      if(LStartMonth > 12) then
         LStartMonth := LStartMonth - 12;

      for LIndex := 0 to 11 do
      begin
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
        LStartMonth := LStartMonth + 1;
      end;

      {if (FSummaryOutputDataSource.CurrentSource in [sodsBlobFile,sodsBlobDatabase]) then
      begin
        LBlockNumber      := 0;
        for LElementIndex := 0 to LElementList.Count -1 do
        begin
          LElement := TSummaryOutputDataElement(LElementList.Items[LElementIndex]);
          LValues := TStringList.Create;
          try
            LPos := -1;
            LBlockNumber := LBlockNumber + 1;
            LoadValuesBlobWithDateValuesIntoAStringList(LValues,LElement);
            for LCount := 0 to (LValues.Count div 12)-1 do
            begin
              LChangeYear := False;
              for LIndex := 0 to 11 do
              begin
                LPos := LPos + 1;
                if(LPos <= ADataContainer.Count) and (LBlockNumber <= 1)  then
                begin
                  LYear := StrToInt(copy(LValues[LPos],1,pos(':',LValues[LPos])-1));
                  if LChangeYear then
                    LYear := LYear + 1;
                  LDate := EncodeDate(LYear,LMonthNumbers[LIndex],1);
                  ADataContainer.Add(DateToStr(LDate));
                end;
                LMonthData := Copy(LValues[LPos],pos(':',LValues[LPos])+1,length(LValues[LPos]));
                LLineData  := ADataContainer[LPos];
                LLineData  := LLineData + ',' + LMonthData;
                ADataContainer.Strings[LPos] := LLineData;
                if(LMonthNumbers[LIndex] = 12) then
                  LChangeYear := True;
              end;
            end;
            Result := True
          finally
            LValues.Free;
          end;
        end;
      end
      else}
      if (FSummaryOutputDataSource.CurrentSource = sodsSumFile) then
      begin
        LBlockNumber      := 0;
        for LElementIndex := 0 to LElementList.Count -1 do
        begin
          LElement := TSummaryOutputDataElement(LElementList.Items[LElementIndex]);
          LValues := TStringList.Create;
          try
            LPos := -1;
            LBlockNumber := LBlockNumber + 1;
            LoadValuesLinesWithDateValuesIntoAStringList(LValues,LElement);
            for LCount := 0 to (LValues.Count div 12)-1 do
            begin
              LChangeYear := False;
              for LIndex := 0 to 11 do
              begin
                LPos := LPos + 1;
                if(LPos <= ADataContainer.Count) and (LBlockNumber <= 1)  then
                begin
                  LYear := StrToInt(copy(LValues[LPos],1,pos(':',LValues[LPos])-1));
                  if LChangeYear then
                    LYear := LYear + 1;
                  LDate := EncodeDate(LYear,LMonthNumbers[LIndex],1);
                  ADataContainer.Add(DateToStr(LDate));
                end;
                LMonthData := Copy(LValues[LPos],pos(':',LValues[LPos])+1,length(LValues[LPos]));
                LLineData  := ADataContainer[LPos];
                LLineData  := LLineData + ',' + LMonthData;
                ADataContainer.Strings[LPos] := LLineData;
                if(LMonthNumbers[LIndex] = 12) then
                  LChangeYear := True;
              end;
            end;
            Result := True
          finally
            LValues.Free;
          end;
        end;
      end;
    finally
      LElementList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetChannelAreaBoxPlotData(ADataContainer: TStrings; ADataType: TOutputDataType;
                                                      ANetworkElementID : integer; const ALoadCase: integer;
                                                      var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetChannelAreaBoxPlotData';
var
  LChannelList              : IChannelList;
  LChannel                  : IGeneralFlowChannel;

  LChannelIndex             : integer;
  LIndexA                   : integer;
  LCount                    : integer;
  LIdentifier               : integer;

  LSourceValue              : double;
  LDestinationValue         : double;

  LTempData          : TStringList;
  LDestination       : TStringList;
  LSource            : TStringList;
  LLineData          : TStringList;
begin
  Result := False;
  try
    LTempData    := TStringList.Create;
    LSource      := TStringList.Create;
    LDestination := TStringList.Create;
    LLineData    := TStringList.Create;
    ADataContainer.Clear;
    LChannelList     := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    try
      for LChannelIndex := 0 to LChannelList.ChannelCount - 1 do
      begin
        LChannel := LChannelList.ChannelByIndex[LChannelIndex];
        if(LChannel <> nil) then
        begin
          if(LChannel.ChannelArea = ANetworkElementID) then
          begin
            LIdentifier := LChannel.ChannelNumber;
            if(GetBoxPlotData(LTempData,ADataType,LIdentifier,ALoadCase,AErrors) ) then
            begin
              if(ADataContainer.Count = 0) then
              begin
                 ADataContainer.AddStrings(LTempData);
              end
              else
              begin
                for LCount := 0 to ADataContainer.Count-1 do
                begin
                   LSource.CommaText := LTempData[LCount];
                   LDestination.CommaText := ADataContainer[LCount];

                   LLineData.Clear;
                   LLineData.Add(LSource[0]);
                   for LIndexA := 1 to LSource.Count-1 do
                   begin
                     LSourceValue      := StrToFloat(LSource[LIndexA]);
                     LDestinationValue := StrToFloat(LDestination[LIndexA]);
                     LLineData.Add(FormatFloat('##0.000',LSourceValue+LDestinationValue));
                   end;
                   ADataContainer[LCount] := LLineData.CommaText;
                end;
              end;
            end;
            Result := True;
          end;
        end
      end;
    finally
      LTempData.Free;
      LSource.Free;
      LLineData.Free;
      LDestination.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetChannelAreaComplianceGridData(ADataContainer, AComplyContainer: TStrings;
                                                             ALoadCaseNumber, ASequenceNumber, ANetworkElementID: integer;
                                                             var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetChannelAreaComplianceGridData';
var
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;

  LChannelIndex             : integer;
  LIndexA                   : integer;
  LCount                    : integer;
  LIdentifier               : integer;

  LSourceValue              : double;
  LDestinationValue         : double;

  LTempData          : TStringList;
  LDestination       : TStringList;
  LSource            : TStringList;
  LLineData          : TStringList;
begin
  Result := False;
  try
    LTempData    := TStringList.Create;
    LSource      := TStringList.Create;
    LDestination := TStringList.Create;
    LLineData    := TStringList.Create;
    ADataContainer.Clear;
    try
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      for LChannelIndex := 0 to LChannelList.ChannelCount - 1 do
      begin
        LChannel := LChannelList.ChannelByIndex[LChannelIndex];
        if(LChannel <> nil) then
        begin
          if(LChannel.ChannelArea = ANetworkElementID) then
          begin
            if(LChannel.ChannelType in [2,8,11]) then
            begin
              LIdentifier := LChannel.ChannelNumber;
              if(GetComplianceGridBlockData(LTempData,AComplyContainer,ALoadCaseNumber,ASequenceNumber,LIdentifier,AErrors)) then
              begin
                if(ADataContainer.Count = 0) then
                begin
                   ADataContainer.AddStrings(LTempData);
                end
                else
                begin
                  for LCount := 0 to ADataContainer.Count-1 do
                  begin
                     LSource.CommaText := LTempData[LCount];
                     LDestination.CommaText := ADataContainer[LCount];

                     LLineData.Clear;
                     LLineData.Add(LSource[0]);
                     for LIndexA := 1 to LSource.Count-1 do
                     begin
                       LSourceValue      := StrToFloat(LSource[LIndexA]);
                       LDestinationValue := StrToFloat(LDestination[LIndexA]);
                       LLineData.Add(FormatFloat('##0.000',LSourceValue+LDestinationValue));
                     end;
                     ADataContainer[LCount] := LLineData.CommaText;
                  end;
                end;
              end;
              Result := True;
            end;
          end
        end;
      end;
    finally
      LTempData.Free;
      LSource.Free;
      LLineData.Free;
      LDestination.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetChannelAreaMonthlyWaterBalancelData(AChannelAreaNumber, ALoadCaseNumber,
                                                                          ASequenceNumber, AMonthNumber: integer;
                                                                          AWaterBalanceData: TStrings;
                                                                          var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetChannelAreaMonthlyWaterBalancelData';
var
  LChannelIndex                         : integer;
  LChannelList                          : IChannelList;
  LChannel                              : IGeneralFlowChannel;
  LChannelIdentifier                    : integer;
  LChannelAreaMonthlyWaterBalancelTotal : double;
begin
  Result := False;
  try
    LChannelAreaMonthlyWaterBalancelTotal := 0.0;
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    for LChannelIndex := 0 to LChannelList.ChannelCount - 1 do
    begin
      LChannel := LChannelList.ChannelByIndex[LChannelIndex];
      if(LChannel <> nil) then
      begin
        if(LChannel.ChannelArea = AChannelAreaNumber) then
        begin
          LChannelIdentifier := LChannel.ChannelNumber;
          if(GetMonthlyChannelWaterBalancelData(LChannelIdentifier,ALoadCaseNumber,ASequenceNumber,AMonthNumber,AWaterBalanceData,AErrors)) then
          begin
            LChannelAreaMonthlyWaterBalancelTotal := LChannelAreaMonthlyWaterBalancelTotal + StrToFloat(UpperCase(Trim(AWaterBalanceData.Values['AVERAGE FLOW'])));
          end;
          Result := True;
        end;
      end;
    end;
    AWaterBalanceData.Clear;
    AWaterBalanceData.Add('Average flow='+FormatFloat('#####0.000',LChannelAreaMonthlyWaterBalancelTotal));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetChannelAreaAnnualWaterBalancelData(AChannelAreaNumber, ALoadCaseNumber, ASequenceNumber: integer;
                                                                  AWaterBalanceData: TStrings; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetChannelAreaAnnualWaterBalancelData';
var
  LChannelIndex                         : integer;
  LChannelList                          : IChannelList;
  LChannel                              : IGeneralFlowChannel;
  LChannelIdentifier                    : integer;
  LChannelAreaAnnualWaterBalancelTotal  : double;
begin
  Result := False;
  try
    LChannelAreaAnnualWaterBalancelTotal := 0.0;
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    for LChannelIndex := 0 to LChannelList.ChannelCount - 1 do
    begin
      LChannel := LChannelList.ChannelByIndex[LChannelIndex];
      if(LChannel <> nil) then
      begin
        if(LChannel.ChannelArea = AChannelAreaNumber) then
        begin
          LChannelIdentifier := LChannel.ChannelNumber;
          if(GetAnnualChannelWaterBalancelData(LChannelIdentifier,ALoadCaseNumber,ASequenceNumber,AWaterBalanceData,AErrors)) then
          begin
            LChannelAreaAnnualWaterBalancelTotal := LChannelAreaAnnualWaterBalancelTotal + StrToFloat(UpperCase(Trim(AWaterBalanceData.Values['AVERAGE FLOW'])));
          end;
          Result := True;
        end;
      end;
    end;
    AWaterBalanceData.Clear;
    AWaterBalanceData.Add('Average flow='+FormatFloat('#####0.000',LChannelAreaAnnualWaterBalancelTotal));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.LoadDatasetValuesIntoAStringList(ADataList: TStrings; ADataSet: TAbstractModelDataset);
const OPNAME = 'TSummaryOutputData.LoadDatasetValuesIntoAStringList';
var
  LValue,
  lFieldName: string;
  LIndex : integer;
begin
  try
    if(ADataList <> nil) and (ADataSet <> nil) then
    begin
      ADataList.Clear;
      ADataSet.DataSet.First;
      while not ADataSet.DataSet.Eof do
      begin
        for LIndex := 3 to 14 do
        begin
           LFieldName := Format('%s%2.2d',['GenericValue',LIndex]);
           LValue     := FormatFloat('#####0.000',ADataSet.DataSet.FieldByName(lFieldName).AsFloat);
           ADataList.Add(LValue);
        end;
        ADataSet.DataSet.Next;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.LoadValuesLinesValuesIntoAStringList(ADataList: TStrings; AElement: TSummaryOutputDataElement);
const OPNAME = 'TSummaryOutputData.LoadValuesLinesValuesIntoAStringList';
var
  LIndex        : integer;
  LIndex1       : integer;
  LMonthData    : double;
  LMonthDataStr : string;
  LObjectlist   : TObjectlist;
  LValueLines   : TValuesLine;
begin
  try
    ADataList.Clear;
    if(AElement <> nil) then
    begin
      LObjectlist := AElement.FValuesLines;
      if LObjectlist <> nil then
      begin
        for Lindex := 0 to AElement.FValuesLines.Count -1 do
        begin
          LValueLines := TValuesLine(LObjectlist.Items[Lindex]);
          for Lindex1 := 2 to 13 do
          begin
            LMonthData  := LValueLines.FValues[LIndex1].FData;
            LMonthDataStr := FormatFloat('######0.000',LMonthData);
            ADataList.Add(LMonthDataStr);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.LoadValuesLinesWithDateValuesIntoAStringList(ADataList: TStrings; AElement: TSummaryOutputDataElement);
const OPNAME = 'TSummaryOutputData.LoadValuesLinesWithDateValuesIntoAStringList';
var
  LIndex        : integer;
  LIndex1       : integer;
  LMonthData    : double;
  LMonthDataStr : string;
  LYearData     : double;
  LYearDataStr  : string;
  LObjectlist   : TObjectlist;
  LValueLines   : TValuesLine;
begin
  try
    ADataList.Clear;
    if(AElement <> nil) then
    begin
      LObjectlist := AElement.FValuesLines;
      if LObjectlist <> nil then
      begin
        for Lindex := 0 to AElement.FValuesLines.Count -1 do
        begin
          LValueLines := TValuesLine(LObjectlist.Items[Lindex]);
          for Lindex1 := 2 to 13 do
          begin
            LYearData := LValueLines.FValues[1].FData;
            LYearDataStr := FormatFloat('###0000.###',LYearData);
            LMonthData := LValueLines.FValues[LIndex1].FData;
            LMonthDataStr := LYearDataStr + ':'+ FormatFloat('######0.000',LMonthData);
            ADataList.Add(LMonthDataStr);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TSummaryOutputData.LoadValuesBlobWithDateValuesIntoAStringList(ADataList: TStrings; AElement: TSummaryOutputDataElement);
const OPNAME = 'TSummaryOutputData.LoadValuesBlobWithDateValuesIntoAStringList';
var
  LAgent      : TSumOutBlobFileAgent;
begin
  try
    ADataList.Clear;
    if(AElement <> nil) then
    begin
      LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        //LAgent.LoadValuesBlobWithDateValuesIntoAStringList(FSumOutBlob,ADataList,AElement);
      finally
        LAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TSummaryOutputData.GetBlockNextSignificantMonth(ABlockType: TOutputDataType;AElementID,ALoadCaseNumber,
         ASequenceNumber,ACurrentMonthNumber: integer;var AErrors: string): integer;
const OPNAME = 'TSummaryOutputData.GetBlockNextSignificantMonth';
var
  LValue   : string;
  LIndex   : integer;
  LValues  : TStringList;
begin
  Result := ACurrentMonthNumber;
  try
    LValues := TStringList.Create;
    try
      if GetBlockDataSet(btMonthlyAverageChannelFlow,AElementID,LValues,AErrors) then
      begin
        if(LValues.Count > 0) and (ACurrentMonthNumber <= LValues.Count) then
        begin
          if(ACurrentMonthNumber < 1) then
             ACurrentMonthNumber := 1;

          LIndex := ACurrentMonthNumber -1;
          LValue := LValues[LIndex];
          LIndex := LIndex + 1;
          for LIndex := LIndex to LValues.Count -1 do
          begin
            if(LValue <> LValues[LIndex]) then
            begin
              Result := LIndex+1;
              Break;
            end;
          end;
        end;
      end;
    finally
      LValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetBlockPreviousSignificantMonth(ABlockType: TOutputDataType;AElementID,ALoadCaseNumber,
         ASequenceNumber,ACurrentMonthNumber: integer;var AErrors: string): integer;
const OPNAME = 'TSummaryOutputData.GetBlockPreviousSignificantMonth';
var
  LValue  : string;
  LIndex  : integer;
  LValues  : TStringList;
begin
  Result := ACurrentMonthNumber;
  try
    LValues := TStringList.Create;
    try
      if GetBlockDataSet(ABlockType,AElementID,LValues,AErrors) then
      begin
        if(LValues.Count > 0) and (ACurrentMonthNumber <= LValues.Count) then
        begin
          if(ACurrentMonthNumber < 1) then
             ACurrentMonthNumber := 1;

          LIndex := ACurrentMonthNumber -1;
          LValue := LValues[LIndex];
          LIndex := LIndex - 1;
          for LIndex := LIndex downto 0 do
          begin
            if(LValue <> LValues[LIndex]) then
            begin
              Result := LIndex+1;
              Break;
            end;
          end;
        end;
      end;
    finally
      LValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetComplianceGridBlockData(ADataContainer, AComplyContainer: TStrings;
         ALoadCaseNumber, ASequenceNumber, ANetworkElementID: integer; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetComplianceGridBlockData';
var
  lChannel : IGeneralFlowChannel;
  LLoadCaseNumber,
  LSequenceNumber : integer;
begin
  Result := False;
  try
    lChannel :=
     TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[ANetworkElementID];
    if (lChannel <> nil) then
    begin
      if (lChannel.ChannelType in [2,8,11,14]) then
      begin
        LLoadCaseNumber := FDataSelection.LoadCase;
        LSequenceNumber := FDataSelection.Sequence;
        try
          FDataSelection.LoadCase := ALoadCaseNumber;
          FDataSelection.Sequence := ASequenceNumber;
          if GetBlockData(ADataContainer,btMonthlyAverageChannelFlow,ANetworkElementID,AErrors) then
          begin
             case lChannel.ChannelType of
              2 : Result := PopulateChannelMasterControlDeficits(ALoadCaseNumber,ADataContainer,AComplyContainer,lChannel,AErrors);
              8 :
                if lChannel.IFRFeature = nil then
                  Result := PopulateChannelMinMaxDeficits(ALoadCaseNumber,ADataContainer,AComplyContainer,lChannel,AErrors);

              11: Result := PopulateChannelDemandDeficits(ALoadCaseNumber,ADataContainer,AComplyContainer,lChannel,AErrors);
              14: Result := PopulateIrrigationChannelDeficits(ALoadCaseNumber,ADataContainer,AComplyContainer,lChannel,AErrors);
             end;
             if lChannel.IFRFeature <> nil then
               Result := PopulateChannelIFRDeficits(ALoadCaseNumber,ADataContainer,AComplyContainer,lChannel,AErrors);

          end;
        finally
          FDataSelection.LoadCase := LLoadCaseNumber;
          FDataSelection.Sequence := LSequenceNumber;
        end;
      end
      else
        AErrors := FAppModules.Language.GetString('ErrorString.ChannelTypeWarning');
    end
    else
      AErrors := 'There is no channel with channel number ('+IntToStr(ANetworkElementID)+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.PopulateChannelMasterControlDeficits(ALoadCaseNumber: integer;
         ADataContainer, AComplyContainer: TStrings;AChannel: IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.PopulateChannelMasterControlDeficits';
var
  LCount,
  LIndex: integer;
  LDeficits,
  LCompliance: TStringList;
  LFactor: double;
  LMonthFactor : double;
  LTotal: double;
  lTarget     : double;
  LValue: double;
  LAverage : double;
  LMonthlyFactors : array[0..12] of double;
  LApplySensitivity,
  LFoundDeficit : boolean;
begin
  Result := False;
  try
    if (AChannel.MasterControlFeature = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedMasterControlFeature');
      Exit;
    end;
    AComplyContainer.Clear;
    LDeficits   := TStringList.Create;
    LCompliance := TStringList.Create;
    try
      LTotal := 0.0;
      lTarget := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TargetYieldByIndex[ALoadCaseNumber];
      for LCount := 0 to 11 do
      begin
        LFactor := AChannel.MasterControlFeature.FactorByMonth[LCount+1];
        if(LFactor = NullFloat) then
        begin
          LMonthlyFactors[LCount] := 0.0;
        end
        else
        begin
          lValue  := lTarget * lFactor * 1000000 / 365.25 / 86400.0;
          LTotal  := LTotal + (lValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount+1]);
          LMonthlyFactors[LCount] := lValue;
        end;
      end;
      LMonthlyFactors[12] := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;

      for LCount := 0 to ADataContainer.Count -1 do
      begin
        LDeficits.CommaText := ADataContainer.Strings[LCount];
        LCompliance.Clear;
        LFoundDeficit := False;
        LTotal := 0.0;
        for LIndex := 0 to 13 do
        begin
          if(LIndex = 0) then
          begin
            LCompliance.Add('N');
            Continue;
          end;
          if (LIndex = 13) then
          begin
            if (LFoundDeficit) then
              LCompliance.Add('Y')
            else
              LCompliance.Add('N');
            LAverage := LTotal/12;
            LDeficits.Strings[LIndex] := FormatFloat('######0.000',LAverage);
            LFoundDeficit := False;
            Continue;
          end;

          LValue       := StrToFloat(LDeficits[LIndex]);
          LMonthFactor := StrToFloat(FormatFloat('######0.000', LMonthlyFactors[LIndex-1]));
          LApplySensitivity := False;
          if (FDataSelection.ApplySensitivity = stvAbsolute) then
            LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(LMonthFactor - LValue)))<=FDataSelection.Sensitivity)
          else
          if (FDataSelection.ApplySensitivity = stvPercentage) and (LMonthFactor <> 0) then
            LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(((LMonthFactor - LValue)/LMonthFactor)*100)))<=FDataSelection.FPercSensitivity);



          if not (LApplySensitivity) and (LValue < LMonthFactor) then
          begin

            LValue := LMonthlyFactors[LIndex-1] - LValue;
            LDeficits[LIndex] := FormatFloat('######0.000',LValue);
            LCompliance.Add('Y');
            LFoundDeficit := True;
          end
          else
          begin
            if LApplySensitivity then
              LValue := LMonthlyFactors[LIndex-1] - LValue
            else
              LValue := LValue - LMonthlyFactors[LIndex-1];
            LDeficits[LIndex] := FormatFloat('######0.000',LValue);
            LCompliance.Add('N');
          end;
          LTotal := LTotal + LValue;
        end;
        AComplyContainer.Add(LCompliance.CommaText);
        ADataContainer.Strings[LCount] := LDeficits.CommaText;
      end;
      Result := True;
    finally
      LDeficits.Free;
      LCompliance.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSummaryOutputData.PopulateChannelMinMaxDeficits(ALoadCaseNumber: integer;
         ADataContainer,AComplyContainer: TStrings;AChannel: IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.PopulateChannelMinMaxDeficits';
var
  LArcCount,
  LCount,
  LIndex: integer;
  LDeficits,
  LCompliance: TStringList;
  LMonthFactor : double;
  LFactor: double;
  LTotal: double;
  LValue: double;
  LAverage : double;
  LMonthlyFactors : array[0..12] of double;
  LApplySensitivity,
  LFoundDeficit : boolean;
begin
  Result := False;
  try
    if (AChannel.ChannelPenalty = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedPenaltyStructure');
      Exit;
    end;
    if (AChannel.MinMaxFlowConstraint = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedMinMaxFlowConstraint');
      Exit;
    end;
    AComplyContainer.Clear;
    LDeficits   := TStringList.Create;
    LCompliance := TStringList.Create;
    try
      LTotal := 0.0;
      LArcCount := AChannel.ChannelPenalty.ChannelPenaltyArcCount;
      for LCount := 0 to 11 do
      begin
        LFactor := 0;
        for LIndex := 1 to LArcCount do
        begin
          lValue := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[LIndex,LCount+1];
          if(lValue > LFactor) then
            LFactor := lValue;
        end;
        LMonthlyFactors[LCount] := LFactor;
        LTotal  := LTotal + (LFactor * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount+1]);
      end;
      LMonthlyFactors[12] := LTotal/TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
      for LCount := 0 to ADataContainer.Count -1 do
      begin
        LDeficits.CommaText := ADataContainer.Strings[LCount];
        LCompliance.Clear;
        LTotal := 0.0;
        LFoundDeficit := False;
        for LIndex := 0 to 13 do
        begin
          if(LIndex = 0) then
          begin
            LCompliance.Add('N');
            Continue;
          end;
          if (LIndex = 13) then
          begin
            if (LFoundDeficit) then
              LCompliance.Add('Y')
            else
              LCompliance.Add('N');
            LAverage := LTotal/12;
            LDeficits.Strings[LIndex] := FormatFloat('######0.000',LAverage);
            LFoundDeficit := False;
            Continue;
          end;
          LValue := StrToFloat(LDeficits.Strings[LIndex]);
          LMonthFactor := StrToFloat(FormatFloat('######0.000', LMonthlyFactors[LIndex-1]));

          LApplySensitivity := False;
          if (FDataSelection.ApplySensitivity = stvAbsolute) then
            LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(LMonthFactor - LValue)))<=FDataSelection.Sensitivity)
          else
          if (FDataSelection.ApplySensitivity = stvPercentage) and (LMonthFactor <> 0)then
            LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(((LMonthFactor - LValue)/LMonthFactor)*100)))<=FDataSelection.FPercSensitivity);


          if not (LApplySensitivity) and (LValue < LMonthFactor) then
          begin
            LValue := LMonthlyFactors[LIndex-1] - LValue;
            LDeficits.Strings[LIndex] := FormatFloat('######0.000',LValue);
            LFoundDeficit := True;
            LCompliance.Add('Y');
          end
          else
          begin
            if LApplySensitivity then
              LValue := LMonthlyFactors[LIndex-1] - LValue
            else
              LValue := LValue - LMonthlyFactors[LIndex-1];
            LDeficits.Strings[LIndex] := FormatFloat('######0.000',LValue);
            LCompliance.Add('N');
          end;
          LTotal := LTotal + LValue;
        end;
        AComplyContainer.Add(LCompliance.CommaText);
        ADataContainer.Strings[LCount] := LDeficits.CommaText;
      end;
      Result := True;
    finally
      LDeficits.Free;
      LCompliance.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.PopulateChannelIFRDeficits(ALoadCaseNumber: integer;ADataContainer, AComplyContainer: TStrings;AChannel : IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.PopulateChannelMinMaxDeficits';
var
  LCount,
  LIndex: integer;
  LDeficits,
  LCompliance: TStringList;
  LMonthFactor : double;
  LTotal: double;
  LValue: double;
  LNewValue{,
  LAverage} : double;
  LDemand,
  LChannelDemandValues : TStringList;
  LApplySensitivity,
  LFoundDeficit : boolean;
begin
  Result := False;
  try
    AComplyContainer.Clear;
    LDeficits   := TStringList.Create;
    LCompliance := TStringList.Create;
    LChannelDemandValues := TStringList.Create;
    LDemand := TStringList.Create;
    try

      if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDemandOutputData.GetChannelDemandValues(
               LChannelDemandValues,AChannel.ChannelNumber,AErrors) then
      if LChannelDemandValues.Count>0 then
      begin
        for LCount := 0 to ADataContainer.Count -1 do
        begin
          if LCount >= LChannelDemandValues.Count-1 then Exit;

          LDeficits.CommaText := ADataContainer.Strings[LCount];
          LDemand.CommaText := LChannelDemandValues[LCount];
          LCompliance.Clear;
          LTotal := 0.0;
          LFoundDeficit := False;
          for LIndex := 0 to 13 do
          begin
            if(LIndex = 0) then
            begin
              LCompliance.Add('N');
              Continue;
            end;
            {if (LIndex = 13) then
            begin
              if (LFoundDeficit) then
                LCompliance.Add('Y')
              else
                LCompliance.Add('N');
              LAverage := LTotal/12;
              LDeficits.Strings[LIndex] := FormatFloat('######0.000',LAverage);
              LFoundDeficit := False;
              Continue;
            end;}
            LValue := StrToFloat(LDeficits.Strings[LIndex]);
            LMonthFactor := StrToFloat(FormatFloat('######0.000', StrToFloat(LDemand[LIndex])));
            LNewValue    := LMonthFactor - LValue;
            LApplySensitivity := False;
            if (FDataSelection.ApplySensitivity = stvAbsolute) then
              LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(LMonthFactor - LValue)))<=FDataSelection.Sensitivity)
            else
            if (FDataSelection.ApplySensitivity = stvPercentage) and (LMonthFactor<>0) then
              LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(((LMonthFactor - LValue)/LMonthFactor)*100)))<=FDataSelection.FPercSensitivity);

            if LNewValue < 0 then
              LNewValue := 0;
            if not (LApplySensitivity) and (LValue < LMonthFactor) then
            begin
              LDeficits.Strings[LIndex] := FormatFloat('######0.000',LNewValue);
              LFoundDeficit := True;
              LCompliance.Add('Y');
            end
            else
            begin
              LDeficits.Strings[LIndex] := FormatFloat('######0.000',LNewValue);
              LCompliance.Add('N');
            end;
            LTotal := LTotal + LValue;
          end;
          AComplyContainer.Add(LCompliance.CommaText);
          ADataContainer.Strings[LCount] := LDeficits.CommaText;
        end;
      end;
      Result := True;
    finally
      LDeficits.Free;
      LCompliance.Free;
      LChannelDemandValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.PopulateChannelDemandDeficits(ALoadCaseNumber: integer;
         ADataContainer, AComplyContainer: TStrings;AChannel: IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.PopulateChannelDemandDeficits';
var
  LCount,
  LCount2,
  LIndex: integer;
  LInputYear,
  LOutputYear: integer;
  LDeficits,
  LInputDemands,
  LOutputDemands,
  LDemandFileData : TStringList;
  LInputValue,
  LValue,
  LOutputValue: double;
  LApplySensitivity : boolean;
begin
  Result := False;
  try
    if (AChannel.SpecifiedDemandFeature = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.UnassignedDemandFeature');
      Exit;
    end;

    AComplyContainer.Clear;
    LDemandFileData   := TStringList.Create;
    try
      GetSpecifiedDemandFeatureDataSet(LDemandFileData,AChannel,AErrors);
      if (LDemandFileData.Count = 0) then
      begin
        AErrors := FAppModules.Language.GetString('ErrorString.NoChannelDemandFileImported');
        GetSpecifiedDemandFeatureFileData(LDemandFileData,AChannel,AErrors);
      end;

      if (LDemandFileData.Count > 0) then
      begin
        LInputDemands  := TStringList.Create;
        LOutputDemands := TStringList.Create;
        LDeficits      := TStringList.Create;
        try
          LIndex := 0;
          LDemandFileData.Sort;
          TStringList(ADataContainer).Sort;
          for LCount := 0 to ADataContainer.Count -1 do
          begin
            LOutputDemands.CommaText := ADataContainer.Strings[LCount];
            LInputDemands.CommaText  := LDemandFileData.Strings[LIndex];
            LInputYear  := StrToInt(LInputDemands.Strings[0]);
            LOutputYear := StrToInt(LOutputDemands.Strings[0]);

            while (LInputYear < LOutputYear) do
            begin
              LIndex := LIndex + 1;
              LInputDemands.CommaText  := LDemandFileData.Strings[LIndex];
              LInputYear  := StrToInt(LInputDemands.Strings[0]);
            end;

            if(LInputYear > LOutputYear) then
            begin
              LDeficits.CommaText := 'N,Y,Y,,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y';
              for LCount2 := 1 to LOutputDemands.Count -1 do
                LOutputDemands.Strings[LCount2] := 'No file data';
            end
            else
            begin
              LDeficits.Clear;
              LDeficits.Add('N');
              for LCount2 := 1 to LOutputDemands.Count -1 do
              begin

                LInputValue  := StrToFloat(LInputDemands.Strings[LCount2]);
                LOutputValue := StrToFloat(LOutputDemands.Strings[LCount2]);

                LApplySensitivity := False;
                if (FDataSelection.ApplySensitivity = stvAbsolute) then
                  LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(LInputValue - LOutputValue)))<=FDataSelection.Sensitivity)
                else
                if (FDataSelection.ApplySensitivity = stvPercentage) and (LInputValue<>0) then
                  LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(((LInputValue - LOutputValue)/LInputValue)*100)))<=FDataSelection.FPercSensitivity);

                if(LOutputValue < LInputValue) then
                begin
                  LValue := LInputValue - LOutputValue;
                  LOutputDemands.Strings[LCount2] := FormatFloat('######0.000',LValue);
                  LDeficits.Add('Y');
                end
                else
                begin
                  if LApplySensitivity then
                    LValue := LInputValue - LOutputValue
                  else
                    LValue := LOutputValue - LInputValue;

                  LOutputDemands.Strings[LCount2] := FormatFloat('######0.000',LValue);
                  LDeficits.Add('N');
                end;
              end;
              LIndex := LIndex + 1;
            end;
            ADataContainer.Strings[LCount] := LOutputDemands.CommaText;
            AComplyContainer.Add(LDeficits.CommaText);
          end;

        finally
          LInputDemands.Free;
          LOutputDemands.Free;
          LDeficits.Free;
        end;
        Result := True;
      end;
    finally
      LDemandFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.PopulateIrrigationChannelDeficits(ALoadCaseNumber: integer; ASupplyContainer, ADeficitOutputContainer: TStrings;
                                                              AChannel: IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.PopulateIrrigationChannelDeficits';
var
  LIndex,
  LInnerIndex                 : integer;
  LTotal,
  LAverage,
  LDemandLimitValue,
  LDemandValue,
  LSupplyValue,
  LNewValue                   : double;
  LDemandLineData,
  LSupplyLineData,
  LComplianceContainer,
  LChannelDemandContainer,
  LDemandContainer            : TStringList;
  LApplySensitivity,
  LFoundDeficit               : Boolean;
  LIrrigationBlock            : TIrrigationBlock;
  LIrrigationBlockList        : TIrrigationBlockList;
  LFileAgent                  : TDemandOutputFileAgent;
  LChannelNumber,
  LSequenceCount,
  LLoadCaseCount,
  LYearInAnalysis,
  LCurrentSequence,
  LCurrentLoadCase            : integer;
  LIrrigationBlockName        : string;
  LDataSelection              : IOutputDataSelection;
  LYieldModelData : IYieldModelData;
begin
  Result := False;
  try
    LDemandLineData         := TStringList.Create;
    LSupplyLineData         := TStringList.Create;
    LDemandContainer        := TStringList.Create;
    LChannelDemandContainer := TStringList.Create;
    LComplianceContainer    := TStringList.Create;
    try
      LIrrigationBlockList        := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIrrigationBlockList;
      LIrrigationBlock            := LIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(AChannel.DownStreamNodeNumber);
      LDataSelection              := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
      LYieldModelData             := (FAppModules.Model.ModelData as IYieldModelData);
      if(LIrrigationBlock  <> nil) and (LDataSelection <> nil) then
      begin
        LFileAgent := TDemandOutputFileAgent.Create(FAppModules);
        try
          LChannelNumber             := AChannel.ChannelNumber;
          LSequenceCount             := LYieldModelData.RunConfigurationData.NumberOfSequencesInAnalysis;
          LLoadCaseCount             := LYieldModelData.RunConfigurationData.NrOfActiveLoadCases;
          LYearInAnalysis            := LYieldModelData.RunConfigurationData.YearsInAnalysis;
          LCurrentSequence           := LDataSelection.Sequence;
          LCurrentLoadCase           := LDataSelection.LoadCase;
          LIrrigationBlockName       := LIrrigationBlock.BlockName;
          if not LFileAgent.ReadChannelDataCommaText(LChannelNumber,LSequenceCount,LLoadCaseCount,LYearInAnalysis,LCurrentSequence,LCurrentLoadCase,LIrrigationBlockName,LChannelDemandContainer) then
            AErrors := FAppModules.Language.GetString('ErrorString.IrrigationOutputFileNotFound');
        finally
          LFileAgent.Free;
        end;

        for LIndex := 0 to ASupplyContainer.Count - 1 do
        begin
          if(LIndex >= LChannelDemandContainer.Count) then
            Break;

          LDemandContainer.Clear;
          LDemandLineData.CommaText := LChannelDemandContainer[LIndex];
          LSupplyLineData.CommaText := ASupplyContainer[LIndex];

          LDemandContainer.Add(LSupplyLineData[0]);
          LComplianceContainer.Clear;
          LTotal := 0.0;
          LFoundDeficit := False;
          for LInnerIndex := 0 to 13 do
          begin
            if(LInnerIndex = 0) then
            begin
              LComplianceContainer.Add('N');
              Continue;
            end;
            if(LInnerIndex = 13) then
            begin
              if LFoundDeficit then
                LComplianceContainer.Add('Y')
              else
                LComplianceContainer.Add('N');
              LAverage := LTotal / 12;
              LDemandContainer.Add(FormatFloat('######0.000',LAverage));
              LFoundDeficit := False;
              Continue;
            end;

            LDemandValue := StrToFloat(FormatFloat('######0.000',StrToFloat(LDemandLineData[LInnerIndex])));
            if LIrrigationBlock.DiversionChannelMaxDemand.InUse then
            begin
              LDemandLimitValue := LIrrigationBlock.DiversionChannelMaxDemand.MaxDemandByIndex[LInnerIndex];
              if(LDemandValue > LDemandLimitValue) then
                 LDemandValue := LDemandLimitValue;

            end;

            LSupplyValue := StrToFloat(FormatFloat('######0.000',StrToFloat(LSupplyLineData[LInnerIndex])));
            LNewValue    := LDemandValue - LSupplyValue;

            LApplySensitivity := False;
            if (FDataSelection.ApplySensitivity = stvAbsolute) then
              LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(LDemandValue - LSupplyValue)))<=FDataSelection.Sensitivity)
            else
            if (FDataSelection.ApplySensitivity = stvPercentage) and (LDemandValue <> 0) then
              LApplySensitivity := (StrToFloat(FormatFloat('######0.000',(((LDemandValue - LSupplyValue)/LDemandValue)*100)))<=FDataSelection.FPercSensitivity);

            if(LNewValue < 0.0) then
               LNewValue := 0.00;

            if not (LApplySensitivity) and (LNewValue > 0.0) then
            begin
              LDemandContainer.Add(FormatFloat('######0.000',LNewValue));
              LFoundDeficit := True;
              LComplianceContainer.Add('Y');
            end
            else
            begin
              LDemandContainer.Add(FormatFloat('######0.000',LNewValue));
              LComplianceContainer.Add('N');
            end;
            LTotal := LTotal + LNewValue;
          end;
          ADeficitOutputContainer.Add(LComplianceContainer.CommaText);
          ASupplyContainer.Strings[LIndex] := LDemandContainer.CommaText;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LDemandLineData);
      FreeAndNil(LSupplyLineData);
      FreeAndNil(LDemandContainer);
      FreeAndNil(LChannelDemandContainer);
      FreeAndNil(LComplianceContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetSpecifiedDemandFeatureFileData(ADemandFileData: TStrings; AChannel: IGeneralFlowChannel;
  var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetSpecifiedDemandFeatureFileData';
var
  LCount     : integer;
  LIndex     : integer;
  LFileAgent : TDemandFileAgent;
  lFeature   : ISpecifiedDemandFeature;
  LMonthDays : array[1..13] of double;
  LYearData  : TStringList;
  LMonthData : string;
  LMonthValue : double;
begin
  Result := False;
  try
    ADemandFileData.Clear;
    lFeature := AChannel.SpecifiedDemandFeature;
    if(lFeature = nil) then Exit;

    if not FileExists(lFeature.SpecifiedDemandFileName) then
    begin
      AErrors := AErrors + 'File ('+lFeature.SpecifiedDemandFileName + ') does not exist.';
    end
    else
    begin
      LFileAgent := TDemandFileAgent.Create(FAppModules);
      LYearData  := TStringList.Create;
      try
        if LFileAgent.ReadDataCommaText(lFeature.SpecifiedDemandFileName,ADemandFileData) then
        begin
          LMonthValue := 0.0;
          for LIndex := 1 to 12 do
          begin
            LMonthDays[Lindex] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LIndex];
            LMonthValue        := LMonthValue + LMonthDays[Lindex];
          end;
          LMonthDays[13] := LMonthValue/12;

          for LIndex := 0 to ADemandFileData.Count-1 do
          begin
            LYearData.CommaText := ADemandFileData[LIndex];
            for LCount := 1 to 13 do
            begin
              if(LCount >= LYearData.Count) then Break;
              LMonthData     := LYearData[LCount];
              LMonthValue    := StrToFloatDef(LMonthData,0.0);
              LMonthValue    := (LMonthValue * 1000000.0)/(LMonthDays[LCount] * 86400.0);
              LYearData[LCount]  := FormatFloat('##0.000',LMonthValue);
            end;
            ADemandFileData[LIndex] := LYearData.CommaText;
          end;
          Result := True;
        end;
      finally
        LYearData.Free;
        LFileAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetSpecifiedDemandFeatureDataSet(ADemandFileData   : TStrings;
          AChannel: IGeneralFlowChannel; var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetSpecifiedDemandFeatureDataSet';
var
  LIndex: integer;
  LTotal: double;
  LMonthDataStr,
  LFieldName,
  LLineData,
  LSQL: string;
  LMonthData: double;
  lFeature : ISpecifiedDemandFeature;
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LDataSet : TAbstractModelDataset;
  LMonthDays : array[1..13] of double;
begin
  Result := True;
  try
    ADemandFileData.Clear;
    lFeature := AChannel.SpecifiedDemandFeature;
    LFileNameObject := nil;
    LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
    for LIndex:= 0 to LFileNamesList.FilesCount-1 do
    begin
      if(UpperCase(lFeature.SpecifiedDemandFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].FileName)) then
      begin
        LFileNameObject := LFileNamesList.FileNameObject[LIndex];
        Break;
      end;
    end;
    if(LFileNameObject = nil) then
    begin
      AErrors := FAppModules.Language.GetString('ErrorString.NoDemandFileForSelectedChannel');
    end
    else
    begin
      for LIndex := 1 to 12 do
      begin
        LMonthDays[Lindex] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LIndex];
      end;
      LMonthDays[13] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;;
      LSQL := 'SELECT DemandYearValue as [Year],'+
              'DemandMonthValue01 as Value01,'+
              'DemandMonthValue02 as Value02,'+
              'DemandMonthValue03 as Value03,'+
              'DemandMonthValue04 as Value04,'+
              'DemandMonthValue05 as Value05,'+
              'DemandMonthValue06 as Value06,'+
              'DemandMonthValue07 as Value07,'+
              'DemandMonthValue08 as Value08,'+
              'DemandMonthValue09 as Value09,'+
              'DemandMonthValue10 as Value10,'+
              'DemandMonthValue11 as Value11,'+
              'DemandMonthValue12 as Value12'+
              ' FROM DemandFileData A WHERE ' +
               GetScenarioWhereClause +
              //' AND FileType = '+IntToStr(LFileNameObject.FileGroup)+
              ' AND FileNumber = '+IntToStr(LFileNameObject.FileNumber);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LTotal := 0.0;
          LLineData  := Trim(LDataSet.DataSet.FieldByName('Year').AsString) + ',';
          for LIndex := 1 to 12 do
          begin
            LFieldName    := Format('%s%2.2d',['Value',LIndex]);
            LMonthData    := LDataSet.DataSet.FieldByName(lFieldName).AsFloat;
            LMonthData    := (LMonthData * 1000000.0)/(LMonthDays[LIndex] * 86400.0);
            LTotal        := LTotal + LMonthData;
            LMonthDataStr := FormatFloat('######0.000',LMonthData);
            LLineData  := LLineData + LMonthDataStr + ',';
          end;
          LTotal    := LTotal/12;;
          //LTotal    := (LTotal * 1000000.0)/(LMonthDays[13] * 86400.0);
          LMonthDataStr := FormatFloat('######0.000',LTotal);
          LLineData  := LLineData + LMonthDataStr ;
          ADemandFileData.Add(LLineData);
          LDataSet.DataSet.Next;
        end;
        Result := True;
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetScenarioWhereClause: string;
const OPNAME = 'TSummaryOutputData.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.PopulateDataSelection(ADataSelection: TOutputDataSelection);
const OPNAME = 'TSummaryOutputData.PopulateDataSelection';
begin
  try
    FDataSelection := ADataSelection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputData.LoadValuesLinesWithAveragesIntoAStringlist(
                                            ADataList: TStrings; AElement: TSummaryOutputDataElement);
const OPNAME = 'TSummaryOutputData.LoadValuesLinesWithAveragesIntoAStringlist';
var
  LIndex        : integer;
  LIndex1       : integer;
  LMonthData    : double;
  LMonthDataStr : string;
  LObjectlist   : TObjectlist;
  LValueLines   : TValuesLine;
begin
  try
    ADataList.Clear;
    if(AElement <> nil) then
    begin
      LObjectlist := AElement.FValuesLines;
      if LObjectlist <> nil then
      begin
        for Lindex := 0 to AElement.FValuesLines.Count -1 do
        begin
          LValueLines := TValuesLine(LObjectlist.Items[Lindex]);
          for Lindex1 := 2 to 14 do
          begin
            LMonthData  := LValueLines.FValues[LIndex1].FData;
            if LIndex1 = 14 then
              LMonthDataStr := 'AVE:'+ FormatFloat('######0.000',LMonthData)
            else
              LMonthDataStr := FormatFloat('######0.000',LMonthData);
            ADataList.Add(LMonthDataStr);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TSummaryOutputData.LoadBlobValuesWithAveragesIntoAStringlist(ADataList: TStrings; AElement: TSummaryOutputDataElement);
const OPNAME = 'TSummaryOutputData.LoadBlobValuesWithAveragesIntoAStringlist';
var
  LAgent      : TSumOutBlobFileAgent;
begin
  try
    ADataList.Clear;
    if(AElement <> nil) and FSumOutBlob.BlobLoaded  then
    begin
      LAgent := TSumOutBlobFileAgent.Create(FAppModules);
      try
        //LAgent.LoadBlobValuesWithAveragesIntoAStringlist(FSumOutBlob,ADataList,AElement);
      finally
        LAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TSummaryOutputData.LoadDatasetValuesWithAveragesIntoAStringList(ADataList: TStrings; ADataSet: TAbstractModelDataset);
const OPNAME = 'TSummaryOutputData.LoadDatasetValuesWithAveragesIntoAStringList';
var
  LValue,
  lFieldName: string;
  LIndex : integer;
begin
  try
    if(ADataList <> nil) and (ADataSet <> nil) then
    begin
      ADataList.Clear;
      ADataSet.DataSet.First;
      while not ADataSet.DataSet.Eof do
      begin
        for LIndex := 3 to 15 do
        begin
           LFieldName := Format('%s%2.2d',['GenericValue',LIndex]);
           if LIndex = 15 then LValue := 'AVE:'+ FormatFloat('#####0.000',ADataSet.DataSet.FieldByName(lFieldName).AsFloat)
           else LValue := FormatFloat('#####0.000',ADataSet.DataSet.FieldByName(lFieldName).AsFloat);
           ADataList.Add(LValue);
        end;
        ADataSet.DataSet.Next;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TSummaryOutputData.PopulateFromDataBaseBlob: boolean;
const OPNAME = 'TSummaryOutputData.PopulateFromDataBaseBlob';
begin
  Result := False;
  try
    Result := FSumOutBlob.LoadBlobFromDB;
    Result := Result and PopulateElementsFromBlobObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{function TSummaryOutputData.PopulateFromFileBlob: boolean;
const OPNAME = 'TSummaryOutputData.PopulateFromFileBlob';
begin
  Result := False;
  try
    Result := FSumOutBlob.ReadBlobFromFile;
    Result := Result and PopulateElementsFromBlobObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TSummaryOutputData.Get_SumOutBlob: ISumOutBlob;
const OPNAME = 'TSummaryOutputData.Get_SumOutBlob';
begin
  Result := nil;
  try
    //Result := FSumOutBlob;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputData.GetReservoirAreaData(ADataSource: TStrings;ADataType: TOutputDataType;
                                                 ANetworkElementID: integer;
                                                 var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetReservoirAreaData';
var
  LIndex             : integer;
  LIndexA            : integer;
  LCount             : integer;
  LIdentifier        : integer;

  LSourceValue       : double;
  LDestinationValue  : double;

  LTempData          : TStringList;
  LDestination       : TStringList;
  LSource            : TStringList;
  LLineData          : TStringList;

  LReservoirData     : IReservoirData;
  LReservoirDataList : IReservoirDataList;
begin
  Result := False;
  try
    LTempData    := TStringList.Create;
    LSource      := TStringList.Create;
    LDestination := TStringList.Create;
    LLineData    := TStringList.Create;
    ADataSource.Clear;
    try
      LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                      .ReservoirList;
      for LIndex := 0 to LReservoirDataList.ReservoirCount -1 do
      begin
        LReservoirData := LReservoirDataList.ReservoirOrNodeByIndex[LIndex];
        if LReservoirData <> nil then
        begin
          if LReservoirData.ReservoirConfigurationData.GroupID = ANetworkElementID then
          begin
            LIdentifier := LReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
            if GetBlockData(LTempData,ADataType,LIdentifier,AErrors) then
            begin
              if(ADataSource.Count = 0) then
              begin
                 ADataSource.AddStrings(LTempData);
              end
              else
              begin
                for LCount := 0 to ADataSource.Count-1 do
                begin
                   LSource.CommaText := LTempData[LCount];
                   LDestination.CommaText := ADataSource[LCount];

                   LLineData.Clear;
                   LLineData.Add(LSource[0]);
                   for LIndexA := 1 to LSource.Count-1 do
                   begin
                     LSourceValue      := StrToFloat(LSource[LIndexA]);
                     LDestinationValue := StrToFloat(LDestination[LIndexA]);
                     LLineData.Add(FormatFloat('##0.000',LSourceValue+LDestinationValue));
                   end;
                   ADataSource[LCount] := LLineData.CommaText;
                end;
              end;
            end;
            Result := true;
          end;
        end;
      end;
    finally
      LTempData.Free;
      LSource.Free;
      LLineData.Free;
      LDestination.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TSummaryOutputData.GetReservoirAreaBoxPlotData(ADataContainer: TStrings; ADataType: TOutputDataType;
                                                        ANetworkElementID: integer; const ALoadCase: integer;
                                                        var AErrors: string): boolean;
const OPNAME = 'TSummaryOutputData.GetReservoirAreaBoxPlotData';
var
  LReservoirDataList : IReservoirDataList;
  LReservoirData     : IReservoirData;

  LReservoirIndex    : integer;
  LIndex             : integer;
  LCount             : integer;
  LIdentifier        : integer;

  LSourceValue       : double;
  LDestinationValue  : double;

  LDestinationData   : TStringList;
  LSourceData        : TStringList;
  LDestSourceData    : TStrings;
  LLineData          : TStringList;
begin
  Result := False;
  try
    ADataContainer.Clear;
    LSourceData      := TStringList.Create;
    LDestinationData := TStringList.Create;
    LLineData        := TStringList.Create;
    LDestSourceData  := TStringList.Create;
    LReservoirDataList     := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
    try
      for LReservoirIndex := 0 to LReservoirDataList.ReservoirCount - 1 do
      begin
        LReservoirData := LReservoirDataList.ReservoirOrNodeByIndex[LReservoirIndex];
        if(LReservoirData <> nil) then
        begin
          if(LReservoirData.ReservoirConfigurationData.GroupID = ANetworkElementID) then
          begin
            LIdentifier := LReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
            if(GetBoxPlotData(LDestSourceData,ADataType,LIdentifier,ALoadCase,AErrors) ) then
            begin
              if(ADataContainer.Count = 0) then
              begin
                 ADataContainer.AddStrings(LDestSourceData);
              end
              else
              begin
                for LCount := 0 to LDestSourceData.Count - 1 do
                begin
                  if(LCount >= LDestSourceData.Count) then Break;

                   LSourceData.CommaText := ADataContainer[LCount];
                   LDestinationData.CommaText := LDestSourceData[LCount];
                   LLineData.Clear;
                   LLineData.Add(LSourceData[0]);
                   for LIndex := 1 to LSourceData.Count - 1 do
                   begin
                     LSourceValue      := StrToFloat(LSourceData[LIndex]);
                     LDestinationValue := StrToFloat(LDestinationData[LIndex]);
                     LLineData.Add(FormatFloat('##0.000',LSourceValue + LDestinationValue));
                   end;
                   ADataContainer[LCount] := LLineData.CommaText;
                end;
              end;
            end;
            Result := True;
          end;
        end
      end;
    finally
      LSourceData.Free;
      LLineData.Free;
      LDestinationData.Free;
      LDestSourceData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TSummaryOutputData.PopulateElementsFromBlobObject: boolean;
const OPNAME = 'TSummaryOutputData.PopulateElementsFromBlobObject';
var
  LAgent: TSumOutBlobFileAgent;
begin
  Result := False;
  try
    LAgent := TSumOutBlobFileAgent.Create(FAppModules);
    try
      //Result := LAgent.PopulateElementsFromBlobObject(FSumOutBlob,PopulateElement);
    finally
      LAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TSummaryOutputData.ReCalculateElementSequenceNumbers: boolean;
const OPNAME = 'TSummaryOutputData.ReCalculateElementSequenceNumbers';
var
  LIndex     : integer;
  LElement   : TSummaryOutputDataElement;
  LElementID : integer;
  LBlockType : TOutputDataType;
  LSequencNumber : integer;
  LSequenceIndex : integer;
  lConfigData    : IRunConfigurationData;
begin
  Result := False;
  try
    if(FDBItemsList.Count > 0) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LElement    := TSummaryOutputDataElement(FDBItemsList.Items[0]);
      LElementID  := LElement.ElementID;
      LBlockType  := LElement.BlockType;

      LSequenceIndex := 1;
      if (lConfigData.RunSequenceType = 'H') then
        LSequencNumber := lConfigdata.StartSequenceNumber
      else
        LSequencNumber := lConfigdata.StartSequenceNumber+1;
      LElement.SequenceNumber := LSequencNumber;
      for LIndex := 1  to FDBItemsList.Count -1 do
      begin
        LElement := TSummaryOutputDataElement(FDBItemsList.Items[LIndex]);
        if((LElement.ElementID = LElementID) and (LElement.BlockType = LBlockType)) then
        begin
          if(lConfigData.NumberOfSequencesInAnalysis > 10) then
            LSequencNumber := LSequencNumber + 1
          else
          begin
            LSequenceIndex := LSequenceIndex + 1;
            LSequencNumber := lConfigData.SequenceToBeAnalysedByIndex[LSequenceIndex];
          end;
        end;
        LElement.SequenceNumber := LSequencNumber;
      end;
    end;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSummaryOutputDataElement }

procedure TSummaryOutputDataElement.CopyValuesLines(AValuesLines: TObjectList);
const OPNAME = 'TSummaryOutputDataElement.CopyValuesLines';
var
  LIndex : integer;
  LValueLine  : TValuesLine;

begin
  try
    if FValuesLines = nil then
      FValuesLines := TObjectList.Create(True);
    for LIndex := 0 to AValuesLines.Count-1 do
    begin
      LValueLine  := TValuesLine.Create(nil);
      LValueLine.Assign(TValuesLine(AValuesLines.Items[LIndex]));
      FValuesLines.Add(LValueLine)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataElement.Populate(ABlockType: TOutputDataType; AElementID,ABlockNumber,ALoadCaseNumber,ASequenceNumber: integer;
              AAnnualWaterDemand,AAnnualPowerDemand: double;AHeading,ATitle : string ;AValuesLines: TObjectlist{TStringList{});
const OPNAME = 'TSummaryOutputDataElement.Populate';
begin
  try
    FBlockType          := ABlockType;
    FElementID          := AElementID;
    FBlockNumber        := ABlockNumber;
    FLoadCaseNumber     := ALoadCaseNumber;
    FSequenceNumber     := ASequenceNumber;
    FAnnualWaterDemand  := AAnnualWaterDemand;
    FAnnualPowerDemand  := AAnnualPowerDemand;
    FBlockHeading       := AHeading;
    FBlockTitle         := ATitle;
    if AValuesLines <> nil then
      CopyValuesLines(AValuesLines);
    //FValuesLines        := AValuesLines;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataElement.PopulateBlockIndex(ABlockType: TOutputDataType; AElementID, ABlockNumber, ALoadCaseNumber,
  ASequenceNumber: integer; AAnnualWaterDemand, AAnnualPowerDemand: double;
  AHeading, ATitle: string; ABlockIndex: Int64);
const OPNAME = 'TSummaryOutputDataElement.PopulateBlockIndex';
begin
  try
    FBlockType          := ABlockType;
    FElementID          := AElementID;
    FBlockNumber        := ABlockNumber;
    FLoadCaseNumber     := ALoadCaseNumber;
    FSequenceNumber     := ASequenceNumber;
    FAnnualWaterDemand  := AAnnualWaterDemand;
    FAnnualPowerDemand  := AAnnualPowerDemand;
    FBlockHeading       := AHeading;
    FBlockTitle         := ATitle;
    FBlockIndex         := ABlockIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataElement.Initialise: boolean;
const OPNAME = 'TSummaryOutputDataElement.Initialise';
begin
  Result := False;
  try
    FBlockType          := btNone;
    FElementID          := NullInteger;
    FBlockNumber        := NullInteger;
    FLoadCaseNumber     := NullInteger;
    FSequenceNumber     := NullInteger;
    FAnnualWaterDemand  := NullFloat;
    FAnnualPowerDemand  := NullFloat;
    FBlockHeading       := '';
    FBlockTitle         := '';
    Result              := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPlottingOutputElement }

procedure TPlottingOutputElement.Populate(AElementType : TNetworkElementType;AElementDataTypeType: TOutputDataType;
          AElementID: integer; AElementName: string);
const OPNAME = 'TPlottingOutputElement.Populate';
begin
  try
    FElementID       := AElementID;
    FElementType     := AElementType;
    FElementDataType := AElementDataTypeType;
    FElementName     := AElementName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputElement.Initialise: boolean;
const OPNAME = 'TPlottingOutputElement.Initialise';
begin
  Result := False;
  try
    FElementID       := NullInteger;
    FElementType     := votNone;
    FElementDataType := btNone;
    FElementName     := '';
    Result           := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TPlottingOutputData }

procedure TPlottingOutputData.CreateMemberObjects;
const OPNAME = 'TPlottingOutputData.CreateMemberObjects';
begin
  inherited;
  try

    FDBItemsList  := TObjectList.Create(True);
//    FFileItemsList := TObjectList.Create(True);
    FDataSelection := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlottingOutputData.DestroyMemberObjects;
const OPNAME = 'TPlottingOutputData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDBItemsList);
//    FreeAndNil(FFileItemsList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.ElementsCount: integer;
const OPNAME = 'TPlottingOutputData.ElementsCount';
begin
  Result := 0;
  try
    Result := FDBItemsList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.GetElementByIndex(AIndex: integer): TPlottingOutputElement;
const OPNAME = 'TPlottingOutputData.GetElementByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDBItemsList.Count) then
    Result := TPlottingOutputElement(FDBItemsList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.GetElementByNo(AElement: integer;var AElementInex : integer):TPlottingOutputElement;
const OPNAME = 'TPlottingOutputData.GetElementByNo';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDBItemsList.Count-1 do
    begin
      if AElement = GetElementByIndex(LIndex).FElementID then
      begin
        Result := GetElementByIndex(LIndex);
        AElementInex := LIndex;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.GetDataByElement(ADataType: TOutputDataType;AElement : integer;AData : TStrings;var AErrors : string):boolean;
const OPNAME = 'TPlottingOutputData.GetElementByIndex';
var
  LLoadCase : integer;
  LSeqNo : integer;
  LAgent : TPlotFileAgent;
  LElement : TPlottingOutputElement;
  LFileNameObject: TAbstractModelFileName;
  LElementInex : integer;
  LIndex : integer;
  LLineValue : TStringList;
  LCount,
  LYear : integer;
  LValue : double;
  LChannel : IGeneralFlowChannel;
  LMasterControlFeature : IMasterControlFeature;
begin
  Result := False;
  try
    if Assigned(AData) then
    begin
      LAgent := TPlotFileAgent.Create(FAppModules);
      try
        LLoadCase := 1;
        LSeqNo := 1;
        if Assigned(FDataSelection) then
        begin
          LLoadCase := FDataSelection.LoadCase;
          LSeqNo := FDataSelection.Sequence;
        end;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.GetPlotOutFile;

        LElement := GetElementByNo(AElement,LElementInex);
        if (LElement = nil) then
        begin
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[AElement];
          if LChannel <> nil then
          begin
            LMasterControlFeature := LChannel.MasterControlFeature;
            if (LMasterControlFeature = nil) then
            begin
              AErrors := FAppModules.Language.GetString('ErrorString.SelectedNetworkElementNotIncluded');
              Exit;
            end
            else
              Result := LAgent.ReadDataFromFile(ADataType,LFileNameObject,AData,LLoadCase,
              LSeqNo,votMasterControl,LChannel.ChannelName,AErrors);
          end;
        end
        else
          Result := LAgent.ReadDataFromFile(ADataType,LFileNameObject,AData,LLoadCase,LSeqNo,LElement.ElementType,LElement.ElementName,AErrors);

        if Result then
        begin
          LLineValue := TStringList.Create;
          try
            LYear := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
            for LIndex := 0 to AData.Count-1 do
            begin
              LLineValue.CommaText := AData[LIndex];
              LValue := 0;
              for LCount := 0 to LLineValue.Count-1 do
                if Trim(LLineValue[LCount]) <> '' then
                  LValue := StrToFloat(Trim(LLineValue[LCount]));
              if LValue > 0 then
                LValue := LValue/LLineValue.Count;
              LLineValue.Add(FormatFloat('######0.000',LValue));
              LLineValue.Insert(0,InttoStr(LYear));
              AData[LIndex] := LLineValue.CommaText;
              LYear := LYear+1;
            end;
          finally
            FreeAndNil(LLineValue);
          end;
        end;
      finally
        FreeAndNil(LAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.GetBoxPlotDataByElement(ADataType: TOutputDataType;AElement : integer;AData : TStrings;var AErrors : string):boolean;
const OPNAME = 'TPlottingOutputData.GetBoxPlotDataByElement';
var
  LLoadCase : integer;
  LAgent : TPlotFileAgent;
  LElement : TPlottingOutputElement;
  LFileNameObject: TAbstractModelFileName;
  LElementInex : integer;
  LIndex : integer;
  LLineValue : TStringList;
  LAnnualValues : TStringList;
  LTempValues : TStringList;
  LCount,
  LIndex1,
  LYear : integer;
  LChannel : IGeneralFlowChannel;
  LMasterControlFeature : IMasterControlFeature;
  LMonthNumbers : array[0..11] of integer;
  LStartMonth : integer;
  LMonthData : string;
  LMonthDataStr : string;
  LLineData : string;
  LPos,
  LSeq : integer;
  LChangeYear : boolean;
  LDate : TDateTime;
  LIndex2,
  LBlockNumber,
  LMonthCount : integer;
begin
  Result := False;
  try
    if Assigned(AData) then
    begin
      LAgent := TPlotFileAgent.Create(FAppModules);
      try
        LLoadCase := 1;
        if Assigned(FDataSelection) then
          LLoadCase := FDataSelection.LoadCase;
        LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.GetPlotOutFile;
        LElement := GetElementByNo(AElement,LElementInex);
        if (LElement = nil) then
        begin
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
              NetworkElementData.ChannelList.ChannelByChannelNumber[AElement];
          if LChannel <> nil then
          begin
            LMasterControlFeature := LChannel.MasterControlFeature;
            if (LMasterControlFeature = nil) then
            begin
              AErrors := FAppModules.Language.GetString('ErrorString.SelectedNetworkElementNotIncluded');
              Exit;
            end
            else
              Result := LAgent.ReadDataFromFile(ADataType,LFileNameObject,AData,LLoadCase,
                        0,votMasterControl,LChannel.ChannelName,AErrors);
          end;
        end
        else
          Result := LAgent.ReadDataFromFile(ADataType,LFileNameObject,AData,LLoadCase,0,LElement.ElementType,LElement.ElementName,AErrors);
        if Result then
        begin
          LLineValue := TStringList.Create;
          LAnnualValues := TStringList.Create;
          LTempValues := TStringList.Create;
          try
            LMonthCount := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.PeriodsInAnalysis;
            LStartMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber + FAppModules.StudyArea.CalendarStartMonth - 1;
            LSeq := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NumberOfSequencesInAnalysis;
            if(LStartMonth > 12) then
               LStartMonth := LStartMonth - 12;
            for LIndex := 0 to 11 do
            begin
              if(LStartMonth > 12) then
                 LStartMonth := 1;
              LMonthNumbers[LIndex] := LStartMonth;
              LStartMonth := LStartMonth + 1;
            end;
            LLineValue.Assign(AData);
            AData.Clear;
            Lindex1 := 0;
            LBlockNumber := 0;
            for LIndex := 1 to LSeq do
            begin
              LYear := TYieldModelDataObject(FAppModules.Model.ModelData).
                       RunConfigurationData.StartYearOther;
              LPos := 0;
              LBlockNumber := LBlockNumber + 1;
              LTempValues.Clear;
              while Lindex1<=LLineValue.Count-1 do
              begin
                if (LMonthCount = LPos) then break;
                LAnnualValues.Clear;
                LAnnualValues.CommaText := LLineValue[LIndex1];
                for LCount := 0 to 11 do
                begin
                  LMonthData := Trim(LAnnualValues[LCount]);
                  LMonthDataStr := IntToStr(LYear) + ':'+ LMonthData;
                  LTempValues.Add(LMonthDataStr);
                  LPos := LPos+1;
                end;
                LIndex1 := LIndex1+1;
                LYear := LYear+1;
              end;
              LPos := -1;
              for LCount := 0 to (LTempValues.Count div 12)-1 do
              begin
                LChangeYear := False;
                for LIndex2 := 0 to 11 do
                begin
                  LPos := LPos + 1;
                  if (LPos<=AData.Count) and (LBlockNumber<=1) then
                  begin
                    LYear := StrToInt(copy(LTempValues[LPos],1,pos(':',LTempValues[LPos])-1));
                    if LChangeYear then
                      LYear := LYear + 1;
                    LDate := EncodeDate(LYear,LMonthNumbers[LIndex2],1);
                    AData.Add(DateToStr(LDate));
                  end;
                  LMonthData := Copy(LTempValues[LPos],pos(':',LTempValues[LPos])+1,length(LTempValues[LPos]));
                  LLineData  := AData[LPos];
                  LLineData  := LLineData + ',' + LMonthData;
                  AData.Strings[LPos] := LLineData;
                  if(LMonthNumbers[LIndex2] = 12) then
                    LChangeYear := True;
                end;
              end;
            end;
          finally
            FreeAndNil(LLineValue);
            FreeAndNil(LAnnualValues);
            FreeAndNil(LTempValues);
          end;
        end;
      finally
        FreeAndNil(LAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TPlottingOutputData.Initialise: boolean;
const OPNAME = 'TPlottingOutputData.Initialise';
begin
  Result := inherited Initialise;
  try
    FDBItemsList.Clear;
//    FFileItemsList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlottingOutputData.Populate(AElementType : TNetworkElementType;AElementDataType: TOutputDataType;
          AElementID: integer; ElementName: string);
const OPNAME = 'TPlottingOutputData.Populate';
var
  LElement: TPlottingOutputElement;
begin
  try
    LElement := TPlottingOutputElement.Create(FAppModules);
    LElement.Initialise;
    FDBItemsList.Add(LElement);
    LElement.Populate(AElementType,AElementDataType,AElementID,ElementName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.StudyHasChanged: boolean;
const OPNAME = 'TPlottingOutputData.StudyHasChanged';
begin
  Result :=  inherited StudyHasChanged;
  try
    FDBItemsList.Clear;
//    FFileItemsList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlottingOutputData.GetPlottingDataSet(ADataSet: TAbstractModelDataset;
         ExcludeMasterControl, ExcludeTotals: boolean; var AErrors: string): boolean;
const OPNAME = 'TPlottingOutputData.GetPlottingDataSet';
var
  LSQL: string;
begin
  Result :=  False;
  try
    AErrors := '';
    if(ADataSet = nil) then
       raise Exception.Create('DataSet parameter is not assigned.');

    if(FDBItemsList.Count = 0) then
      AErrors := FAppModules.Language.GetString('ErrorString.PLTOutFileNotImported')
    else
    begin
      LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,LoadCaseNumber,SequenceNumber,ElementType,ElementNumber,'+
          ' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12'+
          ' FROM pltFileData'+
          ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
          '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
          '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
          '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';

      if not ExcludeMasterControl then
         LSQL := LSQL + ' AND (ElementType  <> 3)';
      if not ExcludeTotals then
         LSQL := LSQL + ' AND (ElementType  <> 4)';
      LSQL := LSQL + ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
      ADataSet.DataSet.Close;
      ADataSet.SetSQL(LSQL);
      ADataSet.DataSet.Open;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlottingOutputData.PopulateDataSelection(ADataSelection: TOutputDataSelection);
const OPNAME = 'TPlottingOutputData.PopulateDataSelection';
begin
  try
    FDataSelection := ADataSelection;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOutputDataSelection }

function TOutputDataSelection._AddRef: Integer;
const OPNAME = 'TOutputDataSelection._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDataSelection._Release: Integer;
const OPNAME = 'TOutputDataSelection._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDataSelection.Populate(ALoadCase,ASequence,AMonth,ADisplayMonth,ADecisionMonth : integer; AUnits : TOutputUnits;
              AValueType: TOutputValueType; ATimeStep: TOutputTimeStep; AHighlight : boolean;
              AAverageType : TOutputAverageType; AAverageStartDate,
              AAverageEndDate   : TDateTime;APlotOption:TOutputPlotOptions;AYearsToSkip : integer;
              AApplySensitivity : TSensitivity; APercSensitivity, ASensitivity : double);
const OPNAME = 'TOutputDataSelection.Populate';
var
  LStartSequenceNr : integer;
  LEndSequenceNr   : integer;
  LModelCalendar   : IModelCalendar;
  LConfigData : IRunConfigurationData;
begin
  try
    if(FAppModules.Model <> nil) and (FAppModules.Model.ModelData <> nil) then
    begin
      LConfigData    := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LModelCalendar := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar;
      //if (lConfigData.RunSequenceType = 'H') then
        LStartSequenceNr := lConfigData.StartSequenceNumber;
        if(LStartSequenceNr < 1) then
          LStartSequenceNr := 1;
      //else
      //  LStartSequenceNr := lConfigData.StartSequenceNumber+1;
      LEndSequenceNr     := LStartSequenceNr + lConfigData.NumberOfSequencesInAnalysis-1;

      if(LEndSequenceNr < LStartSequenceNr) then
        LEndSequenceNr     := LStartSequenceNr;
      if(ASequence < LStartSequenceNr) then
        ASequence     := LStartSequenceNr;
      if(ASequence > LEndSequenceNr) then
        ASequence     := LEndSequenceNr;


      if(FAverageStartDate = 0.0) and (FAverageEndDate = 0.0) then
      begin
        AAverageStartDate := LModelCalendar.CalenderStartDate[0];
        AAverageEndDate   := IncMonth(LConfigData.YearsInAnalysis*12);
      end;
    end;

    FLoadCase          := ALoadCase;
    FSequence          := ASequence;
    FMonth             := AMonth;
    FDisplayMonth      := ADisplayMonth;
    FDecisionMonth     := ADecisionMonth;
    FUnits             := AUnits;
    FValueType         := AValueType;
    FTimeStep          := ATimeStep;
    FHighlight         := AHighlight;
    FAverageType       := AAverageType;
    FAverageStartDate  := AAverageStartDate;
    FAverageEndDate    := AAverageEndDate;
    FPlotOption        := APlotOption;
    FYearsToSkip       := AYearsToSkip;
    FApplySensitivity  := AApplySensitivity;
    FSensitivity       := ASensitivity;
    FPercSensitivity   := APercSensitivity;
    FPopulated         := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Initialise: boolean;
const OPNAME = 'TOutputDataSelection.Initialise';
var
  lConfigData    : IRunConfigurationData;
begin
  Result := inherited Initialise;
  try
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    FLoadCase  := 1;
    if (lConfigData.RunSequenceType = 'H') then
      FSequence := lConfigdata.StartSequenceNumber
    else
      FSequence := lConfigdata.StartSequenceNumber+1;
    FMonth     := 1;

    FDecisionMonth  := 0;
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      if(lConfigData.NrOfDecisionMonths > 0) then
        FDecisionMonth := lConfigData.DecisionMonthByIndex[1];
    end;

    FDisplayMonth := 0;
    FUnits     := ouPerSecond;
    FValueType := ovtSupply;
    FTimeStep  := otsMonthly;
    FApplySensitivity := stvNone;
    FPercSensitivity := 0;
    FSensitivity := 0.001;
    FHighlight := True;
    FPopulated := False;
    FAverageType      := oatSequence;
    FAverageStartDate := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.CalenderStartDate[0];
    FAverageEndDate   := IncMonth(FAverageStartDate,TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis*12);;
    Result     := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_DisplayMonth: Integer;
const OPNAME = 'TOutputDataSelection.Get_DisplayMonth';
begin
  Result := 0;
  try
    Result := FDisplayMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_Highlight: WordBool;
const OPNAME = 'TOutputDataSelection.Get_Highlight';
begin
  Result := FALSE;
  try
    Result := FHighlight;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_LoadCase: Integer;
const OPNAME = 'TOutputDataSelection.Get_LoadCase';
begin
  Result := 0;
  try
    Result := FLoadCase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelection.Set_LoadCase(Value: Integer);
const OPNAME = 'TOutputDataSelection.Set_LoadCase';
begin
  try
    FLoadCase := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDataSelection.Set_Sequence(Value: Integer);
const OPNAME = 'TOutputDataSelection.Set_Sequence';
begin
  try
    FSequence := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_Month: Integer;
const OPNAME = 'TOutputDataSelection.Get_Month';
begin
  Result := 0;
  try
    Result := FMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_Sequence: Integer;
const OPNAME = 'TOutputDataSelection.Get_Sequence';
begin
  Result := 0;
  try
    Result := FSequence;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_TimeStep: TOutputTimeStep;
const OPNAME = 'TOutputDataSelection.Get_TimeStep';
begin
  Result := 0;
  try
    Result := FTimeStep;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_Units: TOutputUnits;
const OPNAME = 'TOutputDataSelection.Get_Units';
begin
  Result := 0;
  try
    Result := FUnits;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_ValueType: TOutputValueType;
const OPNAME = 'TOutputDataSelection.Get_ValueType';
begin
  Result := 0;
  try
    Result := FValueType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_AverageEndDate: TDateTime;
const OPNAME = 'TOutputDataSelection.Get_AverageEndDate';
begin
  Result := 0.0;
  try
    Result := FAverageEndDate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_AverageStartDate: TDateTime;
const OPNAME = 'TOutputDataSelection.Get_AverageStartDate';
begin
  Result := 0.0;
  try
    Result := FAverageStartDate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_AverageType: TOutputAverageType;
const OPNAME = 'TOutputDataSelection.Get_AverageType';
begin
  Result := oatSequence;
  try
    Result := FAverageType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_DecisionMonth: Integer;
const OPNAME = 'TOutputDataSelection.Get_DecisionMonth';
begin
  Result := 0;
  try
    Result := FDecisionMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_PlotOption: TOutputPlotOptions;
const OPNAME = 'TOutputDataSelection.Get_PlotOption';
begin
  Result := 0;
  try
    Result := FPlotOption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_YearsToSkip: Integer;
const OPNAME = 'TOutputDataSelection.Get_YearsToSkip';
begin
  Result := 0;
  try
    Result := FYearsToSkip;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_ApplySensitivity: TSensitivity;
const OPNAME = 'TOutputDataSelection.Get_ApplySensitivity';
begin
  Result := stvNone;
  try
    Result := FApplySensitivity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelection.Set_ApplySensitivity(AValue: TSensitivity);
const OPNAME = 'TOutputDataSelection.Set_ApplySensitivity';
begin
  try
    FApplySensitivity := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_Sensitivity: Double;
const OPNAME = 'TOutputDataSelection.Get_Sensitivity';
begin
  Result := 0;
  try
    Result := FSensitivity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelection.Set_Sensitivity(Value: Double);
const OPNAME = 'TOutputDataSelection.Set_Sensitivity';
begin
  try
    FSensitivity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelection.Get_PercSensitivity: Double;
const OPNAME = 'TOutputDataSelection.Get_PercSensitivity';
begin
  Result := 0;
  try
    Result := FPercSensitivity;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelection.Set_PercSensitivity(Value: Double);
const OPNAME = 'TOutputDataSelection.Set_PercSensitivity';
begin
  try
    FPercSensitivity := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TOutputDataSelection.Set_TimeStep(Value: TOutputTimeStep);
const OPNAME = 'TOutputDataSelection.Set_PercSensitivity';
begin
  try
    FTimeStep := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSummaryOutputDataSource }


procedure TSummaryOutputDataSource.CreateMemberObjects;
const OPNAME = 'TSummaryOutputDataSource.CreateMemberObjects';
begin
  inherited;
  try
    FCurrentSource := sodsNone;
    FAvailableSources := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataSource.DestroyMemberObjects;
const OPNAME = 'TSummaryOutputDataSource.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FAvailableSources);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.Initialise: boolean;
const OPNAME = 'TSummaryOutputDataSource.Initialise';
begin
  Result := False;
  try
    FAvailableSources.Clear;
    FAvailableSources.Add('0');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.StudyHasChanged: boolean;
const OPNAME = 'TSummaryOutputDataSource.StudyHasChanged';
var
  LValue : integer;
begin
  Result := False;
  try
    LValue := FAppModules.IniFile.ReadInteger('Settings','SummaryOutputDataSourceName',0);
    FCurrentSource := TSummaryOutputDataSourceName(LValue);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataSource.AddSource(ASource: TSummaryOutputDataSourceName);
const OPNAME = 'TSummaryOutputDataSource.AddSource';
var
  LValue : string;
begin
  if (ASource = sodsNone) then Exit;
  try
    LValue := IntToStr(Ord(ASource));
    FAvailableSources.Add(LValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataSource.RemoveSource(ASource: TSummaryOutputDataSourceName);
const OPNAME = 'TSummaryOutputDataSource.RemoveSource';
var
  LValue : string;
begin
  if (ASource = sodsNone) then Exit;
  try
    LValue := IntToStr(Ord(ASource));
    if(FAvailableSources.IndexOf(LValue) >= 0) then
     FAvailableSources.Delete(FAvailableSources.IndexOf(LValue));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.Get_CurrentSource: TSummaryOutputDataSourceName;
const OPNAME = 'TSummaryOutputDataSource.Get_CurrentSource';
begin
  Result := sodsNone;
  try
    if(FAppModules.Model.ModelName = CPlanning) then
       Result := sodsSumFile
    else
      Result := FCurrentSource;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSummaryOutputDataSource.Set_CurrentSource(ASource: TSummaryOutputDataSourceName);
const OPNAME = 'TSummaryOutputDataSource.Set_CurrentSource';
var
  LValue : string;
begin
  if (ASource = sodsNone) then Exit;
  try
    if(FAppModules.Model.ModelName = CPlanning) then
       ASource := sodsSumFile;

    LValue := IntToStr(Ord(ASource));
    if(FAvailableSources.IndexOf(LValue) >= 0) then
    begin
     FCurrentSource := ASource;
     FAppModules.IniFile.WriteInteger('Settings','SummaryOutputDataSourceName',Ord(FCurrentSource));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.Get_DataSourceAvailable(ASource: TSummaryOutputDataSourceName): boolean;
const OPNAME = 'TSummaryOutputDataSource.Get_DataSourceAvailable';
begin
  Result := False;
  try
    Result := (FAvailableSources.IndexOf(IntToStr(Ord(ASource))) >= 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.Get_DataSourceByIndex(AIndex: integer): TSummaryOutputDataSourceName;
const OPNAME = 'TSummaryOutputDataSource.Get_DataSourceByIndex';
begin
  Result := sodsNone;
  try
    if(AIndex >= 0) and (AIndex < FAvailableSources.Count) then
    Result := TSummaryOutputDataSourceName(StrToIntDef(FAvailableSources[AIndex],0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSummaryOutputDataSource.Get_DataSourcesCount: integer;
const OPNAME = 'TSummaryOutputDataSource.Get_DataSourcesCount';
begin
  Result := 0;
  try
    Result := FAvailableSources.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDemandOutputData }

function TDemandOutputData.CalculateAverageDemandValues(AChannelDemandValues: TStrings): boolean;
const OPNAME = 'TDemandOutputData.CalculateAverageDemandValues';
var
  LRow,
  LCol           : integer;
  LYearCount     : integer;
  LDataContainer : TStringList;
  LYear          : integer;
  LValues        : array[1..13] of double;
  LDaysPerMonth  : array[1..13] of double;
  LMonthlyTotals : array[1..13] of double;
  LMonthValue    : double;
  LTotal         : double;
  LGrandTotal    : double;
  lYieldModelData : IYieldModelData;
begin
  Result := False;
  try
    LYearCount := AChannelDemandValues.Count;
    if(LYearCount = 0) then
    begin
      Result := True;
      Exit;
    end;

    LDataContainer := TStringList.Create;
    try
      lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      for LCol := 1 to 12 do
        LDaysPerMonth[LCol] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[LCol];
      LDaysPerMonth[13] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;

      for LCol := 1 to 12 do
        LMonthlyTotals[LCol] := 0.0;

      LGrandTotal := 0.0;
      for LRow := 0 to LYearCount-1 do
      begin
        LDataContainer.CommaText := AChannelDemandValues[LRow];
        if(LDataContainer.Count >= 13) then
        begin
          LYear  := StrToIntDef(LDataContainer[0],0);
          LTotal      := 0.0;
          for LCol := 1 to 12 do
          begin
            LMonthValue := StrToFloatDef(LDataContainer[LCol],0.0);
            LTotal := LTotal + LMonthValue*LDaysPerMonth[LCol];
            LValues[LCol] := LMonthValue;
            LMonthlyTotals[LCol] := LMonthlyTotals[LCol]+ LMonthValue;
          end;
          LTotal      := LTotal/LDaysPerMonth[13];
          LValues[13] := LTotal;;
          LGrandTotal := LGrandTotal + LTotal;
          AChannelDemandValues[LRow] := IntToStr(LYear) +','+ DoubleArrayToCommaText(LValues,3);
        end;
      end;
      LMonthlyTotals[13] := LGrandTotal;
      for LCol := 1 to 13 do
        LMonthlyTotals[LCol] := LMonthlyTotals[LCol]/ LYearCount;
      AChannelDemandValues.Add('Average,'+DoubleArrayToCommaText(LMonthlyTotals,3));
    finally
      LDataContainer.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetChannelDemandValues(ADataContainer: TStrings;AChannelNumber: integer; var AErrors: string): boolean;
const OPNAME = 'TDemandOutputData.GetChannelDemandValues';
var
  LChannel : IGeneralFlowChannel;
begin
  Result := False;
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                .ChannelList.ChannelByChannelNumber[AChannelNumber];
    if (LChannel = nil) then
    begin
      AErrors := 'Channel number '+ IntToStr(AChannelNumber) + ' does not exist in the list on all channels.';
    end
    else
    begin
      if(LChannel.ChannelType = 2) then
        Result := GetMasterControlDemandValues(ADataContainer,LChannel)
      else if(LChannel.ChannelType = 8) and (LChannel.IFRFeature = nil) then
        Result := GetMinMaxDemandValues(ADataContainer,LChannel)
      else if(LChannel.ChannelType = 11) then
        Result := GetSpecifiedDemandValues(ADataContainer,LChannel)
      else if(LChannel.ChannelType = 14) then
        Result := GetIrrigationDemandValues(ADataContainer,LChannel)
      else if(LChannel.IFRFeature <> nil) then
        Result := GetIFRDemandValues(ADataContainer,LChannel);
      if Result then CalculateAverageDemandValues(ADataContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetIrrigationDemandValues(AChannelDemandValues: TStrings;AChannel: IGeneralFlowChannel): boolean;
const OPNAME = 'TDemandOutputData.GetIrrigationDemandValues';
var
  LIrrigationBlock            : TIrrigationBlock;
  LIrrigationBlockList        : TIrrigationBlockList;
  LFileAgent                  : TDemandOutputFileAgent;
  LDataSelection              : IOutputDataSelection;
  LYieldModelData             : IYieldModelData;
  LChannelNumber,
  LSequenceCount,
  LLoadCaseCount,
  LYearInAnalysis,
  LCurrentSequence,
  LCurrentLoadCase            : integer;
  LIrrigationBlockName        : string;
begin
  Result := False;
  try
    LIrrigationBlockList        := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIrrigationBlockList;
    LIrrigationBlock            := LIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(AChannel.DownStreamNodeNumber);
    LDataSelection              := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
    LYieldModelData             := (FAppModules.Model.ModelData as IYieldModelData);
    if(LIrrigationBlock  <> nil) and (LDataSelection <> nil) then
    begin
      LFileAgent := TDemandOutputFileAgent.Create(FAppModules);
      try
        LChannelNumber             := AChannel.ChannelNumber;
        LSequenceCount             := LYieldModelData.RunConfigurationData.NumberOfSequencesInAnalysis;
        LLoadCaseCount             := LYieldModelData.RunConfigurationData.NrOfActiveLoadCases;
        LYearInAnalysis            := LYieldModelData.RunConfigurationData.YearsInAnalysis;
        LCurrentSequence           := LDataSelection.Sequence;
        LCurrentLoadCase           := LDataSelection.LoadCase;
        LIrrigationBlockName       := LIrrigationBlock.BlockName;
        Result := LFileAgent.ReadChannelDataCommaText(LChannelNumber,LSequenceCount,LLoadCaseCount,LYearInAnalysis,LCurrentSequence,LCurrentLoadCase,LIrrigationBlockName,AChannelDemandValues);
      finally
        LFileAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetMasterControlDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
const OPNAME = 'TDemandOutputData.GetMasterControlDemandValues';
var
  LDemand          : double;
  LLoadCase,
  LYearsInAnalysis,
  LStartYear,
  LIndex,
  LInnerIndex      : integer;
  LLineData        : TStringList;
  LFactor          : double;
  LFeature         : IMasterControlFeature;
begin
  Result := False;
  try
    LYearsInAnalysis := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
    LStartYear       := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
    LLoadCase        := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.LoadCase;
    LLineData        := TStringList.Create;
    LFeature         := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[AChannel.ChannelNumber].MasterControlFeature;
    AChannelDemandValues.Clear;
    try
      if LFeature <> nil then
      begin
        for LIndex := 1 to LYearsInAnalysis do
        begin
          LLineData.Clear;
          LLineData.Add(IntToStr(LStartYear));
          for LInnerIndex := 0 to 11 do
          begin
            LDemand := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.TargetYieldByIndex[LLoadCase];
            LFactor := LFeature.FactorByMonth[LInnerIndex+1];
            LDemand := (LDemand * 1000000.0)/(365.25 * 86400.0);
            LLineData.Add(FormatFloat('######0.000',LDemand*LFactor))
          end;
          LStartYear := LStartYear + 1;
          AChannelDemandValues.Add(LLineData.CommaText);
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LLineData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetMinMaxDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
const OPNAME = 'TDemandOutputData.GetMinMaxDemandValues';
var
  LValue,
  LFactor,
  LDemand          : double;
  LYearsInAnalysis,
  LStartYear,
  LIndex,
  LInnerIndex,
  LArcCount        : integer;
  LLineData        : TStringList;
  LMonthlyFactors  : array[0..11] of double;
begin
  Result := False;
  try
    LYearsInAnalysis := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
    LStartYear       := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
    LLineData        := TStringList.Create;
    AChannelDemandValues.Clear;
    try
      LArcCount := AChannel.ChannelPenalty.ChannelPenaltyArcCount;
      for LIndex := 0 to 11 do
      begin
        LFactor := 0.0;
        for LInnerIndex := 1 to LArcCount do
        begin
          LValue := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[LInnerIndex,LIndex + 1];
          if(LValue > LFactor) then
            LFactor := LValue;
        end;
        LMonthlyFactors[LIndex] := LFactor;
      end;

      for LIndex := 1 to LYearsInAnalysis do
      begin
        LLineData.Clear;
        LLineData.Add(IntToStr(LStartYear));
        for LInnerIndex := 0 to 11 do
        begin
          LDemand := LMonthlyFactors[LInnerIndex];
          LLineData.Add(FormatFloat('######0.000',LDemand))
        end;
        LStartYear := LStartYear + 1;
        AChannelDemandValues.Add(LLineData.CommaText);
      end;
      Result := True;
    finally
      FreeAndNil(LLineData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetSpecifiedDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
const OPNAME = 'TDemandOutputData.GetSpecifiedDemandValues';
var
  LErrors : string;
begin
  Result := False;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetSpecifiedDemandFeatureFileData(AChannelDemandValues,AChannel,LErrors)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDemandOutputData.GetIFRDemandValues(AChannelDemandValues: TStrings; AChannel: IGeneralFlowChannel): boolean;
const OPNAME = 'TDemandOutputData.GetIFRDemandValues';
var
  LIFRFeature    : TIFRFeature;
  LReferenceData : TStringList;
  LMonth : integer;
  LIndex : integer;
  LDemand : string;
  LDemandData : TStringList;
begin
  Result := False;
  try
    LIFRFeature :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(AChannel.ChannelNumber);
    if LIFRFeature = nil then
      LIFRFeature :=  TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.CastAnnualIFRFeatureByNumber(AChannel.ChannelNumber);
    LReferenceData := TStringList.Create;
    LDemandData := TStringList.Create;
    try
      if LIFRFeature <> nil then
      begin
        if not LIFRFeature.GetNodeReferenceFlowData(LReferenceData) then
          Exit;
        for LIndex := 0 to LReferenceData.Count-1 do
        begin
          LDemandData.CommaText := LReferenceData[LIndex];
          if LDemandData.Count>0 then
          begin
            for LMonth := 1 to 12 do
            begin
              LDemand := FormatFloat('000.000',
                         LIFRFeature.GetRequirementFlowFromReferenceFlow(
                         LMonth,StrToFloat(LDemandData[LMonth])));
              LDemandData[LMonth] := LDemand;
            end;
          end;
          AChannelDemandValues.Add(LDemandData.CommaText);
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LReferenceData);
      FreeAndNil(LDemandData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

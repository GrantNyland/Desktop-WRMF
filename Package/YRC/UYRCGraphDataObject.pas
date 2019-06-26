//
//
//  UNIT      : Contains TYRCGraphDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCGraphDataObject;

interface

uses
  Classes,
  sysutils,
  Contnrs,
  DB,
  Types,
  UAbstractObject,
  UAbstractYRCData,
  UCurveFittingOperations,
  UCurveFittingConstants;

type
  TCoefficient = class(TObject)
  protected
    FTargetDraft,
    FACoefficient,
    FBCoefficient,
    FCCoefficient,
    FDCoefficient,
    FXBreakPoint : double;
  public
    procedure Reset;
    property TargetDraft   : double   read  FTargetDraft    write  FTargetDraft;
    property ACoefficient  : double   read  FACoefficient   write  FACoefficient;
    property BCoefficient  : double   read  FBCoefficient   write  FBCoefficient;
    property CCoefficient  : double   read  FCCoefficient   write  FCCoefficient;
    property DCoefficient  : double   read  FDCoefficient   write  FDCoefficient;
    property XBreakPoint   : double   read  FXBreakPoint    write  FXBreakPoint;
  end;

  TCoefficientList = class(TObject)
  protected
    FCoefficientList: TObjectList;
    function Get_CoefficientByIndex(AIndex : integer): TCoefficient;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function NewCoefficient : TCoefficient;
    procedure Reset;
    function CoefficientCount: integer;
    property CoefficientByIndex[AIndex : integer]: TCoefficient read  Get_CoefficientByIndex;
  end;

  TYRCLanguageStrings = class(TAbstractYRCLanguageStrings)
  protected
    FCaptionFormatStr1  : string;
    FCaptionFormatStr2  : string;
    FYearsFormatStr     : string;
    FYieldFormatStr     : string;
    FLegendCaption      : string;
    FBottomAxisCaption  : string;
    FLeftAxisCaption    : string;
    function GetCaptionFormatStr1: string; override;
    procedure SetCaptionFormatStr1(ACaptionFormatStr1: string); override;
    function GetCaptionFormatStr2: string; override;
    procedure SetCaptionFormatStr2(ACaptionFormatStr2: string); override;
    function GetYearsFormatStr: string; override;
    procedure SetYearsFormatStr(AYearsFormatStr: string); override;
    function GetYieldFormatStr: string; override;
    procedure SetYieldFormatStr(AYieldFormatStr: string); override;
    function GetLegendCaption: string; override;
    procedure SetLegendCaption(ALegendCaption: string); override;
    function GetBottomAxisCaption: string; override;
    procedure SetBottomAxisCaption(ABottomAxisCaption: string); override;
    function GetLeftAxisCaption: string; override;
    procedure SetLeftAxisCaption(ALeftAxisCaption: string); override;
  public
    procedure Reset;override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
  end;

  TYRCChartProperties  = class(TAbstractYRCChartProperties)
  protected
    FMarginBottom              : integer;
    FMarginLeft                : integer;
    FMarginRight               : integer;
    FMarginTop                 : integer;
    FBottomAxisMinimum         : double;
    FBottomAxisIncrement       : double;
    FBottomAxisMaximum         : double;
    FBottomAxisMinorTickCount  : integer;
    FLeftAxisMinimum           : double;
    FLeftAxisIncrement         : double;
    FLeftAxisMaximum           : double;
    FLeftAxisMinorTickCount    : integer;
    FRightAxisMinimum          : double;
    FRightAxisIncrement        : double;
    FRightAxisMaximum          : double;
    FRightAxisMinorTickCount   : integer;
    FLegendTopPos              : integer;
    FLegendVertMargin          : integer;
    FLabelStr                  : string;
    FZoomIndex                 : integer;
    FZoomValue                 : double;

    FShowTargetDrafts          : TShowTargetDrafts;
    FChartMode                 : TChartMode;
    FChartEditMode             : TChartEditMode;

    FHideRawPoints             : boolean;
    FHideRawLines              : boolean;
    FHideFittedPoints          : boolean;
    FShowFirmYieldLabels       : boolean;
    FShowCursorPosition        : boolean;
    FMaxYValue                 : double;
    function GetMarginBottom: integer; override;
    procedure SetMarginBottom(AMarginBottom: integer); override;
    function GetMarginLeft: integer; override;
    procedure SetMarginLeft(AMarginLeft: integer); override;
    function GetMarginRight: integer; override;
    procedure SetMarginRight(AMarginRight: integer); override;
    function GetMarginTop: integer; override;
    procedure SetMarginTop(AMarginTop: integer); override;
    function GetBottomAxisMinimum:double ; override;
    procedure SetBottomAxisMinimum(AValue: double); override;
    function GetBottomAxisIncrement: double; override;
    procedure SetBottomAxisIncrement(ABottomAxisIncrement: double); override;
    function GetBottomAxisMaximum: double; override;
    procedure SetBottomAxisMaximum(ABottomAxisMaximum: double); override;
    function GetBottomAxisMinorTickCount: integer; override;
    procedure SetBottomAxisMinorTickCount(ABottomAxisMinorTickCount: integer); override;
    function GetLeftAxisMinimum:double ; override;
    procedure SetLeftAxisMinimum(AValue: double); override;
    function GetLeftAxisIncrement: double; override;
    procedure SetLeftAxisIncrement(ALeftAxisIncrement: double); override;
    function GetLeftAxisMaximum: double; override;
    procedure SetLeftAxisMaximum(ALeftAxisMaximum: double); override;
    function GetLeftAxisMinorTickCount: integer; override;
    procedure SetLeftAxisMinorTickCount(ALeftAxisMinorTickCount: integer); override;
    function GetRightAxisMinimum:double ; override;
    procedure SetRightAxisMinimum(AValue: double); override;
    function GetRightAxisIncrement: double; override;
    procedure SetRightAxisIncrement(ARightAxisIncrement: double); override;
    function GetRightAxisMaximum: double; override;
    procedure SetRightAxisMaximum(ARightAxisMaximum: double); override;
    function GetRightAxisMinorTickCount: integer; override;
    procedure SetRightAxisMinorTickCount(ARightAxisMinorTickCount: integer); override;
    function GetLegendTopPos: integer; override;
    procedure SetLegendTopPos(ALegendTopPos: integer); override;
    function GetLegendVertMargin: integer; override;
    procedure SetLegendVertMargin(ALegendVertMargin: integer); override;
    function GetZoomIndex:integer ; override;
    procedure SetZoomIndex(AValue: integer); override;
    function GetZoomValue:double ; override;
    procedure SetZoomValue(AValue: double); override;
    function GetShowTargetDrafts:TShowTargetDrafts ; override;
    procedure SetShowTargetDrafts(AValue: TShowTargetDrafts); override;
    function GetChartMode:TChartMode ; override;
    procedure SetChartMode(AValue: TChartMode); override;
    function GetChartEditMode:TChartEditMode ; override;
    procedure SetChartEditMode(AValue: TChartEditMode); override;
    function GetHideRawpoints:boolean ; override;
    procedure SetHideRawpoints(AValue: boolean); override;
    function GetHideFittedPoints:boolean ; override;
    procedure SetHideFittedPoints(AValue: boolean); override;
    function GetHideRawLines:boolean ; override;
    procedure SetHideRawLines(AValue: boolean); override;
    function GetShowFirmYieldLabels:boolean ; override;
    procedure SetShowFirmYieldLabels(AValue: boolean); override;
    function GetShowCursorPosition:boolean ; override;
    procedure SetShowCursorPosition(AValue: boolean); override;
    function GetMaxYValue:double ; override;
    procedure SetMaxYValue(AValue: double); override;

    procedure PopulateValuesFromLabel;
    procedure PopulateLabelFromValues;
  public
    procedure Reset;override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
  end;

  TYRCFunctionConstants = class(TAbstractYRCFunctionConstants)
  protected
    FConstantsArray: TYRCConstantsArray;
    function GetConstantsArray: TYRCConstantsArray; override;
  public
    procedure Reset;override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
    function Valid: boolean;override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
  end;

  TYRCRecordPointArrayObject = class(TAbstractYRCRecordPointArrayObject)
  protected
    FYRCRecordPointArray : TYRCRecordPointArray;
    FSorted: Boolean;
    function GetYRCRecordPointArray: TYRCRecordPointArray; override;
    function GetSorted: Boolean; override;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Reset; override;
    procedure Sort(ASortOrder: TYRCRecordPointArraySortOrder); override;
    function AddPoints(APointsArray: TYRCRecordPointArray): boolean;override;
    function DeletePoint(AIndex: integer): boolean; override;
    function UpdatePoint(AIndex: integer;AXValue,AXTValue,YValue: double): boolean; override;
    function MovePointToTheEnd(var AIndex: integer): boolean;
    function ZeroYCount: integer; override;

    function CopyValuesFrom(ASource:TObject): boolean; override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
  end;

  TYRCTargetDraft = class(TAbstractYRCTargetDraft)
  protected
    FTargetDraftSavedMode        : TChartEditMode;
    FPlaneID                     : integer;
    FTargetDraftID               : integer;
    FTargetDraftXValue           : Double;
    FTargetDraftXTValue          : Double;
    FTargetDraftYValue           : Double;
    FTargetDraftRecurance        : Double;
    FTargetDraftYears            : integer;
    FIndexOf100Value             : integer;
    FDeterministicPointsState    : integer;
    FSaved                       : boolean;
    FForceCurveThrough100        : boolean;
    FDeterministicPoint1XValue   : Double;
    FDeterministicPoint1XTValue  : Double;
    FDeterministicPoint2XValue   : Double;
    FDeterministicPoint2XTValue  : Double;

    FOriginalPointsArray         : TYRCRecordPointArrayObject;
    FPureRegressionPointsArray   : TYRCRecordPointArrayObject;
    FRegressionPointsArray       : TYRCRecordPointArrayObject;
    FDeterministicPointsArray    : TYRCRecordPointArrayObject;

    FPureRegressionConstants     : TYRCFunctionConstants;
    FRegressionConstants         : TYRCFunctionConstants;
    FDeterministicConstants      : TYRCFunctionConstants;

    FPointsArray                 : TYRCFlatPointArray;
    FSavedOriginalConstants      : TYRCFunctionConstants;
    FLabelProperties             : TLabelProperties;

    function GetTargetDraftSavedMode: TChartEditMode; override;
    procedure SetChartEditMode(ATargetDraftMode: TChartEditMode); override;
    function GetPlaneID: integer; override;
    procedure SetPlaneID(APlaneID: integer); override;
    function GetTargetDraftID: integer; override;
    procedure SetTargetDraftID(ATargetDraftID: integer); override;
    function GetTargetDraftXValue: Double; override;
    procedure SetTargetDraftXValue(ATargetDraftXValue: Double); override;
    function GetTargetDraftXTValue: Double;override;
    procedure SetTargetDraftXTValue(ATargetDraftXTValue: Double); override;
    function GetTargetDraftYValue: Double; override;
    procedure SetTargetDraftYValue(ATargetDraftYValue: Double); override;
    function GetTargetDraftRecurance: Double; override;
    procedure SetTargetDraftRecurance(ATargetDraftRecurance: Double); override;
    function GetTargetDraftYears: integer; override;
    procedure SetTargetDraftYears(ATargetDraftYears: integer); override;
    function GetYValueAt100 : double; override;
    procedure SetYValueAt100(AValue: double); override;
    procedure AddYValueAt100(AValue: double);
    procedure UpdateYValueAt100(AValue: double);
    procedure DeleteYValueAt100(AValue: double);
    function CalculateTransformedXValue(ASequence, AFailures: integer): double;
    function GetFormula : string; override;
    function GetForceCurveThrough100: boolean; override;
    procedure SetForceCurveThrough100(AForce:boolean); override;
    function GetOriginalPointsArray        : TAbstractYRCRecordPointArrayObject; override;
    function GetRegressionPointsArray      : TAbstractYRCRecordPointArrayObject;override;
    function GetPureRegressionPointsArray  : TAbstractYRCRecordPointArrayObject; override;
    function GetDeterministicPointsArray   : TAbstractYRCRecordPointArrayObject; override;
    function GetPureRegressionConstants    : TAbstractYRCFunctionConstants; override;
    function GetRegressionConstants        : TAbstractYRCFunctionConstants; override;
    function GetDeterministicConstants     : TAbstractYRCFunctionConstants; override;
    function GetLabelProperties            : TLabelProperties; override;

    function GetTargetDraftSavedModeFromInt(ATargetDraftSavedMode: integer): TChartEditMode;
    function AddTwoPointsArrays(Const AFirstSource,ASecondSource:TYRCRecordPointArray;
             var AResultArray: TYRCRecordPointArray): boolean;
    function DoRegressionCurveFitting: boolean;
    function DoCurveFitting (ACurveType            : TCurveType;
                             ACurveConstantsObject : TYRCFunctionConstants;
                             AYXPairsArrayObject   : TYRCRecordPointArrayObject;
                             AForce                : Boolean): boolean;
    procedure CalculateTargetDraftProperties;
    function GetRegressionFourPoints(var AYRCFourPointsArray: TYRCFourPointsArray): boolean;
    function ValidateUpdateDeterministicPoint(AIndex: integer;AXValue,AXTValue,AYValue: double): boolean;
    procedure RefreshDeterministicMiddleTwoPoints;
  public
    procedure CreateMemberObjects;override;
    procedure DestroyMemberObjects;override;
    procedure Reset;override;
    function CurveFitted: boolean; override;
    function YValueAt100Added: boolean; override;
    function RegressionPointsAdded: boolean; override;
    function DeterministicPointsChanged: boolean; override;
    function ValidFourPoints(AFourPoints: TYRCFourPointsArray): boolean;
    procedure ResetPureRegressionPoints; override;
    procedure ResetRegressionPoints; override;
    procedure ResetDeterministicPoints; override;
    function AddRegressionPoints(APointsArray: TYRCRecordPointArray): boolean; override;
    function DeleteRegressionPoint(AIndex: integer): boolean; override;
    function UpdateRegressionPoint(AIndex: integer;AXValue,AXTValue,AYValue: double): boolean; override;
    function UpdateDeterministicPoint(AIndex: integer;AXValue,AXTValue,AYValue: double): boolean;  override;

    function LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function TransformCurvePoints(AOriginalPointsArray : TAbstractYRCRecordPointArrayObject) : boolean; override;
    procedure ApplyPlottingBase(APlottingBase: integer); override;
    function GetCurveConstants(AConstants: TYRCFunctionConstants): boolean;
  end;

  TYRCPlane = class(TAbstractYRCPlane)
  protected
    FPlaneID                   : integer;
    FPlaneYears                : integer;
    FTargetDraftIndex          : integer;
    FTargetDraftList           : TObjectList;
    FYXPointArrayObject        : TYRCRecordPointArrayObject;
    FYearsArrayObject          : TYRCRecordPointArrayObject;
    FAssurancePointArrayObject : TYRCRecordPointArrayObject;
    FAssuranceYearsArrayObject : TYRCRecordPointArrayObject;

    function GetTargetDraft(AIndex: Integer): TAbstractYRCTargetDraft; override;
    function GetPlaneYears: integer; override;
    function GetPlaneID: integer; override;
    procedure SetPlaneID(APlaneID: integer); override;
    function GetYXPointArrayObject: TAbstractYRCRecordPointArrayObject; override;
    function GetYearsArrayObject: TAbstractYRCRecordPointArrayObject; override;
    function GetAssurancePointArrayObject: TAbstractYRCRecordPointArrayObject; override;
    function GetAssuranceYearsArrayObject: TAbstractYRCRecordPointArrayObject; override;
    function GetTargetDraftIndex: integer; override;
    procedure SetTargetDraftIndex(ATargetDraftIndex: integer); override;
    function GetSelectedTargetDraft:TAbstractYRCTargetDraft; override;
    procedure PopulateYXPointArrayObject;
    function AddTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): Integer; override;
    function DeleteTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): boolean; override;

    function CalculateDeterministicPointsFromCoefficients(ACoefficientList:TCoefficientList): boolean;
    function GetTargetDraftPerTargetValue(AValue: double):TYRCTargetDraft;

    function CopyValuesFrom(ASource:TObject): boolean; override;
    function LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean; override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
  public
    procedure CreateMemberObjects;override;
    procedure DestroyMemberObjects;override;
    procedure Reset;override;
    function Changed: boolean;
    function LoadCoefficientFile: boolean; override;
    function TargetDraftCount: integer; override;
    function GetCastTargetDraft(AIndex: Integer): TYRCTargetDraft;
    function UpdateSelectedAssuranceIntervalSaved(AAssuranceIntervalArraySaved:TIntegerArray):boolean; override;
    function UpdateSelectedAssuranceIntervalDefault(AAssuranceIntervalArraySaved:TIntegerArray):boolean; override;
    function UpdateSelectedAssuranceIntervalYears(AAssuranceIntervalArraySaved:TIntegerArray):boolean; override;
    procedure ApplyPlottingBase(APlottingBase: integer); override;
  end;

  TYRCGraphDataObject = class(TAbstractYRCGraphDataObject)
  protected
    FPlaneIndex,
    FPlottingBase: integer;
    FErrorMsg,
    FChartName: string;
    FPlanesList: TObjectList;
    FYRCLanguageStrings: TYRCLanguageStrings;
    FYRCChartProperties:TYRCChartProperties;
    FAssuranceIntervalDefaultArray : TIntegerArray;
    FAssuranceIntervalSavedArray   : TIntegerArray;
    FAssuranceIntervalYearsArray   : TIntegerArray;

    function GetPlaneIndex: integer; override;
    procedure SetPlaneIndex(APlaneIndex: Integer); override;
    function GetPlottingBase: integer; override;
    function GetPeriodLength: integer; override;
    procedure SetPeriodLength(AYearNumber: integer); override;
    procedure SetPlottingBase(APlottingBase: Integer); override;
    function GetErrorMsg: string; override;
    procedure SetErrorMsg(AErrorMsg: string); override;
    function GetChartName: boolean; override;
    procedure SetChartName(AChartName: string); override;
    function GetYRCLanguageStrings: TAbstractYRCLanguageStrings; override;
    function GetYRCChartProperties: TAbstractYRCChartProperties; override;

    function GetPlane(AIndex: Integer): TAbstractYRCPlane; override;
    function GetPlaneIndexByYearNumber(AYearNumber: Integer): Integer;
    function GetTargetDraft(APlaneIndex,ATargetDraftIndex: Integer): TAbstractYRCTargetDraft; override;
    function GetSelectedPlane: TAbstractYRCPlane; override;
    function GetSelectedTargetDraft: TAbstractYRCTargetDraft; override;
    function GetSelectedTargetDraftIndex: integer; override;
    function GetTargetDraftSavedMode(APlaneIndex,ATargetDraftIndex: Integer): TChartEditMode; override;
    function GetAssuranceIntervalSavedArray: TIntegerArray; override;
    procedure SetAssuranceIntervalSavedArray(AValue: TIntegerArray); override;
    function GetAssuranceIntervalDefaultArray: TIntegerArray; override;
    procedure SetAssuranceIntervalDefaultArray(AValue: TIntegerArray); override;
    function GetAssuranceIntervalYearsArray: TIntegerArray; override;
    procedure SetAssuranceIntervalYearsArray(AValue: TIntegerArray); override;
    function UpdateSelectedAssuranceIntervalSaved:boolean; override;
    function UpdateSelectedAssuranceIntervalDefault:boolean; override;
    function UpdateSelectedAssuranceIntervalYears:boolean; override;

    function GetPlanesCount: integer; override;
    function GetYearsCount: integer; override;
    function GetSequencesCount: integer; override;

    function GetChartNumberFromPrimaryKey(Const APrimaryKeyProperty : array of string): boolean;
    function SaveChartIdToDB(Const APrimaryKeyProperty : array of string): boolean;

    procedure CalculateMaxYield;
    //procedure CalculateSummaries;
    function CalculateTransformedXValue(ASequence, AFailures: integer): double;
    function CreateCoefficientFile: boolean;
    procedure LoadSelectedAssuranceInterval;
    procedure SaveSelectedAssuranceInterval;
    function AddPlane(AYRCPlane: TAbstractYRCPlane): Integer; override;
    function DeleteChartDatabaseData(Const APrimaryKeyProperty : array of string): boolean;
  public
    procedure CreateMemberObjects;override;
    procedure DestroyMemberObjects;override;
    procedure Reset;override;
    function Changed: boolean;
    function GetCastPlane(AIndex: Integer): TYRCPlane;
    function Initialise: boolean; override;
    function DeleteTargetDraft(ATargetDraftIndex: Integer): boolean; override;
    function MergeData(ANewData:TAbstractYRCGraphDataObject; ADeleteIndexCommaText,AAddIndexCommaText: string): boolean; override;
    function CalculateRIOnTargetDraft(APlaneIndex, ATargetDraftIndex : integer; AExcProb : double) : double; override;
    function MinYearNumber: integer;override;
    function MaxYearNumber: integer;override;
    function SaveAssuranceInterval : boolean; override;
    procedure ResetSelectedTargetDraftPoints; override;
    function TargetDraftValuesCommaText: string; override;

    function LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function DeleteChartData(Const APrimaryKeyProperty : array of string): boolean; override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
  end;

implementation


uses
  Math,
  VCL.Forms,
  VCL.Dialogs,
  UUtilities,
  UDataSetType,
  UConstants,
  USumOutDataObjects,
  UYRCModelDataObject,
  UErrorHandlingOperations;

function IsPointsEqual(const AYRCPoint1, AYRCPoint2 : TYRCPoint):boolean;
const OPNAME = 'IsPointsEqual';
begin
  Result := False;
  try
    Result := CompareDouble(AYRCPoint1.XValue, AYRCPoint2.XValue,4) and
              CompareDouble(AYRCPoint1.YValue, AYRCPoint2.YValue,4);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCRecordPointArrayObject }

procedure TYRCRecordPointArrayObject.CreateMemberObjects;
const OPNAME = 'TYRCRecordPointArrayObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    Finalize(FYRCRecordPointArray);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRecordPointArrayObject.DestroyMemberObjects;
const OPNAME = 'TYRCRecordPointArrayObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    Finalize(FYRCRecordPointArray);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TYRCRecordPointArrayObject.Reset;
const OPNAME = 'TYRCRecordPointArrayObject.Reset';
begin
  inherited Reset;
  try
    FSorted := False;
    Finalize(FYRCRecordPointArray);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TYRCRecordPointArrayObject.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.CopyValuesFrom';
var
  LCount: integer;
  LYRCRecordPointArrayObject: TYRCRecordPointArrayObject;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCRecordPointArrayObject) then
    begin
      Result := False;
      LYRCRecordPointArrayObject := TYRCRecordPointArrayObject(ASource);
      FSorted := LYRCRecordPointArrayObject.FSorted;
      SetLength(FYRCRecordPointArray,Length(LYRCRecordPointArrayObject.YRCRecordPointArray));
      for LCount := 0 to High(LYRCRecordPointArrayObject.YRCRecordPointArray) do
      begin
        FYRCRecordPointArray[LCount].YValue  := LYRCRecordPointArrayObject.YRCRecordPointArray[LCount].YValue;
        FYRCRecordPointArray[LCount].XValue  := LYRCRecordPointArrayObject.YRCRecordPointArray[LCount].XValue;
        FYRCRecordPointArray[LCount].XTValue := LYRCRecordPointArrayObject.YRCRecordPointArray[LCount].XTValue;
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRecordPointArrayObject.Sort(ASortOrder: TYRCRecordPointArraySortOrder);
const OPNAME = 'TYRCRecordPointArrayObject.Sort';
var
  LCount,
  LIndex,
  LPos : integer;
  LSortedValues: TStringList;
  LYValueStr: string;
  LRecordPointArray : TYRCRecordPointArray;
begin
  FSorted := False;
  try
    LSortedValues := TStringList.Create;
    SetLength(LRecordPointArray, Length(FYRCRecordPointArray));
    try
      LSortedValues.Sorted := True;
      LSortedValues.Duplicates := dupAccept;
      for LCount := Low(FYRCRecordPointArray) to High(FYRCRecordPointArray) do
      begin
        LYValueStr := FormatFloat('0000000.000',FYRCRecordPointArray[LCount].YValue);
        LSortedValues.AddObject(LYValueStr,TObject(LCount));
      end;

      case ASortOrder of
        soAscending:
        begin
          for LCount := 0 to LSortedValues.Count-1 do
          begin
            LIndex := integer(LSortedValues.Objects[LCount]);
            LRecordPointArray[LCount].YValue  := FYRCRecordPointArray[LIndex].YValue;
            LRecordPointArray[LCount].XValue  := FYRCRecordPointArray[LIndex].XValue;
            LRecordPointArray[LCount].XTValue := FYRCRecordPointArray[LIndex].XTValue;
          end;
        end;
        soDescending:
        begin
          LPos := 0;
          for LCount := LSortedValues.Count-1  downto 0 do
          begin
            LIndex := integer(LSortedValues.Objects[LCount]);
            LRecordPointArray[LPos].YValue  := FYRCRecordPointArray[LIndex].YValue;
            LRecordPointArray[LPos].XValue  := FYRCRecordPointArray[LIndex].XValue;
            LRecordPointArray[LPos].XTValue := FYRCRecordPointArray[LIndex].XTValue;
            LPos := LPos + 1;
          end;
        end
      end;//case

      for LCount := Low(LRecordPointArray) to High(LRecordPointArray) do
      begin
        FYRCRecordPointArray[LCount].YValue  := LRecordPointArray[LCount].YValue;
        FYRCRecordPointArray[LCount].XValue  := LRecordPointArray[LCount].XValue;
        FYRCRecordPointArray[LCount].XTValue := LRecordPointArray[LCount].XTValue;
      end;
      FSorted := True;
    finally
      LSortedValues.Free;
      Finalize(LRecordPointArray);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.ZeroYCount: integer;
const OPNAME = 'TYRCRecordPointArrayObject.ZeroYCount';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := Low(FYRCRecordPointArray) to High(FYRCRecordPointArray) do
      if (FYRCRecordPointArray[LCount].YValue < 0.001) then
        Result := Result + 1;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.AddPoints(APointsArray: TYRCRecordPointArray): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.AddPoints';
var
  LStart,
  LCount,
  LIndex: integer;
begin
  Result := False;
  try
    if(Length(APointsArray) > 0) then
    begin

      LStart := Length(FYRCRecordPointArray);
      SetLength(FYRCRecordPointArray,
                Length(FYRCRecordPointArray) + Length(APointsArray));
      LIndex := 0;
      for LCount := LStart to High(FYRCRecordPointArray) do
      begin
        FYRCRecordPointArray[LCount].XValue  := APointsArray[LIndex].XValue;
        FYRCRecordPointArray[LCount].XTValue := APointsArray[LIndex].XTValue;
        FYRCRecordPointArray[LCount].YValue  := APointsArray[LIndex].YValue;
        LIndex := LIndex + 1;
      end;

      Loaded := True;
      Changed := True;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.DeletePoint(AIndex: integer): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.DeletePoint';
var
  LCount: integer;
begin
  Result := False;
  try
    if(AIndex >= Low(FYRCRecordPointArray)) and
      (AIndex <= High(FYRCRecordPointArray)) then
    begin
      for LCount := AIndex to High(FYRCRecordPointArray) -1 do
      begin
        FYRCRecordPointArray[LCount].XValue  :=  FYRCRecordPointArray[LCount+1].XValue;
        FYRCRecordPointArray[LCount].XTValue :=  FYRCRecordPointArray[LCount+1].XTValue;
        FYRCRecordPointArray[LCount].YValue  :=  FYRCRecordPointArray[LCount+1].YValue;
      end;
      SetLength(FYRCRecordPointArray,Length(FYRCRecordPointArray) -1);
      Changed := True;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.UpdatePoint(AIndex: integer; AXValue,AXTValue,YValue: double): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.UpdatePoint';
begin
  Result := False;
  try
    if(AIndex >= Low(FYRCRecordPointArray)) and
      (AIndex <= High(FYRCRecordPointArray)) then
    begin
      FYRCRecordPointArray[AIndex].XValue  := AXValue;
      FYRCRecordPointArray[AIndex].XTValue := AXTValue;
      FYRCRecordPointArray[AIndex].YValue  := YValue;

      Changed := True;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.MovePointToTheEnd(var AIndex: integer): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.MovePointToTheEnd';
var
  LPointsArray: TYRCRecordPointArray;
begin
  Result := False;
  try
    if(AIndex >= Low(FYRCRecordPointArray)) and
      (AIndex <= High(FYRCRecordPointArray)) then
    begin
      if(Length(FYRCRecordPointArray) = 0) then
        Result := False
      else
      if(Length(FYRCRecordPointArray) = 1) then
        Result := True
      else
      begin
        SetLength(LPointsArray,1);
        try
          LPointsArray[0].XValue  := FYRCRecordPointArray[AIndex].XValue;
          LPointsArray[0].XTValue := FYRCRecordPointArray[AIndex].XTValue;
          LPointsArray[0].YValue  := FYRCRecordPointArray[AIndex].YValue;
          if DeletePoint(AIndex) then
          begin
            Result := AddPoints(LPointsArray);
            if Result then
            begin
              Changed := True;
              AIndex := High(FYRCRecordPointArray);
            end;
          end;
        finally
          Finalize(LPointsArray);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.LoadDataFromDB';
var
 LDatabaseName,
 LWhereClause: string;
 LDataSet: TAbstractModelDataSet;
 LIndex: integer;
begin
  Result := False;
  try
    Self.Reset;
    Result := (Length(APrimaryKeyProperty) >= 9);
    Result := Result and GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          ('SELECT Model,StudyAreaName,SubArea,Scenario, ChartID,PlaneNumber,TargetDraftID,' +
           ' CurveType,PointIndex,XValue,YValue FROM yrcTargetDraftPoint' +
           ' WHERE ' + LWhereClause +
           ' ORDER BY Model,StudyAreaName,SubArea,Scenario, ChartID,PlaneNumber,TargetDraftID,CurveType,PointIndex');
        LDataSet.DataSet.Open;

        if (LDataSet.DataSet.RecordCount <= 0) then
          Result := True
        else
        begin
          SetLength(Self.FYRCRecordPointArray,LDataSet.DataSet.RecordCount);
          LIndex := 0;
          while not LDataSet.DataSet.EOF do
          begin
            Self.FYRCRecordPointArray[LIndex].YValue  := LDataSet.DataSet.FieldByName('YValue').AsFloat;
            Self.FYRCRecordPointArray[LIndex].XValue  := LDataSet.DataSet.FieldByName('XValue').AsFloat;
            Self.FYRCRecordPointArray[LIndex].XTValue := LDataSet.DataSet.FieldByName('XValue').AsFloat;
            LDataSet.DataSet.Next;
            LIndex := LIndex + 1;
          end;
          Result := inherited LoadDataFromDB(APrimaryKeyProperty);
          FSorted := Result;
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCRecordPointArrayObject.SaveDataToDB';
var
  LDatabaseName,
  LWhereClause: string;
  LTableName: string;
  LDataSet: TAbstractModelDataSet;
  LCount: integer;
begin
  Result := False;
  try
    if (Length(APrimaryKeyProperty) < 9) then
      Exit ;

    {if not Changed then
    begin
      Result := True;
      Exit;
    end;
    }
    if GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause)then
    begin
      LTableName := 'yrcTargetDraftPoint';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
       Exit;

      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          ('INSERT INTO yrcTargetDraftPoint ' +
           '(Model,StudyAreaName,SubArea,Scenario, ChartID,PlaneNumber,TargetDraftID, ' +
           'CurveType,PointIndex,XValue,YValue) VALUES ' +
           '(:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:PlaneNumber, ' +
           ':TargetDraftID,:CurveType,:PointIndex,:XValue,:YValue)');

        for LCount := Low(Self.FYRCRecordPointArray) to High(Self.FYRCRecordPointArray) do
        begin
          LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                              'ChartID','PlaneNumber','TargetDraftID',
                              'CurveType','PointIndex','XValue','YValue'],
                             [APrimaryKeyProperty[1], APrimaryKeyProperty[2],
                              APrimaryKeyProperty[3], APrimaryKeyProperty[4],
                              APrimaryKeyProperty[5],APrimaryKeyProperty[6],
                              APrimaryKeyProperty[7],APrimaryKeyProperty[8],
                              IntToStr(LCount),
                              FloatToStr(Self.FYRCRecordPointArray[LCount].XValue),
                              FloatToStr(Self.FYRCRecordPointArray[LCount].YValue)]);
          LDataSet.ExecSQL;
          LDataSet.DataSet.Close;
        end;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
      Result := Result and inherited SaveDataToDB(APrimaryKeyProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.GetSorted: Boolean;
const OPNAME = 'TYRCRecordPointArrayObject.GetSorted';
begin
  Result := False;
  try
    Result := FSorted;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRecordPointArrayObject.GetYRCRecordPointArray: TYRCRecordPointArray;
const OPNAME = 'TYRCRecordPointArrayObject.GetYRCRecordPointArray';
begin
  Result := nil;
  try
    Result := FYRCRecordPointArray;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCLanguageStrings }

procedure TYRCLanguageStrings.Reset;
const OPNAME = 'TYRCLanguageStrings.Reset';
begin
  inherited Reset;
  try
    FCaptionFormatStr1  := 'YRC.CaptionFormatStr1';
    FCaptionFormatStr2  := 'YRC.CaptionFormatStr2';
    FYearsFormatStr     := 'YRC.YearsFormatStr';
    FYieldFormatStr     := 'YRC.YieldFormatStr';
    FLegendCaption      := 'YRC.LegendCaption';
    FBottomAxisCaption  := 'YRC.BottomAxisCaption';
    FLeftAxisCaption    := 'YRC.LeftAxisCaption';
    Changed            := True;
    Loaded             := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCLanguageStrings.CopyValuesFrom';
var
  LYRCLanguageStrings: TYRCLanguageStrings;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCLanguageStrings) then
    begin
      Result := False;
      LYRCLanguageStrings := TYRCLanguageStrings(ASource);
      FCaptionFormatStr1  := LYRCLanguageStrings.FCaptionFormatStr1;
      FCaptionFormatStr2  := LYRCLanguageStrings.FCaptionFormatStr2;
      FYearsFormatStr     := LYRCLanguageStrings.FYearsFormatStr;
      FYieldFormatStr     := LYRCLanguageStrings.FYieldFormatStr;
      FLegendCaption      := LYRCLanguageStrings.FLegendCaption;
      FBottomAxisCaption  := LYRCLanguageStrings.FBottomAxisCaption;
      FLeftAxisCaption    := LYRCLanguageStrings.FLeftAxisCaption;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCLanguageStrings.LoadDataFromDB';
var
 LDatabaseName,
 LWhereClause: string;
 LTableName: string;
 LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    Self.Reset;
    if (Length(APrimaryKeyProperty) >= 6)  and
       GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause)then
    begin
      LTableName := 'yrcLanguageStrings';
      ChartNumber  := StrToInt(APrimaryKeyProperty[5]);

      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          ('SELECT Model,StudyAreaName,SubArea,Scenario,ChartID,CaptionFormat1,CaptionFormat2,' +
           'YearsFormat,YieldFormat,LegendCaption,BottomAxisCaption,LeftAxisCaption' +
           ' FROM yrcLanguageStrings WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;
        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          Self.ChartNumber        := LDataSet.DataSet.FieldByName('ChartID').AsInteger;
          Self.CaptionFormatStr1  := Trim(LDataSet.DataSet.FieldByName('CaptionFormat1').AsString);
          Self.CaptionFormatStr2  := Trim(LDataSet.DataSet.FieldByName('CaptionFormat2').AsString);
          Self.YearsFormatStr	    := Trim(LDataSet.DataSet.FieldByName('YearsFormat').AsString);
          Self.YieldFormatStr	    := Trim(LDataSet.DataSet.FieldByName('YieldFormat').AsString);
          Self.LegendCaption	    := Trim(LDataSet.DataSet.FieldByName('LegendCaption').AsString);
          Self.BottomAxisCaption  := Trim(LDataSet.DataSet.FieldByName('BottomAxisCaption').AsString);
          Self.LeftAxisCaption	  := Trim(LDataSet.DataSet.FieldByName('LeftAxisCaption').AsString);
          Result := inherited LoadDataFromDB(APrimaryKeyProperty);
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCLanguageStrings.SaveDataToDB';
var
 LDatabaseName,
 LWhereClause: string;
 LTableName: string;
 LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    if(Length(APrimaryKeyProperty) < 6) then
     Exit;

    {if not Changed then
    begin
      Result := True;
      Exit;
    end;}

    Result := GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      LTableName := 'yrcLanguageStrings';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
       Exit;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          ('INSERT INTO yrcLanguageStrings ' +
           '(Model,StudyAreaName,SubArea,Scenario,ChartID,CaptionFormat1,CaptionFormat2,' +
           'YearsFormat,YieldFormat,LegendCaption,BottomAxisCaption,LeftAxisCaption) VALUES ' +
           '(:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:CaptionFormat1,:CaptionFormat2,' +
           ':YearsFormat,:YieldFormat,:LegendCaption,:BottomAxisCaption,:LeftAxisCaption)');

        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'ChartID','CaptionFormat1','CaptionFormat2','YearsFormat',
                            'YieldFormat','LegendCaption','BottomAxisCaption','LeftAxisCaption'],
                           [APrimaryKeyProperty[1], APrimaryKeyProperty[2],
                            APrimaryKeyProperty[3], APrimaryKeyProperty[4],
                            IntToStr(Self.ChartNumber),
                            Self.CaptionFormatStr1, Self.CaptionFormatStr2,
                            Self.YearsFormatStr, Self.YieldFormatStr, Self.LegendCaption,
                            Self.BottomAxisCaption, Self.LeftAxisCaption]);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
      Result := Result and inherited SaveDataToDB(APrimaryKeyProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetBottomAxisCaption: string;
const OPNAME = 'TYRCLanguageStrings.GetBottomAxisCaption';
begin
  Result := '';
  try
    Result := FBottomAxisCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetCaptionFormatStr1: string;
const OPNAME = 'TYRCLanguageStrings.GetCaptionFormatStr1';
begin
  Result := '';
  try
    Result := FCaptionFormatStr1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetCaptionFormatStr2: string;
const OPNAME = 'TYRCLanguageStrings.GetCaptionFormatStr2';
begin
  Result := '';
  try
    Result := FCaptionFormatStr2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetLeftAxisCaption: string;
const OPNAME = 'TYRCLanguageStrings.GetLeftAxisCaption';
begin
  Result := '';
  try
    Result := FLeftAxisCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetLegendCaption: string;
const OPNAME = 'TYRCLanguageStrings.GetLegendCaption';
begin
  Result := '';
  try
    Result := FLegendCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetYieldFormatStr: string;
const OPNAME = 'TYRCLanguageStrings.GetYieldFormatStr';
begin
  Result := '';
  try
    Result := FYieldFormatStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetBottomAxisCaption(ABottomAxisCaption: string);
const OPNAME = 'TYRCLanguageStrings.SetBottomAxisCaption';
begin
  try
    if(FBottomAxisCaption <> ABottomAxisCaption) then
      FBottomAxisCaption := ABottomAxisCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetCaptionFormatStr1(ACaptionFormatStr1: string);
const OPNAME = 'TYRCLanguageStrings.SetCaptionFormatStr1';
begin
  try
    if(FCaptionFormatStr1 <> ACaptionFormatStr1) then
      FCaptionFormatStr1 := ACaptionFormatStr1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetCaptionFormatStr2(ACaptionFormatStr2: string);
const OPNAME = 'TYRCLanguageStrings.SetCaptionFormatStr2';
begin
  try
    if(FCaptionFormatStr2 <> ACaptionFormatStr2) then
      FCaptionFormatStr2 := ACaptionFormatStr2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetLeftAxisCaption(ALeftAxisCaption: string);
const OPNAME = 'TYRCLanguageStrings.SetLeftAxisCaption';
begin
  try
    if(FLeftAxisCaption <> ALeftAxisCaption) then
      FLeftAxisCaption := ALeftAxisCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetLegendCaption(ALegendCaption: string);
const OPNAME = 'TYRCLanguageStrings.SetLegendCaption';
begin
  try
    if(FLegendCaption <> ALegendCaption) then
      FLegendCaption := ALegendCaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetYearsFormatStr(AYearsFormatStr: string);
const OPNAME = 'TYRCLanguageStrings.SetYearsFormatStr';
begin
  try
    if(FYearsFormatStr <> AYearsFormatStr) then
      FYearsFormatStr := AYearsFormatStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCLanguageStrings.SetYieldFormatStr(AYieldFormatStr: string);
const OPNAME = 'TYRCLanguageStrings.SetYieldFormatStr';
begin
  try
    if(FYieldFormatStr <> AYieldFormatStr) then
      FYieldFormatStr := AYieldFormatStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCLanguageStrings.GetYearsFormatStr: string;
const OPNAME = 'TYRCLanguageStrings.GetYearsFormatStr';
begin
  Result := '';
  try
    Result := FYearsFormatStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCChartProperties }

function TYRCChartProperties.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCChartProperties.CopyValuesFrom';
var
  LYRCChartProperties: TYRCChartProperties;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCChartProperties) then
    begin
      Result := False;
      LYRCChartProperties        := TYRCChartProperties(ASource);
      FMarginBottom              := LYRCChartProperties.MarginBottom;
      FMarginLeft                := LYRCChartProperties.MarginLeft;
      FMarginRight               := LYRCChartProperties.MarginRight;
      FMarginTop                 := LYRCChartProperties.MarginTop;
      FBottomAxisIncrement       := LYRCChartProperties.BottomAxisIncrement;
      FBottomAxisMaximum         := LYRCChartProperties.BottomAxisMaximum;
      FBottomAxisMinorTickCount  := LYRCChartProperties.BottomAxisMinorTickCount;
      FLeftAxisIncrement         := LYRCChartProperties.LeftAxisIncrement;
      FLeftAxisMaximum           := LYRCChartProperties.LeftAxisMaximum;
      FLeftAxisMinorTickCount    := LYRCChartProperties.LeftAxisMinorTickCount;
      FRightAxisIncrement        := LYRCChartProperties.RightAxisIncrement;
      FRightAxisMaximum          := LYRCChartProperties.RightAxisMaximum;
      FRightAxisMinorTickCount   := LYRCChartProperties.RightAxisMinorTickCount;
      FLegendTopPos              := LYRCChartProperties.LegendTopPos;
      FLegendVertMargin          := LYRCChartProperties.LegendVertMargin;
      FZoomIndex                 := LYRCChartProperties.ZoomIndex;
      FZoomValue                 := LYRCChartProperties.ZoomValue;

      FShowTargetDrafts          := LYRCChartProperties.ShowTargetDrafts;
      FChartMode                 := LYRCChartProperties.ChartMode;
      FChartEditMode             := LYRCChartProperties.ChartEditMode;

      FHideRawPoints             := LYRCChartProperties.HideRawPoints;
      FHideFittedPoints          := LYRCChartProperties.HideFittedPoints;
      FHideRawLines              := LYRCChartProperties.HideRawLines;
      FShowFirmYieldLabels       := LYRCChartProperties.ShowFirmYieldLabels;
      FShowCursorPosition        := LYRCChartProperties.ShowCursorPosition;
      FMaxYValue                 := LYRCChartProperties.MaxYValue;
      FBottomAxisMinimum         := LYRCChartProperties.BottomAxisMinimum;
      FLeftAxisMinimum           := LYRCChartProperties.LeftAxisMinimum;
      FRightAxisMinimum          := LYRCChartProperties.RightAxisMinimum;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.Reset;
const OPNAME = 'TYRCChartProperties.Reset';
begin
  inherited Reset;
  try
    FMarginBottom              := 0;
    FMarginLeft                := 2;
    FMarginRight               := 2;
    FMarginTop                 := 2;
    FBottomAxisMinimum         := 0;
    FBottomAxisIncrement       := 5;
    FBottomAxisMaximum         := 100;
    FBottomAxisMinorTickCount  := 4;
    FLeftAxisMinimum           := 0;
    FLeftAxisIncrement         := 50;
    FLeftAxisMaximum           := 250;
    FLeftAxisMinorTickCount    := 4;
    FRightAxisMinimum          := 0;
    FRightAxisIncrement        := 50;
    FRightAxisMaximum          := 250;
    FRightAxisMinorTickCount   := 4;
    FLegendTopPos              := 0;
    FLegendVertMargin          := 4;
    FZoomIndex                 := 0;
    FZoomValue                 := 0.0;

    FShowTargetDrafts          := stdAll;
    FChartMode                 := cmView;
    FChartEditMode             := tdmNone;

    FHideRawPoints             := True;
    FHideFittedPoints          := False;
    FHideRawLines              := True;
    FShowFirmYieldLabels       := True;
    FShowCursorPosition        := True;
    FMaxYValue                 := -1.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCChartProperties.LoadDataFromDB';
var
 LDatabaseName,
 LWhereClause: string;
 LTableName: string;
 LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    Self.Reset;
    if (Length(APrimaryKeyProperty) >= 6)  and
       GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause)then
    begin
      LTableName := 'yrcChartProperty';
      ChartNumber  := StrToInt(APrimaryKeyProperty[5]);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' SELECT Model,StudyAreaName,SubArea,Scenario,ChartID,' +
           ' MarginBottom,MarginLeft,MarginRight,MarginTop,BottomAxisIncrement,' +
           ' BottomAxisMaximum,BottomAxisMinorTickCount,LeftAxisIncrement,LeftAxisMaximum,' +
           ' LeftAxisMinorTickCount,RightAxisIncrement,RightAxisMaximum,RightAxisMinorTickCount,' +
           ' LegendTopPos,LegendVertMargin,Label,BottomAxisMinimum,LeftAxisMinimum,RightAxisMinimum'+
           ' FROM yrcChartProperty WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;

        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          Self.FChartNumber      	        :=      LDataSet.DataSet.FieldByName('ChartID').AsInteger;
          Self.FMarginBottom		          :=      LDataSet.DataSet.FieldByName('MarginBottom').AsInteger;
          Self.FMarginLeft		            :=      LDataSet.DataSet.FieldByName('MarginLeft').AsInteger;
          Self.FMarginRight		            :=      LDataSet.DataSet.FieldByName('MarginRight').AsInteger;
          Self.FMarginTop		              :=      LDataSet.DataSet.FieldByName('MarginTop').AsInteger;
          Self.FBottomAxisMinimum	        :=      LDataSet.DataSet.FieldByName('BottomAxisMinimum').AsFloat;
          Self.FBottomAxisIncrement	      :=      LDataSet.DataSet.FieldByName('BottomAxisIncrement').AsFloat;
          Self.FBottomAxisMaximum         :=      LDataSet.DataSet.FieldByName('BottomAxisMaximum').AsFloat;
          Self.FBottomAxisMinorTickCount  :=      LDataSet.DataSet.FieldByName('BottomAxisMinorTickCount').AsInteger;
          Self.FLeftAxisMinimum  	        :=      LDataSet.DataSet.FieldByName('LeftAxisMinimum').AsFloat;
          Self.FLeftAxisIncrement	        :=      LDataSet.DataSet.FieldByName('LeftAxisIncrement').AsFloat;
          Self.FLeftAxisMaximum		        :=      LDataSet.DataSet.FieldByName('LeftAxisMaximum').AsFloat;
          Self.FLeftAxisMinorTickCount	  :=      LDataSet.DataSet.FieldByName('LeftAxisMinorTickCount').AsInteger;
          Self.FRightAxisMinimum  	      :=      LDataSet.DataSet.FieldByName('RightAxisMinimum').AsFloat;
          Self.FRightAxisIncrement	      :=      LDataSet.DataSet.FieldByName('RightAxisIncrement').AsFloat;
          Self.FRightAxisMaximum	        :=      LDataSet.DataSet.FieldByName('RightAxisMaximum').AsFloat;
          Self.FRightAxisMinorTickCount	  :=      LDataSet.DataSet.FieldByName('RightAxisMinorTickCount').AsInteger;
          Self.FLegendTopPos		          :=      LDataSet.DataSet.FieldByName('LegendTopPos').AsInteger;
          Self.FLegendVertMargin	        :=      LDataSet.DataSet.FieldByName('LegendVertMargin').AsInteger;
          Self.FLabelStr		              :=      Trim(LDataSet.DataSet.FieldByName('Label').AsString);
          Self.PopulateValuesFromLabel;
          Result := inherited LoadDataFromDB(APrimaryKeyProperty);
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCChartProperties.SaveDataToDB';
var
 LDatabaseName,
 LWhereClause: string;
 LTableName: string;
 LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    if(Length(APrimaryKeyProperty) < 6) then
     Exit;

    {if not Changed then
    begin
      Result := True;
      Exit;
    end;
    }
    Result := GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      ChartNumber  := StrToInt(APrimaryKeyProperty[5]);
      LTableName := 'yrcChartProperty';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
       Exit;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        Self.PopulateLabelFromValues;
        LDataSet.SetSQL
          (' INSERT INTO yrcChartProperty' +
           ' (Model,StudyAreaName,SubArea,Scenario,ChartID,MarginBottom,MarginLeft,MarginRight,MarginTop,'+
           ' BottomAxisIncrement,BottomAxisMaximum,BottomAxisMinorTickCount,LeftAxisIncrement,LeftAxisMaximum,' +
           ' LeftAxisMinorTickCount,RightAxisIncrement,RightAxisMaximum,RightAxisMinorTickCount,' +
           ' LegendTopPos,LegendVertMargin,Label,BottomAxisMinimum,LeftAxisMinimum,RightAxisMinimum) VALUES ' +
           ' (:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:MarginBottom,:MarginLeft,:MarginRight,:MarginTop,'+
           ' :BottomAxisIncrement,:BottomAxisMaximum,:BottomAxisMinorTickCount,:LeftAxisIncrement,:LeftAxisMaximum,' +
           ' :LeftAxisMinorTickCount,:RightAxisIncrement,:RightAxisMaximum,:RightAxisMinorTickCount,' +
           ' :LegendTopPos,:LegendVertMargin,:Label,:BottomAxisMinimum,:LeftAxisMinimum,:RightAxisMinimum)');

        LDataSet.SetParams([
        'Model','StudyAreaName','SubArea','Scenario','ChartID','MarginBottom','MarginLeft', 'MarginRight','MarginTop',
        'BottomAxisIncrement','BottomAxisMaximum','BottomAxisMinorTickCount','LeftAxisIncrement','LeftAxisMaximum',
        'LeftAxisMinorTickCount','RightAxisIncrement','RightAxisMaximum','RightAxisMinorTickCount',
        'LegendTopPos','LegendVertMargin','Label','BottomAxisMinimum','LeftAxisMinimum','RightAxisMinimum'],
        [APrimaryKeyProperty[1], APrimaryKeyProperty[2], APrimaryKeyProperty[3], APrimaryKeyProperty[4],
         IntToStr(ChartNumber),IntToStr(MarginBottom), IntToStr(MarginLeft), IntToStr(MarginRight),IntToStr(MarginTop),
         FloatToStr(BottomAxisIncrement),FloatToStr(BottomAxisMaximum),IntToStr(BottomAxisMinorTickCount),
         FloatToStr(LeftAxisIncrement), FloatToStr(LeftAxisMaximum), IntToStr(LeftAxisMinorTickCount),
         FloatToStr(RightAxisIncrement),FloatToStr(RightAxisMaximum), IntToStr(RightAxisMinorTickCount),
         IntToStr(LegendTopPos), IntToStr(LegendVertMargin),FLabelStr, FloatToStr(BottomAxisMinimum),
         FloatToStr(LeftAxisMinimum),FloatToStr(RightAxisMinimum)]);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
      Result := Result and inherited SaveDataToDB(APrimaryKeyProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetBottomAxisIncrement: double;
const OPNAME = 'TYRCChartProperties.GetBottomAxisIncrement';
begin
  Result := -1;
  try
    Result := FBottomAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetBottomAxisMaximum: double;
const OPNAME = 'TYRCChartProperties.GetBottomAxisMaximum';
begin
  Result := -1;
  try
    Result := FBottomAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetBottomAxisMinorTickCount: integer;
const OPNAME = 'TYRCChartProperties.GetBottomAxisMinorTickCount';
begin
  Result := -1;
  try
    Result := FBottomAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetRightAxisMaximum: double;
const OPNAME = 'TYRCChartProperties.GetRightAxisMaximum';
begin
  Result := -1;
  try
    Result := FRightAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TYRCChartProperties.GetLabelStr: string;
const OPNAME = 'TYRCChartProperties.GetLabelStr';
begin
  Result := '';
  try
    Result := FLabelStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TYRCChartProperties.GetLeftAxisIncrement: double;
const OPNAME = 'TYRCChartProperties.GetLeftAxisIncrement';
begin
  Result := -1;
  try
    Result := FLeftAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetLeftAxisMaximum: double;
const OPNAME = 'TYRCChartProperties.GetLeftAxisMaximum';
begin
  Result := -1;
  try
    Result := FLeftAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetLeftAxisMinorTickCount: integer;
const OPNAME = 'TYRCChartProperties.GetLeftAxisMinorTickCount';
begin
  Result := -1;
  try
    Result := FLeftAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetLegendTopPos: integer;
const OPNAME = 'TYRCChartProperties.GetLegendTopPos';
begin
  Result := -1;
  try
    Result := FLegendTopPos;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetLegendVertMargin: integer;
const OPNAME = 'TYRCChartProperties.GetLegendVertMargin';
begin
  Result := -1;
  try
    Result := FLegendVertMargin;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetMarginBottom: integer;
const OPNAME = 'TYRCChartProperties.GetMarginBottom';
begin
  Result := -1;
  try
    Result := FMarginBottom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetMarginLeft: integer;
const OPNAME = 'TYRCChartProperties.GetMarginLeft';
begin
  Result := -1;
  try
    Result := FMarginLeft;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetMarginRight: integer;
const OPNAME = 'TYRCChartProperties.GetMarginRight';
begin
  Result := -1;
  try
    Result := FMarginRight;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetMarginTop: integer;
const OPNAME = 'TYRCChartProperties.GetMarginTop';
begin
  Result := -1;
  try
    Result := FMarginTop;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetRightAxisIncrement: double;
const OPNAME = 'TYRCChartProperties.GetRightAxisIncrement';
begin
  Result := -1;
  try
    Result := FRightAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetRightAxisMinorTickCount: integer;
const OPNAME = 'TYRCChartProperties.GetRightAxisMinorTickCount';
begin
  Result := -1;
  try
    Result := FRightAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//____________________________________________________________________

function TYRCChartProperties.GetChartEditMode: TChartEditMode;
const OPNAME = 'TYRCChartProperties.GetChartEditMode';
begin
  Result := tdmNone;
  try
    Result := FChartEditMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetChartMode: TChartMode;
const OPNAME = 'TYRCChartProperties.GetChartMode';
begin
  Result := cmView;
  try
    Result := FChartMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetHideRawLines: boolean;
const OPNAME = 'TYRCChartProperties.GetHideRawLines';
begin
  Result := False;
  try
    Result := FHideRawLines;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetHideRawpoints: boolean;
const OPNAME = 'TYRCChartProperties.GetHideRawpoints';
begin
  Result := False;
  try
    Result := FHideRawpoints;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetMaxYValue: double;
const OPNAME = 'TYRCChartProperties.GetMaxYValue';
begin
  Result := NullFloat;
  try
    Result := FMaxYValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetShowCursorPosition: boolean;
const OPNAME = 'TYRCChartProperties.GetShowCursorPosition';
begin
  Result := False;
  try
    Result := FShowCursorPosition;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetShowFirmYieldLabels: boolean;
const OPNAME = 'TYRCChartProperties.GetShowFirmYieldLabels';
begin
  Result := False;
  try
    Result := FShowFirmYieldLabels;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetShowTargetDrafts: TShowTargetDrafts;
const OPNAME = 'TYRCChartProperties.GetShowTargetDrafts';
begin
  Result := stdAll;
  try
    Result := FShowTargetDrafts;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetZoomIndex: integer;
const OPNAME = 'TYRCChartProperties.GetZoomIndex';
begin
  Result := 0;
  try
    Result := FZoomIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetZoomValue: double;
const OPNAME = 'TYRCChartProperties.GetZoomValue';
begin
  Result := 100;
  try
    Result := FZoomValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetBottomAxisIncrement(ABottomAxisIncrement: double);
const OPNAME = 'TYRCChartProperties.SetBottomAxisIncrement';
begin
  try
    if(FBottomAxisIncrement <> ABottomAxisIncrement) then
      FBottomAxisIncrement := ABottomAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetBottomAxisMaximum(ABottomAxisMaximum: double);
const OPNAME = 'TYRCChartProperties.SetBottomAxisMaximum';
begin
  try
    if(FBottomAxisMaximum <> ABottomAxisMaximum) then
      FBottomAxisMaximum := ABottomAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetBottomAxisMinorTickCount(ABottomAxisMinorTickCount: integer);
const OPNAME = 'TYRCChartProperties.SetBottomAxisMinorTickCount';
begin
  try
    if(FBottomAxisMinorTickCount <> ABottomAxisMinorTickCount) then
      FBottomAxisMinorTickCount := ABottomAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TYRCChartProperties.SetLabelStr(ALabelStr: string);
const OPNAME = 'TYRCChartProperties.SetLabelStr';
begin
  try
    if(FLabelStr <> ALabelStr) then
      FLabelStr := ALabelStr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TYRCChartProperties.SetLeftAxisIncrement(ALeftAxisIncrement: double);
const OPNAME = 'TYRCChartProperties.SetLeftAxisIncrement';
begin
  try
    if(FLeftAxisIncrement <> ALeftAxisIncrement) then
      FLeftAxisIncrement := ALeftAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetLeftAxisMaximum(ALeftAxisMaximum: double);
const OPNAME = 'TYRCChartProperties.SetLeftAxisMaximum';
begin
  try
    if(FLeftAxisMaximum <> ALeftAxisMaximum) then
      FLeftAxisMaximum := ALeftAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetLeftAxisMinorTickCount(ALeftAxisMinorTickCount: integer);
const OPNAME = 'TYRCChartProperties.SetLeftAxisMinorTickCount';
begin
  try
    if(FLeftAxisMinorTickCount <> ALeftAxisMinorTickCount) then
      FLeftAxisMinorTickCount := ALeftAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetLegendTopPos(ALegendTopPos: integer);
const OPNAME = 'TYRCChartProperties.SetLegendTopPos';
begin
  try
    if(FLegendTopPos <> ALegendTopPos) then
      FLegendTopPos := ALegendTopPos;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetLegendVertMargin(ALegendVertMargin: integer);
const OPNAME = 'TYRCChartProperties.SetLegendVertMargin';
begin
  try
    if(FLegendVertMargin <> ALegendVertMargin) then
      FLegendVertMargin := ALegendVertMargin;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetMarginBottom(AMarginBottom: integer);
const OPNAME = 'TYRCChartProperties.SetMarginBottom';
begin
  try
    if(FMarginBottom <> AMarginBottom) then
      FMarginBottom := AMarginBottom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetMarginLeft(AMarginLeft: integer);
const OPNAME = 'TYRCChartProperties.SetMarginLeft';
begin
  try
    if(FMarginLeft <> AMarginLeft) then
      FMarginLeft := AMarginLeft;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetMarginRight(AMarginRight: integer);
const OPNAME = 'TYRCChartProperties.SetMarginRight';
begin
  try
    if(FMarginRight <> AMarginRight) then
      FMarginRight := AMarginRight;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetMarginTop(AMarginTop: integer);
const OPNAME = 'TYRCChartProperties.SetMarginTop';
begin
  try
    if(FMarginTop <> AMarginTop) then
      FMarginTop := AMarginTop;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetRightAxisIncrement(ARightAxisIncrement: double);
const OPNAME = 'TYRCChartProperties.SetRightAxisIncrement';
begin
  try
    if(FRightAxisIncrement <> ARightAxisIncrement) then
      FRightAxisIncrement := ARightAxisIncrement;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetRightAxisMaximum(ARightAxisMaximum: double);
const OPNAME = 'TYRCChartProperties.SetRightAxisMaximum';
begin
  try
    if(FRightAxisMaximum <> ARightAxisMaximum) then
      FRightAxisMaximum := ARightAxisMaximum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetRightAxisMinorTickCount(ARightAxisMinorTickCount: integer);
const OPNAME = 'TYRCChartProperties.SetRightAxisMinorTickCount';
begin
  try
    if(FRightAxisMinorTickCount <> ARightAxisMinorTickCount) then
      FRightAxisMinorTickCount := ARightAxisMinorTickCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.PopulateLabelFromValues;
const OPNAME = 'TYRCChartProperties.PopulateLabelFromValues';
var
  LValues: TStringList;
begin
  try
    LValues := TStringList.Create;
    try
      LValues.Add('ZoomIndex=' + IntToStr(FZoomIndex));
      LValues.Add('ZoomValue=' + FormatFloat('#0.0#',FZoomValue));

      LValues.Add('ShowTargetDrafts=' + IntToStr(Ord(FShowTargetDrafts)));
      LValues.Add('ChartMode=' + IntToStr(Ord(FChartMode)));
      LValues.Add('ChartEditMode=' + IntToStr(Ord(FChartEditMode)));

      LValues.Add('HideRawPoints=' + IntToStr(Ord(FHideRawPoints)));
      LValues.Add('HideRawLines=' + IntToStr(Ord(FHideRawLines)));
      LValues.Add('ShowFirmYieldLabels=' + IntToStr(Ord(FShowFirmYieldLabels)));
      LValues.Add('ShowCursorPosition=' + IntToStr(Ord(FShowCursorPosition)));
      LValues.Add('MaxYValue=' + FormatFloat('##0.0#####',FMaxYValue));
      LValues.Add('HideFittedPoints=' + IntToStr(Ord(FHideFittedPoints)));
      FLabelStr := LValues.CommaText;
    finally
      LValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.PopulateValuesFromLabel;
const OPNAME = 'TYRCChartProperties.PopulateValuesFromLabel';
var
  LValues: TStringList;
  LValue: string;
begin
  try
    LValues := TStringList.Create;
    try
      LValues.CommaText    := FLabelStr ;

      LValue               := LValues.Values['ZoomIndex'];
      FZoomIndex           := StrToIntDef(LValue,NullInteger);
      LValue               := LValues.Values['ZoomValue'];
      FZoomValue           := StrToFloatDef(LValue,NullFloat);

      LValue               := LValues.Values['ShowTargetDrafts'];
      FShowTargetDrafts    := TShowTargetDrafts(StrToIntDef(LValue,0));
      LValue               := LValues.Values['ChartMode'];
      FChartMode           := TChartMode(StrToIntDef(LValue,1));
      LValue               := LValues.Values['ChartEditMode'];
      FChartEditMode       := TChartEditMode(StrToIntDef(LValue,0));

      LValue               := LValues.Values['HideRawPoints'];
      FHideRawPoints       := (StrToIntDef(LValue,1) = 1);
      LValue               := LValues.Values['HideRawLines'];
      FHideRawLines        := (StrToIntDef(LValue,1) = 1);
      LValue               := LValues.Values['ShowFirmYieldLabels'];
      FShowFirmYieldLabels := (StrToIntDef(LValue,0) = 1);
      LValue               := LValues.Values['ShowCursorPosition'];
      FShowCursorPosition  := (StrToIntDef(LValue,0) = 1);
      LValue               := LValues.Values['MaxYValue'];
      FMaxYValue           := StrToFloatDef(LValue,NullFloat);
      LValue               := LValues.Values['HideFittedPoints'];
      FHideFittedPoints    := (StrToIntDef(LValue,0) = 1);
    finally
      LValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetChartEditMode(AValue: TChartEditMode);
const OPNAME = 'TYRCChartProperties.SetChartEditMode';
begin
  try
    FChartEditMode := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetChartMode(AValue: TChartMode);
const OPNAME = 'TYRCChartProperties.SetChartMode';
begin
  try
    FChartMode := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetHideRawLines(AValue: boolean);
const OPNAME = 'TYRCChartProperties.SetHideRawLines';
begin
  try
    FHideRawLines := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetHideRawpoints(AValue: boolean);
const OPNAME = 'TYRCChartProperties.SetHideRawpoints';
begin
  try
    FHideRawpoints := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetMaxYValue(AValue: double);
const OPNAME = 'TYRCChartProperties.SetMaxYValue';
begin
  try
    FMaxYValue := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetShowCursorPosition(AValue: boolean);
const OPNAME = 'TYRCChartProperties.SetShowCursorPosition';
begin
  try
    FShowCursorPosition := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetShowFirmYieldLabels(AValue: boolean);
const OPNAME = 'TYRCChartProperties.SetShowFirmYieldLabels';
begin
  try
    FShowFirmYieldLabels := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetShowTargetDrafts(AValue: TShowTargetDrafts);
const OPNAME = 'TYRCChartProperties.SetShowTargetDrafts';
begin
  try
    FShowTargetDrafts := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetZoomIndex(AValue: integer);
const OPNAME = 'TYRCChartProperties.SetZoomIndex';
begin
  try
    if(AValue <> FZoomIndex)  and (AValue in [0,1,2,3]) then
    begin
      FZoomIndex := AValue;
      if(FZoomIndex = 2) then
      begin
        FZoomValue := StrToFloatDef(InputBox('Zoom Selection','Enter start zoom (0..99)','75.00'),100.00);
        if(FZoomValue < 0.0) or (FZoomValue > 100.0) then
          FZoomValue := 0.0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetZoomValue(AValue: double);
const OPNAME = 'TYRCChartProperties.SetZoomValue';
begin
  try
    FZoomValue := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetBottomAxisMinimum: double;
const OPNAME = 'TYRCChartProperties.GetBottomAxisMinimum';
begin
  Result := NullInteger;
  try
    Result := FBottomAxisMinimum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetRightAxisMinimum: double;
const OPNAME = 'TYRCChartProperties.GetRightAxisMinimum';
begin
  Result := NullInteger;
  try
    Result := FRightAxisMinimum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetLeftAxisMinimum: double;
const OPNAME = 'TYRCChartProperties.GetLeftAxisMinimum';
begin
  Result := NullInteger;
  try
    Result := FLeftAxisMinimum;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetBottomAxisMinimum(AValue: double);
const OPNAME = 'TYRCChartProperties.SetBottomAxisMinimum';
begin
  try
    FBottomAxisMinimum := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetRightAxisMinimum(AValue: double);
const OPNAME = 'TYRCChartProperties.SetRightAxisMinimum';
begin
  try
    FRightAxisMinimum := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetLeftAxisMinimum(AValue: double);
const OPNAME = 'TYRCChartProperties.SetLeftAxisMinimum';
begin
  try
    FLeftAxisMinimum := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCChartProperties.GetHideFittedPoints: boolean;
const OPNAME = 'TYRCChartProperties.GetHideFittedPoints';
begin
  Result := False;
  try
    Result := FHideFittedPoints;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCChartProperties.SetHideFittedPoints(AValue: boolean);
const OPNAME = 'TYRCChartProperties.SetHideFittedPoints';
begin
  try
    FHideFittedPoints := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCFunctionConstants }

procedure TYRCFunctionConstants.Reset;
const OPNAME = 'TYRCFunctionConstants.Reset';
var
  LCount: integer;
begin
  inherited Reset;
  try
    for LCount := 0 to 3 do
      FConstantsArray[LCount] := 0.0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFunctionConstants.Valid: boolean;
const OPNAME = 'TYRCFunctionConstants.Valid';
var
  LCount: integer;
begin
  Result := False;
  try
    for LCount := 0 to 3 do
      Result := Result or (FConstantsArray[LCount]  <> 0.0);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFunctionConstants.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCFunctionConstants.CopyValuesFrom';
var
  LCount: integer;
  LYRCFunctionConstants: TYRCFunctionConstants;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCFunctionConstants) then
    begin
      Result := False;
      LYRCFunctionConstants := TYRCFunctionConstants(ASource);
      for LCount := 0 to 3 do
        FConstantsArray[LCount] := LYRCFunctionConstants.ConstantsArray[LCount];
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFunctionConstants.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCFunctionConstants.LoadDataFromDB';
var
 LDatabaseName,
 LWhereClause: string;
 LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    Self.Reset;
    Result := (Length(APrimaryKeyProperty) >= 9);
    Result := Result and GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' SELECT Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,' +
           ' CurveType,AConstValue,BConstValue,CConstValue,DConstValue' +
           ' FROM yrcTargetDraftConst WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;

        if(LDataSet.DataSet.RecordCount <= 0) then
          Result := True
        else
        begin
          Self.FConstantsArray[0]    := LDataSet.DataSet.FieldByName('AConstValue').AsFloat;
          Self.FConstantsArray[1]    := LDataSet.DataSet.FieldByName('BConstValue').AsFloat;
          Self.FConstantsArray[2]    := LDataSet.DataSet.FieldByName('CConstValue').AsFloat;
          Self.FConstantsArray[3]    := LDataSet.DataSet.FieldByName('DConstValue').AsFloat;
          Result := inherited LoadDataFromDB(APrimaryKeyProperty);
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFunctionConstants.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCFunctionConstants.SaveDataToDB';
var
  LDatabaseName,
  LWhereClause: string;
  LTableName: string;
  LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    if(Length(APrimaryKeyProperty) < 9) then
     Exit;

    {if not Changed then
    begin
      Result := True;
      Exit;
    end;
    }
    Result := GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      LTableName := 'yrcTargetDraftConst';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
       Exit;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' INSERT INTO yrcTargetDraftConst' +
           ' (Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,' +
           ' CurveType,AConstValue,BConstValue,CConstValue,DConstValue)' +
           ' VALUES (:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:PlaneNumber,' +
           ':TargetDraftID,:CurveType,:AConstValue,:BConstValue,:CConstValue,:DConstValue)');

        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'ChartID','PlaneNumber','TargetDraftID',
                            'CurveType','AConstValue','BConstValue','CConstValue',
                            'DConstValue'],
                           [APrimaryKeyProperty[1], APrimaryKeyProperty[2],
                            APrimaryKeyProperty[3], APrimaryKeyProperty[4],
                            APrimaryKeyProperty[5],APrimaryKeyProperty[6],
                            APrimaryKeyProperty[7],APrimaryKeyProperty[8],
                            FloatToStr(Self.FConstantsArray[0]),
                            FloatToStr(Self.FConstantsArray[1]), FloatToStr(Self.FConstantsArray[2]),
                            FloatToStr(Self.FConstantsArray[3])]);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
      Result := Result and inherited SaveDataToDB(APrimaryKeyProperty);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFunctionConstants.GetConstantsArray: TYRCConstantsArray;
const OPNAME = 'TYRCFunctionConstants.GetConstantsArray';
begin
  Result := FConstantsArray;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCTargetDraft }

procedure TYRCTargetDraft.CreateMemberObjects;
const OPNAME = 'TYRCTargetDraft.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FIndexOf100Value                := -1;
    FDeterministicPointsState       := 0;
    FOriginalPointsArray            := TYRCRecordPointArrayObject.Create(FAppModules);
    FPureRegressionPointsArray      := TYRCRecordPointArrayObject.Create(FAppModules);
    FRegressionPointsArray          := TYRCRecordPointArrayObject.Create(FAppModules);
    FDeterministicPointsArray       := TYRCRecordPointArrayObject.Create(FAppModules);

    FPureRegressionConstants        := TYRCFunctionConstants.Create(FAppModules);
    FRegressionConstants            := TYRCFunctionConstants.Create(FAppModules);
    FDeterministicConstants         := TYRCFunctionConstants.Create(FAppModules);
    FSavedOriginalConstants         := TYRCFunctionConstants.Create(FAppModules);

    FPointsArray                    := nil;
    FForceCurveThrough100           := False;
    FLabelProperties                := TLabelProperties.Create;
    FSaved                          := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.DestroyMemberObjects;
const OPNAME = 'TYRCTargetDraft.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FOriginalPointsArray.Free;
    FPureRegressionPointsArray.Free;
    FRegressionPointsArray.Free;
    FDeterministicPointsArray.Free;

    FPureRegressionConstants.Free;
    FRegressionConstants.Free;
    FDeterministicConstants.Free;
    FSavedOriginalConstants.Free;
    FLabelProperties.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.Reset;
const OPNAME = 'TYRCTargetDraft.Reset';
begin
  inherited Reset;
  try
    FTargetDraftSavedMode        := tdmNone;
    FPlaneID                     := 0;
    FTargetDraftID               := 0;
    FTargetDraftXValue           := 0.0;
    FTargetDraftXTValue          := 0.0;
    FTargetDraftYValue           := 0.0;
    FTargetDraftRecurance        := 0.0;
    FTargetDraftYears            := 0;
    FDeterministicPoint1XValue   := 0.0;
    FDeterministicPoint1XTValue  := 0.0;
    FDeterministicPoint2XValue   := 0.0;
    FDeterministicPoint2XTValue  := 0.0;

    FOriginalPointsArray.Reset;
    FPureRegressionPointsArray.Reset;
    FRegressionPointsArray.Reset;
    FDeterministicPointsArray.Reset;
    SetLength(FDeterministicPointsArray.FYRCRecordPointArray,4);

    FPureRegressionConstants.Reset;
    FRegressionConstants.Reset;
    FDeterministicConstants.Reset;
    FLabelProperties.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.DoCurveFitting (ACurveType            : TCurveType;
                                         ACurveConstantsObject : TYRCFunctionConstants;
                                         AYXPairsArrayObject   : TYRCRecordPointArrayObject;
                                         AForce                : Boolean): boolean;
const OPNAME = 'TYRCTargetDraft.DoCurveFitting';
var
  LIndex,
  LCount                : integer;
  LConstantsArray       : TYRCConstantsArray;
  LPointsArray          : TYRCFlatPointArray;
  LFlatPointArrayLength : integer;
begin
  Result := False;
  Finalize(FPointsArray);
  try
    if Assigned(ACurveConstantsObject) and Assigned(AYXPairsArrayObject) then
    begin
      if(Length(AYXPairsArrayObject.FYRCRecordPointArray) = 0) then
         raise Exception.Create('Curve fitting cannot be done on an empty list of points.');

      ACurveConstantsObject.Reset;
      for LCount := 0 to 3 do
          LConstantsArray[LCount] := 0.0;
      LIndex := 0;

      LFlatPointArrayLength := Length(AYXPairsArrayObject.FYRCRecordPointArray) * 2;
      SetLength(LPointsArray, LFlatPointArrayLength);
      //SetLength(LPointsArray, LFlatPointArrayLength);
      try
        for LCount := Low(AYXPairsArrayObject.FYRCRecordPointArray) to
          High(AYXPairsArrayObject.FYRCRecordPointArray) do
        begin
          LPointsArray[LIndex]     := AYXPairsArrayObject.FYRCRecordPointArray[LCount].YValue;
          LPointsArray[LIndex + 1] := AYXPairsArrayObject.FYRCRecordPointArray[LCount].XTValue;
          LIndex := LIndex + 2;
        end;

        Result := CalculateCurveCoefficientsDelphi(ACurveType,LConstantsArray,LPointsArray,AForce);
        if Result then
        begin
          for LCount := 0 to 3 do
            ACurveConstantsObject.FConstantsArray[LCount] := LConstantsArray[LCount];
          ACurveConstantsObject.Changed := True;
          ACurveConstantsObject.Loaded  := True;
        end;
      finally
        Finalize(LPointsArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCTargetDraft.CopyValuesFrom';
var
  LYRCTargetDraft: TYRCTargetDraft;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCTargetDraft) then
    begin
      Result := False;
      LYRCTargetDraft := TYRCTargetDraft(ASource);
      FTargetDraftSavedMode        := LYRCTargetDraft.FTargetDraftSavedMode;
      FPlaneID                     := LYRCTargetDraft.FPlaneID;
      FTargetDraftID               := LYRCTargetDraft.FTargetDraftID;
      FTargetDraftXValue           := LYRCTargetDraft.FTargetDraftXValue;
      FTargetDraftXTValue          := LYRCTargetDraft.FTargetDraftXTValue;
      FTargetDraftYValue           := LYRCTargetDraft.FTargetDraftYValue;
      FTargetDraftRecurance        := LYRCTargetDraft.FTargetDraftRecurance;
      FTargetDraftYears            := LYRCTargetDraft.FTargetDraftYears;

      Result :=
        FOriginalPointsArray.CopyValuesFrom(LYRCTargetDraft.FOriginalPointsArray) and
        FPureRegressionPointsArray.CopyValuesFrom(LYRCTargetDraft.FPureRegressionPointsArray) and
        FRegressionPointsArray.CopyValuesFrom(LYRCTargetDraft.FRegressionPointsArray) and
        FDeterministicPointsArray.CopyValuesFrom(LYRCTargetDraft.FDeterministicPointsArray);

      Result := Result and
        FPureRegressionConstants.CopyValuesFrom(LYRCTargetDraft.FPureRegressionConstants) and
        FRegressionConstants.CopyValuesFrom(LYRCTargetDraft.FRegressionConstants) and
        FDeterministicConstants.CopyValuesFrom(LYRCTargetDraft.FDeterministicConstants);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCTargetDraft.LoadDataFromDB';
var
  LDatabaseName,
  LWhereClause: string;
  LDataSet: TAbstractModelDataSet;
  LCount: integer;
  LNewKeyPropertyCurveType: array[0..8] of string;
begin
  Result := False;
  try
    Self.Reset;
    Result := (Length(APrimaryKeyProperty) >= 8);
    Result := Result and GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause);
    if Result then
    begin
      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' SELECT Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID,' +
           ' SavedMode,TargetDraftXValue,TargetDraftYValue,TargetDraftYears,TargetDraftRecurance,IndexOf100,'+
           ' DetPointsChanged, ForceCurveThrough100,Point1X,Point1XT,Point2X,Point2XT,'+
           ' LabelText,LabelLeftTopX,LabelLeftTopY,LabelArrowToX,LabelArrowToY,LabelCustom' +
           ' FROM yrcTargetDraft WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;

        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          Self.ChartNumber                 := LDataSet.DataSet.FieldByName('ChartID').AsInteger;
          Self.FPlaneID                    := LDataSet.DataSet.FieldByName('PlaneNumber').AsInteger;
          Self.FTargetDraftSavedMode       := GetTargetDraftSavedModeFromInt(LDataSet.DataSet.FieldByName('SavedMode').AsInteger);
          Self.FTargetDraftID              := LDataSet.DataSet.FieldByName('TargetDraftID').AsInteger;
          Self.FTargetDraftXValue          := LDataSet.DataSet.FieldByName('TargetDraftXValue').AsFloat;
          Self.FTargetDraftXTValue         := LDataSet.DataSet.FieldByName('TargetDraftXValue').AsFloat;
          Self.FTargetDraftYValue          := LDataSet.DataSet.FieldByName('TargetDraftYValue').AsFloat;
          Self.FTargetDraftYears           := LDataSet.DataSet.FieldByName('TargetDraftYears').AsInteger;
          Self.FTargetDraftRecurance       := LDataSet.DataSet.FieldByName('TargetDraftRecurance').AsFloat;
          Self.FIndexOf100Value            := LDataSet.DataSet.FieldByName('IndexOf100').AsInteger;
          Self.FDeterministicPointsState   := LDataSet.DataSet.FieldByName('DetPointsChanged').AsInteger;
          Self.FForceCurveThrough100       := (LDataSet.DataSet.FieldByName('ForceCurveThrough100').AsInteger = 1);
          Self.FDeterministicPoint1XValue  := LDataSet.DataSet.FieldByName('Point1X').AsFloat;
          Self.FDeterministicPoint1XTValue := LDataSet.DataSet.FieldByName('Point1XT').AsFloat;
          Self.FDeterministicPoint2XValue  := LDataSet.DataSet.FieldByName('Point2X').AsFloat;
          Self.FDeterministicPoint2XTValue := LDataSet.DataSet.FieldByName('Point2XT').AsFloat;
          FLabelProperties.Text            := Trim(LDataSet.DataSet.FieldByName('LabelText').AsString);
          FLabelProperties.LeftTopX        := LDataSet.DataSet.FieldByName('LabelLeftTopX').AsInteger;
          FLabelProperties.LeftTopY        := LDataSet.DataSet.FieldByName('LabelLeftTopY').AsInteger;
          FLabelProperties.ArrowToX        := LDataSet.DataSet.FieldByName('LabelArrowToX').AsInteger;
          FLabelProperties.ArrowToY        := LDataSet.DataSet.FieldByName('LabelArrowToY').AsInteger;
          if(LDataSet.DataSet.FieldByName('LabelCustom').AsInteger = 0) then
            FLabelProperties.Custom          := False
          else
            FLabelProperties.Custom          := True;

          Result := inherited LoadDataFromDB(APrimaryKeyProperty);
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;

      for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
        LNewKeyPropertyCurveType[LCount] := APrimaryKeyProperty[LCount];

      LNewKeyPropertyCurveType[8] := 'P';
      Result := Result and FPureRegressionConstants.LoadDataFromDB(LNewKeyPropertyCurveType);
      Result := Result and FPureRegressionPointsArray.LoadDataFromDB(LNewKeyPropertyCurveType);

      LNewKeyPropertyCurveType[8] := 'R';
      Result := Result and FRegressionConstants.LoadDataFromDB(LNewKeyPropertyCurveType);
      Result := Result and FRegressionPointsArray.LoadDataFromDB(LNewKeyPropertyCurveType);

      LNewKeyPropertyCurveType[8] := 'D';
      Result := Result and FDeterministicConstants.LoadDataFromDB(LNewKeyPropertyCurveType);
      Result := Result and FDeterministicPointsArray.LoadDataFromDB(LNewKeyPropertyCurveType);

      LNewKeyPropertyCurveType[8] := 'O';
      Result := Result and FOriginalPointsArray.LoadDataFromDB(LNewKeyPropertyCurveType);
    end;
    FSaved := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.LoadDataFromFile(AFileBlocks: TObjectList; const AIndexProperty: array of integer): boolean;
const OPNAME = 'TYRCTargetDraft.LoadDataFromFile';
var
  LCount,
  LIndex,
  LIndex2: integer;
  LBlockValues: TSumOutAnualSummaryBlockValues;
begin
  Result := inherited LoadDataFromFile(AFileBlocks,AIndexProperty);
  try
    LIndex  := AIndexProperty[0];
    LIndex2 := AIndexProperty[1];
    if Result and (LIndex >= 0) and (LIndex <= AFileBlocks.Count) then
    begin
      if (AFileBlocks[LIndex] is TSumOutAnualSummaryBlockValues) then
      begin
        LBlockValues := TSumOutAnualSummaryBlockValues(AFileBlocks[LIndex]);
        if(LIndex2 >=  Low(LBlockValues.FValues)) and
          (LIndex2 <= High(LBlockValues.FValues)) then
        begin
          FPlaneID            := LIndex;
          FTargetDraftID      := LIndex2;
          FTargetDraftXValue  := 0.0;
          FTargetDraftXTValue := 0.0;
          FTargetDraftYValue  := LBlockValues.FValues[LIndex2].FData;
          FTargetDraftYears   := LBlockValues.FYearPeriod.FData;
          FDeterministicPoint1XValue   := 0.0;
          FDeterministicPoint1XTValue  := 0.0;
          FDeterministicPoint2XValue   := 0.0;
          FDeterministicPoint2XTValue  := 0.0;

          SetLength(FOriginalPointsArray.FYRCRecordPointArray,LBlockValues.FAnualValues.Count);
          FOriginalPointsArray.LoadDataFromFile(AFileBlocks,AIndexProperty);
          for LCount := 0 to LBlockValues.FAnualValues.Count -1 do
          begin
            FOriginalPointsArray.FYRCRecordPointArray[LCount].XValue :=
              LBlockValues.FAnualValues.AnualSummaryValuesLine[LCount].FFirstInteger.FData;
            FOriginalPointsArray.FYRCRecordPointArray[LCount].XTValue :=
              LBlockValues.FAnualValues.AnualSummaryValuesLine[LCount].FFirstInteger.FData;
            FOriginalPointsArray.FYRCRecordPointArray[LCount].YValue :=
              LBlockValues.FAnualValues.AnualSummaryValuesLine[LCount].AnualSummaryValuesLine[LIndex2].FData;
          end;

          FPureRegressionPointsArray.LoadDataFromFile(AFileBlocks,AIndexProperty);
          FRegressionPointsArray.LoadDataFromFile(AFileBlocks,AIndexProperty);
          FDeterministicPointsArray.LoadDataFromFile(AFileBlocks,AIndexProperty);

          FPureRegressionConstants.LoadDataFromFile(AFileBlocks,AIndexProperty);
          FRegressionConstants.LoadDataFromFile(AFileBlocks,AIndexProperty);
          FDeterministicConstants.LoadDataFromFile(AFileBlocks,AIndexProperty);
          FSavedOriginalConstants.LoadDataFromFile(AFileBlocks,AIndexProperty);

          CalculateTargetDraftProperties;
          FSaved := True;
          ResetPureRegressionPoints;
          ResetRegressionPoints;
          ResetDeterministicPoints;
        end;
      end;
    end
    else
      Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.CalculateTargetDraftProperties;
const OPNAME = 'TYRCTargetDraft.CalculateTargetDraftProperties';
var
  LPointCount,
  LZeroCount,
  LNonZeroCount: integer;
  LPointsArrayObject : TYRCRecordPointArrayObject;
begin
  try
    LPointsArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    try
      LPointsArrayObject.CopyValuesFrom(FOriginalPointsArray);
      LPointsArrayObject.Sort(soAscending);
      if LPointsArrayObject.Sorted then
      begin
        LZeroCount    := LPointsArrayObject.ZeroYCount - 1;
        if (LZeroCount < 0) then
          LZeroCount := 0;
        LPointCount   := Length(LPointsArrayObject.FYRCRecordPointArray);
        LNonZeroCount := LPointCount - LZeroCount + 2;
        if(LZeroCount = 0) then
        begin
         FTargetDraftRecurance := 0.0;
         FTargetDraftXValue    := 0.0;
         FTargetDraftXTValue   := 0.0;
        end
        else
        begin
         FTargetDraftXValue    := (LZeroCount*100)/ LPointCount;
         FTargetDraftXTValue   := (LZeroCount*100)/ LPointCount;
         FTargetDraftRecurance := 1/( 1 - Power( Abs(1 - LNonZeroCount/LPointCount), (1 / FTargetDraftYears) ) );
         FTargetDraftRecurance := Round(FTargetDraftRecurance);
        end;
      end;
    finally
      LPointsArrayObject.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetRegressionFourPoints(var AYRCFourPointsArray: TYRCFourPointsArray): boolean;
const OPNAME = 'TYRCTargetDraft.GetRegressionFourPoints';
var
  LIndex,
  LCount: integer;
  LWidth,
  LYValue: double;
  LXValue: double;
begin
  Result := False;
  try
    if FRegressionConstants.Valid then
    begin
      for LCount := 0 to 3 do
      begin
        AYRCFourPointsArray[LCount].YValue  := 0.0;
        AYRCFourPointsArray[LCount].XValue  := 0.0;
        AYRCFourPointsArray[LCount].XTValue := 0.0;
      end;
      LWidth := Abs((100.0 -  FTargetDraftXTValue)/3.0);

      AYRCFourPointsArray[0].XValue  := FTargetDraftXValue;
      AYRCFourPointsArray[0].XTValue := FTargetDraftXTValue;
      AYRCFourPointsArray[0].YValue  := FTargetDraftYValue;
      AYRCFourPointsArray[1].XValue  := FTargetDraftXValue  + LWidth;
      AYRCFourPointsArray[1].XTValue := FTargetDraftXTValue + LWidth;
      AYRCFourPointsArray[2].XValue  := FTargetDraftXValue  + LWidth + LWidth;
      AYRCFourPointsArray[2].XTValue := FTargetDraftXTValue + LWidth + LWidth;
      AYRCFourPointsArray[3].XValue  := 100.00;
      AYRCFourPointsArray[3].XTValue := 100.00;

      if (FDeterministicPoint1XValue   = 0.0) and (FDeterministicPoint1XTValue  = 0.0) and
         (FDeterministicPoint2XValue   = 0.0) and (FDeterministicPoint2XTValue  = 0.0) then
      begin
        FDeterministicPoint1XValue    := AYRCFourPointsArray[1].XValue;
        FDeterministicPoint1XTValue  := AYRCFourPointsArray[1].XTValue;
        FDeterministicPoint2XValue   := AYRCFourPointsArray[2].XValue;
        FDeterministicPoint2XTValue  := AYRCFourPointsArray[2].XTValue;
      end;

      for LCount := 0 to 3 do
      begin
        LXValue := AYRCFourPointsArray[LCount].XTValue;
        LYValue := 0.0;
        for LIndex := 0 to 3 do
         LYValue := LYValue + (FRegressionConstants.ConstantsArray[LIndex] * Power(LXValue,LIndex));
        AYRCFourPointsArray[LCount].YValue := LYValue;
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.ResetPureRegressionPoints;
const OPNAME = 'TYRCTargetDraft.ResetPureRegressionPoints';
var
  LIndex,
  LCount: integer;
  LPointCount,
  LZeroCount: integer;
  LXValue,
  LYValue: double;
  LPointsArrayObject : TYRCRecordPointArrayObject;
begin
 try
    FPureRegressionConstants.Reset;
    FPureRegressionPointsArray.Reset;

    LPointsArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    try
      LPointsArrayObject.CopyValuesFrom(FOriginalPointsArray);
      LPointsArrayObject.Sort(soAscending);
      if LPointsArrayObject.Sorted then
      begin
        LZeroCount    := LPointsArrayObject.ZeroYCount;
        LPointCount   := Length(LPointsArrayObject.FYRCRecordPointArray);
        LIndex := 0;
        if (LZeroCount = 0) then
          SetLength(FPureRegressionPointsArray.FYRCRecordPointArray, LPointCount)
        else
        if (LZeroCount >= 1) then
        begin
          SetLength(FPureRegressionPointsArray.FYRCRecordPointArray, ((LPointCount - LZeroCount) + 1));
          FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XValue  := FTargetDraftXValue;
          FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XTValue := FTargetDraftXValue;
          FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].YValue  := FTargetDraftYValue;
          LIndex := LIndex + 1;
        end;

        if (LZeroCount = 0) then
        begin
          for LCount := (Low(LPointsArrayObject.FYRCRecordPointArray)) to High(LPointsArrayObject.FYRCRecordPointArray) do
          begin
            LXValue := ((LCount / (Length(LPointsArrayObject.FYRCRecordPointArray))) * 100);
            LYValue := (1 - LPointsArrayObject.FYRCRecordPointArray[LCount].YValue) * FTargetDraftYValue;
            FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XValue  := LXValue;
            FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XTValue := LXValue;
            FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].YValue  := LYValue;
            LIndex := LIndex + 1;
          end;
        end
        else
        if (LZeroCount >= 1) then
        begin
          for LCount := (Low(LPointsArrayObject.FYRCRecordPointArray)) to High(LPointsArrayObject.FYRCRecordPointArray) do
          begin
            if (LPointsArrayObject.FYRCRecordPointArray[LCount].YValue > 0.0000) then
            begin
              LXValue := (((LCount) / (Length(LPointsArrayObject.FYRCRecordPointArray))) * 100);
              LYValue := (1 - LPointsArrayObject.FYRCRecordPointArray[LCount].YValue)  * FTargetDraftYValue;
              FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XValue  := LXValue;
              FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].XTValue := LXValue;
              FPureRegressionPointsArray.FYRCRecordPointArray[LIndex].YValue  := LYValue;
              LIndex := LIndex + 1;
            end;
          end;
        end;
        if DoCurveFitting(ctTrinomialRegression,FPureRegressionConstants,FPureRegressionPointsArray,FForceCurveThrough100) then
        begin
          FPureRegressionPointsArray.FSorted  := True;
          FPureRegressionPointsArray.Changed := True;
          FPureRegressionPointsArray.Loaded  := True;
        end;
      end;
    finally
      LPointsArrayObject.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.ResetRegressionPoints;
const OPNAME = 'TYRCTargetDraft.ResetRegressionPoints';
begin
  try
    FRegressionPointsArray.Reset;
    FRegressionConstants.CopyValuesFrom(FPureRegressionConstants);
    FTargetDraftSavedMode        := tdmRegression;
    FIndexOf100Value             := -1;
    FForceCurveThrough100        := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.ValidFourPoints(AFourPoints: TYRCFourPointsArray): boolean;
const OPNAME = 'TYRCTargetDraft.ValidFourPoints';
begin
  Result := False;
  try
    Result := (IsPointsEqual(AFourPoints[0], AFourPoints[1])) and
              (IsPointsEqual(AFourPoints[0], AFourPoints[2])) and
              (IsPointsEqual(AFourPoints[0], AFourPoints[3]));
    Result := not Result;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCTargetDraft.ResetDeterministicPoints;
const OPNAME = 'TYRCTargetDraft.ResetDeterministicPoints';
var
  LFourPoints: TYRCFourPointsArray;
  LCount: integer;
  LResult: boolean;
begin
  try
    FDeterministicConstants.Reset;
    FDeterministicPointsArray.Reset;
    FDeterministicPoint1XValue   := 0.0;
    FDeterministicPoint1XTValue  := 0.0;
    FDeterministicPoint2XValue   := 0.0;
    FDeterministicPoint2XTValue  := 0.0;

    SetLength(FDeterministicPointsArray.FYRCRecordPointArray,4);
    if FSaved then
    begin
      if GetRegressionFourPoints(LFourPoints) then
      begin
        if ValidFourPoints(LFourPoints) then
        begin
          for LCount := 0 to 3 do
          begin
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].YValue  := LFourPoints[LCount].YValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XValue  := LFourPoints[LCount].XValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XTValue := LFourPoints[LCount].XTValue;
          end;
          LResult := DoCurveFitting(ctTrinomialDeterministic,FDeterministicConstants,FDeterministicPointsArray,FForceCurveThrough100);
          FSavedOriginalConstants.CopyValuesFrom(FDeterministicConstants);
          RefreshDeterministicMiddleTwoPoints;
          FSaved := False;
          Changed := Changed or LResult;
          FDeterministicPointsState := 0;
        end;
      end;
    end
    else
    begin
      if GetRegressionFourPoints(LFourPoints) then
      begin
        if ValidFourPoints(LFourPoints) then
        begin
          for LCount := 0 to 3 do
          begin
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].YValue  := LFourPoints[LCount].YValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XValue  := LFourPoints[LCount].XValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XTValue := LFourPoints[LCount].XTValue;
          end;
          FDeterministicConstants.CopyValuesFrom(FSavedOriginalConstants);
          RefreshDeterministicMiddleTwoPoints;
          FSaved := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCTargetDraft.SaveDataToDB';
var
  LDatabaseName,
  LWhereClause: string;
  LTableName: string;
  LDataSet: TAbstractModelDataSet;
  LCount :integer;
  LPrimaryKeyTargetDraft: array [0..7] of string;
  LPrimaryKeyCurvePoint: array [0..8] of string;
  LCustom : integer;
begin
  Result := False;
  try
    if(Length(APrimaryKeyProperty) < 7) then
     Exit;

    for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
    begin
      LPrimaryKeyTargetDraft[LCount] := APrimaryKeyProperty[LCount];
      LPrimaryKeyCurvePoint[LCount]  := APrimaryKeyProperty[LCount];
    end;
    LPrimaryKeyTargetDraft[7] := IntToStr(Self.TargetDraftID);
    LPrimaryKeyCurvePoint[7]  := IntToStr(Self.TargetDraftID);

    Result := GetWhereClause(LPrimaryKeyTargetDraft,'',LDatabaseName,LWhereClause);
    //if Result and Changed then
    if Result then
    begin
      //Target Drafts
      LTableName := 'yrcTargetDraft';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
        Exit;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' INSERT INTO yrcTargetDraft' +
           ' (Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,TargetDraftID' +
           ' ,SavedMode,TargetDraftXValue,TargetDraftYValue,TargetDraftYears,TargetDraftRecurance,IndexOf100'+
           ' ,DetPointsChanged,ForceCurveThrough100,Point1X,Point1XT,Point2X,Point2XT' +
           ' ,LabelText,LabelLeftTopX,LabelLeftTopY,LabelArrowToX,LabelArrowToY,LabelCustom)' +
           ' VALUES ' +
           ' (:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:PlaneNumber,:TargetDraftID' +
           ' ,:SavedMode,:TargetDraftXValue,:TargetDraftYValue,:TargetDraftYears,:TargetDraftRecurance,:IndexOf100'+
           ' ,:DetPointsChanged,:ForceCurveThrough100,:Point1X,:Point1XT,:Point2X,:Point2XT'+
           ' ,:LabelText,:LabelLeftTopX,:LabelLeftTopY,:LabelArrowToX,:LabelArrowToY,:LabelCustom)');
        if FLabelProperties.Custom then LCustom := 1 else LCustom := 0;
        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'ChartID','PlaneNumber','TargetDraftID',
                            'SavedMode','TargetDraftXValue','TargetDraftYValue',
                            'TargetDraftYears','TargetDraftRecurance',
                            'IndexOf100','DetPointsChanged','ForceCurveThrough100',
                            'Point1X','Point1XT','Point2X','Point2XT',
                            'LabelText','LabelLeftTopX','LabelLeftTopY','LabelArrowToX','LabelArrowToY','LabelCustom'],
                           [LPrimaryKeyTargetDraft[1], LPrimaryKeyTargetDraft[2],
                            LPrimaryKeyTargetDraft[3], LPrimaryKeyTargetDraft[4],
                            IntToStr(Self.ChartNumber), IntToStr(Self.FPlaneID),
                            IntToStr(Self.TargetDraftID), IntToStr(Integer(Self.FTargetDraftSavedMode)),
                            FloatToStr(Self.FTargetDraftXValue), FloatToStr(Self.FTargetDraftYValue),
                            IntToStr(Self.FTargetDraftYears), FloatToStr(Self.FTargetDraftRecurance),
                            IntToStr(FIndexOf100Value),IntToStr(FDeterministicPointsState),
                            IntToStr(Ord(FForceCurveThrough100)),FloatToStr(Self.FDeterministicPoint1XValue),
                            FloatToStr(Self.FDeterministicPoint1XTValue),FloatToStr(Self.FDeterministicPoint2XValue),
                            FloatToStr(Self.FDeterministicPoint2XTValue),
                            FLabelProperties.Text,
                            IntToStr(FLabelProperties.LeftTopX),
                            IntToStr(FLabelProperties.LeftTopY),
                            IntToStr(FLabelProperties.ArrowToX),
                            IntToStr(FLabelProperties.ArrowToY),
                            IntToStr(LCustom)]);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
    FPureRegressionConstants.ChartNumber   := Self.ChartNumber;
    FPureRegressionPointsArray.ChartNumber := Self.ChartNumber;
    FRegressionConstants.ChartNumber       := Self.ChartNumber;
    FRegressionPointsArray.ChartNumber     := Self.ChartNumber;
    FDeterministicConstants.ChartNumber    := Self.ChartNumber;
    FDeterministicPointsArray.ChartNumber  := Self.ChartNumber;
    FOriginalPointsArray.ChartNumber       := Self.ChartNumber;

    LPrimaryKeyCurvePoint[8] := 'P';
    Result := Result and FPureRegressionConstants.SaveDataToDB(LPrimaryKeyCurvePoint);
    Result := Result and FPureRegressionPointsArray.SaveDataToDB(LPrimaryKeyCurvePoint);

    LPrimaryKeyCurvePoint[8] := 'R';
    Result := Result and FRegressionConstants.SaveDataToDB(LPrimaryKeyCurvePoint);
    Result := Result and FRegressionPointsArray.SaveDataToDB(LPrimaryKeyCurvePoint);

    LPrimaryKeyCurvePoint[8] := 'D';
    Result := Result and FDeterministicConstants.SaveDataToDB(LPrimaryKeyCurvePoint);
    Result := Result and FDeterministicPointsArray.SaveDataToDB(LPrimaryKeyCurvePoint);

    LPrimaryKeyCurvePoint[8] := 'O';
    Result := Result and FOriginalPointsArray.SaveDataToDB(LPrimaryKeyCurvePoint);

    Result := Result and inherited SaveDataToDB(LPrimaryKeyTargetDraft);
    FSaved := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.DoRegressionCurveFitting: boolean;
const OPNAME = 'TYRCTargetDraft.DoRegressionCurveFitting';
var
  LPointsArray: TYRCRecordPointArray;
  LPointsArrayObject: TYRCRecordPointArrayObject;
  LFourPoints: TYRCFourPointsArray;
  LCount : integer;
begin
  Result := False;
  try
    LPointsArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    try
      Finalize(LPointsArray);
      if  AddTwoPointsArrays(FPureRegressionPointsArray.FYRCRecordPointArray,
                          FRegressionPointsArray.FYRCRecordPointArray,LPointsArray) then
      begin
        LPointsArrayObject.AddPoints(LPointsArray);
        Result  := DoCurveFitting(ctTrinomialRegression,FRegressionConstants,LPointsArrayObject, FForceCurveThrough100);
        FDeterministicConstants.CopyValuesFrom(FRegressionConstants);
        Changed := Changed or Result;
      end;

      SetLength(FDeterministicPointsArray.FYRCRecordPointArray,4);
      if GetRegressionFourPoints(LFourPoints) then
      begin
        if ValidFourPoints(LFourPoints) then
        begin
          for LCount := 0 to 3 do
          begin
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].YValue  := LFourPoints[LCount].YValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XValue  := LFourPoints[LCount].XValue;
            FDeterministicPointsArray.FYRCRecordPointArray[LCount].XTValue := LFourPoints[LCount].XTValue;
          end;
          Result := DoCurveFitting(ctTrinomialDeterministic,FDeterministicConstants,FDeterministicPointsArray,FForceCurveThrough100);
          RefreshDeterministicMiddleTwoPoints;
          Changed := Changed or Result;
          FDeterministicPointsState := 0;
        end;
      end;
    finally
      LPointsArrayObject.Free;
      Finalize(LPointsArray);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.AddRegressionPoints(APointsArray: TYRCRecordPointArray): boolean;
const OPNAME = 'TYRCTargetDraft.AddRegressionPoints';
begin
  Result := False;
  try
    if (Length(APointsArray) > 0) then
    begin
      if FRegressionPointsArray.AddPoints(APointsArray) then
      begin
        if(FIndexOf100Value >= 0) then
          FRegressionPointsArray.MovePointToTheEnd(FIndexOf100Value);
        Result := DoRegressionCurveFitting;
        FSaved := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.DeleteRegressionPoint(AIndex: integer): boolean;
const OPNAME = 'TYRCTargetDraft.DeleteRegressionPoint';
begin
  Result := False;
  try
    if FRegressionPointsArray.DeletePoint(AIndex) then
    begin
      Result := DoRegressionCurveFitting;
      FSaved := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.UpdateRegressionPoint(AIndex: integer; AXValue,AXTValue,AYValue: double): boolean;
const OPNAME = 'TYRCTargetDraft.UpdateRegressionPoint';
begin
  Result := False;
  try
    if FRegressionPointsArray.UpdatePoint(AIndex,AXValue,AXTValue,AYValue) then
    begin
      Result := DoRegressionCurveFitting;
      FSaved := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.AddTwoPointsArrays(const AFirstSource,ASecondSource: TYRCRecordPointArray;
         var AResultArray: TYRCRecordPointArray): boolean;
const OPNAME = 'TYRCTargetDraft.AddTwoPointsArrays';
var
  LCount,
  LIndex: integer;
begin
  Result := False;
  try
    SetLength(AResultArray, Length(AFirstSource) + Length(ASecondSource));
    LIndex := 0;
    for LCount := Low(AFirstSource) to high(AFirstSource) do
    begin
      AResultArray[LIndex].YValue  := AFirstSource[LCount].YValue;
      AResultArray[LIndex].XValue  := AFirstSource[LCount].XValue;
      AResultArray[LIndex].XTValue := AFirstSource[LCount].XTValue;
      LIndex := LIndex + 1;
    end;
    for LCount := Low(ASecondSource) to high(ASecondSource) do
    begin
      AResultArray[LIndex].YValue  := ASecondSource[LCount].YValue;
      AResultArray[LIndex].XValue  := ASecondSource[LCount].XValue;
      AResultArray[LIndex].XTValue := ASecondSource[LCount].XTValue;
      LIndex := LIndex + 1;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.ValidateUpdateDeterministicPoint(AIndex: integer; AXValue,AXTValue, AYValue: double): boolean;
const OPNAME = 'TYRCTargetDraft.ValidateUpdateDeterministicPoint';
var
  LDiff: double;
  LYValue : double;
  LXValue : double;
  A,
  B,
  C,
  D : double;
begin
  Result := False;
  try
    if (AIndex >= 0) and (AIndex < Length(FDeterministicPointsArray.FYRCRecordPointArray)) then
    begin
      // You can only edit the first point if there in no break point.
      if(AIndex = 0) then
      begin
        if(FTargetDraftYValue > 0) then
        begin
          A := FPureRegressionConstants.ConstantsArray[0];
          B := FPureRegressionConstants.ConstantsArray[1];
          C := FPureRegressionConstants.ConstantsArray[2];
          D := FPureRegressionConstants.ConstantsArray[3];
          LXValue := FTargetDraftXTValue;
          LYValue := A + (B * LXValue) + (C * Power(LXValue, 2)) + (D * Power(LXValue, 3));
          LDiff := Abs(FTargetDraftYValue - LYValue);
          LDiff :=  (LDiff/FTargetDraftYValue)*100.0;
          Result := LDiff > 0.01;
        end;
      end
      else
      begin
        // X Value must always be more than the prev. xvalue for regression.
        Result := (AXTValue > FDeterministicPointsArray.FYRCRecordPointArray[AIndex -1].XTValue);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.UpdateDeterministicPoint(AIndex: integer; AXValue,AXTValue, AYValue: double): boolean;
const OPNAME = 'TYRCTargetDraft.UpdateDeterministicPoint';
var
  LFourPoints: TYRCFourPointsArray;
  LCount : integer;
begin
  Result := False;
  try
    if(AIndex = 0) then
    begin
      AXValue  := FTargetDraftXValue;
      AXTValue := FTargetDraftXTValue;
    end;

    if ValidateUpdateDeterministicPoint(AIndex,AXValue,AXTValue,AYValue) and
       FDeterministicPointsArray.UpdatePoint(AIndex,AXValue,AXTValue,AYValue) then
    begin
      Result := DoCurveFitting(ctTrinomialDeterministic,FDeterministicConstants,FDeterministicPointsArray,FForceCurveThrough100);
      FRegressionConstants.CopyValuesFrom(FDeterministicConstants);
      Changed := Changed or Result;
      FDeterministicPointsState := 1;
    end;
    SetLength(FDeterministicPointsArray.FYRCRecordPointArray,4);
    if GetRegressionFourPoints(LFourPoints) then
    begin
      if ValidFourPoints(LFourPoints) then
      begin
        for LCount := 0 to 3 do
        begin
          FDeterministicPointsArray.FYRCRecordPointArray[LCount].YValue  := LFourPoints[LCount].YValue;
          FDeterministicPointsArray.FYRCRecordPointArray[LCount].XValue  := LFourPoints[LCount].XValue;
          FDeterministicPointsArray.FYRCRecordPointArray[LCount].XTValue := LFourPoints[LCount].XTValue;
        end;
        Result := DoCurveFitting(ctTrinomialDeterministic,FDeterministicConstants,FDeterministicPointsArray,FForceCurveThrough100);
        if(AIndex = 1) then
        begin
          FDeterministicPoint1XValue  := AXValue;
          FDeterministicPoint1XTValue := AXTValue;
        end;
        if(AIndex = 2) then
        begin
          FDeterministicPoint2XValue  := AXValue;
          FDeterministicPoint2XTValue := AXTValue;
        end;
        RefreshDeterministicMiddleTwoPoints;
        Changed := Changed or Result;
        //FDeterministicPointsState := 0;
      end;
    end;
    FSaved := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.RefreshDeterministicMiddleTwoPoints;
const OPNAME = 'TYRCTargetDraft.RefreshDeterministicMiddleTwoPoints';
var
  LXTValue,
  LYValue : double;
  A,
  B,
  C,
  D : double;
begin
  try
    if (FDeterministicPoint1XValue   = 0.0) and (FDeterministicPoint1XTValue  = 0.0) and
       (FDeterministicPoint2XValue   = 0.0) and (FDeterministicPoint2XTValue  = 0.0) then
      Exit
    else
    begin
      if(FDeterministicPoint1XTValue  <> FDeterministicPointsArray.YRCRecordPointArray[1].XTValue) then
      begin
        A       := DeterministicConstants.ConstantsArray[0];
        B       := DeterministicConstants.ConstantsArray[1];
        C       := DeterministicConstants.ConstantsArray[2];
        D       := DeterministicConstants.ConstantsArray[3];
        LXTValue := FDeterministicPoint1XTValue;
        LYValue := A + (B * LXTValue) + (C * Power(LXTValue, 2)) + (D * Power(LXTValue, 3));
        FDeterministicPointsArray.FYRCRecordPointArray[1].YValue  := LYValue;
        FDeterministicPointsArray.FYRCRecordPointArray[1].XValue  := FDeterministicPoint1XValue;
        FDeterministicPointsArray.FYRCRecordPointArray[1].XTValue := FDeterministicPoint1XTValue;
      end;
      if(FDeterministicPoint2XTValue  <> FDeterministicPointsArray.YRCRecordPointArray[2].XTValue) then
      begin
        A       := DeterministicConstants.ConstantsArray[0];
        B       := DeterministicConstants.ConstantsArray[1];
        C       := DeterministicConstants.ConstantsArray[2];
        D       := DeterministicConstants.ConstantsArray[3];
        LXTValue := FDeterministicPoint2XTValue;
        LYValue := A + (B * LXTValue) + (C * Power(LXTValue, 2)) + (D * Power(LXTValue, 3));
        FDeterministicPointsArray.FYRCRecordPointArray[2].YValue  := LYValue;
        FDeterministicPointsArray.FYRCRecordPointArray[2].XValue  := FDeterministicPoint2XValue;
        FDeterministicPointsArray.FYRCRecordPointArray[2].XTValue := FDeterministicPoint2XTValue;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetChartEditMode(ATargetDraftMode: TChartEditMode);
const OPNAME = 'TYRCTargetDraft.SeTChartEditMode';
begin
  try
    if (FTargetDraftSavedMode <> ATargetDraftMode) then
    begin
      FTargetDraftSavedMode := ATargetDraftMode;
      Changed := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetPlaneID: integer;
const OPNAME = 'TYRCTargetDraft.GetPlaneID';
begin
  Result := -1;
  try
    Result := FPlaneID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftID: integer;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftID';
begin
  Result := -1;
  try
    Result := FTargetDraftID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftRecurance: Double;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftRecurance';
begin
  Result := -1.0;
  try
    Result := FTargetDraftRecurance;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftSavedMode: TChartEditMode;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftSavedMode';
begin
  Result := tdmNone;
  try
    Result := FTargetDraftSavedMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftXValue: Double;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftXValue';
begin
  Result := -1.0;
  try
    Result := FTargetDraftXValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftXTValue: Double;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftXTValue';
begin
  Result := -1.0;
  try
    Result := FTargetDraftXTValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftXTValue(ATargetDraftXTValue: Double);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftXTValue';
begin
  try
     if CompareDouble(FTargetDraftXTValue,ATargetDraftXTValue,3) then
      FTargetDraftXTValue := ATargetDraftXTValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftYears: integer;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftYears';
begin
  Result := -1;
  try
    Result := FTargetDraftYears;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftYValue: Double;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftYValue';
begin
  Result := -1.0;
  try
    Result := FTargetDraftYValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetPlaneID(APlaneID: integer);
const OPNAME = 'TYRCTargetDraft.SetPlaneID';
begin
  try
    if(FPlaneID <> APlaneID) then
      FPlaneID := APlaneID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftID(ATargetDraftID: integer);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftID';
begin
  try
    if(FTargetDraftID <> ATargetDraftID) then
      FTargetDraftID := ATargetDraftID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftRecurance(ATargetDraftRecurance: Double);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftRecurance';
begin
  try
     if CompareDouble(FTargetDraftRecurance,ATargetDraftRecurance,3) then
      FTargetDraftRecurance := ATargetDraftRecurance;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftXValue(ATargetDraftXValue: Double);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftXValue';
begin
  try
     if CompareDouble(FTargetDraftXValue,ATargetDraftXValue,3) then
      FTargetDraftXValue := ATargetDraftXValue;
     SetTargetDraftXTValue(ATargetDraftXValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftYears(ATargetDraftYears: integer);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftYears';
begin
  try
    if(FTargetDraftYears <> ATargetDraftYears) then
      FTargetDraftYears := ATargetDraftYears;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetTargetDraftYValue(ATargetDraftYValue: Double);
const OPNAME = 'TYRCTargetDraft.SetTargetDraftYValue';
begin
  try
     if CompareDouble(FTargetDraftYValue,ATargetDraftYValue,3) then
      FTargetDraftYValue := ATargetDraftYValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetDeterministicConstants: TAbstractYRCFunctionConstants;
const OPNAME = 'TYRCTargetDraft.GetDeterministicConstants';
begin
  Result := nil;
  try
    Result := FDeterministicConstants;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetDeterministicPointsArray: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCTargetDraft.GetDeterministicPointsArray';
begin
  Result := nil;
  try
    Result := FDeterministicPointsArray;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetOriginalPointsArray: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCTargetDraft.GetOriginalPointsArray';
begin
  Result := nil;
  try
    Result := FOriginalPointsArray;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetPureRegressionConstants: TAbstractYRCFunctionConstants;
const OPNAME = 'TYRCTargetDraft.GetPureRegressionConstants';
begin
  Result := nil;
  try
    Result := FPureRegressionConstants;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetPureRegressionPointsArray: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCTargetDraft.GetPureRegressionPointsArray';
begin
  Result := nil;
  try
    Result := FPureRegressionPointsArray;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetRegressionConstants: TAbstractYRCFunctionConstants;
const OPNAME = 'TYRCTargetDraft.GetRegressionConstants';
begin
  Result := nil;
  try
    Result := FRegressionConstants;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetRegressionPointsArray: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCTargetDraft.GetRegressionPointsArray';
begin
  Result := nil;
  try
    Result := FRegressionPointsArray;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetLabelProperties: TLabelProperties;
const OPNAME = 'TYRCTargetDraft.GetLabelProperties';
begin
  Result := nil;
  try
    Result := FLabelProperties;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetTargetDraftSavedModeFromInt(ATargetDraftSavedMode: integer): TChartEditMode;
const OPNAME = 'TYRCTargetDraft.GetTargetDraftSavedModeFromInt';
begin
  Result := tdmNone;
  try
    Result := TChartEditMode(ATargetDraftSavedMode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetYValueAt100: double;
const OPNAME = 'TYRCTargetDraft.GetYValueAt100';
begin
  Result := NullFloat;
  try
    if(FIndexOf100Value >= 0) and (FIndexOf100Value < Length(FRegressionPointsArray.YRCRecordPointArray)) then
      Result := FRegressionPointsArray.YRCRecordPointArray[FIndexOf100Value].YValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetForceCurveThrough100: boolean;
const OPNAME = 'TYRCTargetDraft.GetForceCurveThrough100';
begin
  Result := False;
  try
    Result := FForceCurveThrough100;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.SetForceCurveThrough100(AForce: boolean);
const OPNAME = 'TYRCTargetDraft.SetForceCurveThrough100';
begin
  try
    FForceCurveThrough100 := AForce;
    DoRegressionCurveFitting;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetCurveConstants(AConstants: TYRCFunctionConstants): boolean;
const OPNAME = 'TYRCTargetDraft.GetCurveConstants';
var
  LPointsArrayObject: TYRCRecordPointArrayObject;
  LPointsArray: TYRCRecordPointArray;
  LIndex: integer;
  LValue : double;
begin
  Result := False;
  try
    if(AConstants <> nil) then
    begin
      LPointsArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
      try
        case FTargetDraftSavedMode of
          tdmRegression:
          begin
            if AddTwoPointsArrays(FPureRegressionPointsArray.FYRCRecordPointArray,
                                  FRegressionPointsArray.FYRCRecordPointArray,LPointsArray) then
            begin
              LPointsArrayObject.AddPoints(LPointsArray);
              for LIndex := Low(LPointsArrayObject.FYRCRecordPointArray) to High(LPointsArrayObject.FYRCRecordPointArray) do
              begin
                LValue := (FTargetDraftYValue - LPointsArrayObject.FYRCRecordPointArray[LIndex].YValue)/FTargetDraftYValue;
                LPointsArrayObject.FYRCRecordPointArray[LIndex].YValue := LValue;
              end;

              if not FForceCurveThrough100 then
                LPointsArrayObject.Sort(soDescending);

              for LIndex := Low(LPointsArrayObject.FYRCRecordPointArray) to High(LPointsArrayObject.FYRCRecordPointArray) do
              begin
                //LValue := (LIndex+1)/Length(FPureRegressionPointsArray.FYRCRecordPointArray);
                LValue := 1 - ((LPointsArrayObject.FYRCRecordPointArray[LIndex].XTValue - FTargetDraftXTValue)/(100.0 - FTargetDraftXTValue));
                //LPointsArrayObject.FYRCRecordPointArray[LIndex].XValue  := LValue;
                LPointsArrayObject.FYRCRecordPointArray[LIndex].XTValue := LValue;
              end;

              if not FForceCurveThrough100 then
                LPointsArrayObject.Sort(soAscending);

              Result := DoCurveFitting(ctTrinomialRegression,AConstants,LPointsArrayObject,FForceCurveThrough100);
            end;
          end;
          tdmDeterministic:
          begin
            LPointsArrayObject.AddPoints(FDeterministicPointsArray.FYRCRecordPointArray);
            for LIndex := Low(LPointsArrayObject.FYRCRecordPointArray) to High(LPointsArrayObject.FYRCRecordPointArray) do
            begin
              LValue := (FTargetDraftYValue - LPointsArrayObject.FYRCRecordPointArray[LIndex].YValue)/FTargetDraftYValue;
              LPointsArrayObject.FYRCRecordPointArray[LIndex].YValue := LValue;;
            end;

            if not FForceCurveThrough100 then
              LPointsArrayObject.Sort(soDescending);

            for LIndex := Low(LPointsArrayObject.FYRCRecordPointArray) to High(LPointsArrayObject.FYRCRecordPointArray) do
            begin
              //LPointsArrayObject.FYRCRecordPointArray[LIndex].XValue  := (LIndex+1)/Length(FDeterministicPointsArray.FYRCRecordPointArray);
              LValue := 1 -((LPointsArrayObject.FYRCRecordPointArray[LIndex].XTValue - FTargetDraftXTValue)/(100.0 - FTargetDraftXTValue)) ;
              LPointsArrayObject.FYRCRecordPointArray[LIndex].XValue  := LValue;
              LPointsArrayObject.FYRCRecordPointArray[LIndex].XTValue := LValue;

            end;

            if not FForceCurveThrough100 then
              LPointsArrayObject.Sort(soAscending);

            Result := DoCurveFitting(ctTrinomialDeterministic,AConstants,LPointsArrayObject,FForceCurveThrough100);
          end;
        end;//case
      finally
        LPointsArrayObject.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.GetFormula: string;
const OPNAME = 'TYRCTargetDraft.GetFormula';
var
  LConstants: TYRCFunctionConstants;
begin
  Result := '';
  try
    LConstants         := TYRCFunctionConstants.Create(FAppModules);
    try
      if GetCurveConstants(LConstants) then
        Result := Format('y = (%f) + (%f)x + (%f)x'#178' + (%f)x'#179,
                  [LConstants.ConstantsArray[0],
                  LConstants.ConstantsArray[1],
                  LConstants.ConstantsArray[2],
                  LConstants.ConstantsArray[3]]);

    finally
      LConstants.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.AddYValueAt100(AValue: double);
const OPNAME = 'TYRCTargetDraft.AddYValueAt100';
var
  LYRCRecordPointArray:TYRCRecordPointArray;
begin
  try
    if (FIndexOf100Value < 0) then
    begin
      SetLength(LYRCRecordPointArray,1);
      try
        LYRCRecordPointArray[0].XValue  := 100.0;
        LYRCRecordPointArray[0].XTValue := 100.0;
        LYRCRecordPointArray[0].YValue  := AValue;
        AddRegressionPoints(LYRCRecordPointArray);
        FIndexOf100Value := High(FRegressionPointsArray.YRCRecordPointArray);
      finally
        Finalize(LYRCRecordPointArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.DeleteYValueAt100(AValue: double);
const OPNAME = 'TYRCTargetDraft.DeleteYValueAt100';
begin
  try
    if (FIndexOf100Value >= 0) then
    begin
      FForceCurveThrough100 := False;
      DeleteRegressionPoint(FIndexOf100Value);
      FIndexOf100Value := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.UpdateYValueAt100(AValue: double);
const OPNAME = 'TYRCTargetDraft.UpdateYValueAt100';
begin
  try
    if (FIndexOf100Value >= 0) then
    begin
      UpdateRegressionPoint(FIndexOf100Value,100.0,100.0, AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYRCTargetDraft.SetYValueAt100(AValue: double);
const OPNAME = 'TYRCTargetDraft.SetYValueAt100';
begin
  try
    if (FIndexOf100Value >= 0) then
    begin
      if(AValue = NullFloat) then
        DeleteYValueAt100(AValue)
      else
        UpdateYValueAt100(AValue);
    end
    else
      AddYValueAt100(AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.CurveFitted: boolean;
const OPNAME = 'TYRCTargetDraft.CurveFitted';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.YValueAt100Added: boolean;
const OPNAME = 'TYRCTargetDraft.YValueAt100Added';
begin
  Result := False;
  try
    Result := (FIndexOf100Value >= 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.RegressionPointsAdded: boolean;
const OPNAME = 'TYRCTargetDraft.RegressionPointsAdded';
begin
  Result := False;
  try

    Result := (Length(FRegressionPointsArray.YRCRecordPointArray) > 1) or
              ((Length(FRegressionPointsArray.YRCRecordPointArray) = 1) and (FIndexOf100Value < 0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.DeterministicPointsChanged: boolean;
const OPNAME = 'TYRCTargetDraft.DeterministicPointsChanged';
begin
  Result := False;
  try
    Result := (FDeterministicPointsState <> 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.TransformCurvePoints(AOriginalPointsArray: TAbstractYRCRecordPointArrayObject): boolean;
const OPNAME = 'TYRCTargetDraft.TransformCurvePoints';

  function CalculateTransformedXValue(ASequence, AFailures: integer): double;
  const OPNAME = 'UYRCGraphDataObject.CalculateTransformedXValue';
  begin
    Result := 0.00;
    try
      Result := ASequence / AFailures;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

  function CalculateTransformedYValue(ADeficit, ATargetDraft: double): double;
  const OPNAME = 'UYRCGraphDataObject.CalculateTransformedYValue';
  begin
    Result := 0.00;
    try
      Result := ADeficit * ATargetDraft;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

  function GetNumFailures(AValueArray : array of double): integer;
  const OPNAME = 'UYRCGraphDataObject.GetNumFailures';
  var
    LCount : integer;
  begin
    Result := 0;
    try
      for LCount := 0 to Length(AValueArray) - 1 do
        if AValueArray[LCount] > 0.0000 then
         Result := Result + 1
        else
         Continue;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

var
  LCount             : integer;
  LRankValuesArray   : TIntegerArray;
  LDeficitValueArray : TYRCFlatPointArray;
  LNumSequences,
  LNumFailures       : integer;
  LSequencesToUse    : integer;
begin
  Result := False;
  try
    LNumSequences := Length(AOriginalPointsArray.YRCRecordPointArray);
    SetLength(LDeficitValueArray, LNumSequences);
    SetLength(LRankValuesArray, LNumSequences);
    try
      AOriginalPointsArray.Sort(soDescending);
      for LCount := 0 to High(AOriginalPointsArray.YRCRecordPointArray) do
      begin
        LDeficitValueArray[LCount] := AOriginalPointsArray.YRCRecordPointArray[LCount].YValue;
        LRankValuesArray[LCount]   := (LCount + 1);  //Round(AOriginalPointsArray.YRCRecordPointArray[LCount].XValue);
      end;
      LNumFailures  := GetNumFailures(LDeficitValueArray);
      {if (LNumFailures = LNumSequences) then
        LSequencesToUse := LNumFailures - 1
      else
        LSequencesToUse := LNumFailures;}
      LSequencesToUse := LNumFailures;
      FPureRegressionPointsArray.Reset;
      SetLength(FPureRegressionPointsArray.FYRCRecordPointArray, LSequencesToUse);
      for LCount := 0 to LSequencesToUse - 1 do
      begin
        FPureRegressionPointsArray.FYRCRecordPointArray[LCount].XValue  := CalculateTransformedXValue(LRankValuesArray[LCount], LNumSequences);
        FPureRegressionPointsArray.FYRCRecordPointArray[LCount].XTValue := FPureRegressionPointsArray.FYRCRecordPointArray[LCount].XValue;
        FPureRegressionPointsArray.FYRCRecordPointArray[LCount].YValue  := CalculateTransformedYValue(LDeficitValueArray[LCount], FTargetDraftYValue);
      end;
      Result := DoCurveFitting(ctTrinomialRegression,
                               FPureRegressionConstants,
                               FPureRegressionPointsArray,FForceCurveThrough100);
    finally
      Finalize(LDeficitValueArray);
      Finalize(LRankValuesArray);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCTargetDraft.CalculateTransformedXValue(ASequence,AFailures: integer): double;
const OPNAME = 'TYRCTargetDraft.CalculateTransformedXValue';
var
  LStr : string;
begin
  Result := 0.00;
  try
    LStr    := FormatFloat('###0.0000', (ASequence / AFailures));
    Result := StrToFloat(LStr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCTargetDraft.ApplyPlottingBase(APlottingBase: integer);
const OPNAME = 'TYRCTargetDraft.ApplyPlottingBase';
var
  LIndex: integer;
  LPointsArray: TYRCRecordPointArray;
  LPointsArrayObject: TYRCRecordPointArrayObject;
begin
  try
    for LIndex := Low(FOriginalPointsArray.YRCRecordPointArray) to High(FOriginalPointsArray.YRCRecordPointArray) do
      FOriginalPointsArray.YRCRecordPointArray[LIndex].XTValue :=
      ConvertXValueToPlottingBase(FOriginalPointsArray.YRCRecordPointArray[LIndex].XValue,FTargetDraftYears,APlottingBase);

    for LIndex := Low(FPureRegressionPointsArray.YRCRecordPointArray) to High(FPureRegressionPointsArray.YRCRecordPointArray) do
      FPureRegressionPointsArray.YRCRecordPointArray[LIndex].XTValue :=
      ConvertXValueToPlottingBase(FPureRegressionPointsArray.YRCRecordPointArray[LIndex].XValue,FTargetDraftYears,APlottingBase);

    for LIndex := Low(FRegressionPointsArray.YRCRecordPointArray) to High(FRegressionPointsArray.YRCRecordPointArray) do
      FRegressionPointsArray.YRCRecordPointArray[LIndex].XTValue :=
      ConvertXValueToPlottingBase(FRegressionPointsArray.YRCRecordPointArray[LIndex].XValue,FTargetDraftYears,APlottingBase);

    for LIndex := Low(FDeterministicPointsArray.YRCRecordPointArray) to High(FDeterministicPointsArray.YRCRecordPointArray) do
      FDeterministicPointsArray.YRCRecordPointArray[LIndex].XTValue :=
      ConvertXValueToPlottingBase(FDeterministicPointsArray.YRCRecordPointArray[LIndex].XValue,FTargetDraftYears,APlottingBase);

    FTargetDraftXTValue := ConvertXValueToPlottingBase(FTargetDraftXValue,FTargetDraftYears,APlottingBase);
    DoCurveFitting(ctTrinomialDeterministic,FDeterministicConstants,FDeterministicPointsArray,FForceCurveThrough100);
    DoCurveFitting(ctTrinomialRegression,FPureRegressionConstants,FPureRegressionPointsArray,FForceCurveThrough100);
    //DoCurveFitting(ctTrinomialRegression,FRegressionConstants,FRegressionPointsArray);

    LPointsArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    LPointsArray := nil;
    try
      if  AddTwoPointsArrays(FPureRegressionPointsArray.FYRCRecordPointArray,
                          FRegressionPointsArray.FYRCRecordPointArray,LPointsArray) then
      begin
        LPointsArrayObject.AddPoints(LPointsArray);
        DoCurveFitting(ctTrinomialRegression,FRegressionConstants,LPointsArrayObject,FForceCurveThrough100);
      end;
    finally
      LPointsArrayObject.Free;
      Finalize(LPointsArray);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCPlane }

procedure TYRCPlane.CreateMemberObjects;
const OPNAME = 'TYRCPlane.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTargetDraftList := TObjectList.Create(True);
    FYXPointArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    FYearsArrayObject   := TYRCRecordPointArrayObject.Create(FAppModules);
    FAssurancePointArrayObject := TYRCRecordPointArrayObject.Create(FAppModules);
    FAssuranceYearsArrayObject   := TYRCRecordPointArrayObject.Create(FAppModules);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.DestroyMemberObjects;
const OPNAME = 'TYRCPlane.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FTargetDraftList.Free;
    FYXPointArrayObject.Free;
    FYearsArrayObject.Free;
    FAssurancePointArrayObject.Free;
    FAssuranceYearsArrayObject.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.Reset;
const OPNAME = 'TYRCPlane.Reset';
begin
  inherited Reset;
  try
    FPlaneID           := -1;
    FPlaneYears        := 0;
    FTargetDraftIndex  := -1;
    FTargetDraftList.Clear;
    FYXPointArrayObject.Reset;
    FYearsArrayObject.Reset;
    FAssurancePointArrayObject.Reset;
    FAssuranceYearsArrayObject.Reset;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetTargetDraft(AIndex: Integer): TAbstractYRCTargetDraft;
const OPNAME = 'TYRCPlane.GetTargetDraft';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FTargetDraftList.Count) then
      Result := TYRCTargetDraft(FTargetDraftList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.AddTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): Integer;
const OPNAME = 'TYRCPlane.AddTargetDraft';
var
  LSortData   : TStringList;
  LIndex      : integer;
begin
  Result := -1;
  try
    if Assigned(AYRCTargetDraft) then
    begin
      Result := FTargetDraftList.Add(AYRCTargetDraft);
      AYRCTargetDraft.PlaneID := Self.PlaneID;
    end;
    LSortData := TStringList.Create;
    try
      LSortData.Sorted := True;
      LSortData.Duplicates := dupAccept;

      for LIndex := 0 to FTargetDraftList.Count -1 do
         LSortData.AddObject(FormatFloat('0000000.0000',Self.TargetDraft[LIndex].TargetDraftYValue),
                             FTargetDraftList.Items[LIndex]);

      FTargetDraftList.OwnsObjects := False;
      try
        for LIndex := 0 to FTargetDraftList.Count -1 do
          FTargetDraftList.Items[LIndex] := LSortData.Objects[LIndex];
      finally
        FTargetDraftList.OwnsObjects := True;
      end;
    finally
      LSortData.Free;
    end;
    PopulateYXPointArrayObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYRCPlane.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCPlane.CopyValuesFrom';
var
  LCount: integer;
  LYRCPlane: TYRCPlane;
  LYRCTargetDraft: TYRCTargetDraft;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCPlane) then
    begin
      Result := False;
      LYRCPlane := TYRCPlane(ASource);
      Self.FYXPointArrayObject.CopyValuesFrom(LYRCPlane.FYXPointArrayObject);
      Self.FYearsArrayObject.CopyValuesFrom(LYRCPlane.FYearsArrayObject);
      for LCount := 0 to LYRCPlane.FTargetDraftList.Count - 1 do
      begin
        if Assigned(TargetDraft[LCount]) then
        begin
          LYRCTargetDraft := TYRCTargetDraft.Create(FAppModules);
          Result :=  LYRCTargetDraft.CopyValuesFrom(TargetDraft[LCount]);
          if Result then
          begin
            AddTargetDraft(LYRCTargetDraft);
          end
          else
          begin
            LYRCTargetDraft.Free;
            Break;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCPlane.LoadDataFromDB';
var
  LDatabaseName,
  LWhereClause: string;
  LDataSet: TAbstractModelDataSet;
  LTargetDraft: TYRCTargetDraft;
  LTargetDraftList: TStringList;
  LCount: integer;
  LNewKeyPropertyTargetDraft: array[0..7] of string;
begin
  Result := False;
  try
    Self.Reset;
    Result := (Length(APrimaryKeyProperty) >= 7);
    Result := Result and GetWhereClause(APrimaryKeyProperty,'A.',LDatabaseName,LWhereClause);
    if Result then
    begin
      for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
        LNewKeyPropertyTargetDraft[LCount] := APrimaryKeyProperty[LCount];

      LWhereClause := LWhereClause + ' AND B.Model = A.Model'+
                                     ' AND B.StudyAreaName = A.StudyAreaName'+
                                     ' AND B.SubArea = A.SubArea'+
                                     ' AND B.Scenario = A.Scenario'+
                                     ' AND B.ChartID = A.ChartID'+
                                     ' AND B.PlaneNumber = A.PlaneNumber';

      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' SELECT A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.ChartID,A.PlaneNumber' +
           ' ,A.YearNumber,A.TargetDraftIndex, B.TargetDraftID FROM yrcPlane A,yrcTargetDraft B' +
           ' WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;

        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          Self.ChartNumber        := LDataSet.DataSet.FieldByName('ChartID').AsInteger;
          Self.PlaneID            := LDataSet.DataSet.FieldByName('PlaneNumber').AsInteger;
          Self.FPlaneYears        := LDataSet.DataSet.FieldByName('YearNumber').AsInteger;
          Self.FTargetDraftIndex  := LDataSet.DataSet.FieldByName('TargetDraftIndex').AsInteger;

          LTargetDraftList := TStringList.Create;
          try
            while not LDataSet.DataSet.EOF do
            begin
              if not LDataSet.DataSet.FieldByName('TargetDraftID').IsNull then
                LTargetDraftList.Add(IntToStr(LDataSet.DataSet.FieldByName('TargetDraftID').AsInteger));
              LDataSet.DataSet.Next;
            end;
            LDataSet.DataSet.Close;

            for LCount := 0 to LTargetDraftList.Count -1 do
            begin
              LTargetDraft := TYRCTargetDraft.Create(FAppModules);
              LNewKeyPropertyTargetDraft[7] := LTargetDraftList[LCount];
              Result := LTargetDraft.LoadDataFromDB(LNewKeyPropertyTargetDraft);
              if Result then
                AddTargetDraft(LTargetDraft)
              else
              begin
                LTargetDraft.Free;
                Break;
              end;
            end;
          finally
            LTargetDraftList.Free;
          end;

          Result := Result and inherited LoadDataFromDB(APrimaryKeyProperty);
          if Result then
           PopulateYXPointArrayObject;
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.LoadDataFromFile(AFileBlocks: TObjectList; const AIndexProperty: array of integer): boolean;
const OPNAME = 'TYRCPlane.LoadDataFromFile';
var
  LIndex,
  LCount: integer;
  LIndexProperty: array [0..1] of integer;
  LYRCTargetDraft: TYRCTargetDraft;
  LSumOutAnualSummaryBlockValues: TSumOutAnualSummaryBlockValues;
begin
  Self.Reset;
  Result := inherited LoadDataFromFile(AFileBlocks,AIndexProperty);
  try
    LIndex := AIndexProperty[0];
    LIndexProperty[0] := LIndex;
    if(LIndex >= 0) and (LIndex <= AFileBlocks.Count) then
    begin
      if (AFileBlocks[LIndex] is TSumOutAnualSummaryBlockValues) then
      begin
        LSumOutAnualSummaryBlockValues := TSumOutAnualSummaryBlockValues(AFileBlocks[LIndex]);
        for LCount := Low(LSumOutAnualSummaryBlockValues.FValues) to High(LSumOutAnualSummaryBlockValues.FValues) do
        begin
          if not LSumOutAnualSummaryBlockValues.FValues[LCount].FInitalised then
          Break;
          LYRCTargetDraft := TYRCTargetDraft.Create(FAppModules);
          LIndexProperty[1] := LCount;
          Result := LYRCTargetDraft.LoadDataFromFile(AFileBlocks,LIndexProperty);
          if not Result then
            Break;
          Self.FPlaneID    := LIndex;
          AddTargetDraft(LYRCTargetDraft);
        end;
        if Result then
        begin
          Self.FPlaneYears :=  GetPlaneYears;
          PopulateYXPointArrayObject;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCPlane.SaveDataToDB';
var
  LDatabaseName,
  LWhereClause: string;
  LTableName: string;
  LDataSet: TAbstractModelDataSet;
  LCount: integer;
  LTargetDraft: TYRCTargetDraft;
  LKeyPropertyPlane: array[0..6] of string;
begin
  Result := False;
  try
    if (Length(APrimaryKeyProperty) < 6) then
      Exit ;

    for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
      LKeyPropertyPlane[LCount] := APrimaryKeyProperty[LCount];
    LKeyPropertyPlane[6] := IntToStr(Self.FPlaneID);

    Result := GetWhereClause(LKeyPropertyPlane,'',LDatabaseName,LWhereClause);
    //if Result and Changed then
    if Result then
    begin
      LTableName := 'yrcPlane';
      if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
       Exit;

      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' INSERT INTO yrcPlane' +
           ' (Model,StudyAreaName,SubArea,Scenario,ChartID,PlaneNumber,YearNumber,TargetDraftIndex)' +
           ' VALUES ' +
           ' (:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:PlaneNumber,:YearNumber,:TargetDraftIndex)');

        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'ChartID','PlaneNumber','YearNumber','TargetDraftIndex'],
                           [LKeyPropertyPlane[1], LKeyPropertyPlane[2],
                            LKeyPropertyPlane[3], LKeyPropertyPlane[4],
                            IntToStr(Self.ChartNumber), IntToStr(Self.FPlaneID),
                            IntToStr(Self.FPlaneYears),IntToStr(Self.FTargetDraftIndex)]);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;

    for LCount := 0 to FTargetDraftList.Count -1 do
    begin
      LTargetDraft := TYRCTargetDraft(TargetDraft[LCount]);
      Result :=  Assigned(LTargetDraft);
      if Result then
         LTargetDraft.ChartNumber := Self.ChartNumber;
      Result := Result and LTargetDraft.SaveDataToDB(LKeyPropertyPlane);
      if not Result then
        Break;
    end;
    Result := Result and inherited SaveDataToDB(LKeyPropertyPlane);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.TargetDraftCount: integer;
const OPNAME = 'TYRCPlane.TargetDraftCount';
begin
  Result := 0;
  try
    Result := FTargetDraftList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetPlaneYears: integer;
const OPNAME = 'TYRCPlane.GetPlaneYears';
begin
  Result := 0;
  try
    if(TargetDraftCount > 0) then
      Result := TargetDraft[0].TargetDraftYears;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.Changed: boolean;
const OPNAME = 'TYRCPlane.Changed';
//var
//  LCount: integer;
//  LTargetDraft: TYRCTargetDraft;
begin
  Result := inherited Changed;
  try
    {for LCount := 0 to FTargetDraftList.Count - 1 do
    begin
      if Result then
        Break;
      LTargetDraft := GetTargetDraft(LCount);
      if Assigned(LTargetDraft) then
      if LTargetDraft.Changed then
        Result := True;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.PopulateYXPointArrayObject;
const OPNAME = 'TYRCPlane.PopulateYXPointArrayObject';
var
  LCount: integer;
  LTargetDraft: TYRCTargetDraft;
begin
  try
    SetLength(FYXPointArrayObject.FYRCRecordPointArray,FTargetDraftList.Count);
    SetLength(FYearsArrayObject.FYRCRecordPointArray,FTargetDraftList.Count);

    for LCount := 0 to FTargetDraftList.Count -1 do
    begin
      LTargetDraft := TYRCTargetDraft(TargetDraft[LCount]);
      if Assigned(LTargetDraft) then
      begin
        FYXPointArrayObject.FYRCRecordPointArray[LCount].XValue  := LTargetDraft.TargetDraftXValue;
        FYXPointArrayObject.FYRCRecordPointArray[LCount].XtValue := LTargetDraft.TargetDraftXTValue;
        FYXPointArrayObject.FYRCRecordPointArray[LCount].YValue  := LTargetDraft.TargetDraftYValue;
        FYearsArrayObject.FYRCRecordPointArray[LCount].XValue    := 1.0;
        FYearsArrayObject.FYRCRecordPointArray[LCount].XTValue   := 1.0;
        FYearsArrayObject.FYRCRecordPointArray[LCount].YValue    := LTargetDraft.TargetDraftRecurance;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetPlaneID: integer;
const OPNAME = 'TYRCPlane.GetPlaneID';
begin
  Result := -1;
  try
    Result := FPlaneID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetYearsArrayObject: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCPlane.GetYearsArrayObject';
begin
  Result := nil;
  try
    Result := FYearsArrayObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetYXPointArrayObject: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCPlane.GetYXPointArrayObject';
begin
  Result := nil;
  try
    Result := FYXPointArrayObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.SetPlaneID(APlaneID: integer);
const OPNAME = 'TYRCPlane.SetPlaneID';
begin
  try
    if(FPlaneID <> APlaneID) then
      FPlaneID := APlaneID;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.UpdateSelectedAssuranceIntervalSaved(AAssuranceIntervalArraySaved: TIntegerArray): boolean;
const OPNAME = 'TYRCPlane.UpdateSelectedAssuranceIntervalSaved';
function CalculateXPoint(APointYears,APlaneYears:integer ): double;
const OPNAME = 'UYRCGraphDataObject.CalculateXPoint';
begin
  Result := 1 - (1 - Power(1 - (1/APointYears),APlaneYears));
  Result := Result * 100;
end;

var
  LPlottingBase,
  LIndex : integer;
begin
  Result := False;
  try
    LPlottingBase := TYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject.PlottingBase;
    if(LPlottingBase > 0) then
    begin
      SetLength(FAssurancePointArrayObject.FYRCRecordPointArray,Length(AAssuranceIntervalArraySaved));
      SetLength(FAssuranceYearsArrayObject.FYRCRecordPointArray,Length(AAssuranceIntervalArraySaved));

      for LIndex := Low(AAssuranceIntervalArraySaved) to High(AAssuranceIntervalArraySaved) do
      begin
        if(AAssuranceIntervalArraySaved[LIndex] = 0) then
        begin
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].XValue  := 0.0;
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].XTValue := 0.0;
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].YValue  := 0.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].XValue  := 0.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].XTValue := 0.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].YValue  := 0.0;
        end
        else
        begin
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].XValue  := CalculateXPoint(AAssuranceIntervalArraySaved[LIndex],LPlottingBase);
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].XTValue := FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].XValue;
          FAssurancePointArrayObject.FYRCRecordPointArray[LIndex].YValue  := 100.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].XValue  := 1.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].XTValue := 1.0;
          FAssuranceYearsArrayObject.FYRCRecordPointArray[LIndex].YValue  := AAssuranceIntervalArraySaved[LIndex];
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.UpdateSelectedAssuranceIntervalDefault(AAssuranceIntervalArraySaved: TIntegerArray): boolean;
const OPNAME = 'TYRCPlane.UpdateSelectedAssuranceIntervalDefault';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.UpdateSelectedAssuranceIntervalYears(AAssuranceIntervalArraySaved: TIntegerArray): boolean;
const OPNAME = 'TYRCPlane.UpdateSelectedAssuranceIntervalYears';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetAssurancePointArrayObject: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCPlane.GetAssurancePointArrayObject';
begin
  Result := nil;
  try
    Result := FAssurancePointArrayObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetAssuranceYearsArrayObject: TAbstractYRCRecordPointArrayObject;
const OPNAME = 'TYRCPlane.GetAssuranceYearsArrayObject';
begin
  Result := nil;
  try
    Result := FAssuranceYearsArrayObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetTargetDraftIndex: integer;
const OPNAME = 'TYRCPlane.GetTargetDraftIndex';
begin
  Result := -1;
  try
    Result := FTargetDraftIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.SetTargetDraftIndex(ATargetDraftIndex: integer);
const OPNAME = 'TYRCPlane.SetTargetDraftIndex';
begin
  try
   if(ATargetDraftIndex >= -1) and (ATargetDraftIndex < TargetDraftCount) and (ATargetDraftIndex <> FTargetDraftIndex) then
   begin
     FTargetDraftIndex := ATargetDraftIndex;
     SetChanged(True);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCPlane.ApplyPlottingBase(APlottingBase: integer);
const OPNAME = 'TYRCPlane.ApplyPlottingBase';
var
  LIndex: integer;
  LTargetDraft: TAbstractYRCTargetDraft;
  LAssuranceIntervalArray: TIntegerArray;
begin
  try
    for LIndex := 0 to FTargetDraftList.Count - 1 do
    begin
      LTargetDraft := GetTargetDraft(LIndex);
      if Assigned(LTargetDraft) then
        LTargetDraft.ApplyPlottingBase(APlottingBase);
    end;
    for LIndex := Low(FYXPointArrayObject.YRCRecordPointArray) to High(FYXPointArrayObject.YRCRecordPointArray) do
    begin
      FYXPointArrayObject.YRCRecordPointArray[LIndex].XTValue :=
      ConvertXValueToPlottingBase(FYXPointArrayObject.YRCRecordPointArray[LIndex].XValue,PlaneYears,APlottingBase);
    end;

    if(Length(FAssuranceYearsArrayObject.FYRCRecordPointArray) > 0) then
    begin
      SetLength(LAssuranceIntervalArray,Length(FAssuranceYearsArrayObject.FYRCRecordPointArray));
      for LIndex := Low(FAssuranceYearsArrayObject.YRCRecordPointArray) to High(FAssuranceYearsArrayObject.YRCRecordPointArray) do
      begin
        LAssuranceIntervalArray[LIndex] := Trunc(FAssuranceYearsArrayObject.YRCRecordPointArray[LIndex].YValue);
      end;
      UpdateSelectedAssuranceIntervalSaved(LAssuranceIntervalArray);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.DeleteTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): boolean;
const OPNAME = 'TYRCPlane.DeleteTargetDraft';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(AYRCTargetDraft) then
    begin
      LIndex := FTargetDraftList.IndexOf(AYRCTargetDraft);
      if(LIndex >= 0) then
      begin
       FTargetDraftList.Delete(LIndex);
       PopulateYXPointArrayObject;
       Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetCastTargetDraft(AIndex: Integer): TYRCTargetDraft;
const OPNAME = 'TYRCPlane.GetCastTargetDraft';
var
  LYRCTargetDraft:TAbstractYRCTargetDraft;
begin
  Result := nil;
  try
    LYRCTargetDraft := GetTargetDraft(AIndex);
    if(LYRCTargetDraft <> nil) then
      Result := TYRCTargetDraft(LYRCTargetDraft);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TYRCPlane.GetSelectedTargetDraft: TAbstractYRCTargetDraft;
const OPNAME = 'TYRCPlane.GetSelectedTargetDraft';
begin
  Result := nil;
  try
    Result := GetTargetDraft(FTargetDraftIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.LoadCoefficientFile: boolean;
const OPNAME = 'TYRCPlane.LoadCoefficientFile';
var
  LIndex : integer;
  LLineData,
  LCoefficientFileName: string;
  LSelectFileDialog:TOpenDialog;
  LLineDataContainer,
  LFileContents: TStringList;
  LTargetDraft: double;
  LCoefficientList: TCoefficientList;
  LCoefficient : TCoefficient;
begin
  Result := False;
  try
    LCoefficientFileName := '';
    LSelectFileDialog := TOpenDialog.Create(nil);
    try
      LSelectFileDialog.Options  := LSelectFileDialog.Options + [ofFileMustExist];
      LSelectFileDialog.DefaultExt := 'COF';
      LSelectFileDialog.Filter  := '(*.COF)|*.COF';
      LSelectFileDialog.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LSelectFileDialog.Execute then
        LCoefficientFileName := LSelectFileDialog.FileName;
    finally
      LSelectFileDialog.Free;
    end;

    if FileExists(LCoefficientFileName) then
    begin
      LFileContents      := TStringList.Create;
      LLineDataContainer := TStringList.Create;
      LCoefficientList   := TCoefficientList.Create;
      try
        LFileContents.LoadFromFile(LCoefficientFileName);
        for LIndex := 0 to LFileContents.Count-1 do
        begin
          LLineData := LFileContents[LIndex];
          LLineData := CommaTextString(LLineData);
          LLineDataContainer.CommaText := LLineData;
          if(LLineDataContainer.Count <> 6) then
          begin
            ShowMessage('File ('+LCoefficientFileName+') is not a valid YRC coefficient file format.');
            Exit;
          end;
          LTargetDraft := StrToFloatDef(LLineDataContainer[0],NullFloat);
          if(LTargetDraft = NullFloat) then
          begin
            ShowMessage('File ('+LCoefficientFileName+') is not a valid YRC coefficient file format.');
            Exit;
          end;
          if(LTargetDraft = 1.0) then
            Break;

            LCoefficient := LCoefficientList.NewCoefficient;
            LCoefficient.Reset;
            LCoefficient.TargetDraft  := StrToFloat(LLineDataContainer[0]);
            LCoefficient.ACoefficient := StrToFloat(LLineDataContainer[1]);
            LCoefficient.BCoefficient := StrToFloat(LLineDataContainer[2]);
            LCoefficient.CCoefficient := StrToFloat(LLineDataContainer[3]);
            LCoefficient.DCoefficient := StrToFloat(LLineDataContainer[4]);
            LCoefficient.XBreakPoint  := StrToFloat(LLineDataContainer[5]);
        end;

        if(LCoefficientList.CoefficientCount > 0) then
        begin
          Result := CalculateDeterministicPointsFromCoefficients(LCoefficientList);
        end;
      finally
        LFileContents.Free;
        LLineDataContainer.Free;
        LCoefficientList.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.CalculateDeterministicPointsFromCoefficients(ACoefficientList: TCoefficientList): boolean;
const OPNAME = 'TYRCPlane.CalculateDeterministicPointsFromCoefficients';
var
  LIStep,
  LStepsCount,
  LCount,
  LIndex : integer;
  LXStep,
  LXValue,
  LTransYValue,
  LTransXValue,
  LYValue,
  LTargetDraftXValue,
  LTargetDraftYValue: double;
  LXValues,
  LYValues: array[0..120] of double;
  LCoefficient : TCoefficient;
  LTargetDraft : TYRCTargetDraft;
  LCurveConstantsObject : TYRCFunctionConstants;
  LYXPairsArrayObject   : TYRCRecordPointArrayObject;
begin
  Result := False;
  try
    if(ACoefficientList = nil) or (ACoefficientList.CoefficientCount = 0) then Exit;

    LCurveConstantsObject := TYRCFunctionConstants.Create(FAppModules);
    LYXPairsArrayObject   := TYRCRecordPointArrayObject.Create(FAppModules);
    try
      for LIndex := 0 to ACoefficientList.CoefficientCount-1 do
      begin
        LCoefficient := ACoefficientList.CoefficientByIndex[LIndex];
        LTargetDraft := GetTargetDraftPerTargetValue(LCoefficient.TargetDraft);
        if(LTargetDraft = nil) then
        begin
          ShowMessage('Target draft ('+ FloatToStr(LCoefficient.TargetDraft)+') found in the coefficient file but not found in the current sum.out.');
          Exit;
        end;
      end;

      for LIndex := 0 to ACoefficientList.CoefficientCount-1 do
      begin
        LCoefficient := ACoefficientList.CoefficientByIndex[LIndex];
        LTargetDraft := GetTargetDraftPerTargetValue(LCoefficient.TargetDraft);

        LTargetDraftXValue  := 100.0 - (100.0 * LCoefficient.XBreakPoint);
        LTargetDraftYValue  := LCoefficient.FTargetDraft;

        LTargetDraft.TargetDraftXValue  := LTargetDraftXValue;
        LTargetDraft.TargetDraftXTValue := LTargetDraftXValue;

        LXValue     := LTargetDraftXValue;
        LXStep      := (100.0 - LTargetDraftXValue)/100.0;
        LStepsCount := 0;
        while LXValue < 100.0 do
        begin
          LTransXValue := 1 - ((LXValue - LTargetDraftXValue)/(100.0 - LTargetDraftXValue));
          LTransYValue := LCoefficient.ACoefficient +
                         (LCoefficient.BCoefficient * LTransXValue) +
                         (LCoefficient.CCoefficient * (LTransXValue * LTransXValue)) +
                         (LCoefficient.DCoefficient * (LTransXValue * LTransXValue* LTransXValue));
          LYValue      := LTargetDraftYValue - (LTargetDraftYValue * LTransYValue);

          if(LYValue < 0.0) then
            Break;

          LXValues[LStepsCount] := LXValue;
          LYValues[LStepsCount] := LYValue;
          LStepsCount := LStepsCount + 1;
          LXValue := LXValue + LXStep;
        end;

        if(LStepsCount > 0) then
        begin
          LCurveConstantsObject.Reset;
          LYXPairsArrayObject.Reset;

          SetLength(LYXPairsArrayObject.FYRCRecordPointArray,4);

          LIStep := LStepsCount div 3;
          LCount := 0;
          LYXPairsArrayObject.FYRCRecordPointArray[0].YValue  := LYValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[0].XValue  := LXValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[0].XTValue := LXValues[LCount];
          LCount := LIStep;
          LYXPairsArrayObject.FYRCRecordPointArray[1].YValue  := LYValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[1].XValue  := LXValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[1].XTValue := LXValues[LCount];
          LCount := LIStep+ LIStep;
          LYXPairsArrayObject.FYRCRecordPointArray[2].YValue  := LYValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[2].XValue  := LXValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[2].XTValue := LXValues[LCount];
          LCount := LStepsCount-1;
          LYXPairsArrayObject.FYRCRecordPointArray[3].YValue  := LYValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[3].XValue  := LXValues[LCount];
          LYXPairsArrayObject.FYRCRecordPointArray[3].XTValue := LXValues[LCount];

          if LTargetDraft.DoCurveFitting(ctTrinomialDeterministic,LCurveConstantsObject,LYXPairsArrayObject,True) then
          begin
            LTargetDraft.FDeterministicConstants.CopyValuesFrom(LCurveConstantsObject);

            LTargetDraft.FDeterministicPointsArray.Reset;
            SetLength(LTargetDraft.FDeterministicPointsArray.FYRCRecordPointArray,4);

            LYXPairsArrayObject.FYRCRecordPointArray[0].YValue  := LTargetDraft.TargetDraftYValue;
            LYXPairsArrayObject.FYRCRecordPointArray[0].XValue  := LTargetDraft.TargetDraftXValue;
            LYXPairsArrayObject.FYRCRecordPointArray[0].XTValue := LTargetDraft.TargetDraftXTValue;

            LXStep  := (100.0 - LTargetDraft.TargetDraftXValue) / 3.0;

            LXValue := LTargetDraft.TargetDraftXValue + LXStep;
            LYValue := LCurveConstantsObject.ConstantsArray[0] +
                      (LCurveConstantsObject.ConstantsArray[1] * LXValue) +
                      (LCurveConstantsObject.ConstantsArray[2] * (LXValue * LXValue)) +
                      (LCurveConstantsObject.ConstantsArray[3] * (LXValue * LXValue* LXValue));
            LYXPairsArrayObject.FYRCRecordPointArray[1].YValue  := LYValue;
            LYXPairsArrayObject.FYRCRecordPointArray[1].XValue  := LXValue;
            LYXPairsArrayObject.FYRCRecordPointArray[1].XTValue := LXValue;

            LXValue := LTargetDraft.TargetDraftXValue + LXStep + LXStep;
            LYValue := LCurveConstantsObject.ConstantsArray[0] +
                      (LCurveConstantsObject.ConstantsArray[1] * LXValue) +
                      (LCurveConstantsObject.ConstantsArray[2] * (LXValue * LXValue)) +
                      (LCurveConstantsObject.ConstantsArray[3] * (LXValue * LXValue* LXValue));
            LYXPairsArrayObject.FYRCRecordPointArray[2].YValue  := LYValue;
            LYXPairsArrayObject.FYRCRecordPointArray[2].XValue  := LXValue;
            LYXPairsArrayObject.FYRCRecordPointArray[2].XTValue := LXValue;

            LXValue := 100.0;
            LYValue := LCurveConstantsObject.ConstantsArray[0] +
                      (LCurveConstantsObject.ConstantsArray[1] * LXValue) +
                      (LCurveConstantsObject.ConstantsArray[2] * (LXValue * LXValue)) +
                      (LCurveConstantsObject.ConstantsArray[3] * (LXValue * LXValue* LXValue));
            LYXPairsArrayObject.FYRCRecordPointArray[3].YValue  := LYValue;
            LYXPairsArrayObject.FYRCRecordPointArray[3].XValue  := LXValue;
            LYXPairsArrayObject.FYRCRecordPointArray[3].XTValue := LXValue;

            LTargetDraft.FDeterministicPointsArray.CopyValuesFrom(LYXPairsArrayObject);

            LTargetDraft.FDeterministicPoint1XValue   := LYXPairsArrayObject.FYRCRecordPointArray[1].XValue;
            LTargetDraft.FDeterministicPoint1XTValue  := LYXPairsArrayObject.FYRCRecordPointArray[1].XValue;
            LTargetDraft.FDeterministicPoint2XValue   := LYXPairsArrayObject.FYRCRecordPointArray[2].XValue;
            LTargetDraft.FDeterministicPoint2XTValue  := LYXPairsArrayObject.FYRCRecordPointArray[2].XValue;

            LTargetDraft.FSaved := False;
            LTargetDraft.Changed := True;
            LTargetDraft.FTargetDraftSavedMode := tdmDeterministic;
            LTargetDraft.FDeterministicPointsState := 0;
          end;
        end;
      end;
    finally
      LCurveConstantsObject.Free;
      LYXPairsArrayObject.Free;
    end;


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCPlane.GetTargetDraftPerTargetValue(AValue: double): TYRCTargetDraft;
const OPNAME = 'TYRCPlane.GetTargetDraftPerTargetValue';
var
  LIndex : integer;
  LYRCTargetDraft: TYRCTargetDraft;
begin
  Result := nil;
  try
    for LIndex := 0 to TargetDraftCount-1 do
    begin
      LYRCTargetDraft := GetCastTargetDraft(LIndex);
      if(Abs(AValue - LYRCTargetDraft.TargetDraftYValue) < 0.0001) then
      begin
        Result := LYRCTargetDraft;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYRCGraphDataObject }

procedure TYRCGraphDataObject.CreateMemberObjects;
const OPNAME = 'TYRCGraphDataObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPlaneIndex          := -1;
    FPlottingBase        := -1;
    FChartName           := '';
    FErrorMsg            := '';
    FPlanesList          := TObjectList.Create(True);
    FYRCLanguageStrings  := TYRCLanguageStrings.Create(FAppModules);
    FYRCChartProperties  := TYRCChartProperties.Create(FAppModules);

    SetLength(FAssuranceIntervalYearsArray, 1);

    SetLength(FAssuranceIntervalSavedArray, 1);

    SetLength(FAssuranceIntervalDefaultArray, 1);
    //FAssuranceIntervalArray[0] := 20;
    //FAssuranceIntervalArray[1] := 50;
    //FAssuranceIntervalArray[2] := 100;
    //FAssuranceIntervalArray[3] := 200;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.DestroyMemberObjects;
const OPNAME = 'TYRCGraphDataObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    SaveSelectedAssuranceInterval;
    FPlanesList.Free;
    FYRCLanguageStrings.Free;
    FYRCChartProperties.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.Reset;
const OPNAME = 'TYRCGraphDataObject.Reset';
begin
  inherited Reset;
  try
    FPlaneIndex          := -1;
    FPlottingBase        := -1;
    FChartName           := '';
    FErrorMsg            := '';
    FPlanesList.Clear;
    FYRCLanguageStrings.Reset;
    FYRCChartProperties.Reset;
    LoadSelectedAssuranceInterval;
    SetAssuranceIntervalSavedArray(FAssuranceIntervalSavedArray);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.Initialise: boolean;
const OPNAME = 'TYRCGraphDataObject.Initialise';
begin
  Result := False;
  try
    LoadSelectedAssuranceInterval;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetPlane(AIndex: Integer): TAbstractYRCPlane;
const OPNAME = 'TYRCGraphDataObject.GetPlane';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FPlanesList.Count) then
      Result := TYRCPlane(FPlanesList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCGraphDataObject.CopyValuesFrom';
var
  LCount: integer;
  LYRCPlane: TYRCPlane;
  LYRCGraphDataObject: TYRCGraphDataObject;
begin
  Result := inherited CopyValuesFrom(ASource);
  try
    if Result and (ASource is TYRCGraphDataObject) then
    begin
      Result := False;
      LYRCGraphDataObject := TYRCGraphDataObject(ASource);
      Result := FYRCLanguageStrings.CopyValuesFrom(ASource) and
                FYRCChartProperties.CopyValuesFrom(ASource);
      if Result then
      begin
        for LCount := 0 to LYRCGraphDataObject.FPlanesList.Count - 1 do
        begin
          if Assigned(YRCPlane[LCount]) then
          begin
            LYRCPlane := TYRCPlane.Create(FAppModules);
            Result :=  LYRCPlane.CopyValuesFrom(YRCPlane[LCount]);
            if Result then
            begin
              AddPlane(LYRCPlane);
            end
            else
            begin
              LYRCPlane.Free;
              Break;
            end;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.LoadDataFromDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.LoadDataFromDB';
var
  LDatabaseName,
  LWhereClause: string;
  LDataSet: TAbstractModelDataSet;
  LYRCPlane : TYRCPlane;
  LCount: integer;
  LNewKeyProperty: array[0..5] of string;
  LNewKeyPropertyPlane: array[0..6] of string;
  LPlanesList: TStringList;
begin
  Result := False;
  try
    Self.Reset;
    Result := (Length(APrimaryKeyProperty) >= 5);
    Result := Result and GetWhereClause(APrimaryKeyProperty,'A.',LDatabaseName,LWhereClause);
    if Result then
    begin
      for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
      begin
        LNewKeyProperty[LCount]      := APrimaryKeyProperty[LCount];
        LNewKeyPropertyPlane[LCount] := APrimaryKeyProperty[LCount];
      end;

      LWhereClause := LWhereClause + ' AND B.Model = A.Model'+
                                     ' AND B.StudyAreaName = A.StudyAreaName'+
                                     ' AND B.SubArea = A.SubArea'+
                                     ' AND B.Scenario = A.Scenario'+
                                     ' AND B.ChartID = A.ChartID';

      Result := False;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' SELECT' +
           ' A.Model,A.StudyAreaName,A.SubArea,A.Scenario,A.ChartID,A.DateCreated' +
           ' ,A.ChartName,A.CurrentChart,A.PlaneIndex,A.RequiredPlaneIndex,B.YearNumber,B.PlaneNumber' +
           ' FROM yrcChart A, yrcPlane B WHERE ' + LWhereClause);
        LDataSet.DataSet.Open;

        Self.ChartNumber      := -1;
        Self.FPlaneIndex      := -1;
        if LDataSet.DataSet.RecordCount <= 0 then
          Result := True
        else
        begin
          Self.ChartNumber      := LDataSet.DataSet.FieldByName('ChartID').AsInteger;
          if not LDataSet.DataSet.FieldByName('PlaneIndex').IsNull then
           Self.FPlaneIndex := LDataSet.DataSet.FieldByName('PlaneIndex').AsInteger;
          if not LDataSet.DataSet.FieldByName('RequiredPlaneIndex').IsNull then
           Self.FPlottingBase  := LDataSet.DataSet.FieldByName('RequiredPlaneIndex').AsInteger;
          Self.FChartName        := Trim(LDataSet.DataSet.FieldByName('ChartName').AsString);

          LNewKeyPropertyPlane[5] := IntToStr(ChartNumber);
          LNewKeyProperty[5]      := IntToStr(ChartNumber);

          Result := FYRCLanguageStrings.LoadDataFromDB(LNewKeyProperty);
          Result := Result and FYRCChartProperties.LoadDataFromDB(LNewKeyProperty);

          if Result then
          begin
            LPlanesList := TStringList.Create;
            try
              while not LDataSet.DataSet.EOF do
              begin
                if not LDataSet.DataSet.FieldByName('PlaneNumber').IsNull then
                  LPlanesList.Add(IntToStr(LDataSet.DataSet.FieldByName('PlaneNumber').AsInteger));
                LDataSet.DataSet.Next;
              end;
              LDataSet.DataSet.Close;

              for LCount := 0 to LPlanesList.Count -1 do
              begin
                LYRCPlane := TYRCPlane.Create(FAppModules);
                LNewKeyPropertyPlane[6] := LPlanesList[LCount];
                Result := LYRCPlane.LoadDataFromDB(LNewKeyPropertyPlane);
                if Result then
                  AddPlane(LYRCPlane)
                else
                begin
                  LYRCPlane.Free;
                  Break;
                end;
              end;
            finally
              LPlanesList.Free;
            end;
          end;
          if Result then
          begin
            //CalculateMaxYield;
            //CalculateSummaries;
            UpdateSelectedAssuranceIntervalSaved;
          end;
          Result := Result and inherited LoadDataFromDB(APrimaryKeyProperty);
        end;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;

    if not Result then
    begin
      Self.Reset;
      Result := True;
    end;
    if Result and (PlanesCount > 0) then
    begin
      if(FPlottingBase > 0) then
      begin
        LCount := FPlottingBase;
        FPlottingBase := -1;
        PlottingBase  := LCount;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.SaveDataToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.SaveDataToDB';
var
 LCount: integer;
 LYRCPlane : TYRCPlane;
 LKeyPropertyChart: array[0..5] of string;
begin
  Result := False;
  try
    if (Length(APrimaryKeyProperty) < 5) then
      Exit ;

    Result := GetChartName;
    Result := Result and GetChartNumberFromPrimaryKey(APrimaryKeyProperty);
    if Result then
    begin
      for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
        LKeyPropertyChart[LCount] := APrimaryKeyProperty[LCount];
      LKeyPropertyChart[5] := IntToStr(Self.ChartNumber);

      FYRCLanguageStrings.ChartNumber := Self.ChartNumber;
      FYRCChartProperties.ChartNumber := Self.ChartNumber;

      DeleteChartDatabaseData(LKeyPropertyChart);
      Application.ProcessMessages;
      Result := CreateCoefficientFile;

      Result := Result and SaveChartIdToDB(LKeyPropertyChart);
      Result := Result and FYRCLanguageStrings.SaveDataToDB(LKeyPropertyChart);
      Result := Result and FYRCChartProperties.SaveDataToDB(LKeyPropertyChart);
      Application.ProcessMessages;
      if Result then
      begin
        for LCount := 0 to FPlanesList.Count - 1 do
        begin
          LYRCPlane := TYRCPlane(YRCPlane[LCount]);
          Result :=  Assigned(LYRCPlane);
          if Result then
            LYRCPlane.ChartNumber := Self.ChartNumber;
          Result := Result and LYRCPlane.SaveDataToDB(LKeyPropertyChart);
          Application.ProcessMessages;
          if not Result then
            Break;
        end;
      end;
      Result := Result and inherited SaveDataToDB(LKeyPropertyChart);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;
const OPNAME = 'TYRCGraphDataObject.LoadDataFromFile';
var
  LCount: integer;
  LYRCPlane:TYRCPlane;
begin
  Self.Reset;
  Result := inherited LoadDataFromFile(AFileBlocks,AIndexProperty);
  try
    if(AFileBlocks.Count > 0) then
    begin
      Result := Result and FYRCLanguageStrings.LoadDataFromFile(AFileBlocks,AIndexProperty);
      Result := Result and FYRCChartProperties.LoadDataFromFile(AFileBlocks,AIndexProperty);
      for LCount := 0 to AFileBlocks.Count - 1 do
      begin
        if not Result then
          Break;
        LYRCPlane := TYRCPlane.Create(FAppModules);
        AddPlane(LYRCPlane);
        Result := Result and LYRCPlane.LoadDataFromFile(AFileBlocks,[LCount]);
        if not Result then
          Break;
      end;

      if Result then
      begin
        CalculateMaxYield;
        //CalculateSummaries;
        //UpdateSelectedAssuranceIntervalSaved;
      end;
    end;

    if not Result then
    begin
      Self.Reset;
      Result := True;
    end;

    if Result and (PlanesCount > 0) then
    begin
     Self.PlaneIndex        := 0;
     if(SelectedPlane <> nil) then
       Self.PlottingBase  := SelectedPlane.PlaneYears
     else
       Self.PlottingBase  := 0;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.DeleteTargetDraft(ATargetDraftIndex: Integer): boolean;
const OPNAME = 'TYRCGraphDataObject.DeleteTargetDraft';
var
  LIndex: integer;
  LYRCPlane    : TYRCPlane;
  LTargetDraft : TYRCTargetDraft;
begin
  Result := False;
  try
    for LIndex := 0 to FPlanesList.Count-1 do
    begin
      LYRCPlane := GetCastPlane(LIndex);
      if Assigned(LYRCPlane) then
      begin
        LTargetDraft := LYRCPlane.GetCastTargetDraft(ATargetDraftIndex);
        if(LTargetDraft <> nil) then
          LYRCPlane.DeleteTargetDraft(LTargetDraft);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.AddPlane(AYRCPlane: TAbstractYRCPlane): Integer;
const OPNAME = 'TYRCGraphDataObject.AddPlane';
begin
  Result := -1;
  try
    if Assigned(AYRCPlane) then
    begin
      Result := FPlanesList.Add(AYRCPlane);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.MergeData(ANewData: TAbstractYRCGraphDataObject;
         ADeleteIndexCommaText,AAddIndexCommaText: string): boolean;
const OPNAME = 'TYRCGraphDataObject.MergeData';
var
  LSourceYRCPlane,
  LDestYRCPlane     : TYRCPlane;
  LTargetDraft      : TYRCTargetDraft;
  LDestTargetDraft  : TYRCTargetDraft;
  LData             : TStringList;
  LPlaneIndex,
  LTargetDraftIndex,
  LIndex            : integer;
begin
  Result := False;
  try
    if(ANewData = nil) then Exit;
    if(Trim(AAddIndexCommaText) = '') then Exit;
    LData := TStringList.Create;
    try
      LData.CommaText := ADeleteIndexCommaText;
      for LIndex := LData.Count-1 downto 0 do
      begin
        LTargetDraftIndex := StrToIntDef(LData[LIndex],-1);
        if(LTargetDraftIndex >= 0) then
          Self.DeleteTargetDraft(LTargetDraftIndex);
      end;
      LData.Clear;
      LData.CommaText := AAddIndexCommaText;
      for LIndex := 0 to LData.Count -1 do
      begin
        LTargetDraftIndex := StrToIntDef(LData[LIndex],-1);
        if(LTargetDraftIndex >= 0) then
        begin
          for LPlaneIndex := 0 to Self.FPlanesList.Count -1 do
          begin
            LSourceYRCPlane := TYRCGraphDataObject(ANewData).GetCastPlane(LPlaneIndex);
            LDestYRCPlane   := Self.GetCastPlane(LPlaneIndex);
            if(LSourceYRCPlane <> nil) and (LDestYRCPlane <> nil)then
            begin
              LTargetDraft    := LSourceYRCPlane.GetCastTargetDraft(LTargetDraftIndex);
              if(LTargetDraft <> nil) then
              begin
                LDestTargetDraft := TYRCTargetDraft.Create(FAppModules);
                LDestTargetDraft.CopyValuesFrom(LTargetDraft);
                LDestYRCPlane.AddTargetDraft(LDestTargetDraft);
              end;
            end;
          end;
        end;
      end;
      CalculateMaxYield;
      //CalculateSummaries;
      //UpdateSelectedAssuranceIntervalSaved;
      Result := True;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.CalculateMaxYield;
const OPNAME = 'TYRCGraphDataObject.CalculateMaxYield';
var
  LIndex,
  LCount: integer;
  LMaxYield: double;
  LMaxValue: integer;
begin
  try
    LMaxYield := 0.0;
    for LCount := 0 to FPlanesList.Count - 1 do
    begin
      for LIndex := 0 to YRCPlane[LCount].TargetDraftCount - 1 do
      begin
       LMaxYield := max(LMaxYield,YRCPlane[LCount].TargetDraft[LIndex].TargetDraftYValue);
      end;
    end;
    LMaxYield  := LMaxYield + (LMaxYield * 0.10);
    LMaxValue  := Ceil(LMaxYield);
    FYRCChartProperties.FMaxYValue          := LMaxYield;
    FYRCChartProperties.FLeftAxisMaximum    := LMaxValue;
    FYRCChartProperties.FRightAxisMaximum   := LMaxValue;
    {if(LMaxYield > 0.0) then
    begin
      LMaxValue := Trunc(LMaxYield);
      LMaxValue := (LMaxValue + 50) - (LMaxValue mod 50);
      FYRCChartProperties.FLeftAxisMaximum           := LMaxValue;
      FYRCChartProperties.FRightAxisMaximum          := LMaxValue;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetSequencesCount: integer;
const OPNAME = 'TYRCGraphDataObject.GetSequencesCount';
var
  LPlane: TYRCPlane;
  LYRCTargetDraft: TYRCTargetDraft;
begin
  Result := 0;
  try
    LPlane := nil;
    LYRCTargetDraft := nil;
    if (FPlanesList.Count > 0) and Assigned(FPlanesList.Items[0]) then
      LPlane := TYRCPlane(FPlanesList.Items[0]);
    if Assigned(LPlane) and (LPlane.FTargetDraftList.Count > 0) and Assigned(LPlane.FTargetDraftList.Items[0]) then
      LYRCTargetDraft := TYRCTargetDraft(LPlane.FTargetDraftList.Items[0]);
    if Assigned(LYRCTargetDraft) then
      Result := Length(LYRCTargetDraft.FOriginalPointsArray.FYRCRecordPointArray);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetYearsCount: integer;
const OPNAME = 'TYRCGraphDataObject.GetYearsCount';
begin
  Result := 0;
  try
    Result := FPlanesList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TYRCGraphDataObject.CalculateSummaries;
const OPNAME = 'TYRCGraphDataObject.CalculateSummaries';
var
  LPlane: TYRCPlane;
  LYRCTargetDraft: TYRCTargetDraft;
begin
  try
    FYRCChartProperties.YearsCount := FPlanesList.Count;
    FYRCChartProperties.PlanesCount:= FPlanesList.Count;
    FYRCChartProperties.SequencesCount := 0;

    LPlane := nil;
    LYRCTargetDraft := nil;
    if (FPlanesList.Count > 0) and Assigned(FPlanesList.Items[0]) then
      LPlane := TYRCPlane(FPlanesList.Items[0]);
    if Assigned(LPlane) and (LPlane.FTargetDraftList.Count > 0) and Assigned(LPlane.FTargetDraftList.Items[0]) then
      LYRCTargetDraft := TYRCTargetDraft(LPlane.FTargetDraftList.Items[0]);
    if Assigned(LYRCTargetDraft) then
      FYRCChartProperties.SequencesCount := Length(LYRCTargetDraft.FOriginalPointsArray.FYRCRecordPointArray);
  except on E: Exception do HandleError(E, OPNAME) end;
end;}


function TYRCGraphDataObject.GetPlanesCount: integer;
const OPNAME = 'TYRCGraphDataObject.GetPlanesCount';
begin
  Result := 0;
  try
    Result := FPlanesList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetTargetDraft(APlaneIndex, ATargetDraftIndex: Integer): TAbstractYRCTargetDraft;
const OPNAME = 'TYRCGraphDataObject.GetTargetDraft';
var
  LPlane : TYRCPlane;
begin
  Result := nil;
  try
    LPlane := TYRCPlane(YRCPlane[APlaneIndex]);
    if Assigned(LPlane) then
      Result := LPlane.TargetDraft[ATargetDraftIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetTargetDraftSavedMode(APlaneIndex, ATargetDraftIndex: Integer): TChartEditMode;
const OPNAME = 'TYRCGraphDataObject.GetTargetDraftSavedMode';
var
  LYRCTargetDraft: TYRCTargetDraft;
begin
  Result := tdmNone;
  try
    LYRCTargetDraft := TYRCTargetDraft(YRCTargetDraft[APlaneIndex,ATargetDraftIndex]);
    if Assigned(LYRCTargetDraft) then
      Result := LYRCTargetDraft.TargetDraftSavedMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.Changed: boolean;
const OPNAME = 'TYRCGraphDataObject.Changed';
//var
//  LCount: integer;
//  LPlane: TYRCPlane;
begin
  Result := inherited Changed;
  try
    //for LCount := 0 to FPlanesList.Count - 1 do
    //begin
    //  if Result then
    //    Break;
    //  LPlane := GetPlane(LCount);
    //  if Assigned(LPlane) then
    //  if LPlane.Changed then
    //    Result := True;
    //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.CalculateTransformedXValue(ASequence, AFailures: integer): double;
const OPNAME = 'TYRCGraphDataObject.CalculateTransformedXValue';
var
  LStr : string;
begin
  Result := 0.00;
  try
    LStr    := FormatFloat('###0.0000', (ASequence / AFailures));
    Result := StrToFloat(LStr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.CreateCoefficientFile: boolean;
const OPNAME = 'TYRCGraphDataObject.CreateCoefficientFile';
var
  LIndex        : integer;
  LCount        : integer;
  LPlane        : TYRCPlane;
  LTargetDraft  : TYRCTargetDraft;
  LLineData     : string;
  LFileName     : string;
  LFileData     : TStringList;
  LFilter       : string;
  LDefaultExt   : string;
  LTitle        : string;
  LInitialDir   : string;
  LFormat       : string;
  lConstants    : TYRCFunctionConstants;
begin
  Result := False;
  try
    LPlane := TYRCPlane(GetPlane(FPlaneIndex));
    if Assigned(LPlane) then
    begin
      LFilter     := '*.cof';
      LDefaultExt := 'cof';
      LTitle      := 'Select cofficient file name(Cancel to skip creating file)';
      LInitialDir := '';
      LFormat     := '####0.000000';
      if PromptForFileName(LFileName,LFilter,LDefaultExt,LTitle,LInitialDir,True) then
      if (LFileName <> '') then
      begin
        LFileData    := TStringList.Create;
        lConstants   := TYRCFunctionConstants.Create(FAppModules);
        try
          for LIndex := 0 to LPlane.TargetDraftCount -1 do
          begin
            LTargetDraft := TYRCTargetDraft(LPlane.TargetDraft[LIndex]);
            if Assigned(LTargetDraft) then
            begin
              lConstants.Reset;
              if LTargetDraft.GetCurveConstants(lConstants) then
              begin
                LLineData := FormatFloat(LFormat,LTargetDraft.FTargetDraftYValue) + ' ';
                for LCount := 0 to 3 do
                  LLineData := LLineData + FormatFloat(LFormat,lConstants.ConstantsArray[LCount]) + ' ';
                if(Length(LTargetDraft.FPureRegressionPointsArray.FYRCRecordPointArray) = 1) then
                  LLineData := LLineData + FormatFloat(LFormat,0.0) + ' '
                else
                  LLineData := LLineData + FormatFloat(LFormat,1 - (LTargetDraft.FTargetDraftXTValue /100.0)) + ' ';
                LFileData.Add(LLineData);
              end;
            end;
          end;
          LLineData := '1.000000 0.000000 0.000000 0.000000 0.000000 0.000000';
          for LCount := LFileData.Count to 10 do
            LFileData.Add(LLineData);
          LFileData.SaveToFile(LFileName);
          Result := True;
        finally
          LFileData.Free;
          lConstants.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetChartName: boolean;
const OPNAME = 'TYRCGraphDataObject.GetChartName';
var
  LChartName: string;
begin
  Result := False;
  try
    if(FChartName <> '') then
    begin
       Result := True;
    end
    else
    begin
      LChartName := InputBox('Enter the chart name','Chart Name',FChartName);
      if(LChartName <> FChartName) then
      begin
        FChartName := LChartName;
        SetChanged(True);
      end;
      Result := (Trim(FChartName) <> '');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetChartNumberFromPrimaryKey(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.GetChartNumberFromPrimaryKey';
var
  LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    if(ChartNumber >= 0) then
    begin
       Result := True;
    end
    else
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL('SELECT MAX(ChartID) AS MaxChartID FROM yrcChart');
        LDataSet.DataSet.Open;
        if (LDataSet.DataSet.RecordCount = 0) then
          ChartNumber := 0
        else
          ChartNumber := LDataSet.DataSet.FieldByName('MaxChartID').AsInteger;
        Result := True;
        SetChanged(True);
      finally
        LDataSet.Free;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.SaveChartIdToDB(const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.SaveChartIdToDB';
var
  LDatabaseName,
  LWhereClause: string;
  //LTableName: string;
  LDataSet: TAbstractModelDataSet;
begin
  Result := False;
  try
    {if not Changed then
    begin
      Result := True;
      Exit;
    end;}

    if(Length(APrimaryKeyProperty) >= 6)  and
       GetWhereClause(APrimaryKeyProperty,'',LDatabaseName,LWhereClause) then
    begin
      //LTableName := 'yrcChart';
      //if not ClearTableData(LDatabaseName,LTableName,LWhereClause) then
      // Exit;

      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL
          (' INSERT INTO yrcChart' +
           ' (Model,StudyAreaName,SubArea,Scenario,ChartID,DateCreated,ChartName' +
           ' ,PlaneIndex,RequiredPlaneIndex)' +
           ' VALUES ' +
           ' (:Model,:StudyAreaName,:SubArea,:Scenario,:ChartID,:DateCreated,:ChartName' +
           ' ,:PlaneIndex,:RequiredPlaneIndex)');

        LDataSet.SetParams(['Model','StudyAreaName','SubArea','Scenario',
                            'ChartID','DateCreated','ChartName','PlaneIndex',
                            'RequiredPlaneIndex'],
                           [APrimaryKeyProperty[1], APrimaryKeyProperty[2],
                            APrimaryKeyProperty[3], APrimaryKeyProperty[4],
                            IntToStr(Self.ChartNumber), DateToStr(Now),
                            Self.FChartName, IntToStr(Self.FPlaneIndex),
                            IntToStr(Self.FPlottingBase)]);
        LDataSet.ExecSQL;
        Result := True;
        SetChanged(False);
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetPlottingBase(APlottingBase: Integer);
const OPNAME = 'TYRCGraphDataObject.SetPlottingBase';
var
  LIndex: integer;
  LPlane: TAbstractYRCPlane;
begin
  try
    if(APlottingBase <> FPlottingBase) and (APlottingBase > 0)  then
    begin
      FPlottingBase := APlottingBase;
      //FYRCChartProperties.FPlottingBase := APlottingBase;
      SetChanged(True);
      for LIndex := 0 to FPlanesList.Count-1 do
      begin
        LPlane := GetPlane(LIndex);
        if(LPlane <> nil)  then
          LPlane.ApplyPlottingBase(APlottingBase);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetPlaneIndex(APlaneIndex: Integer);
const OPNAME = 'TYRCGraphDataObject.SetPlaneIndex';
begin
  try
   if(APlaneIndex <> FPlaneIndex) and (APlaneIndex >= 0) and
     (APlaneIndex < PlanesCount)  then
   begin
     FPlaneIndex := APlaneIndex;
     SetChanged(True);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetPlaneIndexByYearNumber(AYearNumber: Integer): integer;
const OPNAME = 'TYRCGraphDataObject.GetPlaneIndexByYearNumber';
var
  LCount: integer;
begin
  Result := -1;
  try
    for LCount := 0 to PlanesCount-1 do
    begin
       if(YRCPlane[LCount].PlaneYears = AYearNumber) then
       begin
          Result := LCount;
          Break;
       end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetErrorMsg: string;
const OPNAME = 'TYRCGraphDataObject.GetErrorMsg';
begin
  Result := '';
  try
    Result := FErrorMsg;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetErrorMsg(AErrorMsg: string);
const OPNAME = 'TYRCGraphDataObject.SetErrorMsg';
begin
  try
    FErrorMsg :=  AErrorMsg;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetPlottingBase: integer;
const OPNAME = 'TYRCGraphDataObject.GetPlottingBase';
begin
  Result := -1;
  try
    Result := FPlottingBase;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetPlaneIndex: integer;
const OPNAME = 'TYRCGraphDataObject.GetPlaneIndex';
begin
  Result := -1;
  try
    Result := FPlaneIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetChartName(AChartName: string);
const OPNAME = 'TYRCGraphDataObject.SetChartName';
begin
  try
    FChartName := AChartName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetYRCChartProperties: TAbstractYRCChartProperties;
const OPNAME = 'TYRCGraphDataObject.GetYRCChartProperties';
begin
  Result := nil;
  try
    Result := FYRCChartProperties;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetYRCLanguageStrings: TAbstractYRCLanguageStrings;
const OPNAME = 'TYRCGraphDataObject.GetYRCLanguageStrings';
begin
  Result := nil;
  try
    Result := FYRCLanguageStrings;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetAssuranceIntervalSavedArray: TIntegerArray;
const OPNAME = 'TYRCGraphDataObject.GetAssuranceIntervalSavedArray';
begin
  Result := FAssuranceIntervalSavedArray;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetAssuranceIntervalDefaultArray: TIntegerArray;
const OPNAME = 'TYRCGraphDataObject.GetAssuranceIntervalDefaultArray';
begin
  Result := FAssuranceIntervalDefaultArray;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetAssuranceIntervalYearsArray: TIntegerArray;
const OPNAME = 'TYRCGraphDataObject.GetAssuranceIntervalYearsArray';
begin
  Result := FAssuranceIntervalYearsArray;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.UpdateSelectedAssuranceIntervalSaved: boolean;
const OPNAME = 'TYRCGraphDataObject.UpdateSelectedAssuranceIntervalSaved';
var
  LIndex: integer;
  LYRCPlane:TAbstractYRCPlane;
begin
  Result := False;
  try
    for LIndex := 0 to FPlanesList.Count -1 do
    begin
      LYRCPlane := YRCPlane[LIndex];
      if Assigned(LYRCPlane) then
      begin
        Result := LYRCPlane.UpdateSelectedAssuranceIntervalSaved(FAssuranceIntervalSavedArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.UpdateSelectedAssuranceIntervalDefault: boolean;
const OPNAME = 'TYRCGraphDataObject.UpdateSelectedAssuranceIntervalDefault';
var
  LIndex: integer;
  LYRCPlane:TAbstractYRCPlane;
begin
  Result := False;
  try
    for LIndex := 0 to FPlanesList.Count -1 do
    begin
      LYRCPlane := YRCPlane[LIndex];
      if Assigned(LYRCPlane) then
      begin
        Result := LYRCPlane.UpdateSelectedAssuranceIntervalDefault(FAssuranceIntervalDefaultArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.UpdateSelectedAssuranceIntervalYears: boolean;
const OPNAME = 'TYRCGraphDataObject.UpdateSelectedAssuranceIntervalYears';
var
  LIndex: integer;
  LYRCPlane:TAbstractYRCPlane;
begin
  Result := False;
  try
    for LIndex := 0 to FPlanesList.Count -1 do
    begin
      LYRCPlane := YRCPlane[LIndex];
      if Assigned(LYRCPlane) then
      begin
        Result := LYRCPlane.UpdateSelectedAssuranceIntervalYears(FAssuranceIntervalYearsArray);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.LoadSelectedAssuranceInterval;
const OPNAME = 'TYRCGraphDataObject.LoadSelectedAssuranceInterval';
  function GetValue(AValueString : string) : integer;
  const OPNAME = 'UYRCGraphDataObject.GetValue';
  var
    LPos : integer;
  begin
    Result := 0;
    try
      LPos   := Pos('=', AValueString);
      Result := StrToInt(Copy(AValueString, (LPos + 1), (Length(AValueString) - LPos)));
    except on E: Exception do HandleError(E, OPNAME) end;
  end;
var
  LStrings : TStringList;
  LIndex : integer;
begin
  try
    LStrings := TStringList.Create;
    try
      LStrings.Clear;
      FAppModules.ViewIni.ReadSectionValues(ClassName + 'Defaults', LStrings);
      SetLength(FAssuranceIntervalDefaultArray, LStrings.Count);
      for LIndex := 0 to LStrings.Count - 1 do
        FAssuranceIntervalDefaultArray[LIndex] := GetValue(LStrings[LIndex]);

      LStrings.Clear;
      FAppModules.ViewIni.ReadSectionValues(ClassName + 'Saved', LStrings);
      SetLength(FAssuranceIntervalSavedArray, LStrings.Count);
      for LIndex := 0 to LStrings.Count - 1 do
        FAssuranceIntervalSavedArray[LIndex] := GetValue(LStrings[LIndex]);

      LStrings.Clear;
      FAppModules.ViewIni.ReadSectionValues(ClassName + 'Years', LStrings);
      SetLength(FAssuranceIntervalYearsArray, LStrings.Count);
      for LIndex := 0 to LStrings.Count - 1 do
        FAssuranceIntervalYearsArray[LIndex] := GetValue(LStrings[LIndex]);
      //FAssuranceIntervalArray[0] := FAppModules.ViewIni.ReadInteger(ClassName, 'AssuranceInterval0',0);
      //FAssuranceIntervalArray[1] := FAppModules.ViewIni.ReadInteger(ClassName, 'AssuranceInterval1',0);
      //FAssuranceIntervalArray[2] := FAppModules.ViewIni.ReadInteger(ClassName, 'AssuranceInterval2',0);
      //FAssuranceIntervalArray[3] := FAppModules.ViewIni.ReadInteger(ClassName, 'AssuranceInterval3',0);
    finally
      LStrings.Clear;
      LStrings.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SaveSelectedAssuranceInterval;
const OPNAME = 'TYRCGraphDataObject.SaveSelectedAssuranceInterval';
var
  LIndex : integer;
  LStringList : TStringList;
begin
  try
    LStringList := TStringList.Create;
    try
      LStringList.Clear;
      FAppModules.ViewIni.ReadSection(ClassName + 'Saved', LStringList);
      for LIndex := 0 to LStringList.Count - 1 do
        FAppModules.ViewIni.DeleteKey(ClassName + 'Saved', 'AssuranceInterval' + IntToStr(LIndex));

      LStringList.Clear;
      FAppModules.ViewIni.ReadSection(ClassName + 'Years', LStringList);
      for LIndex := 0 to LStringList.Count - 1 do
        FAppModules.ViewIni.DeleteKey(ClassName + 'Years', 'AssuranceInterval' + IntToStr(LIndex));

      for LIndex := 0 to Length(FAssuranceIntervalSavedArray) - 1 do
        FAppModules.ViewIni.WriteInteger(ClassName + 'Saved',
                                        'AssuranceInterval' + IntToStr(LIndex),
                                         FAssuranceIntervalSavedArray[LIndex]);

      for LIndex := 0 to Length(FAssuranceIntervalYearsArray) - 1 do
        FAppModules.ViewIni.WriteInteger(ClassName + 'Years',
                                        'AssuranceInterval' + IntToStr(LIndex),
                                         FAssuranceIntervalYearsArray[LIndex]);
    finally
      LStringList.Clear;
      LStringList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetAssuranceIntervalSavedArray(AValue: TIntegerArray);
const OPNAME = 'TYRCGraphDataObject.SetAssuranceIntervalSavedArray';
var
  LIndex: integer;
begin
  try
    SetLength(FAssuranceIntervalSavedArray, Length(AValue));
    for LIndex := Low(AValue) to High(AValue) do
      FAssuranceIntervalSavedArray[LIndex] := AValue[LIndex];
    UpdateSelectedAssuranceIntervalSaved;
    SaveSelectedAssuranceInterval;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetAssuranceIntervalDefaultArray(AValue: TIntegerArray);
const OPNAME = 'TYRCGraphDataObject.SetAssuranceIntervalDefaultArray';
var
  LIndex: integer;
begin
  try
    SetLength(FAssuranceIntervalDefaultArray, Length(AValue));
    for LIndex := Low(AValue) to High(AValue) do
      FAssuranceIntervalDefaultArray[LIndex] := AValue[LIndex];
    UpdateSelectedAssuranceIntervalDefault;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetAssuranceIntervalYearsArray(AValue: TIntegerArray);
const OPNAME = 'TYRCGraphDataObject.SetAssuranceIntervalYearsArray';
var
  LIndex: integer;
begin
  try
    SetLength(FAssuranceIntervalYearsArray, Length(AValue));
    for LIndex := Low(AValue) to High(AValue) do
      FAssuranceIntervalYearsArray[LIndex] := AValue[LIndex];
    UpdateSelectedAssuranceIntervalYears;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetSelectedPlane: TAbstractYRCPlane;
const OPNAME = 'TYRCGraphDataObject.GetSelectedPlane';
begin
  Result := nil;
  try
    Result := GetPlane(FPlaneIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetSelectedTargetDraft: TAbstractYRCTargetDraft;
const OPNAME = 'TYRCGraphDataObject.GetSelectedTargetDraft';
begin
  Result := nil;
  try
    if(SelectedPlane <> nil) then
      Result := SelectedPlane.SelectedTargetDraft;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetSelectedTargetDraftIndex: integer;
const OPNAME = 'TYRCGraphDataObject.GetSelectedTargetDraftIndex';
begin
  Result := -1;
  try
    if(SelectedPlane <> nil) then
      Result := SelectedPlane.TargetDraftIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.SaveAssuranceInterval: boolean;
const OPNAME = 'TYRCGraphDataObject.SaveAssuranceInterval';
begin
  Result := False;
  try
    SaveSelectedAssuranceInterval;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.CalculateRIOnTargetDraft(APlaneIndex, ATargetDraftIndex: integer; AExcProb : double): double;
const OPNAME = 'TYRCGraphDataObject.CalculateRIOnTargetDraft';
var
  LTargetDraft : TAbstractYRCTargetDraft;
  LPlane       : TAbstractYRCPlane;
  LYears       : integer;
begin
  Result := NullFloat;
  try
    LPlane := GetPlane(APlaneIndex);
    if Assigned(LPlane) then
    begin
      LTargetDraft := LPlane.TargetDraft[ATargetDraftIndex];
      if Assigned(LTargetDraft) then
      begin
        LYears := LTargetDraft.TargetDraftYears;
        Result := 1 / (1 - Power((AExcProb / 100), (1/LYears)));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetPeriodLength: integer;
const OPNAME = 'TYRCGraphDataObject.GetPeriodLength';
var
  LPlane: TAbstractYRCPlane;
begin
  Result := -1;
  try
    LPlane := GetPlane(FPlaneIndex);
    if(LPlane <> nil)  then
      Result := LPlane.PlaneYears;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.SetPeriodLength(AYearNumber: integer);
const OPNAME = 'TYRCGraphDataObject.SetPeriodLength';
var
  LIndex: integer;
begin
  try
    LIndex := GetPlaneIndexByYearNumber(AYearNumber);
    if(LIndex >= 0) and (LIndex < PlanesCount) then
      PlaneIndex := LIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.MaxYearNumber: integer;
const OPNAME = 'TYRCGraphDataObject.MaxYearNumber';
var
  LIndex: integer;
  LPlane: TAbstractYRCPlane;
begin
  Result := 0;
  try
    for LIndex := 0 to FPlanesList.Count-1 do
    begin
      LPlane := GetPlane(LIndex);
      if(LPlane <> nil)  then
      begin
        if(LPlane.PlaneYears > Result) then
          Result := LPlane.PlaneYears;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.MinYearNumber: integer;
const OPNAME = 'TYRCGraphDataObject.MinYearNumber';
var
  LIndex: integer;
  LPlane: TAbstractYRCPlane;
begin
  Result := 0;
  try
    for LIndex := 0 to FPlanesList.Count-1 do
    begin
      LPlane := GetPlane(LIndex);
      if(LPlane <> nil)  then
      begin
        if(Result = 0) then
          Result := LPlane.PlaneYears
        else
          if(LPlane.PlaneYears < Result) then
          Result := LPlane.PlaneYears;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCGraphDataObject.ResetSelectedTargetDraftPoints;
const OPNAME = 'TYRCGraphDataObject.ResetSelectedTargetDraftPoints';
var
  LTargetDraft: TAbstractYRCTargetDraft;
begin
  try
    LTargetDraft := SelectedTargetDraft;
    if(LTargetDraft <> nil) then
    begin
      Case LTargetDraft.TargetDraftSavedMode of
        tdmDeterministic:
        begin
          LTargetDraft.ResetDeterministicPoints;
          LTargetDraft.ResetRegressionPoints;
        end;
        tdmRegression:
        begin
          LTargetDraft.ResetRegressionPoints;
          LTargetDraft.ResetDeterministicPoints;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.GetCastPlane(AIndex: Integer): TYRCPlane;
const OPNAME = 'TYRCGraphDataObject.GetCastPlane';
var
  LYRCPlane: TAbstractYRCPlane;
begin
  Result := nil;
  try
    LYRCPlane := GetPlane(AIndex);
    if(LYRCPlane <> nil) then
      Result := TYRCPlane(LYRCPlane);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.TargetDraftValuesCommaText: string;
const OPNAME = 'TYRCGraphDataObject.TargetDraftValuesCommaText';
var
  LIndex: integer;
  LYRCPlane: TYRCPlane;
  LValues: TStringList;
begin
  Result := '';
  try
    LYRCPlane := GetCastPlane(0);
    if(LYRCPlane <> nil) then
    begin
      LValues := TStringList.Create;
      try
        for LIndex := 0 to LYRCPlane.TargetDraftCount - 1 do
          LValues.Add(SmartFloatFormat(LYRCPlane.TargetDraft[LIndex].TargetDraftYValue,6,2));
        Result := LValues.CommaText;
      finally
        LValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.DeleteChartData(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.DeleteChartData';
begin
  Result := False;
  try
    if SavedToDB then
    begin
      if DeleteChartDatabaseData(APrimaryKeyProperty) then
      begin
        Self.Reset;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCGraphDataObject.DeleteChartDatabaseData( const APrimaryKeyProperty: array of string): boolean;
const OPNAME = 'TYRCGraphDataObject.DeleteChartDatabaseData';
var
 LCount: integer;
 LTableName,
 LDatabaseName,
 LWhereClause: string;
 LKeyPropertyChart: array[0..5] of string;

begin
  Result := False;
  try
    //if SavedToDB then
    //begin
      LDatabaseName  := FAppModules.Database.DatabaseName;
      LWhereClause := '';

      for LCount := Low(APrimaryKeyProperty) to high(APrimaryKeyProperty) do
        LKeyPropertyChart[LCount] := APrimaryKeyProperty[LCount];
      LKeyPropertyChart[5] := IntToStr(Self.ChartNumber);

      Result := GetWhereClause(LKeyPropertyChart,'',LDatabaseName,LWhereClause);
      if Result then
      begin
        LTableName := 'yrcChart';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcChartProperty';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcLanguageStrings';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcPlane';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcTargetDraft';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcTargetDraftConst';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);

        LTableName := 'yrcTargetDraftPoint';
        ClearTableData(LDatabaseName,LTableName,LWhereClause);
        Result := True;
      end;
    //end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCoefficient }

procedure TCoefficient.Reset;
const OPNAME = 'TCoefficient.Reset';
begin
  try
    FTargetDraft  := NullFloat;
    FACoefficient := NullFloat;
    FBCoefficient := NullFloat;
    FCCoefficient := NullFloat;
    FDCoefficient := NullFloat;
    FXBreakPoint  := NullFloat;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCoefficientList }

procedure TCoefficientList.AfterConstruction;
const OPNAME = 'TCoefficientList.AfterConstruction';
begin
  inherited;
  try
    FCoefficientList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCoefficientList.BeforeDestruction;
const OPNAME = 'TCoefficientList.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FCoefficientList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficientList.CoefficientCount: integer;
const OPNAME = 'TCoefficientList.CoefficientCount';
begin
  Result := 0;
  try
    Result := FCoefficientList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficientList.Get_CoefficientByIndex(AIndex: integer): TCoefficient;
const OPNAME = 'TCoefficientList.Get_CoefficientByIndex';
begin
  Result := Nil;
  try
    if(AIndex >= 0) and (AIndex < FCoefficientList.Count) then
      Result := TCoefficient(FCoefficientList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCoefficientList.NewCoefficient: TCoefficient;
const OPNAME = 'TCoefficientList.NewCoefficient';
var
  LCoefficient: TCoefficient;
begin
  Result := Nil;
  try
    LCoefficient := TCoefficient.Create;
    FCoefficientList.Add(LCoefficient);
    Result := LCoefficient;
    Result.Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCoefficientList.Reset;
const OPNAME = 'TCoefficientList.Reset';
begin
  try
    FCoefficientList.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

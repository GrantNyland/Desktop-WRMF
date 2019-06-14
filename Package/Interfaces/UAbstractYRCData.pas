//
//
//  UNIT      : Contains TAbstractYRCObject Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 27/08/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAbstractYRCData;

interface

uses
  Classes,
  SysUtils,
  Contnrs,

  //  DWAF VCL
  UAbstractObject,
  UAbstractDataObject,
  UAbstractModelData;

type
  TYRCRecordPointArraySortOrder = (soAscending, soDescending);

  TAbstractYRCObject = class(TAbstractAppDataObject)
  protected
    function GetSavedToDB: boolean; virtual; abstract;
    procedure SetSavedToDB(ASavedToDB: boolean); virtual; abstract;
    function GetLoaded: boolean; virtual; abstract;
    procedure SetLoaded(ALoaded: boolean); virtual; abstract;
    function GetLoadedFromDB: boolean; virtual; abstract;
    procedure SetLoadedFromDB(ALoadedFromDB: boolean); virtual; abstract;
    function GetChartNumber: integer; virtual; abstract;
    procedure SetChartNumber(AChartNumber: integer); virtual; abstract;
    function GetChanged: boolean; virtual; abstract;
    procedure SetChanged(AChanged: boolean); virtual; abstract;

    function GetWhereClause(Const APrimaryKeyProperty : array of string; ATableAlias: string; var ADatabaseName,AWhereClause: string): boolean; virtual; abstract;
    function ClearTableData(ADatabaseName,ATableName,AWhereClause: string): boolean;virtual; abstract;

    function CopyValuesFrom(ASource:TObject): boolean; virtual; abstract;
    function LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;virtual; abstract;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean;  virtual; abstract;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean;  virtual; abstract;
  public
    property SavedToDB     : boolean  read GetSavedToDB     write SetSavedToDB;
    property Loaded        : boolean  read GetLoaded        write SetLoaded;
    property LoadedFromDB  : boolean  read GetLoadedFromDB  write SetLoadedFromDB;
    property ChartNumber   : integer  read GetChartNumber   write SetChartNumber;
    property Changed       : boolean  read GetChanged       write SetChanged;
  end;

  TYRCObject = class(TAbstractYRCObject)
  protected
    FChanged  : boolean;
    FSavedToDB: boolean;
    FLoaded   : boolean;
    FLoadedFromDB: boolean;
    FChartNumber : integer;

    function  GetSavedToDB: boolean; override;
    procedure SetSavedToDB(ASavedToDB: boolean); override;
    function  GetLoaded: boolean; override;
    procedure SetLoaded(ALoaded: boolean);override;
    function  GetLoadedFromDB: boolean; override;
    procedure SetLoadedFromDB(ALoadedFromDB: boolean); override;
    function  GetChartNumber: integer; override;
    procedure SetChartNumber(AChartNumber: integer);override;
    function GetChanged: boolean; override;
    procedure SetChanged(AChanged: boolean); override;

    function GetTargetDraftSavedMode(ATypeConstant: integer):TChartEditMode;
    function GetCurveGroupType(ATypeConstant: string):TCurveGroupType;

    function GetWhereClause(Const APrimaryKeyProperty : array of string; ATableAlias: string; var ADatabaseName,AWhereClause: string): boolean; override;
    function ClearTableData(ADatabaseName,ATableName,AWhereClause: string): boolean; override;

    function LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;override;
    function LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean; override;
    function CopyValuesFrom(ASource:TObject): boolean; override;
  public
    procedure Reset; override;
  end;

  TAbstractYRCLanguageStrings = class(TYRCObject)
  protected
    function GetCaptionFormatStr1: string; virtual; abstract;
    procedure SetCaptionFormatStr1(ACaptionFormatStr1: string); virtual; abstract;
    function GetCaptionFormatStr2: string; virtual; abstract;
    procedure SetCaptionFormatStr2(ACaptionFormatStr2: string); virtual; abstract;
    function GetYearsFormatStr: string; virtual; abstract;
    procedure SetYearsFormatStr(AYearsFormatStr: string); virtual; abstract;
    function GetYieldFormatStr: string; virtual; abstract;
    procedure SetYieldFormatStr(AYieldFormatStr: string); virtual; abstract;
    function GetLegendCaption: string; virtual; abstract;
    procedure SetLegendCaption(ALegendCaption: string); virtual; abstract;
    function GetBottomAxisCaption: string; virtual; abstract;
    procedure SetBottomAxisCaption(ABottomAxisCaption: string); virtual; abstract;
    function GetLeftAxisCaption: string; virtual; abstract;
    procedure SetLeftAxisCaption(ALeftAxisCaption: string); virtual; abstract;
  public
    property CaptionFormatStr1   : string        read GetCaptionFormatStr1   write SetCaptionFormatStr1;
    property CaptionFormatStr2   : string        read GetCaptionFormatStr2   write SetCaptionFormatStr2;
    property YearsFormatStr      : string        read GetYearsFormatStr      write SetYearsFormatStr;
    property YieldFormatStr      : string        read GetYieldFormatStr      write SetYieldFormatStr;
    property LegendCaption       : string        read GetLegendCaption       write SetLegendCaption;
    property BottomAxisCaption   : string        read GetBottomAxisCaption   write SetBottomAxisCaption;
    property LeftAxisCaption     : string        read GetLeftAxisCaption     write SetLeftAxisCaption;
  end;

  TAbstractYRCChartProperties  = class(TYRCObject)
  protected
    function GetMarginBottom: integer; virtual; abstract;
    procedure SetMarginBottom(AMarginBottom: integer); virtual; abstract;
    function GetMarginLeft: integer; virtual; abstract;
    procedure SetMarginLeft(AMarginLeft: integer); virtual; abstract;
    function GetMarginRight: integer; virtual; abstract;
    procedure SetMarginRight(AMarginRight: integer); virtual; abstract;
    function GetMarginTop: integer; virtual; abstract;
    procedure SetMarginTop(AMarginTop: integer); virtual; abstract;
    function GetBottomAxisMinimum:double ; virtual; abstract;
    procedure SetBottomAxisMinimum(AValue: double); virtual; abstract;
    function GetBottomAxisIncrement: double; virtual; abstract;
    procedure SetBottomAxisIncrement(ABottomAxisIncrement: double); virtual; abstract;
    function GetBottomAxisMaximum: double; virtual; abstract;
    procedure SetBottomAxisMaximum(ABottomAxisMaximum: double); virtual; abstract;
    function GetBottomAxisMinorTickCount: integer; virtual; abstract;
    procedure SetBottomAxisMinorTickCount(ABottomAxisMinorTickCount: integer); virtual; abstract;
    function GetLeftAxisMinimum:double ; virtual; abstract;
    procedure SetLeftAxisMinimum(AValue: double); virtual; abstract;
    function GetLeftAxisIncrement: double; virtual; abstract;
    procedure SetLeftAxisIncrement(ALeftAxisIncrement: double); virtual; abstract;
    function GetLeftAxisMaximum: double; virtual; abstract;
    procedure SetLeftAxisMaximum(ALeftAxisMaximum: double); virtual; abstract;
    function GetLeftAxisMinorTickCount: integer; virtual; abstract;
    procedure SetLeftAxisMinorTickCount(ALeftAxisMinorTickCount: integer); virtual; abstract;
    function GetRightAxisMinimum:double ; virtual; abstract;
    procedure SetRightAxisMinimum(AValue: double); virtual; abstract;
    function GetRightAxisIncrement: double; virtual; abstract;
    procedure SetRightAxisIncrement(ARightAxisIncrement: double); virtual; abstract;
    function GetRightAxisMaximum: double; virtual; abstract;
    procedure SetRightAxisMaximum(ARightAxisMaximum: double); virtual; abstract;
    function GetRightAxisMinorTickCount: integer; virtual; abstract;
    procedure SetRightAxisMinorTickCount(ARightAxisMinorTickCount: integer); virtual; abstract;
    function GetLegendTopPos: integer; virtual; abstract;
    procedure SetLegendTopPos(ALegendTopPos: integer); virtual; abstract;
    function GetLegendVertMargin: integer; virtual; abstract;
    procedure SetLegendVertMargin(ALegendVertMargin: integer); virtual; abstract;

    function GetZoomIndex:integer ; virtual; abstract;
    procedure SetZoomIndex(AValue: integer); virtual; abstract;
    function GetZoomValue:double ; virtual; abstract;
    procedure SetZoomValue(AValue: double); virtual; abstract;
    function GetShowTargetDrafts:TShowTargetDrafts ; virtual; abstract;
    procedure SetShowTargetDrafts(AValue: TShowTargetDrafts); virtual; abstract;
    function GetChartMode:TChartMode ; virtual; abstract;
    procedure SetChartMode(AValue: TChartMode); virtual; abstract;
    function GetChartEditMode:TChartEditMode ; virtual; abstract;
    procedure SetChartEditMode(AValue: TChartEditMode); virtual; abstract;
    function GetHideRawpoints:boolean ; virtual; abstract;
    procedure SetHideRawpoints(AValue: boolean); virtual; abstract;
    function GetHideFittedPoints:boolean ; virtual; abstract;
    procedure SetHideFittedPoints(AValue: boolean); virtual; abstract;
    function GetHideRawLines:boolean ; virtual; abstract;
    procedure SetHideRawLines(AValue: boolean); virtual; abstract;
    function GetShowFirmYieldLabels:boolean ; virtual; abstract;
    procedure SetShowFirmYieldLabels(AValue: boolean); virtual; abstract;
    function GetShowCursorPosition:boolean ; virtual; abstract;
    procedure SetShowCursorPosition(AValue: boolean); virtual; abstract;
    function GetMaxYValue:double ; virtual; abstract;
    procedure SetMaxYValue(AValue: double); virtual; abstract;
  public
    property  MarginBottom              : integer read GetMarginBottom             write SetMarginBottom;
    property  MarginLeft                : integer read GetMarginLeft               write SetMarginLeft;
    property  MarginRight               : integer read GetMarginRight              write SetMarginRight;
    property  MarginTop                 : integer read GetMarginTop                write SetMarginTop;
    property  BottomAxisMinimum         : double  read GetBottomAxisMinimum        write SetBottomAxisMinimum;
    property  BottomAxisIncrement       : double  read GetBottomAxisIncrement      write SetBottomAxisIncrement;
    property  BottomAxisMaximum         : double  read GetBottomAxisMaximum        write SetBottomAxisMaximum;
    property  BottomAxisMinorTickCount  : integer read GetBottomAxisMinorTickCount write SetBottomAxisMinorTickCount;
    property  LeftAxisMinimum           : double  read GetLeftAxisMinimum          write SetLeftAxisMinimum;
    property  LeftAxisIncrement         : double  read GetLeftAxisIncrement        write SetLeftAxisIncrement;
    property  LeftAxisMaximum           : double  read GetLeftAxisMaximum          write SetLeftAxisMaximum;
    property  LeftAxisMinorTickCount    : integer read GetLeftAxisMinorTickCount   write SetLeftAxisMinorTickCount;
    property  RightAxisMinimum          : double  read GetRightAxisMinimum         write SetRightAxisMinimum;
    property  RightAxisIncrement        : double  read GetRightAxisIncrement       write SetRightAxisIncrement;
    property  RightAxisMaximum          : double  read GetRightAxisMaximum         write SetRightAxisMaximum;
    property  RightAxisMinorTickCount   : integer read GetRightAxisMinorTickCount  write SetRightAxisMinorTickCount;
    property  LegendTopPos              : integer read GetLegendTopPos             write SetLegendTopPos;
    property  LegendVertMargin          : integer read GetLegendVertMargin         write SetLegendVertMargin;

    property ZoomIndex                  : integer           read GetZoomIndex           write SetZoomIndex;
    property ZoomValue                  : double            read GetZoomValue           write SetZoomValue;

    property ShowTargetDrafts           : TShowTargetDrafts read GetShowTargetDrafts    write SetShowTargetDrafts;
    property ChartMode                  : TChartMode        read GetChartMode           write SetChartMode;
    property ChartEditMode              : TChartEditMode    read GetChartEditMode       write SetChartEditMode;

    property HideRawPoints              : boolean           read GetHideRawpoints       write SetHideRawpoints;
    property HideFittedPoints           : boolean           read GetHideFittedPoints    write SetHideFittedPoints;
    property HideRawLines               : boolean           read GetHideRawLines        write SetHideRawLines;
    property ShowFirmYieldLabels        : boolean           read GetShowFirmYieldLabels write SetShowFirmYieldLabels;
    property ShowCursorPosition         : boolean           read GetShowCursorPosition  write SetShowCursorPosition;
    property MaxYValue                  : double            read GetMaxYValue           write SetMaxYValue;
  end;

  TAbstractYRCFunctionConstants = class(TYRCObject)
  protected
    function GetConstantsArray: TYRCConstantsArray; virtual; abstract;
  public
    function Valid: boolean; virtual; abstract;
    property ConstantsArray : TYRCConstantsArray read GetConstantsArray ;
  end;

  TAbstractYRCRecordPointArrayObject = class(TYRCObject)
  protected
    function GetYRCRecordPointArray: TYRCRecordPointArray; virtual; abstract;
    function GetSorted: Boolean; virtual; abstract;
  public
    procedure Sort(ASortOrder: TYRCRecordPointArraySortOrder); virtual; abstract;
    function AddPoints(APointsArray: TYRCRecordPointArray): boolean;virtual; abstract;
    function DeletePoint(AIndex: integer): boolean; virtual; abstract;
    function UpdatePoint(AIndex: integer;AXValue,AXTValue,YValue: double): boolean; virtual; abstract;
    function ZeroYCount: integer; virtual; abstract;

    property YRCRecordPointArray  : TYRCRecordPointArray  read GetYRCRecordPointArray;
    property Sorted  : Boolean  read GetSorted;
  end;

  TLabelProperties = class(TObject)
  protected
    FText     : string;
    FLeftTopX : integer;
    FLeftTopY : integer;
    FArrowToX : integer;
    FArrowToY : integer;
    FCustom   : boolean;
  public
    procedure Reset;
    procedure Populate(AText: string;ALeftTopX,ALeftTopY,AArrowToX,AArrowToY: integer;ACustom   : boolean);
    property Text     : string  read   FText      write  FText;
    property LeftTopX : integer read   FLeftTopX  write  FLeftTopX;
    property LeftTopY : integer read   FLeftTopY  write  FLeftTopY;
    property ArrowToX : integer read   FArrowToX  write  FArrowToX;
    property ArrowToY : integer read   FArrowToY  write  FArrowToY;
    property Custom   : boolean read   FCustom    write  FCustom;
  end;

  TAbstractYRCTargetDraft = class(TYRCObject)
  private
  protected
    function GetTargetDraftSavedMode: TChartEditMode; virtual; abstract;
    procedure SetChartEditMode(ATargetDraftMode: TChartEditMode); virtual; abstract;
    function GetPlaneID: integer; virtual; abstract;
    procedure SetPlaneID(APlaneID: integer); virtual; abstract;
    function GetTargetDraftID: integer; virtual; abstract;
    procedure SetTargetDraftID(ATargetDraftID: integer); virtual; abstract;
    function GetTargetDraftXValue: Double; virtual; abstract;
    procedure SetTargetDraftXValue(ATargetDraftXValue: Double); virtual; abstract;
    function GetTargetDraftXTValue: Double; virtual; abstract;
    procedure SetTargetDraftXTValue(ATargetDraftXTValue: Double); virtual; abstract;
    function GetTargetDraftYValue: Double; virtual; abstract;
    procedure SetTargetDraftYValue(ATargetDraftYValue: Double); virtual; abstract;
    function GetTargetDraftRecurance: Double; virtual; abstract;
    procedure SetTargetDraftRecurance(ATargetDraftRecurance: Double); virtual; abstract;
    function GetTargetDraftYears: integer; virtual; abstract;
    procedure SetTargetDraftYears(ATargetDraftYears: integer); virtual; abstract;
    function GetYValueAt100: double; virtual; abstract;
    procedure SetYValueAt100(AValue: double); virtual; abstract;
    function GetFormula: string; virtual; abstract;
    function GetForceCurveThrough100: boolean; virtual; abstract;
    procedure SetForceCurveThrough100(AForce:boolean); virtual; abstract;

    function GetOriginalPointsArray        : TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetRegressionPointsArray      : TAbstractYRCRecordPointArrayObject;virtual; abstract;
    function GetPureRegressionPointsArray  : TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetDeterministicPointsArray   : TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetPureRegressionConstants    : TAbstractYRCFunctionConstants; virtual; abstract;
    function GetRegressionConstants        : TAbstractYRCFunctionConstants; virtual; abstract;
    function GetDeterministicConstants     : TAbstractYRCFunctionConstants; virtual; abstract;
    function GetLabelProperties            : TLabelProperties; virtual; abstract;
  public
    procedure ResetPureRegressionPoints; virtual; abstract;
    procedure ResetRegressionPoints; virtual; abstract;
    procedure ResetDeterministicPoints; virtual; abstract;

    function AddRegressionPoints(APointsArray: TYRCRecordPointArray): boolean; virtual; abstract;
    function DeleteRegressionPoint(AIndex: integer): boolean; virtual; abstract;
    function UpdateRegressionPoint(AIndex: integer;AXValue,AXTValue,AYValue: double): boolean; virtual; abstract;
    function UpdateDeterministicPoint(AIndex: integer;AXValue,AXTValue,AYValue: double): boolean; virtual; abstract;

    function CurveFitted: boolean; virtual; abstract;
    function YValueAt100Added: boolean; virtual; abstract;
    function RegressionPointsAdded: boolean; virtual; abstract;
    function DeterministicPointsChanged: boolean; virtual; abstract;
    function TransformCurvePoints(AOriginalPointsArray : TAbstractYRCRecordPointArrayObject) : boolean; virtual; abstract;
    procedure ApplyPlottingBase(APlottingBase: integer); virtual; abstract;

    property PlaneID              : integer   read GetPlaneID              write SetPlaneID;
    property TargetDraftID        : integer   read GetTargetDraftID        write SetTargetDraftID;
    property TargetDraftXValue    : Double    read GetTargetDraftXValue    write SetTargetDraftXValue;
    property TargetDraftXTValue   : Double    read GetTargetDraftXTValue   write SetTargetDraftXTValue;
    property TargetDraftYValue    : Double    read GetTargetDraftYValue    write SetTargetDraftYValue;
    property TargetDraftRecurance : Double    read GetTargetDraftRecurance write SetTargetDraftRecurance;
    property TargetDraftYears     : integer   read GetTargetDraftYears     write SetTargetDraftYears;
    property YValueAt100          : Double    read GetYValueAt100          write SetYValueAt100;
    property ForceCurveThrough100 : boolean   read GetForceCurveThrough100 write SetForceCurveThrough100;
    property Formula              : string    read GetFormula;
    property TargetDraftSavedMode : TChartEditMode   read GetTargetDraftSavedMode  write SeTChartEditMode;

    property OriginalPointsArrayObject    : TAbstractYRCRecordPointArrayObject  read GetOriginalPointsArray;
    property PureRegressionPointsArray    : TAbstractYRCRecordPointArrayObject  read GetPureRegressionPointsArray;
    property RegressionPointsArray        : TAbstractYRCRecordPointArrayObject  read GetRegressionPointsArray;
    property DeterministicPointsArray     : TAbstractYRCRecordPointArrayObject  read GetDeterministicPointsArray;

    property PureRegressionConstants      : TAbstractYRCFunctionConstants       read GetPureRegressionConstants;
    property RegressionConstants          : TAbstractYRCFunctionConstants       read GetRegressionConstants;
    property DeterministicConstants       : TAbstractYRCFunctionConstants       read GetDeterministicConstants;
    property LabelProperties              : TLabelProperties                    read GetLabelProperties;
  end;

  TAbstractYRCPlane = class(TYRCObject)
  protected
    function GetPlaneID: integer; virtual; abstract;
    procedure SetPlaneID(APlaneID: integer); virtual; abstract;
    function GetPlaneYears: integer; virtual; abstract;
    function GetTargetDraft(AIndex: Integer): TAbstractYRCTargetDraft;virtual; abstract;
    function GetSelectedTargetDraft:TAbstractYRCTargetDraft; virtual; abstract;
    function GetYXPointArrayObject: TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetYearsArrayObject: TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetAssurancePointArrayObject: TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetAssuranceYearsArrayObject: TAbstractYRCRecordPointArrayObject; virtual; abstract;
    function GetTargetDraftIndex: integer; virtual; abstract;
    procedure SetTargetDraftIndex(ATargetDraftIndex: integer); virtual; abstract;
    function AddTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): Integer; virtual; abstract;
    function DeleteTargetDraft(AYRCTargetDraft: TAbstractYRCTargetDraft): boolean; virtual; abstract;
  public
    procedure ApplyPlottingBase(APlottingBase: integer); virtual; abstract;
    function TargetDraftCount: integer; virtual; abstract;
    function LoadCoefficientFile: boolean; virtual; abstract;
    function UpdateSelectedAssuranceIntervalSaved(AAssuranceIntervalArraySaved:TIntegerArray):boolean; virtual; abstract;
    function UpdateSelectedAssuranceIntervalDefault(AAssuranceIntervalArrayDefault:TIntegerArray):boolean; virtual; abstract;
    function UpdateSelectedAssuranceIntervalYears(AAssuranceIntervalArrayYears:TIntegerArray):boolean; virtual; abstract;

    property PlaneID : integer   read GetPlaneID   write SetPlaneID;
    property PlaneYears : integer   read GetPlaneYears;
    property TargetDraft[AIndex: Integer]: TAbstractYRCTargetDraft read GetTargetDraft;
    property TargetDraftIndex: integer  read GetTargetDraftIndex write SetTargetDraftIndex;
    property SelectedTargetDraft : TAbstractYRCTargetDraft read GetSelectedTargetDraft;
    property YXPointArrayObject    : TAbstractYRCRecordPointArrayObject  read GetYXPointArrayObject;
    property YearsArrayObject    : TAbstractYRCRecordPointArrayObject  read GetYearsArrayObject;
    property AssurancePointArrayObject    : TAbstractYRCRecordPointArrayObject  read GetAssurancePointArrayObject;
    property AssuranceYearsArrayObject    : TAbstractYRCRecordPointArrayObject  read GetAssuranceYearsArrayObject;
  end;

  TAbstractYRCGraphDataObject = class(TYRCObject)
  protected
    function GetPlaneIndex: integer; virtual; abstract;
    procedure SetPlaneIndex(APlaneIndex: Integer); virtual; abstract;
    function GetPlottingBase: integer; virtual; abstract;
    procedure SetPlottingBase(APlottingBase: Integer); virtual; abstract;
    function GetErrorMsg: string; virtual; abstract;
    procedure SetErrorMsg(AErrorMsg: string); virtual; abstract;
    function GetChartName: boolean; virtual; abstract;
    procedure SetChartName(AChartName: string); virtual; abstract;
    function GetYRCLanguageStrings: TAbstractYRCLanguageStrings; virtual; abstract;
    function GetYRCChartProperties: TAbstractYRCChartProperties; virtual; abstract;

    function GetPlanesCount: integer; virtual; abstract;
    function GetYearsCount: integer; virtual; abstract;
    function GetSequencesCount: integer; virtual; abstract;
    function GetPeriodLength: integer; virtual; abstract;
    procedure SetPeriodLength(APeriodLength: integer); virtual; abstract;

    function GetPlane(AIndex: Integer): TAbstractYRCPlane; virtual; abstract;
    function GetTargetDraft(APlaneIndex,ATargetDraftIndex: Integer): TAbstractYRCTargetDraft; virtual; abstract;
    function GetTargetDraftSavedMode(APlaneIndex,ATargetDraftIndex: Integer): TChartEditMode; virtual; abstract;
    function GetSelectedPlane: TAbstractYRCPlane; virtual; abstract;
    function GetSelectedTargetDraft: TAbstractYRCTargetDraft; virtual; abstract;
    function GetSelectedTargetDraftIndex: integer; virtual; abstract;

    function GetAssuranceIntervalSavedArray: TIntegerArray; virtual; abstract;
    procedure SetAssuranceIntervalSavedArray(AValue: TIntegerArray); virtual; abstract;
    function GetAssuranceIntervalDefaultArray: TIntegerArray; virtual; abstract;
    procedure SetAssuranceIntervalDefaultArray(AValue: TIntegerArray); virtual; abstract;
    function GetAssuranceIntervalYearsArray: TIntegerArray; virtual; abstract;
    procedure SetAssuranceIntervalYearsArray(AValue: TIntegerArray); virtual; abstract;

    function UpdateSelectedAssuranceIntervalSaved:boolean; virtual; abstract;
    function UpdateSelectedAssuranceIntervalDefault:boolean; virtual; abstract;
    function UpdateSelectedAssuranceIntervalYears:boolean; virtual; abstract;
    function AddPlane(AYRCPlane: TAbstractYRCPlane): Integer;  virtual; abstract;
  public
    function DeleteTargetDraft(ATargetDraftIndex: Integer): boolean; virtual; abstract;
    function SaveAssuranceInterval : boolean; virtual; abstract;
    function CalculateRIOnTargetDraft(APlaneIndex, ATargetDraftIndex : integer; AExcProb : double) : double; virtual; abstract;
    function MinYearNumber: integer;virtual; abstract;
    function MaxYearNumber: integer;virtual; abstract;
    function TargetDraftValuesCommaText: string;virtual; abstract;
    function MergeData(ANewData:TAbstractYRCGraphDataObject; ADeleteIndexCommaText,AAddIndexCommaText: string): boolean; virtual; abstract;
    function DeleteChartData(Const APrimaryKeyProperty : array of string): boolean; virtual; abstract;
    procedure ResetSelectedTargetDraftPoints; virtual; abstract;

    property PlottingBase     : integer read GetPlottingBase     write SetPlottingBase;
    property PeriodLength     : integer read GetPeriodLength     write SetPeriodLength;
    property PlaneIndex       : integer read GetPlaneIndex       write SetPlaneIndex;
    property ErrorMsg         : string  read GetErrorMsg         write SetErrorMsg;
    property PlanesCount      : integer read GetPlanesCount;
    property YearsCount       : integer read GetYearsCount;
    property SequencesCount   : integer read GetSequencesCount;
    property ChartName        : boolean read GetChartName;

    property YRCPlane[AIndex: Integer]: TAbstractYRCPlane read GetPlane;
    property YRCTargetDraft[APlaneIndex,ATargetDraftIndex: Integer]: TAbstractYRCTargetDraft read GetTargetDraft;
    property YRCTargetDraftSavedMode[APlaneIndex,ATargetDraftIndex: Integer]: TChartEditMode read GetTargetDraftSavedMode;
    property SelectedPlane: TAbstractYRCPlane read GetSelectedPlane;
    property SelectedTargetDraft: TAbstractYRCTargetDraft read GetSelectedTargetDraft;
    property SelectedTargetDraftIndex: integer read GetSelectedTargetDraftIndex;

    property YRCLanguageStrings: TAbstractYRCLanguageStrings read GetYRCLanguageStrings;
    property YRCChartProperties: TAbstractYRCChartProperties read GetYRCChartProperties;

    property SelectedAssuranceIntervalDefaultArray : TIntegerArray read GetAssuranceIntervalDefaultArray write SetAssuranceIntervalDefaultArray;
    property SelectedAssuranceIntervalYearsArray   : TIntegerArray read GetAssuranceIntervalYearsArray   write SetAssuranceIntervalYearsArray;
    property SelectedAssuranceIntervalsavedArray   : TIntegerArray read GetAssuranceIntervalSavedArray   write SetAssuranceIntervalSavedArray;
  end;

implementation

uses
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TYRCObject }

procedure TYRCObject.Reset;
const OPNAME = 'TYRCObject.Reset';
begin
  try
    FChanged     := False;
    FSavedToDB   := False;
    FLoaded      := False;
    FLoadedFromDB:= False;
    FChartNumber := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.CopyValuesFrom(ASource: TObject): boolean;
const OPNAME = 'TYRCObject.CopyValuesFrom';
//var
//  LYRCObject : TYRCObject;
begin
  Result := False;
  try
    if Assigned(ASource) and (ASource is TYRCObject) then
    begin
      Reset;
      //LYRCObject    := TYRCObject(ASource);
      FChanged      := True;
      //FSavedToDB    := LYRCObject.SavedToDB;
      FLoaded       := True;
      //FLoadedFromDB := LYRCObject.LoadedFromDB;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.LoadDataFromDB(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCObject.LoadDataFromDB';
begin
  Result := False;
  try
    FChanged     := False;
    FSavedToDB   := True;
    FLoaded      := True;
    FLoadedFromDB:= True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.LoadDataFromFile(AFileBlocks: TObjectList; Const AIndexProperty : array of integer): boolean;
const OPNAME = 'TYRCObject.LoadDataFromFile';
begin
  Result := False;
  try
    if Assigned(AFileBlocks) then
    begin
      FChanged     := True;
      FSavedToDB   := False;
      FLoaded      := True;
      FLoadedFromDB:= False;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.SaveDataToDB(Const APrimaryKeyProperty : array of string): boolean;
const OPNAME = 'TYRCObject.SaveDataToDB';
begin
  Result := False;
  try
    FChanged     := False;
    FSavedToDB   := True;
    FLoaded      := True;
    FLoadedFromDB:= True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.ClearTableData(ADatabaseName, ATableName,AWhereClause: string): boolean;
const OPNAME = 'TYRCObject.ClearTableData';
var
  LDataSet : TAbstractModelDataSet;
begin
  Result := False;
  try
    if (ADatabaseName <> '') and (ATableName <> '') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL('DELETE FROM ' + ATableName + ' WHERE '+ AWhereClause);
        LDataSet.ExecSQL;
        Result := True;
      finally
        LDataSet.DataSet.Close;
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TYRCObject.GetWhereClause( const APrimaryKeyProperty: array of string; ATableAlias: string;
         var ADatabaseName, AWhereClause: string): boolean;
const OPNAME = 'TYRCObject.GetWhereClause';
begin
  Result := False;
  try
    ADatabaseName := '';
    AWhereClause  := '';
    if (Length(APrimaryKeyProperty) > 0) then
    begin
      if (Length(APrimaryKeyProperty) >= 1) then
       ADatabaseName := APrimaryKeyProperty[0];
      if (Length(APrimaryKeyProperty) >= 2) then
          AWhereClause  := AWhereClause + ATableAlias + 'Model = '+ QuotedStr(APrimaryKeyProperty[1]);
      if (Length(APrimaryKeyProperty) >= 3) then
          AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'StudyAreaName = '+ QuotedStr(APrimaryKeyProperty[2]);
      if (Length(APrimaryKeyProperty) >= 4) then
          AWhereClause  := AWhereClause + ' AND  '+ ATableAlias + 'SubArea = '+ QuotedStr(APrimaryKeyProperty[3]);
      if (Length(APrimaryKeyProperty) >= 5) then
          AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'Scenario = '+ QuotedStr(APrimaryKeyProperty[4]);
      if (Length(APrimaryKeyProperty) >= 6) then
        AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'ChartID = '+ APrimaryKeyProperty[5];
      if (Length(APrimaryKeyProperty) >= 7) then
        AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'PlaneNumber = '+ APrimaryKeyProperty[6];
      if (Length(APrimaryKeyProperty) >= 8) then
        AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'TargetDraftID = '+ APrimaryKeyProperty[7];
      if (Length(APrimaryKeyProperty) >= 9) then
        AWhereClause  := AWhereClause + ' AND '+ ATableAlias + 'CurveType = '+ QuotedStr(APrimaryKeyProperty[8]);

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetChanged: boolean;
const OPNAME = 'TYRCObject.GetChanged';
begin
  Result := False;
  try
    Result := FChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetCurveGroupType(ATypeConstant: string): TCurveGroupType;
const OPNAME = 'TYRCObject.GetCurveGroupType';
var
  LConstant: char;
begin
  Result := ctPureRegression;
  try
    if (Length(ATypeConstant) > 0) then
    begin
      LConstant := (UpperCase(ATypeConstant))[1];
      case LConstant of
        'D': Result := ctDeterministic;
        'R': Result := ctRegression;
        'O': Result := ctOriginal;
        'P': Result := ctPureRegression;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetTargetDraftSavedMode(ATypeConstant: integer): TChartEditMode;
const OPNAME = 'TYRCObject.GetTargetDraftSavedMode';
begin
  Result := tdmNone;
  try
    case ATypeConstant of
      1: Result := tdmDeterministic;
      2: Result := tdmRegression;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetChartNumber: integer;
const OPNAME = 'TYRCObject.GetChartNumber';
begin
  Result := -1;
  try
    Result := FChartNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetLoaded: boolean;
const OPNAME = 'TYRCObject.GetLoaded';
begin
  Result := False;
  try
    Result := FLoaded;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetLoadedFromDB: boolean;
const OPNAME = 'TYRCObject.GetLoadedFromDB';
begin
  Result := False;
  try
    Result := FLoadedFromDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCObject.GetSavedToDB: boolean;
const OPNAME = 'TYRCObject.GetSavedToDB';
begin
  Result := False;
  try
    Result := FSavedToDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCObject.SetChartNumber(AChartNumber: integer);
const OPNAME = 'TYRCObject.SetChartNumber';
begin
  try
    if(FChartNumber <> AChartNumber) then
      FChartNumber := AChartNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCObject.SetLoaded(ALoaded: boolean);
const OPNAME = 'TYRCObject.SetLoaded';
begin
  try
    if(FLoaded <> ALoaded) then
      FLoaded := ALoaded;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCObject.SetChanged(AChanged: boolean);
const OPNAME = 'TYRCObject.SetChanged';
begin
  try
    if(FChanged <> AChanged) then
      FChanged := AChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCObject.SetLoadedFromDB(ALoadedFromDB: boolean);
const OPNAME = 'TYRCObject.SetLoadedFromDB';
begin
  try
    if(FLoadedFromDB <> ALoadedFromDB) then
      FLoadedFromDB := ALoadedFromDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCObject.SetSavedToDB(ASavedToDB: boolean);
const OPNAME = 'TYRCObject.SetSavedToDB';
begin
  try
    if(FSavedToDB <> ASavedToDB) then
      FSavedToDB := ASavedToDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLabelProperties }

procedure TLabelProperties.Populate(AText: string; ALeftTopX, ALeftTopY,AArrowToX, AArrowToY: integer; ACustom: boolean);
const OPNAME = 'TLabelProperties.Reset';
begin
  try
    FText     := AText;
    FLeftTopX := ALeftTopX;
    FLeftTopY := ALeftTopY;
    FArrowToX := AArrowToX;
    FArrowToY := AArrowToY;
    FCustom   := ACustom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLabelProperties.Reset;
const OPNAME = 'TLabelProperties.Reset';
begin
  try
    FText     := '';
    FLeftTopX := NullInteger;
    FLeftTopY := NullInteger;
    FArrowToX := NullInteger;
    FArrowToY := NullInteger;
    FCustom   := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

//
//
//  UNIT      : Contains TTimeSeriesComparitorSubareaDataManager Class
//  AUTHOR    : Dziedzi Ramulondi (Arivia)
//  DATE      : 2003/05/01
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorSubAreaDataManager;

interface

uses
  Classes,
  VCLTee.Chart,
  VCL.ComCtrls,
  UAbstractObject;
  //UTimeSeriesComparitorData;
type

  TTimeSeriesComparitorSubAreaData  = class(TObject)
  protected
    FOldModelCode: String;
    FOldStudyAreaCode: String;
    FOldSubAreaCode: String;
    FNewModelCode: String;
    FNewStudyAreaCode: String;
    FNewSubAreaCode: String;
  public
    constructor Create;
    function SubAreaHasChanged: boolean;
    property OldModelCode: string       read FOldModelCode      write FOldModelCode;
    property OldStudyAreaCode: string   read FOldStudyAreaCode  write FOldStudyAreaCode;
    property OldSubAreaCode: string     read FOldSubAreaCode    write FOldSubAreaCode;
    property NewModelCode: string       read FNewModelCode      write FNewModelCode;
    property NewStudyAreaCode: string   read FNewStudyAreaCode  write FNewStudyAreaCode;
    property NewSubAreaCode: string     read FNewSubAreaCode    write FNewSubAreaCode;
  end;


  TTimeSeriesComparitorSubareaDataManager = class(TAbstractAppObject)
  protected
    FSubAreData: TTimeSeriesComparitorSubAreaData;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function SubAreaHasChanged: boolean;
    property SubAreData: TTimeSeriesComparitorSubAreaData read FSubAreData;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TSubArea }

constructor TTimeSeriesComparitorSubAreaData.Create;
const OPNAME = 'TTimeSeriesComparitorSubAreaData.Create';
begin
  inherited Create;
  try
    FOldModelCode      := '';
    FOldStudyAreaCode  := '';
    FOldSubAreaCode    := '';
    FNewModelCode      := '';
    FNewStudyAreaCode  := '';
    FNewSubAreaCode    := '';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubAreaData.SubAreaHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorSubAreaData.SubAreaHasChanged';
begin
  Result := False;
  try
    Result := (FOldModelCode     <> FNewModelCode) or
              (FOldStudyAreaCode <> FNewStudyAreaCode) or
              (FOldSubAreaCode   <> FNewSubAreaCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorSubareaDataManager }

procedure TTimeSeriesComparitorSubareaDataManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSubareaDataManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSubAreData  := TTimeSeriesComparitorSubAreaData.Create;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorSubareaDataManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorSubareaDataManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSubAreData);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubareaDataManager.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorSubareaDataManager.Initialise';
begin
  Result := inherited Initialise;
  try

  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubareaDataManager.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorSubareaDataManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FSubAreData.OldModelCode     := FSubAreData.NewModelCode;
    FSubAreData.OldStudyAreaCode := FSubAreData.NewStudyAreaCode;
    FSubAreData.FOldSubAreaCode  := FSubAreData.NewSubAreaCode;

    FSubAreData.NewModelCode     := FAppModules.StudyArea.ModelCode;
    FSubAreData.NewStudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
    FSubAreData.NewSubAreaCode   := FAppModules.StudyArea.SubAreaCode;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorSubareaDataManager.SubAreaHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorSubareaDataManager.SubAreaHasChanged';
begin
  Result := False;
  try
    Result := FSubAreData.SubAreaHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.


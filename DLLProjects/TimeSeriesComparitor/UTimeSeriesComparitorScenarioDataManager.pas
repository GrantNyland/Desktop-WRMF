//
//
//  UNIT      : Contains TTimeSeriesComparitorScenarioViewDataManager Class
//  AUTHOR    : Dziedzi Ramulondi (Arivia)
//  DATE      : 2003/05/01
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorScenarioDataManager;

interface

uses
  DB,
  Classes,
  VCLTee.Chart,
  VCL.ComCtrls,
  Contnrs,
  UAbstractObject;
  //,UTimeSeriesComparitorData;
type


  TTimeSeriesComparitorScenarioViewData  = Class(TAbstractAppObject)
  protected
    FModelCode: String;
    FStudyAreaCode: String;
    FSubAreaCode: String;
    FScenarioCode: String;
    FSenarioViewName: string;
    FChanged: boolean;
    procedure SetSenarioViewName(SenarioViewName: string);
  public
     function Initialise: boolean; override;
     function StudyHasChanged: boolean; override;
     property ModelCode: string read FModelCode write FModelCode;
     property StudyAreaCode: string read FStudyAreaCode write FStudyAreaCode;
     property SubAreaCode: string read FSubAreaCode write FSubAreaCode;
     property ScenarioCode: string read FScenarioCode write FScenarioCode;
     property SenarioViewName: string read FSenarioViewName write SetSenarioViewName;
     property Changed: boolean read FChanged write FChanged;
  end;


  {TTimeSeriesComparitorScenarioViewDataManager = class(TAbstractAppObject)
  protected
    FScenarioViewList: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function  GetScenarioViewByIndex(AIndex: integer)  :TTimeSeriesComparitorScenarioViewData;
    function ScenarioViewByName(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode: string): TTimeSeriesComparitorScenarioViewData;
  public
    function PopulateFromDataset(ADataset: TDataSet): boolean;

    function CurrentView:TTimeSeriesComparitorScenarioViewData;
    function AddCurrentView(ACurrentViewName: string): boolean;
    function SelectCurrentView(ACurrentViewName: string): boolean;
    function RenameCurrentView(AOldViewName,ACurrentViewName: string): boolean;
    function DeleteCurrentView(ACurrentViewName: string): boolean;

    //function AddScenarioView(AScenarioView: TTimeSeriesComparitorScenarioViewData): boolean;
    property ScenarioViewByIndex[AIndex: integer]: TTimeSeriesComparitorScenarioViewData read GetScenarioViewByIndex;
    property ScenarioViewList: TObjectList read FScenarioViewList;
  end;}

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorScenarioViewData }


function TTimeSeriesComparitorScenarioViewData.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewData.Initialise';
begin
  Result := Inherited Initialise;
  try
    FModelCode       := '';
    FStudyAreaCode   := '';
    FSubAreaCode     := '';
    FScenarioCode    := '';
    FSenarioViewName := '';
    Result           := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewData.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewData.StudyHasChanged';
begin
  Result := Inherited StudyHasChanged;
  try
    FModelCode     := FAppModules.StudyArea.ModelCode;
    FStudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
    FSubAreaCode   := FAppModules.StudyArea.SubAreaCode;
    FScenarioCode  := FAppModules.StudyArea.ScenarioCode;
    Result         := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorScenarioViewData.SetSenarioViewName(SenarioViewName: string);
const OPNAME = 'TTimeSeriesComparitorScenarioViewData.SetSenarioViewName';
begin
  try
    FSenarioViewName := SenarioViewName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorScenarioViewDataManager }
{
procedure TTimeSeriesComparitorScenarioViewDataManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FScenarioViewList := TObjectList.Create(True);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorScenarioViewDataManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FScenarioViewList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorScenarioViewDataManager.GetScenarioViewByIndex(AIndex: integer): TTimeSeriesComparitorScenarioViewData;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.GetScenarioViewByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FScenarioViewList.Count) then
      Result := TTimeSeriesComparitorScenarioViewData(FScenarioViewList.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{function TTimeSeriesComparitorScenarioViewDataManager.AddScenarioView(AScenarioView: TTimeSeriesComparitorScenarioViewData): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.AddScenarioView';
begin
  Result := False;
  try
    if Assigned(AScenarioView) then
    begin
      FScenarioViewList.Add(AScenarioView);
      Result := True;
    end
  except on E : Exception do HandleError(E,OPNAME); end;
end;}
{
function TTimeSeriesComparitorScenarioViewDataManager.ScenarioViewByName(AModelCode,AStudyAreaCode, ASubAreaCode,
         AScenarioCode:string ): TTimeSeriesComparitorScenarioViewData;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.ScenarioViewByName';
var
  LScenarioView: TTimeSeriesComparitorScenarioViewData;
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FScenarioViewList.Count - 1 do
    begin
      LScenarioView := TTimeSeriesComparitorScenarioViewData(FScenarioViewList.Items[LIndex]);
      if Assigned(LScenarioView) then
      begin
        if(LScenarioView.ModelCode = AModelCode) and
          (LScenarioView.StudyAreaCode = AStudyAreaCode) and
          (LScenarioView.SubAreaCode = ASubAreaCode) and
          (LScenarioView.ScenarioCode = AScenarioCode) then
        begin
          Result := LScenarioView;
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.AddCurrentView(ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.AddCurrentView';
var
  LScenarioViewData: TTimeSeriesComparitorScenarioViewData;
begin
  Result := False;
  try
    LScenarioViewData := CurrentView;
    if Assigned(LScenarioViewData) then
    begin
      LScenarioViewData.SenarioViewName := ACurrentViewName;
    end
    else
    begin
     LScenarioViewData := TTimeSeriesComparitorScenarioViewData.Create;
     LScenarioViewData.ModelCode      := FAppModules.StudyArea.ModelCode;
     LScenarioViewData.StudyAreaCode  := FAppModules.StudyArea.StudyAreaCode;
     LScenarioViewData.SubAreaCode    := FAppModules.StudyArea.SubAreaCode;
     LScenarioViewData.ScenarioCode   := FAppModules.StudyArea.ScenarioCode;
     LScenarioViewData.SenarioViewName := ACurrentViewName;
     FScenarioViewList.Add(LScenarioViewData);
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.DeleteCurrentView(ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.DeleteCurrentView';
var
  LScenarioViewData: TTimeSeriesComparitorScenarioViewData;
begin
  Result := False;
  try
    LScenarioViewData := CurrentView;
    if Assigned(LScenarioViewData) then
    begin
      FScenarioViewList.Delete(FScenarioViewList.IndexOf(LScenarioViewData));
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.RenameCurrentView(AOldViewName, ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.RenameCurrentView';
var
  LScenarioViewData: TTimeSeriesComparitorScenarioViewData;
begin
  Result := False;
  try
    LScenarioViewData := CurrentView;
    if Assigned(LScenarioViewData) then
    begin
      LScenarioViewData.SenarioViewName := ACurrentViewName;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.SelectCurrentView(ACurrentViewName: string): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.SelectCurrentView';
var
  LScenarioViewData: TTimeSeriesComparitorScenarioViewData;
begin
  Result := False;
  try
    LScenarioViewData := CurrentView;
    if Assigned(LScenarioViewData) then
    begin
      LScenarioViewData.SenarioViewName := ACurrentViewName;
      Result := True;
    end
    else
    begin
      Result := AddCurrentView(ACurrentViewName);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.PopulateFromDataset(ADataset: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.PopulateFromDataset';
var
  LScenarioView: TTimeSeriesComparitorScenarioViewData;
begin
  Result := False;
  try
    ScenarioViewList.Clear;
    while not ADataset.Eof do
    begin
      LScenarioView := TTimeSeriesComparitorScenarioViewData.Create;
      LScenarioView.ModelCode       := Trim(ADataset.FieldByName('Model').AsString);
      LScenarioView.StudyAreaCode   := Trim(ADataset.FieldByName('StudyAreaName').AsString);
      LScenarioView.SubAreaCode     := Trim(ADataset.FieldByName('SubArea').AsString);
      LScenarioView.ScenarioCode    := Trim(ADataset.FieldByName('Scenario').AsString);
      LScenarioView.SenarioViewName := Trim(ADataset.FieldByName('ScenarioViewName').AsString);
      LScenarioView.Changed         := False;
      ScenarioViewList.Add(LScenarioView);
      ADataset.Next;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorScenarioViewDataManager.CurrentView: TTimeSeriesComparitorScenarioViewData;
const OPNAME = 'TTimeSeriesComparitorScenarioViewDataManager.CurrentView';
begin
  Result := nil;
  try
    Result :=  ScenarioViewByName(FAppModules.StudyArea.ModelCode,
                                  FAppModules.StudyArea.StudyAreaCode,
                                  FAppModules.StudyArea.SubAreaCode,
                                  FAppModules.StudyArea.ScenarioCode);
  except on E : Exception do HandleError(E,OPNAME); end;
end;}

end.


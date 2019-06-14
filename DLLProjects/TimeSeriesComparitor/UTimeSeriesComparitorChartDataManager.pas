//
//
//  UNIT      : Contains TTimeSeriesComparitorChartDataManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/19
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorChartDataManager;

interface
uses
  DB,
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  Contnrs,
  UAbstractObject,
  UTimeSeriesComparitorSeriesDataManager;

type
  TTimeSeriesComparitorChartData = Class(TAbstractAppObject)
  protected
    FSeriesDataManager: TTimeSeriesComparitorSeriesDataManager;
    FModelCode: String;
    FStudyAreaCode: String;
    FSubAreaCode: String;
    FChartName: string;
    FDateCreated  : TDateTime;
    FHeaderCaption,
    FFooterCaption: string;
    FAddedToView,
    FChanged: boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetChartName(AChartName : string);
    procedure SetDateCreated(ADateCreated : TDateTime);
    procedure SetHeaderCaption(AHeaderCaption : string);
    procedure SetFooterCaption(AFooterCaption : string);
    procedure SetAddedToView(AAddedToView : boolean);
  public
     property ModelCode: string read FModelCode write FModelCode;
     property StudyAreaCode: string read FStudyAreaCode write FStudyAreaCode;
     property SubAreaCode: string read FSubAreaCode write FSubAreaCode;
     property ChartName: string read FChartName write SetChartName;
     property DateCreated: TDateTime read FDateCreated write SetDateCreated;
     property HeaderCaption: string read FHeaderCaption write SetHeaderCaption;
     property FooterCaption: string read FFooterCaption write SetFooterCaption;
     property AddedToView: boolean read FAddedToView write SetAddedToView;
     property Changed: boolean read FChanged write FChanged;
     property SeriesDataManager: TTimeSeriesComparitorSeriesDataManager read FSeriesDataManager;
  end;

  TTimeSeriesComparitorChartDataManager = class(TAbstractAppObject)
  protected
    FChartDataList: TObjectList;
    FCurrentChartData: TTimeSeriesComparitorChartData;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetChartDataByIndex(AChartDataIndex: integer): TTimeSeriesComparitorChartData;
    function GetChartDataByName(AChartName: string): TTimeSeriesComparitorChartData;
  public
    function Initialise: boolean; override;
    function PopulateFromDataset(ADataset: TDataSet): boolean;

    function CreateChartData(AChartName: string): TTimeSeriesComparitorChartData;
    function SelectChartData(AChartName: String): TTimeSeriesComparitorChartData;
    function DeleteChartData(AChartName: String): boolean;
    function RenameChartData(AOldChartName,ANewChartName: String): boolean;


    function AddChartDataToView(AChartName: String): boolean;
    function RemoveChartDataFromView(AChartName: String): boolean;

    function ChartNames(ANamesList: TStringList): boolean;

    property ChartDataByIndex[AChartDataIndex: integer]: TTimeSeriesComparitorChartData read GetChartDataByIndex;
    property ChartDataByName[ChartName: string]: TTimeSeriesComparitorChartData read GetChartDataByName;
    property CurrentChartData: TTimeSeriesComparitorChartData read  FCurrentChartData;
    property ChartDataList: TObjectList read FChartDataList;
  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorChartData }

procedure TTimeSeriesComparitorChartData.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSeriesDataManager := TTimeSeriesComparitorSeriesDataManager.Create(FAppModules);
    FModelCode       := '';
    FStudyAreaCode   := '';
    FSubAreaCode     := '';
    FFooterCaption   := '';
    FChartName       := '';
    FDateCreated     := 0;
    FHeaderCaption   := '';
    FFooterCaption   := '';
    FAddedToView     := False;
    FChanged         := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartData.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartData.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FSeriesDataManager);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartData.SetAddedToView(AAddedToView: boolean);
const OPNAME = 'TTimeSeriesComparitorChartData.SetAddedToView';
begin
  try
    FAddedToView := AAddedToView;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartData.SetHeaderCaption(AHeaderCaption: string);
const OPNAME = 'TTimeSeriesComparitorChartData.SetHeaderCaption';
begin
  try
    FHeaderCaption := AHeaderCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartData.SetDateCreated(ADateCreated: TDateTime);
const OPNAME = 'TTimeSeriesComparitorChartData.SetDateCreated';
begin
  try
    FDateCreated := ADateCreated;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartData.SetChartName(AChartName: string);
const OPNAME = 'TTimeSeriesComparitorChartData.SetChartName';
begin
  try
    FChartName := AChartName;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TTimeSeriesComparitorChartData.SetFooterCaption(AFooterCaption: string);
const OPNAME = 'TTimeSeriesComparitorChartData.SetFooterCaption';
begin
  try
    FFooterCaption := AFooterCaption;
    FChanged := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{ TTimeSeriesComparitorChartDataManager }

procedure TTimeSeriesComparitorChartDataManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FChartDataList    := TObjectList.Create(False);
    FCurrentChartData := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorChartDataManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChartDataList);
    inherited CreateMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FChartDataList.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.GetChartDataByIndex(AChartDataIndex: integer): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.GetChartDataByIndex';
begin
  Result := nil;
  try
    if (AChartDataIndex >= 0) and (AChartDataIndex < FChartDataList.Count) then
      Result := TTimeSeriesComparitorChartData(FChartDataList.Items[AChartDataIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorChartDataManager.ChartNames(ANamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.ChartNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FChartDataList.Count - 1 do
      begin
        if Assigned(ChartDataByIndex[LIndex]) then
          ANamesList.Add(ChartDataByIndex[LIndex].ChartName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.GetChartDataByName(AChartName: string): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.GetChartDataByName';
var
  LIndex: integer;
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := nil;
  try
    for LIndex := 0 to FChartDataList.Count - 1 do
    begin
      LChartData := ChartDataByIndex[LIndex];
      if Assigned(LChartData) and
         (LChartData.ChartName      = AChartName) then
      begin
        Result := LChartData;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.PopulateFromDataset( ADataset: TDataSet): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.PopulateFromDataset';
var
  LChartData: TTimeSeriesComparitorChartData;
  LView: string;
begin
  Result := False;
  try
    FChartDataList.Clear;
    LView := Trim(ADataset.FieldByName('ViewName').AsString);
    while not ADataset.Eof do
    begin
      if(LView <> ADataset.FieldByName('ViewName').AsString)) then
        Break;
      LChartData := TTimeSeriesComparitorChartData.Create(FAppModules);
      LChartData.ModelCode       := Trim(ADataset.FieldByName('Model').AsString);
      LChartData.StudyAreaCode   := Trim(ADataset.FieldByName('StudyAreaName').AsString);
      LChartData.SubAreaCode     := Trim(ADataset.FieldByName('SubArea').AsString);
      LChartData.ChartName       := Trim(ADataset.FieldByName('ChartName').AsString);
      LChartData.DateCreated     := ADataset.FieldByName('DateCreated').AsDateTime;
      LChartData.HeaderCaption   := Trim(ADataset.FieldByName('HeaderCaption').AsString);
      LChartData.FooterCaption   := Trim(ADataset.FieldByName('FooterCaption').AsString);
      LChartData.AddedToView     := True;
      LChartData.Changed         := False;
      FChartDataList.Add(LChartData);
      LChartData.FSeriesDataManager.PopulateFromDataset(ADataset);
      //ADataset.Next;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.CreateChartData(AChartName: string): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.CreateChartData';
var
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := nil;
  try
    if(AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if not Assigned(LChartData) then
      begin
        LChartData := TTimeSeriesComparitorChartData.Create(FAppModules);
        LChartData.ModelCode     := FAppModules.StudyArea.ModelCode;
        LChartData.StudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
        LChartData.SubAreaCode   := FAppModules.StudyArea.SubAreaCode;
        LChartData.ChartName     := AChartName;
        FChartDataList.Add(LChartData);
        SelectChartData(AChartName);
      end;
      Result := LChartData;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.DeleteChartData(AChartName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.DeleteChartData';
var
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if(AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if Assigned(LChartData) then
      begin
        FChartDataList.Remove(LChartData);
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.SelectChartData(AChartName: String): TTimeSeriesComparitorChartData;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.SelectChartData';
var
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := nil;
  try
    if(AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if Assigned(LChartData) then
      begin
        FCurrentChartData := LChartData;
        Result := LChartData;
      end
      else
        Result := CreateChartData(AChartName);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.AddChartDataToView(AChartName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.AddChartDataToView';
var
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if(AChartName <> '') then
    begin
      LChartData := GetChartDataByName(AChartName);
      if Assigned(LChartData) then
      begin
        LChartData.AddedToView  := True;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.RemoveChartDataFromView(AChartName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.RemoveChartDataFromView';
begin
  Result := False;
  try
    Result := DeleteChartData(AChartName);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorChartDataManager.RenameChartData(AOldChartName, ANewChartName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorChartDataManager.RenameChartData';
var
  LChartData: TTimeSeriesComparitorChartData;
begin
  Result := False;
  try
    if(AOldChartName <> '') then
    begin
      LChartData := GetChartDataByName(AOldChartName);
      if Assigned(LChartData) then
      begin
        LChartData.ChartName  := ANewChartName;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

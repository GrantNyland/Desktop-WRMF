//
//
//  UNIT      : Contains TTimeSeriesComparitorViewManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/19
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorViewManager;

interface
uses
  DB,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  Contnrs,
  UAbstractObject,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorViewList; 

type

  TTimeSeriesComparitorViewManager = class(TAbstractAppObject)
  protected
    FViewDataList: TimeSeriesComparitorViewList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetViewDataByIndex(AViewDataIndex: integer): TTimeSeriesComparitorView;
    function GetViewDataByName(AViewName: string): TTimeSeriesComparitorView;
  public
    function Initialise: boolean; override;

    function CreateViewData(AViewName: string): TTimeSeriesComparitorView;
    function SelectViewData(AViewName: String): TTimeSeriesComparitorView;
    function DeleteViewData(AViewName: String): boolean; overload;
    function DeleteViewData(Var AView: TTimeSeriesComparitorView): boolean; overload;
    function RenameViewData(AOldViewName,ANewViewName: String): boolean;
    function RemoveChartFromViews(AChart: TTimeSeriesComparitorChart): boolean;
    function ViewNames(ANamesList: TStringList): boolean;

    property ViewDataByIndex[AViewDataIndex: integer]: TTimeSeriesComparitorView read GetViewDataByIndex;
    property ViewDataByName[ViewName: string]: TTimeSeriesComparitorView read GetViewDataByName;
    property TSCViewDataList: TimeSeriesComparitorViewList read FViewDataList;
  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorViewManager }

procedure TTimeSeriesComparitorViewManager.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorViewManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FViewDataList    := TimeSeriesComparitorViewList.Create(FAppModules);
    FViewDataList.OwnsObjects := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorViewManager.DestroyMemberObjects;
const OPNAME = 'TTimeSeriesComparitorViewManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FViewDataList);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataList.Initialise;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.GetViewDataByIndex(AViewDataIndex: integer): TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorViewManager.GetViewDataByIndex';
begin
  Result := nil;
  try
    if (AViewDataIndex >= 0) and (AViewDataIndex < FViewDataList.ViewsCount) then
      Result := TTimeSeriesComparitorView(FViewDataList.ViewByIndex[AViewDataIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorViewManager.ViewNames(ANamesList: TStringList): boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.ViewNames';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(ANamesList) then
    begin
      ANamesList.Clear;
      for LIndex := 0 to FViewDataList.ViewsCount - 1 do
      begin
        if Assigned(ViewDataByIndex[LIndex]) then
          ANamesList.Add(ViewDataByIndex[LIndex].ViewName);
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.GetViewDataByName(AViewName: string): TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorViewManager.GetViewDataByName';
var
  LIndex: integer;
  LViewData: TTimeSeriesComparitorView;
begin
  Result := nil;
  try
    for LIndex := 0 to FViewDataList.ViewsCount - 1 do
    begin
      LViewData := ViewDataByIndex[LIndex];
      if Assigned(LViewData) and
         (LViewData.ViewName      = AViewName) then
      begin
        Result := LViewData;
        Break;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.CreateViewData(AViewName: string): TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorViewManager.CreateViewData';
var
  LViewData: TTimeSeriesComparitorView;
begin
  Result := nil;
  try
    if(AViewName <> '') then
    begin
      LViewData := GetViewDataByName(AViewName);
      if not Assigned(LViewData) then
      begin
        LViewData := TTimeSeriesComparitorView.Create(FAppModules);
        LViewData.ModelCode     := FAppModules.StudyArea.ModelCode;
        LViewData.StudyAreaCode := FAppModules.StudyArea.StudyAreaCode;
        LViewData.SubAreaCode   := FAppModules.StudyArea.SubAreaCode;
        LViewData.ViewName      := AViewName;
        FViewDataList.AddView(LViewData);
      end;
      Result := LViewData;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.DeleteViewData(AViewName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.DeleteViewData';
var
  LViewData: TTimeSeriesComparitorView;
begin
  Result := False;
  try
    if(AViewName <> '') then
    begin
      LViewData := GetViewDataByName(AViewName);
      if Assigned(LViewData) then
      begin
        FViewDataList.RemoveView(LViewData);
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.DeleteViewData( var AView: TTimeSeriesComparitorView): boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.DeleteViewData';
begin
  Result := False;
  try
    if Assigned(AView) then
    begin
      Result := FViewDataList.RemoveView(AView);
      if Result then
      begin
        AView := nil;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.SelectViewData(AViewName: String): TTimeSeriesComparitorView;
const OPNAME = 'TTimeSeriesComparitorViewManager.SelectViewData';
var
  LViewData: TTimeSeriesComparitorView;
begin
  Result := nil;
  try
    if(AViewName <> '') then
    begin
      LViewData := GetViewDataByName(AViewName);
      if Assigned(LViewData) then
      begin
        Result := LViewData;
      end
      else
      Result := CreateViewData(AViewName);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTimeSeriesComparitorViewManager.RenameViewData(AOldViewName, ANewViewName: String): boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.RenameViewData';
var
  LViewData: TTimeSeriesComparitorView;
begin
  Result := False;
  try
    if(AOldViewName <> '') then
    begin
      LViewData := GetViewDataByName(AOldViewName);
      if Assigned(LViewData) then
      begin
        LViewData.ViewName  := ANewViewName;
        Result := True;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorViewManager.RemoveChartFromViews(AChart: TTimeSeriesComparitorChart): boolean;
const OPNAME = 'TTimeSeriesComparitorViewManager.RemoveChartFromViews';
var
  LIndex: integer;
  LViewData: TTimeSeriesComparitorView;
begin
  Result := False;
  try
    if Assigned(AChart) then
    begin
      for LIndex := 0 to FViewDataList.ViewsCount - 1 do
      begin
        LViewData := ViewDataByIndex[LIndex];
        if Assigned(LViewData) and (LViewData.CurrentChart = AChart) then
          LViewData.CurrentChart := nil;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

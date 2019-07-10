

unit UDailyDiversionDataObject;

interface
uses
  Classes,
  SysUtils,
  Contnrs,
  UAbstractModelData,
  UDailyDiversionGaugeData,
  UViewModelDataObject,
  UFilesLineTypeObject,
  UFileNames,
  UIFRFeatures,
  UDailyIFRData,
  UAbstractObject;


type
  TDailyDiversionDataObject = class(TAbstractModelData)
  protected
    FDailyIFRDataList: TDailyIFRDataList;
    FDailyDiversionGaugeDataList: TDailyDiversionGaugeDataList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetDailyDiversionControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
    function GetDailyIFRDataByStationNo(AStationNo: string) : TDailyIFRData;
  public
    procedure Reset;
    function RefreshCalculatedData : boolean;
    function RefreshDailyInstreamFlowData(ADiversionGauge : TDiversionGauge)  : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function LoadData: boolean;
    function RefreshDailyReferenceData(AStationID : integer) : boolean;
    function ClearFile14(AStationID:integer) : boolean;
    function RefreshFlowDiversionRelationship(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
    function ClearDailyFlowDataFromCSVFile(AStationID : integer) : boolean;
    function ClearDailyInstreamFlowFileData(AStationID : integer) : boolean;
    function ClearFlowDiversionRelation(AStationID : integer) : boolean;
    function GenerateFlowDiversionRelation(AStationID : integer) : boolean;
    function Initialise: boolean; override;
    function GenerateWRYMData(AStationID : integer) : boolean;
    function ExportDailyIFR(AStationID : integer) : boolean;
    function ExportMonthlyIFR(AStationID : integer) : boolean;
    function ExportFlowDiversionRelationship(AStationID : integer) : boolean;
    function GenerateDailyIFR(ADiversionGauge : TDiversionGauge;ADailyIFRData : TDailyIFRData) : boolean;
    function GetViewDataItems(AViewId : string; AItemsList : TViewModelDataItemsList; var AHandled : boolean): boolean; override;
    property DailyDiversionGaugeDataList : TDailyDiversionGaugeDataList read FDailyDiversionGaugeDataList;
    property DailyIFRDataList : TDailyIFRDataList read FDailyIFRDataList;
    property DailyIFRData[AstationNo : string] : TDailyIFRData read GetDailyIFRDataByStationNo;

end;
implementation
uses
  VCL.Controls,
  VCL.Dialogs,
  UConstants,
  UDailyDivesionProgressBar,
  UDailyDiversionGaugeDataLoadAgent,
  UErrorHandlingOperations;

{ TDailyDiversionDataObject }

function TDailyDiversionDataObject._AddRef: Integer;
const OPNAME = 'TDailyDiversionDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionDataObject._Release: Integer;
const OPNAME = 'TDailyDiversionDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDailyDiversionDataObject.CreateMemberObjects;
const OPNAME = 'TDailyDiversionDataObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FDailyDiversionGaugeDataList := TDailyDiversionGaugeDataList.Create(FAppModules);
    FDailyIFRDataList := TDailyIFRDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionDataObject.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionDataObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FDailyDiversionGaugeDataList);
    FreeandNil(FDailyIFRDataList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionDataObject.GetViewDataItems(AViewId: string;
  AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TDailyDiversionDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;//inherited GetViewDataItems(AViewId, AItemsList, AHandled);
  try
    if (not AHandled) then
    begin
      if (Trim(AViewId) <> '') and Assigned(AItemsList) then
      begin
        LUpperViewId := UpperCase(Trim(AViewId));
        if (Pos('DAILYDIVERSIONSTATIONDATA',LUpperViewId) = 1) then
          AHandled := GetDailyDiversionControlViewDataItems(AViewId,AItemsList);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionDataObject.GetDailyDiversionControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TDailyDiversionDataObject.GetDailyDiversionControlViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LDiversionGauge    : TDiversionGauge;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewID := UpperCase(Trim(AViewID));
    if (Trim(AViewID) <> '') and Assigned(AItemsList) then
    begin
      for LIndex := 0 to FDailyDiversionGaugeDataList.DailyDiversionGaugeDataCount - 1 do
      begin
        LDiversionGauge := FDailyDiversionGaugeDataList.DiversionGaugeByIndex[lIndex];
        if Assigned(LDiversionGauge) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Caption     := Trim(LDiversionGauge.StationNo);
            LViewModelDataItem.Weighting   := LDiversionGauge.StationID;
            LViewModelDataItem.ParamNames  :=  'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LDiversionGauge.StationID);
            LViewModelDataItem.DataType    := 'DAILYDIVERSIONSTATIONDATA';
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionDataObject.Initialise: boolean;
const OPNAME = 'TDailyDiversionDataObject.Initialise';
begin
  Result := False;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionDataObject.Reset;
const OPNAME = 'TDailyDiversionDataObject.Reset';
begin
  try
    FDailyDiversionGaugeDataList.Initialise;
    FDailyIFRDataList.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionDataObject.LoadData: boolean;
const OPNAME = 'TDailyDiversionDataObject.LoadData';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.LoadDailyDiversionGaugeData(FDailyDiversionGaugeDataList);
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.RefreshDailyReferenceData(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.RefreshDailyReferenceData';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.LoadDailyFlowData(FDailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID]);
      LLoadAgent.LoadDailyFlowDataStartEndDate(FDailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID]);
      FDailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID].AddDailyFlowDataGaps;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.GetDailyIFRDataByStationNo(AStationNo : string) : TDailyIFRData;
const OPNAME = 'TDailyDiversionDataObject.GetDailyIFRDataByStationNo';
begin
  Result := nil;
  try
    Result := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRDataList.GetDailyIFRDataByStationNo(AStationNo);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ClearFile14(AStationID:integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.ClearFile14';
var
  LStationNo : string;
begin
  Result := False;
  try
    LStationNo := DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID].StationNo;
    Result := DailyIFRDataList.GetDailyIFRDataByStationNo(LStationNo).Initialise;
    if Result then
    begin
      FAppModules.ViewIni.WriteString('TFilesActionDailyDiversionManager','StationNo_'+LStationNo,'');
      FAppModules.ViewIni.WriteString('TFilesActionDailyDiversionManager','File14_'+LStationNo,'');
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ClearFlowDiversionRelation(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.ClearFlowDiversionRelation';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    try
      if LDiversionGauge <> nil then
      begin
        if LLoadAgent.ClearFlowDiversionRelation(LDiversionGauge) then
          Result := LDiversionGauge.ClearFlowDiversionRelationship;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ClearDailyInstreamFlowFileData(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.ClearDailyInstreamFlowFileData';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    try
      if LDiversionGauge <> nil then
      begin
        if LLoadAgent.ClearDailyInstreamFlowFileData(LDiversionGauge) then
          Result := LDiversionGauge.ClearDailyInstreamFlowFileData;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ClearDailyFlowDataFromCSVFile(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.ClearDailyFlowDataFromCSVFile';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    try
      if LDiversionGauge <> nil then
      begin
        if LLoadAgent.ClearDailyFlowDataFromCSVFile(LDiversionGauge) then
          Result := LDiversionGauge.ClearDailyFlowData;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.GenerateWRYMData(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.GenerateWRYMData';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LDiversionGauge : TDiversionGauge;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    try
      if LDiversionGauge <> nil then
      begin
        Result := LLoadAgent.GenerateWRYMData(LDiversionGauge);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.GenerateDailyIFR(ADiversionGauge : TDiversionGauge;ADailyIFRData : TDailyIFRData) : boolean;
const OPNAME = 'TDailyDiversionDataObject.GenerateDailyIFR';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    try
      if (ADiversionGauge <> nil) and (ADailyIFRData <> nil) then
      begin
        Result := LLoadAgent.GenerateDailyIFR(ADiversionGauge,ADailyIFRData);
        if Result then
          FAppModules.Model.StudyDataHasChanged(sdccAdd,'InstreamAvgFlow','DAILYDIVERSIONDATA',IntToStr(ADiversionGauge.StationID));
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionDataObject.GenerateFlowDiversionRelation(AStationID : integer) : boolean;
const OPNAME = 'TDailyDiversionDataObject.GenerateFlowDiversionRelation';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
  LDiversionGauge : TDiversionGauge;
//  LDailyDiversionDatePickerDialog : TDailyDiversionDatePickerDialog;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    try
      if LDiversionGauge <> nil then
      begin
        Result := LLoadAgent.GenerateFlowDiversionRelation(LDiversionGauge, LDiversionGauge.StartDate, LDiversionGauge.EndDate);
{        LDailyDiversionDatePickerDialog := TDailyDiversionDatePickerDialog.CreateWithoutDFM(nil,FAppModules);
        try
          LDailyDiversionDatePickerDialog.Initialise;
          LDailyDiversionDatePickerDialog.StartDate := LDiversionGauge.StartDate;
          LDailyDiversionDatePickerDialog.EndDate := LDiversionGauge.EndDate;
          LDailyDiversionDatePickerDialog.ShowModal;
          if (LDailyDiversionDatePickerDialog.ModalResult = mrOK) then
            Result := LLoadAgent.GenerateFlowDiversionRelation(LDiversionGauge, LDailyDiversionDatePickerDialog.StartDate, LDailyDiversionDatePickerDialog.EndDate);
        finally
          FreeAndNil(LDailyDiversionDatePickerDialog);
        end;}
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDailyDiversionDataObject.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.RefreshCalculatedData: boolean;
const OPNAME = 'TDailyDiversionDataObject.RefreshCalculatedData';
begin
  Result := False;
  try
    Result := FDailyDiversionGaugeDataList.Initialise;
    Result := Result and LoadData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.RefreshFlowDiversionRelationship(ADiversionGauge : TDiversionGauge; AStartDate, AEndDate : TDateTime) : boolean;
const OPNAME = 'TDailyDiversionDataObject.RefreshFlowDiversionRelationship';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try
   LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
   try
     Result := LLoadAgent.LoadFlowDiversionRelation(ADiversionGauge,AStartDate, AEndDate);
   finally
     FreeAndNil(LLoadAgent);
   end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.RefreshDailyInstreamFlowData(ADiversionGauge : TDiversionGauge) : boolean;
const OPNAME = 'TDailyDiversionDataObject.RefreshDailyInstreamFlowData';
var
  LLoadAgent : TDailyDiversionGaugeDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TDailyDiversionGaugeDataLoadAgent.Create(FAppModules);
    try
      if (ADiversionGauge <> nil) then
      begin
        Result := LLoadAgent.LoadDailyInstreamFlowData(ADiversionGauge);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ExportDailyIFR(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionDataObject.ExportDailyIFR';
var
  LDiversionGauge : TDiversionGauge;
  LDailyInstream  : TDailyInstreamFlowData;
  LData           : TStringList;
  LIndex          : integer;
  LSaveDlg        : TSaveDialog;
  LStr            : string;
  LYear,
  LMonth,
  LDay            : word;
begin
  Result := False;
  try
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    if LDiversionGauge <> nil then
    begin
      if LDiversionGauge.DailyInstreamDataCount > 0 then
      begin
        LData := TStringList.Create;
        try
          for LIndex := 0 to LDiversionGauge.DailyInstreamDataCount-1 do
          begin
            LDailyInstream := LDiversionGauge.InstreamFlowDataByIndex[LIndex];
            DecodeDate(LDailyInstream.InstreamDate,LYear,LMonth,LDay);
            LStr := Format('%s,%s,%d,%s,%s,%s',[DateToStr(LDailyInstream.InstreamDate),FormatFloat('0.000',LDailyInstream.AvgFlow),
                    LDailyInstream.QualityCode,FormatFloat('0.000',LDailyInstream.FactoredInstreamFlow),
                    FormatFloat('0.000',LDailyInstream.DailyAvailableFlow[LMonth]),
                    FormatFloat('0.000',LDailyInstream.DailyDiversionFlow[LMonth])]);
            LData.Add(LStr);
          end;
          LSaveDlg := TSaveDialog.Create(nil);
          try
            LSaveDlg.FileName :=  'Daily IFR Data for '+LDiversionGauge.StationNo;
            if (LSaveDlg.Execute) then
              LData.SaveToFile(LSaveDlg.FileName+'.csv');
          finally
            LSaveDlg.Free;
          end;
        finally
          LData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ExportMonthlyIFR(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionDataObject.ExportMonthlyIFR';
var
  LDiversionGauge : TDiversionGauge;
  LMonthlyInstreamFlowData : TMonthlyInstreamFlowData;
  LData : TStringList;
  LIndex : integer;
  LSaveDlg : TSaveDialog;
  LStr : string;
  LYearTotal : double;
  LCount : integer;
  LAvarages : array[1..12] of double;
  LRecCount : array[1..12] of double;
  LDailyDivesionProgressBar : TDailyDivesionProgressBar;
  LGrandAvarage : double;
begin
  Result := False;
  try
    LDiversionGauge :=  DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    if LDiversionGauge <> nil then
    begin
      if LDiversionGauge.MonthlyInstreamFlowCount > 0 then
      begin
        LData := TStringList.Create;
        try
          LDailyDivesionProgressBar := TDailyDivesionProgressBar.CreateWithoutDFM(nil, FAppModules);
          LSaveDlg := TSaveDialog.Create(nil);
          try
            LSaveDlg.FileName :=  'Monthly IFR Data for '+LDiversionGauge.StationNo;
            LDailyDivesionProgressBar.ProgressPosition := 0;
            LDailyDivesionProgressBar.MaxProgress := LDiversionGauge.MonthlyInstreamFlowCount;
            LDailyDivesionProgressBar.Initialise;
            LDailyDivesionProgressBar.ShowForm;
            LStr := 'Year';
            for LCount := MinMonths to MaxMonths do
            begin
              LStr := LStr+','+CHydroMonthsDesc[LCount];
              LRecCount[LCount] := 0;
              LAvarages[LCount] := 0;
            end;
            LStr := LStr+',Total';
            LData.Add(LStr);
            LGrandAvarage := 0;
            for LIndex := 0 to LDiversionGauge.MonthlyInstreamFlowCount-1 do
            begin
              LDailyDivesionProgressBar.ProgressPosition := LDailyDivesionProgressBar.ProgressPosition + 1;
              LMonthlyInstreamFlowData := LDiversionGauge.MonthlyInstreamFlowDataByIndex[LIndex];
              if (LMonthlyInstreamFlowData <> nil) then
              begin
                LYearTotal := 0;
                LStr := IntToStr(LMonthlyInstreamFlowData.Year)+'/'+IntToStr(LMonthlyInstreamFlowData.Year+1);
                for LCount := MinMonths to MaxMonths do
                begin
                  if LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] = NullFloat then
                    LStr := LStr+','
                  else
                  begin
                    LStr := LStr+','+FormatFloat('0.000',LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount]);
                    LYearTotal := LYearTotal + LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount];
                    LAvarages[LCount] := LAvarages[LCount] + LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount];
                    LRecCount[LCount] := LRecCount[LCount]+1;
                  end;
                end;
                LGrandAvarage := LGrandAvarage + LYearTotal;
                LStr := LStr+','+FormatFloat('0.000',LYearTotal);
              end;
              LData.Add(LStr);
            end;
            LStr := 'Average';
            for LCount := MinMonths to MaxMonths do
            begin
              LAvarages[LCount] := LAvarages[LCount]/LRecCount[LCount];
              LStr := LStr+','+FormatFloat('####0.000',LAvarages[LCount]);
            end;
            LGrandAvarage := LGrandAvarage/LDiversionGauge.MonthlyDailyFlowCount;
            LStr := LStr+','+FormatFloat('####0.000',LGrandAvarage);
            LData.Add(LStr);
            if (LSaveDlg.Execute) then
              LData.SaveToFile(LSaveDlg.FileName+'.csv');
          finally
            LSaveDlg.Free;
            FreeAndNil(LDailyDivesionProgressBar);
          end;
        finally
          LData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionDataObject.ExportFlowDiversionRelationship(AStationID: integer): boolean;
const OPNAME = 'TDailyDiversionDataObject.ExportFlowDiversionRelationship';
var
  LDiversionGauge : TDiversionGauge;
  LWRYMData       : TWRYMChannelData;
  LData           : TStringList;
  LIndex          : integer;
  LSaveDlg        : TSaveDialog;
  LStr            : string;
begin
  Result := False;
  try
    LDiversionGauge := DailyDiversionGaugeDataList.DiversionGaugeByStationID[AStationID];
    if LDiversionGauge <> nil then
    begin
      if LDiversionGauge.WRYMDataCount > 0 then
      begin
        LData := TStringList.Create;
        try
          for LIndex := 0 to LDiversionGauge.WRYMDataCount - 1 do
          begin
            LWRYMData := LDiversionGauge.WRYMDataByIndex[LIndex];
            if(LWRYMData <> nil) then
            begin
              LStr := Format('%s,%s,%s,%s',[IntToStr(LWRYMData.Identifier),
                      FormatFloat('0.000',LWRYMData.ReferenceFlow),
                      FormatFloat('0.000',LWRYMData.DiversionFlow),
                      FormatFloat('0.000',LWRYMData.NonDiversionFlow)]);
            end;
            LData.Add(LStr);
          end;
          LSaveDlg := TSaveDialog.Create(nil);
          try
            LSaveDlg.FileName := 'Flow Diversion Relationship Data for ' + LDiversionGauge.StationNo;
            if (LSaveDlg.Execute) then
              LData.SaveToFile(LSaveDlg.FileName + '.csv');
          finally
            LSaveDlg.Free;
          end;
        finally
          LData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

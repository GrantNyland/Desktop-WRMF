{******************************************************************************}
{*  UNIT      : Contains the class TWeatherEventsValidator.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/20                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UWeatherEventsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
//  UChangeData,
  VoaimsCom_TLB,
  UWeatherEventsDialog;

type
  TWeatherEventsValidator = class(TAbstractDataDialogValidator)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoRefreshData (Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateControls;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function WeatherEventsDialog : TWeatherEventsDialog;
    procedure SetDateRange (AStartDate : TDateTime;
                            AEndDate   : TDateTime);
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations, Math, DateUtils;

{ TWeatherEventsValidator }

procedure TWeatherEventsValidator.CreateMemberObjects;
const OPNAME = 'TWeatherEventsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TWeatherEventsDialog.Create(FPanelOwner,FAppModules);
    with WeatherEventsDialog do
    begin
      BtnRefresh.OnClick := DoRefreshData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.DestroyMemberObjects;
const OPNAME = 'TWeatherEventsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.Initialise: boolean;
const OPNAME = 'TWeatherEventsValidator.Initialise';
var
  lDate : TDateTime;
begin
  Result := inherited Initialise;
  try
    with WeatherEventsDialog do
    begin
      lDate := FAppModules.WeatherEvents.Data.EarliestDate;
      DtpStartDate.Date := lDate;
      lDate := FAppModules.WeatherEvents.Data.LatestDate;
      DtpEndDate.Date := lDate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.PopulateControls;
const OPNAME = 'TWeatherEventsValidator.PopulateControls';
var
  lAreaLst : TStringList;
  lIndex   : integer;
begin
  try
    with WeatherEventsDialog do
    begin
      CbxAreas.Clear;
      lAreaLst := TStringList.Create;
      try
        lAreaLst.CommaText := FAppModules.WeatherEvents.Data.GetAreas;
        CbxAreas.Items.Add('All Areas');
        for lIndex := 0 to lAreaLst.Count - 1 do
          CbxAreas.Items.Add(lAreaLst.Strings[lIndex]);
        CbxAreas.ItemIndex := 0;
      finally
        FreeAndNil(lAreaLst);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := WeatherEventsDialog.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.ClearDataViewer;
const OPNAME = 'TWeatherEventsValidator.ClearDataViewer';
var
  lRow : integer;
  lCol : integer;
begin
  inherited ClearDataViewer;
  try
    with WeatherEventsDialog do
    begin
      for lRow := 1 to GrdWeatherEvents.RowCount - 1 do
        for lCol := 0 to GrdWeatherEvents.ColCount - 1 do
          GrdWeatherEvents.Cells[lCol, lRow] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.PopulateDataViewer;
const OPNAME = 'TWeatherEventsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    PopulateControls;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.DoRefreshData;
const OPNAME = 'TWeatherEventsValidator.DoRefreshData';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.RePopulateDataViewer;
const OPNAME = 'TWeatherEventsValidator.RePopulateDataViewer';
var
  lDataString   : string;
  lDataList     : TStringList;
  lIndex        : integer;
  lPos          : integer;
  lEventStr     : string;
  lArea         : string;
  lStart        : string;
  lStartDate    : string;
  lEnd          : string;
  lEndDate      : string;
  lEvent        : string;
  lComment      : string;
begin
  try
    ClearDataViewer;
    with WeatherEventsDialog do
    begin
      if ((DtpStartDate.Date >= 0) AND (DtpEndDate.Date >= 0)) then
      begin
        if (CbxAreas.ItemIndex = 0) then
          lDataString := FAppModules.WeatherEvents.Data.FindWeatherEvents(Trunc(DtpStartDate.Date), Trunc(DtpEndDate.Date), '')
        else
          lDataString := FAppModules.WeatherEvents.Data.FindWeatherEvents(Trunc(DtpStartDate.Date), Trunc(DtpEndDate.Date), CbxAreas.Text);
        lDataList   := TStringList.Create;
        try
          lDataList.CommaText := lDataString;
          GrdWeatherEvents.RowCount := 1 + lDataList.Count;
          if (GrdWeatherEvents.RowCount > 1) then
            GrdWeatherEvents.FixedRows := 1;
          for lIndex := 0 to lDataList.Count - 1 do
          begin
            lEventStr  := lDataList.Strings[lIndex];
            lPos       := Pos('|', lEventStr);
            lStart     := Copy(lEventStr, 1, lPos - 1);
            lEventStr  := Copy(lEventStr, lPos + 1, Length(lEventStr) - lPos);
            lPos       := Pos('|', lEventStr);
            lEnd       := Copy(lEventStr, 1, lPos - 1);
            lEventStr  := Copy(lEventStr, lPos + 1, Length(lEventStr) - lPos);
            lPos       := Pos('|', lEventStr);
            lArea      := Copy(lEventStr, 1, lPos - 1);
            lEventStr  := Copy(lEventStr, lPos + 1, Length(lEventStr) - lPos);
            lPos       := Pos('|', lEventStr);
            lEvent     := Copy(lEventStr, 1, lPos - 1);
            lComment   := Copy(lEventStr, lPos + 1, Length(lEventStr) - lPos);
            lStartDate := Format('%4s/%2s/%2s', [Copy(lStart, 1, 4), Copy(lStart, 5, 2), Copy(lStart, 7, 2)]);
            lEndDate   := Format('%4s/%2s/%2s', [Copy(lEnd, 1, 4), Copy(lEnd, 5, 2), Copy(lEnd, 7, 2)]);
            GrdWeatherEvents.Cells[0, lIndex+1] := IntToStr(lIndex + 1);
            GrdWeatherEvents.Cells[1, lIndex+1] := lStartDate;
            GrdWeatherEvents.Cells[2, lIndex+1] := lEnddate;
            GrdWeatherEvents.Cells[3, lIndex+1] := lArea;
            GrdWeatherEvents.Cells[4, lIndex+1] := lEvent;
            GrdWeatherEvents.Cells[5, lIndex+1] := lComment;
          end;
        finally
          FreeAndNil(lDataList);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.SaveState: boolean;
const OPNAME = 'TWeatherEventsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.WeatherEventsDialog : TWeatherEventsDialog;
const OPNAME = 'TWeatherEventsValidator.WeatherEventsDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TWeatherEventsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                      AFieldName : string;
                                                      AOldValue  : string;
                                                      ANewValue  : string): boolean;
const OPNAME = 'TWeatherEventsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.StudyHasChanged: boolean;
const OPNAME = 'TWeatherEventsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsValidator.SetDateRange (AStartDate : TDateTime;
                                                AEndDate   : TDateTime);
const OPNAME = 'TWeatherEventsValidator.SetDateRange';
begin
  try
    with WeatherEventsDialog do
    begin
      DtpStartDate.Date := AStartDate;
      DtpEndDate.Date   := AEndDate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TWeatherEventsValidator.ProcessMetaDataEvent';
{var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lRow           : integer;
  lFieldIndex    : string;
  lParamChange   : IParameterChange;
  lChangeList    : IChangeList;}
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
{      lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
      if (lChangeList <> nil) then
      begin
        with WeatherEventsDialog do
        begin
          if (FActiveControl = GrdParamChanges) then
          begin
            lRow           := GrdParamChanges.Row;
            lFieldIndex    := IntToStr(lRow);
            lFieldProperty := FAppModules.FieldProperties.FieldProperty('ChangeListItem');
            lParamChange   := lChangeList.ParamChangeByIndex(lRow-1);
            if (lFieldProperty <> nil) AND (lParamChange <> nil) then
            begin
              lKeyValues := lChangeList.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.MetaData.ShowMetaData
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              PopulateDataViewer;
              Result := TRUE;
            end;
          end;
        end;
      end;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


//
//
//  UNIT      : Contains TEstimatedHistoricValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 12/07/2007
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UEstimatedHistoricValidator;

interface

uses
  VCL.Grids,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UEstimatedHistoricDialog,

  UFileNames,
  UFilesActionYieldManager,
  UAbstractFileNamesObject,
  UDataFileObjects,
  UFilesActionAbstractManager;
  
type
  TEstimatedHistoricValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FNoFailureValue : double;
    procedure CreateMemberObjects; override;
    procedure OnEditControlEnter(ASender: TObject); override;
    procedure OnEditControltExit(ASender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnSelectedCellHasChanged(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure RePopulateDataViewer;
    procedure PopulateDebugData(AData : TStrings);
    procedure DoOnDrawCell(Sender: TObject;ACol,ARow:Longint;Rect:TRect;State:TGridDrawState);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function EstimatedHistoricDialog: TEstimatedHistoricDialog;

end;

implementation
uses
  SysUtils,
  System.UITypes,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UUknownDatabaseAgent,
  UUnKnownFileAgent,
  UErrorHandlingOperations,
  UAbstractModelData;

const
  lblMin = 'SPECIFIED MINIMUM YIELD';
  lblMax = 'SPECIFIED MAXIMUM YIELD';
  lblNoFailure = 'PREVIOUS YIELD WITHOUT FAILURES';
  lblFailure = 'PREVIOUS YIELD WITH FAILURES';

{ TEstimatedHistoricValidator }

procedure TEstimatedHistoricValidator.CreateMemberObjects;
const OPNAME = 'TEstimatedHistoricValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TEstimatedHistoricDialog.Create(FPanelOwner,FAppModules);
    EstimatedHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutYield'));
    EstimatedHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutYield'));
    EstimatedHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutYield'));
    EstimatedHistoricDialog.HistoricGrid.OnDrawCell := DoOnDrawCell;
    TabShetCaption := FAppModules.Language.GetString('ViewData.EstimatedHistoric');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TEstimatedHistoricValidator.ClearDataViewer;
const OPNAME = 'TEstimatedHistoricValidator.ClearDataViewer';
var
  LCol, LRow : integer;
begin
  inherited ClearDataViewer;
  try
    for LRow := 1 to EstimatedHistoricDialog.HistoricGrid.RowCount-1 do
      for LCol := 0 to EstimatedHistoricDialog.HistoricGrid.ColCount-1 do
        EstimatedHistoricDialog.HistoricGrid.Cells[LCol,LRow] := '';
    EstimatedHistoricDialog.HistoricGrid.RowCount := 2;
    FNoFailureValue := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.DoOnDrawCell(Sender: TObject; ACol,ARow: Integer; Rect: TRect; State: TGridDrawState);
const OPNAME = 'TEstimatedHistoricValidator.DoOnDrawCell';
var
  LHistoricFirmYield : double;
begin
  try
    if (ACol = 1) and (ARow >1) then
    begin
      if (Trim((Sender as TFieldStringGrid).Cells[ACol, ARow]) <> '') then
      begin
        LHistoricFirmYield := StrToFloat((Sender as TFieldStringGrid).Cells[ACol, ARow]);
        if LHistoricFirmYield = FNoFailureValue then
        begin
          (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
          if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
            (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack;
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
          (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricValidator.EstimatedHistoricDialog: TEstimatedHistoricDialog;
const OPNAME = 'TEstimatedHistoricValidator.EstimatedHistoricDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TEstimatedHistoricDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricValidator.Initialise: boolean;
const OPNAME = 'TEstimatedHistoricValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricValidator.LanguageHasChanged: boolean;
const OPNAME = 'TEstimatedHistoricValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.OnEditControlEnter(ASender: TObject);
const OPNAME = 'TEstimatedHistoricValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(ASender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TEstimatedHistoricValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(ASender);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.OnSelectedCellHasChanged(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TEstimatedHistoricValidator.OnSelectedCellHasChanged';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TEstimatedHistoricValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.PopulateDataViewer;
const OPNAME = 'TEstimatedHistoricValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.PopulateDebugData(AData : TStrings);
const OPNAME = 'TEstimatedHistoricValidator.PopulateDebugData';
var
  LNoFailureValue : double;
  LIndex : integer;
  LFirmYieldData : TStringList;
  LNoFailure,
  LFailure,
  LValue : string;
begin
  try
    if AData.Count > 0 then
    begin
      LFirmYieldData := TStringList.Create;
      LFirmYieldData.Duplicates := dupIgnore;
      try
        for LIndex := 0 to AData.Count-1 do
        begin
          if Pos(lblMin,AData[LIndex]) > 1 then
            EstimatedHistoricDialog.lblMin.Caption := Format('Min = %s mcm/a',[Copy(AData[LIndex],Pos('=',AData[LIndex])+1,Length(AData[LIndex]))]);
          if Pos(lblMax,AData[LIndex]) > 1 then
            EstimatedHistoricDialog.lblMax.Caption := Format('Max = %s mcm/a',[Copy(AData[LIndex],Pos('=',AData[LIndex])+1,Length(AData[LIndex]))]);
          if Pos(lblNoFailure,AData[LIndex]) > 1 then
          begin
            LNoFailure := 'No Failure='+Copy(AData[LIndex],Pos('=',AData[LIndex])+1,Length(AData[LIndex]));
            if LFirmYieldData.IndexOf(LNoFailure) < 0 then
              LFirmYieldData.Add(LNoFailure);
            LNoFailureValue := StrToFloat(Trim(Copy(AData[LIndex],Pos('=',AData[LIndex])+1,Length(AData[LIndex]))));
            if LNoFailureValue > FNoFailureValue then
              FNoFailureValue := LNoFailureValue;
          end;
          if Pos(lblFailure,AData[LIndex]) > 1 then
          begin
            LFailure := 'Failure='+Copy(AData[LIndex],Pos('=',AData[LIndex])+1,Length(AData[LIndex]));
            if LFirmYieldData.IndexOf(LFailure) < 0 then
              LFirmYieldData.Add(LFailure);
          end;
        end;
        EstimatedHistoricDialog.HistoricGrid.RowCount := LFirmYieldData.Count+1;
        for LIndex := 0 to LFirmYieldData.Count-1 do
        begin
          LValue := LFirmYieldData[LIndex];
          EstimatedHistoricDialog.HistoricGrid.Cells[0,LIndex+1] := IntToStr(LIndex+1);
          EstimatedHistoricDialog.HistoricGrid.Cells[1,LIndex+1] := Copy(LValue,Pos('=',LValue)+1,Length(LValue));
          EstimatedHistoricDialog.HistoricGrid.Cells[2,LIndex+1] := Copy(LValue,1,Pos('=',LValue)-1);
        end;
      finally
        LFirmYieldData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TEstimatedHistoricValidator.RePopulateDataViewer;
const OPNAME = 'TEstimatedHistoricValidator.RePopulateDataViewer';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LData : TStringList;
  LUnKnownFileAgent: TUnKnownFileAgent;
begin
  try
    LCurrentFileNames := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.OutputFileNames;
    LData := TStringList.Create;
    LUnKnownFileAgent := TUnKnownFileAgent.Create(FAppModules);
    try
      if LCurrentFileNames <> nil then
      begin
        for LCount := 0 to LCurrentFileNames.Count - 1 do
        begin
          if not FileExists(LCurrentFileNames.FileNameObject[LCount].FileName) then
            Continue;
          case GetOutputFileType(LCurrentFileNames.FileNameObject[LCount].FileName) of
            oftDebug:
            begin
              LResult := LUnKnownFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],LData,nil);
              if LResult then
                PopulateDebugData(LData);
            end;
          end;
        end;
      end;
    finally
      LData.Free;
      LUnKnownFileAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TEstimatedHistoricValidator.SaveState: boolean;
const OPNAME = 'TEstimatedHistoricValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,ANewValue: string): boolean;
const OPNAME = 'TEstimatedHistoricValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TEstimatedHistoricValidator.StudyHasChanged: boolean;
const OPNAME = 'TEstimatedHistoricValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

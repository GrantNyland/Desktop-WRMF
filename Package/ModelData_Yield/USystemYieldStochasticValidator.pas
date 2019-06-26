//
//
//  UNIT      : Contains the class TSystemYieldStochasticValidator.
//  AUTHOR    : Sam Dhlamini(ARIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit USystemYieldStochasticValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  USystemYieldStochasticDialog;

type
  TSystemYieldStochasticValidator = class(TAbstractDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure RePopulateDataViewer;
    procedure PopulateStochasticGraph(AIndex : integer;ATargetDraftData,AReliability : TStrings);
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function SystemYieldStochasticDialog: TSystemYieldStochasticDialog;
    function CanExport : Boolean; override;
    procedure DoExport(AFileName: string = ''); override;
  end;

implementation

uses
  SysUtils,
  UConstants,
  VCL.Graphics,
  UMainMenuEventType,
  UYieldModelDataObject,
  URunConfigurationData,
  USystemYieldObject,
  UErrorHandlingOperations,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.Series;

{ TSystemYieldStochasticValidator }

procedure TSystemYieldStochasticValidator.CreateMemberObjects;
const OPNAME = 'TSystemYieldStochasticValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TSystemYieldStochasticDialog.Create(FPanelOwner,FAppModules);

    SystemYieldStochasticDialog.StochasticGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutTargetDraft'));
    SystemYieldStochasticDialog.StochasticGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutNoOfFailureSequences'));
    SystemYieldStochasticDialog.StochasticGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutReliabilityOfSupply'));
    SystemYieldStochasticDialog.StochasticGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutRecurrenceIntervalOfFailure'));

    SystemYieldStochasticDialog.StochasticGrid.Cells[0, 0] := FAppModules.Language.GetString('TField.SumOutTargetDraft');
    SystemYieldStochasticDialog.StochasticGrid.Cells[1, 0] := FAppModules.Language.GetString('TField.SumOutNoOfFailureSequences');
    SystemYieldStochasticDialog.StochasticGrid.Cells[2, 0] := FAppModules.Language.GetString('TField.SumOutReliabilityOfSupply');
    SystemYieldStochasticDialog.StochasticGrid.Cells[3, 0] := FAppModules.Language.GetString('TField.SumOutRecurrenceIntervalOfFailure');

    SystemYieldStochasticDialog.StochasticGrid.OnColEnter            := OnStringGridColEnter;
    TabShetCaption := FAppModules.Language.GetString('ViewData.Stochastic');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.Initialise: boolean;
const OPNAME = 'TSystemYieldStochasticValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldStochasticValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticValidator.ClearDataViewer;
const OPNAME = 'TSystemYieldStochasticValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticValidator.PopulateDataViewer;
const OPNAME = 'TSystemYieldStochasticValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticValidator.RePopulateDataViewer;
const OPNAME = 'TSystemYieldStochasticValidator.RePopulateDataViewer';
var
  lSystemYieldObject : TSystemYieldObject;
  lRowCount,
  lGridIndex,
  lIndex2,
  lIndex             : integer;
  lPeriodLength      : TPeriodLength;
  LFieldProperty     : TAbstractFieldProperty;
  LTargetDraftData  : TStringList;
  LReliability     : TStringList;
begin
  try
    try
      lSystemYieldObject := TSystemYieldObject.Create(FAppModules);
      lSystemYieldObject.SystemYieldType := ytStochastic;
      lSystemYieldObject.Initialise;
      lGridIndex := 0;
      {SystemYieldStochasticDialog.ClearChart;
      SystemYieldStochasticDialog.PrepareChart;
      SystemYieldStochasticDialog.StochasticGraph.BottomAxis.SetMinMax(0,100);
      SystemYieldStochasticDialog.StochasticGraph.BottomAxis.Increment := 10;}
      for LIndex := 0 to 9 do
      begin
        try
          LPeriodLength := LSystemYieldObject.PriodLengthData[LIndex];
          if (LPeriodLength <> nil) then
          begin
            if LPeriodLength.PeriodCount = 0 then
              break;
            SystemYieldStochasticDialog.StochasticGrid.Visible := True;
            SystemYieldStochasticDialog.lblStochastic.Visible := True;
            SystemYieldStochasticDialog.lblNumberOfSeqAnalysed.Visible := True;
            if (LIndex > 0) then
              SystemYieldStochasticDialog.lblStochastic.Caption := FAppModules.Language.GetString('LabelCaption.MultipleStochasticYieldResult')
            else
            if (LIndex = 0) then
              SystemYieldStochasticDialog.lblStochastic.Caption := FAppModules.Language.GetString('LabelCaption.StochasticYieldResult');
            SystemYieldStochasticDialog.lblNumberOfSeqAnalysed.Caption := FAppModules.Language.GetString('LabelCaption.NrOfSequencesAnalysed')+
                                                                IntToStr(lSystemYieldObject.NumberOfSeqAnalysed);
            if (LIndex > 0) then
              LGridIndex := lGridIndex + (lPeriodLength.RecordCounter + 1)
            else
              LGridIndex := 2;
            LRowCount := lGridIndex + lPeriodLength.RecordCounter;
            SystemYieldStochasticDialog.StochasticGrid.RowCount := LRowCount;
            LTargetDraftData  := TStringList.Create;
            LReliability     := TStringList.Create;
            try
              for LIndex2 := 0 to LPeriodLength.RecordCounter do
              begin
                if LPeriodLength.TargetDraft[LIndex2] = 0 then
                  break;
                if lIndex2 = 0 then
                  SystemYieldStochasticDialog.StochasticGrid.Cells[0,(LIndex2+lGridIndex)-1] := 'Period Length (years) = '+IntToStr(lPeriodLength.FPeriod );
                LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
                SystemYieldStochasticDialog.StochasticGrid.Cells[0,lIndex2+lGridIndex] := Format(LFieldProperty.FormatStringGrid,[LPeriodLength.TargetDraft[LIndex2]]);
                SystemYieldStochasticDialog.StochasticGrid.Cells[1,lIndex2+lGridIndex] := Format('%d',[Trunc(lPeriodLength.NoOfFaiureSeq[LIndex2])]);
                SystemYieldStochasticDialog.StochasticGrid.Cells[2,lIndex2+lGridIndex] := Format('%6f',[lPeriodLength.ReliabilityOfSupply[LIndex2]]);
                SystemYieldStochasticDialog.StochasticGrid.Cells[3,lIndex2+lGridIndex] := Format('%6f',[lPeriodLength.RecurrenceIntaval[LIndex2]]);
                LTargetDraftData.Add(Format(LFieldProperty.FormatStringGrid,[LPeriodLength.TargetDraft[LIndex2]]));
                LReliability.Add(Format('%6f',[lPeriodLength.ReliabilityOfSupply[LIndex2]]));
              end;
              if (LTargetDraftData.Count>0) and (LReliability.Count>0) then
              begin
                //SystemYieldStochasticDialog.StochasticGraph.Legend.Visible := True;
                //SystemYieldStochasticDialog.StochasticGraph.LeftAxis.Title.Caption := FAppModules.Language.GetString('GraphHeading.TargetDraft');
                //SystemYieldStochasticDialog.StochasticGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('YRC.BottomAxisCaption');
                PopulateStochasticGraph(LIndex,LTargetDraftData,LReliability);
              end;
            finally
              FreeAndNil(LTargetDraftData);
              FreeAndNil(LReliability);
            end;
          end;

          if (LPeriodLength.PeriodCount = 0) and not (LIndex >= 0) then
          begin
            SystemYieldStochasticDialog.lblStochastic.Visible := False;
            SystemYieldStochasticDialog.lblNumberOfSeqAnalysed.Visible := False;
            SystemYieldStochasticDialog.StochasticGrid.Visible := False;
            SystemYieldStochasticDialog.Visible := False;
          end;
        finally
          if Assigned(LPeriodLength) then
            FreeAndNil(LPeriodLength);
        end;
      end
    finally
      FreeAndNil(LSystemYieldObject);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldStochasticValidator.PopulateStochasticGraph(AIndex : integer;ATargetDraftData,AReliability : TStrings);
const OPNAME = 'TSystemYieldStochasticValidator.PopulateStochasticGraph';
begin
  try
    if not TYieldModelDataObject(FAppModules.Model.ModelData).CastYRCGraphDataObject.Loaded then
    begin
      if TYieldModelDataObject(FAppModules.Model.ModelData).CastYRCGraphDataObject.SavedToDB then
        FAppModules.Model.ProcessEvent(CmeYRCLoadFromDB,nil)
      else
        FAppModules.Model.ProcessEvent(CmeYRCLoadFromFile,nil)
    end;
    SystemYieldStochasticDialog.StochasticGraph.Initialise;
    SystemYieldStochasticDialog.StochasticGraph.LanguageHasChanged;
    SystemYieldStochasticDialog.StochasticGraph.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSystemYieldStochasticValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.StudyHasChanged: boolean;
const OPNAME = 'TSystemYieldStochasticValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.SystemYieldStochasticDialog: TSystemYieldStochasticDialog;
const OPNAME = 'TSystemYieldStochasticValidator.SystemYieldStochasticDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TSystemYieldStochasticDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldStochasticValidator.CanExport: Boolean;
const OPNAME = 'TSystemYieldStochasticValidator.CanExport';
begin
   Result := False;
  try
    if (SystemYieldStochasticDialog.StochasticGrid.Cells[0,1] <> '') then
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldStochasticValidator.DoExport(AFileName: string = '');
const OPNAME = 'TSystemYieldStochasticValidator.DoExport';
begin
  try
    if (SystemYieldStochasticDialog.StochasticGrid.Cells[0,1] <> '') then
      SystemYieldStochasticDialog.StochasticGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


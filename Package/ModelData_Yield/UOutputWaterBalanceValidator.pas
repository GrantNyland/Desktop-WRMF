//
//
//  UNIT      : Contains the class TOutputWaterBalanceValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputWaterBalanceValidator;

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
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputWaterBalanceDialog;

type
  TOutputWaterBalanceValidator = class(TAbstractOutputDialogValidator)
  private
    FLoadCase     : integer;
    FSequence     : integer;
    FMonth        : integer;
    FUnits        : TOutputUnits;
    FValueType    : TOutputValueType;
    FTimeStep     : TOutputTimeStep;
    FHighLight    : WordBool;
    FDisplayMonth : integer;
    FCurrentViewData : TOutputDataType;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure RePopulateDataViewer;
    procedure PopulateChannelWaterBalance(AValues: TStrings);
    procedure PopulateReservoirWaterBalance(ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings);
    procedure ClearWaterBalanceViewer;
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure GetOutputDataSelection;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ShowSelectedRecord(ASender: TObject;ACurrentRecord: integer); override;
    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;

    function ViewDialog: TOutputWaterBalanceDialog;
  end;

implementation

uses
  SysUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TOutputWaterBalanceValidator }

procedure TOutputWaterBalanceValidator.CreateMemberObjects;
const OPNAME = 'TOutputWaterBalanceValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel                              := TOutputWaterBalanceDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.btnDetail.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SummaryOut');
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.DestroyMemberObjects;
const OPNAME = 'TOutputWaterBalanceValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.Initialise: boolean;
const OPNAME = 'TOutputWaterBalanceValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.GetOutputDataSelection;
const OPNAME = 'TOutputWaterBalanceValidator.GetOutputDataSelection';
var
  LDataSelection : IOutputDataSelection;
begin
  try
   LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
   if (LDataSelection <> nil) then
   begin
     FLoadCase     := LDataSelection.LoadCase;
     FSequence     := LDataSelection.Sequence;
     FMonth        := LDataSelection.Month;
     FUnits        := LDataSelection.Units;
     FValueType    := LDataSelection.ValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWaterBalanceValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.WaterBalance');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.ClearDataViewer;
const OPNAME = 'TOutputWaterBalanceValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.PopulateDataViewer;
const OPNAME = 'TOutputWaterBalanceValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    GetOutputDataSelection;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.SaveState: boolean;
const OPNAME = 'TOutputWaterBalanceValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.ViewDialog : TOutputWaterBalanceDialog;
const OPNAME = 'TOutputWaterBalanceValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputWaterBalanceDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputWaterBalanceValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputWaterBalanceValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TOutputWaterBalanceValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TOutputWaterBalanceValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TOutputWaterBalanceValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputWaterBalanceValidator.GetNextSignificantRecord';
var
  LError : string;
begin
  Result := ACurrentRecord;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                GetBlockNextSignificantMonth(btMonthlyAverageChannelFlow,FIdentifier,FLoadCase,FSequence,ACurrentRecord,LError);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWaterBalanceValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputWaterBalanceValidator.GetPreviousSignificantRecord';
var
  LError : string;
begin
  Result := ACurrentRecord;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                GetBlockPreviousSignificantMonth(btMonthlyAverageChannelFlow,FIdentifier,FLoadCase,FSequence,ACurrentRecord,LError);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.ClearWaterBalanceViewer;
const OPNAME = 'TOutputWaterBalanceValidator.ClearWaterBalanceViewer';
begin
  try
    ViewDialog.MainViewer.Initialise;
    ViewDialog.DetailViewer.Initialise;
    ViewDialog.MainViewer.Visible := False;
    ViewDialog.DetailViewer.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.ShowSelectedRecord(ASender: TObject;ACurrentRecord: integer);
const OPNAME = 'TOutputWaterBalanceValidator.ShowSelectedRecord';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.RePopulateDataViewer;
const OPNAME = 'TOutputWaterBalanceValidator.RePopulateDataViewer';
var
  LTotalsValues,
  LSummaryValues,
  LInflowValues,
  LOutflowValues : TStringList;
  LError         : string;
  LResult        : boolean;
begin
  try
    FCurrentViewData := btNone;
    ClearWaterBalanceViewer;
    if (FIdentifier >= 0) AND (NetworkElementType <> votNone) then
    begin
      if (FTimeStep = otsMonthly) AND (FMonth <= 0) then Exit;

      FErrorMessage := '';
      ViewDialog.ShowError(FErrorMessage);

      LResult := False;
      case FNetworkElementType of
        votReservoir:
        begin
          LSummaryValues := TStringList.Create;
          LInflowValues  := TStringList.Create;
          LOutflowValues := TStringList.Create;
          LTotalsValues  := TStringList.Create;
          try
            LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                       GetReservoirWaterBalancelData(FIdentifier,FLoadCase,FSequence,LSummaryValues,
                       LInflowValues,LOutflowValues,LTotalsValues,LError);
            FCurrentViewData := btReservoirWaterBalance;
            if LResult then
              PopulateReservoirWaterBalance(LSummaryValues,LInflowValues,LOutflowValues,LTotalsValues)
            else
              ViewDialog.ShowError(LError);
          finally
            LSummaryValues.Free;
            LInflowValues.Free;
            LOutflowValues.Free;
            LTotalsValues.Free;
          end;
        end;

        votChannel:
        begin
          LSummaryValues := TStringList.Create;
          try
            case FTimeStep of
              otsMonthly:
              begin
                LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                           GetMonthlyChannelWaterBalancelData(FIdentifier,FLoadCase,FSequence,
                           FSequence,LSummaryValues,LError);
                FCurrentViewData := btMonthlyChannelWaterBalance;
              end;
              otsAnnual:
              begin
                LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                           GetAnnualChannelWaterBalancelData(FIdentifier,FLoadCase,FSequence,
                           LSummaryValues,LError);
                FCurrentViewData := btAnnualChannelWaterBalancel;
              end;
             end;//case

            if LResult then
              PopulateChannelWaterBalance(LSummaryValues)
            else
              ViewDialog.ShowError(LError);
          finally
            LSummaryValues.Free;
          end;
        end;
        
        votChannelArea:
        begin
          LSummaryValues := TStringList.Create;
          try
            case FTimeStep of
              otsMonthly:
              begin
                LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                           GetChannelAreaMonthlyWaterBalancelData(FIdentifier,FLoadCase,FSequence,
                           FSequence,LSummaryValues,LError);
                FCurrentViewData := btMonthlyChannelWaterBalance;
              end;
              otsAnnual:
              begin
                LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                           GetChannelAreaAnnualWaterBalancelData(FIdentifier,FLoadCase,FSequence,
                           LSummaryValues,LError);
                FCurrentViewData := btAnnualChannelWaterBalancel;
              end;
             end;//case

            if LResult then
              PopulateChannelWaterBalance(LSummaryValues)
            else
              ViewDialog.ShowError(LError);
          finally
            LSummaryValues.Free;
          end;
        end;

        votWetland:
        begin
          LSummaryValues := TStringList.Create;
          LInflowValues  := TStringList.Create;
          LOutflowValues := TStringList.Create;
          LTotalsValues  := TStringList.Create;
          try
            LResult := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                       GetReservoirWaterBalancelData(FIdentifier,FLoadCase,FSequence,LSummaryValues,
                       LInflowValues,LOutflowValues,LTotalsValues,LError);
            FCurrentViewData := btReservoirWaterBalance;
            if LResult then
              PopulateReservoirWaterBalance(LSummaryValues,LInflowValues,LOutflowValues,LTotalsValues)
            else
              ViewDialog.ShowError(LError);
          finally
            LSummaryValues.Free;
            LInflowValues.Free;
            LOutflowValues.Free;
            LTotalsValues.Free;
          end;
        end;

      end;
    end;
    ViewDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputWaterBalanceValidator.PopulateChannelWaterBalance(AValues: TStrings);
const OPNAME = 'TOutputWaterBalanceValidator.PopulateChannelWaterBalance';
var
  LPos,
  LIndex         : integer;
  LName,
  LGroupName,
  LValue: string;
  lChannel       : IGeneralFlowChannel;
begin
  try
    ViewDialog.MainViewer.Visible := True;
    ViewDialog.DetailViewer.Visible := False;
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      LGroupName := lChannel.ChannelName + '('+ IntToStr(lChannel.ChannelNumber) + ')';
      ViewDialog.MainViewer.ObjectName := LGroupName;
      for LIndex := 0 to AValues.Count -1 do
      begin
        LPos := Pos('=',AValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(AValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(AValues.Strings[LIndex],LPos+1,Length(AValues.Strings[LIndex]));
          ViewDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctValue);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.PopulateReservoirWaterBalance(
          ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings);
const OPNAME = 'TOutputWaterBalanceValidator.PopulateReservoirWaterBalance';
var
  LPos,
  LIndex         : integer;
  LName,
  LGroupName,
  LValue: string;
  lReservoir     : IReservoirData;
begin
  try
    ViewDialog.MainViewer.Visible := True;
    ViewDialog.DetailViewer.Visible := ViewDialog.ShowDetail;
    lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (lReservoir <> nil) then
    begin
      LGroupName := lReservoir.ReservoirConfigurationData.ReservoirName + '('+
                    IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ')';

      ViewDialog.MainViewer.ObjectName := LGroupName;
      //summary
      for LIndex := 0 to ASummaryValues.Count -1 do
      begin
        LPos := Pos('=',ASummaryValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(ASummaryValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(ASummaryValues.Strings[LIndex],LPos+1,Length(ASummaryValues.Strings[LIndex]));
          if(LIndex = (ASummaryValues.Count -1)) then
            ViewDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctAggregate)
          else
            ViewDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctValue);
        end;
      end;
      //Inflow
      ViewDialog.DetailViewer.ObjectName := 'Mass Balance at '+LGroupName;
      for LIndex := 0 to AInflowValues.Count -1 do
      begin
        LPos := Pos('=',AInflowValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(AInflowValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(AInflowValues.Strings[LIndex],LPos+1,Length(AInflowValues.Strings[LIndex]));
          if(LIndex = (AInflowValues.Count -1)) then
            ViewDialog.DetailViewer.AddProperty('Inflow',LName,LValue,ctAggregate)
          else
            ViewDialog.DetailViewer.AddProperty('Inflow',LName,LValue,ctValue);
        end;
      end;
      //Outflow
      for LIndex := 0 to AOutflowValues.Count -1 do
      begin
        LPos := Pos('=',AOutflowValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(AOutflowValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(AOutflowValues.Strings[LIndex],LPos+1,Length(AOutflowValues.Strings[LIndex]));
          if(LIndex = (AOutflowValues.Count -1)) then
            ViewDialog.DetailViewer.AddProperty('Outflow',LName,LValue,ctAggregate)
          else
            ViewDialog.DetailViewer.AddProperty('Outflow',LName,LValue,ctValue);
        end;
      end;
      //Totals
      for LIndex := 0 to ATotalsValues.Count -1 do
      begin
        LPos := Pos('=',ATotalsValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(ATotalsValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(ATotalsValues.Strings[LIndex],LPos+1,Length(ATotalsValues.Strings[LIndex]));
          ViewDialog.DetailViewer.AddProperty('Results',LName,LValue,ctAggregate)
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputWaterBalanceValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdGraph,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


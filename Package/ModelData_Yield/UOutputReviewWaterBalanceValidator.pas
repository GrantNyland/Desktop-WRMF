//
//
//  UNIT      : Contains the class TOutputReviewWaterBalanceValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputReviewWaterBalanceValidator;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputReviewWaterBalanceDialog;

type
  TOutputReviewWaterBalanceValidator = class(TAbstractLoadCaseDialogValidator)
  protected
    FIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure RePopulateDataViewer;
    procedure PopulateChannelWaterBalance(AValues: TStrings);
    procedure PopulateReservoirWaterBalance(ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ShowSelectedLoadCase(ASelectedLoadCase: integer); override;
    function GetNextSignificantLoadCase(ACurrentLoadCase: integer): integer; override;
    function GetPreviousSignificantLoadCase(ACurrentLoadCase: integer): integer; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;

    function WaterBalanceDialog: TOutputReviewWaterBalanceDialog;
    property Identifier: integer read FIdentifier write FIdentifier;

  end;

implementation

uses
  SysUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TOutputReviewWaterBalanceValidator }

procedure TOutputReviewWaterBalanceValidator.CreateMemberObjects;
const OPNAME = 'TOutputReviewWaterBalanceValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FViewObjectType := votNone;
    FPanel := TOutputReviewWaterBalanceDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.DestroyMemberObjects;
const OPNAME = 'TOutputReviewWaterBalanceValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.Initialise: boolean;
const OPNAME = 'TOutputReviewWaterBalanceValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    WaterBalanceDialog.LoadCaseNavigator.AddValidator(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputReviewWaterBalanceValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.ReviewWaterBalance');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.ClearDataViewer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.PopulateDataViewer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.RePopulateDataViewer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.RePopulateDataViewer';
var
  LLoadCaseCount: integer;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LLoadCaseCount := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.SummaryOutputData.LoadCaseCount;
      WaterBalanceDialog.LoadCaseNavigator.LoadCaseCount := LLoadCaseCount;
      if(LLoadCaseCount > 0) then
        WaterBalanceDialog.LoadCaseNavigator.CurrentLoadCase := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.SaveState: boolean;
const OPNAME = 'TOutputReviewWaterBalanceValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.WaterBalanceDialog : TOutputReviewWaterBalanceDialog;
const OPNAME = 'TOutputReviewWaterBalanceValidator.WaterBalanceDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputReviewWaterBalanceDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputReviewWaterBalanceValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputReviewWaterBalanceValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TOutputReviewWaterBalanceValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TOutputReviewWaterBalanceValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.GetNextSignificantLoadCase(ACurrentLoadCase: integer): integer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.GetNextSignificantLoadCase';
begin
  Result := 0;
  try
    Result := ACurrentLoadCase  + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceValidator.GetPreviousSignificantLoadCase(ACurrentLoadCase: integer): integer;
const OPNAME = 'TOutputReviewWaterBalanceValidator.GetPreviousSignificantLoadCase';
begin
  Result := 0;
  try
    Result := ACurrentLoadCase  - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.ShowSelectedLoadCase(ASelectedLoadCase: integer);
const OPNAME = 'TOutputReviewWaterBalanceValidator.ShowSelectedLoadCase';
var
  //lChannel       : IGeneralFlowChannel;
  //lReservoir     : IReservoirConfigurationData;
  LTotalsValues,
  LSummaryValues,
  LInflowValues,
  LOutflowValues : TStringList;
  LError         : string;
begin
  try
    WaterBalanceDialog.MainViewer.Initialise;
    WaterBalanceDialog.DetailViewer.Initialise;
    FErrorMessage := '';
    WaterBalanceDialog.ShowError(FErrorMessage);
    if(FIdentifier >= 0) then
    begin
      case FViewObjectType of
        votReservoir:
        begin
          LSummaryValues := TStringList.Create;
          LInflowValues  := TStringList.Create;
          LOutflowValues := TStringList.Create;
          LTotalsValues  := TStringList.Create;
          try
            if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.SummaryOutputData.PerformAvarageWaterBalance(
               FIdentifier,ASelectedLoadCase,LSummaryValues,LInflowValues,LOutflowValues,LTotalsValues,LError) then
              PopulateReservoirWaterBalance(LSummaryValues,LInflowValues,LOutflowValues,LTotalsValues)
            else
              WaterBalanceDialog.ShowError(LError);

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
            if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.SummaryOutputData.GetWaterBalanceAvarageChanneFlowData(
               FIdentifier,ASelectedLoadCase,LSummaryValues,LError) then
              PopulateChannelWaterBalance(LSummaryValues)
            else
              WaterBalanceDialog.ShowError(LError);
          finally
            LSummaryValues.Free;
          end;
        end;
      end;
    end;
    WaterBalanceDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputReviewWaterBalanceValidator.PopulateChannelWaterBalance(AValues: TStrings);
const OPNAME = 'TOutputReviewWaterBalanceValidator.PopulateChannelWaterBalance';
var
  LPos,
  LIndex         : integer;
  LName,
  LGroupName,
  LValue: string;
  lChannel       : IGeneralFlowChannel;
begin
  try
    WaterBalanceDialog.MainViewer.Visible := True;
    WaterBalanceDialog.DetailViewer.Visible := False;
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      LGroupName := lChannel.ChannelName + '('+ IntToStr(lChannel.ChannelNumber) + ')';
      WaterBalanceDialog.MainViewer.ObjectName := LGroupName;
      for LIndex := 0 to AValues.Count -1 do
      begin
        LPos := Pos('=',AValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(AValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(AValues.Strings[LIndex],LPos+1,Length(AValues.Strings[LIndex]));
          WaterBalanceDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctValue);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceValidator.PopulateReservoirWaterBalance(
          ASummaryValues,AInflowValues,AOutflowValues,ATotalsValues: TStrings);
const OPNAME = 'TOutputReviewWaterBalanceValidator.PopulateReservoirWaterBalance';
var
  LPos,
  LIndex         : integer;
  LName,
  LGroupName,
  LValue: string;
  lReservoir     : IReservoirData;
begin
  try
    WaterBalanceDialog.MainViewer.Visible := True;
    WaterBalanceDialog.DetailViewer.Visible := WaterBalanceDialog.ShowDetail;
    lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (lReservoir <> nil) then
    begin
      LGroupName := lReservoir.ReservoirConfigurationData.ReservoirName + '('+
                    IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ')';

      WaterBalanceDialog.MainViewer.ObjectName := LGroupName;
      //summary
      for LIndex := 0 to ASummaryValues.Count -1 do
      begin
        LPos := Pos('=',ASummaryValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(ASummaryValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(ASummaryValues.Strings[LIndex],LPos+1,Length(ASummaryValues.Strings[LIndex]));
          if(LIndex = (ASummaryValues.Count -1)) then
            WaterBalanceDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctAggregate)
          else
            WaterBalanceDialog.MainViewer.AddProperty(LGroupName,LName,LValue,ctValue);
        end;
      end;
      //Inflow
      WaterBalanceDialog.DetailViewer.ObjectName := 'Mass Balance at '+LGroupName;
      for LIndex := 0 to AInflowValues.Count -1 do
      begin
        LPos := Pos('=',AInflowValues.Strings[LIndex]);
        if(LPos > 1) then
        begin
          LName  := Copy(AInflowValues.Strings[LIndex],1,LPos-1);
          LValue := Copy(AInflowValues.Strings[LIndex],LPos+1,Length(AInflowValues.Strings[LIndex]));
          if(LIndex = (AInflowValues.Count -1)) then
            WaterBalanceDialog.DetailViewer.AddProperty('Inflow',LName,LValue,ctAggregate)
          else
            WaterBalanceDialog.DetailViewer.AddProperty('Inflow',LName,LValue,ctValue);
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
            WaterBalanceDialog.DetailViewer.AddProperty('Outflow',LName,LValue,ctAggregate)
          else
            WaterBalanceDialog.DetailViewer.AddProperty('Outflow',LName,LValue,ctValue);
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
          WaterBalanceDialog.DetailViewer.AddProperty('Results',LName,LValue,ctAggregate)
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


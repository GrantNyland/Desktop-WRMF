{******************************************************************************}
{*  UNIT      : Contains the class TChannelGrowthValidator   *}
{*  AUTHOR    : Sam M. Dhlamini                                                 *}
{*  DATE      : 2013/11/29                                                    *}
{*  COPYRIGHT : Copyright © 2013 DWAF                                         *}
{******************************************************************************}

unit UChannelGrowthValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UGrowthFactorData,
   UYieldModelDataObject,
  UPlanningModelDataObject,
  UGrowthFactorsExcelData,
  UChannelGrowthDialog;

type

  TChannelGrowthValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure DoGraphTypeClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateMinMaxDemands(AGrowthFactors : TGrowthFactors;ADemandProjection : TExelGrowthFactors; AMinMaxGrowthFactors : IMinMaxChannelGrowthFactors; AStartYear, AChannelNo : integer);
    procedure PopulateFactors(AFactors : TStringList;AFieldProperty : TAbstractFieldProperty; AArcNumber,AStartYear : integer);
    procedure UpdateGrowthFactors(ACol, ARow: integer);
    procedure UpdateDemandGrowthFactors(ACol, ARow, AArcNum : integer);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function ChannelGrowthDialog: TChannelGrowthDialog;
  end;

implementation

uses
  VCL.Dialogs,
  SysUtils,
  VCL.Graphics,
  Contnrs,
  UFileNames,
  UUtilities,
  UConstants,
  UAbstractFileNamesObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations,
  UParameterData;

{******************************************************************************}
{* TChannelGrowthValidator                                 *}
{******************************************************************************}

procedure TChannelGrowthValidator.CreateMemberObjects;
const OPNAME = 'TChannelGrowthValidator.CreateMemberObjects';
var
  lpPanel : TChannelGrowthDialog;
begin
  try
    inherited CreateMemberObjects;
    Identifier := -1;
    FPanel  := TChannelGrowthDialog.Create(FPanelOwner,FAppModules);
    lpPanel := ChannelGrowthDialog;
    with lpPanel do
    begin
    //  strgrdFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisbenefitEscalationRate'));
      strgrdFactors.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      strgrdFactors.OnColEnter         := OnStringGridColEnter;
      GraphType.OnClick   := DoGraphTypeClick;
      ArcRadioGroup.OnClick   := DoGraphTypeClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.DestroyMemberObjects;
const OPNAME = 'TChannelGrowthValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthValidator.Initialise: boolean;
const OPNAME = 'TChannelGrowthValidator.Initialise';
var
  LChannel : IGeneralFlowChannel;
  LPenaltyStructure : IChannelPenalty;
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    if (Identifier >= 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        LPenaltyStructure := LChannel.ChannelPenalty;
        if LPenaltyStructure <> nil then
        begin
          with ChannelGrowthDialog do
          begin
            ArcRadioGroup.Visible := (LPenaltyStructure.ChannelPenaltyArcCount > 0 );
            ArcRadioGroup.Columns := LPenaltyStructure.ChannelPenaltyArcCount;
            for LIndex := 1 to  LPenaltyStructure.ChannelPenaltyArcCount do
              ArcRadioGroup.Items.Add('Arc '+IntToStr(LIndex));
            ArcRadioGroup.ItemIndex := 0;
            GraphType.ItemIndex := 0;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelGrowthValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Growth Factors';//FAppModules.Language.GetString('TDisbenefitDefinitionDataDialog.DefinitionDataDialog');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.DoGraphTypeClick(Sender: TObject);
const OPNAME = 'TChannelGrowthValidator.DoGraphTypeClick';
begin
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TChannelGrowthValidator.ClearDataViewer;
const OPNAME = 'TChannelGrowthValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  try
    inherited ClearDataViewer;
    with ChannelGrowthDialog do
    begin
      for LIndex := 1 to strgrdFactors.RowCount - 1 do
        strgrdFactors.Rows[LIndex].Clear;
      GrowthGraph.SeriesList.ClearValues;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.PopulateDataViewer;
const OPNAME = 'TChannelGrowthValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedDemandFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.PopulateMinMaxDemands(AGrowthFactors : TGrowthFactors;ADemandProjection : TExelGrowthFactors; AMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;AStartYear,AChannelNo : integer);
const OPNAME = 'TChannelGrowthValidator.PopulateMinMaxDemands';
var
  LMinMaxDemands : TExelMinMaxChannelGrowthFactors;
  LMinMaxGrowthFactors : TMinMaxChannelGrowthFactors;
  LArc : integer;
  LFactors : TStringList;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    if (AGrowthFactors <> nil) and (AMinMaxGrowthFactors <> nil) and (ADemandProjection <> nil) then
    begin
      LArc := ChannelGrowthDialog.ArcRadioGroup.ItemIndex+1;
      LMinMaxGrowthFactors := AGrowthFactors.CastMinMaxChannelGrowthFactorsByChannelArc(AChannelNo,LArc);
      LMinMaxDemands := ADemandProjection.GetExelMinMaxChannelGrowthFactorsByCannelArc(LArc,AChannelNo);
      if (LMinMaxGrowthFactors <> nil) or (LMinMaxDemands <> nil) then
      begin
        LFactors := TStringList.Create;
        try
          if (LMinMaxDemands <> nil) and (ChannelGrowthDialog.GraphType.ItemIndex = 1) then
          begin
            LFactors.CommaText := LMinMaxDemands.GrowthFactors;
            LFieldProperty := FAppModules.FieldProperties.FieldProperty('ExcelMinMaxGrowthFactors');
            PopulateFactors(LFactors,LFieldProperty, 1, AStartYear);
          end
          else
          if  (ChannelGrowthDialog.GraphType.ItemIndex = 0) then
          begin
            LFactors.CommaText := LMinMaxGrowthFactors.GrowthFactors;
            LFieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxGrowthFactors');
            PopulateFactors(LFactors,LFieldProperty, 1, AStartYear);
          end;
        finally
          LFactors.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TChannelGrowthValidator.PopulateFactors(AFactors : TStringList;AFieldProperty : TAbstractFieldProperty;AArcNumber,AStartYear : integer);
const OPNAME = 'TChannelGrowthValidator.PopulateFactors';
var
  LIndex : integer;
  LDate : TDate;
  LSeriesValue : double;

begin
  try
    if (AFactors <> nil) and (AFieldProperty <> nil) then
    begin
      for LIndex := 0 to AFactors.count-1 do
      begin
        ChannelGrowthDialog.strgrdFactors.Width := 3 + (1 + ChannelGrowthDialog.strgrdFactors.DefaultColWidth) * ChannelGrowthDialog.strgrdFactors.ColCount;
        ChannelGrowthDialog.strgrdFactors.Cells[LIndex,0] := IntToStr(AStartYear+LIndex);
        if (ChannelGrowthDialog.GraphType.ItemIndex = 0) then
        begin
          ChannelGrowthDialog.strgrdFactors.Cells[lIndex,AArcNumber] := Format(AFieldProperty.FormatStringGrid, [StrToFloat(AFactors[LIndex])]);
          LSeriesValue := StrToFloat(AFactors[LIndex]);
          ChannelGrowthDialog.FactorLabel.Caption := 'Growth Factor';
          ChannelGrowthDialog.strgrdFactors.AddFieldProperty(AFieldProperty);
          LDate  := EncodeDate(AStartYear+LIndex, 10,1);
          ChannelGrowthDialog.FactorsLineSeries.AddXY(LDate, LSeriesValue);
        end else
        if (ChannelGrowthDialog.GraphType.ItemIndex = 1) then
        begin
          LSeriesValue := StrToFloat(AFactors[LIndex]);
          ChannelGrowthDialog.FactorLabel.Caption := 'Demand Projection';
          ChannelGrowthDialog.strgrdFactors.Cells[lIndex, AArcNumber] := Format(AFieldProperty.FormatStringGrid, [StrToFloat(AFactors[LIndex])]);
          ChannelGrowthDialog.strgrdFactors.AddFieldProperty(AFieldProperty);
          LDate  := EncodeDate(AStartYear+LIndex, 10,1);
          ChannelGrowthDialog.DemandLineSeries.AddXY(LDate, LSeriesValue);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.RePopulateDataViewer;
const OPNAME = 'TChannelGrowthValidator.RePopulateDataViewer';
var
  LGrowthFactors : TGrowthFactors;
  LRunConfig : IRunConfigurationData;
  LChannel : IGeneralFlowChannel;
  LDemandCentreGrowthFactors : IDemandCentreGrowthFactors;
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LDemandProjection : TExelGrowthFactors;
  LDemandGrowthFactors : TExelDemandChannelGrowthFactors;
  LStartYear: integer;
  LFactors : TStringList;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    ChannelGrowthDialog.ArcRadioGroup.Visible := False;
    if (Identifier >= 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
        LDemandProjection := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
        LDemandCentreGrowthFactors := LGrowthFactors.DemandGrowthFactorsByChannel[LChannel.ChannelNumber];
        LMinMaxGrowthFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[LChannel.ChannelNumber];
        LDemandGrowthFactors := LDemandProjection.GetDemandChannelGrowthFactorsByChannel(LChannel.ChannelNumber);
        if ((LDemandCentreGrowthFactors <> nil) or (LMinMaxGrowthFactors <> nil) ) or
           ((LDemandGrowthFactors <> nil) ) then
        begin
          LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
          LRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
          ChannelGrowthDialog.strgrdFactors.ColCount := LGrowthFactors.NumberOfYears;
          ChannelGrowthDialog.strgrdFactors.ClearFieldProperties;
          if (LRunConfig <> nil) and (LGrowthFactors <> nil) then
          begin
            LStartYear := LRunConfig.HistoricSequenceStartYear;
            LFactors := TStringList.Create;
            try
              if (LDemandCentreGrowthFactors <> nil) and (ChannelGrowthDialog.GraphType.ItemIndex = 0) then
              begin
                LFactors.CommaText :=  LDemandCentreGrowthFactors.GrowthFactors;
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('DemandGrowthFactors');
                PopulateFactors(LFactors, LFieldProperty, 1, LStartYear);
              end else
              if  (LDemandGrowthFactors <> nil) and  (ChannelGrowthDialog.GraphType.ItemIndex = 1) then
              begin
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('ExcelDemandGrowthFactors');
                LFactors.CommaText :=  LDemandGrowthFactors.GrowthFactors;
                PopulateFactors(LFactors, LFieldProperty,1, LStartYear);
              end else
              if (LMinMaxGrowthFactors <> nil) then
              begin
                ChannelGrowthDialog.ArcRadioGroup.Visible := True;
                PopulateMinMaxDemands(LGrowthFactors,LDemandProjection,LMinMaxGrowthFactors,LStartYear, lChannel.ChannelNumber);
              end;
              finally
                LFactors.Free;
              end;
            end;
          end;
        end;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TChannelGrowthValidator.SaveState: boolean;
const OPNAME = 'TChannelGrowthValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthValidator.ChannelGrowthDialog : TChannelGrowthDialog;
const OPNAME = 'TChannelGrowthValidator.ChannelGrowthDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TChannelGrowthDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TChannelGrowthValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelGrowthValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelGrowthValidator.StudyHasChanged';
var
  LChannel : IGeneralFlowChannel;
  LPenaltyStructure : IChannelPenalty;
  LIndex : integer;
begin
  Result := inherited StudyHasChanged;
  try
    if (Identifier >= 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        LPenaltyStructure := LChannel.ChannelPenalty;
        if LPenaltyStructure <> nil then
        begin

          with ChannelGrowthDialog do
          begin
            ArcRadioGroup.Visible := (LPenaltyStructure.ChannelPenaltyArcCount > 0 );
            ArcRadioGroup.Columns := LPenaltyStructure.ChannelPenaltyArcCount;
            for LIndex := 1 to  LPenaltyStructure.ChannelPenaltyArcCount do
              ArcRadioGroup.Items.Add('Arc '+IntToStr(LIndex));
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChannelGrowthValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChannelGrowthValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ChannelGrowthDialog do
    begin

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.OnStringGridCellDataHasChanged;
const OPNAME = 'TChannelGrowthValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ChannelGrowthDialog do
    begin
      if (strgrdFactors = ASender) then
        UpdateGrowthFactors(ACol, ARow);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelGrowthValidator.UpdateGrowthFactors(ACol, ARow: integer);
const OPNAME = 'TChannelGrowthValidator.UpdateGrowthFactors';
var
  LMessage,
  LFieldValue : string;
  LValue : double;
  LGrowthFactors : TGrowthFactors;
  LChannel : IGeneralFlowChannel;
  LDemandCentreGrowthFactors : IDemandCentreGrowthFactors;
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
begin
  try
    if (Identifier >= 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
        LDemandCentreGrowthFactors := LGrowthFactors.DemandGrowthFactorsByChannel[LChannel.ChannelNumber];

        LMinMaxGrowthFactors := LGrowthFactors.CastMinMaxChannelGrowthFactorsByChannelArc(LChannel.ChannelNumber,ChannelGrowthDialog.ArcRadioGroup.ItemIndex+1);
        ChannelGrowthDialog.strgrdFactors.ValidationError[ACol, ARow, gveCellContext] :='';
        LFieldValue := ChannelGrowthDialog.strgrdFactors.Cells[ACol,ARow];
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);
        if (ChannelGrowthDialog.GraphType.ItemIndex = 0) then
        begin
          if (LDemandCentreGrowthFactors <> nil) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty('DemandGrowthFactors', LFieldValue, LMessage)) then
            begin
              LDemandCentreGrowthFactors.GrowthFactorsValueByIndex[ACol] := LValue;
              RepopulateDataViewer;
            end
            else
              ChannelGrowthDialog.strgrdFactors.ValidationError[ACol, ARow, gveCellContext] := LMessage;
          end
          else
          if (LMinMaxGrowthFactors <> nil) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty('MinMaxGrowthFactors', LFieldValue, LMessage, ACol)) then
            begin
              LMinMaxGrowthFactors.GrowthFactorsValueByIndex[ACol] := LValue;
              RepopulateDataViewer;
            end
            else
              ChannelGrowthDialog.strgrdFactors.ValidationError[ACol,ARow, gveCellContext] := LMessage;
          end;
        end else
        if (ChannelGrowthDialog.GraphType.ItemIndex = 1) then
          UpdateDemandGrowthFactors(ACol, ARow,ChannelGrowthDialog.ArcRadioGroup.ItemIndex+1);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelGrowthValidator.UpdateDemandGrowthFactors(ACol, ARow,AArcNum : integer);
const OPNAME = 'TChannelGrowthValidator.UpdateGrowthFactors';
var
  LRowData: TStringList;
  LGrowthFactors: TExelGrowthFactors;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LChannelNumber : integer;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LChannel : IGeneralFlowChannel;
begin
  try
   if (Identifier >= 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin
        if (ARow <= 0) or (ARow >= ChannelGrowthDialog.strgrdFactors.RowCount)then Exit;
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
        LChannelNumber := LChannel.ChannelNumber;
        LMinMaxChannelFactors := LGrowthFactors.GetExelMinMaxChannelGrowthFactorsByCannelArc(AArcNum,LChannelNumber);
        LDemandChannelFactors := LGrowthFactors.GetDemandChannelGrowthFactorsByChannel(LChannelNumber);
        LRowData := TStringList.Create;
        try
          if Assigned(LDemandChannelFactors) then
          begin
            LRowData.Assign(ChannelGrowthDialog.strgrdFactors.Rows[ARow]);
            LDemandChannelFactors.GrowthFactors := LRowData.CommaText;
          end;
          if Assigned(LMinMaxChannelFactors) then
          begin
            LRowData.Assign(ChannelGrowthDialog.strgrdFactors.Rows[ARow]);
            LMinMaxChannelFactors.GrowthFactors := LRowData.CommaText;
          end;
        finally
          LRowData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelGrowthValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TChannelGrowthValidator.DoContextValidation';
var
  lChannelList          : IChannelList;
  lChannel              : IGeneralFlowChannel;
begin
  try
    FAllErrorMessages.Clear;
    if (Identifier > 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
          NetworkElementData.ChannelList;
      lChannel := lChannelList.ChannelByChannelNumber[Identifier];
      if (lChannel <> nil) then
      begin

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


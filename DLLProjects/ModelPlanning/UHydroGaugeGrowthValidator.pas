unit UHydroGaugeGrowthValidator;
{******************************************************************************}
{*  UNIT      : Contains the class THydroGaugeGrowthValidator   *}
{*  AUTHOR    : Sam M. Dhlamini                                                 *}
{*  DATE      : 2014/01/31                                                    *}
{*  COPYRIGHT : Copyright © 2014 DWAF                                         *}
{******************************************************************************}
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
  UHydroGaugeGrowthDialog;

type

  THydroGaugeGrowthValidator = class(TAbstractYieldDataDialogValidator)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure DoGraphTypeClick(Sender: TObject);
    procedure DoListClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure PopulateGridGraphDataViewer;
    procedure PopulateGridGraphData(AFactors : TStringList;AFieldProperty : TAbstractFieldProperty);


  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function HydroGaugeGrowthDialog: THydroGaugeGrowthDialog;
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
{* THydroGaugeGrowthValidator                                 *}
{******************************************************************************}

procedure THydroGaugeGrowthValidator.CreateMemberObjects;
const OPNAME = 'THydroGaugeGrowthValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Identifier := -1;
    FPanel  := THydroGaugeGrowthDialog.Create(FPanelOwner,FAppModules);
    HydroGaugeGrowthDialog.GaugeListBox.OnClick := DoListClick;
    HydroGaugeGrowthDialog.FileRadioGroup.OnClick := DoListClick;
    HydroGaugeGrowthDialog.GrowthTypeRadioGroup.OnClick := DoListClick;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.DestroyMemberObjects;
const OPNAME = 'THydroGaugeGrowthValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.Initialise: boolean;
const OPNAME = 'THydroGaugeGrowthValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.LanguageHasChanged: boolean;
const OPNAME = 'THydroGaugeGrowthValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Growth Factors';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.DoGraphTypeClick(Sender: TObject);
const OPNAME = 'THydroGaugeGrowthValidator.DoGraphTypeClick';
begin
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.DoListClick(Sender: TObject);
const OPNAME = 'THydroGaugeGrowthValidator.DoGraphTypeClick';
begin
  try
    ClearDataViewer;
    PopulateGridGraphDataViewer;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure THydroGaugeGrowthValidator.ClearDataViewer;
const OPNAME = 'THydroGaugeGrowthValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with HydroGaugeGrowthDialog do
    begin
      for LIndex := 1 to strgrdFactors.RowCount - 1 do
        strgrdFactors.Rows[LIndex].Clear;
      GrowthGraph.SeriesList.ClearValues;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.PopulateDataViewer;
const OPNAME = 'THydroGaugeGrowthValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedDemandFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.RePopulateDataViewer;
const OPNAME = 'THydroGaugeGrowthValidator.RePopulateDataViewer';
var
  LParamReference : IParamReference;
  LIndex : integer;
begin
  try
    for LIndex  := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceCount-1 do
    begin
      LParamReference  := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceDataByIndex[LIndex];
      if LParamReference <> nil then
      begin
        HydroGaugeGrowthDialog.GaugeListBox.AddItem(IntToStr(LParamReference.CatchReference),TObject(LParamReference.CatchReference));
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.PopulateGridGraphDataViewer;
const OPNAME = 'THydroGaugeGrowthValidator.PopulateGridGraphDataViewer';
var
  LGauge : integer;
  LGrowthFactors : TGrowthFactors;
  LDemandProjections : TExelGrowthFactors;
  LHydrologyGrowthFactors : THydrologyGrowthFactors;
  LExelHydrologyGrowthFactors: TExelHydrologyGrowthFactors;
  LFactors : TStringList;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    if HydroGaugeGrowthDialog.GaugeListBox.Items.Count>0 then
    begin
      LFactors := TStringList.Create;
      if HydroGaugeGrowthDialog.GaugeListBox.ItemIndex>0 then
        LGauge := integer(HydroGaugeGrowthDialog.GaugeListBox.Items.Objects[HydroGaugeGrowthDialog.GaugeListBox.ItemIndex])
      else
      begin
        LGauge := integer(HydroGaugeGrowthDialog.GaugeListBox.Items.Objects[0]);
        HydroGaugeGrowthDialog.GaugeListBox.Selected[0] := True;
      end;

      LDemandProjections :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
      LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
      LFieldProperty := nil;
      try
        case HydroGaugeGrowthDialog.GrowthTypeRadioGroup.ItemIndex of
          0:
          begin


            if (LGrowthFactors <>nil) and (LGauge>0)then
            begin
              LHydrologyGrowthFactors :=  LGrowthFactors.CastHydrologyGrowthFactorByGaugeNumber(LGauge);
              if (LHydrologyGrowthFactors <> nil) then
              begin
                case HydroGaugeGrowthDialog.FileRadioGroup.ItemIndex of
                  0:
                  begin
                    LFactors.CommaText := LHydrologyGrowthFactors.AFFGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('AFFGrowthFactors');
                  end;
                  1:
                  begin
                    LFactors.CommaText := LHydrologyGrowthFactors.IRRGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('IRRGrowthFactors');
                  end;
                  2:
                  begin
                    LFactors.CommaText := LHydrologyGrowthFactors.URBGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('URBGrowthFactors');
                  end;
                end;
                 HydroGaugeGrowthDialog.FactorLabel.Caption := 'Growth Factors';

              end;
            end;
          end;
          1:
          begin

            if (LDemandProjections <> nil) and (LGauge>0) then
            begin
              LExelHydrologyGrowthFactors :=  LDemandProjections.CastHydrologyGrowthFactorByGaugeNumber(LGauge);
              if (LExelHydrologyGrowthFactors <> nil) then
              begin
                case HydroGaugeGrowthDialog.FileRadioGroup.ItemIndex of
                  0:
                  begin
                    LFactors.CommaText := LExelHydrologyGrowthFactors.AFFGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ExcelAFFGrowthFactors');
                  end;
                  1:
                  begin
                    LFactors.CommaText := LExelHydrologyGrowthFactors.IRRGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ExcelIRRGrowthFactors');
                  end;
                  2:
                  begin
                    LFactors.CommaText := LExelHydrologyGrowthFactors.URBGrowthFactors;
                    LFieldProperty := FAppModules.FieldProperties.FieldProperty('ExcelURBGrowthFactors');
                  end;
                end;
                 HydroGaugeGrowthDialog.FactorLabel.Caption := 'Demand Projection';

              end;
            end;
          end;
        end;
        HydroGaugeGrowthDialog.strgrdFactors.ColCount := LGrowthFactors.NumberOfYears;
        HydroGaugeGrowthDialog.strgrdFactors.ClearFieldProperties;
        if (LFactors.Count>0) and (LFieldProperty <> nil) then
          PopulateGridGraphData(LFactors,LFieldProperty);
      finally
        LFactors.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.PopulateGridGraphData(AFactors : TStringList;AFieldProperty : TAbstractFieldProperty);
const OPNAME = 'THydroGaugeGrowthValidator.PopulateGridGraphData';
var
  LIndex : integer;
  LDate : TDate;
  LSeriesValue : double;
  LRunConfig : IRunConfigurationData;
  LStartYear : integer;
begin
  try
    LRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LRunConfig <> nil) then
    begin
      LStartYear := LRunConfig.HistoricSequenceStartYear;
      if (AFactors <> nil) and (AFieldProperty <> nil) then
      begin
        for LIndex := 0 to AFactors.count-1 do
        begin
          HydroGaugeGrowthDialog.strgrdFactors.Width := 3 + (1 + HydroGaugeGrowthDialog.strgrdFactors.DefaultColWidth) * HydroGaugeGrowthDialog.strgrdFactors.ColCount;
          HydroGaugeGrowthDialog.strgrdFactors.Cells[LIndex,0] := IntToStr(LStartYear+LIndex);
          LSeriesValue := StrToFloat(AFactors[LIndex]);
          HydroGaugeGrowthDialog.strgrdFactors.Cells[lIndex, 1] := Format(AFieldProperty.FormatStringGrid, [StrToFloat(AFactors[LIndex])]);
          HydroGaugeGrowthDialog.strgrdFactors.AddFieldProperty(AFieldProperty);

          LDate  := EncodeDate(LStartYear+LIndex, 10,1);
          if HydroGaugeGrowthDialog.GrowthTypeRadioGroup.ItemIndex = 0 then
            HydroGaugeGrowthDialog.FactorsLineSeries.AddXY(LDate, LSeriesValue)
          else
            HydroGaugeGrowthDialog.DemandLineSeries.AddXY(LDate, LSeriesValue);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.SaveState: boolean;
const OPNAME = 'THydroGaugeGrowthValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.HydroGaugeGrowthDialog : THydroGaugeGrowthDialog;
const OPNAME = 'THydroGaugeGrowthValidator.HydroGaugeGrowthDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := THydroGaugeGrowthDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'THydroGaugeGrowthValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroGaugeGrowthValidator.StudyHasChanged: boolean;
const OPNAME = 'THydroGaugeGrowthValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'THydroGaugeGrowthValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'THydroGaugeGrowthValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with HydroGaugeGrowthDialog do
    begin

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroGaugeGrowthValidator.OnStringGridCellDataHasChanged;
const OPNAME = 'THydroGaugeGrowthValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;
 
procedure THydroGaugeGrowthValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'THydroGaugeGrowthValidator.DoContextValidation';
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


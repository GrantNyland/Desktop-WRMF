unit UGrowthFactorsGraphValidator;
{******************************************************************************}
{*  UNIT      : Contains the class TGrowthFactorsGraphValidator   *}
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
  UGrowthFactorsGraphDialog;

type

  TGrowthFactorsGraphValidator = class(TAbstractYieldDataDialogValidator)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;

    procedure DoGrowthTypeClick(Sender: TObject);
    procedure RePopulateDataViewer;


  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function GrowthFactorsGraphDialog: TGrowthFactorsGraphDialog;
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
{* TGrowthFactorsGraphValidator                                 *}
{******************************************************************************}

procedure TGrowthFactorsGraphValidator.CreateMemberObjects;
const OPNAME = 'TGrowthFactorsGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Identifier := -1;
    FPanel  := TGrowthFactorsGraphDialog.Create(FPanelOwner,FAppModules);
    GrowthFactorsGraphDialog.GrowthTypeRadioGroup.OnClick := DoGrowthTypeClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorsGraphValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphValidator.Initialise: boolean;
const OPNAME = 'TGrowthFactorsGraphValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthFactorsGraphValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Growth Demands Graph';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.DoGrowthTypeClick(Sender: TObject);
const OPNAME = 'TGrowthFactorsGraphValidator.DoGrowthTypeClick';
begin
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TGrowthFactorsGraphValidator.ClearDataViewer;
const OPNAME = 'TGrowthFactorsGraphValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with GrowthFactorsGraphDialog do
    begin
      GrowthGraph.SeriesList.ClearValues;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.PopulateDataViewer;
const OPNAME = 'TGrowthFactorsGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtSpecifiedDemandFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.RePopulateDataViewer;
const OPNAME = 'TGrowthFactorsGraphValidator.RePopulateDataViewer';
var
  LData : TStringList;
  LProjectionFactors : TExelGrowthFactors;
  LGrowthFactors : TGrowthFactors;
  LDemandGrowthFactors:TExelDemandChannelGrowthFactors;
  LDemandCentresGrowthFactors : IDemandCentreGrowthFactors;
  LIndex : integer;
  LCount : integer;
  LTempData : TStringList;

  LDate : TDate;
  LSeriesValue : double;
  LRunConfig : IRunConfigurationData;
  LStartYear : integer;
begin
  try

    LRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if LRunConfig <> nil then
    begin
      LStartYear := LRunConfig.HistoricSequenceStartYear;
      LData := TStringList.Create;
      LTempData := TStringList.Create;
      try
        case GrowthFactorsGraphDialog.GrowthTypeRadioGroup.ItemIndex of
          0:
          begin
            LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;

            if (LGrowthFactors <> nil) then
            begin
              LData.Clear;

              for LIndex := 0 to LGrowthFactors.DemandCentresGrowthFactorsCount-1 do
              begin
                LDemandCentresGrowthFactors := LGrowthFactors.DemandCentresGrowthFactorByIndex[LIndex];
                LTempData.CommaText := LDemandCentresGrowthFactors.GrowthFactors;
                GrowthFactorsGraphDialog.GrowthGraph.Canvas.Font.Style := [fsBold];
                //GrowthFactorsGraphDialog.GrowthGraph.BottomAxis.Title.Caption := 'Demand Growth Factors';
                GrowthFactorsGraphDialog.GrowthGraph.Title.Text.Text := 'Factors(Demand Projection/Base Demand)-1';
                if LIndex = 0 then
                begin
                  LData.CommaText := LTempData.CommaText;
                  Continue;
                end;
                for LCount := 0 to LTempData.Count-1 do
                begin

                  if (Trim(LData[LCount]) <> '') and (Trim(LTempData[LCount]) <> '') then
                  begin
                    LSeriesValue := StrToFloat(LData[LCount])+StrToFloat(LTempData[LCount]);
                    LData[LCount] := FloatToStr(LSeriesValue);
                  end;
                end;

                LTempData.Clear;
              end;

              for LIndex := 0 to LData.Count-1 do
              begin
                LSeriesValue := StrToFloat(LData[LIndex]);
                LDate  := EncodeDate(LStartYear+LIndex, 10,1);
                GrowthFactorsGraphDialog.FactorsLineSeries.AddXY(LDate, LSeriesValue)
              end;
            end;
          end;
          1:
          begin
            LProjectionFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
            GrowthFactorsGraphDialog.GrowthGraph.Title.Text.Text := 'Demands (Mm 3/a)';
            if (LProjectionFactors <> nil) then
            begin
              LData.Clear;

              for LIndex := 0 to LProjectionFactors.DemandChannelGrowthFactorsCount-1 do
              begin
                LDemandGrowthFactors := LProjectionFactors.DemandChannelGrowthFactorsByIndex[LIndex];
                LTempData.CommaText := LDemandGrowthFactors.GrowthFactors;
                if LIndex = 0 then
                begin
                  LData.CommaText := LTempData.CommaText;
                  Continue;
                end;
                for LCount := 0 to LTempData.Count-1 do
                begin

                  if (trim(LData[LCount]) <> '') and (Trim(LTempData[LCount]) <> '') then
                  begin
                    LSeriesValue := StrToFloat(LData[LCount])+StrToFloat(LTempData[LCount]);
                    LData[LCount] := FloatToStr(LSeriesValue);
                  end;
                end;

                LTempData.Clear;
              end;

              for LIndex := 0 to LData.Count-1 do
              begin
                LSeriesValue := StrToFloat(LData[LIndex]);
                LDate  := EncodeDate(LStartYear+LIndex, 10,1);
                GrowthFactorsGraphDialog.DemandLineSeries.AddXY(LDate, LSeriesValue)
              end;
            end;
          end;
        end;
      finally
        LTempData.Free;
        LData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGrowthFactorsGraphValidator.SaveState: boolean;
const OPNAME = 'TGrowthFactorsGraphValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphValidator.GrowthFactorsGraphDialog : TGrowthFactorsGraphDialog;
const OPNAME = 'TGrowthFactorsGraphValidator.GrowthFactorsGraphDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TGrowthFactorsGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TGrowthFactorsGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TGrowthFactorsGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TGrowthFactorsGraphValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGrowthFactorsGraphValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
   
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.OnStringGridCellDataHasChanged;
const OPNAME = 'TGrowthFactorsGraphValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsGraphValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TGrowthFactorsGraphValidator.DoContextValidation';
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


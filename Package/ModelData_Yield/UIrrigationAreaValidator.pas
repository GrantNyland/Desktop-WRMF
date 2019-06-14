{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationAreaValidator.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIrrigationAreaValidator;

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
  UDataEditComponent,
  VoaimsCom_TLB,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UIrrigationAreaDialog;

type
  TIrrigationAreaValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
                                 Rect: TRect; State: TGridDrawState);
    procedure OnSelectCell (Sender: TObject;
                            ACol, ARow: Integer;
                            var CanSelect: Boolean);
    procedure OnRelaxationRadioGroupClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure RePopulateGrid;
    procedure UpdateIrrigationAreaName;
    procedure UpdateDiversionChannelUpstreamNode;
    procedure UpdateReturnFlowChannelDownstreamNode;
    procedure UpdateRelaxationPolicy;
    procedure UpdateDiversion(AMonth : integer;
                              AValue : string);
    procedure UpdateReturnFlow(AMonth : integer;
                               AValue : string);
    procedure UpdateXCoord;
    procedure UpdateYCoord;
    procedure ValidateFeatureName (AFeature : IIrrigationArea);
    procedure ValidateRelaxationPolicy (AFeature : IIrrigationArea);
    procedure ValidateDivertedFlows (AFeature : IIrrigationArea);
    procedure ValidateReturnFlows (AFeature : IIrrigationArea);
    procedure ValidateDiversionUpstreamNode (AFeature : IIrrigationArea);
    procedure ValidateReturnFlowDownstreamNode (AFeature : IIrrigationArea);
    procedure ValidateXCoord(AReservoirData: IReservoirData);
    procedure ValidateYCoord(AReservoirData: IReservoirData);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function IrrigationAreaDialog : TIrrigationAreaDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkElementData;

{******************************************************************************}
{* TIrrigationAreaValidator                                                   *}
{******************************************************************************}

procedure TIrrigationAreaValidator.CreateMemberObjects;
const OPNAME = 'TIrrigationAreaValidator.CreateMemberObjects';
var
  lPanel : TIrrigationAreaDialog;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TIrrigationAreaDialog.Create(FPanelOwner,FAppModules);
    lPanel := IrrigationAreaDialog;
    with lPanel do
    begin
      FeatureNameEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('AreaName');
      FeatureNameEdit.OnEnter            := OnEditControlEnter;
      FeatureNameEdit.OnExit             := OnEditControltExit;

      UpstreamNodeCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('DiversionChannelUpStreamNode');
      UpstreamNodeCbx.OnEnter            := OnEditControlEnter;
      UpstreamNodeCbx.OnExit             := OnEditControltExit;

      DownStreamNodeCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReturnFlowChannelDownStreamNode');
      DownStreamNodeCbx.OnEnter          := OnEditControlEnter;
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      RelaxationRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('RelaxationDemand');
      RelaxationRadioGroup.OnEnter       := OnEditControlEnter;
      RelaxationRadioGroup.OnClick       := OnRelaxationRadioGroupClick;

      MonthlyFlowsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      MonthlyFlowsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DiversionFlow'));
      MonthlyFlowsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReturnFlow'));
      MonthlyFlowsGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      MonthlyFlowsGrid.OnColEnter          := OnStringGridColEnter;
      MonthlyFlowsGrid.OnDrawCell          := StringGridDrawCell;
      MonthlyFlowsGrid.OnSelectCell        := OnSelectCell;
      MonthlyFlowsGrid.OnEnter             := OnEditControlEnter;
      MonthlyFlowsGrid.ShowGridPopupMenu   := True;
      MonthlyFlowsGrid.AllowPasteFromExcel := True;
      MonthlyFlowsGrid.OnPasteFromExcel    := Self.OnAfterPasteGridData;
      MonthlyFlowsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteColumnData;
      MonthlyFlowsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;

      IrrigationAreaXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      IrrigationAreaXCoordEdit.OnEnter       := OnEditControlEnter;
      IrrigationAreaXCoordEdit.OnExit        := OnEditControltExit;

      IrrigationAreaYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      IrrigationAreaYCoordEdit.OnEnter       := OnEditControlEnter;
      IrrigationAreaYCoordEdit.OnExit        := OnEditControltExit;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.DestroyMemberObjects;
const OPNAME = 'TIrrigationAreaValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.Initialise: boolean;
const OPNAME = 'TIrrigationAreaValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with IrrigationAreaDialog.RelaxationRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.RelaxationPolicy0'));
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.RelaxationPolicy1'));
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.RelaxationPolicy2'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationAreaValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.IrrigationArea');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ClearDataViewer;
const OPNAME = 'TIrrigationAreaValidator.ClearDataViewer';
var
  lPanel : TIrrigationAreaDialog;
  lRow   : integer;
  lCol   : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := IrrigationAreaDialog;
    with lPanel do
    begin
      FeatureNameEdit.SetFieldValue('');
      UpStreamNodeCbx.ItemIndex := -1;
      DownStreamNodeCbx.ItemIndex := -1;
      RelaxationRadioGroup.ItemIndex := -1;
      for lCol := 0 to MonthlyFlowsGrid.ColCount - 1 do
        for lRow := 1 to MonthlyFlowsGrid.RowCount - 1 do
          MonthlyFlowsGrid.Cells[lCol, lRow] := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.PopulateDataViewer;
const OPNAME = 'TIrrigationAreaValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIrrArea);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.RePopulateDataViewer;
const OPNAME = 'TIrrigationAreaValidator.RePopulateDataViewer';
var
  lIrrigationArea : IIrrigationArea;
  lReservoirData  : IReservoirData;
  lReservoir      : IReservoirConfigurationData;
  lFieldProperty  : TAbstractFieldProperty;
  lFieldIndex     : string;
  lKeyValues      : string;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lIrrigationArea <> nil) then
      begin
        with IrrigationAreaDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lIrrigationArea.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          FeatureNameEdit.Text := lIrrigationArea.FeatureName;

          RePopulateNodes;
          lFieldProperty := UpStreamNodeCbx.FieldProperty;
          UpStreamNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lIrrigationArea.DiversionChannel <> nil) then
          begin
            lReservoirData :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                 ReservoirList.ReservoirOrNodeByIdentifier[lIrrigationArea.DiversionChannel.UpStreamNodeNumber];
            if (lReservoirData <> nil) then
            begin
              lReservoir := lReservoirData.ReservoirConfigurationData;
              UpstreamNodeCbx.SetFieldIndex
                (UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
            end;
          end;
          lFieldProperty := DownStreamNodeCbx.FieldProperty;
          DownStreamNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lIrrigationArea.ReturnFlowChannel <> nil) then
          begin
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                ReservoirList.ReservoirOrNodeByIdentifier[lIrrigationArea.ReturnFlowChannel.DownStreamNodeNumber];
            if (lReservoirData <> nil) then
            begin
              lReservoir := lReservoirdata.ReservoirConfigurationData;
              DownstreamNodeCbx.SetFieldIndex
                (DownstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
            end;
          end;

          lFieldProperty := RelaxationRadioGroup.FieldProperty;
          RelaxationRadioGroup.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lIrrigationArea.IrrigationPolicy <= 2) then
            RelaxationRadioGroup.ItemIndex := lIrrigationArea.IrrigationPolicy
          else
            RelaxationRadioGroup.ItemIndex := -1;

          lReservoirData  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                           .ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
          if (lReservoirData <> nil) then
          begin
            IrrigationAreaXCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.XCoord);
            IrrigationAreaYCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.YCoord);
          end;

          RepopulateGrid;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.RePopulateNodes;
const OPNAME = 'TIrrigationAreaValidator.RePopulateNodes';
var
  lReservoirList  : IReservoirDataList;
  lIndexA         : integer;
  lReservoir      : IReservoirConfigurationData;
  lIrrigationArea : IIrrigationArea;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lIrrigationArea <> nil) then
      begin
        with IrrigationAreaDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lIrrigationArea.ReturnFlowChannel <> nil) then
                  DownStreamNodeCbx.Items.AddObject('(0) ' +
                  UpperCase(lIrrigationArea.ReturnFlowChannel.SinkName), TObject(lReservoir.ReservoirIdentifier));
                if (lIrrigationArea.DiversionChannel <> nil) then
                  UpStreamNodeCbx.Items.AddObject('(0) ' +
                  UpperCase(lIrrigationArea.DiversionChannel.SourceName), TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                if (lReservoir.NodeType <> ntIrrigationNode) then
                begin
                  DownStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                  UpStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.RePopulateGrid;
const OPNAME = 'TIrrigationAreaValidator.RePopulateGrid';
var
  lMonth          : integer;
  lMonths         : TMonthNamesArray;
  lIrrigationArea : IIrrigationArea;
  lFieldProp1     : TAbstractFieldProperty;
  lFieldProp2     : TAbstractFieldProperty;
  lFieldIndex     : string;
  lKeyValues      : string;
begin
  lMonths := nil;
  try
    if (FIdentifier >= 0) then
    begin
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lIrrigationArea <> nil) then
      begin
        with IrrigationAreaDialog do
        begin
          MonthlyFlowsGrid.Cells[0, 0] := '';
          lFieldProp1 := MonthlyFlowsGrid.FieldProperty(1);
          lFieldProp2 := MonthlyFlowsGrid.FieldProperty(2);
          for lMonth := 1 to 12 do
          begin
            lFieldIndex := IntToStr(lMonth);
            lKeyValues  := lIrrigationArea.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
            MonthlyFlowsGrid.HasMetaData[1, lMonth] :=
              FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
            lKeyValues  := lIrrigationArea.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
            MonthlyFlowsGrid.HasMetaData[2, lMonth] :=
              FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;

            MonthlyFlowsGrid.Cells[0, lMonth] := lMonths[lMonth];
            MonthlyFlowsGrid.Cells[1, lMonth] := Format(lFieldProp1.FormatStringGrid{ '%6.3f'}, [lIrrigationArea.DiversionFlowByMonth[lMonth]]);
            MonthlyFlowsGrid.Cells[2, lMonth] := Format(lFieldProp1.FormatStringGrid{ '%6.3f'}, [lIrrigationArea.ReturnFlowByMonth[lMonth]]);
            MonthlyFlowsGrid.Cells[3, lMonth] := Format(lFieldProp1.FormatStringGrid{ '%6.3f'}, [lIrrigationArea.DiversionFlowByMonth[lMonth] -
                                                                  lIrrigationArea.ReturnFlowByMonth[lMonth]]);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.SaveState: boolean;
const OPNAME = 'TIrrigationAreaValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.IrrigationAreaDialog : TIrrigationAreaDialog;
const OPNAME = 'TIrrigationAreaValidator.IrrigationAreaDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrigationAreaDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrigationAreaValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrigationAreaValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrigationAreaValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrigationAreaValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IrrigationAreaDialog do
    begin
      if ((Sender = FeatureNameEdit) AND (FeatureNameEdit.HasValueChanged)) then
        UpdateIrrigationAreaName
      else
      if ((Sender = UpStreamNodeCbx) AND (UpStreamNodeCbx.HasValueChanged)) then
        UpdateDiversionChannelUpstreamNode
      else
      if ((Sender = DownStreamNodeCbx) AND (DownStreamNodeCbx.HasValueChanged)) then
        UpdateReturnFlowChannelDownstreamNode;
      if ((sender = IrrigationAreaXCoordEdit) AND (IrrigationAreaXCoordEdit.HasValueChanged)) then
       UpdateXCoord;
      if ((sender = IrrigationAreaYCoordEdit) AND (IrrigationAreaYCoordEdit.HasValueChanged)) then
       UpdateYCoord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateIrrigationAreaName;
const OPNAME = 'TIrrigationAreaValidator.UpdateIrrigationAreaName';
var
  lIrrArea : IIrrigationArea;
  lMessage : string;
begin
  try
    lIrrArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AreaName', FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lIrrArea.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lIrrArea.FeatureName);
          DoContextValidation(dvtIrrAreaName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateDiversionChannelUpstreamNode;
const OPNAME = 'TIrrigationAreaValidator.UpdateDiversionChannelUpstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lIrrArea       : IIrrigationArea;
  lMessage       : string;
begin
  try
    lIrrArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (UpStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpstreamNodeCbx.Items.Objects[UpstreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DiversionChannelUpStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            lIrrArea.DiversionChannel.UpStreamNodeNumber := lReservoirNr;
            lReservoirNr := lIrrArea.DiversionChannel.UpStreamNodeNumber;
            UpstreamNodeCbx.SetFieldIndex(UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtIrrAreaDiversionUpstreamNode);
          end
          else
            UpStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateReturnFlowChannelDownstreamNode;
const OPNAME = 'TIrrigationAreaValidator.UpdateReturnFlowChannelDownstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lIrrArea       : IIrrigationArea;
  lMessage       : string;
begin
  try
    lIrrArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (DownStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(DownStreamNodeCbx.Items.Objects[DownStreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'ReturnFlowChannelDownStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            lIrrArea.ReturnFlowChannel.DownStreamNodeNumber := lReservoirNr;
            lReservoirNr := lIrrArea.ReturnFlowChannel.DownStreamNodeNumber;
            DownStreamNodeCbx.SetFieldIndex(DownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtIrrAreaReturnFlowDownstreamNode);
          end
          else
            DownStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateRelaxationPolicy;
const OPNAME = 'TIrrigationAreaValidator.UpdateRelaxationPolicy';
var
  lIrrigationArea : IIrrigationArea;
  lOldType        : integer;
  lNewType        : integer;
  lMessage        : string;
begin
  try
    lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrigationArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        lOldType := lIrrigationArea.IrrigationPolicy;
        lNewType := RelaxationRadioGroup.ItemIndex;
        if (lOldType <> lNewType) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              RelaxationRadioGroup.FieldProperty.FieldName,
              IntToStr(lNewType), lMessage)) then
          begin
            lIrrigationArea.IrrigationPolicy := lNewType;
            if (lIrrigationArea.IrrigationPolicy <= 2) then
              RelaxationRadioGroup.ItemIndex := lIrrigationArea.IrrigationPolicy
            else
              RelaxationRadioGroup.ItemIndex := -1;
          end
          else
            RelaxationRadioGroup.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationAreaValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with IrrigationAreaDialog do
    begin
      if (MonthlyFlowsGrid = ASender) then
      begin
        if (ACol = 1) then
          UpdateDiversion(ARow, Trim(MonthlyFlowsGrid.Cells[ACol, ARow]))
        else
        if (ACol = 2) then
          UpdateReturnFlow(ARow, Trim(MonthlyFlowsGrid.Cells[ACol, ARow]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateDiversion(AMonth : integer;
                                                   AValue : string);
const OPNAME = 'TIrrigationAreaValidator.UpdateDiversion';
var
  lIrrigationArea : IIrrigationArea;
  lValue          : double;
  lMessage        : string;
begin
  try
    lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrigationArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        if (Trim(AValue) = '') then
          AValue := '0.0';
        MonthlyFlowsGrid.ValidationError[1, AMonth, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DiversionFlow', AValue, lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lIrrigationArea.DiversionFlowByMonth[AMonth] := LValue;
          RePopulateGrid;
          DoContextValidation(dvtIrrAreaDiversionFlows);
        end
        else
          MonthlyFlowsGrid.ValidationError[1, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateReturnFlow(AMonth : integer;
                                                    AValue : string);
const OPNAME = 'TIrrigationAreaValidator.UpdateReturnFlow';
var
  lIrrigationArea : IIrrigationArea;
  lValue          : double;
  lMessage        : string;
begin
  try
    lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if (lIrrigationArea <> nil) then
    begin
      with IrrigationAreaDialog do
      begin
        if (Trim(AValue) = '') then
          AValue := '0.0';
        MonthlyFlowsGrid.ValidationError[2, AMonth, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ReturnFlow', AValue, lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lIrrigationArea.ReturnFlowByMonth[AMonth] := LValue;
          RePopulateGrid;
          DoContextValidation(dvtIrrAreaReturnFlows);
        end
        else
          MonthlyFlowsGrid.ValidationError[2, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIrrigationAreaValidator.DoContextValidation';
var
  lFeature     : IIrrigationArea;
  LReservoir   : IReservoirData;
  lFeatureList : IIrrigationAreaList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationAreaList;
      lFeature     := lFeatureList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep3, dvtIrrAreaName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep3, dvtIrrAreaRelaxationPolicy]) then
          ValidateRelaxationPolicy(lFeature);
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep3, dvtIrrAreaDiversionFlows]) then
          ValidateDivertedFlows(lFeature);
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep3, dvtIrrAreaDiversionFlows,
                                dvtIrrAreaReturnFlows]) then
          ValidateReturnFlows(lFeature);
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep1, dvtIrrAreaDiversionUpstreamNode]) then
          ValidateDiversionUpstreamNode(lFeature);
        if (AValidationType in [dvtIrrArea, dvtIrrAreaWizardStep2, dvtIrrAreaReturnFlowDownstreamNode]) then
          ValidateReturnFlowDownstreamNode(lFeature);

        LReservoir  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                       .ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (LReservoir <> nil) then
        begin
          if (AValidationType = dvtIrrArea) then
          begin
            ValidateXCoord(LReservoir);
            ValidateYCoord(LReservoir);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TIrrigationAreaValidator.DetermineWizardStatus';
var
  lFeature       : IIrrigationArea;
  lFeatureList   : IIrrigationAreaList;
  lFieldProperty : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationAreaList;
      lFeature := lFeatureList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lFeature <> nil) then
      begin
        case ASequence of
        1 :
          begin
            DoContextValidation(dvtIrrAreaWizardStep1);
            if ((lFeature.DiversionChannel <> nil) AND
                (lFeature.DiversionChannel.UpStreamNodeNumber <> 0)) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        2 :
          begin
            DoContextValidation(dvtIrrAreaWizardStep2);
            if (lFeature.ReturnFlowChannel <> nil) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        3 :
          begin
            DoContextValidation(dvtIrrAreaWizardStep3);
            lFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionFlow');
            lNotZero := FALSE;
            lIndex   := lFieldProperty.ArrayLow;
            while ((NOT lNotZero) AND (lIndex <= lFieldProperty.ArrayHigh)) do
            begin
              if ((lFeature.DiversionFlowByMonth[lIndex] > 0) OR
                  (lFeature.ReturnFlowByMonth[lIndex] > 0)) then
                lNotZero := TRUE
              else
                lIndex := lIndex + 1;
            end;
            if (lNotZero) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        else
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateFeatureName(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateFeatureName';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'AreaName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateRelaxationPolicy(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateRelaxationPolicy';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'RelaxationDemand')) then
        RelaxationRadioGroup.InValidationError := FALSE
      else
      begin
        RelaxationRadioGroup.InValidationError := TRUE;
        RelaxationRadioGroup.ValidationError := FErrorMessage;
        RelaxationRadioGroup.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateDivertedFlows(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateDivertedFlows';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with IrrigationAreaDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'DiversionFlow')) then
        begin
          for lCol := 1 to 12 do
            MonthlyFlowsGrid.ValidationError[1, lCol, gveCellContext] := ''
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              MonthlyFlowsGrid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              MonthlyFlowsGrid.ValidationError[1, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateReturnFlows(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateReturnFlows';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with IrrigationAreaDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'ReturnFlow')) then
        begin
          for lCol := 1 to 12 do
            MonthlyFlowsGrid.ValidationError[2, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              MonthlyFlowsGrid.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              MonthlyFlowsGrid.ValidationError[2, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateDiversionUpstreamNode(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateDiversionUpstreamNode';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'DiversionUpstreamNode')) then
      begin
        UpStreamNodeCbx.InValidationError := FALSE;
        UpStreamNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        UpStreamNodeCbx.InValidationError := TRUE;
        UpStreamNodeCbx.ValidationError := FErrorMessage;
        UpStreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateReturnFlowDownstreamNode(AFeature: IIrrigationArea);
const OPNAME = 'TIrrigationAreaValidator.ValidateReturnFlowDownstreamNode';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'ReturnFlowDownstreamNode')) then
      begin
        DownStreamNodeCbx.InValidationError := FALSE;
        DownStreamNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        DownStreamNodeCbx.InValidationError := TRUE;
        DownStreamNodeCbx.ValidationError := FErrorMessage;
        DownStreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.OnSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TIrrigationAreaValidator.OnSelectCell';
begin
  CanSelect := (ACol = 1) OR (ACol = 2);
end;

procedure TIrrigationAreaValidator.StringGridDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
const OPNAME = 'TIrrigationAreaValidator.StringGridDrawCell';
var
  lGrid : TStringGrid;
begin
  try
    lGrid := TStringGrid(Sender);
    with lGrid do
    begin
      if ((ARow > 0) AND ((ACol = 1) OR (ACol = 2))) then
        Canvas.Brush.Color := clWhite
      else
        Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect);
      if ((ARow < FixedRows) OR (ACol < FixedCols)) then
      begin
        with Canvas do
        begin
          Pen.Style   := psSolid;
          Pen.Width   := 1;
          Pen.Color   := clWhite;
          MoveTo(Rect.Left, Rect.Bottom-1);
          LineTo(Rect.Left, Rect.Top);
          LineTo(Rect.Right-1, Rect.Top);
          Pen.Color   := clBtnShadow;
          MoveTo(Rect.Left, Rect.Bottom-1);
          LineTo(Rect.Right-1, Rect.Bottom-1);
          LineTo(Rect.Right-1, Rect.Top);
        end;
      end;
      Canvas.TextOut(Rect.Left + 3, Rect.Top + 3, Cells[ACol, ARow]);
      if gdFocused in State then
        Canvas.DrawFocusRect(Rect);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.OnRelaxationRadioGroupClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaValidator.OnRelaxationRadioGroupClick';
begin
  try
    if(IrrigationAreaDialog.RelaxationRadioGroup.HasValueChanged) then
      UpdateRelaxationPolicy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TIrrigationAreaValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IIrrigationArea;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FIdentifier <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (lFeature <> nil) then
      begin
        with IrrigationAreaDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = UpStreamNodeCbx) then
            lFieldProperty := UpStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = DownStreamNodeCbx) then
            lFieldProperty := DownStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = RelaxationRadioGroup) then
            lFieldProperty := RelaxationRadioGroup.FieldProperty
          else
          if (FActiveControl = MonthlyFlowsGrid) then
          begin
            lFieldIndex := IntToStr(MonthlyFlowsGrid.Row);
            lFieldProperty := MonthlyFlowsGrid.FieldProperty(MonthlyFlowsGrid.Col);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateXCoord;
const OPNAME = 'TIrrigationAreaValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LIrrigationArea  : IIrrigationArea;
begin
  try
    if IrrigationAreaDialog.IrrigationAreaXCoordEdit.HasValueChanged then
    begin
      LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                        IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (LIrrigationArea <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (LReservoirObject <> nil) then
        begin
          with IrrigationAreaDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',IrrigationAreaXCoordEdit.Text, lErrorMessage) then
            begin
              IrrigationAreaXCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(IrrigationAreaXCoordEdit.Text);
              IrrigationAreaXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
            end
            else
              IrrigationAreaXCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.UpdateYCoord;
const OPNAME = 'TIrrigationAreaValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LIrrigationArea  : IIrrigationArea;
begin
  try
    if IrrigationAreaDialog.IrrigationAreaYCoordEdit.HasValueChanged then
    begin
      LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
      if (LIrrigationArea <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (LReservoirObject <> nil) then
        begin
          with IrrigationAreaDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',IrrigationAreaYCoordEdit.Text, lErrorMessage) then
            begin
              IrrigationAreaYCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(IrrigationAreaYCoordEdit.Text);
              IrrigationAreaYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
            end
            else
              IrrigationAreaYCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateXCoord(AReservoirData: IReservoirData);
const OPNAME = 'TIrrigationAreaValidator.ValidateXCoord';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      IrrigationAreaXCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'XCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      IrrigationAreaXCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaValidator.ValidateYCoord(AReservoirData: IReservoirData);
const OPNAME = 'TIrrigationAreaValidator.ValidateYCoord';
begin
  try
    with IrrigationAreaDialog do
    begin
      FErrorMessage := '';
      IrrigationAreaYCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'YCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      IrrigationAreaYCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIrrigationAreaValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TIrrigationAreaValidator.OnAfterPasteGridData';
var
  LIrrigationArea  : IIrrigationArea;
  LRow             : integer;
  LDiversionValue,
  LReturnFlowValue : double;
begin
  try
    LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if(LIrrigationArea <> nil) then
    begin
      for LRow := IrrigationAreaDialog.MonthlyFlowsGrid.FixedRows to IrrigationAreaDialog.MonthlyFlowsGrid.RowCount - 1 do
      begin
        LDiversionValue := StrToFloat(IrrigationAreaDialog.MonthlyFlowsGrid.Cells[1,LRow]);
        LIrrigationArea.DiversionFlowByMonth[LRow] := LDiversionValue;
        LReturnFlowValue := StrToFloat(IrrigationAreaDialog.MonthlyFlowsGrid.Cells[2,LRow]);
        LIrrigationArea.ReturnFlowByMonth[LRow] := LReturnFlowValue;
      end;
      RePopulateDataViewer; 
      DoContextValidation(dvtIrrAreaDiversionFlows);
      DoContextValidation(dvtIrrAreaReturnFlows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TIrrigationAreaValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TIrrigationAreaValidator.OnAfterPasteColumnData';
var
  LIrrigationArea : IIrrigationArea;
  LRow            : integer;
  LValue          : double;
begin
  try
    LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[FIdentifier];
    if(LIrrigationArea <> nil) then
    begin
      if(Sender = IrrigationAreaDialog.MonthlyFlowsGrid) then
      begin
        if(IrrigationAreaDialog.MonthlyFlowsGrid.Col = 1) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(IrrigationAreaDialog.MonthlyFlowsGrid.Cells[1,LRow]));
            LIrrigationArea.DiversionFlowByMonth[LRow] := LValue;
          end;
        end
        else
        if(IrrigationAreaDialog.MonthlyFlowsGrid.Col = 2) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(IrrigationAreaDialog.MonthlyFlowsGrid.Cells[2,LRow]));
            LIrrigationArea.ReturnFlowByMonth[LRow] := LValue;
          end;
        end;

      end;
      RePopulateDataViewer;
      DoContextValidation(dvtIrrAreaDiversionFlows);
      DoContextValidation(dvtIrrAreaReturnFlows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


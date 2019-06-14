//
//  UNIT      : Contains the class TReservoirPenaltyValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirPenaltyValidator;

interface

uses

  // Delphi VCL, RTL, etc
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UReservoirPenaltyDialog;

type
  TReservoirPenaltyValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedPenaltyStructure,
    FPenaltyStructureNumber: integer;
    FDragNode : TTreeNode;
    FReservoirNumber : integer;
    FDragFromTreeView: TReservoirPenaltyTreeView;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnAddPenaltyClick(Sender: TObject);
    procedure OnDeletePenaltyClick(Sender: TObject);
    procedure OnAddZoneClick(Sender: TObject);
    procedure OnDeleteZoneClick(Sender: TObject);


    procedure OnEditControlEnter(ASender: TObject); override;
    procedure OnEditControltExit(ASender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnStringGridColEnter(Sender: TObject;ACol, ARow: integer); override;

    procedure OnAddColumn(ASender : TObject);
    procedure OnRemoveColumn(ASender : TObject);
    procedure OnComboBoxChange(ASender : TObject);
    procedure OnStringGridCellHasChanged(ASender: TObject; ACol, ARow: integer);

    procedure OnTreeViewStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure OnTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);

    function UpdatePenaltyZoneName(AValue : string; AZone : integer) : boolean;
    function UpdatePenaltyZonePolicy(AValue : string; AZone : integer; APolicy : integer) : boolean;
    function UpdatePenaltyValue(AValue : string; AZone : integer; AStructure : integer) : boolean;

    function UpdateValue(ASender : TFieldEdit) : boolean; overload;
    function UpdateValue(ACol, ARow: integer;AValue: string): boolean; overload;

    procedure RePopulateDataViewer;
    procedure SetPenaltyStructureNumber(const Value: integer);
    procedure SetReservoirNumber(const AValue: integer);
    procedure ValidatePenaltyStructure(APenaltyIndex: integer;APenaltyData: IReservoirPenalty);
    procedure ValidateAllPenaltyStructures;
  public
    function Initialise: boolean; override;
    function RestoreState : boolean;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirPenaltyDialog: TReservoirPenaltyDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    property PenaltyStructureNumber: integer read FPenaltyStructureNumber write SetPenaltyStructureNumber;
    property ReservoirNumber: integer read FReservoirNumber write SetReservoirNumber;

  end;

implementation

uses

  // Delphi VCL, RTL, etc
  Contnrs,
  SysUtils,
  VCL.Graphics,
  Math,
  // arivia.kom
  UConstants,
  UReservoirData,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UReservoirPenaltyActionDialog,
  UErrorHandlingOperations;

{ TReservoirPenaltyValidator }

procedure TReservoirPenaltyValidator.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDragFromTreeView := nil;
    FDragNode         := nil;
    FPenaltyStructureNumber := -1;
    FReservoirNumber        := -1;
    FSelectedPenaltyStructure := 0;
    FPanel := TReservoirPenaltyDialog.Create(FPanelOwner,FAppModules);

    ReservoirPenaltyDialog.OnTreeViewStartDrag := OnTreeViewStartDrag;
    ReservoirPenaltyDialog.OnTreeViewDragOver  := OnTreeViewDragOver;
    ReservoirPenaltyDialog.OnTreeViewDragDrop  := OnTreeViewDragDrop;

    ReservoirPenaltyDialog.ChkBalancingData.FieldProperty := FAppModules.FieldProperties.FieldProperty('BalancingData');
    ReservoirPenaltyDialog.ChkBalancingData.OnEnter       := OnEditControlEnter;
    ReservoirPenaltyDialog.ChkBalancingData.OnExit        := OnEditControltExit;

    ReservoirPenaltyDialog.EdtRuleCurve.FieldProperty  := FAppModules.FieldProperties.FieldProperty('RuleCurve');
    ReservoirPenaltyDialog.EdtRuleCurve.OnEnter        := OnEditControlEnter;
    ReservoirPenaltyDialog.EdtRuleCurve.OnExit         := OnEditControltExit;

    //ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirPenalty'));
    ReservoirPenaltyDialog.GrdPenalty.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReservoirPenaltyDialog.GrdPenalty.OnColEnter         := OnStringGridColEnter;
    ReservoirPenaltyDialog.GrdPenalty.OnAfterCellChange  := OnStringGridCellHasChanged;
    ReservoirPenaltyDialog.GrdPenalty.OnEnter            := OnEditControlEnter;

    //ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PenaltyStruct'));
    ReservoirPenaltyDialog.GrdReservoir.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReservoirPenaltyDialog.GrdReservoir.OnColEnter         := OnStringGridColEnter;
    ReservoirPenaltyDialog.GrdReservoir.OnAfterCellChange  := OnStringGridCellHasChanged;

    ReservoirPenaltyDialog.BtnAddPenalty.FieldProperty    := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
    ReservoirPenaltyDialog.BtnDeletePenalty.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
    ReservoirPenaltyDialog.BtnAddPenalty.OnClick          := OnAddPenaltyClick;
    ReservoirPenaltyDialog.BtnDeletePenalty.OnClick       := OnDeletePenaltyClick;

    ReservoirPenaltyDialog.BtnAddZone.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    ReservoirPenaltyDialog.BtnDeleteZone.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    ReservoirPenaltyDialog.BtnAddZone.OnClick             := OnAddZoneClick;
    ReservoirPenaltyDialog.BtnDeleteZone.OnClick          := OnDeleteZoneClick;

    ReservoirPenaltyDialog.RadMode.FieldProperty          :=  FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');

    ReservoirPenaltyDialog.BtnAddReservior.FieldProperty  :=  FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');

    ReservoirPenaltyDialog.BtnDeleteReservior.FieldProperty :=  FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');

    // The Framework is supposed to handle saving and restoring state
    RestoreState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyValidator.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

function TReservoirPenaltyValidator.Initialise: boolean;
const OPNAME = 'TReservoirPenaltyValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('StrategyIndicator'));
    ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BalancingVariable'));
    ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BalancingPolicy'));

    ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('StrategyIndicator'));
    ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BalancingVariable'));
    ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BalancingPolicy'));
    ReservoirPenaltyDialog.RadMode.ItemIndex := FAppModules.ViewIni.ReadInteger(Self.ClassName, 'RAD', 0);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPenaltyValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Penalty');;
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.ClearDataViewer;
const OPNAME = 'TReservoirPenaltyValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ReservoirPenaltyDialog.NumberOfStorageZones := 0;
    ReservoirPenaltyDialog.NumberOfPenaltyStructures := 0;

    ReservoirPenaltyDialog.EdtRuleCurve.SetFieldValue('');
    ReservoirPenaltyDialog.RuleCurveLevel := 0;

    ReservoirPenaltyDialog.OnAddColumn := nil;
    ReservoirPenaltyDialog.OnDeleteColumn := nil;
    ReservoirPenaltyDialog.OnColumnTextChange := nil;

    ReservoirPenaltyDialog.BtnAddZone.IsEnabled := False;
    ReservoirPenaltyDialog.BtnAddPenalty.IsEnabled := False;
    ReservoirPenaltyDialog.BtnAddReservior.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeleteZone.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeletePenalty.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeleteReservior.IsEnabled := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirPenaltyValidator.RePopulateDataViewer';
var
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyZoneData      : IReservoirPenaltyZoneData;
  lReservoirPenalty     : IReservoirPenalty;
  LPenaltyStructID      : integer;
  LIndex                : integer;
  LCount                : integer;
  LZonePenalty          : double;
  LCurrentTreeView      : TReservoirPenaltyTreeView;
  LReservoirList        : TObjectList;
  LReservoirObject      : TReservoirData;
  LCurrentNode          : TTreeNode;
  LMainNode             : TTreeNode;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
  lFieldProperty        : TAbstractFieldProperty;
  lKeyValues            : string;
  lFieldIndex           : string;
  lCol                  : integer;
begin
  try
    ReservoirPenaltyDialog.BtnAddZone.IsEnabled := False;
    ReservoirPenaltyDialog.BtnAddPenalty.IsEnabled := False;
    ReservoirPenaltyDialog.BtnAddReservior.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeleteZone.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeletePenalty.IsEnabled := False;
    ReservoirPenaltyDialog.BtnDeleteReservior.IsEnabled := False;

    LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
    if (LPenaltyStructureList <> nil) then
    begin

      LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
      if (LPenaltyCountsData <> nil) then
      begin
        lFieldProperty := ReservoirPenaltyDialog.EdtRuleCurve.FieldProperty;
        lKeyValues := LPenaltyCountsData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        ReservoirPenaltyDialog.EdtRuleCurve.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
        ReservoirPenaltyDialog.EdtRuleCurve.SetFieldValue(IntToStr(LPenaltyCountsData.ZoneRuleCurve));
        ReservoirPenaltyDialog.RuleCurveLevel := LPenaltyCountsData.ZoneRuleCurve;

        lFieldProperty := ReservoirPenaltyDialog.ChkBalancingData.FieldProperty;
        lKeyValues := LPenaltyCountsData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
        ReservoirPenaltyDialog.ChkBalancingData.HasMetaData :=
          FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

        ReservoirPenaltyDialog.NumberOfPenaltyStructures := LPenaltyCountsData.PenaltyStructureCount;
        ReservoirPenaltyDialog.NumberOfStorageZones      := LPenaltyCountsData.StorageZoneCount;

        // Should Clear FieldProperty List Here
        for LCount := 0 to LPenaltyCountsData.PenaltyStructureCount do
        begin
          ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirPenalty'));
          if(ReservoirPenaltyDialog.PenaltyTreeViewByIndex[LCount] <> nil) then
          begin
            ReservoirPenaltyDialog.PenaltyTreeViewByIndex[LCount].FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
            ReservoirPenaltyDialog.PenaltyTreeViewByIndex[LCount].OnEnter := OnEditControlEnter;
            //ReservoirPenaltyDialog.PenaltyTreeViewByIndex[LCount].OnExit := OnEditControltExit;
          end;

          if(ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LCount-1] <> nil) then
          begin
            ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LCount-1].FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
            ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LCount-1].OnEnter := OnEditControlEnter;
            //ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LCount].OnExit := OnEditControltExit;
          end;
        end;
      end;

      for lCol := 1 to ReservoirPenaltyDialog.GrdPenalty.ColCount-1 do
      begin
        lFieldProperty := ReservoirPenaltyDialog.GrdPenalty.FieldProperty(lCol-1);
        for LCount := 0 to LPenaltyStructureList.PenaltyZoneCount - 1 do
        begin
          LPenaltyZoneData := LPenaltyStructureList.ReservoirPenaltyZoneByIndex[LCount];
          if (LPenaltyZoneData <> nil) then
          begin
            lFieldIndex := IntToStr(lCol-1) + ',' + IntToStr(LCount);
            lKeyValues := LPenaltyZoneData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            ReservoirPenaltyDialog.GrdPenalty.HasMetaData[lCol-1, LCount] :=
                 FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

            ReservoirPenaltyDialog.GrdPenalty.Cells[0, LCount + 1] := LPenaltyZoneData.ZoneName;
            ReservoirPenaltyDialog.GrdPenalty.Cells[1, LCount + 1] := IntToStr(LPenaltyZoneData.StrategyIndicator);
            ReservoirPenaltyDialog.GrdPenalty.Cells[2, LCount + 1] := IntToStr(LPenaltyZoneData.BalancingVariable);
            ReservoirPenaltyDialog.GrdPenalty.Cells[3, LCount + 1] := IntToStr(LPenaltyZoneData.BalancingPolicy);

            ReservoirPenaltyDialog.GrdReservoir.Cells[0, LCount + 1] := LPenaltyZoneData.ZoneName;
            ReservoirPenaltyDialog.GrdReservoir.Cells[1, LCount + 1] := IntToStr(LPenaltyZoneData.StrategyIndicator);
            ReservoirPenaltyDialog.GrdReservoir.Cells[2, LCount + 1] := IntToStr(LPenaltyZoneData.BalancingVariable);
            ReservoirPenaltyDialog.GrdReservoir.Cells[3, LCount + 1] := IntToStr(LPenaltyZoneData.BalancingPolicy);
          end;
        end;
      end;
      LReservoirList := TObjectList.Create(False);
      try
        LCurrentTreeView := ReservoirPenaltyDialog.PenaltyTreeViewByIndex[0];
        if Assigned(LCurrentTreeView) then
        begin
          LCurrentTreeView.PennaltyStructureNumber := 0;
          LCurrentTreeView.Items.Clear;
          if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.GetReservoirsWithNoPenaltyStructure(LReservoirList) then
          begin
            LMainNode := LCurrentTreeView.Items.Add(nil,'Reservoirs without penalty structures');
            for LIndex := 0 to LReservoirList.Count -1 do
            begin
              LReservoirObject := TReservoirData(LReservoirList.Items[LIndex]);
              LCurrentNode := LCurrentTreeView.Items.AddChild(LMainNode,LReservoirObject.ReservoirConfigurationData.ReservoirName);
              LCurrentNode.Data := LReservoirObject;
            end;
            LCurrentTreeView.AlphaSort;
            LCurrentTreeView.FullExpand;
            LCurrentTreeView.TopItem := LCurrentTreeView.Items.GetFirstNode;
          end;
        end;

        for LCount := 0 to LPenaltyStructureList.PenaltyCount - 1 do
        begin
          lReservoirPenalty := LPenaltyStructureList.ReservoirPenaltyByIndex[LCount];
          if (lReservoirPenalty <> nil) then
          begin
            LCurrentTreeView := ReservoirPenaltyDialog.PenaltyTreeViewByIndex[LCount + 1];
            if Assigned(LCurrentTreeView) then
            begin
              LCurrentTreeView.PennaltyStructureNumber := LCount + 1;
              LCurrentTreeView.Items.Clear;
              if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.GetReservoirsPerPenaltyStructure(lReservoirPenalty.ReservoirPenaltyID,LReservoirList) then
              begin
                LMainNode := LCurrentTreeView.Items.Add(nil,'Penalty Structure ' + IntToStr(lReservoirPenalty.ReservoirPenaltyID));
                for LIndex := 0 to LReservoirList.Count -1 do
                begin
                  LReservoirObject := TReservoirData(LReservoirList.Items[LIndex]);
                  LCurrentNode := LCurrentTreeView.Items.AddChild(LMainNode,LReservoirObject.ReservoirConfigurationData.ReservoirName);
                  LCurrentNode.Data := LReservoirObject;
                end;
                LCurrentTreeView.AlphaSort;
                LCurrentTreeView.FullExpand;
                LCurrentTreeView.TopItem := LCurrentTreeView.Items.GetFirstNode;
              end;
            end;
          end;
        end;
      finally
        LReservoirList.Free;
      end;
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
      for LCount := 0 to LPenaltyStructureList.PenaltyCount - 1 do
      begin
        lFieldIndex := IntToStr(lCount+1);
        lReservoirPenalty := LPenaltyStructureList.ReservoirPenaltyByIndex[LCount];
        if (lReservoirPenalty <> nil) then
        begin
          for LIndex := 1 to LPenaltyCountsData.StorageZoneCount do
          begin
            lKeyValues  := lReservoirPenalty.GetKeyValues(lFieldProperty.FieldName, IntToStr(LIndex));
            ReservoirPenaltyDialog.GrdPenalty.HasChanges[4 + LCount, LIndex] :=
              FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            ReservoirPenaltyDialog.GrdPenalty.HasMetaData[4 + LCount, LIndex] :=
             (FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil);

            LZonePenalty := lReservoirPenalty.ReservoirPenaltyValueByIndex[LIndex];
            ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(lFieldProperty);
            if(LZonePenalty = NullFloat) then
              ReservoirPenaltyDialog.GrdPenalty.Cells[4 + LCount, LIndex] := ''
            else
              ReservoirPenaltyDialog.GrdPenalty.Cells[4 + LCount, LIndex] := Format(lFieldProperty.FormatStringGrid,[LZonePenalty]);//FormatFloat('###0.###',LZonePenalty);
          end;
        end;
      end;

      for LCount := 0 to ReservoirPenaltyDialog.NumberOfComboBoxes - 1 do
      begin
        LCurrentTreeView := ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LCount];
        if Assigned(LCurrentTreeView) then
        begin
          LPenaltyStructID  := LCurrentTreeView.PennaltyStructureNumber;
          lReservoirPenalty := LPenaltyStructureList.ReservoirPenaltyByIdentifier[LPenaltyStructID];
          if (lReservoirPenalty <> nil) then
          begin
            for LIndex := 0 to lReservoirPenalty.PenaltyValueCount - 1 do
            begin
            ReservoirPenaltyDialog.GrdPenalty.AddFieldProperty(lFieldProperty);
              LZonePenalty := lReservoirPenalty.ReservoirPenaltyValueByIndex[LIndex+1];
              ReservoirPenaltyDialog.GrdReservoir.Cells[4 + LCount, LIndex + 1] := Format(lFieldProperty.FormatStringGrid,[LZonePenalty]);//FormatFloat('###0.###',LZonePenalty);
            end;
          end;
        end;
      end;

      ReservoirPenaltyDialog.OnAddColumn := OnAddColumn;
      ReservoirPenaltyDialog.OnDeleteColumn := OnRemoveColumn;
      ReservoirPenaltyDialog.OnColumnTextChange := OnComboBoxChange;

      ReservoirPenaltyDialog.ViewMode := FViewMode;
      ReservoirPenaltyDialog.RadMode.OnClick(nil);
      if (FPenaltyStructureNumber <= 0) AND
         (ReservoirPenaltyDialog.GrdPenalty.ColCount > 4) AND
         (ReservoirPenaltyDialog.GrdPenalty.RowCount > 1) then
      begin
       ReservoirPenaltyDialog.GrdPenalty.Col := 4;
       ReservoirPenaltyDialog.GrdPenalty.Row := 1;
      end;
    end;
    ReservoirPenaltyDialog.ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnStringGridCellHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirPenaltyValidator.OnStringGridCellHasChanged';
begin
  try
    ReservoirPenaltyDialog.ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnEditControlEnter(ASender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(ASender);
  try
    if (FActiveControl = nil) AND (ASender.ClassNameIs('TColumnSelectGrid')) then
      FActiveControl := TColumnSelectGrid(ASender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnEditControltExit';
var
  LMessage : string;
begin
  inherited OnEditControltExit(ASender);
  try
    LMessage := '';
    if(ASender.ClassName = TFieldEdit.ClassName) and
      TFieldEdit(ASender).HasValueChanged then
    begin
      if (ASender = ReservoirPenaltyDialog.EdtRuleCurve) then
      begin
        if (strToInt(ReservoirPenaltyDialog.EdtRuleCurve.Text) >
            TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList.
            ReservoirPenaltyCounts.StorageZoneCount -1) then
        begin
          LMessage := FAppModules.language.GetString('TField.ZoneRuleCurveLevel');
          ReservoirPenaltyDialog.EdtRuleCurve.FieldValidationError := LMessage;
          Exit;
        end;
      end;

      if UpdateValue(TFieldEdit(ASender)) then
      begin
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.ReservoirPenaltyDialog:TReservoirPenaltyDialog;
const OPNAME = 'TReservoirPenaltyValidator.ReservoirPenaltyDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirPenaltyDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirPenaltyValidator.StudyDataHasChanged';
var
  LNewPenalty,
  LOldPenalty: integer;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'BALANCINGPOLICY') or
      (UpperCase(AFieldName) = 'STRATEGYINDICATOR') or
      (UpperCase(AFieldName) = 'BALANCINGVARIABLE') or
      (UpperCase(AFieldName) = 'ZONERULECURVE') or
      (UpperCase(AFieldName) = 'RESERVOIRZONENAME') or
      (UpperCase(AFieldName) = 'RESERVOIRPENALTY') then
      begin
        RePopulateDataViewer;
        LanguageHasChanged;
      end
      else
      if(UpperCase(AFieldName) = 'PENALTYSTRUCT') then
      begin
        if(FViewMode in [vmSelect,vmEditableSelect]) then
        begin
          LNewPenalty := StrToInt(ANewValue);
          LOldPenalty := StrToInt(AOldValue);
          if(FPenaltyStructureNumber = LOldPenalty) then
          begin
            PenaltyStructureNumber := LNewPenalty;
            FSelectedPenaltyStructure := LNewPenalty;
          end;
        end;
        PopulateDataViewer;
        LanguageHasChanged;
      end
      else
      if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
         ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
         (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
         (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
         (UpperCase(AFieldName) = 'ELEMENTORDER') then
        PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirPenaltyValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnAddColumn(ASender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnAddColumn';
var
  LIndex : Integer;
  LReservoirData: IReservoirData;
begin
  try
    ReservoirPenaltyDialog.NumberOfComboBoxes := ReservoirPenaltyDialog.NumberOfComboBoxes + 1;
    for lIndex := 0 to ReservoirPenaltyDialog.NumberOfComboBoxes -1 do
    begin
      ReservoirPenaltyDialog.ComboBoxByIndex[lIndex].FieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
    end;

    ReservoirPenaltyDialog.GrdReservoir.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirPenalty'));
    ReservoirPenaltyDialog.ComboBoxByIndex[ReservoirPenaltyDialog.NumberOfComboBoxes - 1].Items.Clear;
    for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirCount - 1 do
    begin
      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIndex[LIndex];
      if (LReservoirData <> nil) then
        ReservoirPenaltyDialog.ComboBoxByIndex[ReservoirPenaltyDialog.NumberOfComboBoxes - 1].Items.AddObject(
          LReservoirData.ReservoirConfigurationData.ReservoirName, TObject(
          LReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
    end;
    ReservoirPenaltyDialog.ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnComboBoxChange(ASender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnComboBoxChange';
var
  LDataObject      : TYieldModelDataObject;
  LReservoirIndex  : Integer;
  LComboIndex      : integer;
  LPenaltyIndex    : integer;
  LPenaltyStructID : integer;
  LReservoirID     : integer;
  LTreeView        : TReservoirPenaltyTreeView;
  LTreeNode        : TTreeNode;
  LPenaltyStructureData: IReservoirPenalty;
begin
  try
    if Assigned(FAppModules.Model().ModelData()) then
    begin
      for LComboIndex := 0 to ReservoirPenaltyDialog.NumberOfComboBoxes - 1 do
      begin
        if ASender = ReservoirPenaltyDialog.ComboBoxByIndex[LComboIndex] then
        begin
          if TComboBox(ASender).ItemIndex >= 0 then
          begin
            LReservoirID := Integer(TComboBox(ASender).Items.Objects[TComboBox(ASender).ItemIndex]);
            LDataObject := TYieldModelDataObject(FAppModules.Model().ModelData());
            LPenaltyStructID := LDataObject.NetworkElementData.ReservoirList.ReservoirByIdentifier[LReservoirID].ReservoirConfigurationData.PenaltyStructIdentifier;

            LPenaltyStructureData := LDataObject.NetworkElementData.ReservoirPenaltyStructureList.ReservoirPenaltyByIdentifier[LPenaltyStructID];
            if (LPenaltyStructureData <> nil) then
            begin
              for LPenaltyIndex := 0 to LPenaltyStructureData.PenaltyValueCount - 1 do
                ReservoirPenaltyDialog.GrdReservoir.Cells[4 + LComboIndex, LPenaltyIndex + 1] :=
                  FloatToStr(LDataObject.NetworkElementData.ReservoirPenaltyStructureList.ReservoirPenaltyByIdentifier[LPenaltyStructID].ReservoirPenaltyValueByIndex[LPenaltyIndex+1]);

              ReservoirPenaltyDialog.GrdReservoir.Cells[4 + LComboIndex, 0] := 'Penalty Structure ' + IntToStr(LPenaltyStructID);

              LTreeView := ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LComboIndex];
              LTreeView.Items.Clear;
              LTreeView.PennaltyStructureNumber := LPenaltyStructID;
              LTreeNode := LTreeView.Items.Add(nil, 'Penalty Structure ' + IntToStr(LPenaltyStructID));
              for LReservoirIndex := 0 to LDataObject.NetworkElementData.ReservoirList.ReservoirCount - 1 do
              begin
                 if LDataObject.NetworkElementData.ReservoirList.ReservoirByIndex[LReservoirIndex].ReservoirConfigurationData.PenaltyStructIdentifier
                   = LPenaltyStructID then
                   LTreeView.Items.AddChild(LTreeNode, LDataObject.NetworkElementData.ReservoirList.ReservoirByIndex[LReservoirIndex].ReservoirConfigurationData.ReservoirName);
              end;

              LTreeView.FullExpand;
              LTreeView.TopItem := LTreeView.Items.GetFirstNode;
            end
            else
            begin
              for LPenaltyIndex := 0 to ReservoirPenaltyDialog.GrdReservoir.RowCount - 1 do
                ReservoirPenaltyDialog.GrdReservoir.Cells[4 + LComboIndex, LPenaltyIndex + 1] := '';
              LTreeView := ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LComboIndex];
              LTreeView.Items.Clear;
              LTreeView.PennaltyStructureNumber := LPenaltyStructID;
            end;
          end;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnRemoveColumn(ASender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnRemoveColumn';
var
  LIndex: integer;
begin
  try
    LIndex := ReservoirPenaltyDialog.GrdReservoir.Col - 4;
    for LIndex := LIndex+1 to ReservoirPenaltyDialog.NumberOfComboBoxes -1 do
    begin
      ReservoirPenaltyDialog.ComboBoxByIndex[LIndex -1].ItemIndex :=
      ReservoirPenaltyDialog.ComboBoxByIndex[LIndex].ItemIndex;
      ReservoirPenaltyDialog.ComboBoxByIndex[LIndex].OnChange(ReservoirPenaltyDialog.ComboBoxByIndex[LIndex]);
    end;
    ReservoirPenaltyDialog.NumberOfComboBoxes := ReservoirPenaltyDialog.NumberOfComboBoxes - 1;
    ReservoirPenaltyDialog.ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.RestoreState: boolean;
const OPNAME = 'TReservoirPenaltyValidator.RestoreState';
begin
  Result := False;
  try
    // This cannot be tested as yet, we need to be able to save before we can restore -- but that is is still work in progress
    // The SaveState must be tesed extensively first.
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.SaveState: boolean;
const OPNAME = 'TReservoirPenaltyValidator.SaveState';
var
  LIndex : integer;
begin
  Result := inherited SaveState;
  try
    FAppModules.ViewIni.WriteString(Self.ClassName,'MODEL', FAppModules.StudyArea.ModelCode);
    FAppModules.ViewIni.WriteString(Self.ClassName,'STUDYAREA', FAppModules.StudyArea.StudyAreaCode);
    FAppModules.ViewIni.WriteString(Self.ClassName,'SUBAREA', FAppModules.StudyArea.SubAreaCode);
    FAppModules.ViewIni.WriteString(Self.ClassName,'SCENARIO', FAppModules.StudyArea.ScenarioCode);

    // This has not been tested as yet, we need to be able to save and restore -- but this is still work in progress
    for LIndex := 0 to ReservoirPenaltyDialog.NumberOfComboBoxes - 1 do
      FAppModules.ViewIni.WriteString(Self.ClassName, Format('COMBOBOX%d', [LIndex]),
        ReservoirPenaltyDialog.ComboBoxByIndex[LIndex].Items.Strings[ReservoirPenaltyDialog.ComboBoxByIndex[LIndex].ItemIndex]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.UpdateValue(ASender : TFieldEdit) : boolean;
const OPNAME = 'TReservoirPenaltyValidator.UpdateValue';
var
  LMessage: string;
  LPenaltyStructureList : IReservoirPenaltyList;
begin
  // This function is duplicated across most validators but as per the discussion with Dziedzi
  // it will remain duplicated and its place re-evaluated at some future time
  Result := False;
  try
    if (ASender.HasValueChanged) then
    begin
      LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
      if (LPenaltyStructureList <> nil) then
      begin
        Result := FAppModules.FieldProperties.ValidateFieldProperty(ASender.FieldProperty.FieldName,
             ASender.Text,LMessage);
        ASender.FieldValidationError := LMessage;
        if Result then
        begin
          if(ASender = ReservoirPenaltyDialog.EdtRuleCurve) then
          begin
            TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList.
              ReservoirPenaltyCounts.ZoneRuleCurve := StrToInt(TFieldEdit(ASender).Text);
            ValidateAllPenaltyStructures;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnStringGridCellDataHasChanged(
  ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirPenaltyValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if ACol < 4 then
      UpdateValue(ACol, ARow, TFieldStringGrid(ASender).Cells[ACol, ARow])
    else
    if ((ReservoirPenaltyDialog.GrdPenalty = ASender) AND
        (NOT ReservoirPenaltyDialog.GrdPenalty.HasChanges[ACol,ARow])) then
      UpdatePenaltyValue(TFieldStringGrid(ASender).Cells[ACol, ARow], ARow, ACol - 4)
    else if ReservoirPenaltyDialog.GrdReservoir = ASender then
      if Assigned(FAppModules.Model().ModelData()) then
        if TComboBox(ReservoirPenaltyDialog.ComboBoxByIndex[ACol- 4]).ItemIndex >= 0 then
          UpdatePenaltyValue(TFieldStringGrid(ASender).Cells[ACol, ARow], ARow, ACol - 4)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.UpdateValue(ACol, ARow: integer;
  AValue: string): boolean;
const OPNAME = 'TReservoirPenaltyValidator.UpdateValue';
begin
  Result := False;
  try
    case ACol
      of 0 : UpdatePenaltyZoneName(AValue, ARow - 1);
      1..3 : UpdatePenaltyZonePolicy(AValue, ARow - 1, ACol - 1);
    else
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.UpdatePenaltyZoneName(
  AValue: string; AZone: integer): boolean;
const OPNAME = 'TReservoirPenaltyValidator.UpdatePenaltyZoneName';
var
  LPenaltyObject : IReservoirPenaltyList;
begin
  Result := False;
  try
    LPenaltyObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
    if (LPenaltyObject <> nil) and
       (LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].ZoneName <> AValue) then
    begin
      LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].ZoneName := AValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.UpdatePenaltyZonePolicy(
  AValue: string; AZone, APolicy: integer): boolean;
const OPNAME = 'TReservoirPenaltyValidator.UpdatePenaltyZonePolicy';
var
  LPenaltyObject : IReservoirPenaltyList;
  LErrorMessage: string;
begin
  Result := False;
  try
    LPenaltyObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
    if (LPenaltyObject <> nil) then
    begin
      ReservoirPenaltyDialog.GrdPenalty.ValidationError[APolicy+1, AZone+1, gveCellField] := '';
      if FAppModules.FieldProperties.ValidateFieldProperty(
         ReservoirPenaltyDialog.GrdPenalty.FieldProperty(APolicy+1).FieldName,AValue,LErrorMessage) then
      begin
        case APolicy of
          0 :
            begin
              if (LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].StrategyIndicator <> StrToInt(AValue)) then
                LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].StrategyIndicator := StrToInt(AValue);
            end;
          1 :
            begin
              if (LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].BalancingVariable <> StrToInt(AValue)) then
                LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].BalancingVariable := StrToInt(AValue);
            end;
          2 :
            begin
              if (LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].BalancingPolicy <> StrToInt(AValue)) then
                LPenaltyObject.ReservoirPenaltyZoneByIndex[AZone].BalancingPolicy := StrToInt(AValue);
            end;
        end;
      end
      else
        ReservoirPenaltyDialog.GrdPenalty.ValidationError[APolicy+1, AZone+1, gveCellField] := LErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.UpdatePenaltyValue(AValue: string; AZone, AStructure: integer): boolean;
const OPNAME = 'TReservoirPenaltyValidator.UpdatePenaltyValue';
var
   LPenaltyListObject : TReservoirPenaltyStructureList;
   LPenaltyObject     : TReservoirPenalty;
   LValidationMsg     : string;
   LTreeView          : TReservoirPenaltyTreeView;
   LPenaltyStructID   : integer;
begin
  Result := False;
  try
    LPenaltyListObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirPenaltyStructureList;
    if Assigned(LPenaltyListObject) then
    begin
      ReservoirPenaltyDialog.GrdPenalty.ValidationError[AStructure+4,AZone+1,gveCellField] := '';
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty
               ('ReservoirPenalty',AValue,LValidationMsg,AZone)) then
        ReservoirPenaltyDialog.GrdPenalty.ValidationError[AStructure+4,AZone+1,gveCellField] := LValidationMsg
      else
      begin
        LPenaltyObject := nil;
        if (ReservoirPenaltyDialog.RadMode.ItemIndex = 1) then
        begin
          LTreeView := ReservoirPenaltyDialog.ReservoirTreeViewByIndex[AStructure];
          if Assigned(LTreeView) then
          begin
            LPenaltyStructID := LTreeView.PennaltyStructureNumber;
            LPenaltyObject   := LPenaltyListObject.CastReservoirPenaltyStructureByIdentifier[LPenaltyStructID];
          end;
        end
        else
          LPenaltyObject := LPenaltyListObject.CastReservoirPenaltyStructureByIndex[AStructure];
        if Assigned(LPenaltyObject) then
        begin
          if (LPenaltyObject.ReservoirPenaltyValueByIndex[AZone] <> StrToFloat(AValue)) then
          begin
            LPenaltyObject.ReservoirPenaltyValueByIndex[AZone] := StrToFloat(AValue);
            ReservoirPenaltyDialog.GrdPenalty.ValidationError[AStructure+4,AZone+1,gveColContext] := '';
            ValidatePenaltyStructure(AStructure,LPenaltyObject);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.PopulateDataViewer;
const OPNAME = 'TReservoirPenaltyValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtReservoirPenalty);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnTreeViewStartDrag(Sender: TObject; var DragObject: TDragObject);
const OPNAME = 'TReservoirPenaltyValidator.OnTreeViewStartDrag';
var
  LTreeView: TReservoirPenaltyTreeView;
begin
  try
    FDragFromTreeView := nil;
    FDragNode         := nil;
    case ViewMode of
      vmSelect:
      begin
        CancelDrag;
      end;
      vmEditableSelect:
      begin
        if Assigned(Sender) and Sender.ClassNameIs('TReservoirPenaltyTreeView') then
        begin
          LTreeView := TReservoirPenaltyTreeView(Sender);
          if Assigned(LTreeView.Selected) and (LTreeView.Selected.Level = 1) then
          begin
            if(LTreeView.Selected.Data <> nil) and
              (TReservoirData(LTreeView.Selected.Data).ReservoirConfigurationData.ReservoirIdentifier = FReservoirNumber) then
            begin
              FDragFromTreeView := LTreeView;
              FDragNode := LTreeView.Selected;
            end
            else
              CancelDrag;
          end
          else
            CancelDrag;
        end
        else
          CancelDrag;
      end;
      vmEditable:
      begin
        if Assigned(Sender) and Sender.ClassNameIs('TReservoirPenaltyTreeView') then
        begin
          LTreeView := TReservoirPenaltyTreeView(Sender);
          if Assigned(LTreeView.Selected) and (LTreeView.Selected.Level = 1) then
          begin
            FDragFromTreeView := LTreeView;
            FDragNode := LTreeView.Selected;
          end
          else
            CancelDrag;
        end
        else
          CancelDrag;
      end;
    end;//case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnTreeViewDragOver(Sender,Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
const OPNAME = 'TReservoirPenaltyValidator.OnTreeViewDragOver';
begin
  try
    Accept := false;
    {Only accept drag and drop from a TTreeView}
     if(Sender <> FDragFromTreeView ) then
          Accept := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnTreeViewDragDrop(Sender,Source: TObject; X, Y: Integer);
const OPNAME = 'TReservoirPenaltyValidator.OnTreeViewDragDrop';
var
  LDropTreeView : TReservoirPenaltyTreeView;
  LNewNode,
  LDropNode : TTreeNode;
begin
  try
    {Only accept drag and drop from a TTreeView}
     if(Sender <> FDragFromTreeView ) then
     begin
       LDropTreeView := TReservoirPenaltyTreeView(Sender);
       LDropNode := LDropTreeView.GetNodeAt(X, Y);
       if not Assigned(LDropNode) then
       begin
         if(LDropTreeView.Items.Count > 0) then
           LDropNode := LDropTreeView.Items[0];
       end;
       if Assigned(LDropNode) and (LDropNode.Level = 1)then
         LDropNode := LDropNode.Parent;
       if Assigned(LDropNode) and (LDropNode.Level = 0)then
       begin
         LNewNode := LDropTreeView.Items.AddChildObject(LDropNode,FDragNode.Text,FDragNode.Data);
         if Assigned(LNewNode) then
         begin
           FDragFromTreeView.Items.Delete(FDragNode);
           if Assigned(LNewNode.Data) then
              TReservoirData(LNewNode.Data).ReservoirConfigurationData.PenaltyStructIdentifier := LDropTreeView.PennaltyStructureNumber;
           LDropTreeView.AlphaSort;
           LDropTreeView.FullExpand;
         end;
       end;
     end;
    FDragFromTreeView := nil;
    FDragNode         := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.SetPenaltyStructureNumber(const Value: integer);
const OPNAME = 'TReservoirPenaltyValidator.SetPenaltyStructureNumber';
begin
  try
    FPenaltyStructureNumber := Value;
    ReservoirPenaltyDialog.SelectedPenaltyStructureCol := Value + 3;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.SetReservoirNumber(const AValue: integer);
const OPNAME = 'TReservoirPenaltyValidator.SetReservoirNumber';
var
  LReservoir: IReservoirData;
begin
  try
    FReservoirNumber := AValue;
    LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ReservoirList.ReservoirByIdentifier[AValue];
    if (LReservoir <> nil) and (LReservoir.ReservoirPenaltyStructureData <> nil) then
      PenaltyStructureNumber := LReservoir.ReservoirPenaltyStructureData.ReservoirPenaltyID
    else
      PenaltyStructureNumber := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnStringGridColEnter(Sender: TObject;ACol, ARow: integer);
const OPNAME = 'TReservoirPenaltyValidator.OnStringGridColEnter';
begin
  inherited OnStringGridColEnter(Sender,ACol, ARow);
  try
    if (Sender = ReservoirPenaltyDialog.GrdPenalty) then
    begin
      if(FViewMode in [vmSelect,vmEditableSelect]) and (ACol >3)then
      begin
        FSelectedPenaltyStructure := Max(0,ACol - 3);
        ReservoirPenaltyDialog.SelectedPenaltyStructureCol := ACol;
        ReservoirPenaltyDialog.ShowSelectPenaltyStructure;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnAddPenaltyClick(Sender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnAddPenaltyClick';
var
  LCopyFromPenaltyID,
  LInsertAfterPenaltyID,
  LPenaltyCount,
  LIndex: integer;
begin
  try
    LPenaltyCount := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList.PenaltyCount;
    LCopyFromPenaltyID    := -1;
    LInsertAfterPenaltyID := -1;
    frmPenaltyAction := TfrmPenaltyAction.Create(nil);
    try
      frmPenaltyAction.CmbCopy.Items.Clear;
      frmPenaltyAction.CmbInsert.Items.Clear;
      frmPenaltyAction.CmbDelete.Items.Clear;
      for LIndex := 1 to LPenaltyCount do
      begin
        frmPenaltyAction.CmbCopy.Items.Add('Penalty Structure ' +IntToStr(LIndex));
        frmPenaltyAction.CmbInsert.Items.Add('Penalty Structure ' +IntToStr(LIndex));
        frmPenaltyAction.CmbDelete.Items.Add('Penalty Structure ' +IntToStr(LIndex));
      end;
      frmPenaltyAction.CmbCopy.Items.Add(FAppModules.language.GetString('TReservoirPenaltyValidator.NewPenalty'));
      frmPenaltyAction.CmbInsert.Items.Add(FAppModules.language.GetString('TReservoirPenaltyValidator.AddLast'));

      frmPenaltyAction.CmbCopy.ItemIndex   := Max(0,FSelectedPenaltyStructure-1);
      frmPenaltyAction.CmbInsert.ItemIndex := LPenaltyCount;
      frmPenaltyAction.CmbDelete.ItemIndex := Max(0,FSelectedPenaltyStructure-1);

      frmPenaltyAction.Caption := FAppModules.language.GetString('FrmCaption.AddPenalty');

      frmPenaltyAction.SetPenaltyAction(paAdd);
      frmPenaltyAction.ShowModal;
      if(frmPenaltyAction.ModalResult = mrOk) then
      begin
       if(frmPenaltyAction.CmbCopy.ItemIndex >= 0) and
         (frmPenaltyAction.CmbInsert.ItemIndex >= 0) then
       begin
          if(frmPenaltyAction.CmbCopy.ItemIndex = (frmPenaltyAction.CmbCopy.Items.Count -1)) then
            LCopyFromPenaltyID    := 0
          else
            LCopyFromPenaltyID    := frmPenaltyAction.CmbCopy.ItemIndex + 1;
          if(frmPenaltyAction.CmbInsert.ItemIndex = (frmPenaltyAction.CmbInsert.Items.Count -1)) then
            LInsertAfterPenaltyID    := frmPenaltyAction.CmbInsert.Items.Count
          else
            LInsertAfterPenaltyID    := frmPenaltyAction.CmbInsert.ItemIndex + 1;
       end;
      end;
    finally
      frmPenaltyAction.Free;
    end;
    if(LCopyFromPenaltyID >= 0) and
      (LInsertAfterPenaltyID > 0) then
    begin
      if TYieldModelDataObject(FAppModules.Model.ModelData).
         CastNetworkElementData.CastReservoirPenaltyStructureList.AddPenaltyStructure(LCopyFromPenaltyID,LInsertAfterPenaltyID) then
      begin
         RePopulateDataViewer;
         ReservoirPenaltyDialog.LanguageHasChanged;
         ReservoirPenaltyDialog.Resize;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnDeletePenaltyClick(Sender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnDeletePenaltyClick';
var
  LDeletePenaltyID,
  LPenaltyCount,
  LIndex: integer;
begin
  try
    LPenaltyCount := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList.PenaltyCount;
    if (LPenaltyCount = 0) then Exit;
    LDeletePenaltyID := -1;
    frmPenaltyAction := TfrmPenaltyAction.Create(nil);
    try
      frmPenaltyAction.CmbCopy.Items.Clear;
      frmPenaltyAction.CmbInsert.Items.Clear;
      frmPenaltyAction.CmbDelete.Items.Clear;
      for LIndex := 1 to LPenaltyCount do
      begin
        frmPenaltyAction.CmbCopy.Items.Add('Penalty Structure ' +IntToStr(LIndex));
        frmPenaltyAction.CmbInsert.Items.Add('Penalty Structure ' +IntToStr(LIndex));
        frmPenaltyAction.CmbDelete.Items.Add('Penalty Structure ' +IntToStr(LIndex));
      end;
      frmPenaltyAction.CmbCopy.Items.Add('New Penalty');
      frmPenaltyAction.CmbInsert.Items.Add('New Penalty');

      frmPenaltyAction.CmbCopy.ItemIndex   := Max(0,FSelectedPenaltyStructure-1);
      frmPenaltyAction.CmbInsert.ItemIndex := LPenaltyCount;
      frmPenaltyAction.CmbDelete.ItemIndex := Max(0,FSelectedPenaltyStructure-1);

      frmPenaltyAction.Caption := FAppModules.language.GetString('FrmCaption.DeletePenalty');

      frmPenaltyAction.SetPenaltyAction(paDelete);
      frmPenaltyAction.ShowModal;
      if(frmPenaltyAction.ModalResult = mrOk) then
      begin
       if(frmPenaltyAction.CmbDelete.ItemIndex >= 0) then
         LDeletePenaltyID := frmPenaltyAction.CmbDelete.ItemIndex + 1;
      end;
    finally
      frmPenaltyAction.Free;
    end;
    if(LDeletePenaltyID >= 0) then
    begin
      if TYieldModelDataObject(FAppModules.Model.ModelData).
         CastNetworkElementData.CastReservoirPenaltyStructureList.DeletePenaltyStructure(LDeletePenaltyID) then
      begin
        RePopulateDataViewer;
        ReservoirPenaltyDialog.LanguageHasChanged;
        ReservoirPenaltyDialog.Resize;
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnAddZoneClick(Sender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnAddZoneClick';
var
  LZoneNumber: integer;
begin
  try
    if(ReservoirPenaltyDialog.NumberOfStorageZones = 0) then
      LZoneNumber := 1
    else
     LZoneNumber := ReservoirPenaltyDialog.GrdPenalty.Row -1;

    if Assigned(TYieldModelDataObject(FAppModules.Model.ModelData).
       CastNetworkElementData.CreateReservoirsZone(LZoneNumber,FReservoirNumber)) then
    begin
       RePopulateDataViewer;
       ReservoirPenaltyDialog.Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.OnDeleteZoneClick(Sender: TObject);
const OPNAME = 'TReservoirPenaltyValidator.OnDeleteZoneClick';
var
  LZoneNumber: integer;
begin
  try
    if(ReservoirPenaltyDialog.NumberOfStorageZones = 0) then
      LZoneNumber := 1
    else
      LZoneNumber := ReservoirPenaltyDialog.GrdPenalty.Row -1;
    if TYieldModelDataObject(FAppModules.Model.ModelData).
      CastNetworkElementData.DeleteReservoirsZone(LZoneNumber) then
    begin
      RePopulateDataViewer;
      ReservoirPenaltyDialog.Resize;
    end
    else
    begin
      RePopulateDataViewer;
      ReservoirPenaltyDialog.Resize;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.ValidatePenaltyStructure(APenaltyIndex: integer;
          APenaltyData: IReservoirPenalty);
const OPNAME = 'TReservoirPenaltyValidator.ValidatePenaltyStructure';
var
  LIndex: integer;
begin
  try
    if Assigned(APenaltyData) then
    begin
     FErrorMessage := '';
     if (NOT APenaltyData.Validate(FErrorMessage,'RuleCurve')) then
       FAllErrorMessages.Add(FErrorMessage);
     for LIndex := 1 to ReservoirPenaltyDialog.GrdPenalty.RowCount-1 do
       ReservoirPenaltyDialog.GrdPenalty.ValidationError[APenaltyIndex+4,LIndex,gveColContext] := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirPenaltyValidator.DoContextValidation';
begin
  try
    ValidateAllPenaltyStructures;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyValidator.ValidateAllPenaltyStructures;
const OPNAME = 'TReservoirPenaltyValidator.ValidateAllPenaltyStructures';
var
  LIndex,
  LPenaltyStructID : integer;
  LTreeView        : TReservoirPenaltyTreeView;
  lPenaltyList     : IReservoirPenaltyList;
  LPenaltyObject   : IReservoirPenalty;
begin
  try
    FAllErrorMessages.Clear;
    lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
    if Assigned(lPenaltyList) then
    begin
      if (ReservoirPenaltyDialog.RadMode.ItemIndex = 1) then
      begin
        for LIndex := 0 to ReservoirPenaltyDialog.NumberOfPenaltyStructures -1 do
        begin
          LTreeView := ReservoirPenaltyDialog.ReservoirTreeViewByIndex[LIndex];
          if Assigned(LTreeView) then
          begin
            LPenaltyStructID := LTreeView.PennaltyStructureNumber;
            LPenaltyObject   := lPenaltyList.ReservoirPenaltyByIdentifier[LPenaltyStructID];
            if Assigned(LPenaltyObject) then
              ValidatePenaltyStructure(LIndex,LPenaltyObject);
          end;
        end;
      end
      else
      begin
        for LIndex := 0 to lPenaltyList.PenaltyCount -1 do
        begin
          LPenaltyObject   := lPenaltyList.ReservoirPenaltyByIndex[LIndex];
          if Assigned(LPenaltyObject) then
            ValidatePenaltyStructure(LIndex,LPenaltyObject);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TReservoirPenaltyValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lPenalty       : IReservoirPenalty;
  lPenaltyIndex  : integer;
  lZoneIndex     : integer;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      with ReservoirPenaltyDialog do
      begin
        if (FActiveControl = GrdPenalty) AND (GrdPenalty.Col >= 4) then
        begin
          lPenaltyIndex := GrdPenalty.Col - 4;
          lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirPenaltyStructureList.
                          ReservoirPenaltyByIndex[lPenaltyIndex];
          if (lPenalty <> nil) then
          begin
            lZoneIndex := GrdPenalty.Row;
            if (lZoneIndex <= lPenalty.PenaltyValueCount) then
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
              if (lFieldProperty <> nil) then
              begin
                lKeyValues := lPenalty.GetKeyValues(lFieldProperty.FieldName, IntToStr(lZoneIndex));
                FAppModules.Changes.ShowParameterChanges
                  (lFieldProperty.FieldName, lKeyValues, IntToStr(lPenaltyIndex+1));
                Result := TRUE;
                RePopulateDataViewer;
                DoContextValidation(dvtReservoirPenalty);
                FAppModules.Changes.SetParameterChanges(TRUE);
                Result := TRUE;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TReservoirPenaltyValidator.ProcessMetaDataEvent';
var
  lFieldProperty     : TAbstractFieldProperty;
  lKeyValues         : string;
  lPenalty           : IReservoirPenalty;
  lPenaltyCountsData : IReservoirPenaltyCounts;
  lPenaltyIndex      : integer;
  lZoneIndex         : integer;
  lFieldIndex        : string;
  lReservoir         : IReservoirPenaltyZoneData;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      with ReservoirPenaltyDialog do
      begin
        if (FActiveControl = GrdPenalty) AND (GrdPenalty.Col >= 4) then
        begin
          lPenaltyIndex := GrdPenalty.Col - 4;
          lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirPenaltyStructureList.
                          ReservoirPenaltyByIndex[lPenaltyIndex];
          if (lPenalty <> nil) then
          begin
            lZoneIndex := GrdPenalty.Row;
            if (lZoneIndex <= lPenalty.PenaltyValueCount) then
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirPenalty');
              if (lFieldProperty <> nil) then
              begin
                lKeyValues := lPenalty.GetKeyValues(lFieldProperty.FieldName, IntToStr(lZoneIndex));
                FAppModules.MetaData.ShowMetaData
                  (lFieldProperty.FieldName, lKeyValues, IntToStr(lPenaltyIndex+1));
                Result := TRUE;
              end;
            end;
          end;
        end
        else
        if (FActiveControl = EdtRuleCurve) then
        begin
          LPenaltyCountsData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                           ReservoirPenaltyStructureList.ReservoirPenaltyCounts;
          if (LPenaltyCountsData <> nil) then
          begin
            lFieldIndex := '';
            lFieldProperty := EdtRuleCurve.FieldProperty;
            lKeyValues := LPenaltyCountsData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end
        else
        if (FActiveControl = ChkBalancingData) then
        begin
          LPenaltyCountsData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                           ReservoirPenaltyStructureList.ReservoirPenaltyCounts;
          if (LPenaltyCountsData <> nil) then
          begin
            lFieldIndex := '';
            lFieldProperty := ChkBalancingData.FieldProperty;
            lKeyValues := LPenaltyCountsData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end
        else
        if ((FActiveControl = GrdPenalty) AND (GrdPenalty.Col < 4)) then
        begin
          lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirPenaltyStructureList.ReservoirPenaltyZoneByIndex[GrdPenalty.Col];
          if (lReservoir <> nil) then
          begin
            lFieldIndex := IntToStr(GrdPenalty.Col) + ',' + IntToStr(GrdPenalty.Row);
            lFieldProperty := GrdPenalty.FieldProperty(GrdPenalty.Col);
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyValidator.DetermineWizardStatus(ASequence: integer): integer;
const OPNAME = 'TReservoirPenaltyValidator.DetermineWizardStatus';
var
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    begin
      LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
      if (LPenaltyStructureList <> nil) then
      begin
        LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
        if (LPenaltyCountsData <> nil) then
        begin
          DoContextValidation(dvtReservoirPenalty);
          if (LPenaltyCountsData.PenaltyStructureCount > 0) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



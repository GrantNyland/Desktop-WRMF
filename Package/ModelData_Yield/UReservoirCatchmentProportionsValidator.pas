//
//
//  UNIT      : Contains the class TReservoirCatchmentProportionsValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirCatchmentProportionsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,     VCL.dialogs,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UReservoirCatchmentProportionsDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TReservoirCatchmentProportionsValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FDragNode         : TTreeNode;
    FDragFromTreeView : TFieldTreeView;
    FBusyUpdating     : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(ASender: TObject); override;

    procedure OnCatchmentComboBoxChange(Sender: TObject);
    procedure OnRadMonthlyAnnualClick(Sender: TObject);
    procedure OnChkMARClick(Sender: TObject);

    procedure OnStringGridKeyPress(Sender: TObject; var Key: Char);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnGridSetEditText(Sender: TObject; ACol, ARow: Longint; const Value: String);
    procedure OnNodeSelectionChange(Sender: TObject; ANode: TTreeNode);
    procedure OnAddRemoveBtnClick(Sender: TObject);

    procedure OnTreeViewStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure OnTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OnCurrentCatchmentTreeViewHasChanged;

    procedure SelectCurrentCatchment(ANode: TTreeNode);
    procedure AddReservoirToCurrentCatchment(ANode: TTreeNode);
    procedure RemoveCurrentCatchment(ANode: TTreeNode);
    procedure RemoveReservoirFromCurrentCatchment(ANode: TTreeNode);

    procedure PopulateAllCatchmentTreeView;
    procedure PopulateProportionsGrid(ATotalsOnly: boolean = False);
    procedure PopulateChartSeries;
    procedure PopulateFileNames(AFileNames: TStringList);
    procedure PopulateTimeSeriesChart(AFileNames: TStringList);
    procedure SelectCurrentReservoir;
    procedure PopulateReservoirTreeView(AReservoirObject: IReservoirData);
    procedure PopulateNodeChart(AReservoirObject: IReservoirData);

    procedure SetNodeChartData(AReservoirObject: IReservoirData);
    procedure CalculateMovedNodeProportions(AReservoirObject: IReservoirData);
    function UpdateValue(ASender: TFieldEdit) : boolean;
    function GetMonthLabel(AMonthValue : integer) : string;

    procedure RePopulateDataViewer;
    procedure SetViewMode(AViewMode: TViewMode);override;
    function CopyNodeToTreeView(ASourceNode,ADestNode: TTreeNode; ASourceTreeView,ADestTreeView: TTreeView): boolean;
    function MoveNodeToTreeView(ASourceNode,ADestNode: TTreeNode; ASourceTreeView,ADestTreeView: TTreeView): boolean;
    function FindNodeByCaption(ACaption: string; ATreeNodes: TTreeNodes): TTreeNode;
    function FindNodeByCatchmentRef(ACatchmentRef: integer; ATreeNodes: TTreeNodes): TTreeNode;
    function IsCurrentNodeSpecifiedReservoir(ATreeNode: TTreeNode): boolean;
    function CurrentReservoir:IReservoirData;
    procedure UpdateDrainageScale (AReservoirData : IReservoirData;
                                   ARow           : integer);
    procedure UpdateAfforestationScale(AReservoirData : IReservoirData;
                                       ARow           : integer);
    procedure UpdateIrrigationScale(AReservoirData : IReservoirData;
                                    ARow           : integer);
    procedure UpdateUrbanRunOff(AReservoirData : IReservoirData;
                                ARow           : integer);
    procedure UpdateCatchmentArea;
    procedure ValidateCatchmentRefNumber(AReservoir : IReservoirData);
    procedure ValidateSumDrainageScale(AReservoir : IReservoirData);
    procedure ValidateSumAfforestationScale(AReservoir: IReservoirData);
    procedure ValidateSumIrrigationScale(AReservoir: IReservoirData);
    procedure ValidateSumUrbanRunOff(AReservoir: IReservoirData);
    procedure ValidateCatchmentArea(AReservoir: IReservoirData);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirCatchmentProportionsDialog: TReservoirCatchmentProportionsDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  // DWAF
  SysUtils,
  VCLTee.TeEngine,
  VCLTee.Chart,
  VCLTee.TeeProcs,
  VCLTee.Series,
  Variants,
  Math,
  VCL.Graphics,
  Contnrs,


  // arivia.kom
  UConstants,
  UParameterData,
  UDataSetType,
  UReservoirData,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TReservoirCatchmentProportionsValidator }

procedure TReservoirCatchmentProportionsValidator.CreateMemberObjects;
const OPNAME = 'TReservoirCatchmentProportionsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FBusyUpdating := False;
    FPanel := TReservoirCatchmentProportionsDialog.Create(FPanelOwner,FAppModules);

    ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.OnChange := OnNodeSelectionChange;
    ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.OnStartDrag := OnTreeViewStartDrag;
    ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.OnDragOver := OnTreeViewDragOver;
    ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.OnDragDrop := OnTreeViewDragDrop;

    ReservoirCatchmentProportionsDialog.TvwAllCatchment.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.TvwAllCatchment.OnChange := OnNodeSelectionChange;
    ReservoirCatchmentProportionsDialog.TvwAllCatchment.OnStartDrag := OnTreeViewStartDrag;
    ReservoirCatchmentProportionsDialog.TvwAllCatchment.OnDragOver := OnTreeViewDragOver;
    ReservoirCatchmentProportionsDialog.TvwAllCatchment.OnDragDrop := OnTreeViewDragDrop;

    ReservoirCatchmentProportionsDialog.BtnAdd.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.BtnAdd.OnClick := OnAddRemoveBtnClick;
    ReservoirCatchmentProportionsDialog.BtnAdd.OnEnter := OnEditControlEnter;

    ReservoirCatchmentProportionsDialog.BtnRemove.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.BtnRemove.OnClick := OnAddRemoveBtnClick;
    ReservoirCatchmentProportionsDialog.BtnRemove.OnEnter := OnEditControlEnter;

    ReservoirCatchmentProportionsDialog.GrdProportions.OnKeyPress := OnStringGridKeyPress;
    ReservoirCatchmentProportionsDialog.GrdProportions.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReservoirCatchmentProportionsDialog.GrdProportions.OnColEnter := OnStringGridColEnter;
    ReservoirCatchmentProportionsDialog.GrdProportions.OnSetEditText := OnGridSetEditText;

    ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DrainageScale'));
    if(TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionCount > 0) then
      ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Identifier'))
    else
      ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AfforestationScale'));
    ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IrrigationScale'));
    if (FAppModules.Model.ModelName = CPlanning) then
      ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('UrbanRunOff'));
    ReservoirCatchmentProportionsDialog.GrdProportions.OnEnter := OnEditControlEnter;

    ReservoirCatchmentProportionsDialog.RadFiles.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.RadFiles.OnEnter := OnEditControlEnter;
    ReservoirCatchmentProportionsDialog.RadFiles.OnExit  := OnEditControltExit;
    //ReservoirCatchmentProportionsDialog.RadFiles.OnClick := OnRadChartViewClick;

    ReservoirCatchmentProportionsDialog.RadChartView.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.RadChartView.OnEnter := OnEditControlEnter;
    ReservoirCatchmentProportionsDialog.RadChartView.OnExit  := OnEditControltExit;
    //ReservoirCatchmentProportionsDialog.RadChartView.OnClick := OnRadChartViewClick;

    ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.OnEnter := OnEditControlEnter;
    ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.OnExit  := OnEditControltExit;
    ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentAreaParam');
    
    ReservoirCatchmentProportionsDialog.RadMonthlyAnnual.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.ChkMAR.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentRef');
    ReservoirCatchmentProportionsDialog.RadMonthlyAnnual.OnClick := OnRadMonthlyAnnualClick;
    ReservoirCatchmentProportionsDialog.ChkMAR.OnClick := OnChkMARClick;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirCatchmentProportionsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.Initialise: boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FDragNode         := nil;
    FDragFromTreeView := nil;
    FIdentifier := -1;
    if (FAppModules.User.UserRights in CUR_EditData) and
       (FAppModules.StudyArea <> nil) and
       (not (FAppModules.StudyArea.ScenarioLocked)) then
    begin
      ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.DragMode := dmAutomatic;
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.DragMode := dmAutomatic;
    end
    else
    begin
      ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.DragMode := dmManual;
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.DragMode := dmManual;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.CatchmentProportions');;
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ClearDataViewer;
const OPNAME = 'TReservoirCatchmentProportionsValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    {ReservoirCatchmentProportionsDialog.EdtFilePropInc.SetFieldValue('-9999.99');
    ReservoirCatchmentProportionsDialog.EdtFilePropAff.SetFieldValue('-9999.99');
    ReservoirCatchmentProportionsDialog.EdtFilePropIrr.SetFieldValue('-9999.99');
    ReservoirCatchmentProportionsDialog.EdtFileSel.Items.Clear;

    ReservoirCatchmentProportionsDialog.EdtFileInc.SetFieldValue('');
    ReservoirCatchmentProportionsDialog.EdtFileRnk.SetFieldValue('');
    ReservoirCatchmentProportionsDialog.EdtFileAff.SetFieldValue('');
    ReservoirCatchmentProportionsDialog.EdtFileIrr.SetFieldValue('');
    ReservoirCatchmentProportionsDialog.EdtFileRan.SetFieldValue('');

    ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.Clear;
    ReservoirCatchmentProportionsDialog.GrdProportions.RowCount := 2;

    ReservoirCatchmentProportionsDialog.ChtTimeSeries.SeriesList.Clear;
    ReservoirCatchmentProportionsDialog.ChtTimeSeries.RemoveAllSeries;
    ReservoirCatchmentProportionsDialog.ChtTimeSeries.UndoZoom;

    ReservoirCatchmentProportionsDialog.ChtNode.UndoZoom;
    ReservoirCatchmentProportionsDialog.ChtNode.Title.Text.Text := '';

    ReservoirCatchmentProportionsDialog.INCPieSeries.ParentChart := nil;
    ReservoirCatchmentProportionsDialog.IRRPieSeries.ParentChart := nil;
    ReservoirCatchmentProportionsDialog.AFFPieSeries.ParentChart := nil;

    ReservoirCatchmentProportionsDialog.ChtNode.SeriesList.Clear;
    ReservoirCatchmentProportionsDialog.ChtNode.MarginTop := 10;
    ReservoirCatchmentProportionsDialog.ChtNode.MarginBottom := 10;
    ReservoirCatchmentProportionsDialog.ChtNode.Align := alClient;
    ReservoirCatchmentProportionsDialog.RadFiles.OnClick(nil);
    }
    ReservoirCatchmentProportionsDialog.RadMonthlyAnnual.ItemIndex := 0;
    ReservoirCatchmentProportionsDialog.ChkMAR.Checked := False;
    ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Clear;
    ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Enabled := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirCatchmentProportionsValidator.RePopulateDataViewer';
{var
  LReservoirObject: IReservoirData;
  LFileNames: TStringList;
  LCatchmentRef: integer;
}
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
     
    end;
    {if(FIdentifier >= 0) then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        //  FormatFloat('###0.###', LReservoirObject.ReservoirConfigurationData.DrainageScale));

        ReservoirCatchmentProportionsDialog.EdtFilePropInc.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirConfigurationData.DrainageScale));
        ReservoirCatchmentProportionsDialog.EdtFilePropAff.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirConfigurationData.AfforestationScale));
        ReservoirCatchmentProportionsDialog.EdtFilePropIrr.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirConfigurationData.IrrigationScale));

        PopulateReservoirTreeView(LReservoirObject);
        PopulateNodeChart(LReservoirObject);

        LCatchmentRef := LReservoirObject.ReservoirConfigurationData.CatchmentRef;
        if(LCatchmentRef > 0) then
        begin
          LFileNames := TStringList.Create;
          try
            TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFilesForCatchment(LCatchmentRef,LFileNames);
            PopulateFileNames(LFileNames);
            PopulateTimeSeriesChart(LFileNames);
          finally
            LFileNames.Free;
          end;
        end;
      end;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateReservoirTreeView(AReservoirObject: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateReservoirTreeView';
var
  lResevoirList   : TObjectList;
  LReservoirName,
  LFileName       : string;
  LCount          : integer;
  LParamReference : IParamReference;
  LMainNode       : TTreeNode;
  LIncTotal,
  LAffTotal,
  LIrrTotal,
  LUrbTotal       : double;
  LReservoirData  : TReservoirData;
  LFieldProperty  : TAbstractFieldProperty;

begin
  try
    if(AReservoirObject = nil) then
      Exit;

    if (AReservoirObject.ReservoirConfigurationData.CatchmentRef > 0) then
    begin
      LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceDataByCatchNumber[
                         AReservoirObject.ReservoirConfigurationData.CatchmentRef];
      if (LParamReference <> nil) then
      begin
        LFileName := LParamReference.FileReference;
        LFileName := ExtractFileName(LFileName);
        lResevoirList := TObjectList.Create(False);
        try
          if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.GetReservoirsAndNodesPerCatchmentRef(
            AReservoirObject.ReservoirConfigurationData.CatchmentRef, lResevoirList) then
          begin
            LMainNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.Add(nil,LFileName);
            ReservoirCatchmentProportionsDialog.GridRowCount := lResevoirList.Count + 2;

            LIncTotal := 0.0;
            LAffTotal := 0.0;
            LIrrTotal := 0.0;
            LUrbTotal := 0.0;
            for LCount := 0 to lResevoirList.Count - 1 do
            begin
              LReservoirData := TReservoirData(lResevoirList.Items[LCount]);
              LReservoirName := LReservoirData.ReservoirConfigurationData.ReservoirName;
              ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.AddChild(LMainNode,LReservoirName);

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('DrainageScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,LCount+1] := Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.DrainageScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.DrainageScale);

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('AfforestationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,LCount+1] :=Format(LFieldProperty.FormatStringGrid,
                                                                                       [LReservoirData.ReservoirConfigurationData.AfforestationScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.AfforestationScale);


              LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,LCount+1] := Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.IrrigationScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.IrrigationScale);

              LIncTotal := LIncTotal + LReservoirData.ReservoirConfigurationData.DrainageScale;
              LAffTotal := LAffTotal + LReservoirData.ReservoirConfigurationData.AfforestationScale;
              LIrrTotal := LIrrTotal + LReservoirData.ReservoirConfigurationData.IrrigationScale;

              if (FAppModules.Model.ModelName = CPlanning) then
              begin
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('UrbanRunOff');
                ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
                ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3,LCount+1] :=Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.UrbanRunOff]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.UrbanRunOff);
                LIncTotal := LIncTotal + LReservoirData.ReservoirConfigurationData.UrbanRunOff;
              end;

            end;
            if(lResevoirList.Count > 0) then
            begin
              LFieldProperty := FAppModules.FieldProperties.FieldProperty('DrainageScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,lResevoirList.Count+1] :=Format(LFieldProperty.FormatStringGrid,[LIncTotal])+'%';

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('AfforestationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,lResevoirList.Count+1] := Format(LFieldProperty.FormatStringGrid,[LAffTotal])+'%';

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,lResevoirList.Count+1] := Format(LFieldProperty.FormatStringGrid,[LIrrTotal])+'%';

{         ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,lResevoirList.Count+1] := FormatFloat('##0.###',LIncTotal)+'%';
          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,lResevoirList.Count+1] := FormatFloat('##0.###',LAffTotal)+'%';
          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,lResevoirList.Count+1] := FormatFloat('##0.###',LIrrTotal)+'%';  }
              if (FAppModules.Model.ModelName = CPlanning) then
              begin
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('UrbanRunOff');
                ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
                ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3,lResevoirList.Count+1] :=Format(LFieldProperty.FormatStringGrid,[LUrbTotal])+'%';// FormatFloat('##0.###',LUrbTotal)+'%';
              end;
            end;
          end;

          TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.Get_ReservoirByIdentifier(1);
          if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.GetReservoirsAndNodesOutSideCatchmentRef(
            AReservoirObject.ReservoirConfigurationData.CatchmentRef,lResevoirList) then
          begin
            LMainNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.Add(nil,'Other');
            for LCount := 0 to lResevoirList.Count - 1 do
            begin
              LReservoirName := TReservoirData(lResevoirList.Items[LCount]).ReservoirConfigurationData.ReservoirName;
              ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.AddChild(LMainNode,LReservoirName);
            end;
          end;
          ReservoirCatchmentProportionsDialog.TvwAllCatchment.FullExpand;
          ReservoirCatchmentProportionsDialog.TvwAllCatchment.TopItem := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.GetFirstNode;
        finally
          lResevoirList.Free;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.SaveState: boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
    if ((Sender = ReservoirCatchmentProportionsDialog.RadChartView) OR
        (Sender = ReservoirCatchmentProportionsDialog.RadFiles)) then
    begin
      DoContextValidation(dvtResCatchAll);
    end;
    {if Sender.ClassNameIs('TFieldEdit') then
    begin
      if Sender = ReservoirCatchmentProportionsDialog.EdtFileInc then
        ReservoirCatchmentProportionsDialog.DispalyINCChart(TFieldEdit(Sender).Text)
      else if Sender = ReservoirCatchmentProportionsDialog.EdtFilePropInc then
        ReservoirCatchmentProportionsDialog.DispalyINCChart(ReservoirCatchmentProportionsDialog.EdtFileInc.Text)

      else if Sender = ReservoirCatchmentProportionsDialog.EdtFileAff then
        ReservoirCatchmentProportionsDialog.DispalyAFFChart(TFieldEdit(Sender).Text)
      else if Sender = ReservoirCatchmentProportionsDialog.EdtFilePropAff then
        ReservoirCatchmentProportionsDialog.DispalyAFFChart(ReservoirCatchmentProportionsDialog.EdtFileAff.Text)

      else if Sender = ReservoirCatchmentProportionsDialog.EdtFileIrr then
        ReservoirCatchmentProportionsDialog.DispalyIRRChart(TFieldEdit(Sender).Text)
      else if Sender = ReservoirCatchmentProportionsDialog.EdtFilePropIrr then
        ReservoirCatchmentProportionsDialog.DispalyIRRChart(ReservoirCatchmentProportionsDialog.EdtFileIrr.Text)

      else if Sender = ReservoirCatchmentProportionsDialog.EdtFileRnk then
        ReservoirCatchmentProportionsDialog.DispalyRNKChart(TFieldEdit(Sender).Text)
      else if Sender = ReservoirCatchmentProportionsDialog.EdtFileRan then
        ReservoirCatchmentProportionsDialog.DispalyRANChart(TFieldEdit(Sender).Text);

    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnEditControltExit';
//var
//  LReservoirObject: IReservoirData;
begin
  inherited OnEditControltExit(ASender);
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      if((ASender = CatchmentAreaEdt) and (CatchmentAreaEdt.HasValueChanged ))then
        UpdateCatchmentArea;
    end;

    {if ASender.ClassName = TFieldEdit.ClassName then
      if TFieldEdit(ASender).HasValueChanged then
        if UpdateValue(TFieldEdit(ASender)) then
        begin
          if TFieldEdit(ASender).Color = clRed then
            TFieldEdit(ASender).Color := clWindow;

          LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[
            FIdentifier];

          if (LReservoirObject <> nil) then
          begin
            if ASender = ReservoirCatchmentProportionsDialog.EdtFilePropInc then
              LReservoirObject.ReservoirConfigurationData.DrainageScale := StrToFloat(TFieldEdit(ASender).Text)
            else if ASender = ReservoirCatchmentProportionsDialog.EdtFilePropAff then
              LReservoirObject.ReservoirConfigurationData.AfforestationScale := StrToFloat(TFieldEdit(ASender).Text)
            else if ASender = ReservoirCatchmentProportionsDialog.EdtFilePropIrr then
              LReservoirObject.ReservoirConfigurationData.IrrigationScale := StrToFloat(TFieldEdit(ASender).Text);
          end;

          RePopulateDataViewer;

        end
      else begin end
    else
      if TFieldEdit(ASender).Color = clRed then
        TFieldEdit(ASender).Color := clWindow;
   }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.ReservoirCatchmentProportionsDialog:TReservoirCatchmentProportionsDialog;
const OPNAME = 'TReservoirCatchmentProportionsValidator.ReservoirCatchmentProportionsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TReservoirCatchmentProportionsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if FBusyUpdating then Exit;
    if(AFieldName = 'ReservoirName') or
      (AFieldName = 'DrainageScale') or
      (AFieldName = 'AfforestationScale') or
      (AFieldName = 'IrrigationScale')then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateNodeChart;
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateNodeChart';
begin
  try
    SetNodeChartData(AReservoirObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateFileNames(AFileNames: TStringList);
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateFileNames';
{var
  LFileExt,
  LFileName: string;
  LCount: integer;
}
begin
  try
    {if (AFileNames = nil) then Exit;

    for LCount := 0 to AFileNames.Count -1 do
    begin
      LFileName := AFileNames[LCount];
      LFileName := ExtractFileName(LFileName);
      LFileExt := UpperCase(ExtractFileExt(LFileName));
      if LFileExt = '.INC' then
        ReservoirCatchmentProportionsDialog.EdtFileInc.SetFieldValue(LFileName);
      if LFileExt = '.RNK' then
        ReservoirCatchmentProportionsDialog.EdtFileRnk.SetFieldValue(LFileName);
      if LFileExt = '.AFF' then
        ReservoirCatchmentProportionsDialog.EdtFileAff.SetFieldValue(LFileName);
      if LFileExt = '.IRR' then
        ReservoirCatchmentProportionsDialog.EdtFileIrr.SetFieldValue(LFileName);
      if LFileExt = '.RAN' then
        ReservoirCatchmentProportionsDialog.EdtFileRan.SetFieldValue(LFileName);
    end; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.SetNodeChartData(AReservoirObject: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.SetNodeChartData';
{var
  LReseroirData: TObjectList;
  LFileName: string;
  LCount: integer;
  LParamReference : IParamReference;
  LReservoirData: IReservoirData;
}
begin
  try
    if(AReservoirObject = nil) then
      Exit;

{    if (AReservoirObject.ReservoirConfigurationData.CatchmentRef > 0) then
    begin
      LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceDataByCatchNumber[
                         AReservoirObject.ReservoirConfigurationData.CatchmentRef];
      LFileName := LParamReference.FileReference;
      LFileName := ExtractFileName(LFileName);
      LReseroirData := TObjectList.Create(False);
      try
        if TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.GetReservoirsAndNodesPerCatchmentRef(
          AReservoirObject.ReservoirConfigurationData.CatchmentRef, LReseroirData) then
        begin
          ReservoirCatchmentProportionsDialog.INCPieSeries.ParentChart := nil;
          ReservoirCatchmentProportionsDialog.IRRPieSeries.ParentChart := nil;
          ReservoirCatchmentProportionsDialog.AFFPieSeries.ParentChart := nil;

          ReservoirCatchmentProportionsDialog.INCPieSeries.Clear;
          ReservoirCatchmentProportionsDialog.IRRPieSeries.Clear;
          ReservoirCatchmentProportionsDialog.AFFPieSeries.Clear;

          for LCount := 0 to LReseroirData.Count - 1 do
          begin
            LReservoirData := IReservoirData(LReseroirData.Items[LCount]);

            ReservoirCatchmentProportionsDialog.INCPieSeries.Add(
              LReservoirData.ReservoirConfigurationData.DrainageScale,
                LReservoirData.ReservoirConfigurationData.ReservoirName);

            ReservoirCatchmentProportionsDialog.IRRPieSeries.Add(
              LReservoirData.ReservoirConfigurationData.IrrigationScale,
                LReservoirData.ReservoirConfigurationData.ReservoirName);

            ReservoirCatchmentProportionsDialog.AFFPieSeries.Add(
              LReservoirData.ReservoirConfigurationData.AfforestationScale,
                LReservoirData.ReservoirConfigurationData.ReservoirName);

          end;
        end;
        
      finally
        LReseroirData.Free;
      end;
    end;}

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.UpdateValue(ASender: TFieldEdit) : boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateValue';
var
  LMessage : String;
begin
  // This function is duplicated across most validators but as per the discussion with Dziedzi
  // it will remain duplicated and its place re-evaluated at some future time
  Result := False;
  try
    if (ASender.HasValueChanged) then
    begin
      Result := FAppModules.FieldProperties.ValidateFieldProperty(
                ASender.FieldProperty.FieldName,ASender.Text,LMessage);
      ASender.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateDataViewer;
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    //LParamReference := nil;
    ClearDataViewer;
    PopulateAllCatchmentTreeView;
    SelectCurrentReservoir;
    PopulateProportionsGrid;
    //RePopulateDataViewer;
    DoContextValidation(dvtResCatchAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnCatchmentComboBoxChange(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnCatchmentComboBoxChange';
begin
  try
    //RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.SetViewMode(AViewMode: TViewMode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.SetViewMode';
begin
  inherited SetViewMode(AViewMode);
  try

  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TReservoirCatchmentProportionsValidator.PopulateAllCatchmentTreeView;
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateAllCatchmentTreeView';
var
  LCatchCount,
  LResCount: integer;
  LParamSetup:TParamSetup;
  LParamReference:TParamReference;
  LReservoirDataList:TReservoirDataList;
  LReservoirList:TObjectList;
  LReservoir:TReservoirData;
  LReferenceNode: TTreeNode;
begin
  try
    ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.Clear;
    LParamSetup := TYieldModelDataObject(FAppModules.Model.ModelData).CastCastParameterData;
    LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList;
    if (LParamSetup <> nil) and (LReservoirDataList <> nil) then
    begin
      LReservoirList := TObjectList.Create(False);
      try
        for LCatchCount := -1 to LParamSetup.ReferenceCount -1 do
        begin
          if(LCatchCount < 0) then
          begin
            LParamReference := TParamReference.Create(FAppModules);
            LParamReference.InitialiseToUnAssigned;
          end
          else
            LParamReference := LParamSetup.CastReferenceDataByIndex[LCatchCount];
          if (LParamReference <> nil) then
          begin
            LReferenceNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.AddObject(
               nil,ExtractFileName(LParamReference.FileReference),TObject(LParamReference.CatchReference));
            if(LCatchCount < 0) then
              LReservoirDataList.GetReservoirsWithNoCatchmentRef(LReservoirList)
            else
              LReservoirDataList.GetReservoirsAndNodesPerCatchmentRef(LParamReference.CatchReference,LReservoirList);
            for LResCount := 0 to LReservoirList.Count - 1 do
            begin
              LReservoir := TReservoirData(LReservoirList.Items[LResCount]);

              if (LReservoir <> nil) and (not (LReservoir.ReservoirConfigurationData.NodeType = ntNodeWithoutInflow))  then
                ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items.AddChildObject(
                LReferenceNode,LReservoir.ReservoirConfigurationData.ReservoirName,
                TObject(LReservoir.ReservoirConfigurationData.ReservoirIdentifier));
            end;
          end;
        end;
      finally
        LReservoirList.Free;
      end;
      ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.IsEnabled := True;
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.IsEnabled := True;
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.AlphaSort(True);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.SelectCurrentReservoir;
const OPNAME = 'TReservoirCatchmentProportionsValidator.SelectCurrentReservoir';
var
  LSelectedReservoir:IReservoirData;
  LNode: TTreeNode;
begin
  try
    //ReservoirCatchmentProportionsDialog.TvwAllCatchment.Color := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Color;
    if(FIdentifier >= 0) then
    begin
      //ReservoirCatchmentProportionsDialog.TvwAllCatchment.Color := clCream;
      LSelectedReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (LSelectedReservoir <> nil) then
      begin
        LNode := FindNodeByCatchmentRef(LSelectedReservoir.ReservoirConfigurationData.CatchmentRef,
                 ReservoirCatchmentProportionsDialog.TvwAllCatchment.Items);
        if (LNode <> nil) then
           SelectCurrentCatchment(LNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnNodeSelectionChange(Sender: TObject; ANode: TTreeNode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnNodeSelectionChange';
var
  LNode,
  LMainNode           : TTreeNode;
  LParamReference     : IParamReference;
  LCatchmentRef       : Integer;
begin
  try
    ReservoirCatchmentProportionsDialog.ShowTreeViewHint(allNone);
    ReservoirCatchmentProportionsDialog.BtnAdd.IsEnabled := False;
    ReservoirCatchmentProportionsDialog.BtnRemove.IsEnabled := False;
    case FViewMode of
      vmEditable:
      begin
        if(ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected <> nil) then
        begin
          ReservoirCatchmentProportionsDialog.BtnAdd.IsEnabled := (ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected.Level = 0);
          ReservoirCatchmentProportionsDialog.BtnAdd.IsEnabled := ReservoirCatchmentProportionsDialog.BtnAdd.IsEnabled or
                                                                  ((ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected.Level = 1) and
                                                                  (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected <> nil));
        end;
        if(ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected <> nil) then
        begin
          ReservoirCatchmentProportionsDialog.BtnRemove.IsEnabled    := (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected.Level = 0);
          ReservoirCatchmentProportionsDialog.BtnRemove.IsEnabled    := ReservoirCatchmentProportionsDialog.BtnRemove.IsEnabled or
                                                                        ((ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected.Level = 1) and
                                                                        (ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected <> nil));
        end;
      end;
      vmSelect,
      vmEditableSelect:
      begin
        if(ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected <> nil) and
          (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected.Level = 1) then
        begin
          ReservoirCatchmentProportionsDialog.BtnRemove.IsEnabled := IsCurrentNodeSpecifiedReservoir(ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected) and
                                                                     (ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected <> nil) and
                                                                     (ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected.Level = 0);
        end;
      end;
    end;//case
    LNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected;
    if (LNode <> nil) then
    begin
      if(LNode.Level = 0) then
      begin
        if(FIdentifier < 0) then
        begin
          ReservoirCatchmentProportionsDialog.ShowTreeViewHint(allCatchment);
        end;
      end
      else
      if(LNode.Level = 1) then
      begin
        if(FIdentifier < 0) then
        begin
          if (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected <> nil) then
          begin
            ReservoirCatchmentProportionsDialog.ShowTreeViewHint(allReservoir);
          end
        end
      end;
    end;

    LNode := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected;
    if (LNode <> nil) then
    begin

      LMainNode  := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode;
      if (LMainNode <> nil) then
      begin
        LCatchmentRef := Integer(LMainNode.Data);
        if(LCatchmentRef > 0) then
        begin
          LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                              ReferenceDataByCatchNumber[LCatchmentRef];
          if LParamReference <> nil then
          begin
            if LParamReference.CatchmentArea = 0 then
              ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.SetFieldValue(1.000)
            else
              ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.SetFieldValue(LParamReference.CatchmentArea);
          end
          else
          begin
            ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Clear;
            ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Enabled := False;
          end;
        end;
      end;

      if(LNode.Level = 0) then
      begin
        if(FIdentifier < 0) then
        begin
          ReservoirCatchmentProportionsDialog.ShowTreeViewHint(curCatchment);
        end;
      end
      else
      if(LNode.Level = 1) then
      begin
        if(FIdentifier < 0) then
        begin
          if(ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected <> nil) then
          begin
            ReservoirCatchmentProportionsDialog.ShowTreeViewHint(curReservoir);
          end;
        end
        else
        begin
          if IsCurrentNodeSpecifiedReservoir(LNode) and
             (ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected <> nil) then
          begin
            ReservoirCatchmentProportionsDialog.ShowTreeViewHint(curReservoir);
          end;
        end;
      end;
    end;

    if(Sender = ReservoirCatchmentProportionsDialog.TvwCurrentCatchment) then
      OnCurrentCatchmentTreeViewHasChanged
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnAddRemoveBtnClick(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnAddRemoveBtnClick';
var
  LNode: TTreeNode;
begin
  try
    FBusyUpdating := True;
    try
      if (Sender = ReservoirCatchmentProportionsDialog.BtnAdd) then
      begin
        LNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected;
        if not (LNode <> nil) then
        else
          if(LNode.Level = 0) then
            SelectCurrentCatchment(LNode)
          else
            if(LNode.Level = 1) then
            AddReservoirToCurrentCatchment(LNode);
      end;
      if (Sender = ReservoirCatchmentProportionsDialog.BtnRemove) then
      begin
        LNode := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected;
        if(LNode = nil) then
        else
          if(LNode.Level = 0) then
            RemoveCurrentCatchment(LNode)
          else
            if(LNode.Level = 1) then
              RemoveReservoirFromCurrentCatchment(LNode);
      end;
    finally
      FBusyUpdating := False;
    end;
    OnNodeSelectionChange(nil,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.AddReservoirToCurrentCatchment(ANode: TTreeNode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.AddReservoirToCurrentCatchment';
var
  LReservoirIdentifier: integer;
  LReservoirName: string;
  LReservoirNode: TTreeNode;
  LReservoirData: IReservoirData;
begin
  try
    if (ANode <> nil) then
    begin
      LReservoirName := ANode.Text;
      if(MoveNodeToTreeView(ANode,ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem,
                       ReservoirCatchmentProportionsDialog.TvwAllCatchment,
                       ReservoirCatchmentProportionsDialog.TvwCurrentCatchment)) then
      begin
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem.Expand(True);
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.AlphaSort(True);
        PopulateProportionsGrid;
        LReservoirNode := FindNodeByCaption(LReservoirName,ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items);
        if (LReservoirNode <> nil) then
        begin
          ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected := LReservoirNode;
          if(LReservoirNode.Level = 1) then
          begin
            LReservoirIdentifier := Integer(LReservoirNode.Data);
            LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LReservoirIdentifier];
            if(LReservoirData <> nil) then
            begin
              LReservoirData.ReservoirConfigurationData.CatchmentRef := Integer(LReservoirNode.Parent.Data);
              CalculateMovedNodeProportions(LReservoirData);
              DoContextValidation(dvtResCatchRef);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.RemoveCurrentCatchment(ANode: TTreeNode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.RemoveCurrentCatchment';
begin
  try
    if (ANode <> nil) then
    begin
      MoveNodeToTreeView(ANode,nil,
                         ReservoirCatchmentProportionsDialog.TvwCurrentCatchment,
                         ReservoirCatchmentProportionsDialog.TvwAllCatchment);
      ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Enabled := True;
      //ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem.Expand(True);
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected := nil;
      ReservoirCatchmentProportionsDialog.TvwAllCatchment.AlphaSort(True);
      OnCurrentCatchmentTreeViewHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.RemoveReservoirFromCurrentCatchment(ANode: TTreeNode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.RemoveReservoirFromCurrentCatchment';
var
  LReservoirName: string;
  LCatchmentNode,
  LReservoirNode: TTreeNode;
  LReservoirIdentifier: integer;
  LReservoirData: IReservoirData;
begin
  try
    if (ANode <> nil) then
    begin
      LCatchmentNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected;
      if (LCatchmentNode <> nil) then
      begin
        if(LCatchmentNode.Level > 0) then
           LCatchmentNode := LCatchmentNode.Parent;
        LReservoirName := ANode.Text;
        if(MoveNodeToTreeView(ANode,LCatchmentNode,
                         ReservoirCatchmentProportionsDialog.TvwCurrentCatchment,
                         ReservoirCatchmentProportionsDialog.TvwAllCatchment)) then
        begin
          if(FIdentifier >= 0) then
          begin
            SelectCurrentCatchment(LCatchmentNode);
            LReservoirNode := FindNodeByCaption(LReservoirName,ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items);
            if (LReservoirNode <> nil) then
            begin
              ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected := LReservoirNode;
              if(LReservoirNode.Level = 1) then
              begin
                LReservoirIdentifier := Integer(LReservoirNode.Data);
                LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LReservoirIdentifier];
                if(LReservoirData <> nil) then
                begin
                  LReservoirData.ReservoirConfigurationData.CatchmentRef := Integer(LReservoirNode.Parent.Data);
                  CalculateMovedNodeProportions(LReservoirData);
                  DoContextValidation(dvtResCatchRef);
                end;
              end;
            end;
          end
          else
          begin
            PopulateProportionsGrid;
            LCatchmentNode.Expand(True);
            LCatchmentNode.Item[0].Owner.AlphaSort(True);
            LReservoirNode := FindNodeByCaption(LReservoirName,LCatchmentNode.Item[0].Owner);
            if (LReservoirNode <> nil) then
            begin
              ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected := LReservoirNode;
              if(LReservoirNode.Level = 1) then
              begin
                LReservoirIdentifier := Integer(LReservoirNode.Data);
                LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LReservoirIdentifier];
                if(LReservoirData <> nil) then
                begin
                  LReservoirData.ReservoirConfigurationData.CatchmentRef := Integer(LReservoirNode.Parent.Data);
                  CalculateMovedNodeProportions(LReservoirData);
                  DoContextValidation(dvtResCatchRef);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.SelectCurrentCatchment(ANode: TTreeNode);
const OPNAME = 'TReservoirCatchmentProportionsValidator.SelectCurrentCatchment';
var
  LNode: TTreeNode;
  LSelectedReservoir:IReservoirData;
begin
  try
    LNode := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem;
    if (LNode <> nil) then
    begin
      if not MoveNodeToTreeView(LNode,nil,
             ReservoirCatchmentProportionsDialog.TvwCurrentCatchment,
             ReservoirCatchmentProportionsDialog.TvwAllCatchment) then
             Exit;
    end;
    
    if (ANode <> nil) then
    begin
      if MoveNodeToTreeView(ANode,nil,
         ReservoirCatchmentProportionsDialog.TvwAllCatchment,
         ReservoirCatchmentProportionsDialog.TvwCurrentCatchment) then
      begin
        LSelectedReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (LSelectedReservoir = nil) then
          LNode :=ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem
        else
          LNode := FindNodeByCaption(LSelectedReservoir.ReservoirConfigurationData.ReservoirName,
                   ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items);

        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.IsEnabled := True;
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem.Expand(True);
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected := LNode;
        ReservoirCatchmentProportionsDialog.TvwAllCatchment.Selected := 
          ReservoirCatchmentProportionsDialog.TvwAllCatchment.TopItem;
        ReservoirCatchmentProportionsDialog.TvwAllCatchment.AlphaSort(True);
        OnCurrentCatchmentTreeViewHasChanged;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.CopyNodeToTreeView(ASourceNode,ADestNode: TTreeNode;
          ASourceTreeView, ADestTreeView: TTreeView): boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.CopyNodeToTreeView';
var
  LCount   : Integer;
  LNewNode : TTreeNode;
begin
  Result := False;
  try
    if (ASourceNode <> nil) and (ASourceTreeView <> nil) and (ADestTreeView <> nil)  then
    begin
      if (ADestNode <> nil) then
        LNewNode := ADestTreeView.Items.AddChild(ADestNode,ASourceNode.Text)
      else
        LNewNode := ADestTreeView.Items.Add(nil,ASourceNode.Text);
      LNewNode.Assign(ASourceNode);

      for LCount := 0 to ASourceNode.Count -1 do
        CopyNodeToTreeView(ASourceNode.Item[LCount],LNewNode,ASourceTreeView, ADestTreeView);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.MoveNodeToTreeView(ASourceNode, ADestNode: TTreeNode;
          ASourceTreeView,ADestTreeView: TTreeView): boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.MoveNodeToTreeView';
var
  LSelectedReservoir:IReservoirData;
  LNode : TTreeNode;
begin
  Result := False;
  try
    LSelectedReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (LSelectedReservoir <> nil) then
    begin
      LNode := FindNodeByCatchmentRef(LSelectedReservoir.ReservoirConfigurationData.CatchmentRef,ADestTreeView.Items);
      if (LNode <> nil) then
      begin
        Result := True;
        ASourceTreeView.Items.Delete(ASourceNode);
        Exit;
      end;
    end;
    Result := CopyNodeToTreeView(ASourceNode, ADestNode,ASourceTreeView,ADestTreeView);
    if Result then
    begin
       ASourceTreeView.Items.Delete(ASourceNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.FindNodeByCaption(ACaption: string; ATreeNodes: TTreeNodes): TTreeNode;
const OPNAME = 'TReservoirCatchmentProportionsValidator.FindNodeByCaption';
var
  LCount: integer;
begin
  Result := nil;
  try
    if (ATreeNodes <> nil) then
    begin
      for LCount := 0 to ATreeNodes.Count -1 do
      begin
        if(ATreeNodes.Item[LCount].Text = ACaption) then
        begin
          Result := ATreeNodes.Item[LCount];
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.FindNodeByCatchmentRef(ACatchmentRef: integer;
         ATreeNodes: TTreeNodes): TTreeNode;
const OPNAME = 'TReservoirCatchmentProportionsValidator.FindNodeByCatchmentRef';
var
  LCount: integer;
begin
  Result := nil;
  try
    if (ATreeNodes <> nil) then
    begin
      for LCount := 0 to ATreeNodes.Count -1 do
      begin
        if(ATreeNodes.Item[LCount].Level = 0) and (Integer(ATreeNodes.Item[LCount].Data) = ACatchmentRef) then
        begin
          Result := ATreeNodes.Item[LCount];
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnTreeViewStartDrag(Sender: TObject; var DragObject: TDragObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnTreeViewStartDrag';
var
  LTreeView: TFieldTreeView;
  LCancelAction: boolean;
begin
  try
    FDragFromTreeView := nil;
    FDragNode         := nil;
    if (Sender <> nil) then
    begin
      LCancelAction := False;
      LTreeView := TFieldTreeView(sender);
      if (LTreeView  = ReservoirCatchmentProportionsDialog.TvwAllCatchment) then
        LCancelAction := not ReservoirCatchmentProportionsDialog.BtnAdd.Enabled;
      if (LTreeView  = ReservoirCatchmentProportionsDialog.TvwCurrentCatchment) then
        LCancelAction := not ReservoirCatchmentProportionsDialog.BtnRemove.Enabled;

      if LCancelAction then
      begin
        CancelDrag;
        Exit;
      end;

      if(FIdentifier < 0) then
      begin
        if (LTreeView.Selected <> nil) then
        begin
          FDragFromTreeView := LTreeView;
          FDragNode := LTreeView.Selected;
        end
        else
          CancelDrag;
      end
      else
      begin
        if (LTreeView = ReservoirCatchmentProportionsDialog.TvwCurrentCatchment) and
           (LTreeView.Selected <> nil) and (LTreeView.Selected.Level = 1) and
           IsCurrentNodeSpecifiedReservoir(LTreeView.Selected)then
        begin
          FDragFromTreeView := LTreeView;
          FDragNode := LTreeView.Selected;
        end
        else
          CancelDrag;
      end;
    end
    else
      CancelDrag;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnTreeViewDragOver(Sender,Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnTreeViewDragOver';
var
  LTreeView: TFieldTreeView;
begin
  try
    Accept := false;
    if (Sender <> nil) then
    begin
      LTreeView := TFieldTreeView(Sender);
      if(LTreeView <> FDragFromTreeView ) then
          Accept := true;
      // and (LTreeView.Items.Count > 0) then
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnTreeViewDragDrop(Sender,Source: TObject; X, Y: Integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnTreeViewDragDrop';
var
  LDropNode : TTreeNode;
begin
  try
    {Only accept drag and drop from a TTreeView}
    if(Sender <> FDragFromTreeView ) and (FDragFromTreeView <> nil) and (FDragNode <> nil) then
    begin
      FBusyUpdating := True;
      try
        if (FDragFromTreeView = ReservoirCatchmentProportionsDialog.TvwAllCatchment) then
        begin
          if(FDragNode.Level = 0) then
            SelectCurrentCatchment(FDragNode)
          else
            if(FDragNode.Level = 1) then
            AddReservoirToCurrentCatchment(FDragNode);
        end;
        if (FDragFromTreeView = ReservoirCatchmentProportionsDialog.TvwCurrentCatchment) then
        begin
          LDropNode := ReservoirCatchmentProportionsDialog.TvwAllCatchment.GetNodeAt(X,Y);
          if (LDropNode <> nil) then
             ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Selected := LDropNode;

          if(FDragNode.Level = 0) then
            RemoveCurrentCatchment(FDragNode)
          else
            if(FDragNode.Level = 1) then
              RemoveReservoirFromCurrentCatchment(FDragNode);
        end;
      finally
        FBusyUpdating := False;
      end;
      OnNodeSelectionChange(nil,nil);
    end;
    FDragFromTreeView := nil;
    FDragNode         := nil;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateProportionsGrid(ATotalsOnly: boolean = False);
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateProportionsGrid';
var
  LCount: integer;
  LMainNode: TTreeNode;
  LIncTotal,
  LAffTotal,
  LIrrTotal,
  LUrbTotal: double;
  LReservoirData: IReservoirData;
  LReservoirIdentifier: integer;
  lCol             : integer;
  lFieldIndex      : string;
  lKeyValues       : string;
  lFieldProperty   : TAbstractFieldProperty;
  LMineDamUsed     : boolean;
begin
  try
    LMineDamUsed := False;
    with ReservoirCatchmentProportionsDialog do
    begin
      if not ATotalsOnly then
      begin
        GridRowCount := 2;
        for LCount := 1 to ReservoirCatchmentProportionsDialog.GrdProportions.RowCount -1 do
          ReservoirCatchmentProportionsDialog.GrdProportions.Rows[LCount].Clear;
      end;

      LMainNode  := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode;
      if (LMainNode <> nil) then
      begin
        if not ATotalsOnly then
          ReservoirCatchmentProportionsDialog.GridRowCount := LMainNode.Count + 2;

        LIncTotal := 0.0;
        LAffTotal := 0.0;
        LIrrTotal := 0.0;
        LUrbTotal := 0.0;
        for LCount := 0 to LMainNode.Count - 1 do
        begin
          LReservoirIdentifier := Integer(LMainNode.Item[LCount].Data);
          LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LReservoirIdentifier];
          if(LReservoirData.ReservoirConfigurationData.NodeType in [ntMineUndergroundDam]) then
            LMineDamUsed := True;

          for lCol := 0 to GrdProportions.ColCount -1  do
          begin
            lFieldProperty := GrdProportions.FieldProperty(lCol);
            lFieldIndex := IntToStr(lCol)+ ',' + IntToStr(LCount+1);
            lKeyValues := LReservoirData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
            GrdProportions.HasMetaData[lCol, LCount+1] :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          end;

          if not ATotalsOnly then
          begin
              LFieldProperty := FAppModules.FieldProperties.FieldProperty('DrainageScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,LCount+1] := Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.DrainageScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.DrainageScale);

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('AfforestationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,LCount+1] :=Format(LFieldProperty.FormatStringGrid,
                                                                                       [LReservoirData.ReservoirConfigurationData.AfforestationScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.AfforestationScale);


              LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationScale');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,LCount+1] := Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.IrrigationScale]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.IrrigationScale);


            if (FAppModules.Model.ModelName = CPlanning ) then
            begin
              LFieldProperty := FAppModules.FieldProperties.FieldProperty('UrbanRunOff');
              ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
              ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3,LCount+1] :=Format(LFieldProperty.FormatStringGrid,
                                                                                        [LReservoirData.ReservoirConfigurationData.UrbanRunOff]);// FormatFloat('##0.###',LReservoirData.ReservoirConfigurationData.UrbanRunOff);
            end;
          end;

          LIncTotal := LIncTotal + LReservoirData.ReservoirConfigurationData.DrainageScale;
          LAffTotal := LAffTotal + LReservoirData.ReservoirConfigurationData.AfforestationScale;
          LIrrTotal := LIrrTotal + LReservoirData.ReservoirConfigurationData.IrrigationScale;
          LUrbTotal := LUrbTotal + LReservoirData.ReservoirConfigurationData.UrbanRunOff;
        end;
        if(LMainNode.Count > 0) then
        begin
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('DrainageScale');
          ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,LMainNode.Count+1] := Format(LFieldProperty.FormatStringGrid,[LIncTotal])+'%';

          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AfforestationScale');
          ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,LMainNode.Count+1] := Format(LFieldProperty.FormatStringGrid,[LAffTotal])+'%';

          LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationScale');
          ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,LMainNode.Count+1] := Format(LFieldProperty.FormatStringGrid,[LIrrTotal])+'%';
//          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0,LMainNode.Count+1] := FormatFloat('##0.###',LIncTotal)+'%';
//          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1,LMainNode.Count+1] := FormatFloat('##0.###',LAffTotal)+'%';
//          ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2,LMainNode.Count+1] := FormatFloat('##0.###',LIrrTotal)+'%';
//          if (FAppModules.Model.ModelName = CPlanning ) then
//            ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3,LMainNode.Count+1] := FormatFloat('##0.###',LUrbTotal)+'%';
              if (FAppModules.Model.ModelName = CPlanning) then
              begin
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('UrbanRunOff');
                ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
                ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3,LMainNode.Count+1] :=Format(LFieldProperty.FormatStringGrid,[LUrbTotal])+'%';// FormatFloat('##0.###',LUrbTotal)+'%';
              end;

        end;
      end;
      for lCol := 0 to GrdProportions.ColCount-1 do
      begin
        GrdProportions.IsColumnEnabled[lCol] := not LMineDamUsed;
      end;
      GrdProportions.Refresh;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnStringGridKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnStringGridKeyPress';
begin
  try
    if(ReservoirCatchmentProportionsDialog.GrdProportions.Row = ReservoirCatchmentProportionsDialog.GrdProportions.RowCount -1) then
      Key := #0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnGridSetEditText';
begin
  try
    if(ARow = (ReservoirCatchmentProportionsDialog.GrdProportions.RowCount -1)) then
    begin
      ReservoirCatchmentProportionsDialog.GrdProportions.CancelEdits;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnStringGridCellDataHasChanged';
var
  LReservoirData : IReservoirData;
  LMainNode      : TTreeNode;
  LReservoirIdentifier: integer;
begin
  inherited;
  try
    if (ARow > 0) AND (ARow < ReservoirCatchmentProportionsDialog.GrdProportions.RowCount -1) then
    begin
      LMainNode  := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem;
      if (LMainNode <> nil) then
      begin
        LReservoirIdentifier := Integer(LMainNode.Item[ARow-1].Data);
        LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LReservoirIdentifier];
        if (LReservoirData <> nil) then
        begin
          case ACol of
            0 : UpdateDrainageScale(lReservoirData, ARow);
            1 : UpdateAfforestationScale(lReservoirData, ARow);
            2 : UpdateIrrigationScale(lReservoirData, ARow);
            3 : UpdateUrbanRunOff(lReservoirData, ARow);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.UpdateDrainageScale
                                                   (AReservoirData : IReservoirData;
                                                    ARow           : integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateDrainageScale';
var
  LValue         : double;
  LFieldValue,
  LErrorMessage  : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    if (AReservoirData <> nil) then
    begin
      LFieldValue := ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0, ARow];
      if FAppModules.FieldProperties.ValidateFieldProperty('DrainageScale',LFieldValue,LErrorMessage) then
      begin
        LValue := StrToFloat(LFieldValue);
        AReservoirData.ReservoirConfigurationData.DrainageScale := LValue;
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[0,ARow,gveCellField] := '';

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('DrainageScale');
        ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
        ReservoirCatchmentProportionsDialog.GrdProportions.Cells[0, ARow] := Format(LFieldProperty.FormatStringGrid,
                                                                              [AReservoirData.ReservoirConfigurationData.DrainageScale]);
      //   FormatFloat('##0.###',AReservoirData.ReservoirConfigurationData.DrainageScale);
        PopulateProportionsGrid(True);
        DoContextValidation(dvtResCatchSumDrainageScale);
      end
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[0,ARow,gveCellField] := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.UpdateAfforestationScale
                                                   (AReservoirData : IReservoirData;
                                                    ARow           : integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateAfforestationScale';
var
  LValue         : double;
  LFieldValue,
  LErrorMessage  : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    if (AReservoirData <> nil) then
    begin
      LFieldValue := ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1, ARow];
      if FAppModules.FieldProperties.ValidateFieldProperty('AfforestationScale',LFieldValue,LErrorMessage) then
      begin
        LValue := StrToFloat(LFieldValue);
        AReservoirData.ReservoirConfigurationData.AfforestationScale := LValue;
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[1,ARow,gveCellField] := '';

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('AfforestationScale');
        ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
        ReservoirCatchmentProportionsDialog.GrdProportions.Cells[1, ARow] := Format(LFieldProperty.FormatStringGrid,
                                                                                 [AReservoirData.ReservoirConfigurationData.AfforestationScale]);
        //  FormatFloat('##0.###',AReservoirData.ReservoirConfigurationData.AfforestationScale);
        PopulateProportionsGrid(True);
        DoContextValidation(dvtSumAfforestationScale);
      end
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[1,ARow,gveCellField] := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.UpdateIrrigationScale
                                                   (AReservoirData : IReservoirData;
                                                    ARow           : integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateIrrigationScale';
var
  LValue         : double;
  LFieldValue,
  LErrorMessage  : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    if(AReservoirData <> nil) then
    begin
      LFieldValue := ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2, ARow];
      if FAppModules.FieldProperties.ValidateFieldProperty('IrrigationScale',LFieldValue,LErrorMessage) then
      begin
        LValue := StrToFloat(LFieldValue);
        AReservoirData.ReservoirConfigurationData.IrrigationScale := LValue;
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[2,ARow,gveCellField] := '';

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('IrrigationScale');
        ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
        ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2, ARow] := Format(LFieldProperty.FormatStringGrid ,
                                                                               [AReservoirData.ReservoirConfigurationData.IrrigationScale]);
//        ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2, ARow] :=
//          FormatFloat('##0.###',AReservoirData.ReservoirConfigurationData.IrrigationScale);
        PopulateProportionsGrid(True);
        DoContextValidation(dvtResCatchSumIrrigationScale);
      end
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[2,ARow,gveCellField] := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.UpdateUrbanRunOff(AReservoirData: IReservoirData;
                                                                    ARow: integer);
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateUrbanRunOff';
var
  LValue         : double;
  LFieldValue,
  LErrorMessage  : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  inherited;
  try
    if(AReservoirData <> nil) then
    begin
      LFieldValue := ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3, ARow];
      if FAppModules.FieldProperties.ValidateFieldProperty('UrbanRunOff',LFieldValue,
         LErrorMessage) then
      begin
        LValue := StrToFloat(LFieldValue);
        AReservoirData.ReservoirConfigurationData.UrbanRunOff := LValue;
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[3,ARow,gveCellField] := '';

        LFieldProperty := FAppModules.FieldProperties.FieldProperty('UrbanRunOff');
        ReservoirCatchmentProportionsDialog.GrdProportions.AddFieldProperty(LFieldProperty);
        ReservoirCatchmentProportionsDialog.GrdProportions.Cells[2, ARow] := Format(LFieldProperty.FormatStringGrid ,
                                                                               [AReservoirData.ReservoirConfigurationData.UrbanRunOff]);
//      ReservoirCatchmentProportionsDialog.GrdProportions.Cells[3, ARow] :=
//          FormatFloat('##0.###',AReservoirData.ReservoirConfigurationData.UrbanRunOff);
        PopulateProportionsGrid(True);
        DoContextValidation(dvtResCatchSumUrbanRunOff);
      end
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[3,ARow,gveCellField] := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateChartSeries;
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateChartSeries';
var
  LFileNames: TStringList;
  LCatchmentRef: integer;
  LMainNode: TTreeNode;
begin
  try
    ReservoirCatchmentProportionsDialog.INCLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.RNKLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.AFFLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.IRRLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.RANLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.NETTLineSeriesA.Clear;
    ReservoirCatchmentProportionsDialog.URBLineSeriesA.Clear;

    ReservoirCatchmentProportionsDialog.MARLineSeriesINC.Clear;
    ReservoirCatchmentProportionsDialog.MARLineSeriesRNK.Clear;
    ReservoirCatchmentProportionsDialog.MARLineSeriesAFF.Clear;
    ReservoirCatchmentProportionsDialog.MARLineSeriesIRR.Clear;
    ReservoirCatchmentProportionsDialog.MARLineSeriesRAN.Clear;
    ReservoirCatchmentProportionsDialog.MARLineSeriesURB.Clear;

    ReservoirCatchmentProportionsDialog.INCLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.RNKLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.AFFLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.IRRLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.RANLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.NETTLineSeriesM.Clear;
    ReservoirCatchmentProportionsDialog.URBLineSeriesM.Clear;

    ReservoirCatchmentProportionsDialog.PopulateFileNames(nil);

    LMainNode  := ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode;
    if (LMainNode <> nil) then
    begin
      LCatchmentRef := Integer(LMainNode.Data);
      if(LCatchmentRef > 0) then
      begin
        LFileNames := TStringList.Create;
        try
          TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFilesForCatchment(LCatchmentRef,LFileNames);
          ReservoirCatchmentProportionsDialog.PopulateFileNames(LFileNames);
          PopulateTimeSeriesChart(LFileNames);
        finally
          LFileNames.Free;
        end;
      end;
    end;
    ReservoirCatchmentProportionsDialog.RefreshTimeSeriesChart;;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.PopulateTimeSeriesChart(AFileNames: TStringList);
const OPNAME = 'TReservoirCatchmentProportionsValidator.PopulateTimeSeriesChart';
var
  LFieldName,
  LFileExt,
  LFileName: string;
  LDataSet: TAbstractModelDataset;
  LLineSeriesA,
  LLineSeriesM,
  LMARLineSeries : TLineSeries;
  LCount: integer;
  LIndex: integer;
  LMonth: integer;
  LYearValueM,
  LYearValueA : integer;
  LDate: TDateTime;
  LYValue,
  LINCValue,
  LAFFValue,
  LIRRValue,
  LMonthValue: double;

  LAnnualTotalVal,
  LMARTotalVal,
  LMARTotalAve : double;
  LMARTotalYrs : integer;
  LMarXMin,
  LMarXMax : integer;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if (LDataSet <> nil) then
      begin
        for LCount := 0 to AFileNames.Count -1 do
        begin
          LFileName := AFileNames[LCount];
          LFileName := ExtractFileName(LFileName);
          LFileExt  := UpperCase(ExtractFileExt(LFileName));

          LLineSeriesA    := nil;
          LLineSeriesM    := nil;
          LMARLineSeries  := nil;


          //LMARLineSeriesA := ReservoirCatchmentProportionsDialog.MARLineSeriesA;
          //LMARLineSeriesM := ReservoirCatchmentProportionsDialog.MARLineSeriesM;
          if (LFileExt = '.INC') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.INCLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.INCLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesINC;
          end;
          if (LFileExt = '.RNK') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.RNKLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.RNKLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesRNK;
          end;
          if (LFileExt = '.AFF') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.AFFLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.AFFLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesAFF;
          end;
          if (LFileExt = '.IRR') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.IRRLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.IRRLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesIRR;
          end;
          if (LFileExt = '.RAN') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.RANLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.RANLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesRAN;
          end;
          if (LFileExt = '.URB') then
          begin
            LLineSeriesA   := ReservoirCatchmentProportionsDialog.URBLineSeriesA;
            LLineSeriesM   := ReservoirCatchmentProportionsDialog.URBLineSeriesM;
            LMARLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesURB;
          end;

          if (not (LLineSeriesA   <> nil)) and
             (not (LLineSeriesM   <> nil)) and
             (not (LMARLineSeries <> nil)) then
            Continue;

          LDataSet.DataSet.Close;
          if not TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFileDataSet(AFileNames[LCount],LDataSet) then
            Continue;
          if not LDataSet.DataSet.Active then
            Continue;
          if (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            Continue;
          LMARTotalVal  := 0.0;
          LMarXMin      := 0;
          LMARTotalYrs  := 0;
          while not LDataSet.DataSet.Eof do
          begin
            if LDataSet.DataSet.Bof then
              LMarXMin := LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
            LYearValueA := LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
            LAnnualTotalVal := 0;
            for LIndex := 0 to 11 do
            begin
              LYearValueM := LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
              LMonth := FAppModules.StudyArea.CalendarStartMonth + LIndex;
              if(LMonth > 12) then
              begin
                LYearValueM := LYearValueM + 1;
                LMonth     := LMonth - 12;
              end;
              LDate       := EncodeDate(LYearValueM, LMonth, 1);
              LFieldName  := Format('%s%2.2d',['HydroMonthValue',LIndex + 1]);
              LMonthValue := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LAnnualTotalVal := LAnnualTotalVal + LMonthValue;
              LLineSeriesM.AddXY(LDate,LMonthValue);
            end;
            LLineSeriesA.AddXY(LYearValueA, LAnnualTotalVal, IntToStr(LYearValueA));
            LMARTotalVal  := LMARTotalVal  + LAnnualTotalVal;
            //LLineSeriesA.AddXY(LYearValueA, LDataSet.DataSet.FieldByName('HydroTotalValue').AsInteger, IntToStr(LYearValueA));
            //LMARTotalVal  := LMARTotalVal  + LDataSet.DataSet.FieldByName('HydroTotalValue').AsInteger;
            LMARTotalYrs  := LMARTotalYrs + 1;
            LDataSet.DataSet.Next;
          end;
          LMarXMax     := LDataSet.DataSet.FieldByName('HydroYearValue').AsInteger;
          LMARTotalAve := (LMARTotalVal / LMARTotalYrs);
          LMARLineSeries.AddXY(LMarXMin, LMARTotalAve);
          LMARLineSeries.AddXY(LMarXMax, LMARTotalAve);
        end;

        if (ReservoirCatchmentProportionsDialog.INCLineSeriesA.Count > 0) and
           (ReservoirCatchmentProportionsDialog.INCLineSeriesA.Count = ReservoirCatchmentProportionsDialog.AFFLineSeriesA.Count) and
           (ReservoirCatchmentProportionsDialog.INCLineSeriesA.Count = ReservoirCatchmentProportionsDialog.IRRLineSeriesA.Count) then
        begin
          for LCount := 0 to ReservoirCatchmentProportionsDialog.INCLineSeriesA.XValues.Count - 1 do
          begin

            LDate := ReservoirCatchmentProportionsDialog.INCLineSeriesA.XValues.Value[LCount];
            LINCValue := ReservoirCatchmentProportionsDialog.INCLineSeriesA.YValues.Value[LCount];
            LAFFValue := ReservoirCatchmentProportionsDialog.AFFLineSeriesA.YValues.Value[LCount];
            LIRRValue := ReservoirCatchmentProportionsDialog.IRRLineSeriesA.YValues.Value[LCount];
            LYValue   := LINCValue - (LAFFValue + LIRRValue);
            ReservoirCatchmentProportionsDialog.NETTLineSeriesA.AddXY(LDate,LYValue);
          end;
        end;
      end;
 //   ReservoirCatchmentProportionsDialog.CalculateStatistics;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.OnCurrentCatchmentTreeViewHasChanged;
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnCurrentCatchmentTreeViewHasChanged';
begin
  try
    ReservoirCatchmentProportionsDialog.ChtTimeSeries.RemoveAllSeries;
    ReservoirCatchmentProportionsDialog.ChtTimeSeries.Title.Text.Text := '';

    PopulateProportionsGrid;
    PopulateChartSeries;

    if (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.TopItem <> nil) then
    begin
      ReservoirCatchmentProportionsDialog.RadFiles.ItemIndex       := 0;
      ReservoirCatchmentProportionsDialog.RadChartView.ItemIndex   := 0;
      ReservoirCatchmentProportionsDialog.RadFiles.IsEnabled         := False;
      ReservoirCatchmentProportionsDialog.RadChartView.IsEnabled     := True;
      //ReservoirCatchmentProportionsDialog.GrdProportions.IsEnabled   := True;
      ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Enabled := True;
    end
    else
    begin
      ReservoirCatchmentProportionsDialog.RadFiles.ItemIndex        := -1;
      ReservoirCatchmentProportionsDialog.RadChartView.ItemIndex    := -1;
      ReservoirCatchmentProportionsDialog.RadFiles.IsEnabled        := False;
      ReservoirCatchmentProportionsDialog.RadChartView.IsEnabled    := True;
      ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Enabled  := False;
      ReservoirCatchmentProportionsDialog.CatchmentAreaEdt.Clear;

      //ReservoirCatchmentProportionsDialog.GrdProportions.IsEnabled:= True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.IsCurrentNodeSpecifiedReservoir(ATreeNode: TTreeNode): boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.IsCurrentNodeSpecifiedReservoir';
var
  LReservoir:IReservoirData;
begin
  Result := False;
  try
    if(FIdentifier < 0) or (ATreeNode = nil) then
      Result := True
    else
    begin
      LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                          ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (LReservoir <> nil) then
      begin
        Result := LReservoir.ReservoirConfigurationData.ReservoirName = ATreeNode.Text;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TReservoirCatchmentProportionsValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).
              NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.CalculateMovedNodeProportions(AReservoirObject: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.CalculateMovedNodeProportions';
var
  LIncTotal,
  LAffTotal,
  LIrrTotal: double;
  LIndex: integer;
  LResevoir: IReservoirData;
  LReservoirDataList:TReservoirDataList;

begin
  try
      LIncTotal := 0.0;
      LAffTotal := 0.0;
      LIrrTotal := 0.0;

      LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList;
      for LIndex := 0 to LReservoirDataList.ReservoirAndNodesCount -1 do
      begin
        LResevoir := LReservoirDataList.ReservoirOrNodeByIndex[LIndex];
        if(LResevoir.ReservoirConfigurationData.CatchmentRef = AReservoirObject.ReservoirConfigurationData.CatchmentRef) and
          (LResevoir.ReservoirConfigurationData.ReservoirIdentifier <> AReservoirObject.ReservoirConfigurationData.ReservoirIdentifier) then
        begin
          LIncTotal := LIncTotal + LResevoir.ReservoirConfigurationData.DrainageScale;
          LAffTotal := LAffTotal + LResevoir.ReservoirConfigurationData.AfforestationScale;
          LIrrTotal := LIrrTotal + LResevoir.ReservoirConfigurationData.IrrigationScale;
        end;
      end;

      AReservoirObject.ReservoirConfigurationData.DrainageScale      := 100.0 - LIncTotal;
      AReservoirObject.ReservoirConfigurationData.AfforestationScale := 100.0 - LAffTotal;
      AReservoirObject.ReservoirConfigurationData.IrrigationScale    := 100.0 - LIrrTotal;

    PopulateProportionsGrid;
    PopulateChartSeries;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirCatchmentProportionsValidator.DoContextValidation';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
      lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];

      case AValidationType of
        dvtResCatchAll :
          begin
            if (FAppModules.StudyArea.ModelVersion = '7') then
              ValidateCatchmentArea(lReservoir);
          end;
        dvtParamCatchmentArea :
           begin
             if (FAppModules.StudyArea.ModelVersion = '7') then
               ValidateCatchmentArea(lReservoir);
           end;
      end;
      
      if (lReservoir <> nil) then
      begin
        if (AValidationType in [dvtResCatchAll, dvtResCatchRef]) then
          ValidateCatchmentRefNumber(lReservoir);
        if (AValidationType in [dvtResCatchAll, dvtResCatchRef, dvtResCatchSumDrainageScale]) then
          ValidateSumDrainageScale(lReservoir);
        if (AValidationType in [dvtResCatchAll, dvtResCatchRef, dvtResCatchSumIrrigationScale]) then
          ValidateSumIrrigationScale(lReservoir);
        if (AValidationType in [dvtResCatchAll, dvtResCatchRef, dvtSumAfforestationScale]) then
          ValidateSumAfforestationScale(lReservoir);
        if (FAppModules.Model.ModelName = CPlanning) then
          if ((AValidationType in [dvtResCatchAll, dvtResCatchRef]) or
              (AValidationType = dvtResCatchSumUrbanRunOff)) then
             ValidateSumUrbanRunOff(lReservoir);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirCatchmentProportionsValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReservoirCatchmentProportionsValidator.DetermineWizardStatus';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          DoContextValidation(dvtResCatchAll);
          if (lReservoir.ReservoirConfigurationData.CatchmentRef <> 0) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;    
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateCatchmentRefNumber
                                                   (AReservoir : IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateCatchmentRefNumber';
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'CatchmentRefNumber')) then
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.ValidationError := ''
      else
      begin
        ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.ValidationError := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateSumDrainageScale
                                                   (AReservoir : IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateSumDrainageScale';
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'SumDrainageScale')) then
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[0, 0, gveColContext] := ''
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[0, 0, gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateSumAfforestationScale(AReservoir: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateSumAfforestationScale';
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'SumAfforestationScale')) then
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[1, 0, gveColContext] := ''
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[1, 0, gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateSumIrrigationScale(AReservoir: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateSumIrrigationScale';
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'SumIrrigationScale')) then
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[2, 0, gveColContext] := ''
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[2, 0, gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateSumUrbanRunOff(AReservoir: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateSumUrbanRunOff';
begin
  try
    with ReservoirCatchmentProportionsDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'SumUrbanRunOff')) then
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[3, 0, gveColContext] := ''
      else
      begin
        ReservoirCatchmentProportionsDialog.GrdProportions.ValidationError[3, 0, gveColContext] := FErrorMessage;
        FAllErrorMessages.Add(FErrorMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TReservoirCatchmentProportionsValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TReservoirCatchmentProportionsValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if ((FActiveControl <> nil) AND (FPanel.Visible)) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil ) then
      begin
        with ReservoirCatchmentProportionsDialog do
        begin
          lFieldIndex := '';
          lFieldProperty := nil;
          if (FActiveControl = GrdProportions) then
          begin
            lFieldIndex := IntToStr(GrdProportions.Col)+ ',' + IntToStr(GrdProportions.Row);
            lFieldProperty   := GrdProportions.FieldProperty(GrdProportions.Col);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName,lKeyValues);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex);
            PopulateReservoirTreeView(lReservoir);
            Result := TRUE;
          end
        end;
      end;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


procedure TReservoirCatchmentProportionsValidator.OnChkMARClick(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnChkMARClick';
var
  LLineSeries : TLineSeries;
begin
  try
    LLineSeries := nil;
    case ReservoirCatchmentProportionsDialog.RadFiles.ItemIndex of
      0: LLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesINC;
      1: LLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesAFF;
      2: LLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesIRR;
      3: LLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesRAN;
      4: LLineSeries := ReservoirCatchmentProportionsDialog.MARLineSeriesURB;
    end;
    ReservoirCatchmentProportionsDialog.ToggleMARLines(LLineSeries);
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirCatchmentProportionsValidator.OnRadMonthlyAnnualClick(Sender: TObject);
const OPNAME = 'TReservoirCatchmentProportionsValidator.OnRadMonthlyAnnualClick';
begin
  try
    ReservoirCatchmentProportionsDialog.ChtTimeSeries.UndoZoom;
    ReservoirCatchmentProportionsDialog.ChkMAR.Enabled :=
      (ReservoirCatchmentProportionsDialog.RadMonthlyAnnual.ItemIndex = 0);
    if (ReservoirCatchmentProportionsDialog.RadMonthlyAnnual.ItemIndex = 1) then
      if (ReservoirCatchmentProportionsDialog.ChkMAR.Checked) then
        ReservoirCatchmentProportionsDialog.ChkMAR.Checked := False;
    ReservoirCatchmentProportionsDialog.ToggleMonthlyAnnual;
    OnEditControlEnter(ReservoirCatchmentProportionsDialog.RadMonthlyAnnual);
    DoContextValidation(dvtResCatchAll);
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TReservoirCatchmentProportionsValidator.GetMonthLabel(AMonthValue: integer): string;
const OPNAME = 'TReservoirCatchmentProportionsValidator.GetMonthLabel';
begin
  try
    if (AMonthValue > 0) and
       (AMonthValue < 13) then
    begin
      case AMonthValue of
        1:  Result := 'JAN';
        2:  Result := 'FEB';
        3:  Result := 'MAR';
        4:  Result := 'APR';
        5:  Result := 'MAY';
        6:  Result := 'JUN';
        7:  Result := 'JUL';
        8:  Result := 'AUG';
        9:  Result := 'SEP';
        10: Result := 'OCT';
        11: Result := 'NOV';
        12: Result := 'DEC';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.UpdateCatchmentArea;
const OPNAME = 'TReservoirCatchmentProportionsValidator.UpdateCatchmentArea';
var
  LParamReference : IParamReference;
  LMessage : string;
  LCatchmentRef : Integer;
begin
  try
    LParamReference := nil;
    if (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode <> nil) then
    begin
      LCatchmentRef := Integer(ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode.Data);
      if(LCatchmentRef > 0) then
      begin
        LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                            ReferenceDataByCatchNumber[LCatchmentRef];
      end;
    end;

    if (LParamReference <> nil) then
    begin
      with ReservoirCatchmentProportionsDialog do
      begin
        CatchmentAreaEdt.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        CatchmentAreaEdt.FieldProperty.FieldName,
                        CatchmentAreaEdt.Text, LMessage) then
        begin
          LParamReference.CatchmentArea := StrToFloat(CatchmentAreaEdt.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtParamCatchmentArea);
        end
        else
          CatchmentAreaEdt.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirCatchmentProportionsValidator.ValidateCatchmentArea(AReservoir: IReservoirData);
const OPNAME = 'TReservoirCatchmentProportionsValidator.ValidateCatchmentArea';
var
  LMessage        : Widestring;
  LParamReference : IParamReference;
  LCatchmentRef   : Integer;
begin
  try
    if AReservoir = nil then
      Exit;

    if (ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode <> nil) then
    begin
      LCatchmentRef := Integer(ReservoirCatchmentProportionsDialog.TvwCurrentCatchment.Items.GetFirstNode.Data);
      if(LCatchmentRef > 0) then
      begin
        LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                            ReferenceDataByCatchNumber[LCatchmentRef];
        if (LParamReference <> nil)then
        begin
          with ReservoirCatchmentProportionsDialog do
          begin
            LMessage := '';
            CatchmentAreaEdt.ContextValidationError := '';
            if (not LParamReference.Validate(LMessage, CatchmentAreaEdt.FieldProperty.FieldName)) then
              CatchmentAreaEdt.FieldValidationError := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



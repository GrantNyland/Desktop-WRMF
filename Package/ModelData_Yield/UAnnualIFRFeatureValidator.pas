unit UAnnualIFRFeatureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCLTee.Series,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UAnnualIFRFeatureDialog;

type
  TMonthError = class(TObject)
  private
    FMonthError : array [1..12] of Boolean;
    function GetMonthError (AMonth : integer) : Boolean;
    procedure SetMonthError (AMonth : integer;
                             AError : Boolean);
  public
    procedure SetToFalse;
    property MonthError [AMonth : integer] : Boolean read GetMonthError write SetMonthError; default;
  end;

  TAnnualIFRFeatureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    FFileNameContainer : TStringList;    
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnReferenceNodesClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnIFRExistsClick(Sender: TObject);
    procedure OnIFRLossClick(Sender: TObject);
    procedure DoCalcOptionCbxChange(Sender : TObject);
    procedure UnitOptionsRadioGroupOnClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateReferenceNodes;
    procedure RePopulateAnnualGrid;
    procedure ClearAnnualGrid;
    procedure SetGridWith;
    procedure CheckReferenceNodes;
    procedure UpdateFeatureName;
    procedure UpdateNumberOfPoints;
    procedure UpdateReferenceNodes;

//  procedure UpdateSiteData;

    procedure UpdateCalculationOption;
    procedure UpdateAnnualGrid;
    procedure UpdateAnnualRelease;
    procedure UpdateIFRFeatureExists;
  //  procedure UpdateIFRLoss;
 //   procedure UpdateMonthlyIFRLossGrid(AIndex: integer; AValue: string);

    procedure ValidateFeatureName(AFeature : IIFRFeature);
    procedure ValidateReferenceNodes (AFeature : IIFRFeature);
    procedure ValidatePointsCount(AFeature: IIFRFeature);
    procedure ValidateReleases(AFeature: IIFRFeature);
    procedure ValidateAnnualInFlow(AFeature: IIFRFeature);
    procedure ValidateCalculationOption(AFeature: IIFRFeature);

    procedure UpdateMARDisplay(AReservoirName: string);
    procedure PopulateFileNames(AFileNameContainer: TStringList);
    function  CalculateMAR(AFileNames: TStringList): double;
    procedure GetSelectedNodes(out ANodesNameList: TStringList);
    procedure CalculteMAROfSelectedNodes;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
//    function ProcessParameterChangeEvent : boolean; override;
//    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function AnnualIFRFeatureDialog : TAnnualIFRFeatureDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  Math,
  SysUtils,
  VCL.Graphics,
  ContNrs,
  UConstants,
  UUtilities,
  UIFRFeatures,
  UIFRDataObject,
  UParameterData,
  UDataSetType,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UPlanningModelDataObject,
  UNetworkElementData, VCL.Grids;

{******************************************************************************}
{* TAnnualIFRFeatureValidator                                                       *}
{******************************************************************************}

procedure TAnnualIFRFeatureValidator.CreateMemberObjects;
const OPNAME = 'TAnnualIFRFeatureValidator.CreateMemberObjects';
var
  lPanel : TAnnualIFRFeatureDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TAnnualIFRFeatureDialog.Create(FPanelOwner,FAppModules);
    lPanel := AnnualIFRFeatureDialog;
    FFileNameContainer := TStringList.Create;
    with lPanel do
    begin
{      SitesCbx.FieldProperty        := FAppModules.FieldProperties.FieldProperty('IFRSiteID');
      SitesCbx.OnEnter              := OnEditControlEnter;
      SitesCbx.OnExit               := OnEditControltExit;
 }
      FeatureNameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRFeatureName');
      FeatureNameEdit.OnEnter       := OnEditControlEnter;
      FeatureNameEdit.OnExit        := OnEditControltExit;

      NrOfPointsEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('AnnualNumberOfClasses');
      NrOfPointsEdit.OnEnter        := OnEditControlEnter;
      NrOfPointsEdit.OnExit         := OnEditControltExit;

      CalcOptionCbx.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRCalcOption');
      CalcOptionCbx.OnEnter        := OnEditControlEnter;
      CalcOptionCbx.OnExit         := OnEditControltExit;
      CalcOptionCbx.OnChange       := DoCalcOptionCbxChange;

      edtCalcOption.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRCalcOption');
      edtCalcOption.OnEnter        := OnEditControlEnter;
      edtCalcOption.OnExit         := OnEditControltExit;


      ReferenceNodesCheckLbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('RefNodeNumber');
      ReferenceNodesCheckLbx.OnEnter       := OnEditControlEnter;
      ReferenceNodesCheckLbx.OnClick       := OnReferenceNodesClick;

      rdgAnnualMonthlyOption.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRBaseOption');
      rdgAnnualMonthlyOption.OnEnter        := OnEditControlEnter;
      rdgAnnualMonthlyOption.OnExit         := OnEditControltExit;

      AnnualGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      AnnualGrid.OnColEnter         := OnStringGridColEnter;

      UnitsOptionRadioGroup.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRUnitsOption');
      UnitsOptionRadioGroup.OnEnter        := OnEditControlEnter;
      UnitsOptionRadioGroup.OnExit         := OnEditControltExit;
      UnitsOptionRadioGroup.OnClick        := UnitOptionsRadioGroupOnClick;

      ChkboxFIFRFeatureExists.FieldProperty:= FAppModules.FieldProperties.FieldProperty('IFRStatusIndicator');
      ChkboxFIFRFeatureExists.OnClick      := OnIFRExistsClick;

   {   IFRLoss.FieldProperty                     :=  FAppModules.FieldProperties.FieldProperty('IFRLoss');
      IFRLoss.OnClick                           :=  OnIFRLossClick;
      IFRLoss.OnEnter                           :=  OnEditControlEnter;
      IFRLoss.OnExit                            :=  OnEditControltExit;

      MonthlyIFRLossGrid.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      MonthlyIFRLossGrid.OnEnter                := OnEditControlEnter;
      MonthlyIFRLossGrid.OnColEnter             := OnStringGridColEnter;
       }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.DestroyMemberObjects;
const OPNAME = 'TAnnualIFRFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FFileNameContainer.Free;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.Initialise: boolean;
const OPNAME = 'TAnnualIFRFeatureValidator.Initialise';
var
  LMonth : string;
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    AnnualIFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex := 0;
    for LIndex := 1 to 12 do
    begin
      LMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
      AnnualIFRFeatureDialog.AnnualGrid.Cells[LIndex,0] := LMonth;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UnitOptionsRadioGroupOnClick(Sender: TObject);
const OPNAME ='TAnnualIFRFeatureValidator.UnitOptionsRadioGroupOnClick';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TAnnualIFRFeatureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TAnnualIFRFeatureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.IFRFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.ClearDataViewer;
const OPNAME = 'TAnnualIFRFeatureValidator.ClearDataViewer';
var
  lPanel : TAnnualIFRFeatureDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := AnnualIFRFeatureDialog;
    with lPanel do
    begin
      FeatureNameEdit.SetFieldValue('');
      NrOfPointsEdit.Text := '-1';
      ReferenceNodesCheckLbx.Items.Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.PopulateDataViewer;
const OPNAME = 'TAnnualIFRFeatureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIFRFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.RePopulateDataViewer;
const OPNAME = 'TAnnualIFRFeatureValidator.RePopulateDataViewer';
var
  lIFRFeature    : TIFRFeature;
  lMonths        : TMonthNamesArray;
 // LCol           : integer;
begin
  lMonths := nil;
  try
    AnnualIFRFeatureDialog.LanguageHasChanged;
    if (FFeatureID >= 0) then
    begin
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastIFRFeatureList.CastAnnualIFRFeatureByID(FFeatureID);
      if (lIFRFeature <> nil) then
      begin
        with AnnualIFRFeatureDialog do
        begin
          FeatureNameEdit.SetFieldValue(lIFRFeature.FeatureName);
          if LIFRFeature.CalculationOption <> NullFloat then
          begin
            edtCalcOption.SetFieldValue(LIFRFeature.CalculationOption);
            if LIFRFeature.CalculationOption = 0 then
              CalcOptionCbx.ItemIndex := 0
            else
              CalcOptionCbx.ItemIndex := 1;

          end
          else
          begin
            edtCalcOption.Text := '';
            CalcOptionCbx.ItemIndex := -1;
          end;
          NrOfPointsEdit.SetFieldValue(lIFRFeature.NrOfInflowIFRPoints);
          RePopulateReferenceNodes;
          CalculteMAROfSelectedNodes;
          RePopulateAnnualGrid;

          ChkboxFIFRFeatureExists.Checked := (lIFRFeature.IFRFeatureExists =1);

          {
          IFRLoss.Checked := (lIFRFeature.IFRLoss=1);
          if not IFRLoss.Checked then
          begin
            MonthlyIFRLossGrid.Visible := False;
            AnnualGrid.Top := 200;
          end
          else
          begin
            MonthlyIFRLossGrid.Visible := True;
            AnnualGrid.Top := MonthlyIFRLossGrid.Top+ MonthlyIFRLossGrid.Height+10;
            MonthlyIFRLossGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyIFRLoss'));
            for lCol := 1 to 12 do
            begin
              MonthlyIFRLossGrid.Cells[lCol-1, 0] := lMonths[lCol];
              MonthlyIFRLossGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyIFRLoss'));
              MonthlyIFRLossGrid.Cells[lCol-1, 1] := FormatFloat('##0.00',lIFRFeature.MonthlyIFRLossByIndex[lCol-1]);
            end;
          end;
                   }
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TAnnualIFRFeatureValidator.RePopulateReferenceNodes;
const OPNAME = 'TAnnualIFRFeatureValidator.RePopulateReferenceNodes';
var
  lIndex         : integer;
  lReservoirList : IReservoirDataList;
  lReservoirData : IReservoirData;
  lReservoir     : IReservoirConfigurationData;
  LIFRFeature    : IIFRFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
      if (lIFRFeature <> nil) then
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        if (lReservoirList <> nil) then
        begin
          with AnnualIFRFeatureDialog do
          begin
            ReferenceNodesCheckLbx.Items.Clear;
            for lIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoirData := lReservoirList.ReservoirOrNodeByIndex[lIndex];
              lReservoir     := lReservoirData.ReservoirConfigurationData;
              if (lReservoir.NodeType in NodeWithInflowAndReservoirSet) then
              begin
                ReferenceNodesCheckLbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
              end;
            end;
          end;
          CheckReferenceNodes;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.ClearAnnualGrid;
const OPNAME = 'TAnnualIFRFeatureValidator.ClearAnnualGrid';
var
  LRow,
  LCol : integer;
begin
  try
    for LCol := 0 to AnnualIFRFeatureDialog.AnnualGrid.ColCount-1 do
    begin
      for LRow := 1 to AnnualIFRFeatureDialog.AnnualGrid.RowCount-1 do
        AnnualIFRFeatureDialog.AnnualGrid.Cells[LCol,LRow] := '';
    end;
    AnnualIFRFeatureDialog.AnnualGrid.RowCount := 2;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAnnualIFRFeatureValidator.RePopulateAnnualGrid;
const OPNAME = 'TAnnualIFRFeatureValidator.RePopulateAnnualGrid';
var
  LIFRFeature : IIFRFeature;
  LDaysInMonth : double;
  LIndex : integer;
  LMonths : integer;
  LFieldProperty : TAbstractFieldProperty;
  LValue : double;
begin
  try
    if (FFeatureID >= 0) then
    begin
      LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
      if (LIFRFeature <> nil) then
      begin
        ClearAnnualGrid;
        SetGridWith;
        if (LIFRFeature.NrOfInflowIFRPoints = 0) then Exit;
        AnnualIFRFeatureDialog.AnnualGrid.RowCount := AnnualIFRFeatureDialog.AnnualGrid.RowCount +
                                                      LIFRFeature.NrOfInflowIFRPoints-1;
        AnnualIFRFeatureDialog.AnnualGrid.Height   := (AnnualIFRFeatureDialog.AnnualGrid.DefaultRowHeight*
                                                      AnnualIFRFeatureDialog.AnnualGrid.RowCount)+
                                                      AnnualIFRFeatureDialog.AnnualGrid.DefaultRowHeight ;
        for LIndex := 0 to LIFRFeature.NrOfInflowIFRPoints-1 do
        begin
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AnnualInflow');
          AnnualIFRFeatureDialog.AnnualGrid.AddFieldProperty(LFieldProperty);
          AnnualIFRFeatureDialog.AnnualGrid.Cells[0,LIndex+1] := Format(LFieldProperty.FormatStringGrid, [LIFRFeature.AnnualInflow[LIndex]]);
          for LMonths := 1 to 12 do
          begin
            AnnualIFRFeatureDialog.AnnualGrid.ColWidths[LMonths] := 45;
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LMonths];
            LFieldProperty := FAppModules.FieldProperties.FieldProperty('IFRReleaseVariables');
            AnnualIFRFeatureDialog.AnnualGrid.AddFieldProperty(LFieldProperty);
            LValue := LIFRFeature.ReleaseByIndexAndMonth[LIndex+1,LMonths];
            if(AnnualIFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
              LValue := (LValue *(60*60*24*LDaysInMonth))/ Power(10, 6);
            if (LValue <> NullFloat) then
              AnnualIFRFeatureDialog.AnnualGrid.Cells[LMonths,LIndex+1] := Format(LFieldProperty.FormatStringGrid, [lValue])
            else
              AnnualIFRFeatureDialog.AnnualGrid.Cells[LMonths,LIndex+1] := '';
          end;
        end;
      end;
      DoContextValidation(dvtIFRFeatureAnnualInflows);
      DoContextValidation(dvtIFRFeatureReleases);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TAnnualIFRFeatureValidator.SetGridWith;
const OPNAME = 'TAnnualIFRFeatureValidator.SetGridWith';
var
  LMonths : integer;
begin
  try
    for LMonths := 1 to 12 do
      AnnualIFRFeatureDialog.AnnualGrid.ColWidths[LMonths] := 46;
    AnnualIFRFeatureDialog.AnnualGrid.Width := AnnualIFRFeatureDialog.AnnualGrid.ColWidths[0]+
                                               (AnnualIFRFeatureDialog.AnnualGrid.ColWidths[1]*12)+10;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TAnnualIFRFeatureValidator.CheckReferenceNodes;
const OPNAME = 'TAnnualIFRFeatureValidator.CheckReferenceNodes';
var
  lIndex         : integer;
  lItemIndex     : integer;
  lReservoirList : IReservoirDataList;
  lRefNodeNr     : integer;
  lReferenceNode : IReservoirData;
  lIFRFeature    : IIFRFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
      if (lIFRFeature <> nil) then
      begin
        with AnnualIFRFeatureDialog do
        begin
          for lIndex := 0 to lIFRFeature.ReferenceNodeNumbersCount - 1 do
          begin
            lRefNodeNr     := lIFRFeature.ReferenceNodeNumberByIndex[lIndex];
            lReferenceNode := lReservoirList.ReservoirOrNodeByIdentifier[lRefNodeNr];
            if (lReferenceNode <> nil) then
            begin
              lItemIndex := ReferenceNodesCheckLbx.Items.IndexOfObject(TObject(lRefNodeNr));
              if  (lItemIndex <> -1) then
                ReferenceNodesCheckLbx.Checked[lItemIndex] := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TAnnualIFRFeatureValidator.SaveState: boolean;
const OPNAME = 'TAnnualIFRFeatureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.AnnualIFRFeatureDialog : TAnnualIFRFeatureDialog;
const OPNAME = 'TAnnualIFRFeatureValidator.AnnualIFRFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TAnnualIFRFeatureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TAnnualIFRFeatureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.StudyHasChanged: boolean;
const OPNAME = 'TAnnualIFRFeatureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if (Sender = AnnualIFRFeatureDialog.NrOfPointsEdit) and
      (AnnualIFRFeatureDialog.NrOfPointsEdit.HasValueChanged)then
      UpdateNumberOfPoints
    else
    if (Sender = AnnualIFRFeatureDialog.AnnualGrid) then
      UpdateAnnualGrid
    else
    if (Sender = AnnualIFRFeatureDialog.edtCalcOption) and
       (AnnualIFRFeatureDialog.edtCalcOption.HasValueChanged) or
       (Sender = AnnualIFRFeatureDialog.CalcOptionCbx) and
       (AnnualIFRFeatureDialog.CalcOptionCbx.HasValueChanged) then
      UpdateCalculationOption
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateFeatureName;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateFeatureName';
var
  lFeature  : IIFRFeature;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
          FeatureNameEdit.FieldProperty.FieldName,
          FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtIFRFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TAnnualIFRFeatureValidator.UpdateNumberOfPoints;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateNumberOfPoints';
var
  LIFRFeature : IIFRFeature;
  LMessage    : string;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('AnnualNumberOfClasses',NrOfPointsEdit.Text, lMessage)) then
        begin
          NrOfPointsEdit.FieldValidationError := LMessage;
          LIFRFeature.NrOfInflowIFRPoints := StrToInt(Trim(NrOfPointsEdit.Text));
          NrOfPointsEdit.SetFieldValue(lIFRFeature.NrOfInflowIFRPoints);
          RePopulateDataViewer;
          DoContextValidation(dvtPointsCount);
        end
        else
          NrOfPointsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateReferenceNodes;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateReferenceNodes';
var
  lIFRFeature     : IIFRFeature;
  lIndex          : integer;
  lReferenceNodes : TStringList;
  lNewNode        : IReservoirData;
  lMessage        : string;
  LChannelNr      : integer;
  IReservoirList  : IReservoirDataList;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        IReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList;

        lReferenceNodes := TStringList.Create;
        lReferenceNodes.Clear;
        for lIndex := 0 to ReferenceNodesCheckLbx.Items.Count - 1 do
        begin
          if (ReferenceNodesCheckLbx.Checked[lIndex]) then
          begin
            LChannelNr := integer(ReferenceNodesCheckLbx.Items.Objects[lIndex]);
            lNewNode   := IReservoirList.ReservoirOrNodeByIdentifier[LChannelNr];
            if (lNewNode <> nil) then
              lReferenceNodes.Add(IntToStr(lNewNode.ReservoirConfigurationData.ReservoirIdentifier));
          end;
        end;

        lIFRFeature.ReferenceNodeNumbers := lReferenceNodes.CommaText;
        RePopulateDataViewer;

        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ReferenceNodeCount', IntToStr(lReferenceNodes.Count), lMessage)) then
        begin
          ReferenceNodesCheckLbx.InValidationError := False;
          ReferenceNodesCheckLbx.ShowErrorState(FALSE);
          CheckReferenceNodes;
          DoContextValidation(dvtIFRFeatureReferenceNodes);
        end
        else
        begin
          ReferenceNodesCheckLbx.InValidationError := False;
          ReferenceNodesCheckLbx.ValidationError := lMessage;
          ReferenceNodesCheckLbx.ShowErrorState(TRUE);
        end;
        FreeAndNil(lReferenceNodes);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TAnnualIFRFeatureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with AnnualIFRFeatureDialog do
    begin
      if (AnnualGrid.Col = 0) and (AnnualGrid.Row > 0) then
        UpdateAnnualGrid;
      if (AnnualGrid.Col > 0) and (AnnualGrid.Row > 0) then
        UpdateAnnualRelease
      //else if ((MonthlyIFRLossGrid = ASender) AND (ARow > 0)) then
      //  UpdateMonthlyIFRLossGrid(ACol, Trim(MonthlyIFRLossGrid.Cells[ACol, ARow]));

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.DoCalcOptionCbxChange(Sender : TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.DoCalcOptionCbxChange';
var
  LIFRFeature : IIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if LIFRFeature <> nil then
    begin
      case AnnualIFRFeatureDialog.CalcOptionCbx.ItemIndex of
        0 : AnnualIFRFeatureDialog.edtCalcOption.Text := '0.0';
        1 :
        begin
            if LIFRFeature.CalculationOption <> NullFloat then
              AnnualIFRFeatureDialog.edtCalcOption.SetFieldValue(LIFRFeature.CalculationOption)
            else
              AnnualIFRFeatureDialog.edtCalcOption.Text := '';
        end;
      end;
      ValidateCalculationOption(LIFRFeature);
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TMonthError }

function TMonthError.GetMonthError(AMonth: integer): Boolean;
const OPNAME = 'TMonthError.GetMonthError';
begin
  Result := FMonthError[AMonth];
end;

procedure TMonthError.SetMonthError(AMonth: integer; AError: Boolean);
const OPNAME = 'TMonthError.SetMonthError';
begin
  FMonthError[AMonth] := AError;
end;

procedure TMonthError.SetToFalse;
const OPNAME = 'TMonthError.SetToFalse';
var
  lMonth : integer;
begin
  for lMonth := 1 to 12 do
    FMonthError[lMonth] := FALSE;
end;

procedure TAnnualIFRFeatureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TAnnualIFRFeatureValidator.DoContextValidation';
var
  lFeature     : IIFRFeature;
  lFeatureList : IIFRFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.IFRFeatureList;
      lFeature     := lFeatureList.AnnualIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtIFRFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtIFRFeatureReferenceNodes]) then
          ValidateReferenceNodes(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtPointsCount]) then
          ValidatePointsCount(lFeature);
        case AValidationType of
          dvtIFRFeature,
          dvtIFRFeatureCalculationOption : ValidateCalculationOption(lFeature);
        end;
        case  AValidationType of
          dvtIFRFeature,
          dvtIFRFeatureAnnualInflows : ValidateAnnualInFlow(lFeature);
        end;
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep2, dvtIFRFeatureReleases]) then
          ValidateReleases(lFeature);

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TAnnualIFRFeatureValidator.DetermineWizardStatus';
var
  lFeature       : IIFRFeature;
  lFeatureList   : IIFRFeatureList;
  lIFRVariables  : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
  lMonth         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.IFRFeatureList;
      lFeature := lFeatureList.AnnualIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        case ASequence of
        1 :
          begin
            DoContextValidation(dvtIFRFeatureWizardStep1);
            if (lFeature.ReferenceNodeNumbersCount > 0) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        2 :
          begin
            DoContextValidation(dvtIFRFeatureWizardStep2);
            lIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
            lNotZero := FALSE;
            lIndex   := lIFRVariables.ArrayLow;
            while ((NOT lNotZero) AND (lIndex <= lFeature.NrOfInflowIFRPoints)) do
            begin
              lMonth := lIFRVariables.ArrayLowDimTwo;
              while ((NOT lNotZero) AND (lMonth <= lIFRVariables.ArrayHighDimTwo)) do
              begin
                if (lFeature.InflowByIndexAndMonth[lIndex, lMonth] > 0) then
                  lNotZero := TRUE
                else
                  lMonth := lMonth + 1;
              end;
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
procedure TAnnualIFRFeatureValidator.ValidateReferenceNodes(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateReferenceNodes';
begin
  try
    if (AFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'ReferenceNodes')) then
          ReferenceNodesCheckLbx.InValidationError := FALSE
        else
        begin
          ReferenceNodesCheckLbx.InValidationError := FALSE;
          ReferenceNodesCheckLbx.ValidationError := FErrorMessage;
          ReferenceNodesCheckLbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.ValidateFeatureName(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateFeatureName';
begin
  try
    with AnnualIFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TAnnualIFRFeatureValidator.ValidatePointsCount(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidatePointsCount';
begin
  try
    with AnnualIFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (not AFeature.Validate(FErrorMessage, 'AnnualNumberOfClasses')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfPointsEdit.ContextValidationError :=  FErrorMessage;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{procedure TAnnualIFRFeatureValidator.ValidateIFRFeatureAnnualInflows(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateIFRFeatureAnnualInflows';
var
  LMonth     : integer;
  LErrorCols : TStringlist;
  LErrors    : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        LErrorCols := TStringlist.Create;
        LErrors    := TStringlist.Create;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'AnnualInflow')) then
        begin
          for LMonth := 1 to 12 do
            AnnualGrid.ValidationError[LMonth, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors,lErrorCols);
          for LMonth := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lMonth)) >= 0) then
              AnnualGrid.ValidationError[LMonth, 0, gveColContext] := lErrors.Text
            else
              AnnualGrid.ValidationError[LMonth, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TAnnualIFRFeatureValidator.ValidateCalculationOption(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateCalculationOption';
begin
  try
    with AnnualIFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (not AFeature.Validate(FErrorMessage, 'IFRCalcOption')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        CalcOptionCbx.InValidationError := True;
        CalcOptionCbx.ShowErrorState(TRue);
      end;
      if Trim(edtCalcOption.Text) = '' then
      begin
        FErrorMessage := 'ERROR:Null not allowed';
        FAllErrorMessages.Add(Trim(FErrorMessage));
        CalcOptionCbx.InValidationError := True;
        CalcOptionCbx.ShowErrorState(TRue);
      end
      else
      if (CalcOptionCbx.ItemIndex > 0) and (Trim(FErrorMessage) = '') and
        (StrToFloat(Trim(edtCalcOption.Text))= 0) then
      begin
        FErrorMessage := 'ERROR:Option 2 cannot be zero';
        FAllErrorMessages.Add(Trim(FErrorMessage));
        CalcOptionCbx.InValidationError := True;
        CalcOptionCbx.ShowErrorState(TRue);
      end;
      edtCalcOption.ContextValidationError :=  FErrorMessage;
      CalcOptionCbx.ValidationError :=  FErrorMessage;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
{
procedure TAnnualIFRFeatureValidator.UpdateSiteData;
const OPNAME ='TAnnualIFRFeatureValidator.UpdateSiteData';
var
  LIFRSite       : TIFRSiteDataObject;
  LIFRSiteList   : TIFRSiteDataList;
  LIFRFeature    : TIFRFeature;
  LIdentifier    : integer;
  LIndex         : integer;
begin
  try
    with AnnualIFRFeatureDialog do
    begin
      if (SitesCbx.ItemIndex < 0) then Exit;
      LIndex := AnnualIFRFeatureDialog.SitesCbx.ItemIndex;
      LIdentifier := Integer(SitesCbx.Items.Objects[LIndex]);

      LIFRSiteList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.IFRSiteList;
      LIFRSite := LIFRSiteList.IFRSiteDataByIdentifier[LIdentifier];
      if(LIFRSite <> nil) then
      begin
        LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.CastIFRFeatureByID(FFeatureID);
        if(LIFRFeature <> nil) then
        begin
          LIFRFeature.PopulateWithSiteData(LIFRSite);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

procedure TAnnualIFRFeatureValidator.UpdateCalculationOption;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateCalculationOption';
var
  lIFRFeature : IIFRFeature;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        if Trim(edtCalcOption.Text) = '' then Exit;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            edtCalcOption.FieldProperty.FieldName,
            edtCalcOption.Text, lMessage)) then
        begin
          CalcOptionCbx.ValidationError := LMessage;
          edtCalcOption.FieldValidationError := LMessage;
          LIFRFeature.CalculationOption := StrToFloat(Trim(edtCalcOption.Text));
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeatureCalculationOption);

          //CalcOptionCbx.ItemIndex := CalcOptionCbx.Items.IndexOf(FloatToStr(lIFRFeature.CalculationOption));
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateAnnualGrid;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateAnnualGrid';
var
  LIFRFeature : IIFRFeature;
  LValue      : double;
  LMessage    : string;
  LRow : integer;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];

    if (LIFRFeature <> nil) then
    begin
      LRow := AnnualIFRFeatureDialog.AnnualGrid.Row;
      if LRow > 0 then
      begin
        if Trim(AnnualIFRFeatureDialog.AnnualGrid.Cells[0,LRow]) = '' then
          LValue := 0.0
        else
          LValue := StrToFloat(AnnualIFRFeatureDialog.AnnualGrid.Cells[0,LRow]);
        AnnualIFRFeatureDialog.AnnualGrid.ValidationError[0, LRow, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AnnualInflow',FloatToStr(LValue),LMessage,LRow)) then
        begin
          LIFRFeature.AnnualInflow[LRow] := LValue;
          RePopulateAnnualGrid;
          DoContextValidation(dvtIFRFeatureAnnualInflows);
        end
        else
          AnnualIFRFeatureDialog.AnnualGrid.ValidationError[0, LRow, gveCellField] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateAnnualRelease;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateAnnualRelease';
var
  LIFRFeature : IIFRFeature;
  LValue : double;
  LMessage : string;
  LDaysInMonth : double;
  LRow,
  LCol : integer;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (LIFRFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        LCol := AnnualGrid.Col;
        LRow := AnnualGrid.Row;
        AnnualGrid.ValidationError[LCol, LRow, gveCellField] := '';
        if Trim(AnnualGrid.Cells[LCol, LRow]) <> '' then
          LValue := StrToFloat(AnnualGrid.Cells[LCol, LRow])
        else
          LValue := 0.0;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'IFRReleaseVariables', FloatToStr(LValue), LMessage, LRow, LCol)) then
        begin
          if(AnnualIFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
          begin
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LCol];
            lValue := (lValue /(60*60*24*LDaysInMonth))* Power(10, 6);
          end;
          lIFRFeature.ReleaseByIndexAndMonth[LRow, LCol] := LValue;
          RePopulateAnnualGrid;
          DoContextValidation(dvtIFRFeatureReleases);
        end
        else
          AnnualGrid.ValidationError[LCol, LRow, gveCellField] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.ValidateReleases(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateReleases';
var
  lMonth     : integer;
  lErrorCols : TStringlist;
  lErrors    : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        lErrorCols := TStringlist.Create;
        lErrors    := TStringlist.Create;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Releases')) then
        begin
          for lMonth := 1 to 12 do
            AnnualGrid.ValidationError[lMonth, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors,lErrorCols);
          for lMonth := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lMonth)) >= 0) then
              AnnualGrid.ValidationError[lMonth, 0, gveColContext] := lErrors.Text
            else
              AnnualGrid.ValidationError[lMonth, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.ValidateAnnualInFlow(AFeature: IIFRFeature);
const OPNAME = 'TAnnualIFRFeatureValidator.ValidateAnnualInFlow';
var
  lErrorCols : TStringlist;
  lErrors    : TStringList;
  LClass : integer;
begin
  try
    if (AFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        lErrorCols := TStringlist.Create;
        lErrors    := TStringlist.Create;
        try
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'AnnualInflow')) then
          begin
            AnnualGrid.ValidationError[0, 0, gveColContext] := '';
          end
          else
          begin

            ExtractErrorsAndColumns(FErrorMessage, lErrors,lErrorCols);
            AnnualGrid.ValidationError[0, 0, gveColContext] := '';
            for LClass := 1 to 10 do
            begin
              if (lErrorCols.IndexOf(IntToStr(LClass)) >= 0) then
                AnnualGrid.ValidationError[0, 0, gveColContext] := lErrors.Text
            end;
            FAllErrorMessages.AddStrings(lErrors);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrors);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TAnnualIFRFeatureValidator.OnReferenceNodesClick(Sender: TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.OnReferenceNodesClick';
begin
  try
    if(AnnualIFRFeatureDialog.ReferenceNodesCheckLbx.HasValueChanged) then
    begin
      UpdateReferenceNodes;
      CalculteMAROfSelectedNodes;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.CalculteMAROfSelectedNodes;
const OPNAME = 'TAnnualIFRFeatureValidator.CalculteMAROfSelectedNodes';
var
  LIndex,
  LCounter : Integer;
  LReservoirNameslist : TStringList;
  LMARValue: double;
begin
  try
    LMARValue := 0.0;
    LReservoirNameslist := TStringList.Create;
    try
      GetSelectedNodes(LReservoirNameslist);
      for LCounter := 0 to LReservoirNameslist.Count - 1 do
      begin
        UpdateMARDisplay(LReservoirNameslist.Strings[LCounter]);
        for LIndex := 0 to FFileNameContainer.Count - 1 do
        begin
          LMARValue := LMARValue + CalculateMAR(FFileNameContainer);
        end;
      end;
      AnnualIFRFeatureDialog.TotalMAREdit.Text := '';
      AnnualIFRFeatureDialog.TotalMAREdit.Text := Format('%.5f',[LMARValue]);
    finally
      LReservoirNameslist.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateMARDisplay(AReservoirName: string);
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateMARDisplay';
var
  LReservoirData : IReservoirData;
  LReservoirDataList : IReservoirDatalist;
  LCatchmentRef : Integer;
  LParamSetup : TParamSetup;
  LParamReference : IParamReference;
  LFileNames : TStringList;
begin
  try
    LFileNames := TStringList.Create;
    try
      LReservoirDataList := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
      LReservoirData := LReservoirDataList.ReservoirOrNodeByName[AReservoirName];
      if (LReservoirData <> nil) then
      begin
        LCatchmentRef := LReservoirData.ReservoirConfigurationData.CatchmentRef;
        LParamSetup   := TParamSetup.Create(AppModules);
        LParamReference := LParamSetup.ReferenceDataByCatchNumber[LCatchmentRef];
        TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFilesForCatchment(LCatchmentRef,LFileNames);
        PopulateFileNames(LFileNames);
      end;
    finally
      LFileNames.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.GetSelectedNodes(out ANodesNameList: TStringList);
const OPNAME = 'TAnnualIFRFeatureValidator.GetSelectedNodes';
var
  LIndex: Integer;
begin
  try
    for LIndex := 0 to AnnualIFRFeatureDialog.ReferenceNodesCheckLbx.Count -1 do
    begin
      if AnnualIFRFeatureDialog.ReferenceNodesCheckLbx.Checked[LIndex] then
      begin
        ANodesNameList.Add(
        ReturnSubstringFromChar(AnnualIFRFeatureDialog.ReferenceNodesCheckLbx.Items.Strings[LIndex],')'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAnnualIFRFeatureValidator.CalculateMAR(AFileNames: TStringList): double;
const OPNAME = 'TAnnualIFRFeatureValidator.CalculateMAR';
var
  LFieldName,
  LFileExt,
  LFileName: string;
  LDataSet: TAbstractModelDataset;
  LCount: integer;
  LIndex: integer;
  LMonthValue: double;

  LAnnualTotalVal,
  LMARTotalVal,
  LMARTotalAve : double;
  LMARTotalYrs : integer;
begin
  Result := 0.0;
  try
    LMARTotalAve := 0.0;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if (LDataSet <> nil) then
      begin
        for LCount := 0 to AFileNames.Count -1 do
        begin
          LFileName := AFileNames[LCount];
          LFileName := ExtractFileName(LFileName);
          LFileExt  := UpperCase(ExtractFileExt(LFileName));

          if (LFileExt <> '.INC') then
            Continue;

          LDataSet.DataSet.Close;
          if not TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFileDataSet(AFileNames[LCount],LDataSet) then
            Continue;
          if not LDataSet.DataSet.Active then
            Continue;
          if (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            Continue;
          LMARTotalVal  := 0.0;
          LMARTotalYrs  := 0;
          while not LDataSet.DataSet.Eof do
          begin
            LAnnualTotalVal := 0;
            for LIndex := 0 to 11 do
            begin
              LFieldName  := Format('%s%2.2d',['HydroMonthValue',LIndex + 1]);
              LMonthValue := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LAnnualTotalVal := LAnnualTotalVal + LMonthValue;
            end;
            LMARTotalVal  := LMARTotalVal  + LAnnualTotalVal;
            LMARTotalYrs  := LMARTotalYrs + 1;
            LDataSet.DataSet.Next;
          end;
          LMARTotalAve := (LMARTotalVal / LMARTotalYrs);
        end;
        Result := LMARTotalAve;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;  

procedure TAnnualIFRFeatureValidator.PopulateFileNames(AFileNameContainer: TStringList);
const OPNAME = 'TAnnualIFRFeatureValidator.PopulateFileNames';
var
  LFileExt,
  LFileName: string;
  LCount: integer;
begin
  try
    FFileNameContainer.Clear;
    FFileNameContainer.Add('');
    if Assigned(AFileNameContainer) then
    begin
      for LCount := 0 to AFileNameContainer.Count - 1  do
      begin
        LFileName := AFileNameContainer[LCount];
        LFileName := ExtractFileName(LFileName);
        LFileExt := UpperCase(ExtractFileExt(LFileName));
        if LFileExt = '.INC' then
          FFileNameContainer.Strings[0] := LFileName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TAnnualIFRFeatureValidator.OnIFRExistsClick(Sender: TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.OnIFRExistsClick';
begin
  try
    if(AnnualIFRFeatureDialog.ChkboxFIFRFeatureExists.HasValueChanged) then
      UpdateIFRFeatureExists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.OnIFRLossClick(Sender: TObject);
const OPNAME = 'TAnnualIFRFeatureValidator.OnIFRLossClick';
begin
  try
   // if(AnnualIFRFeatureDialog.IFRLoss.HasValueChanged) then
   //   UpdateIFRLoss;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TAnnualIFRFeatureValidator.UpdateIFRLoss;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateIFRLoss';
var
  LIFRLoss : integer;
  LErrorMessage: string;
  LIFRFeature  : IIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (LIFRFeature <> nil) then
    begin
      LIFRLoss := LIFRFeature.IFRLoss;

      if((LIFRLoss = 1) and ( not AnnualIFRFeatureDialog.IFRLoss.Checked) or
         (LIFRLoss <> 1) and (AnnualIFRFeatureDialog.IFRLoss.Checked)) then
      begin
        if AnnualIFRFeatureDialog.IFRLoss.Checked then
          LIFRLoss := 1
        else
          LIFRLoss := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         AnnualIFRFeatureDialog.IFRLoss.FieldProperty.FieldName,
         IntToStr(LIFRLoss),LErrorMessage) then
        begin
          LIFRFeature.IFRLoss := LIFRLoss;
          AnnualIFRFeatureDialog.IFRLoss.Checked := (LIFRFeature.IFRLoss = 1);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end
        else
        begin
          AnnualIFRFeatureDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAnnualIFRFeatureValidator.UpdateMonthlyIFRLossGrid(AIndex: integer; AValue: string);
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateMonthlyIFRLossGrid';
var
  lIFRFeature : IIFRFeature;
  lValue      : string;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with AnnualIFRFeatureDialog do
      begin
        MonthlyIFRLossGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MonthlyIFRLoss', AValue, lMessage, AIndex)) then
        begin
          lValue := FormatFloat('##0.00',StrToFloat(AValue));
          lIFRFeature.MonthlyIFRLossByIndex[AIndex] := StrToFloat(lValue);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end
        else
          MonthlyIFRLossGrid.ValidationError[1, AIndex-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  *)

procedure TAnnualIFRFeatureValidator.UpdateIFRFeatureExists;
const OPNAME = 'TAnnualIFRFeatureValidator.UpdateIFRFeatureExists';
var
  LIFRFeatureExists : integer;
  LErrorMessage: string;
  LIFRFeature  : IIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.AnnualIFRFeatureByID[FFeatureID];
    if (LIFRFeature <> nil) then
    begin
      LIFRFeatureExists := LIFRFeature.IFRStatusIndicator;

      if((LIFRFeatureExists = 1) and ( not AnnualIFRFeatureDialog.ChkboxFIFRFeatureExists.Checked) or
         (LIFRFeatureExists <> 1) and (AnnualIFRFeatureDialog.ChkboxFIFRFeatureExists.Checked)) then
      begin
        if AnnualIFRFeatureDialog.chkboxFIFRFeatureExists.Checked then
          LIFRFeatureExists := 1
        else
          LIFRFeatureExists := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         AnnualIFRFeatureDialog.chkboxFIFRFeatureExists.FieldProperty.FieldName,
         IntToStr(LIFRFeatureExists),LErrorMessage) then
        begin
          LIFRFeature.IFRStatusIndicator := LIFRFeatureExists;
          AnnualIFRFeatureDialog.ChkboxFIFRFeatureExists.Checked := (LIFRFeature.IFRStatusIndicator = 1);
        end
        else
        begin
          AnnualIFRFeatureDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


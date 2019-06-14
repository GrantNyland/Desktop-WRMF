//
//
//  UNIT      : Contains TIFRSiteValidator Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRSiteValidator;

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
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UIFRDataObject,
  UIFRSiteDialog;

type
  TIFRSiteValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure RePopulateDataViewer;
    procedure RepopulateChart;
    procedure RepopulateGrid;
    procedure DoOnGraphMonthCbxSelect(Sender : TObject);
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;

    procedure UpdateSiteName;
    procedure UpdateSiteDescription;
    procedure UpdateQuaternaryCatchment;
    procedure UpdateRiverName;
    procedure UpdateAssociatedEMC;
    procedure UpdateLevelOfDetail;
    procedure UpdateLevelOfConfidence;
    procedure UpdateDataSource;
    procedure UpdateXCoord;
    procedure UpdateYCoord;

    {
    procedure UpdateFeatureName;
    procedure UpdateLagInMonths;
    procedure UpdateNumberOfPoints;
    procedure UpdateInflowOption;
    procedure UpdateReferenceNodes;
    procedure UpdateInflow(AIndex : integer;
                           AMonth : integer;
                           AValue : string);
    procedure UpdateRelease(AIndex : integer;
                            AMonth : integer;
                            AValue : string);
    procedure UpdateExceedence(AIndex : integer;
                               AValue : string);
    procedure ValidateFeatureName(AFeature : IIFRFeature);
    procedure ValidateLagInMonths (AFeature : IIFRFeature);
    procedure ValidateReferenceNodes (AFeature : IIFRFeature);
    procedure ValidateInflows (AFeature : IIFRFeature);
    procedure ValidateReleases (AFeature : IIFRFeature);
    procedure ValidatePointsCount(AFeature: IIFRFeature); }
    procedure UpdateInflow(ACol : integer;
                           ARow : integer;
                           AValue : string);
    procedure ValidateInflows (AIFRData : TIFRSiteDataObject);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    {function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    property FeatureID : integer read FIdentifier write FIdentifier;}
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function IFRSiteDialog : TIFRSiteDialog;
  end;

implementation

uses
  VCLTee.Series,
  SysUtils,
  VCL.Graphics,
  ContNrs,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkElementData;

{******************************************************************************}
{* TIFRSiteValidator                                                       *}
{******************************************************************************}

procedure TIFRSiteValidator.CreateMemberObjects;
const OPNAME = 'TIFRSiteValidator.CreateMemberObjects';
var
  lPanel : TIFRSiteDialog;
  Lindex: integer;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TIFRSiteDialog.Create(FPanelOwner,FAppModules);
    lPanel := IFRSiteDialog;
    with lPanel do
    begin
      NameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteName');
      NameEdit.OnEnter       := OnEditControlEnter;
      NameEdit.OnExit        := OnEditControltExit;

      XCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteXCoord');
      XCoordEdit.OnEnter       := OnEditControlEnter;
      XCoordEdit.OnExit        := OnEditControltExit;

      YCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteYCoord');
      YCoordEdit.OnEnter       := OnEditControlEnter;
      YCoordEdit.OnExit        := OnEditControltExit;

      QuaternaryEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteQuaternaryCatchment');
      QuaternaryEdit.OnEnter       := OnEditControlEnter;
      QuaternaryEdit.OnExit        := OnEditControltExit;

      RiverEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteRiverName');
      RiverEdit.OnEnter       := OnEditControlEnter;
      RiverEdit.OnExit        := OnEditControltExit;

      DescriptionEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteDescr');
      DescriptionEdit.OnEnter       := OnEditControlEnter;
      DescriptionEdit.OnExit        := OnEditControltExit;

      AssociatedEMCCbx.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRSiteAssociatedEMC');
      AssociatedEMCCbx.OnEnter       := OnEditControlEnter;
      AssociatedEMCCbx.OnExit        := OnEditControltExit;

      DetailLevelCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteLevelDetail');
      DetailLevelCbx.OnEnter       := OnEditControlEnter;
      DetailLevelCbx.OnExit        := OnEditControltExit;

      ConfidenceLevelCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteLevelConfidence');
      ConfidenceLevelCbx.OnEnter       := OnEditControlEnter;
      ConfidenceLevelCbx.OnExit        := OnEditControltExit;

      InfoSourceEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRSiteSource');
      InfoSourceEdit.OnEnter       := OnEditControlEnter;
      InfoSourceEdit.OnExit        := OnEditControltExit;

      GraphViewTypeCbx.OnSelect    := DoOnGraphMonthCbxSelect;

      ValuesGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      ValuesGrid.OnColEnter         := OnStringGridColEnter;
      ValuesGrid.OnEnter            := OnEditControlEnter;
      ValuesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IFRSiteExceedence'));
      for Lindex := 1 to 12 do
      begin
        ValuesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IFRSiteFlow'));
      end;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.DestroyMemberObjects;
const OPNAME = 'TIFRSiteValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.Initialise: boolean;
const OPNAME = 'TIFRSiteValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIFRSiteValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.IFRFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.IFRSiteDialog : TIFRSiteDialog;
const OPNAME = 'TIFRSiteValidator.IFRSiteDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIFRSiteDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.SaveState: boolean;
const OPNAME = 'TIFRSiteValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.StudyHasChanged: boolean;
const OPNAME = 'TIFRSiteValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIFRSiteValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.ClearDataViewer;
const OPNAME = 'TIFRSiteValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with IFRSiteDialog do
    begin
      NameEdit.SetFieldValue('');
      XCoordEdit.SetFieldValue('');
      YCoordEdit.SetFieldValue('');
      QuaternaryEdit.SetFieldValue('');
      RiverEdit.SetFieldValue('');
      DescriptionEdit.SetFieldValue('');
      InfoSourceEdit.SetFieldValue('');

      AssociatedEMCCbx.ItemIndex  := -1;
      DetailLevelCbx.ItemIndex     := -1;
      ConfidenceLevelCbx.ItemIndex := -1;
      GraphViewTypeCbx.ItemIndex   := -1;

      AssociatedEMCCbx.Text   := '';
      DetailLevelCbx.Text   := '';
      ConfidenceLevelCbx.Text   := '';
      GraphViewTypeCbx.Text   := '';

      IFRGraph.SeriesList.Clear;

      ValuesGrid.RowCount := 2;
      ValuesGrid.Rows[0].Clear;
      ValuesGrid.Rows[1].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.PopulateDataViewer;
const OPNAME = 'TIFRSiteValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.RePopulateDataViewer;
const OPNAME = 'TIFRSiteValidator.RePopulateDataViewer';
var
  LIFRSiteData    : TIFRSiteDataObject;
  LIndex          : integer;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        NameEdit.SetFieldValue(LIFRSiteData.SiteName);
        XCoordEdit.SetFieldValue(LIFRSiteData.XCoord);
        YCoordEdit.SetFieldValue(LIFRSiteData.YCoord);
        QuaternaryEdit.SetFieldValue(LIFRSiteData.QuaternaryCatchment);
        RiverEdit.SetFieldValue(LIFRSiteData.RiverName);
        DescriptionEdit.SetFieldValue(LIFRSiteData.SiteDescription);
        InfoSourceEdit.SetFieldValue(LIFRSiteData.DataSource);

        AssociatedEMCCbx.ItemIndex := -1;
        AssociatedEMCCbx.Items.Clear;
        AssociatedEMCCbx.Items.Add('A');
        AssociatedEMCCbx.Items.Add('B');
        AssociatedEMCCbx.Items.Add('C');
        AssociatedEMCCbx.Items.Add('D');

        ConfidenceLevelCbx.ItemIndex := -1;
        ConfidenceLevelCbx.Items.Clear;
        ConfidenceLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteConfidenceLow'));
        ConfidenceLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteConfidenceMedium'));
        ConfidenceLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteConfidenceHigh'));

        DetailLevelCbx.ItemIndex := -1;
        DetailLevelCbx.Items.Clear;
        DetailLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDetailDesktop'));
        DetailLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDetailRapid'));
        DetailLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDetailIntermediate'));
        DetailLevelCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDetailComprehensive'));

        AssociatedEMCCbx.SetFieldIndex(AssociatedEMCCbx.Items.IndexOf(LIFRSiteData.AssociatedEMC));
        DetailLevelCbx.SetFieldIndex(DetailLevelCbx.Items.IndexOf(LIFRSiteData.LevelOfDetail));
        ConfidenceLevelCbx.SetFieldIndex(ConfidenceLevelCbx.Items.IndexOf(LIFRSiteData.LevelOfConfidence));

        GraphViewTypeCbx.Items.Clear;
        GraphViewTypeCbx.Items.Add(FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteAll'));
        for LIndex := 1 to High(LIFRSiteData.MonthNames) do
          GraphViewTypeCbx.Items.Add(LIFRSiteData.MonthNames[LIndex]);
        GraphViewTypeCbx.ItemIndex   := 0;

        RepopulateChart;
        RepopulateGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIFRSiteValidator.RepopulateChart;
const OPNAME = 'TIFRSiteValidator.RepopulateChart';
var
  LIFRSiteData : TIFRSiteDataObject;
  LRow,LCol    : integer;
  LMinIndex,
  LMaxIndex    : integer;
  LLineSeries  : TLineSeries;

begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      if(Length(LIFRSiteData.ExceedencePercentageArray) = 0) then Exit;
      with IFRSiteDialog do
      begin
        IFRGraph.RemoveAllSeries;
        if(GraphViewTypeCbx.ItemIndex = 0) then
        begin
          LMinIndex := 1;
          LMaxIndex := 12;
        end
        else
        begin
          LMinIndex := GraphViewTypeCbx.ItemIndex;
          LMaxIndex := LMinIndex;
        end;

        IFRGraph.Title.Text.Clear;
        IFRGraph.Title.Text.Add(GraphViewTypeCbx.Text);
        IFRGraph.LeftAxis.Title.Caption := FAppModules.Language.GetString('TIFRSiteDialog.DefinedIFRS');
        IFRGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
        IFRGraph.Legend.Visible := LMaxIndex > LMinIndex;
        for LCol := LMinIndex to LMaxIndex do
        begin
          LLineSeries             := TLineSeries.Create(IFRGraph);
          LLineSeries.ParentChart := IFRGraph;
          LLineSeries.Title       := LIFRSiteData.MonthNames[LCol];
          for LRow := High(LIFRSiteData.ExceedencePercentageArray) downto 0 do
          begin
            LLineSeries.AddY(LIFRSiteData.RequiredFlowsArray[LRow,LCol-1],FloatToStr(LIFRSiteData.ExceedencePercentageArray[LRow]))
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.RepopulateGrid;
const OPNAME = 'TIFRSiteValidator.RepopulateGrid';
var
  LIFRSiteData : TIFRSiteDataObject;
  LRow,LCol       : integer;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      if(Length(LIFRSiteData.ExceedencePercentageArray) = 0) then Exit;
      with IFRSiteDialog do
      begin
        ValuesGrid.RowCount := Length(LIFRSiteData.ExceedencePercentageArray) + 1;
        for LCol := 0 to 12 do
            ValuesGrid.Cells[LCol,0] := LIFRSiteData.MonthNames[LCol];
        for LRow := 1 to ValuesGrid.RowCount-1 do
        begin
           ValuesGrid.Cells[0,LRow] := FloatToStr(LIFRSiteData.ExceedencePercentageArray[LRow-1]);
           for LCol := 1 to 12 do
           begin
             ValuesGrid.Cells[LCol,LRow] := Format('%6.3f', [LIFRSiteData.RequiredFlowsArray[LRow-1,LCol-1]]);
           end;
        end;
      end;
      DoContextValidation(dvtIFRFeatureInflows);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.DoOnGraphMonthCbxSelect(Sender : TObject);
const OPNAME = 'TIFRSiteValidator.DoOnGraphMonthCbxSelect';
begin
  try
    RepopulateChart;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRSiteValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIFRSiteValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IFRSiteDialog do
    begin
      if ((Sender = NameEdit) and (NameEdit.HasValueChanged)) then
        UpdateSiteName
      else if((Sender = XCoordEdit) and (XCoordEdit.HasValueChanged)) then
        UpdateXCoord
      else if((Sender = YCoordEdit) and (YCoordEdit.HasValueChanged)) then
        UpdateYCoord
      else if((Sender = QuaternaryEdit) and (QuaternaryEdit.HasValueChanged)) then
        UpdateQuaternaryCatchment
      else if((Sender = RiverEdit) and (RiverEdit.HasValueChanged)) then
        UpdateRiverName
      else if((Sender = DescriptionEdit) and (DescriptionEdit.HasValueChanged)) then
        UpdateSiteDescription
      else if((Sender = AssociatedEMCCbx) and (AssociatedEMCCbx.HasValueChanged)) then
        UpdateAssociatedEMC
      else if((Sender = InfoSourceEdit) and (InfoSourceEdit.HasValueChanged)) then
        UpdateDataSource
      else if((Sender = DetailLevelCbx) and (DetailLevelCbx.HasValueChanged)) then
        UpdateLevelOfDetail
      else if((Sender = ConfidenceLevelCbx) and (ConfidenceLevelCbx.HasValueChanged)) then
        UpdateLevelOfConfidence
      else if((Sender = AssociatedEMCCbx) and (AssociatedEMCCbx.HasValueChanged)) then
        UpdateAssociatedEMC
      else if((Sender = DetailLevelCbx) and (DetailLevelCbx.HasValueChanged)) then
        UpdateLevelOfDetail
      else if((Sender = ConfidenceLevelCbx) and (ConfidenceLevelCbx.HasValueChanged)) then
        UpdateLevelOfConfidence;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIFRSiteValidator.OnStringGridCellDataHasChanged';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: double;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        if (ValuesGrid = ASender) then
        begin
          LValue := StrToFloat(ValuesGrid.Cells[ACol, ARow]);
          if ((ACol > 0) AND
              (NOT ValuesGrid.HasChanges[ACol,ARow])) then
            UpdateInflow(ACol,ARow, Trim(ValuesGrid.Cells[ACol, ARow]))
          else
            LIFRSiteData.ExceedencePercByIndex[ARow-1] := LValue;
          RepopulateChart;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateAssociatedEMC;
const OPNAME = 'TIFRSiteValidator.UpdateAssociatedEMC';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := AssociatedEMCCbx.Text;
        LIFRSiteData.AssociatedEMC := LValue;
        AssociatedEMCCbx.SetFieldIndex(AssociatedEMCCbx.Items.IndexOf(LIFRSiteData.AssociatedEMC));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateLevelOfConfidence;
const OPNAME = 'TIFRSiteValidator.UpdateLevelOfConfidence';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := ConfidenceLevelCbx.Text;
        LIFRSiteData.LevelOfConfidence := LValue;
        ConfidenceLevelCbx.SetFieldIndex(ConfidenceLevelCbx.Items.IndexOf(LIFRSiteData.LevelOfConfidence));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateLevelOfDetail;
const OPNAME = 'TIFRSiteValidator.UpdateLevelOfDetail';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := DetailLevelCbx.Text;
        LIFRSiteData.LevelOfDetail := LValue;
        DetailLevelCbx.SetFieldIndex(DetailLevelCbx.Items.IndexOf(LIFRSiteData.LevelOfDetail));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateDataSource;
const OPNAME = 'TIFRSiteValidator.UpdateDataSource';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := InfoSourceEdit.Text;
        LIFRSiteData.DataSource := LValue;
        InfoSourceEdit.SetFieldValue(LIFRSiteData.DataSource);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateQuaternaryCatchment;
const OPNAME = 'TIFRSiteValidator.UpdateQuaternaryCatchment';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := QuaternaryEdit.Text;
        LIFRSiteData.QuaternaryCatchment := LValue;
        QuaternaryEdit.SetFieldValue(LIFRSiteData.QuaternaryCatchment);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateRiverName;
const OPNAME = 'TIFRSiteValidator.UpdateRiverName';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := RiverEdit.Text;
        LIFRSiteData.RiverName := LValue;
        RiverEdit.SetFieldValue(LIFRSiteData.RiverName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateSiteDescription;
const OPNAME = 'TIFRSiteValidator.UpdateSiteDescription';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := DescriptionEdit.Text;
        LIFRSiteData.SiteDescription := LValue;
        DescriptionEdit.SetFieldValue(LIFRSiteData.SiteDescription);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateSiteName;
const OPNAME = 'TIFRSiteValidator.UpdateSiteName';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: string;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := NameEdit.Text;
        LIFRSiteData.SiteName := LValue;
        NameEdit.SetFieldValue(LIFRSiteData.SiteName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateXCoord;
const OPNAME = 'TIFRSiteValidator.UpdateXCoord';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: double;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := StrToFloat(XCoordEdit.Text);
        LIFRSiteData.XCoord := LValue;
        XCoordEdit.SetFieldValue(LIFRSiteData.XCoord);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateYCoord;
const OPNAME = 'TIFRSiteValidator.UpdateYCoord';
var
  LIFRSiteData : TIFRSiteDataObject;
  LValue: double;
begin
  try
    LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
    if(LIFRSiteData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        LValue := StrToFloat(YCoordEdit.Text);
        LIFRSiteData.YCoord := LValue;
        YCoordEdit.SetFieldValue(LIFRSiteData.YCoord);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.ValidateInflows(AIFRData: TIFRSiteDataObject);
const OPNAME = 'TIFRSiteValidator.ValidateInflows';
var
  lMonth     : integer;
  lErrorCols : TStringlist;
  lErrors    : TStringlist;
begin
  try
    if (AIFRData <> nil) then
    begin
      with IFRSiteDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrors    := TStringList.Create;
        FErrorMessage := '';
        if (AIFRData.Validate(FErrorMessage,'RequiredInflows')) then
        begin
          for lMonth := 1 to 12 do
            ValuesGrid.ValidationError[lMonth, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors, lErrorCols);
          for lMonth := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lMonth)) >= 0) then
              ValuesGrid.ValidationError[lMonth, 0, gveColContext] := lErrors.Text
            else
              ValuesGrid.ValidationError[lMonth, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIFRSiteValidator.DoContextValidation';
var
  lIFRData     : TIFRSiteDataObject;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lIFRData  := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
      if (lIFRData <> nil) then
      begin
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep2, dvtIFRFeatureInflows]) then
          ValidateInflows(lIFRData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteValidator.UpdateInflow(ACol, ARow: integer; AValue: string);
const OPNAME = 'TIFRSiteValidator.UpdateInflow';
var
  LIFRData    : TIFRSiteDataObject;
  lValue      : double;
  lMessage    : string;
begin
  try
    if FIdentifier >= 0 then
    begin
      lIFRData  := TIFRModelData(FAppModules.Model.ModelData).IFRSiteDataList.IFRSiteDataByIdentifier[FIdentifier];
      if (lIFRData <> nil) then
      begin
        with IFRSiteDialog do
        begin
          ValuesGrid.ValidationError[ACol, ARow, gveCellField] := '';
          if (Trim(AValue) = '') then
            AValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'IFRVariables', AValue, lMessage, ARow, ACol)) then
          begin
            lValue := StrToFloat(AValue);
            lIFRData.RequiredFlowsByIndex[ARow-1, ACol-1] := lValue;
            RepopulateGrid;
            DoContextValidation(dvtIFRFeatureInflows);
          end
          else
            ValuesGrid.ValidationError[ACol, ARow, gveCellField] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


//
//
//  UNIT      : Contains the class TReservoirPhysicalCharacteristicsValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirPhysicalCharacteristicsValidator;

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
  UStartingStorageCalculate,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UReservoirPhysicalCharacteristicsDialog;

type
  TReservoirPhysicalCharacteristicsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FBusyUpdating: boolean;
    FCalculateStartingStorageDialog:TStartingStorageCalculate;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(ASender: TObject); override;
    procedure OnEditControltExit(ASender: TObject); override;
    procedure OnElevationsPastedFromExcel(ASender: TObject);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnInsertRow(Sender: TObject);
    procedure OnDeleteRow(Sender: TObject);
    procedure OnCalculateStartingStoragePerc(Sender: TObject);
    procedure OnGetStartingStoragePerc(Sender: TObject);
    procedure OnSelectedCellHasChanged(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure OnAfterPasteColumnData(Sender: TObject);
    function GetVolumeFromElevation(AElevation : Double; AReservoirObject : IReservoirData) : Double;
    function GetElevationFromVolume(AVolume : Double; AReservoirObject : IReservoirData) : Double;
    function GetElevationFromArea(AArea : Double; AReservoirObject : IReservoirData) : Double;
    function ReservoirStartingStoragePercentage(AReservoirObject : IReservoirData) : Double;

    function PopulateElevationsChart(AReservoirObject : IReservoirData) : boolean;

    function UpdateValue(ASender : TFieldEdit) : boolean;
    procedure RePopulateDataViewer;
    function CurrentReservoir:IReservoirData;
    procedure ValidateAreaWhenFull(AReservoir : IReservoirData);
    procedure ValidateFullStorageLevel(AReservoir : IReservoirData);
    procedure ValidateStartingStorageLevel(AReservoir : IReservoirData);
    procedure ValidateDeadStorageLevel(AReservoir : IReservoirData);
    procedure ValidateBottomOfReservoir(AReservoir : IReservoirData);
    procedure ValidateElevation(AReservoir : IReservoirData);
    procedure ValidateVolumes(AReservoir : IReservoirData);
    procedure ValidateSurfaceArea(AReservoir : IReservoirData);
    procedure ValidatePointsCount(Areservoir : IReservoirData);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirPhysicalCharacteristicsDialog: TReservoirPhysicalCharacteristicsDialog;

    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses

  // Delphi VCl, RTL, etc

  SysUtils,
  VCLTee.Chart,
  UUtilities,
  VCLTee.TeeProcs,
  VCLTee.Series,
  VCLTee.TEEngine,
  Variants,
  Math,
  VCL.Grids,
  VCL.Graphics,
  VCL.Dialogs,
  // arivia.kom
  UConstants,
  UReservoirData,
  UYieldModelDataObject,
  UDDTSDataObject,
  UErrorHandlingOperations;

{ TReservoirPhysicalCharacteristicsValidator }

procedure TReservoirPhysicalCharacteristicsValidator.CreateMemberObjects;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.CreateMemberObjects';
//  var LFieldProperty: TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;
    FBusyUpdating := False;
    FIdentifier := -1;
    FCalculateStartingStorageDialog := nil;
    FPanel := TReservoirPhysicalCharacteristicsDialog.Create(FPanelOwner,FAppModules);

    ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull.FieldProperty := FAppModules.FieldProperties.FieldProperty('AreaFull');
    ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull.OnExit     := OnEditControltExit;
    ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull.IsEnabled  := False;

    ReservoirPhysicalCharacteristicsDialog.EdtStartingStorage.FieldProperty := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
    ReservoirPhysicalCharacteristicsDialog.EdtStartingStorage.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.EdtStartingStorage.OnExit     := OnEditControltExit;

    ReservoirPhysicalCharacteristicsDialog.EdtFullStorageLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('FullSupplyLevel');
    ReservoirPhysicalCharacteristicsDialog.EdtFullStorageLevel.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.EdtFullStorageLevel.OnExit     := OnEditControltExit;

    ReservoirPhysicalCharacteristicsDialog.StartingStorageBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
    ReservoirPhysicalCharacteristicsDialog.StartingStorageBtn.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.StartingStorageBtn.OnExit     := OnEditControltExit;
    ReservoirPhysicalCharacteristicsDialog.StartingStorageBtn.OnClick    := OnGetStartingStoragePerc;


    ReservoirPhysicalCharacteristicsDialog.EdtDeadStorageLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('DeadStorageLevel');
    ReservoirPhysicalCharacteristicsDialog.EdtDeadStorageLevel.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.EdtDeadStorageLevel.OnExit         := OnEditControltExit;

    ReservoirPhysicalCharacteristicsDialog.EdtBottomOfReservoir.FieldProperty  := FAppModules.FieldProperties.FieldProperty('BottomOfReservoir');
    ReservoirPhysicalCharacteristicsDialog.EdtBottomOfReservoir.OnEnter        := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.EdtBottomOfReservoir.OnExit         := OnEditControltExit;

    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SurfaceElevation'));
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Volume'));
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Area'));
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnColEnter := OnStringGridColEnter;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnEnter    := OnEditControlEnter;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnSelectCell := OnSelectedCellHasChanged;
    //ReservoirPhysicalCharacteristicsDialog.GrdElevation.AllowPasteFromExcel := True;
    //ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnPasteFromExcel := OnElevationsPastedFromExcel;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.ShowGridPopupMenu   := True;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AllowPasteFromExcel := True;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnPasteFromExcel    := Self.OnAfterPasteGridData;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;

    ReservoirPhysicalCharacteristicsDialog.BtnInsertRow.OnClick := OnInsertRow;
    ReservoirPhysicalCharacteristicsDialog.BtnDeleteRow.OnClick := OnDeleteRow;

    ReservoirPhysicalCharacteristicsDialog.DamBasinSurveyBtn.FieldProperty :=
      FAppModules.FieldProperties.FieldProperty('DamBasinSurvey');
    ReservoirPhysicalCharacteristicsDialog.DamBasinSurveyBtn.OnEnter := OnEditControlEnter;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.Initialise: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SurfaceElevation'));
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Volume'));
    ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Area'));
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.PhysicalCharacteristics');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ClearDataViewer;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull.SetFieldValue('');
    ReservoirPhysicalCharacteristicsDialog.EdtStartingStorage.SetFieldValue('');
    ReservoirPhysicalCharacteristicsDialog.EdtFullStorageLevel.SetFieldValue('');
    ReservoirPhysicalCharacteristicsDialog.EdtDeadStorageLevel.SetFieldValue('');
    ReservoirPhysicalCharacteristicsDialog.EdtBottomOfReservoir.SetFieldValue('');

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.SeriesList.Clear;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.UndoZoom;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.RePopulateDataViewer';
var
  lCol             : integer;
  lFieldIndex      : string;
  lKeyValues       : string;
  LPointsCount     : integer;
  LCount           : integer;
  LCanSelect       : boolean;
  LReservoirObject : IReservoirData;
  lFieldProperty   : TAbstractFieldProperty;
begin
  try
    if FBusyUpdating then Exit;
    if(FIdentifier >= 0) then
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        LReservoirObject := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

      if (LReservoirObject <> nil) then
      begin
        with ReservoirPhysicalCharacteristicsDialog do
        begin
          lFieldIndex    := '';

          lFieldProperty :=  EdtAreaWhenFull.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          EdtAreaWhenFull.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          if(LReservoirObject.ReservoirConfigurationData.AreaWhenFull = NullFloat) then
            EdtAreaWhenFull.SetFieldValue('')
          else
            EdtAreaWhenFull.SetFieldValue(FloatToStr(LReservoirObject.ReservoirConfigurationData.AreaWhenFull));

          lFieldProperty :=  EdtStartingStorage.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          EdtStartingStorage.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          if(LReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] = NullFloat) then
            EdtStartingStorage.SetFieldValue('')
          else
            EdtStartingStorage.SetFieldValue(
              FloatToStr(LReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1]));

          lFieldProperty :=  EdtFullStorageLevel.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          EdtFullStorageLevel.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          if(LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation = NullFloat) then
            EdtFullStorageLevel.SetFieldValue('')
          else
            EdtFullStorageLevel.SetFieldValue(
              FloatToStr(LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation));

          StartingStorageBtn.IsEnabled := (LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation > 0.0);

          lFieldProperty :=  EdtDeadStorageLevel.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          EdtDeadStorageLevel.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          if(LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation = NullFloat) then
            EdtDeadStorageLevel.SetFieldValue('')
          else
            EdtDeadStorageLevel.SetFieldValue(
              FloatToStr(LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation));

          lFieldProperty :=  EdtBottomOfReservoir.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
          EdtBottomOfReservoir.HasMetaData :=
             FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;
          if(LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation = NullFloat) then
            EdtBottomOfReservoir.SetFieldValue('')
          else
            EdtBottomOfReservoir.SetFieldValue(
              FloatToStr(LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation));

          lFieldProperty := DamBasinSurveyBtn.FieldProperty;
          lKeyValues     := LReservoirObject.GetKeyValues(lFieldProperty.FieldName, '');
          DamBasinSurveyBtn.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;

          LPointsCount := Max(0,LReservoirObject.ReservoirConfigurationData.PointsCount);
          LPointsCount := Min(LPointsCount,15);

          SetRowCount(LPointsCount+1);
          if (LPointsCount = 0) then
          begin
            GrdElevation.Cells[0,1] := ' ';
            GrdElevation.Cells[1,1] := ' ';
            GrdElevation.Cells[2,1] := ' ';

            GrdElevation.Options :=
            GrdElevation.Options - [goEditing,goHorzLine, goVertLine];
          end
          else
          if (LReservoirObject.ReservoirElevationsData <> nil) and
            (LReservoirObject.ReservoirVolumesData <> nil) and
            (LReservoirObject.ReservoirAreasData <> nil) then
          begin
            GrdElevation.Options :=
            GrdElevation.Options + [goEditing,goHorzLine, goVertLine];
            for lCol := 0 to GrdElevation.ColCount -1 do
            begin
              for LCount :=  1 to LPointsCount do
              begin
                lFieldProperty := GrdElevation.FieldProperty(lCol);
                lFieldIndex := IntToStr(lCol)+ ',' + IntToStr(LCount);
                lKeyValues := LReservoirObject.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
                GrdElevation.HasMetaData[lCol, LCount] :=
                  FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

                LFieldProperty    := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
                GrdElevation.AddFieldProperty(LFieldProperty);
                GrdElevation.Cells[0,LCount] :=  Format(LFieldProperty.FormatStringGrid,
                                                 [LReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LCount]]);
//                  FormatFloat('###0.0##', LReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LCount]);

                LFieldProperty    := FAppModules.FieldProperties.FieldProperty('Volume');
                ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(LFieldProperty);
                GrdElevation.Cells[1,LCount] :=Format(LFieldProperty.FormatStringGrid,
                                                [LReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LCount]]);
//                  FormatFloat('###0.0##', LReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LCount]);

                LFieldProperty    := FAppModules.FieldProperties.FieldProperty('Area');
                ReservoirPhysicalCharacteristicsDialog.GrdElevation.AddFieldProperty(LFieldProperty);
                GrdElevation.Cells[2,LCount] :=Format(LFieldProperty.FormatStringGrid,
                                                 [LReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LCount]]);
//                  FormatFloat('###0.0##', LReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LCount]);
              end;
            end;

          end;
        end;
        PopulateElevationsChart(LReservoirObject);
        ReservoirPhysicalCharacteristicsDialog.LanguageHasChanged;
      end;
    end;
    OnSelectedCellHasChanged(nil,0,0,LCanSelect);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.SaveState: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnEditControlEnter(ASender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(ASender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnEditControltExit';
var
  LReservoirObject : IReservoirData;
begin
  inherited OnEditControltExit(ASender);

  try
    if ASender.ClassName = TFieldEdit.ClassName then
      if TFieldEdit(ASender).HasValueChanged then
        if UpdateValue(TFieldEdit(ASender)) then
        begin
          if(FAppModules.Model.ModelName <> CDDTS) then
            LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
          else
            LReservoirObject := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
          if (LReservoirObject <> nil) then
          begin
            TFieldEdit(ASender).Color := clWindow;

            if ASender = ReservoirPhysicalCharacteristicsDialog.EdtAreaWhenFull then
            begin
              LReservoirObject.ReservoirConfigurationData.AreaWhenFull := StrToFloat(TFieldEdit(ASender).Text);
              DoContextValidation(dvtReservoirAreaWhenFull);
            end;

            if ASender = ReservoirPhysicalCharacteristicsDialog.EdtStartingStorage then
            begin
              LReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] := StrToFloat(TFieldEdit(ASender).Text);
              DoContextValidation(dvtReservoirStartingStorageLevel);
              DoContextValidation(dvtReservoirFullStorageLevel);
              DoContextValidation(dvtReservoirDeadStorageLevel);
              DoContextValidation(dvtReservoirBottomOfReservoir);
            end;

            if ASender = ReservoirPhysicalCharacteristicsDialog.EdtFullStorageLevel then
            begin
              LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation := StrToFloat(TFieldEdit(ASender).Text);
              DoContextValidation(dvtReservoirFullStorageLevel);
              DoContextValidation(dvtReservoirStartingStorageLevel);
              DoContextValidation(dvtReservoirDeadStorageLevel);
              DoContextValidation(dvtReservoirBottomOfReservoir);
              DoContextValidation(dvtReservoirAreaWhenFull);;
            end;

            if ASender = ReservoirPhysicalCharacteristicsDialog.EdtDeadStorageLevel then
            begin
              LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation := StrToFloat(TFieldEdit(ASender).Text);
              DoContextValidation(dvtReservoirDeadStorageLevel);
              DoContextValidation(dvtReservoirStartingStorageLevel);
//              DoContextValidation(dvtReservoirFullStorageLevel);
              DoContextValidation(dvtReservoirBottomOfReservoir);

            end;

            if ASender = ReservoirPhysicalCharacteristicsDialog.EdtBottomOfReservoir then
            begin
              LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation := StrToFloat(TFieldEdit(ASender).Text);
              DoContextValidation(dvtReservoirBottomOfReservoir);
              DoContextValidation(dvtReservoirStartingStorageLevel);
              DoContextValidation(dvtReservoirFullStorageLevel);
              DoContextValidation(dvtReservoirDeadStorageLevel);
            end;
          end;
          RePopulateDataViewer;
        end else begin end // Only Needed for the Else
      else

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.ReservoirPhysicalCharacteristicsDialog:TReservoirPhysicalCharacteristicsDialog;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ReservoirPhysicalCharacteristicsDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirPhysicalCharacteristicsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if FBusyUpdating then Exit;
    if(AFieldName = 'DeadStorageLevel') or
      (AFieldName = 'FullSupplyLevel') or
      (AFieldName = 'BottomOfReservoir') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.PopulateElevationsChart;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.PopulateElevationsChart';
var
  LSeries : TCustomSeries;
  LIndex : Integer;
begin
  Result := False;
  try
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.AllowPanning := pmBoth;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.AllowZoom := True;
    //ReservoirPhysicalCharacteristicsDialog.ChtElevation.BackWall.Brush.Color := clWhite;
    //ReservoirPhysicalCharacteristicsDialog.ChtElevation.BackWall.Brush.Style := bsClear;
    //ReservoirPhysicalCharacteristicsDialog.ChtElevation.BackWall.Pen.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Title.Text.Text := '';
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Title.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.AxisVisible := True;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.ClipPoints := True;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Frame.Visible := True;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Legend.Visible := True;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Legend.Alignment := laBottom;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.View3D := False;
    //ReservoirPhysicalCharacteristicsDialog.ChtElevation.View3DWalls := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RemoveAllSeries;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.SeriesList.Clear;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.UndoZoom;

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.AxisVisible := True;

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Legend.Alignment := laBottom;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.Title.Angle := 90;

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.Grid.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.TopAxis.Grid.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.Ticks.Visible := True;

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.LabelsSize        := 20 ;
    //ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.Grid.Color        := clRed;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.LeftAxis.MinorTickCount     := 4 ;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.MinorTickCount    := 4 ;

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.BottomAxis.Axis.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.LeftAxis.Axis.Visible := False;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.RightAxis.Axis.Visible := True;
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.TopAxis.Axis.Visible := False;

    // Starting Volume - Point Series (0)
    LSeries := TPointSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart := ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.VertAxis := aLeftAxis;
    LSeries.HorizAxis := aBottomAxis;
    LSeries.Pointer.Brush.Color := clRed;
    LSeries.Pointer.InflateMargins := True;
    LSeries.Pointer.Pen.Width := 1;
    LSeries.Pointer.Style := psRectangle;
    LSeries.Pointer.Visible := True;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.StartingVolume');

    // Volume - Line Series (1)
    LSeries := TLineSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart :=ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.VertAxis := aLeftAxis;
    LSeries.HorizAxis := aBottomAxis;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.Volume');
    LSeries.XValues.Order := loNone;
    LSeries.YValues.Order := loNone;

    // Surface Area - Line Series (2)
    LSeries := TLineSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart := ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.VertAxis := aLeftAxis;
    LSeries.HorizAxis := aTopAxis;
    LSeries.Color :=clBlue;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.SurfaceArea');
    LSeries.XValues.Order := loNone;
    LSeries.YValues.Order := loNone;

    // Depth (3)
    LSeries := TPointSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart := ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.VertAxis := aRightAxis;
    LSeries.HorizAxis := aBottomAxis;
    LSeries.Pointer.Visible := False;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.Depth');
    LSeries.ShowInLegend := False;
    LSeries.XValues.Order := loNone;
    LSeries.YValues.Order := loNone;

    // Area when full - Point series (4)
    LSeries := TPointSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart := ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.VertAxis := aLeftAxis;
    LSeries.HorizAxis := aTopAxis;
    LSeries.Pointer.Brush.Color := clGreen;
    LSeries.Pointer.InflateMargins := True;
    LSeries.Pointer.Pen.Width := 1;
    LSeries.Pointer.Style := psCircle;
    LSeries.Pointer.Visible := True;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.AreaWhenFull');

    // BOT, DSL, FSL - Point Series (5)
    LSeries := TPointSeries.Create(ReservoirPhysicalCharacteristicsDialog.ChtElevation);
    LSeries.ParentChart := ReservoirPhysicalCharacteristicsDialog.ChtElevation;
    LSeries.Pointer.Brush.Color := clBlack;
    LSeries.Pointer.Pen.Width := 3;
    LSeries.Pointer.Style := psCross;
    LSeries.Pointer.Visible := True;
    LSeries.VertAxis := aLeftAxis;
    LSeries.HorizAxis := aBottomAxis;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.BOT_DSL_FSL');

    for LIndex :=  1 to AReservoirObject.ReservoirElevationsData.StartElevation do
    begin
      ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[1].AddXY(
        AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex],
          AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex]);
      ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[2].AddXY(
        AReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LIndex],
         AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex]);

      ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[3].AddXY(0,
      AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex] -
      AReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);
    end;

    {ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[3].AddXY(0,0);
    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[3].AddXY(0,
      AReservoirObject.ReservoirElevationsData.ReservoirElevations[1] -
        AReservoirObject.ReservoirElevationsData.ReservoirElevations[
          AReservoirObject.ReservoirElevationsData.StartElevation]);
    }

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[5].AddXY(
      GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation, AReservoirObject),
        AReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[5].AddXY(
      GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation, AReservoirObject),
        AReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[5].AddXY(
      GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation, AReservoirObject),
        AReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation);

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[4].AddXY(
      AReservoirObject.ReservoirConfigurationData.AreaWhenFull,
        GetElevationFromArea(AReservoirObject.ReservoirConfigurationData.AreaWhenFull, AReservoirObject));

    ReservoirPhysicalCharacteristicsDialog.ChtElevation.Series[0].AddXY(
      GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1], AReservoirObject),
        AReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1]);

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.GetVolumeFromElevation(
         AElevation : Double; AReservoirObject : IReservoirData) : Double;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.GetVolumeFromElevation';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to AReservoirObject.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AElevation <= AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex]) and
         (AElevation >= AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex + 1]) then
      begin
        LVal1 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal2 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];
        LVal3 := AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex];
        LVal4 := AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := (AElevation-(LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)))))/((LVal1-LVal2)/(LVal3 - LVal4));
        except
          Result := AElevation; // Vertical ASYMTOTE
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.GetElevationFromVolume(AVolume: Double; AReservoirObject: IReservoirData): Double;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.GetElevationFromVolume';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to AReservoirObject.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AVolume <= AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex]) and
         (AVolume >= AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex + 1]) then
      begin
        LVal1 := AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex];
        LVal2 := AReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LIndex+1];
        LVal3 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal4 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := (AVolume-(LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)))))/((LVal1-LVal2)/(LVal3 - LVal4));
        except
          Result := AVolume; // Vertical ASYMTOTE
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.GetElevationFromArea(
         AArea : Double; AReservoirObject : IReservoirData) : Double;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.GetElevationFromArea';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
begin
  Result := 0;
  try
    for LIndex :=  1 to AReservoirObject.ReservoirElevationsData.StartElevation - 1 do
    begin
      if (AArea <= AReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LIndex]) and
         (AArea >= AReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LIndex + 1]) then
      begin
        LVal1 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex];
        LVal2 := AReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LIndex+1];
        LVal3 := AReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LIndex];
        LVal4 := AReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LIndex+1];

        if (abs((abs(LVal2) - abs(LVal1))) < 0.0001) then Continue;
        if (abs((abs(LVal4) - abs(LVal3))) < 0.0001) then Continue;

        try
          // Calculate X given Y for no vertical ASYMTOTE
          Result := AArea*((LVal1-LVal2)/(LVal3-LVal4))+LVal1-(LVal3*((LVal1-LVal2)/(LVal3-LVal4)));
        except
          Result := AArea; // Vertical ASYMTOTE  --- this might be incorrect because here we know x calculating Y
        end;

        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.UpdateValue(ASender: TFieldEdit) : boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.UpdateValue';
var
  LMessage : String;
  LReservoirObject : IReservoirData;
begin
  Result := False;
  try
    if (ASender.HasValueChanged) then
    begin
      Result := FAppModules.FieldProperties.ValidateFieldProperty(
                ASender.FieldProperty.FieldName,ASender.Text,LMessage,1);
      ASender.FieldValidationError := LMessage;
      if(FIdentifier >= 0) then
      begin
        if(FAppModules.Model.ModelName <> CDDTS) then
          LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
        else
         LReservoirObject := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
        PopulateElevationsChart(LReservoirObject);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnStringGridCellDataHasChanged(
  ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnStringGridCellDataHasChanged';
var
  LReservoirObject : IReservoirData;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if(FIdentifier >= 0) then
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        LReservoirObject := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        if (LReservoirObject.ReservoirConfigurationData.PointsCount > 0) then
        begin
          case ACol of
            0 :
              begin
                LReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[ARow] :=
                StrToFloat(TFieldStringGrid(ASender).Cells[ACol, ARow]);
                DoContextValidation(dvtReservoirElevation);
                DoContextValidation(dvtReservoirAreaWhenFull);
              end;
            1 :
              begin
                LReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[ARow] :=
                StrToFloat(TFieldStringGrid(ASender).Cells[ACol, ARow]);
                DoContextValidation(dvtReservoirVolume);
              end;
            2 :
              begin
                LReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[ARow] :=
                StrToFloat(TFieldStringGrid(ASender).Cells[ACol, ARow]);
                RePopulateDataViewer;
                DoContextValidation(dvtReservoirAreaWhenFull);
                DoContextValidation(dvtReservoirSurfaceArea);
              end;
          end;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtReservoirAreaWhenFull);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.PopulateDataViewer;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtReservoirCharacteristicsAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.CurrentReservoir';
begin
  Result := nil;
  try
     if(FAppModules.Model.ModelName <> CDDTS) then
       Result :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
     else
        Result :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateAreaWhenFull(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateAreaWhenFull';
begin
  try
    with ReservoirPhysicalCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'AreaWhenFull')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtAreaWhenFull.FieldValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.DoContextValidation';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try

        if(FAppModules.Model.ModelName <> CDDTS) then
          lReservoirList :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList
        else
          lReservoirList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList;

       // lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
        //                  NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtPointsCount]) then
            ValidatePointsCount(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirAreaWhenFull,
                                  dvtReservoirCharTwo,
                                  dvtReservoirSurfaceArea,
                                  dvtPointsCount]) then
            ValidateAreaWhenFull(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirStartingStorageLevel,
                                  dvtReservoirElevation,
                                  dvtPointsCount]) then
            ValidateStartingStorageLevel(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirFullStorageLevel,
                                  dvtReservoirElevation,
                                  dvtPointsCount]) then
            ValidateFullStorageLevel(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirDeadStorageLevel,
                                  dvtReservoirElevation,
                                  dvtPointsCount]) then
            ValidateDeadStorageLevel(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirBottomOfReservoir,
                                  dvtReservoirElevation,
                                  dvtPointsCount]) then
            ValidateBottomOfReservoir(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirCharOne,
                                  dvtReservoirVolume,
                                  dvtPointsCount]) then
            ValidateVolumes(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirElevation,
                                  dvtPointsCount]) then
            ValidateElevation(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll,
                                  dvtReservoirCharTwo,
                                  dvtReservoirSurfaceArea,
                                  dvtPointsCount]) then
            ValidateSurfaceArea(lReservoir);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
type TSeqSteps = (ssZero, ss1, ss2);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.DetermineWizardStatus';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        if(FAppModules.Model.ModelName <> CDDTS) then
          lReservoirList :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList
        else
          lReservoirList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList;

       // lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
       //                     NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          DoContextValidation(dvtReservoirCharacteristicsAll);
          if (lReservoir.ReservoirConfigurationData.PointsCount > 0) then
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

procedure TReservoirPhysicalCharacteristicsValidator.ValidateStartingStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateStartingStorageLevel';
begin
  try
    with ReservoirPhysicalCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoir.Validate(FErrorMessage,'StartingStorageLevel')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtStartingStorage.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateFullStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateFullStorageLevel';
begin
  try
    with ReservoirPhysicalCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoir.Validate(FErrorMessage,'FullStorageLevel')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtFullStorageLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateDeadStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateDeadStorageLevel';
begin
  try
    with ReservoirPhysicalCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoir.Validate(FErrorMessage,'DeadStorageLevel')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtDeadStorageLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateBottomOfReservoir(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateBottomOfReservoir';
begin
  try
    with ReservoirPhysicalCharacteristicsDialog do
    begin
      FErrorMessage := '';
      if (NOT AReservoir.Validate(FErrorMessage,'BottomOfReservoir')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtBottomOfReservoir.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateVolumes( AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateVolumes';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    if (AReservoir <> nil) then
    begin
      with ReservoirPhysicalCharacteristicsDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if AReservoir.Validate(FErrorMessage,'Volume') then
          begin
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
              GrdElevation.ValidationError[1, lCol-1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0 ) then
               GrdElevation.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
             else
               GrdElevation.ValidationError[1, lCol, gveCellContext] := '';
           end;
             FAllErrorMessages.Add(FErrorMessage);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateSurfaceArea(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateSurfaceArea';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    if (AReservoir <> nil) then
    begin
      with ReservoirPhysicalCharacteristicsDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if AReservoir.Validate(FErrorMessage,'SurfaceArea') then
          begin
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
              GrdElevation.ValidationError[2, lCol-1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0 ) then
               GrdElevation.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
             else
               GrdElevation.ValidationError[2, lCol, gveCellContext] := '';
           end;
             FAllErrorMessages.Add(FErrorMessage);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidatePointsCount(Areservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidatePointsCount';
begin
  try
    if (AReservoir <> nil) then
    begin
      with ReservoirPhysicalCharacteristicsDialog do
      begin
        FErrorMessage := '';
        if AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'PointsCount') then
          GrdElevation.ValidationError[0, 0, gveColContext] := ''
        else
        begin
          GrdElevation.ValidationError[0, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(FErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnDeleteRow(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnDeleteRow';
var
  LReservoirObject : IReservoirData;
  LIndex: integer;
begin
  try
    LIndex := ReservoirPhysicalCharacteristicsDialog.GrdElevation.Row;
    if(LIndex >= 0) then
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        LReservoirObject :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

    //  LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        if LReservoirObject.DeletePhysicalCharacteristicsRow(LIndex) then
        begin
          RePopulateDataViewer;
          DoContextValidation(dvtReservoirAreaWhenFull);
          DoContextValidation(dvtPointsCount);
          DoContextValidation(dvtReservoirAreaWhenFull);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnInsertRow(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnInsertRow';
var
  LReservoirObject : IReservoirData;
  LIndex: integer;
begin
  try
    LIndex := ReservoirPhysicalCharacteristicsDialog.GrdElevation.Row;
    if(LIndex >= 0) then
    begin
       if(FAppModules.Model.ModelName <> CDDTS) then
          LReservoirObject :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
        else
          LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

     // LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        if LReservoirObject.InsertPhysicalCharacteristicsRow(LIndex,0.0,0.0,0.0) then
        begin
          RePopulateDataViewer;
          DoContextValidation(dvtReservoirAreaWhenFull);
          DoContextValidation(dvtPointsCount);
          DoContextValidation(dvtReservoirAreaWhenFull);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnSelectedCellHasChanged(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnSelectedCellHasChanged';
{var
  LReservoirObject : IReservoirData;
  LFieldProperty: TAbstractFieldProperty;
}
begin
  try
    ReservoirPhysicalCharacteristicsDialog.ResetButtonState;
    {ReservoirPhysicalCharacteristicsDialog.BtnInsertRow.Enabled := False;
    ReservoirPhysicalCharacteristicsDialog.BtnDeleteRow.Enabled := False;
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if Assigned(LReservoirObject) then
    begin
      LFieldProperty := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
      if Assigned(LFieldProperty) then
        ReservoirPhysicalCharacteristicsDialog.BtnInsertRow.Enabled :=
        (LReservoirObject.ReservoirElevationsData.StartElevation <= LFieldProperty.ArrayHigh);
    end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.ValidateElevation(AReservoir: IReservoirData);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ValidateElevation';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if (AReservoir <> nil) then
    begin
      with ReservoirPhysicalCharacteristicsDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if AReservoir.Validate(FErrorMessage,'Elevation') then
          begin
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
              GrdElevation.ValidationError[0, lCol-1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to AReservoir.ReservoirConfigurationData.PointsCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0 ) then
               GrdElevation.ValidationError[0, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
             else
               GrdElevation.ValidationError[0, lCol, gveCellContext] := '';
           end;
             FAllErrorMessages.Add(FErrorMessage);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lReservoir     : IReservoirData;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FIdentifier <> 0)) then
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        lReservoir :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        lReservoir :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

     // lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
       //               NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        with ReservoirPhysicalCharacteristicsDialog do
        begin
          lFieldIndex        := '';
          lFieldProperty     := nil;
          if (FActiveControl  = DamBasinSurveyBtn) then
            lFieldProperty   := DamBasinSurveyBtn.FieldProperty;
          if (FActiveControl  = EdtAreaWhenFull) then
            lFieldProperty   := EdtAreaWhenFull.FieldProperty;
          if (FActiveControl  = EdtStartingStorage) then
            lFieldProperty   := EdtStartingStorage.FieldProperty;
          if (FActiveControl  = EdtFullStorageLevel) then
            lFieldProperty   := EdtFullStorageLevel.FieldProperty;
          if (FActiveControl  = EdtDeadStorageLevel) then
            lFieldProperty   := EdtDeadStorageLevel.FieldProperty;
          if (FActiveControl  = EdtBottomOfReservoir) then
            lFieldProperty   := EdtBottomOfReservoir.FieldProperty;
          if (FActiveControl  = GrdElevation) then
          begin
            lFieldIndex := IntToStr(GrdElevation.Col)+ ',' + IntToStr(GrdElevation.Row);
            lFieldProperty   := GrdElevation.FieldProperty(GrdElevation.Col);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
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

procedure TReservoirPhysicalCharacteristicsValidator.OnElevationsPastedFromExcel(ASender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnElevationsPastedFromExcel';
function AllGridValuesValid: boolean;
const OPNAME = 'UReservoirPhysicalCharacteristicsValidator.AllGridValuesValid';
var
  LCol,
  LRow: integer;
  LCellValue : double;
begin
  Result := False;
  try;
    for LRow := 1 to ReservoirPhysicalCharacteristicsDialog.GrdElevation.RowCount-1 do
    begin
      for LCol := 0 to ReservoirPhysicalCharacteristicsDialog.GrdElevation.ColCount-1 do
      begin
        LCellValue := StrToFloatDef(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[LCol,LRow],-1.0);
        if(LCellValue < 0) then Exit;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

var
  LReservoirElevations: TAbstractFieldProperty;
  lReservoir     : IReservoirData;
  LRow    : integer;
begin
  try
    LReservoirElevations := FAppModules.FieldProperties.FieldProperty('SurfaceElevation');
    if (ReservoirPhysicalCharacteristicsDialog.GrdElevation.ColCount <> 3)  then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.InvalidDataPasted'));
      RePopulateDataViewer;
    end
    else
    if ((ReservoirPhysicalCharacteristicsDialog.GrdElevation.RowCount-1) > LReservoirElevations.ArrayHigh)  then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.NoMoreThan')+IntToStr(LReservoirElevations.ArrayHigh)+FAppModules.Language.GetString('Message.PhysicalCharacteristics'));
      RePopulateDataViewer;
    end
    else
    if not AllGridValuesValid then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.DataNotFloatNumbers'));
      RePopulateDataViewer;
    end
    else
    begin
      if(FAppModules.Model.ModelName <> CDDTS) then
        lReservoir :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        lReservoir :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

     // lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
      //                NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir = nil) then
      begin
        RePopulateDataViewer;
      end
      else
      begin
        LRow := ReservoirPhysicalCharacteristicsDialog.GrdElevation.RowCount-1;
        while (lReservoir.ReservoirConfigurationData.PointsCount  <> LRow) do
        begin
          if(lReservoir.ReservoirConfigurationData.PointsCount  < LRow) then
            lReservoir.InsertPhysicalCharacteristicsRow(lReservoir.ReservoirConfigurationData.PointsCount+1,0.0,0.0,0.0)
          else
          lReservoir.DeletePhysicalCharacteristicsRow(lReservoir.ReservoirConfigurationData.PointsCount);
        end;
        FBusyUpdating := True;
        try
          for LRow := 1 to ReservoirPhysicalCharacteristicsDialog.GrdElevation.RowCount-1 do
          begin
            lReservoir.ReservoirElevationsData.ReservoirElevationsByIndex[LRow] :=
              StrToFloat(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[0, LRow]);

            lReservoir.ReservoirVolumesData.ReservoirVolumesByIndex[LRow] :=
              StrToFloat(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[1, LRow]);

            lReservoir.ReservoirAreasData.ReservoirAreasByIndex[LRow] :=
              StrToFloat(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[2, LRow]);
          end;
        finally
          FBusyUpdating := False;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtReservoirCharacteristicsAll);
        DoContextValidation(dvtReservoirAreaWhenFull);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPhysicalCharacteristicsValidator.ReservoirStartingStoragePercentage(AReservoirObject: IReservoirData): Double;
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.ReservoirStartingStoragePercentage';
var
  LDeadStorageVolume,
  LFullSupplyVolume,
  LStartingStorageVolume: double;
begin
  Result := 100.00;
  try
    if(AReservoirObject <> nil) then
    begin
      LFullSupplyVolume      := GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation,AReservoirObject);
      LStartingStorageVolume := GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1],AReservoirObject);
      LDeadStorageVolume     := GetVolumeFromElevation(AReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation,AReservoirObject);
      LFullSupplyVolume      := LFullSupplyVolume - LDeadStorageVolume;
      LStartingStorageVolume := LStartingStorageVolume - LDeadStorageVolume;
      if(LFullSupplyVolume > 0)then
        Result := LStartingStorageVolume/LFullSupplyVolume * 100;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnGetStartingStoragePerc(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnGetStartingStoragePerc';
var
  lReservoir     : IReservoirData;
begin
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
      lReservoir :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
    else
      lReservoir :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

   // lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
   //                 NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (lReservoir <> nil) then
    begin
      FCalculateStartingStorageDialog := TStartingStorageCalculate.CreateWithoutDFM(nil,FAppModules);
      try
        FCalculateStartingStorageDialog.btnCalculate.OnClick := OnCalculateStartingStoragePerc;
        FCalculateStartingStorageDialog.Initialise;
        FCalculateStartingStorageDialog.LanguageHasChanged;
        FCalculateStartingStorageDialog.Populate(
          lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation,
          lReservoir.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1],
          ReservoirStartingStoragePercentage(lReservoir));
        FCalculateStartingStorageDialog.ShowModal;
        if(FCalculateStartingStorageDialog.ModalResult = mrOk) then
        begin
          lReservoir.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1] := StrToFloat(FCalculateStartingStorageDialog.edtNewStartingStorage.Text);
          DoContextValidation(dvtReservoirStartingStorageLevel);
          DoContextValidation(dvtReservoirFullStorageLevel);
          DoContextValidation(dvtReservoirDeadStorageLevel);
          DoContextValidation(dvtReservoirBottomOfReservoir);
          RePopulateDataViewer;
        end;
      finally
        FreeAndNil(FCalculateStartingStorageDialog);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnCalculateStartingStoragePerc(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnCalculateStartingStoragePerc';
var
  lReservoir     : IReservoirData;
  LPerc,
  LFullSupplyVolume,
  LDeadStorageVolume,
  LLiveStorageVolune,
  LNewStartingStorageVolume,
  LNewStartingStorageLevel: double;
begin
  try
    if(FCalculateStartingStorageDialog <> nil) then
    begin
      LPerc       := StrToFloatDef(FCalculateStartingStorageDialog.edtPercentage.Text,-1.0);
      if(LPerc < 0.0) or (Lperc > 100.0) then
      begin
        ShowMessage('The percentage must be between 0.0 and 100.0');
        Exit;
      end;
      if(FAppModules.Model.ModelName <> CDDTS) then
        lReservoir :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
      else
        lReservoir :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

      //lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
      //                NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        LFullSupplyVolume  := GetVolumeFromElevation(lReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation,lReservoir);
        LDeadStorageVolume := GetVolumeFromElevation(lReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation,lReservoir);

        LLiveStorageVolune :=  LFullSupplyVolume - LDeadStorageVolume;


        LNewStartingStorageVolume   := LLiveStorageVolune * LPerc / 100.00;
        LNewStartingStorageVolume   := LDeadStorageVolume + LNewStartingStorageVolume;
        LNewStartingStorageLevel    := GetElevationFromVolume(LNewStartingStorageVolume,lReservoir);

        FCalculateStartingStorageDialog.edtNewStartingStorage.Text := FormatFloat('##0.00',LNewStartingStorageLevel);
        FCalculateStartingStorageDialog.btnOK.Enabled :=
          (FCalculateStartingStorageDialog.edtNewStartingStorage.Text <>FCalculateStartingStorageDialog.edtOldStartingStorage.Text);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnAfterPasteGridData';
begin
  try
    OnElevationsPastedFromExcel(Sender);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirPhysicalCharacteristicsValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TReservoirPhysicalCharacteristicsValidator.OnAfterPasteColumnData';
var
  LReservoirObject : IReservoirData;
  LCol,
  LRow,
  LIndex           : integer;
  LValue           : double;
begin
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
      LReservoirObject :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier]
    else
      LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

    //LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if(LReservoirObject <> nil) then
    begin
      if(Sender = ReservoirPhysicalCharacteristicsDialog.GrdElevation) then
      begin
        LCol := ReservoirPhysicalCharacteristicsDialog.GrdElevation.Col;
        if(LReservoirObject.ReservoirConfigurationData.PointsCount > 0) then
        begin
          LRow := ReservoirPhysicalCharacteristicsDialog.GrdElevation.RowCount;

          if(LReservoirObject.ReservoirConfigurationData.PointsCount <> (LRow - 1)) then
          begin
            for LIndex := LReservoirObject.ReservoirConfigurationData.PointsCount + 1 to LRow - 1 do
              LReservoirObject.InsertPhysicalCharacteristicsRow(LIndex,0.0,0.0,0.0)
          end;

          case LCol of
            0:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[LCol,LRow]));
                  LReservoirObject.ReservoirElevationsData.ReservoirElevationsByIndex[LRow] := LValue;
                  DoContextValidation(dvtReservoirElevation);
                end;
              end;
            1:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[LCol,LRow]));
                  LReservoirObject.ReservoirVolumesData.ReservoirVolumesByIndex[LRow] := LValue;
                  DoContextValidation(dvtReservoirVolume);
                end;
              end;
            2:
              begin
                for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
                begin
                  LValue := StrToFloat(Trim(ReservoirPhysicalCharacteristicsDialog.GrdElevation.Cells[LCol,LRow]));
                  LReservoirObject.ReservoirAreasData.ReservoirAreasByIndex[LRow] := LValue;
                  DoContextValidation(dvtReservoirAreaWhenFull);
                  DoContextValidation(dvtReservoirSurfaceArea);
                end;
              end;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtReservoirCharacteristicsAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.



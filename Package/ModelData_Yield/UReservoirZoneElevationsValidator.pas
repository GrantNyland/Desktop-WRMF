//
//
//  UNIT      : Contains the class TReservoirZoneElevationsValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirZoneElevationsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Math,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UReservoirZoneElevationsDialog;

type
  TReservoirZoneElevationsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FValidateCategoryIndex : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(ASender: TObject); override;
    procedure OnEditControltExit(ASender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteRowsData(Sender: TObject);
    procedure OnAfterPasteColumnData(Sender: TObject);

    function PopulateZoneEevationsChart(AReservoirObject: IReservoirData) : boolean;

    function CanDrawPenaltyLabel(ASender : TObject) : boolean;
    procedure DrawPenaltyLabel(ASender : TObject);

    function UpdateValue(ASender: TFieldEdit) : boolean;
    function UpdateElevation(ACol, ARow: integer;AValue: string): boolean;

    procedure RePopulateDataViewer;

    function CurrentReservoir:IReservoirData;

    procedure ValidateMonthlyZoneElevation(AReservoir: IReservoirData);
    procedure ValidateFullStorageLevel(AReservoir: IReservoirData);
    procedure ValidateDeadStorageLevel(AReservoir: IReservoirData);
    procedure ValidateBottomStorageLevel(AReservoir: IReservoirData);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirZoneElevationsDialog: TReservoirZoneElevationsDialog;
    procedure DoContextValidation(AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus(ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  SysUtils,
  VCLTee.Chart,
  UUtilities,
  VCL.Grids,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCL.Graphics,
  UConstants,

  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TReservoirZoneElevationsValidator }

procedure TReservoirZoneElevationsValidator.CreateMemberObjects;
const OPNAME = 'TReservoirZoneElevationsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FValidateCategoryIndex := 0;
    FPanel := TReservoirZoneElevationsDialog.Create(FPanelOwner,FAppModules);

    ReservoirZoneElevationsDialog.EdtFullSupplyLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('FullSupplyLevel');
    ReservoirZoneElevationsDialog.EdtFullSupplyLevel.OnEnter := OnEditControlEnter;
    ReservoirZoneElevationsDialog.EdtFullSupplyLevel.OnExit := OnEditControltExit;

    ReservoirZoneElevationsDialog.EdtDeadStorageLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('DeadStorageLevel');
    ReservoirZoneElevationsDialog.EdtDeadStorageLevel.OnEnter := OnEditControlEnter;
    ReservoirZoneElevationsDialog.EdtDeadStorageLevel.OnExit := OnEditControltExit;

    ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('BottomOfReservoir');
    ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel.OnEnter := OnEditControlEnter;
    ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel.OnExit := OnEditControltExit;

    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev01'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev02'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev03'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev04'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev05'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev06'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev07'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev08'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev09'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev10'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev11'));
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev12'));

    ReservoirZoneElevationsDialog.GrdZone.OnEnter := OnEditControlEnter;
    ReservoirZoneElevationsDialog.GrdZone.OnExit := OnEditControltExit;
    ReservoirZoneElevationsDialog.GrdZone.OnColEnter := OnStringGridColEnter;
    ReservoirZoneElevationsDialog.GrdZone.ShowGridPopupMenu   := True;
    ReservoirZoneElevationsDialog.GrdZone.AllowPasteFromExcel := True;
    ReservoirZoneElevationsDialog.GrdZone.OnPasteFromExcel    := Self.OnAfterPasteRowsData;
    ReservoirZoneElevationsDialog.GrdZone.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
    ReservoirZoneElevationsDialog.GrdZone.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
    ReservoirZoneElevationsDialog.GrdZone.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteRowsData;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirZoneElevationsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.Initialise: boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirLev01'));
    for LIndex := 1 to 12 do
      ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(
        FAppModules.FieldProperties.FieldProperty(Format('%s%2.2d',['ReservoirLev',LIndex])));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.ZoneElevations');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.ClearDataViewer;
const OPNAME = 'TReservoirZoneElevationsValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    ReservoirZoneElevationsDialog.EdtFullSupplyLevel.SetFieldValue('-9999.99');
    ReservoirZoneElevationsDialog.EdtDeadStorageLevel.SetFieldValue('-9999.99');
    ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel.SetFieldValue('-9999.99');
    ReservoirZoneElevationsDialog.NoOfDrawDownZones := 1;

    if Assigned(TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData) then
      for LIndex := 1 to 12 do
        ReservoirZoneElevationsDialog.GrdZone.Cells[LIndex, 0] :=
          TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex];

    for LIndex := 0 to ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 1 do
      ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex].Clear;

    for LIndex := 0 to ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 1 do
      ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex].AfterDrawValues := nil;

    ReservoirZoneElevationsDialog.ReservoirsAffectedPanel.Caption := '';

    ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Clear;
    ReservoirZoneElevationsDialog.ChtElevation.UndoZoom;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirZoneElevationsValidator.RePopulateDataViewer';
var
  LAffectedReservoirObject,
  LReservoirObject      : IReservoirData;
  LPenaltyStructureData : IReservoirPenalty;
  LZoneCount            : integer;
  LIndex                : integer;
  LReservoirString      : string;
  LConnectString        : string;
  LMessage              : string;
  lFieldProperty        : TAbstractFieldProperty;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin

        ReservoirZoneElevationsDialog.EdtFullSupplyLevel.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation ));
        ReservoirZoneElevationsDialog.EdtDeadStorageLevel.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation));
        ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel.SetFieldValue(
          FloatToStr(LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation));

        ReservoirZoneElevationsDialog.NoOfDrawDownZones :=
          LReservoirObject.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount;

        if(LReservoirObject.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount = 0) then
          ReservoirZoneElevationsDialog.GrdZone.Options := ReservoirZoneElevationsDialog.GrdZone.Options - [goEditing]
        else
        begin
          ReservoirZoneElevationsDialog.GrdZone.Options := ReservoirZoneElevationsDialog.GrdZone.Options + [goEditing];
          for LZoneCount := 0 to LReservoirObject.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1 do
            for LIndex := 1 to 12 do
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirElevation');
              ReservoirZoneElevationsDialog.GrdZone.AddFieldProperty(lFieldProperty);
              ReservoirZoneElevationsDialog.GrdZone.Cells[LIndex, LZoneCount + 1] :=
                Format(lFieldProperty.FormatStringGrid, [LReservoirObject.ReservoirZoneElevationsData.DrawDownLevelByIndex[LZoneCount].MonthlyElevationByIndex[LIndex]]);
            end;
        end;

        LReservoirString := '';
        LConnectString := '';

        if Assigned(FAppModules.Model().ModelData()) and (LReservoirObject.ReservoirPenaltyStructureData <> nil)then
        begin
          for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirCount - 1 do
          begin
            LAffectedReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIndex[LIndex];
            LPenaltyStructureData    := LAffectedReservoirObject.ReservoirPenaltyStructureData;
            if (LPenaltyStructureData <> nil) then
            begin
              if (LPenaltyStructureData.ReservoirPenaltyID = LReservoirObject.ReservoirPenaltyStructureData.ReservoirPenaltyID)
              and (LAffectedReservoirObject.ReservoirConfigurationData.ReservoirIdentifier <>  LReservoirObject.ReservoirConfigurationData.ReservoirIdentifier) then
              begin
                LReservoirString := LReservoirString + LConnectString + LAffectedReservoirObject.ReservoirConfigurationData.ReservoirName;

                LConnectString := ', ';
              end;
            end;
          end;
        end;
        LMessage := FAppModules.Language.GetString('Message.OtherReservoirsAffected');
        ReservoirZoneElevationsDialog.ReservoirsAffectedPanel.Caption := LMessage + LReservoirString;
        PopulateZoneEevationsChart(LReservoirObject);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.SaveState: boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.OnEditControlEnter(ASender: TObject);
const OPNAME = 'TReservoirZoneElevationsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(ASender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TReservoirZoneElevationsValidator.OnEditControltExit';
var
  LReservoirObject : IReservoirData;
begin
  inherited OnEditControltExit(ASender);
  try

    if ASender.ClassName = TFieldEdit.ClassName then
      if TFieldEdit(ASender).HasValueChanged then
        if UpdateValue(TFieldEdit(ASender)) then
        begin

          LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[
            FIdentifier];

          if (LReservoirObject <> nil) then
          begin
            // Only consider TFieldEdit's here

              if ASender = ReservoirZoneElevationsDialog.EdtFullSupplyLevel then
              begin
                LReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation := StrToFloat(TFieldEdit(ASender).Text);
                DoContextValidation(dvtReservoirFullStorageLevel);
                DoContextValidation(dvtReservoirElevation);
              end;

              if ASender = ReservoirZoneElevationsDialog.EdtDeadStorageLevel then
              begin
                LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation := StrToFloat(TFieldEdit(ASender).Text);
                DoContextValidation(dvtReservoirDeadStorageLevel);
                DoContextValidation(dvtReservoirElevation);
              end;

              if ASender = ReservoirZoneElevationsDialog.EdtBottomOfReservoirLevel then
              begin
                LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation := StrToFloat(TFieldEdit(ASender).Text);
                DoContextValidation(dvtReservoirBottomOfReservoir);
                DoContextValidation(dvtReservoirElevation);   
              end;
          end;

          RePopulateDataViewer;

        end
      else begin end

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.ReservoirZoneElevationsDialog:TReservoirZoneElevationsDialog;
const OPNAME = 'TReservoirZoneElevationsValidator.ReservoirZoneElevationsDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirZoneElevationsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'ReservoirZoneName') and (AContext in [sdccDelete,sdccAdd]) then
      PopulateDataViewer;
    if(AFieldName = 'ReservoirZoneName') or
      (AFieldName = 'DeadStorageLevel') or
      (AFieldName = 'ReservoirPenalty') or
      (AFieldName = 'FullSupplyLevel') or
      (AFieldName = 'BottomOfReservoir') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.PopulateZoneEevationsChart(AReservoirObject: IReservoirData): boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.PopulateZoneEevationsChart';
var
  LSeries        : TCustomSeries;
  LIndex         : integer;
  LDrawDownCount : integer;
  LMonthNames    : Array of String;
  LBackSeriesHeight,
  LMax,
  LMin : double;
begin
  Result := False;
  try
    if (AReservoirObject <> nil) then
    begin
      SetLength(LMonthNames, 13);
      for LIndex := 1 to 12 do
        LMonthNames[LIndex - 1] :=
          TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex];

      LMonthNames[12] := '';

      ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Clear;
      ReservoirZoneElevationsDialog.ChtElevation.Align := alClient;

      ReservoirZoneElevationsDialog.ChtElevation.MarginLeft := 2;
      ReservoirZoneElevationsDialog.ChtElevation.MarginRight := 2;
      ReservoirZoneElevationsDialog.ChtElevation.MarginTop := 2;
      ReservoirZoneElevationsDialog.ChtElevation.MarginBottom := 2;

      for LIndex := 0 to AReservoirObject.ReservoirZoneElevationsData.ReservoirZoneLevelsCount do
      begin
        LSeries := TAreaSeries.Create(ReservoirZoneElevationsDialog.ChtElevation);
        LSeries.ParentChart := ReservoirZoneElevationsDialog.ChtElevation;
        LSeries.DrawArea := True;
        LSeries.Pointer.Visible := False;
        LSeries.Stairs := True;
        TAreaseries(LSeries).MultiArea := maNone;
        TAreaseries(LSeries).AreaLinesPen.Width := 3;
      end;

      for LIndex := 0 to ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 2 do
        ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex].AfterDrawValues := DrawPenaltyLabel;

      LSeries := TAreaSeries(ReservoirZoneElevationsDialog.ChtElevation.Series[0]);
      LSeries.AreaLinesPen.Visible := False;
      LSeries.AreaBrush := bsClear;
      LSeries.SeriesColor := ReservoirZoneElevationsDialog.ChtElevation.Color;

      for LIndex := 1 to 13 do
      begin
        ReservoirZoneElevationsDialog.ChtElevation.Series[1].AddXY(LIndex,
          AReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation);

        for LDrawDownCount := 0 to AReservoirObject.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1 do
          ReservoirZoneElevationsDialog.ChtElevation.Series[LDrawDownCount + 2].AddXY(LIndex,
            AReservoirObject.ReservoirZoneElevationsData.DrawDownLevelByIndex[
              LDrawDownCount].MonthlyElevationByIndex[LIndex - LIndex div 13]);

      end;

      ReservoirZoneElevationsDialog.ChtElevation.Series[AReservoirObject.ReservoirZoneElevationsData.ReservoirZoneLevelsCount - 1].AddXY(1,
        AReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
      ReservoirZoneElevationsDialog.ChtElevation.Series[AReservoirObject.ReservoirZoneElevationsData.ReservoirZoneLevelsCount - 1].AddXY(13,
       AReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);


      ReservoirZoneElevationsDialog.ChtElevation.Series[AReservoirObject.ReservoirZoneElevationsData.ReservoirZoneLevelsCount].AddXY(1,
        AReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);
      ReservoirZoneElevationsDialog.ChtElevation.Series[AReservoirObject.ReservoirZoneElevationsData.ReservoirZoneLevelsCount].AddXY(13,
        AReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);

      LMax := 0.0;
      LMin := 0.0;
      ReservoirZoneElevationsDialog.ChtElevation.LeftAxis.CalcMinMax(LMin,LMax);
      for LIndex := 1 to 13 do
      begin
        LBackSeriesHeight := (LMax - LMin) / 10;
        ReservoirZoneElevationsDialog.ChtElevation.Series[0].AddXY(LIndex,
        AReservoirObject.ReservoirZoneElevationsData.FullSupplyLevel.Elevation + LBackSeriesHeight,
        LMonthNames[LIndex - 1]);
      end;

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.DrawPenaltyLabel(ASender : TObject);
const OPNAME  = 'TReservoirZoneElevationsValidator.DrawPenaltyLabel';
var
  LReservoirObject : IReservoirData;
  LIndex           : integer;
  LInnerIndex      : integer;
  lPenaltyVal      : double;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LReservoirObject :=
        TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];

      if (LReservoirObject <> nil) and (LReservoirObject.ReservoirPenaltyStructureData <> nil) then
      begin
        for LIndex := 0 to ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 1 do
        begin
          if ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex] = ASender then
          begin
            if CanDrawPenaltyLabel(ASender) then
            begin
              lPenaltyVal := LReservoirObject.ReservoirPenaltyStructureData.ReservoirPenaltyValueByIndex[LIndex+1];
              if (lPenaltyVal <> NullFloat) then
                ReservoirZoneElevationsDialog.ChtElevation.Canvas.TextOut(
                  TCustomSeries(ASender).CalcXPos(0) + 6,
                  TCustomSeries(ASender).CalcYPos(0),
                  FloatToStr(lPenaltyVal));

              if LIndex < (ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 2) then
                if LIndex > 0 then
                  for LInnerIndex := 1 to 11 do
                  begin
                    if CanDrawPenaltyLabel(ASender) then
                    begin
                      lPenaltyVal := LReservoirObject.ReservoirPenaltyStructureData.ReservoirPenaltyValueByIndex[LIndex+1];
                      if (lPenaltyVal <> NullFloat) then
                      begin
                        ReservoirZoneElevationsDialog.ChtElevation.Canvas.TextOut(
                          TCustomSeries(ASender).CalcXPos(LInnerIndex) + 6,
                          TCustomSeries(ASender).CalcYPos(LInnerIndex),
                          FloatToStr(lPenaltyVal));
                      end;
                    end;
                  end;
              Break;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.UpdateValue(ASender: TFieldEdit) : boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.UpdateValue';
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
      ASender.FieldValidationError :=  LMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirZoneElevationsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if(ASender =  ReservoirZoneElevationsDialog.GrdZone) then
       UpdateElevation(ACol, ARow,ReservoirZoneElevationsDialog.GrdZone.Cells[ACol, ARow]);
    DoContextValidation(dvtReservoirElevation);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.UpdateElevation(ACol,ARow: integer; AValue: string): boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.UpdateElevation';
var
  LReservoirData     : IReservoirData;
  LDrawDownElevation : IDrawDownElevation;
  LElevationValue    : double;
begin
  Result := False;
  try
    LElevationValue := StrToFloat(AValue);
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoirData <> nil) then
    begin

      LDrawDownElevation := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[ARow-1];
      if (LDrawDownElevation <> nil) then
      begin
        LDrawDownElevation.MonthlyElevationByIndex[ACol] := LElevationValue;
        ReservoirZoneElevationsDialog.GrdZone.Cells[ACol,ARow] := FloatToStr(LElevationValue);
        PopulateZoneEevationsChart(LReservoirData);
        FValidateCategoryIndex := ARow;
        DoContextValidation(dvtReservoirElevation);
        FValidateCategoryIndex := 0;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.PopulateDataViewer;
const OPNAME = 'TReservoirZoneElevationsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtReservoirCharacteristicsAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TReservoirZoneElevationsValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).
              NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TReservoirZoneElevationsValidator.DoContextValidation';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          if (AValidationType in [dvtReservoirCharacteristicsAll, dvtReservoirElevation]) then
            ValidateMonthlyZoneElevation(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll, dvtReservoirFullStorageLevel]) then
            ValidateFullStorageLevel(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll, dvtReservoirDeadStorageLevel]) then
            ValidateDeadStorageLevel(lReservoir);
          if (AValidationType in [dvtReservoirCharacteristicsAll, dvtReservoirBottomOfReservoir]) then
            ValidateBottomStorageLevel(lReservoir);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReservoirZoneElevationsValidator.DetermineWizardStatus';
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
          DoContextValidation(dvtReservoirCharacteristicsAll);
          if (lReservoir.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount <> 0) then
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

procedure TReservoirZoneElevationsValidator.ValidateMonthlyZoneElevation(AReservoir: IReservoirData);
const OPNAME = 'TReservoirZoneElevationsValidator.ValidateMonthlyZoneElevation';
var
  lCol        : integer;
  lIndex      : integer;
  lCount      : integer;
  lErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lElevation  : TAbstractFieldProperty;
begin
  try
    if (AReservoir <> nil) then
    begin
    with ReservoirZoneElevationsDialog do
      begin
        lElevation := FAppModules.Fieldproperties.FieldProperty('ReservoirLev');
        if assigned (lElevation) then
        begin
          lErrorCols := TStringList.Create;
          lErrorMsgs := TStringList.Create;
          try
            lErrorCols.Clear;
            FErrorMessage := '';
            if (AReservoir.Validate(FErrorMessage, 'MonthlyZoneElevation')) then
            begin
              for lCol := lElevation.ArrayLow to lElevation.ArrayHigh do
              begin
                for lCount := 0 to AReservoir.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount-1 do
                  ReservoirZoneElevationsDialog.GrdZone.ValidationError[lCol, lCount+1, gveColContext] := ''
              end;
            end
            else
            for lCount := 0 to AReservoir.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount-1 do
            begin
              ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
                for lCol := lElevation.ArrayLow to lElevation.ArrayHigh do
                begin
                  lIndex := lErrorCols.IndexOf(IntToStr(lCol));
                  if (lIndex >= 0) then
                    ReservoirZoneElevationsDialog.GrdZone.ValidationError[lCol, lCount+1,  gveColContext] := lErrorMsgs.Strings[lIndex]
                  else
                    ReservoirZoneElevationsDialog.GrdZone.ValidationError[lCol, lCount+1,  gveColContext] := ''
                end;
            end;
               FAllErrorMessages.AddStrings(lErrorMsgs);
          finally
            FreeAndNil(lErrorCols);
            FreeAndNil(lErrorMsgs);
          end;
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirZoneElevationsValidator.ValidateFullStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirZoneElevationsValidator.ValidateFullStorageLevel';
begin
  try
    with ReservoirZoneElevationsDialog do
    begin
      FErrorMessage := '';
      EdtFullSupplyLevel.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.Validate(FErrorMessage,'FullStorageLevel')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtFullSupplyLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.ValidateDeadStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirZoneElevationsValidator.ValidateDeadStorageLevel';
begin
  try
    with ReservoirZoneElevationsDialog do
    begin
      FErrorMessage := '';
      EdtDeadStorageLevel.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.Validate(FErrorMessage,'DeadStorageLevel')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtDeadStorageLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.ValidateBottomStorageLevel(AReservoir: IReservoirData);
const OPNAME = 'TReservoirZoneElevationsValidator.ValidateBottomStorageLevel';
begin
  try
    with ReservoirZoneElevationsDialog do
    begin
      FErrorMessage := '';
      EdtBottomOfReservoirLevel.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.Validate(FErrorMessage,'BottomOfReservoir')) then
        FAllErrorMessages.Add(FErrorMessage);
      EdtBottomOfReservoirLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationsValidator.CanDrawPenaltyLabel(ASender: TObject): boolean;
const OPNAME = 'TReservoirZoneElevationsValidator.CanDrawPenaltyLabel';
var
  LCount,
  LIndex           : integer;
  LCurrentSeries,
  LNextSeries      : TCustomSeries;
begin
  Result := False;
  try
    if Assigned(ASender) then
    begin
      LIndex := -1;
      for LCount := 0 to ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 1 do
      begin
        LIndex := LIndex + 1;
        if ReservoirZoneElevationsDialog.ChtElevation.Series[LCount] = ASender then
          Break;
      end;

      if (LIndex = 0) or
         (LIndex = (ReservoirZoneElevationsDialog.ChtElevation.SeriesList.Count - 1)) then
      begin
        Result := True;
      end
      else
      begin
        LCurrentSeries := TAreaSeries(ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex]);
        LNextSeries    := TAreaSeries(ReservoirZoneElevationsDialog.ChtElevation.Series[LIndex + 1]);
        if (LCurrentSeries.YValues.Value[0] > LNextSeries.YValues.Value[0]) then
          Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationsValidator.OnAfterPasteRowsData(Sender: TObject);
const OPNAME = 'TReservoirZoneElevationsValidator.OnAfterPasteRowsData';
var
  LReservoirData     : IReservoirData;
  LDrawDownElevation : IDrawDownElevation;
  LElevationValue    : double;
  LLevelCount,
  LCol               : integer;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if(LReservoirData <> nil) then
    begin
      for LLevelCount := 0 to LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1 do
      begin
        LDrawDownElevation := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LLevelCount];
        if(LDrawDownElevation <> nil) then
        begin
          for LCol := ReservoirZoneElevationsDialog.GrdZone.FixedCols to ReservoirZoneElevationsDialog.GrdZone.ColCount - 1 do
          begin
            LElevationValue := StrToFloat(ReservoirZoneElevationsDialog.GrdZone.Cells[LCol,LLevelCount + 1]);
            LDrawDownElevation.MonthlyElevationByIndex[LCol] := LElevationValue;
          end;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtReservoirZoneElevationAll);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirZoneElevationsValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TReservoirZoneElevationsValidator.OnAfterPasteColumnData';
var
  LReservoirData     : IReservoirData;
  LDrawDownElevation : IDrawDownElevation;
  LValue             : double;
  LLevelCount,
  LCol               : integer;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if(LReservoirData <> nil) then
    begin
      if(Sender = ReservoirZoneElevationsDialog.GrdZone) then
      begin
        LCol := ReservoirZoneElevationsDialog.GrdZone.Col;
        for LLevelCount := 0 to LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1 do
        begin
          LDrawDownElevation := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LLevelCount];
          if(LDrawDownElevation <> nil) then
          begin
            LValue := StrToFloat(ReservoirZoneElevationsDialog.GrdZone.Cells[LCol,LLevelCount + 1]);
            LDrawDownElevation.MonthlyElevationByIndex[LCol] := LValue;
          end;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtReservoirZoneElevationAll);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.



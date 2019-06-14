unit URainfallCatchmentSummaryValidator;

interface

uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,
  VCL.Graphics,

  UDataComponent,
  URainfallCatchmentSummaryDialog,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  URainfallCatchmentSummaryMenuItemManager,
  UCatchmentZone,
  UDataObjectRainfall,
  URainfallExportRANFilesForm,
  UGenericModelLinkClasses;

type

  TRainfallCatchmentSummaryValidator = class (TAbstractDataDialogValidator)

  protected
    FMenuItemManager          : TRainfallCatchmentSummaryMenuItemManager;
    FReplaceSelection         : boolean;
    FShowTopGrid              : boolean;
    FShowMiddleGrid           : boolean;
    FShowBottomGrid           : boolean;
    FSystemFlag               : boolean;
    FSelectedCatchment        : string;
    procedure DisplayTopGrid;
    procedure DisplayMiddleGrid;
    procedure DisplayBottomGrid;
    procedure DisplayAllGrids(AGridsFlag : boolean);
    procedure DoCatchmentListboxClick(Sender : TObject);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RainfallCatchmentSummaryDialog : TRainfallCatchmentSummaryDialog;
    procedure PopulateCatchmentFileList;
    function GetSelectedCatchment : TCatchmentZone;

  public
    function Initialise: boolean; override;
    procedure DoToggleTopGrid(ASender    : TObject);
    procedure DoToggleMiddleGrid(ASender : TObject);
    procedure DoToggleBottomGrid(ASender : TObject);
    procedure DoExportData(ASender       : TObject);
    procedure DoExport(AFileName: string = ''); override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext   : TChangeContext;AFieldName : string;AOldValue  : string;
                                  ANewValue : string): Boolean; override;
    function StudyHasChanged   : boolean; override;
    procedure PopulateDataViewer; override;
    property SelectedCatchmentZone  : TCatchmentZone read GetSelectedCatchment;
    property MenuItemManager    : TRainfallCatchmentSummaryMenuItemManager read FMenuItemManager
                                 write FMenuItemManager;
    procedure ClearDataViewer; override;

  end;
implementation
uses
  SysUtils,
  VCL.ImgList,
  VCL.Printers,
  UConstants,
  UDatasetType,
  UErrorHandlingOperations,
  DateUtils, VCL.Grids;

{ TRainfallCatchmentSummaryValidator }

procedure TRainfallCatchmentSummaryValidator.CreateMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TRainfallCatchmentSummaryDialog.Create(nil, FAppModules);
    RainfallCatchmentSummaryDialog.CatchmentListbox.OnClick := DoCatchmentListboxClick;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallCatchmentSummaryValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DisplayBottomGrid;
const OPNAME = 'TRainfallCatchmentSummaryValidator.DisplayBottomGrid';
begin
  try
    FSystemFlag := TRUE;
    if(FShowBottomGrid) then
      DisplayAllGrids(FShowBottomGrid)
    else
    begin
      RainfallCatchmentSummaryDialog.PanelTopGrid.Visible    := FShowBottomGrid;
      RainfallCatchmentSummaryDialog.PanelMiddleGrid.Visible := FShowBottomGrid;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Visible := Not FShowBottomGrid;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Align   := alClient;
      FAppModules.SetMenuItem(CToggleStationUsedGrid,        msUnChecked);
      FAppModules.SetMenuItem(CToggleCatchmentInputGrid,     msUnChecked);
      FAppModules.SetMenuItem(CToggleRunGrid,                msChecked);
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallCatchmentSummaryValidator.DisplayMiddleGrid;
const OPNAME = 'TRainfallCatchmentSummaryValidator.DisplayMiddleGrid';
begin
  try
    FSystemFlag := TRUE;
    if(FShowMiddleGrid) then
      DisplayAllGrids(FShowMiddleGrid)
    else
    begin
      RainfallCatchmentSummaryDialog.PanelTopGrid.Visible    := FShowMiddleGrid;
      RainfallCatchmentSummaryDialog.PanelMiddleGrid.Visible := Not FShowMiddleGrid;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Visible := FShowMiddleGrid;
      FAppModules.SetMenuItem(CToggleStationUsedGrid,        msUnChecked);
      FAppModules.SetMenuItem(CToggleCatchmentInputGrid,     msChecked);
      FAppModules.SetMenuItem(CToggleRunGrid,                msUnChecked);
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError( E, OPNAME) end;
end;

procedure TRainfallCatchmentSummaryValidator.DisplayTopGrid;
const OPNAME = 'TRainfallCatchmentSummaryValidator.DisplayTopGrid';
begin
  try
    FSystemFlag := TRUE;
    if(FShowTopGrid) then
      DisplayAllGrids(FShowTopGrid)
    else
    begin
      RainfallCatchmentSummaryDialog.PanelTopGrid.Visible    := Not FShowTopGrid;
      RainfallCatchmentSummaryDialog.PanelMiddleGrid.Visible := FShowTopGrid;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Visible := FShowTopGrid;
      RainfallCatchmentSummaryDialog.PanelTopGrid.Align      := alClient;
      FAppModules.SetMenuItem(CToggleStationUsedGrid,        msChecked);
      FAppModules.SetMenuItem(CToggleCatchmentInputGrid,     msUnChecked);
      FAppModules.SetMenuItem(CToggleRunGrid,                msUnChecked);
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallCatchmentSummaryValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallCatchmentSummaryValidator.DoExport';
begin
  inherited DoExport;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DoToggleBottomGrid(ASender: TObject);
const OPNAME = 'TRainfallCatchmentSummaryValidator.DoToggleBottomGrid';
begin
  try
    FShowBottomGrid := NOT FShowBottomGrid;
    DisplayBottomGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DoToggleTopGrid(ASender: TObject);
const OPNAME = 'TRainfallCatchmentSummaryValidator.DoToggleTopGrid';
begin
  try
    FShowTopGrid := NOT FShowTopGrid;
    DisplayTopGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DoToggleMiddleGrid(ASender: TObject);
const OPNAME = 'TRainfallCatchmentSummaryValidator.DoToggleMiddleGrid';
begin
  try
    FShowMiddleGrid := NOT FShowMiddleGrid;
    DisplayMiddleGrid;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryValidator.Initialise: boolean;
const OPNAME = 'TRainfallCatchmentSummaryValidator.Initialise';
begin
  Result := False;
  try
    FSystemFlag      := False;
    FShowTopGrid     := True;
    FShowMiddleGrid  := True;
    FShowBottomGrid  := True;
    Result := RainfallCatchmentSummaryDialog.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryValidator.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallCatchmentSummaryValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryValidator.RainfallCatchmentSummaryDialog: TRainfallCatchmentSummaryDialog;
const OPNAME = 'TRainfallCatchmentSummaryValidator.RainfallCatchmentSummaryDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallCatchmentSummaryDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallCatchmentSummaryValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                                ANewValue: string): Boolean;
const OPNAME = 'TRainfallCatchmentSummaryValidator.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallCatchmentSummaryValidator.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.DoExportData(ASender: TObject);
const OPNAME = 'TRainfallCatchmentSummaryValidator.DoExportData';
var
  lForm : TRainfallExportRANFilesForm;
begin
  try
    lForm := TRainfallExportRANFilesForm.CreateWithoutDFM(nil, FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      with RainfallCatchmentSummaryDialog do
      begin
        lForm.SetData(SelectedCatchmentZone);
        lForm.ShowModal;
      end;
    finally
      FreeAndNil(lForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallCatchmentSummaryValidator.DisplayAllGrids(AGridsFlag: boolean);
const OPNAME = 'TRainfallCatchmentSummaryValidator.DisplayAllGrids';
begin
  try
    if(AGridsFlag) then
    begin
      RainfallCatchmentSummaryDialog.PanelTopGrid.Visible    := AGridsFlag;
      RainfallCatchmentSummaryDialog.PanelMiddleGrid.Visible := AGridsFlag;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Visible := AGridsFlag;
      RainfallCatchmentSummaryDialog.PanelTopGrid.Align      := alTop;
      RainfallCatchmentSummaryDialog.PanelTopGrid.Height     := 185;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Align   := alBottom;
      RainfallCatchmentSummaryDialog.PanelBottomGrid.Height  := 185;
      RainfallCatchmentSummaryDialog.PanelMiddleGrid.Align   := alClient;
      FAppModules.SetMenuItem(CToggleStationUsedGrid, msChecked);
      FAppModules.SetMenuItem(CToggleCatchmentInputGrid, msChecked);
      FAppModules.SetMenuItem(CToggleRunGrid, msChecked)
    end
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.PopulateDataViewer;
const OPNAME = 'TRainfallCatchmentSummaryValidator.PopulateDataViewer';
begin
  try
    inherited PopulateDataViewer;
    FSelectedCatchment := FAppModules.ViewIni.ReadString('TRainfallZoneValidator','SelectedCatchment','');
    PopulateCatchmentFileList;
    DoCatchmentListboxClick(nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallCatchmentSummaryValidator.PopulateCatchmentFileList;
const OPNAME = 'TRainfallCatchmentSummaryValidator.PopulateCatchmentFileList';
var
  LCatchmentZone : TCatchmentZone;
  LRainfallObj : TDataObjectRainfall;
  LIndex : integer;
begin
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    for LIndex := 0 to RainfallCatchmentSummaryDialog.CatchmentListbox.Count -1 do
      RainfallCatchmentSummaryDialog.CatchmentListbox.Items.Delete(LIndex);
    RainfallCatchmentSummaryDialog.CatchmentListbox.Clear;
    for LIndex := 0 to LRainfallObj.GetCatchmentZoneListCount -1 do
    begin
      LCatchmentZone := LRainfallObj.GetCatchmentZoneByIndex(LIndex);
      if LCatchmentZone <> nil then
        RainfallCatchmentSummaryDialog.CatchmentListbox.Items.AddObject(LCatchmentZone.CatchmentFileName,LCatchmentZone);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallCatchmentSummaryValidator.DoCatchmentListboxClick(Sender: TObject);
const OPNAME ='TRainfallCatchmentSummaryValidator.DoCatchmentListboxClick';
var
  LCatchmentZone                 : TCatchmentZone;
  LCatchmentOutputFileData       : TCatchmentOutputFileData;
  LDetailOfRainfallStationsUsed  : TDetailOfRainfallStationsUsed;
  LRainfallAsPercentMAP          : TRainfallAsPercentMAP;
  LIndex                         : integer;
  LMonth                         : integer;
begin
  try
    ClearDataViewer;
    LCatchmentZone := GetSelectedCatchment;
    if LCatchmentZone <> nil then
    begin
      LCatchmentOutputFileData := LCatchmentZone.CatchmentOutputFileData;
      if LCatchmentOutputFileData <> nil then
      begin
        if LCatchmentOutputFileData.DetailOfRainfallStationsUsedCount > 0 then
        begin
          RainfallCatchmentSummaryDialog.AvrgRainfallGrd.RowCount :=
          LCatchmentOutputFileData.DetailOfRainfallStationsUsedCount + 1;
          RainfallCatchmentSummaryDialog.RainfallInput.RowCount :=
          4+LCatchmentOutputFileData.RainfallAsPercentMAPCount;
          RainfallCatchmentSummaryDialog.Catchment.RowCount :=
          1+LCatchmentOutputFileData.RainfallAsPercentMAPCount;
        end
        else
        begin
          RainfallCatchmentSummaryDialog.AvrgRainfallGrd.RowCount := 2;
          RainfallCatchmentSummaryDialog.RainfallInput.RowCount   := 2;
          RainfallCatchmentSummaryDialog.Catchment.RowCount       := 2;
        end;
        for LIndex := 0 to LCatchmentOutputFileData.DetailOfRainfallStationsUsedCount -1 do
        begin
          LDetailOfRainfallStationsUsed := LCatchmentOutputFileData.DetailOfRainfallStationsUsedByIndex[LIndex];
          if LDetailOfRainfallStationsUsed <> nil then
          begin
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[0,LIndex+1] := LDetailOfRainfallStationsUsed.Section;
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[1,LIndex+1] := LDetailOfRainfallStationsUsed.Position;
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[2,LIndex+1] := IntToStr(LDetailOfRainfallStationsUsed.MAPInmm);
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[3,LIndex+1] := LDetailOfRainfallStationsUsed.PeriodOfRecord;
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[4,LIndex+1] := LDetailOfRainfallStationsUsed.Latitude;
            RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[5,LIndex+1] := LDetailOfRainfallStationsUsed.Longitude;
          end;
        end;
        if LCatchmentOutputFileData.RainfallAsPercentMAPCount > 0 then
        begin
          for Lindex := 0 to LCatchmentOutputFileData.RainfallAsPercentMAPCount -1 do
          begin
            LRainfallAsPercentMAP := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[Lindex];
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[0,Lindex+1] := IntToStr(LRainfallAsPercentMAP.HydroYear);
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[1,Lindex+1] := IntToStr(LRainfallAsPercentMAP.NoOfGaugesUsed);
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[RainfallCatchmentSummaryDialog.RainfallInput.ColCount-1,0] := 'YEAR';
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[RainfallCatchmentSummaryDialog.RainfallInput.ColCount-1,
            Lindex+1] := FormatFloat('##0.00',LRainfallAsPercentMAP.HydroYearTotal);
            RainfallCatchmentSummaryDialog.Catchment.Cells[0,LIndex+1]     := IntToStr(LRainfallAsPercentMAP.HydroYear);
            for LMonth := MinMonths to MaxMonths do
            begin
              RainfallCatchmentSummaryDialog.RainfallInput.Cells[LMonth+1,LIndex+1] := FloatToStr(LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]);
              RainfallCatchmentSummaryDialog.Catchment.Cells[LMonth,LIndex+1] := FloatToStr(LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]);
            end;
          end;
          for LMonth := MinMonths to MaxMonths do
          begin
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[0,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-3] := 'AVERAGE';
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[LMonth+1,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-3] := FormatFloat('##0.00',LCatchmentOutputFileData.AveragePercentageOfMAPByMonth[LMonth]);
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[0,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-2] := 'AJUSTED';
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[LMonth+1,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-2] := FormatFloat('##0.00',LCatchmentOutputFileData.AjustedPercentageOfMAPByMonth[LMonth]);
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[0,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-1] := 'STD.DEV';
            RainfallCatchmentSummaryDialog.RainfallInput.Cells[LMonth+1,
            RainfallCatchmentSummaryDialog.RainfallInput.RowCount-1] := FormatFloat('##0.00',LCatchmentOutputFileData.StdDevByMonth[LMonth]);
          end;
          RainfallCatchmentSummaryDialog.RainfallInput.Cells[RainfallCatchmentSummaryDialog.RainfallInput.ColCount-1,
                                                             RainfallCatchmentSummaryDialog.RainfallInput.RowCount-3] :=
                                                             FormatFloat('##0.00',LCatchmentOutputFileData.GrandAverage);
          RainfallCatchmentSummaryDialog.RainfallInput.Cells[RainfallCatchmentSummaryDialog.RainfallInput.ColCount-1,
                                                             RainfallCatchmentSummaryDialog.RainfallInput.RowCount-2] :=
                                                             FormatFloat('##0.00',LCatchmentOutputFileData.GrandAjusted);
          RainfallCatchmentSummaryDialog.RainfallInput.Cells[RainfallCatchmentSummaryDialog.RainfallInput.ColCount-1,
                                                             RainfallCatchmentSummaryDialog.RainfallInput.RowCount-1] :=
                                                             FormatFloat('##0.00',LCatchmentOutputFileData.GrandSTDDEV);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallCatchmentSummaryValidator.GetSelectedCatchment : TCatchmentZone;
const OPNAME = 'TRainfallCatchmentSummaryValidator.GetSelectedCatchment';
var
  LIndex : integer;
  LRainfallObj : TDataObjectRainfall;
begin
  Result := nil;
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    for LIndex := 0 to RainfallCatchmentSummaryDialog.CatchmentListbox.Items.Count-1 do
    begin
      if RainfallCatchmentSummaryDialog.CatchmentListbox.Selected[LIndex] then
      begin
        Result :=  LRainfallObj.GetCatchmentZoneByIndex(LIndex);
        FAppModules.ViewIni.WriteString('TRainfallZoneValidator','SelectedCatchment',Result.CatchmentFileName);
        Break;
      end;
    end;
    if (Result = nil) and (Trim(FSelectedCatchment) <> '') then
    begin
      LIndex := RainfallCatchmentSummaryDialog.CatchmentListbox.Items.IndexOf(FSelectedCatchment);
      if (LIndex >= 0) then
      begin
        Result :=  LRainfallObj.GetCatchmentZoneByName(FSelectedCatchment);
        RainfallCatchmentSummaryDialog.CatchmentListbox.Selected[LIndex] := True;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRainfallCatchmentSummaryValidator.ClearDataViewer;
const OPNAME = 'TRainfallCatchmentSummaryValidator.ClearDataViewer';
var
  LCol, LRow : integer;
begin
  try
    inherited ClearDataViewer;
    for LCol := 0 to RainfallCatchmentSummaryDialog.AvrgRainfallGrd.ColCount -1 do
    begin
      for LRow := 1 to RainfallCatchmentSummaryDialog.AvrgRainfallGrd.RowCount-1 do
        RainfallCatchmentSummaryDialog.AvrgRainfallGrd.Cells[LCol,LRow] := '';
    end;
    for LCol := 0 to RainfallCatchmentSummaryDialog.Catchment.ColCount -1 do
    begin
      for LRow := 1 to RainfallCatchmentSummaryDialog.Catchment.RowCount-1 do
        RainfallCatchmentSummaryDialog.Catchment.Cells[LCol,LRow] := '';
    end;
    for LCol := 0 to RainfallCatchmentSummaryDialog.RainfallInput.ColCount -1 do
    begin
      for LRow := 1 to RainfallCatchmentSummaryDialog.RainfallInput.RowCount-1 do
        RainfallCatchmentSummaryDialog.RainfallInput.Cells[LCol,LRow] := '';
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

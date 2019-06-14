//
//
//  UNIT      : Contains TGridOutputEditor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridOutputEditorGrid;

interface

uses
  Classes,
  VCL.ComCtrls,
  UAbstractGridData,
  UGridEditorRowGrid,
  UGridEditorColumnGrid,
  UGridEditorStringGrid,
  UGridEditorGridInterface,
  UAbstractComponent;

type
  TGridOutputEditorEditState = (esBrowse, esAdd, esDelete, esEdit);
  TGridOutputEditorViewMode = (vmBlank, vmRowGrid, vmColumnGrid, vmCustomGrid);
  TGridOutputEditor = class(TAbstractControl, IGridEditorGrid)
  protected
    FViewMode: TGridOutputEditorViewMode;
    FLastNormalViewMode: TGridOutputEditorViewMode;
    FEditState: TGridOutputEditorEditState;
    FGridData: TAbstractGridData;
    FRowGrid: TGridEditorRowGrid;
    FColumnGrid: TGridEditorColumnGrid;
    FCustomGrid: IGridEditorGrid;
    FFieldDescription: TStatusBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetViewMode(AViewMode: TGridOutputEditorViewMode);
    procedure SetGridData(AGridData: TAbstractGridData);
    function GetCurrentGrid: IGridEditorGrid;
    procedure SetCurrentGrid(ACustomGrid: IGridEditorGrid);
    procedure OnCellHasChanged(ASender: TObject; ACol, ARow: integer);
  public

    // Overriden from TAbstractControl.
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

    // Introduced in this class.
    procedure UpdateHintText;
    procedure ClearDataViewer;
    procedure DoCopyToClipboard;
    procedure DoExport(AFileName: string = '');
    procedure DoPrint(ATitle: string);
    procedure DoRotateView(Sender: TObject);
    procedure PopulateGridMonthsNames(AMonthNamesList : TStrings);
    procedure PopulateAverageRow;
    // Properties.
    property ViewMode: TGridOutputEditorViewMode read FViewMode write SetViewMode;
    property LastNormalViewMode: TGridOutputEditorViewMode read FLastNormalViewMode;
    property EditState: TGridOutputEditorEditState read FEditState write FEditState;
    property GridData: TAbstractGridData read FGridData write SetGridData;
    property Grid: IGridEditorGrid read GetCurrentGrid write SetCurrentGrid implements IGridEditorGrid;
  end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.Clipbrd,
  UAbstractObject,
  UErrorHandlingOperations;

procedure TGridOutputEditor.CreateMemberObjects;
const OPNAME = 'TGridOutputEditor.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    // Set ancestor properties.
    Hint      := FAppModules.Language.GetString('TabCaption.GridEditor');
    ShowHint  := False;
    ParentShowHint := False;
    FGridData := nil;

    // Set defaults.
    FViewMode := vmRowGrid;
    FLastNormalViewMode := vmRowGrid;
    FEditState := esBrowse;

    // Create the statust bar.
    FFieldDescription := TStatusBar.Create(self);
    FFieldDescription.Parent := self;
    FFieldDescription.SimplePanel := True;
    FFieldDescription.Align := alBottom;
    FFieldDescription.Visible := True;

    // Create the DB grid.
    FRowGrid := TGridEditorRowGrid.Create(self, FAppModules);
    FRowGrid.Parent  := self;
    FRowGrid.Align   := alClient;
    FRowGrid.Visible := False;
    FRowGrid.OnAfterCellChange := OnCellHasChanged;

    // Create the string grid.
    FColumnGrid := TGridEditorColumnGrid.Create(self, FAppModules);
    FColumnGrid.Parent  := self;
    FColumnGrid.Align   := alClient;
    FColumnGrid.Visible := False;
    FColumnGrid.OnAfterCellChange := OnCellHasChanged;

  // Handle exception.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridOutputEditor.DestroyMemberObjects;
const OPNAME = 'TGridOutputEditor.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGridData);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TGridOutputEditor.GetCurrentGrid: IGridEditorGrid;
const OPNAME = 'TGridOutputEditor.GetCurrentGrid';
begin
  Result := nil;
  try
    case FViewMode of
      vmBlank      : ; // Do nothing.
      vmRowGrid    : Result := FRowGrid;
      vmColumnGrid : Result := FColumnGrid;
      vmCustomGrid : Result := FCustomGrid;
    else
      raise Exception.CreateFmt('Unknown grid view mode [%d].', [integer(FViewMode)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.SetCurrentGrid(ACustomGrid: IGridEditorGrid);
const OPNAME = 'TGridOutputEditor.SetCurrentGrid';
begin
  try
    FCustomGrid := ACustomGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditor.LanguageHasChanged: boolean;
const OPNAME = 'TGridOutputEditor.LanguageHasChanged';
begin
  Result := False;
  try
    inherited LanguageHasChanged;
    if Assigned(FGridData) then
      FGridData.LanguageHasChanged;
    FRowGrid.LanguageHasChanged;
    FColumnGrid.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditor.SaveState: boolean;
const OPNAME = 'TGridOutputEditor.SaveState';
begin
  Result := False;
  try
    inherited SaveState;
    case FViewMode of
      vmRowGrid    : FRowGrid.SaveState;
      vmColumnGrid : FColumnGrid.SaveState;
      vmCustomGrid : FCustomGrid.SaveState;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.SetViewMode(AViewMode: TGridOutputEditorViewMode);
const OPNAME = 'TGridOutputEditor.SetViewMode';
begin
  try
    FViewMode := AViewMode;
    if (FViewMode in [vmRowGrid, vmColumnGrid]) then
      FLastNormalViewMode := FViewMode;
    {if Assigned(Grid) then
    begin
      FFieldDescription.SimpleText := Grid.CurrentFieldHintText;
      FFieldDescription.Visible := GetCurrentGrid.IsVisible;
    end
    else
      FFieldDescription.Visible := False;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.SetGridData(AGridData: TAbstractGridData);
const OPNAME = 'TGridOutputEditor.SetGridData';
begin
  try
    FreeAndNil(FGridData);
    FGridData := AGridData;
    FRowGrid.GridData := AGridData;
    FColumnGrid.GridData := AGridData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.ClearDataViewer;
const OPNAME = 'TGridOutputEditor.ClearDataViewer';
begin
  try
    SetGridData(nil);
    FRowGrid.ClearDataViewer;
    FColumnGrid.ClearDataViewer;
    if Assigned(FCustomGrid) then
      FCustomGrid.ClearDataViewer;
    FFieldDescription.SimpleText := '';
    //FFieldDescription.Visible := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditor.DoCopyToClipboard;
const OPNAME = 'TGridOutputEditor.DoCopyToClipboard';
var LGridData: TStringList;
begin
  try
    LGridData := TStringList.Create;
    try
      GetCurrentGrid.CopyGridDataInto(LGridData);
      ClipBoard.AsText := LGridData.Text;
    finally
      LGridData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.DoExport(AFileName: string = '');
const OPNAME = 'TGridOutputEditor.DoExport';
var
  LGridData: TStringList;
  LExportFilename: string;
begin
  try
    LExportFilename := AFileName;
    if FAppModules.GetExportFilename('.csv',
      'Comma Separated Files (*.csv)|*.csv|All Files (*.*)|*.*', LExportFilename) then
    begin
      LGridData := TStringList.Create;
      try
        GetCurrentGrid.CopyGridDataInto(LGridData);
        LGridData.SaveToFile(LExportFilename);
      finally
        FreeAndNil(LGridData);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditor.DoPrint(ATitle: string);
const OPNAME = 'TGridOutputEditor.DoPrint';
begin
  try
    case FViewMode of
      vmRowGrid    : FRowGrid.DoPrint(ATitle);
      vmColumnGrid : FColumnGrid.DoPrint(ATitle);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditor.DoRotateView(Sender: TObject);
const OPNAME = 'TGridOutputEditor.DoRotateView';
begin
  try
    SaveState;
    case FViewMode of
      vmBlank : ; // Do nothing.
      vmRowGrid :
        begin
          FLastNormalViewMode := vmColumnGrid;
          FViewMode := vmColumnGrid;
          FRowGrid.Visible := False;
          FRowGrid.ClearDataViewer;
          FColumnGrid.PopulateDataViewer;
          FColumnGrid.Visible := True;
        end;
      vmColumnGrid :
        begin
          FLastNormalViewMode := vmRowGrid;
          FViewMode := vmRowGrid;
          FColumnGrid.Visible := False;
          FColumnGrid.ClearDataViewer;
          FRowGrid.PopulateDataViewer;
          FRowGrid.Visible := True;
        end;
    else
      raise Exception.CreateFmt('Unknown grid view mode [%d].', [integer(FViewMode)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.UpdateHintText;
const OPNAME = 'TGridOutputEditor.UpdateHintText';
begin
  try
    FRowGrid.UpdateHintText;
    FColumnGrid.UpdateHintText;
    if Assigned(FCustomGrid) then
      FCustomGrid.UpdateHintText;
    if Assigned(Grid) then
    begin
      FFieldDescription.SimpleText := Grid.CurrentFieldHintText;
      FFieldDescription.Visible := Grid.IsVisible;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.OnCellHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TGridOutputEditor.OnCellHasChanged';
begin
  try
    UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.PopulateGridMonthsNames(AMonthNamesList: TStrings);
const OPNAME = 'TGridOutputEditor.PopulateGridMonthsNames';
var
  LIndex: integer;
begin
  try
    if(AMonthNamesList <> nil) and (AMonthNamesList.Count = 12) then
    begin
      if (FViewMode = vmRowGrid) then
      begin
        if FRowGrid.IsVisible  and (FRowGrid.ColCount = 15) and (FRowGrid.RowCount >= 1)then
        begin
          for LIndex := 0 to 11 do
            FRowGrid.Cells[LIndex+2,0] := AMonthNamesList[LIndex];
          FRowGrid.Cells[1,FRowGrid.RowCount-1] := FAppModules.Language.GetString('OutputGrid.Average');
        end;
      end
      else
      if (FViewMode = vmColumnGrid) then
      begin
        if FColumnGrid.IsVisible  and (FColumnGrid.RowCount = 15) and (FColumnGrid.ColCount >= 1)then
        begin
          for LIndex := 0 to 11 do
            FColumnGrid.Cells[0,LIndex+2] := AMonthNamesList[LIndex];
          FColumnGrid.Cells[FColumnGrid.ColCount-1,1] := FAppModules.Language.GetString('OutputGrid.Average');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditor.PopulateAverageRow;
const OPNAME = 'TGridOutputEditor.PopulateAverageRow';
var
  LCol,
  LRow: integer;
  LTotal: double;
begin
  try
    if (FViewMode = vmRowGrid) then
    begin
      if(FRowGrid.RowCount > 2) then
      begin
        FRowGrid.RowCount := FRowGrid.RowCount + 1;
        for LCol := 2 to FRowGrid.ColCount -2 do
        begin
          LTotal := 0.0;
          for LRow := 1 to FRowGrid.RowCount -2 do
          begin
            LTotal := LTotal + StrToFloatDef(FRowGrid.Cells[LCol,LRow],0.0);
          end;
          FRowGrid.Cells[LCol,FRowGrid.RowCount-1] := Trim(FormatFloat('#### ##0.000',LTotal/(FRowGrid.RowCount-2)));
        end;
        LTotal := 0.0;
        for LRow := 1 to FRowGrid.RowCount -2 do
        begin
          LTotal := LTotal + StrToFloatDef(FRowGrid.Cells[FRowGrid.ColCount-1,LRow],0.0);
        end;
        FRowGrid.Cells[FRowGrid.ColCount-1,FRowGrid.RowCount-1] := Trim(FormatFloat('#### ##0.000',LTotal/(FRowGrid.RowCount-2)));
      end;
    end
    else
    if (FViewMode = vmColumnGrid) then
    begin
      if(FColumnGrid.RowCount > 2) then
      begin
        FColumnGrid.ColCount := FColumnGrid.ColCount + 1;
        for LRow := 2 to FColumnGrid.RowCount -2 do
        begin
          LTotal := 0.0;
          for LCol := 1 to FColumnGrid.ColCount -2 do
          begin
            LTotal := LTotal + StrToFloatDef(FColumnGrid.Cells[LCol,LRow],0.0);
          end;
          FColumnGrid.Cells[FColumnGrid.ColCount-1,LRow] := Trim(FormatFloat('#### ##0.000',LTotal/(FColumnGrid.ColCount-2)));
        end;
        LTotal := 0.0;
        for LCol := 1 to FColumnGrid.ColCount -2 do
        begin
          LTotal := LTotal + StrToFloatDef(FColumnGrid.Cells[LCol,FColumnGrid.RowCount-1],0.0);
        end;
        FColumnGrid.Cells[FColumnGrid.ColCount-1,FColumnGrid.RowCount-1] := Trim(FormatFloat('#### ##0.000',LTotal/(FColumnGrid.ColCount-2)));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


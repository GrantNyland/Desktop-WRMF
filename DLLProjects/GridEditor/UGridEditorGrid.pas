//
//
//  UNIT      : Contains TGridEditorGrid Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/06/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorGrid;

interface

uses
  Classes,
  vcl.ComCtrls,
  UAbstractGridData,
  UGridEditorRowGrid,
  UGridEditorColumnGrid,
  UGridEditorStringGrid,
  UGridEditorGridInterface,
  UAbstractComponent;

type
  TGridEditorGridEditState = (esBrowse, esAdd, esDelete, esEdit);
  TGridEditorGridViewMode = (vmBlank, vmRowGrid, vmColumnGrid, vmCustomGrid);
  TGridEditorGrid = class(TAbstractControl, IGridEditorGrid)
  protected
    FViewMode: TGridEditorGridViewMode;
    FLastNormalViewMode: TGridEditorGridViewMode;
    FEditState: TGridEditorGridEditState;
    FGridData: TAbstractGridData;
    FRowGrid: TGridEditorRowGrid;
    FColumnGrid: TGridEditorColumnGrid;
    FCustomGrid: IGridEditorGrid;
    FFieldDescription: TStatusBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetViewMode(AViewMode: TGridEditorGridViewMode);
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

    // Properties.
    property ViewMode: TGridEditorGridViewMode read FViewMode write SetViewMode;
    property LastNormalViewMode: TGridEditorGridViewMode read FLastNormalViewMode;
    property EditState: TGridEditorGridEditState read FEditState write FEditState;
    property GridData: TAbstractGridData read FGridData write SetGridData;
    property Grid: IGridEditorGrid read GetCurrentGrid write SetCurrentGrid implements IGridEditorGrid;
  end;

implementation

uses
  SysUtils,
  vcl.Controls,
  vcl.Clipbrd,
  UAbstractObject,
  UErrorHandlingOperations;

procedure TGridEditorGrid.CreateMemberObjects;
const OPNAME = 'TGridEditorGrid.CreateMemberObjects';
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

procedure TGridEditorGrid.DestroyMemberObjects;
const OPNAME = 'TGridEditorGrid.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGridData);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TGridEditorGrid.GetCurrentGrid: IGridEditorGrid;
const OPNAME = 'TGridEditorGrid.GetCurrentGrid';
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

procedure TGridEditorGrid.SetCurrentGrid(ACustomGrid: IGridEditorGrid);
const OPNAME = 'TGridEditorGrid.SetCurrentGrid';
begin
  try
    FCustomGrid := ACustomGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorGrid.LanguageHasChanged: boolean;
const OPNAME = 'TGridEditorGrid.LanguageHasChanged';
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

function TGridEditorGrid.SaveState: boolean;
const OPNAME = 'TGridEditorGrid.SaveState';
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

procedure TGridEditorGrid.SetViewMode(AViewMode: TGridEditorGridViewMode);
const OPNAME = 'TGridEditorGrid.SetViewMode';
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

procedure TGridEditorGrid.SetGridData(AGridData: TAbstractGridData);
const OPNAME = 'TGridEditorGrid.SetGridData';
begin
  try
    FreeAndNil(FGridData);
    FGridData := AGridData;
    FRowGrid.GridData := AGridData;
    FColumnGrid.GridData := AGridData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorGrid.ClearDataViewer;
const OPNAME = 'TGridEditorGrid.ClearDataViewer';
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

procedure TGridEditorGrid.DoCopyToClipboard;
const OPNAME = 'TGridEditorGrid.DoCopyToClipboard';
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

procedure TGridEditorGrid.DoExport(AFileName: string = '');
const OPNAME = 'TGridEditorGrid.DoExport';
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

procedure TGridEditorGrid.DoPrint(ATitle: string);
const OPNAME = 'TGridEditorGrid.DoPrint';
begin
  try
    case FViewMode of
      vmRowGrid    : FRowGrid.DoPrint(ATitle);
      vmColumnGrid : FColumnGrid.DoPrint(ATitle);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorGrid.DoRotateView(Sender: TObject);
const OPNAME = 'TGridEditorGrid.DoRotateView';
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

procedure TGridEditorGrid.UpdateHintText;
const OPNAME = 'TGridEditorGrid.UpdateHintText';
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

procedure TGridEditorGrid.OnCellHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TGridEditorGrid.OnCellHasChanged';
begin
  try
    UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


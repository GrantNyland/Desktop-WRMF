//
//
//  UNIT      : Contains  Class
//  AUTHOR    : Sam Dhlamini(Arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UModelHydrologyTabSheetManager;

interface
uses
  { Delphi }
  //Chart,
  VCL.ComCtrls,
  { Dwaf }
  UDataSheet,
  UAbstractObject,
  UAbstractComponent,
  UDataViewerManager,
  UViewDataItem,
  UGraphSheet,
  UModelHydrologyTabSheet,
  UGenericModelLinkClasses,
  UTabsheetManager;

type
  TModelHydrologyTabSheetManager = class( TTabSheetManager  ) //TAbstractTabSheet
  protected

    FGridEditorManager: TDataViewerManager;
    FViewDataGraphManager: TDataViewerManager;
    FViewDataTreeNodeData : TViewDataTreeNodeData;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
//    function GetToolBar: TAbstractToolBar; override;
    procedure PageControlPageChange(Sender: TObject);
    procedure PageControlOnShow(Sender: TObject);
    procedure OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
    procedure CreateGridEditorManager;
    procedure CreateViewDataGraphManager;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ShowGridEditor: boolean;
    function EditGridEditor(AData: TObject): boolean;
    function ShowGraph: boolean;
    class function CanModelHydrologyTabBeCreated : boolean;
    function DoGridAction(AGridActionObject: TObject): boolean; override;
//    property GridEditorManager: TDataViewerManager read FGridEditorManager;
    property GraphManager: TDataViewerManager read FViewDataGraphManager;
    function ResetState: Boolean; override;
    function DoCustomTabSheetEvent ( ACustomModelEvent: TModelMenuData ): Boolean; override;
    function DoJumpRequest ( AViewDataNode: TViewDataNode ): Boolean; override;


//    property CurrentSheet:TAbstractTabSheet read FCurrentSheet write FCurrentSheet;
//    property PageControl: TPageControl read FPageControl write FPageControl;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // arivia.kom
  UErrorHandlingOperations, UDataViewerSheet;

{ TModelHydrologyTabSheetManager }


class function TModelHydrologyTabSheetManager.CanModelHydrologyTabBeCreated: boolean;
const OPNAME = 'TModelHydrologyManager.CanModelHydrologyGraphBeCreated';
begin
  Result := False;
  try
    Result := FileExists ( ExtractFilePath ( ApplicationExeName ) + 'bin\ViewDataGraph.dll' ) or //bin\ViewHydrologyDataGraph.dll
              FileExists ( ExtractFilePath ( ApplicationExeName ) + 'bin\GridEdit.dll' );
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TModelHydrologyTabSheetManager.CreateGridEditorManager;
const OPNAME = 'TModelHydrologyTabSheetManager.CreateGridEditorManager';
begin

  try

    if not ( FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll' ) ) then
      Exit;
    { end if..then }
    { Create a dll }
    CreateDLLObject ( ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll',
                      TAbstractAppObject(FGridEditorManager), FAppModules, False, OPNAME );

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheetManager.CreateMemberObjects;
const OPNAME = 'TModelHydrologyTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TModelHydrologyTabSheet.Create ( nil, AppModules );
//    FGridEditorManager    := nil;
    FViewDataGraphManager := nil;
//    CreateGridEditorManager;
    CreateViewDataGraphManager;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheetManager.CreateViewDataGraphManager;
const OPNAME = 'TModelHydrologyTabSheetManager.CreateViewDataGraphManager';
begin

  try

    if not ( FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll' ) ) then
      Exit;
    { end if..then }
    { Create a dll }
    CreateDLLObject ( ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll',
                      TAbstractAppObject ( FViewDataGraphManager ), FAppModules, False, OPNAME );

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TModelHydrologyTabSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;

  try
    FreeAndNil ( FTabSheet );
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TModelHydrologyTabSheetManager.GetToolBar: TAbstractToolBar;
const OPNAME = 'TModelHydrologyTabSheetManager.GetToolBar';
begin
  Result := nil;
  try
    if Assigned(FCurrentSheet) then
      Result := FCurrentSheet.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
function TModelHydrologyTabSheetManager.DoCustomTabSheetEvent(
  ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
{    if (not Result) and Assigned(FGridEditorManager) then
      Result := FGridEditorManager.DoCustomTabSheetEvent(ACustomModelEvent);}
    if (not Result) and Assigned ( FViewDataGraphManager ) then
      Result := FViewDataGraphManager.DoCustomTabSheetEvent ( ACustomModelEvent );
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelHydrologyTabSheetManager.DoGridAction( AGridActionObject: TObject ): boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.DoGridAction';
begin

  Result := False;

  try

    Result := True;
    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.DoJumpRequest(
  AViewDataNode: TViewDataNode): Boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.DoJumpRequest';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.EditGridEditor ( AData: TObject ): boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.EditGridEditor';
begin

  Result := False;

  try

    if ( Assigned ( FGridEditorManager ) ) then
    begin
      FGridEditorManager.DoGridAction ( AData );
      Result := True;
    end;
    { end if..then }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.Initialise: Boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.Initialise';
begin
  Result := Inherited Initialise;
  try
    Result := ( Result and FTabSheet.Initialise );
{    if Assigned ( FGridEditorManager ) then
    begin
      FGridEditorManager.TabSheet.PageControl := TModelHydrologyTabSheet ( FTabSheet ).PageControl;
    //  FGridEditorManager.TabSheet.Caption := 'Grid';
      Result := ( Result and FGridEditorManager.Initialise );
    end;
    { end if..then }
    if Assigned ( FViewDataGraphManager ) then
    begin
      FViewDataGraphManager.TabSheet.PageControl := TModelHydrologyTabSheet ( FTabSheet ).PageControl;
    //  FViewDataGraphManager.TabSheet.Caption := 'Graph';
      Result := ( Result and FViewDataGraphManager.Initialise );

    end;
    { end if..then }




  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheetManager.LanguageHasChanged: boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    Result := Result and FTabSheet.LanguageHasChanged;
  {  if Assigned ( FGridEditorManager ) then
      Result := Result and FGridEditorManager.LanguageHasChanged;
   }   
    if Assigned ( FViewDataGraphManager ) then
      Result := Result and FViewDataGraphManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;

end;


procedure TModelHydrologyTabSheetManager.OnTabChangeRequest(
  Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TModelHydrologyTabSheetManager.OnTabChangeRequest';
begin

  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheetManager.PageControlOnShow(
  Sender: TObject);
const OPNAME = 'TModelHydrologyTabSheetManager.PageControlOnShow';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyTabSheetManager.PageControlPageChange(
  Sender: TObject);
const OPNAME = 'TModelHydrologyTabSheetManager.PageControlPageChange';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheetManager.ResetState: Boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.ResetState';
begin
  Result := False;
  try
  {
    if Assigned(FGridEditorManager) then
      FGridEditorManager.ResetState;
   }
    if Assigned(FViewDataGraphManager) then
      FViewDataGraphManager.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.ShowGraph: boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.ShowGraph';
begin

  Result := False;

  try
    if ( Assigned ( FViewDataGraphManager ) ) then
    begin
      TModelHydrologyTabSheet( FTabSheet ).PageControl.ActivePage := FViewDataGraphManager.TabSheet;
      Result := True;
    end;
    { end if..then }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.ShowGridEditor: boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.ShowGridEditor';
begin

  Result := False;

  try
{    if ( Assigned ( FGridEditorManager ) ) then
    begin
      TModelHydrologyTabSheet( FTabSheet ).PageControl.ActivePage := FGridEditorManager.TabSheet;
 }     
      Result := True;
//    end;
    { end if..then }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyTabSheetManager.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: string): boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.StudyDataHasChanged';
begin

 Result := inherited StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue );

  try

    Result := ( Result and FTabSheet.StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue ) );

{    if ( Assigned ( FGridEditorManager ) ) then
      Result := ( Result and FGridEditorManager.StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue ) );
    { end if..then }
    if ( Assigned ( FViewDataGraphManager ) ) then
      Result := ( Result and FViewDataGraphManager.StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue ) );
    { end if..then }
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TModelHydrologyTabSheetManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := Result and FTabSheet.StudyHasChanged;
{
    if Assigned(FGridEditorManager) then
      Result := Result and FGridEditorManager.StudyHasChanged;
}      
    if Assigned(FViewDataGraphManager) then
      Result := Result and FViewDataGraphManager.StudyHasChanged;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

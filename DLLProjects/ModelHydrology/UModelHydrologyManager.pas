//
//
//  UNIT      : Contains  Class
//  AUTHOR    : Sam Dhlamini(Arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UModelHydrologyManager;

interface

uses

  // Delphi VCL, RTL, etc

  Contnrs,
  Classes,
  VCL.Controls,
  // DWAF (arivia.kom)
  UFileNames,
  UAbstractObject,

  USQLDatabaseLayer,

  USystemModelManager,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UModelHydrologyMenuItem,
  UModelHydrologyTabSheetManager,
  UChannelDataLoadAgent,
  UFileselectionManager,
  UHydrologyModelFileSelectionManager,
  UFilesActionAbstractManager,
  UTreeViewTabSheet,

  UHydrologyFilesObject,
  UYieldModelDataObject,

  UFilesActionYieldManager,
  UYieldFileSelectionManager,

  UDataViewerManager,
  UDataTabSheetManager,
  UGridActionObject,
  UHydrologyFileTypeDataSetConstructor,
  UModelHydrologyDatasetConstructor;


type
  TModelHydrologyManager = class ( TSystemModelManager )
  protected
    FModelData : TYieldModelDataObject;
    FModelMenuItemManager : TModelHydrologyMenuItem;

    FDataTabSheetManager: TDataTabSheetManager;
    FFileEditManager: TAbstractFileEditManager;

    FFileSelectionManager: TFileselectionManager;
    FModelFilesActionManager: TFilesActionAbstractManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateModelMenuItemManager;
    procedure CreateFileEditManager;
    procedure CreateModelData;
    procedure CreateDataTabSheetManager;
    procedure CreateModelDatasetConstructor;
    procedure DeleteModelDatasetConstructor;
    function HydrologyModelData: TYieldModelDataObject;
    function LoadModelData: boolean;
    function SelectModelFileNamesAndDisplay: boolean;

    function ShowFileEditor: boolean; virtual;

  public
    function ModelName: string; override;
    function Initialise: boolean; override;
    function DoValidateSingleFile: boolean; virtual;
    function PopulateDataList( AModelDataChangeType: TModelDataChangeType;
                               AList: TList ): Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function IsGraphLoaded: Boolean; override;
    function IsGridLoaded: Boolean; override;
    function StudyDataHasChanged ( AContext: TChangeContext; AFieldName,
                                   AOldValue,ANewValue: string ): boolean; override;
    function ProcessCustomModelEvent ( AData: TModelMenuData ): boolean; override;
    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
    function ResetState: boolean; override;
    function ProcessEvent ( AEventType: integer; AData: TObject ): boolean; override;
    function ShowGridEditor: boolean;
    function EditGridEditor(AData: TObject): boolean;
    function ShowGraph: boolean;
    function DoExportSingleFile : boolean;
    function DoImportSingleFile: boolean;
    function DoRefreshFileHints: boolean;
    function DoRunModel : boolean;
    function RefreshModelData: boolean;
    function ModelData: TInterfacedObject; override;
    //function ValidateBusinessRule(ABusinessRule: integer; AObjectsArray: array of TObject;
    //         AErrorMessages: TStrings): boolean; override;
    function CanApplicationClose: Boolean; override;
    procedure ApplicationIsClosing; override;

  end;

implementation

uses
  SysUtils,
  VCL.ComCtrls,
  UDataFileObjects,
  UErrorHandlingOperations;



function TModelHydrologyManager.IsGraphLoaded: Boolean;
const OPNAME = 'TModelHydrologyManager.IsGraphLoaded';
begin
  Result := false;
  try
    Result :=  Assigned ( FDataTabSheetManager ) and
               Assigned ( FDataTabSheetManager.TabSheet );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.IsGridLoaded: Boolean;
const OPNAME = 'TModelHydrologyManager.IsGridLoaded';
begin
  Result := false;
  try

//    Result :=  Assigned ( FModelHydrologyTabSheetManager ) and Assigned ( FModelHydrologyTabSheetManager.GridEditorManager );

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelHydrologyManager.CreateMemberObjects;
const OPNAME = 'TModelHydrologyManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    CreateModelData;
    CreateFileEditManager;
    CreateModelDatasetConstructor;
    { Create File Selection... }
    FFileSelectionManager    := nil;
    FFileSelectionManager := THydrologyModelFileSelectionManager.Create ( FAppModules );
    if Assigned ( FFileSelectionManager ) then
      FOwnedAppObjects.Add ( FFileSelectionManager );
    { end if..then }
    { Create file Action Manager...}
    FModelFilesActionManager := nil;
    FModelFilesActionManager := TFilesActionYieldManager.Create(FAppModules); //TFilesActionHydrologyManager.Create ( FAppModules );
    if Assigned ( FModelFilesActionManager ) then
      FOwnedAppObjects.Add( FModelFilesActionManager );
    {  end if..then }
    { Create Hydrology Tabsheet...}
    CreateDataTabSheetManager;
    { Create Menue Items... }
    CreateModelMenuItemManager;
    {Create the specific GUImanager...}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyManager.CreateDataTabSheetManager;
const OPNAME = 'TModelHydrologyManager.CreateDataTabSheetManager';
begin
  try

    FDataTabSheetManager := nil;
    if TDataTabSheetManager.CanDataTabBeCreated then
      FDataTabSheetManager := TDataTabSheetManager.Create(FAppModules);
    if Assigned(FDataTabSheetManager) then
    begin
      FOwnedAppObjects.Add(FDataTabSheetManager);
      FDataTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TModelHydrologyManager.DestroyMemberObjects;
const OPNAME = 'TModelHydrologyManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    //FreeAndNil ( FModelData );
    DeleteModelDatasetConstructor;
    if Assigned ( FModelMenuItemManager ) then
      FreeAndNil ( FModelMenuItemManager );
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TModelHydrologyManager.CreateModelDatasetConstructor;
const OPNAME = 'TModelHydrologyManager.CreateModelDatasetConstructor';
begin
  try
    // Add the data set constructor.
    TSQLDatabaseLayer ( FAppModules.Database ).AddDataSetConstructor ( TModelHydrologyDatasetConstructor.Create( FAppModules ) ); //THydrologyFileTypeDataSetConstructor
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TModelHydrologyManager.DeleteModelDatasetConstructor;
const OPNAME = 'TModelHydrologyManager.DeleteModelDatasetConstructor';
begin
  try
    TSQLDatabaseLayer ( FAppModules.Database ).DeleteDataSetConstructorsOfType ( TModelHydrologyDatasetConstructor ); //THydrologyFileTypeDataSetConstructor
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.Initialise: boolean;
const OPNAME = 'TModelHydrologyManager.Initialise';
begin
  Result := inherited Initialise;
  try

  Result := HydrologyModelData.Initialise;
  if Assigned ( FFileEditManager ) then
    FFileEditManager.SetOnFileSave ( DoValidateSingleFile );

    Result := ( Result and Assigned ( FDataTabSheetManager ) )
                and ( FDataTabSheetManager.Initialise );

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelHydrologyManager.PopulateDataList ( AModelDataChangeType: TModelDataChangeType;
                                                   AList: TList ): Boolean;
const OPNAME = 'TModelHydrologyManager.PopulateDataList';
{var
  LIndex : integer;}
begin
  Result := False;
  try
{    if ModelDataLoaded and Assigned(AList) then
    begin
      if AModelDataChangeType = mdctReservior then
        for LIndex := 0 to TAbstractYieldModelDataObject ( FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirCount - 1 do
          AList.Add ( TAbstractYieldModelDataObject( FAppModules.Model.ModelData ).NetworkElementData.ReservoirList.ReservoirByIndex [ LIndex ] );
    end;}
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.LanguageHasChanged: boolean;
const OPNAME = 'TModelHydrologyManager.LanguageHasChanged';
begin
  Result := True;//inherited LanguageHasChanged;

  try
    if Assigned(FFileEditManager) then
      Result := Result and FFileEditManager.LanguageHasChanged;

    if Assigned ( FFileSelectionManager ) then
      Result := Result and FFileSelectionManager.LanguageHasChanged;
    { end if..then }

    if Assigned ( FDataTabSheetManager ) then
      Result := Result and ( FDataTabSheetManager.LanguageHasChanged );
    { end if..then }

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyManager.StudyHasChanged: boolean;
const OPNAME = 'TModelHydrologyManager.StudyHasChanged';
begin

  Result := False;
  try
  Result := LoadModelData;
  Result := inherited StudyHasChanged;

  if Assigned ( FModelFilesActionManager ) then
     Result := FModelFilesActionManager.StudyHasChanged;
  { end if..then }
  if Assigned ( FFileEditManager ) and Assigned ( FFileEditManager.TabSheet ) then
    FFileEditManager.TabSheet.StudyHasChanged;
  { end if..then }
  if Assigned ( FDataTabSheetManager ) then
     Result := Result and FDataTabSheetManager.StudyHasChanged;
  { end if..then }
  if Assigned ( FAppModules.MainForm ( ) ) then
    begin
      FAppModules.MainForm.ApplyPreviousTabIndex;
      OnTabHasChanged ( nil );
    end;
  {  end if..then }
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: string): boolean;
const OPNAME = 'TModelHydrologyManager.StudyDataHasChanged';
begin

  Result := ( inherited StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue ) );

  try
    if Assigned ( FDataTabSheetManager ) then
      Result := ( Result and FDataTabSheetManager.StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue ) );
    { end if..then }
    if Assigned ( FFileSelectionManager ) then
      Result := Result and FFileSelectionManager.StudyDataHasChanged ( AContext,AFieldName,AOldValue,ANewValue );
    { end..then }
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyManager.ProcessCustomModelEvent( AData: TModelMenuData ): boolean;
const OPNAME = 'TModelHydrologyManager.ProcessCustomModelEvent';
begin

  result := False;

  try
    if not ( Result and  Assigned ( FDataTabSheetManager ) ) then
      Result :=  FDataTabSheetManager.DoCustomTabSheetEvent(AData);
    { end if..then }
    if not Result then
      inherited ProcessCustomModelEvent ( AData );
    { end if..then }
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyManager.ResetState : boolean;
const OPNAME = 'TModelHydrologyManager.ResetState';
begin

  Result := inherited ResetState;

  try
    if Result and Assigned ( FDataTabSheetManager ) then
      Result := ( Result and FDataTabSheetManager.ResetState );
    { end if..then }

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyManager.ShowGraph: boolean;
const OPNAME = 'TModelHydrologyManager.ShowGraph';
var
  LCanChange: boolean;
begin
  Result := False;
  try
    OnTabChangeRequest ( nil, LCanChange );
    if ( Assigned ( FDataTabSheetManager ) ) and
       ( Assigned ( FDataTabSheetManager.TabSheet  ) ) and { GridEditorManager }
       ( LCanChange ) then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      OnTabHasChanged(nil);
    end;
    { end if...then }
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.ProcessEvent ( AEventType: integer;
                                               AData: TObject): boolean;
const OPNAME = 'TModelHydrologyManager.ProcessEvent';
var
  LCanChange: boolean;
begin
  Result := False;
  try
    case ( AEventType ) of
      CmeViewFileEdit              : ShowFileEditor;
      CmeViewEditGrid              : ShowGridEditor;
      CmeEditGridEditor            : EditGridEditor ( AData );
      CmeViewGraph                 : ShowGraph;
      CmeValidateFile              : DoValidateSingleFile;
      CmeRunModel                  : DoRunModel;
      CmeExportFile                : DoExportSingleFile;
      CmeImportFile                : DoImportSingleFile;
      CmeRefreshFileHints          : DoRefreshFileHints;
      CmeResultPageControlChanged  : OnTabHasChanged(nil);
      CmeResultPageControlChanging :
      begin
        LCanChange := True;
        OnTabChangeRequest(nil,LCanChange);
      end;
    else
      Result := inherited ProcessEvent ( AEventType, AData );
    end;
    { end case..of }

  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.EditGridEditor ( AData: TObject ): boolean;
const OPNAME = 'TModelHydrologyManager.EditGridEditor';
var
  LCanChange: boolean;
begin
  Result := False;
  try
    OnTabChangeRequest ( nil, LCanChange );
    if ( Assigned ( FDataTabSheetManager ) ) and
       ( Assigned ( FDataTabSheetManager.GridEditorManager ) ) and
       ( LCanChange ) then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      FDataTabSheetManager.EditGridEditor( AData );
      OnTabHasChanged ( nil );
    end;
    { end if..then }
   Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.ShowGridEditor: boolean;
const OPNAME = 'TModelHydrologyManager.ShowGridEditor';
var
  LCanChange: boolean;
begin
  Result := False;
  try
    OnTabChangeRequest ( nil, LCanChange );
    if ( Assigned ( FDataTabSheetManager ) ) and
       ( Assigned ( FDataTabSheetManager.TabSheet  ) ) and
       ( LCanChange ) then
    begin
      FAppModules.MainForm.ActivePage := FDataTabSheetManager.TabSheet;
      OnTabHasChanged(nil);
    end;
    { end if...then }
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TModelHydrologyManager.CreateModelMenuItemManager;
const OPNAME = 'TModelHydrologyManager.CreateModelMenuItemManager';
begin

  try
    if not ( Assigned ( FAppModules.MainForm ( ) ) ) then
      Exit;
    { end if..then }
    FModelMenuItemManager := TModelHydrologyMenuItem.Create(FAppModules ,
                            Assigned ( FDataTabSheetManager ),
                            Assigned ( FFileEditManager ));

  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.HydrologyModelData:  TYieldModelDataObject; //THydrologyFileLine; //;
const OPNAME = 'TModelHydrologyManager.HydrologyModelData';
{var
  LHydrologyFileLine : TYieldModelDataObject;//THydrologyFileLine; //
  }
begin
  Result := nil;
  try
    Result := TYieldModelDataObject ( FModelData ); // //THydrologyFileLine ( FModelData );
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TModelHydrologyManager.CreateModelData;
const OPNAME = 'TModelHydrologyManager.CreateModelData';
begin
  inherited;
  FModelData := nil;
  try
    FModelData := TYieldModelDataObject.Create ( FAppModules ); // ( AppModules );  // ( FAppModules );  //THydrologyFileLine.Create; //;
  except on E: Exception do HandleError ( E, OPNAME ); end;
end;


function TModelHydrologyManager.LoadModelData: boolean;
const OPNAME = 'TModelHydrologyManager.LoadModelData';
begin
  Result := False;
  try
    Result := SelectModelFileNamesAndDisplay;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelHydrologyManager.SelectModelFileNamesAndDisplay: boolean;
const OPNAME = 'TModelHydrologyManager.SelectModelFileNamesAndDisplay';
var
  LDataFileObjects: TDataFileObjects;
begin
  Result := False;
  try
    if not Assigned ( FFileSelectionManager ) then
    begin
      Result := True;
    end
    else
    begin
      LDataFileObjects := TDataFileObjects.Create;
      try

        Result := FFileSelectionManager.PopulateFileNames (LDataFileObjects, HydrologyModelData.CastFileNamesObject);

        if Result and
         Assigned ( FFileSelectionManager ) and
         Assigned ( FFileEditManager ) and
         Assigned ( FFileEditManager.TabSheet ) then
         Result := FFileSelectionManager.PopulateTreeView ( TTreeViewTabSheet ( FFileEditManager.TabSheet ).TreeView,
                                                         HydrologyModelData.CastFileNamesObject);
      finally
        FreeAndNil(LDataFileObjects);
      end;
    end

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.DoRunModel: boolean;
const OPNAME = 'TModelHydrologyManager.DoRunModel';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoRunModel;
    TFileNamesList( HydrologyModelData.CastFileNamesObject ).SetFileHintsSet(True);
    if Result and
       Assigned ( FFileSelectionManager ) and
       Assigned ( FFileEditManager ) and
       Assigned ( FFileEditManager.TabSheet ) then
    begin
      //LockWindowUpdate(FAppModules.MainForm.MainForm.Handle);
      //try
        Result := FFileSelectionManager.PopulateTreeView ( TTreeViewTabSheet ( FFileEditManager.TabSheet ).TreeView,
                 HydrologyModelData.CastFileNamesObject);
        FFileEditManager.TabSheet.LanguageHasChanged;
      //finally
      //  LockWindowUpdate(0);
      //  FAppModules.MainForm.MainForm.Refresh;
     // end;
    end;

    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
    begin
//      SetSystemMenuState;
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelHydrologyManager.ShowFileEditor: boolean;
const OPNAME = 'TModelHydrologyManager.ShowFileEditor';
var LCanChange: boolean;
begin
  Result := False;
  try
    if Assigned ( FFileEditManager ) and Assigned ( FFileEditManager.TabSheet ) then
    begin
      OnTabChangeRequest ( nil, LCanChange );
      if LCanChange then
      begin
        FAppModules.MainForm.ActivePage := FFileEditManager.TabSheet;
        OnTabHasChanged(nil);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TModelHydrologyManager.CreateFileEditManager;
const OPNAME = 'TModelHydrologyManager.CreateFileEditManager';
begin
  try
    if not Assigned ( FAppModules.MainForm ( ) ) then
      Exit;

    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\FileEditor.dll',
      TAbstractAppObject( FFileEditManager ), FAppModules, False, OPNAME);

    if Assigned ( FFileEditManager ) then
    begin
      FOwnedAppObjects.Add ( FFileEditManager );
      FFileEditManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.DoValidateSingleFile: boolean;
const OPNAME = 'TModelHydrologyManager.DoValidateSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateSingleFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TModelHydrologyManager.ModelData: TInterfacedObject;
const OPNAME = 'TModelHydrologyManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelHydrologyManager.DoExportSingleFile: boolean;
const OPNAME = 'TModelHydrologyManager.DoExportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoExportSingleFile;
//    SetSystemMenuState;
    if Assigned ( FFileEditManager ) and Assigned ( FFileEditManager.TabSheet ) then
      FFileEditManager.TabSheet.StudyDataHasChanged ( sdccImport,'SingleFile','','' );

  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelHydrologyManager.DoImportSingleFile: boolean;
const OPNAME = 'TModelHydrologyManager.DoImportSingleFile';
begin
  Result := False;
  try

    Result := FModelFilesActionManager.DoImportSingleFile;
    Result := Result and RefreshModelData;

  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.RefreshModelData: boolean;
const OPNAME = 'TModelHydrologyManager.RefreshModelData';
begin
  Result := False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TModelHydrologyManager.DoRefreshFileHints: boolean;
const OPNAME = 'TModelHydrologyManager.DoRefreshFileHints';
begin
  Result := False;
  try

    Result := FModelFilesActionManager.DoRefreshFileHints;

  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

procedure TModelHydrologyManager.ApplicationIsClosing;
const OPNAME = 'TModelHydrologyManager.ApplicationIsClosing';
begin
  inherited ApplicationIsClosing;

  try
    if Assigned ( FFileEditManager ) and Assigned ( FFileEditManager.TabSheet ) then
      FFileEditManager.TabSheet.ApplicationIsClosing;
  { end if..then }

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TModelHydrologyManager.CanApplicationClose: Boolean;
const OPNAME = 'TModelHydrologyManager.CanApplicationClose';
begin
  Result := inherited CanApplicationClose;
  try
    if Assigned ( FFileEditManager ) and Assigned ( FFileEditManager.TabSheet ) then
      Result := Result and FFileEditManager.TabSheet.CanApplicationClose;
  { end if..then }

  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TModelHydrologyManager.ModelName: string;
const OPNAME = 'TModelHydrologyManager.ModelName';
begin
  Result := '';
  try
    Result := CHydrology;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TModelHydrologyManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TModelHydrologyManager.DoStationFilter';
begin
  Result := True;
  try //Do not insert code here
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.


//
//
//  UNIT      : Contains TStomsaFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2002/12/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStomsaFileSelectionManager;

interface

uses
  VCL.ComCtrls,
  UFileNames,
  UDataFileObjects,
  UFileSelectionManager;

type
  TStomsaFileSelectionManager = class(TFileSelectionManager)
  public
//    function Initialise: boolean; override;
    function PopulateFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean; override;
    function PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean; override;
  end;

implementation

uses
  SysUtils,
  Dialogs,
  UDataModule,
  UStomsaModelManager,
  UAbstractFileNamesObject,
  UStomsaFileNamesDatabaseAgent,
  UErrorHandlingOperations;

{ TStomsaFileSelectionManager }
{
function TStomsaFileSelectionManager.Initialise: boolean;
const OPNAME = 'TStomsaFileSelectionManager.Initialise';
begin
  Result := True;
  try
    if inherited Initialise then
      Result := PopulateFileNames(nil, TfmData(TStomsaModelManager(FAppModules.Model).ModelData).DataStorage.FileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
function TStomsaFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TStomsaFileSelectionManager.PopulateFileNames';
var
  LFileNamesDatabaseAgent : TStomsaFileNamesDatabaseAgent;
begin
  inherited PopulateFileNames(ADataFileObjects,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    AFileNamesObject.Reset;

    LFileNamesDatabaseAgent := TStomsaFileNamesDatabaseAgent.Create(FAppModules);
    try
      LFileNamesDatabaseAgent.ReadParamFileName(ADataFileObjects,AFileNamesObject);
      LFileNamesDatabaseAgent.ReadHydrologyFileNames(ADataFileObjects,AFileNamesObject);
    finally
      FreeAndNil(LFileNamesDatabaseAgent);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStomsaFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TStomsaFileSelectionManager.PopulateTreeView';
var
  LMainNode: TTreeNode;
  LIndex: integer;
  LCaption : string;
  LFileName: TAbstractModelFileName;
  LOnTreeViewNodeChangedEvent  :TTVChangedEvent;
  LOnTreeViewNodeChangingEvent :TTVChangingEvent;
begin
  inherited PopulateTreeView(ATreeView,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ATreeView) then
      raise Exception.Create('Tree View object parameter is not yet assigned.');

    LOnTreeViewNodeChangedEvent  := ATreeView.OnChange;
    LOnTreeViewNodeChangingEvent := ATreeView.OnChanging;
    try
      ATreeView.OnChange   := nil;
      ATreeView.OnChanging := nil;
      ATreeView.Selected   := nil;

      //DirectoryFileName
      ATreeView.Items.Clear;

      //ParamFileName

      LCaption  := FAppModules.Language.GetString(AFileNamesObject.ParamFileNames.CaptionStr);
      LMainNode := ATreeView.Items.AddObject(nil,LCaption,AFileNamesObject.ParamFileNames);
      for LIndex := 0 to AFileNamesObject.ParamFileNames.Count - 1 do
      begin
        LFileName := AFileNamesObject.ParamFileNames.FileNameObject[LIndex];
        ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
      end;

      //HydrologyFileNames
      if (AFileNamesObject.HydrologyFileNames.Count > 0) then
      begin
        LCaption  := FAppModules.Language.GetString(AFileNamesObject.HydrologyFileNames.CaptionStr);
        LMainNode := ATreeView.Items.AddObject(nil,LCaption,AFileNamesObject.HydrologyFileNames);
        for LIndex := 0 to AFileNamesObject.HydrologyFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.CastHydrologyFileNames[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;
    finally
      ATreeView.OnChange   := LOnTreeViewNodeChangedEvent;
      ATreeView.OnChanging := LOnTreeViewNodeChangingEvent;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

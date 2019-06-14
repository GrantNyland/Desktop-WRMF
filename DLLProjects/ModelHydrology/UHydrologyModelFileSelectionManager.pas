//
//
//  UNIT      : Contains THydrologyModelFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2002/12/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyModelFileSelectionManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UFileselectionManager,
  UHydrologyModelFileNamesDatabaseAgent;

type
  THydrologyModelFileSelectionManager = class(TFileSelectionManager)
  protected
    FFileNamesDatabaseAgent :THydrologyModelFileNamesDatabaseAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject :TModelFileNames): boolean; override;
    function PopulateTreeView(ATreeView: TTreeView;AFileNamesObject :TModelFileNames): boolean; override;
  end;

implementation

uses
  SysUtils,
  UProgressDialog,
  UErrorHandlingOperations;


{ THydrologyModelFileSelectionManager }

procedure THydrologyModelFileSelectionManager.CreateMemberObjects;
const OPNAME = 'THydrologyModelFileSelectionManager.CreateMemberObjects';
begin
  try
    FFileNamesDatabaseAgent := THydrologyModelFileNamesDatabaseAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyModelFileSelectionManager.DestroyMemberObjects;
const OPNAME = 'THydrologyModelFileSelectionManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFileNamesDatabaseAgent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModelFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'THydrologyModelFileSelectionManager.PopulateFileNames';
begin
  inherited PopulateFileNames(ADataFileObjects,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

      AFileNamesObject.Reset;
      Result := FFileNamesDatabaseAgent.ReadHydrologyFileNames(AFileNamesObject);
      if Result then
        AFileNamesObject.CastHydrologyFileNames.SetFileHintsSet(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyModelFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'THydrologyModelFileSelectionManager.PopulateTreeView';
var
  LMainNode: TTreeNode;
  LIndex: integer;
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

      //HydrologyFileNames
      if (AFileNamesObject.HydrologyFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.HydrologyFileNames.CaptionStr,AFileNamesObject.HydrologyFileNames);
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

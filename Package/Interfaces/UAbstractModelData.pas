//
//
//  UNIT      : Contains TAbstractModelData Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 22/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAbstractModelData;

interface

uses

  // Delphi
  Classes, SysUtils,

  // WRMF
  UAbstractObject,
  UAbstractFileNamesObject,
  UViewModelDataObject;

type
  TAbstractModelData = class(TAbstractAppObject)
  protected

    // Members.
    FFilesLineTypes: TAbstractFilesLineTypes;
    FFileNamesObject: TAbstractModelFileNameList;

    // Overriden from TAbstractAppObject.
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    // Introduced in this class.
    function GetFilesLineTypes: TAbstractFilesLineTypes; virtual;
    function GetFileNamesObject: TAbstractModelFileNameList; virtual;
  public

    // Overriden from TAbstractAppObject.
    function Initialise: Boolean; override;

    // Introduced in this class.
    function GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean; virtual;
    function ViewDataItemExist(AItemType, AItemName: string): boolean; virtual;

    // Properties
    property FilesLineTypes: TAbstractFilesLineTypes read GetFilesLineTypes;
    property FileNamesObject: TAbstractModelFileNameList read GetFileNamesObject;
  end;

implementation

uses
  UFileNames,
  UFilesLineTypeObject,
  UErrorHandlingOperations;

{ TAbstractModelData }

procedure TAbstractModelData.CreateMemberObjects;
const OPNAME = 'TAbstractModelData.CreateMemberObjects';
begin
  try
    FFilesLineTypes := TFilesLineTypes.Create;
    FFileNamesObject := TModelFileNames.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelData.DestroyMemberObjects;
const OPNAME = 'TAbstractModelData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFilesLineTypes);
    FreeAndNil(FFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelData.Initialise: Boolean;
const OPNAME = 'TAbstractModelData.Initialise';
begin
  Result := False;
  try
    if (inherited Initialise) then
    begin
      TFilesLineTypes(FFilesLineTypes).Clear;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelData.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TAbstractModelData.GetViewDataItems';
begin
  Result := False;
  try
    AHandled := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelData.ViewDataItemExist(AItemType, AItemName: string): boolean;
const OPNAME = 'TAbstractModelData.ViewDataItemExist';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAbstractModelData.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TAbstractModelData.GetFilesLineTypes';
begin
  Result := nil;
  try
    Result := FFilesLineTypes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelData.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TAbstractModelData.GetFileNamesObject';
begin
  Result := nil;
  try
    Result := FFileNamesObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

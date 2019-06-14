//
//
//  UNIT      : Contains TFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2002/12/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFileSelectionManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UFileNames,
  UDataFileObjects,
  UAbstractObject;

type
  TFileSelectionManager = class(TAbstractAppObject)
  public
    function PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject :TModelFileNames): boolean; virtual;
    function PopulateTreeView(ATreeView: TTreeView;AFileNamesObject :TModelFileNames): boolean; virtual;
    //function SaveFileNamesToDB(AFileNamesObject :TModelFileNames): boolean; virtual;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TFileSelectionManager }

function TFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TFileSelectionManager.PopulateFileNames';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TFileSelectionManager.PopulateTreeView';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TFileSelectionManager.SaveFileNamesToDB(AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TFileSelectionManager.SaveFileNamesToDB';
begin
  Result := False;
  try
    Result := Assigned(AFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

end.

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
  Classes,
  SysUtils,

  //  DWAF VCL
  UAbstractObject,
  UAbstractFileNamesObject,
  UViewModelDataObject;

type


  TAbstractModelData = class(TAbstractAppObject)
  protected
    function GetFilesLineTypes: TAbstractFilesLineTypes; virtual; abstract;
    function GetFileNamesObject: TAbstractModelFileNameList; virtual; abstract;
  public
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean;virtual; abstract;
    function ViewDataItemExist(AItemType, AItemName : string): boolean; virtual;
    property FilesLineTypes: TAbstractFilesLineTypes read GetFilesLineTypes;
    property FileNamesObject: TAbstractModelFileNameList read GetFileNamesObject;
  end;

implementation

uses
  UErrorHandlingOperations;

{ TAbstractModelData }

function TAbstractModelData.ViewDataItemExist(AItemType,AItemName: string): boolean;
const OPNAME = 'TAbstractModelData.ViewDataItemExist';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

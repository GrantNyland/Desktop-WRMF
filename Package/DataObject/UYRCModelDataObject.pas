//
//
//  UNIT      : Contains  TYRCModelDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCModelDataObject;

interface

uses
  Classes, SysUtils, Contnrs,

  UAbstractObject,
  UAbstractModelData,
  UAbstractFileNamesObject,
  UViewModelDataObject,
  UAbstractYRCData,
  UYRCGraphDataObject;

type
  TYRCModelDataObject = class(TAbstractModelData)
  protected
    FYRCGraphDataObject: TYRCGraphDataObject;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetYRCGraphDataObject: TAbstractYRCGraphDataObject;
  public
    procedure Reset;
    function Initialise: boolean; override;
    function GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean; override;

    property YRCGraphDataObject: TYRCGraphDataObject read FYRCGraphDataObject;
  end;

implementation

uses
  UErrorHandlingOperations;

procedure TYRCModelDataObject.CreateMemberObjects;
const OPNAME = 'TYRCModelDataObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FYRCGraphDataObject := TYRCGraphDataObject.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelDataObject.DestroyMemberObjects;
const OPNAME = 'TYRCModelDataObject.DestroyMemberObjects';
begin
  try
    FreeAndNil(FYRCGraphDataObject);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TYRCModelDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    AHandled := False;
    if (Trim(AViewId) <> '') and Assigned(AItemsList) then
    begin
      LUpperViewId := UpperCase(Trim(AViewId));
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYRCModelDataObject.GetYRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCModelDataObject.GetYRCGraphDataObject';
begin
  Result := nil;
  try
    Result := FYRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCModelDataObject.Initialise: boolean;
const OPNAME = 'TYRCModelDataObject.Initialise';
begin
  Result := False;
  try
    if (inherited Initialise) then
      Result := FYRCGraphDataObject.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCModelDataObject.Reset;
const OPNAME = 'TYRCModelDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

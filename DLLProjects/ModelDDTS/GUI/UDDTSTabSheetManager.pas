//
//
//  UNIT      : Contains TDDTSTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDDTSTabSheetManager;

interface

uses
  VCL.ComCtrls,
  VCL.Controls,
  VCL.ExtCtrls,
  UDataViewerManager,
  UTabSheetManager,
  UGenericModelLinkClasses,
  UAbstractModelObjects;

type
  TDDTSTabSheetManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;override;

  end;

implementation

uses
  SysUtils,
  UDDTSTabSheet,
  UErrorHandlingOperations, UAbstractObject;

procedure TDDTSTabSheetManager.CreateMemberObjects;
const OPNAME = 'TDDTSTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TDDTSTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TDDTSTabSheetManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TDDTSTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheetManager.Initialise: Boolean;
const OPNAME = 'TDDTSTabSheetManager.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

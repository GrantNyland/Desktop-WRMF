//
//
//  UNIT      : Contains TDailyDiversionTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDailyDiversionTabSheetManager;

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
  TDailyDiversionTabSheetManager = class(TDataViewerManager)
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
  UDailyDiversionTabSheet,
  UErrorHandlingOperations, UAbstractObject;

procedure TDailyDiversionTabSheetManager.CreateMemberObjects;
const OPNAME = 'TDailyDiversionTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TDailyDiversionTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDailyDiversionTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionTabSheetManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TDailyDiversionTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheetManager.Initialise: Boolean;
const OPNAME = 'TDailyDiversionTabSheetManager.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

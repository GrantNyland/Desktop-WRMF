//
//
//  UNIT      : Contains TYieldModelDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UYieldModelDataLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UYieldModelDataSQLAgent;

type
  TYieldModelDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TYieldModelDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function StudyDataIsEmpty: boolean;
    function ResetYieldModelData: boolean;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;
procedure TYieldModelDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TYieldModelDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TYieldModelDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TYieldModelDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TYieldModelDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYieldModelDataLoadAgent.StudyDataIsEmpty: boolean;
const OPNAME = 'TYieldModelDataLoadAgent.StudyDataIsEmpty';
begin
  Result := False;
  try
    Result := FSQLAgent.StudyDataIsEmpty;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYieldModelDataLoadAgent.ResetYieldModelData: boolean;
const OPNAME = 'TYieldModelDataLoadAgent.ResetYieldModelData';
begin
  Result := False;
  try
    Result := FSQLAgent.ResetYieldModelData;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

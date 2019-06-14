//
//  UNIT      : Contains TFieldUpdateSQLStep Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFieldUpdateSQLStep;

interface

uses
  UAbstractObject;

type
  TFieldUpdateSQLStep = class(TAbstractFieldUpdateSQLStep)
  protected
    procedure CreateMemberObjects; override;
    procedure TrimSQL(var ASQL: string);
  public
    procedure PopulateMemberVariables(AFieldProperty: TAbstractFieldProperty;
      ATableProperty: TAbstractTableProperty; AStepNo: integer; AFieldInTable, AUpdateSQL, AGetValueSQL: string);
  end;

implementation

uses
  SysUtils,
  Classes,
  UErrorHandlingOperations;

procedure TFieldUpdateSQLStep.CreateMemberObjects;
const OPNAME = 'TFieldUpdateSQLStep.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FFieldProperty := nil;
    FTableProperty := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldUpdateSQLStep.PopulateMemberVariables(
  AFieldProperty: TAbstractFieldProperty;
  ATableProperty: TAbstractTableProperty;
  AStepNo: integer; AFieldInTable, AUpdateSQL, AGetValueSQL: string);
const OPNAME = 'TFieldUpdateSQLStep.PopulateMemberVariables';
begin
  try
    FFieldProperty := AFieldProperty;
    FTableProperty := ATableProperty;
    FStepNo        := AStepNo;
    FFieldInTable  := AFieldInTable;
    FUpdateSQL     := AUpdateSQL;
    FGetValueSQL   := AGetValueSQL;
    TrimSQL(FUpdateSQL);
    TrimSQL(FGetValueSQL);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldUpdateSQLStep.TrimSQL(var ASQL: string);
const OPNAME = 'TFieldUpdateSQLStep.TrimSQL';
var LIndex: integer;
begin
  try
    if (Length(ASQL) > 0) then
    begin

      // Convert line ends to spaces.
      for LIndex := Length(ASQL) downto 1 do
        if CharInSet(ASQL[LIndex] , [#13, #10]) then
          ASQL[LIndex] := ' ';

      // Delete duplicate and unnecassery spaces.
      for LIndex := Length(ASQL) - 1 downto 2 do
      begin
        if (ASQL[LIndex] = ' ') then
        begin
          if CharInSet(ASQL[LIndex - 1], [' ','(',',']) then
            Delete(ASQL, LIndex, 1);
          if CharInSet(ASQL[LIndex + 1], [')',',']) then
            Delete(ASQL, LIndex, 1);
        end;
      end;
      if CharInSet(ASQL[1] , [' ']) then
        Delete(ASQL, 1, 1);
      if CharInSet(ASQL[Length(ASQL)] , [' ']) then
        Delete(ASQL, Length(ASQL), 1);
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

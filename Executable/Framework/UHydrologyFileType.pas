//
//  UNIT      : Contains THydrologyFileType Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/05/10
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UHydrologyFileType;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject;

type
  THydrologyFileType = class(TAbstractHydrologyFileType)
  protected
    FHydrologyFileTypes: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise(AHydrologyFileTypesDataset: TAbstractModelDataset): boolean; reintroduce;
    function GetFileType(AHydrologyFileExt: string): integer; override;

  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure THydrologyFileType.CreateMemberObjects;
const OPNAME = 'THydrologyFileType.CreateMemberObjects';
begin
  try
    FHydrologyFileTypes := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyFileType.DestroyMemberObjects;
const OPNAME = 'THydrologyFileType.DestroyMemberObjects';
begin
  try
    FreeAndNil(FHydrologyFileTypes);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileType.GetFileType(AHydrologyFileExt: string): integer;
const OPNAME = 'THydrologyFileType.GetFileType';
var
  LIndex: integer;
begin
  Result := -1;
  try
    LIndex := FHydrologyFileTypes.IndexOf(UpperCase(AHydrologyFileExt));
    if(LIndex >= 0) then
      Result := Integer(FHydrologyFileTypes.Objects[LIndex]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileType.Initialise(AHydrologyFileTypesDataset: TAbstractModelDataset): boolean;
const OPNAME = 'THydrologyFileType.Initialise';
var
  LHydrologyFileType: integer;
  LHydrologyFileExt: String;
  LAddIndex: integer;
begin
  Result := False;
  try
    FHydrologyFileTypes.Clear;
    AHydrologyFileTypesDataset.DataSet.Open;
    AHydrologyFileTypesDataset.DataSet.First;
    while (not AHydrologyFileTypesDataset.DataSet.Eof) do
    begin
      LHydrologyFileType := AHydrologyFileTypesDataset.DataSet.FieldByName('FileType').AsInteger;
      LHydrologyFileExt  := UpperCase(Trim(AHydrologyFileTypesDataset.DataSet.FieldByName('FileExtention').AsString));
      LAddIndex          := FHydrologyFileTypes.Add(LHydrologyFileExt);
      FHydrologyFileTypes.Objects[LAddIndex] := TObject(LHydrologyFileType);
      AHydrologyFileTypesDataset.DataSet.Next;
    end;
    LHydrologyFileType := 200;
    LHydrologyFileExt  := UpperCase('.prn');
    LAddIndex          := FHydrologyFileTypes.Add(LHydrologyFileExt);
    FHydrologyFileTypes.Objects[LAddIndex] := TObject(LHydrologyFileType);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

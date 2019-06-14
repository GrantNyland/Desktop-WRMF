//
//
//  UNIT      : Contains class TStringListOfStringLists.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStringListOfStringLists;

interface

uses
  Classes;

type
  TListPosition = record
    RowIndex: integer;
    ItemIndex: integer;
  end;
  TStringListOfStringLists = class(TObject)
  protected
    FItems: TStringList;
    FOwnsObjects: boolean;
    function GetRow(ARowIndex: integer): TStringList;
    function GetItem(ARowIndex, AnItemIndex: integer): string;
    function GetObject(ARowIndex, AnItemIndex: integer): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ClearRow(ARowIndex: integer); virtual;
    function RowCount: integer;
    function RowItemsCount(ARowIndex: integer): integer;
    function Add(ARowValue: string): TListPosition; overload;
    function Add(ARowValue, AnItemValue: string; AnObject: TObject = nil): TListPosition; overload;
    procedure AssignFrom(AList: TStringListOfStringLists);
    function FindRow(AValue: string): TStringList;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    property Items: TStringList read FItems;
    property Row[ARowIndex: integer]: TStringList read GetRow;
    property Item[ARowIndex, AnItemIndex: integer]: string read GetItem; default;
    property Objects[ARowIndex, AnItemIndex: integer]: TObject read GetObject;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

constructor TStringListOfStringLists.Create;
const OPNAME = 'TStringListOfStringLists.Create';
begin
  try
    inherited;
    FItems := TStringList.Create;
    FOwnsObjects := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TStringListOfStringLists.Destroy;
const OPNAME = 'TStringListOfStringLists.Destroy';
begin
  try
    Clear;
    FreeAndNil(FItems);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringListOfStringLists.Clear;
const OPNAME = 'TStringListOfStringLists.Clear';
var LRowIndex: integer;
begin
  try
    for LRowIndex := 0 to RowCount - 1 do
    begin
      ClearRow(LRowIndex);
      FItems.Objects[LRowIndex].Free;
      FItems.Objects[LRowIndex] := nil;
    end;
    FItems.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringListOfStringLists.ClearRow(ARowIndex: integer);
const OPNAME = 'TStringListOfStringLists.ClearRow';
var LItemIndex: integer;
begin
  try
    if FOwnsObjects then
    begin
      for LItemIndex := 0 to Row[ARowIndex].Count - 1 do
      begin
        Row[ARowIndex].Objects[LItemIndex].Free;
        Row[ARowIndex].Objects[LItemIndex] := nil;
      end;
    end;
    Row[ARowIndex].Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.RowCount: integer;
const OPNAME = 'TStringListOfStringLists.RowCount';
begin
  Result := 0;
  try
    Result := FItems.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.RowItemsCount(ARowIndex: integer): integer;
const OPNAME = 'TStringListOfStringLists.RowItemsCount';
begin
  Result := 0;
  try
    if (ARowIndex >= 0) and (ARowIndex < FItems.Count) then
      Result := Row[ARowIndex].Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.Add(ARowValue: string): TListPosition;
const OPNAME = 'TStringListOfStringLists.Add';
begin
  Result.RowIndex := -1;
  Result.ItemIndex := -1;
  try
    if (not Assigned(FindRow(ARowValue))) then
      Result.RowIndex := FItems.AddObject(ARowValue, TStringList.Create);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.Add(ARowValue, AnItemValue: string; AnObject: TObject): TListPosition;
const OPNAME = 'TStringListOfStringLists.Add';
var LRow: TStringList;
begin
  Result.RowIndex := -1;
  Result.ItemIndex := -1;
  try
    LRow := FindRow(ARowValue);
    if Assigned(LRow) then
    begin
      Result.RowIndex := FItems.IndexOf(ARowValue);
    end
    else
    begin
      LRow := TStringList.Create;
      Result.RowIndex := FItems.AddObject(ARowValue, LRow);
    end;
    Result.ItemIndex := LRow.AddObject(AnItemValue, AnObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStringListOfStringLists.AssignFrom(AList: TStringListOfStringLists);
const OPNAME = 'TStringListOfStringLists.AssignFrom';
var LRowIndex, LItemIndex: integer;
begin
  try
    Clear;
    for LRowIndex := 0 to AList.RowCount - 1 do
      for LItemIndex := 0 to AList.RowItemsCount(LRowIndex) - 1 do
        Add(AList.Items[LRowIndex], AList[LRowIndex, LItemIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.GetRow(ARowIndex: integer): TStringList;
const OPNAME = 'TStringListOfStringLists.GetRow';
begin
  Result := nil;
  try
    if (ARowIndex >= 0) and (ARowIndex < FItems.Count) then
      Result := TStringList(FItems.Objects[ARowIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.GetItem(ARowIndex, AnItemIndex: integer): string;
const OPNAME = 'TStringListOfStringLists.GetItem';
var LRow: TStringList;
begin
  Result := '';
  try
    LRow := Row[ARowIndex];
    if Assigned(LRow) then
      if (AnItemIndex >= 0) and (AnItemIndex < LRow.Count) then
        Result := LRow[AnItemIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.GetObject(ARowIndex, AnItemIndex: integer): TObject;
const OPNAME = 'TStringListOfStringLists.GetObject';
var LRow: TStringList;
begin
  Result := nil;
  try
    LRow := Row[ARowIndex];
    if Assigned(LRow) then
      if (AnItemIndex >= 0) and (AnItemIndex < LRow.Count) then
        Result := LRow.Objects[AnItemIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStringListOfStringLists.FindRow(AValue: string): TStringList;
const OPNAME = 'TStringListOfStringLists.FindRow';
var LIndex: integer;
begin
  Result := nil;
  try
    LIndex := FItems.IndexOf(AValue);
    if (LIndex >= 0) then
      Result := TStringList(FItems.Objects[LIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

//
//
//  UNIT      : Contains TViewModelDataItem Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 22/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UViewModelDataObject;

interface

uses
  Classes, sysutils,Contnrs;

type

  TViewModelDataItem = class(TObject)
  protected
    FWeighting: Integer;
    FCaption: String;
    FParamNames: String;
    FParamValues: String;
    FDataType: string;
  public
    constructor Create;
    procedure Reset;
    property Weighting   : Integer    read FWeighting     write FWeighting;
    property Caption     : String     read FCaption       write FCaption;
    property ParamNames  : String     read FParamNames    write FParamNames;
    property ParamValues : String     read FParamValues   write FParamValues;
    property DataType    : String     read FDataType      write FDataType;
  end;

  TViewModelDataItemsList  = class(TObject)
  protected
    FViewModelDataItems : TObjectList;
    function GetViewModelDataItemByIndex(AIndex: integer): TViewModelDataItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
    procedure Reset;
    function ItemsCount: integer;
    function AddViewModelDataItem:TViewModelDataItem;
    function RemoveViewModelDataItem(AItem:TViewModelDataItem): boolean;
    property ViewModelDataItemByIndex[AIndex: integer]: TViewModelDataItem read GetViewModelDataItemByIndex;
  end;


implementation


uses UErrorHandlingOperations;

{TViewModelDataItem}


constructor TViewModelDataItem.Create;
const OPNAME = 'TViewModelDataItem.Create';
begin
  inherited Create;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TViewModelDataItem.Reset;
const OPNAME = 'TViewModelDataItem.Reset';
begin
  try
    FWeighting   := 0;
    FCaption     := '';
    FParamNames  := '';
    FParamValues := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TViewModelDataItemsList }

constructor TViewModelDataItemsList.Create;
const OPNAME = 'TViewModelDataItemsList.Create';
begin
  inherited Create;
  try
    FViewModelDataItems := TObjectList.Create(True);
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TViewModelDataItemsList.Destroy;
const OPNAME = 'TViewModelDataItemsList.Destroy';
begin
  try
    FreeAndNil(FViewModelDataItems);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewModelDataItemsList.GetViewModelDataItemByIndex(AIndex: integer): TViewModelDataItem;
const OPNAME = 'TViewModelDataItemsList.GetViewModelDataItemByIndex';
begin
  Result := nil;
  try
    if (AIndex < FViewModelDataItems.Count) and Assigned(FViewModelDataItems.Items[AIndex]) then
      Result := TViewModelDataItem(FViewModelDataItems.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TViewModelDataItemsList.Reset;
const OPNAME = 'TViewModelDataItemsList.Reset';
begin
  try
    FViewModelDataItems.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewModelDataItemsList.AddViewModelDataItem: TViewModelDataItem;
const OPNAME = 'TViewModelDataItemsList.AddViewModelDataItem';
begin
  Result := nil;
  try
    Result := TViewModelDataItem.Create;
    FViewModelDataItems.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewModelDataItemsList.RemoveViewModelDataItem(AItem: TViewModelDataItem): boolean;
const OPNAME = 'TViewModelDataItemsList.AddViewModelDataItem';
begin
  Result := False;
  try
    if(FViewModelDataItems.IndexOf(AItem) >= 0) then
    begin
      FViewModelDataItems.Delete(FViewModelDataItems.IndexOf(AItem));
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewModelDataItemsList.ItemsCount: integer;
const OPNAME = 'TViewModelDataItemsList.ItemsCount';
begin
  Result := 0;
  try
    Result := FViewModelDataItems.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TViewModelDataItemsList.Sort;
const OPNAME = 'TViewModelDataItemsList.Sort';
var
  LContainer: TStringList;
  LCount: integer;
  LViewModelDataItem: TViewModelDataItem;
begin
  try
    if (FViewModelDataItems.Count > 0) then
    begin
      LContainer := TStringList.Create;
      try
        LContainer.Sorted := True;
        LContainer.Duplicates := dupAccept;
        for LCount := 0 to FViewModelDataItems.Count - 1 do
        begin
          LViewModelDataItem := ViewModelDataItemByIndex[LCount];
          LContainer.AddObject(LViewModelDataItem.FCaption,LViewModelDataItem);
        end;
        FViewModelDataItems.OwnsObjects := False;
        try
          FViewModelDataItems.Clear;
          for LCount := 0 to LContainer.Count - 1 do
            FViewModelDataItems.Add(LContainer.Objects[LCount]);
        finally
          FViewModelDataItems.OwnsObjects := True;
        end;
      finally
        LContainer.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

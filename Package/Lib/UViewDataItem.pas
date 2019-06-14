//
//
//  UNIT      : Contains the class TViewDataItem.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataItem;

interface

uses
  Classes,
  Contnrs,
  Vcl.ComCtrls;

type

  TViewChangeResult = class(TObject)
  protected
    FResult: boolean;
  public
    property Result: boolean read FResult write FResult;
  end;

  TParamList = array of string;
  TViewDataSet = class(TObject)
  protected
    FDatasetID: string;
    FEditable: boolean;
    FSQLType: integer;
    FNoDataMessage,
    FViewSQL: string;
    FParamNames: TParamList;
    FParamValues: TParamList;
    FLoadFromFile : boolean;
    function GetParamCount: integer;
    procedure SetParamCount(const AValue: integer);
    function GetIDValues: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure AssignFrom(AViewDataSet: TViewDataSet);
    procedure LoadParams(AParamNames, AParamValues: TStringList); overload;
    procedure LoadParams(AParamNames, AParamValues: TParamList); overload;
    function AreParamsEqual(AParamValues: TParamList): boolean;
    property ParamCount: integer read GetParamCount write SetParamCount;
    property DatasetID: string read FDatasetID write FDatasetID;
    property Editable: boolean read FEditable write FEditable;
    property SQLType: integer read FSQLType write FSQLType;
    property NoDataMessage: string read FNoDataMessage write FNoDataMessage;
    property ViewSQL: string read FViewSQL write FViewSQL;
    property ParamNames: TParamList read FParamNames write FParamNames;
    property ParamValues: TParamList read FParamValues write FParamValues;
    property IDValues: string read GetIDValues;
    property LoadFromFile : boolean read FLoadFromFile write FLoadFromFile;
  end;

  TViewDataNode = class(TObject)
  private
    function UnaryStringAndCommaListCompare(const AString,
      ACommaString: string): boolean;
  protected
    FViewID: string;
    FParentID: string;
    FTopParentID: string;
    FWeighting: integer;
    FOverrideCaption: string;
    FShowSQL: boolean;
    FBitmapName: string;
    FDatasetIDCommaText: string; //have a getter and GetFrom (property FViewDataset
    FSubNodesDatasetID: string;
    FViewDataSet: TList;
    FSubNodesDataSet: TViewDataSet;
    FJumps: TList;
    FIDValues: string;
    FDataType: string;
    FElementID : integer;
    FLoadCase : integer;
    FSequence : integer;
    function FindViewDataset(ADatasetID: string): TViewDataSet;
    function GetViewDataSet(AIndex: integer): TViewDataSet;
    function GetViewDataSetCount: integer;
    function GetViewDatasetID(AIndex: integer): string;
    function GetJump(AIndex: integer): TViewDataNode; virtual;
    function FindJump(AViewID: string): TViewDataNode;
    function GetJumpCount: integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure AssignFrom(AViewDataNodeItem: TViewDataNode);
    procedure ClearViewDatasets;
    procedure AddViewDataset(AViewDataset: TViewDataSet);
    function IsViewDataSetPresent(AIDValues: string): boolean;
    procedure AddJump(AJump: TViewDataNode);
    function IsLanguageSwitchable: boolean; virtual;
    property ViewID: string read FViewID write FViewID;
    property ParentID: string read FParentID write FParentID;
    property TopParentID: string read FTopParentID write FTopParentID;
    property Weighting: integer read FWeighting write FWeighting;
    property OverrideCaption: string read FOverrideCaption write FOverrideCaption;
    property ShowSQL: boolean read FShowSQL write FShowSQL;
    property BitmapName: string read FBitmapName write FBitmapName;
    property DatasetIDCommaText: string read FDatasetIDCommaText write FDatasetIDCommaText;
    property SubNodesDatasetID: string read FSubNodesDataSetID write FSubNodesDataSetID;
    property ViewDataSetCount: integer read GetViewDataSetCount;
    property ViewDataSet[AIndex: integer]: TViewDataSet read GetViewDataSet;
    property SubNodesDataSet: TViewDataSet read FSubNodesDataSet write FSubNodesDataSet;
    property JumpCount: integer read GetJumpCount;
    property Jump[AIndex: integer]: TViewDataNode read GetJump;
    property IDValues: string read FIDValues write FIDValues;
    property DataType    : String     read FDataType      write FDataType;
    property ElementID : integer read FElementID write FElementID;
    property LoadCase : integer read FLoadCase write FLoadCase;
    property Sequence : integer read FSequence write FSequence;
  end;

  TViewDataTreeNodeData = class(TObject)
  protected
    FTreeNode: TTreeNode;
    FViewDataNode: TViewDataNode;
    FData: TObject;  // You can assign whatever object to this member for your view.
  public
    constructor Create;
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
    property ViewDataNode: TViewDataNode read FViewDataNode write FViewDataNode;
    property Data: TObject read FData write FData;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TViewDataSet }

constructor TViewDataSet.Create;
const OPNAME = 'TViewDataSet.Create';
begin
  try
    inherited Create;
    Reset;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TViewDataSet.Destroy;
const OPNAME = 'TViewDataSet.Destroy';
begin
  try
    Reset;
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataSet.Reset;
const OPNAME = 'TViewDataSet.Reset';
begin
  try
    FParamValues   := nil;
    FDatasetID     := '';
    FEditable      := True;
    FSQLType       := 0;
    FNoDataMessage := '';
    FViewSQL       := '';
    SetParamCount(0);
    FLoadFromFile   := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataSet.AssignFrom(AViewDataSet: TViewDataSet);
const OPNAME = 'TViewDataSet.AssignFrom';
var LIndex: integer;
begin
  try

    // Discard existing data.
    Reset;

    // Assign the member fields.
    FDatasetID     := AViewDataSet.DatasetID;
    FEditable      := AViewDataSet.Editable;
    FSQLType       := AViewDataSet.SQLType;
    FViewSQL       := AViewDataSet.ViewSQL;
    FNoDataMessage := AViewDataSet.NoDataMessage;

    // Assign the parameters.
    SetParamCount(AViewDataSet.ParamCount);
    for LIndex := 0 to AViewDataSet.ParamCount - 1 do
      if (LIndex < Length(FParamValues)) and (LIndex < Length(AViewDataSet.ParamValues)) then
        FParamNames[LIndex] := AViewDataSet.ParamNames[LIndex];
    for LIndex := 0 to AViewDataSet.ParamCount - 1 do
      if (LIndex < Length(FParamValues)) and (LIndex < Length(AViewDataSet.ParamValues)) then
        FParamValues[LIndex] := AViewDataSet.ParamValues[LIndex];

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataSet.LoadParams(AParamNames, AParamValues: TStringList);
const OPNAME = 'TViewDataSet.LoadParams';
var LIndex: integer;
begin
  try
    SetParamCount(AParamNames.Count);
    for LIndex := 0 to Length(FParamNames) - 1 do
      if (LIndex < AParamNames.Count) then
        FParamNames[LIndex] := AParamNames[LIndex];
    for LIndex := 0 to Length(FParamValues) - 1 do
      if (LIndex < AParamValues.Count) then
        FParamValues[LIndex] := AParamValues[LIndex];

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataSet.LoadParams(AParamNames, AParamValues: TParamList);
const OPNAME = 'TViewDataSet.LoadParams';
var LIndex: integer;
begin
  try
    SetParamCount(Length(AParamNames));
    for LIndex := 0 to Length(FParamNames) - 1 do
      if (LIndex < Length(AParamNames)) then
        FParamNames[LIndex] := AParamNames[LIndex];
    for LIndex := 0 to Length(FParamValues) - 1 do
      if (LIndex < Length(AParamValues)) then
        FParamValues[LIndex] := AParamValues[LIndex];

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataSet.AreParamsEqual(AParamValues: TParamList): boolean;
const OPNAME = 'TViewDataSet.AreParamsEqual';
var LIndex: integer;
begin
  Result := False;
  try
    try
      if (Length(FParamValues) = Length(AParamValues)) then
      begin
        Result := True;
        for LIndex := 0 to Length(FParamValues) - 1 do
          if (FParamValues[LIndex] <> AParamValues[LIndex]) then
            Result := False;
      end;
    except
      Result := False;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataSet.GetParamCount: integer;
const OPNAME = 'TViewDataSet.GetParamCount';
begin
  Result := 0;
  try
    Result := Length(FParamNames);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataSet.SetParamCount(const AValue: integer);
const OPNAME = 'TViewDataSet.SetParamCount';
begin
  try

    // Destroy current arrays.
    SetLength(FParamNames, 0);
    Finalize(FParamNames);
    FParamNames := nil;
    SetLength(FParamValues, 0);
    Finalize(FParamValues);
    FParamValues := nil;

    // Set new length.
    SetLength(FParamNames, AValue);
    SetLength(FParamValues, AValue);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataSet.GetIDValues: string;
const OPNAME = 'TViewDataSet.GetIDValues';
var LIndex: integer;
begin
  Result := '';
  try
    if (Length(FParamValues) > 0) then
    begin
      Result := FParamValues[0];
      for LIndex := 1 to Length(FParamValues) - 1 do
        Result := Result + ',' + FParamValues[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TViewDataNode }

constructor TViewDataNode.Create;
const OPNAME = 'TViewDataNode.Create';
begin
  try
    inherited Create;
    FJumps := TList.Create;
    FViewDataSet := TList.Create;
    Reset;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TViewDataNode.Destroy;
const OPNAME = 'TViewDataNode.Destroy';
begin
  try
    Reset;
    FreeAndNil(FJumps);
    FreeAndNil(FViewDataSet);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataNode.Reset;
const OPNAME = 'TViewDataNode.Reset';
begin
  try
    FViewID            := '';
    FParentID          := '';
    FTopParentID       := '';
    FWeighting         := 0;
    FOverrideCaption   := '';
    FShowSQL           := True;
    FBitmapName        := '';
   // FDatasetID         := '';
    FSubNodesDatasetID := '';
    FViewDataSet.Clear;
    FJumps.Clear;
    FIDValues          := '';
    FDataType          := '';
    FElementID         := 0;
    FLoadCase          := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataNode.AssignFrom(AViewDataNodeItem: TViewDataNode);
const OPNAME = 'TViewDataNode.AssignFrom';
var LIndex: integer;
begin
  try

    // Discard existing data.
    Reset;

    // Assign the member fields.
    FViewID             := AViewDataNodeItem.ViewID;
    FParentID           := AViewDataNodeItem.ParentID;
    FTopParentID        := AViewDataNodeItem.TopParentID;
    FWeighting          := AViewDataNodeItem.Weighting;
    FOverrideCaption    := AViewDataNodeItem.OverrideCaption;
    FShowSQL            := AViewDataNodeItem.ShowSQL;
    FBitmapName         := AViewDataNodeItem.BitmapName;
    FDatasetIDCommaText := AViewDataNodeItem.FDatasetIDCommaText;
    FSubNodesDatasetID  := AViewDataNodeItem.SubNodesDatasetID;
    FSubNodesDataSet    := AViewDataNodeItem.SubNodesDataSet;
    FIDValues           := AViewDataNodeItem.IDValues;

    // These items can be added because niether this object or the passed
    // object owns the jump reference object.
    for LIndex := 0 to AViewDataNodeItem.JumpCount - 1 do
      FJumps.Add(AViewDataNodeItem.Jump[LIndex]);

    for LIndex := 0 to AViewDataNodeItem.ViewDatasetCount - 1 do
      FViewDataset.Add(AViewDataNodeItem.ViewDataSet[LIndex]);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataNode.AddJump(AJump: TViewDataNode);
const OPNAME = 'TViewDataNode.AddJump';
begin
  try
    if Assigned(AJump) then
    begin
      if Assigned(FindJump(AJump.ViewID)) then
        raise Exception.CreateFmt('Node jump from [%s] to [%s] already exists.', [FViewID, AJump.ViewID]);
      FJumps.Add(AJump);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.FindJump(AViewID: string): TViewDataNode;
const OPNAME = 'TViewDataNode.FindJump';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FJumps.Count - 1 do
    begin
      if (TViewDataNode(FJumps[LIndex]).ViewID = AViewID) then
      begin
        Result := TViewDataNode(FJumps[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.GetJump(AIndex: integer): TViewDataNode;
const OPNAME = 'TViewDataNode.GetJump';
begin
  Result := nil;
  try
    if (GetJumpCount > AIndex) then
      Result := TViewDataNode(FJumps[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.GetJumpCount: integer;
const OPNAME = 'TViewDataNode.GetJumpCount';
begin
  Result := 0;
  try
    Result := FJumps.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.FindViewDataset(ADatasetID: string): TViewDataSet;
const OPNAME = 'TViewDataNode.FindViewDataset';
var LDataSetIndex: integer;
begin
  Result := nil;
  try
    for LDataSetIndex := 0 to FViewDataset.Count - 1 do
    begin
      if (TViewDataSet(FViewDataset[LDataSetIndex]).DatasetID = ADatasetID) then
      begin
        Result := TViewDataSet(FViewDataset[LDataSetIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.GetViewDataset(AIndex: integer): TViewDataSet;
const OPNAME = 'TViewDataNode.GetViewDataset';
begin
  Result := nil;
  try
    if (GetViewDatasetCount > AIndex) then
      Result := TViewDataSet(FViewDataset[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.GetViewDatasetCount: integer;
const OPNAME = 'TViewDataNode.GetViewDatasetCount';
begin
  Result := 0;
  try
    Result := FViewDataset.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.GetViewDatasetID(AIndex: integer): string;
const OPNAME = 'TViewDataNode.GetViewDatasetID';
begin
  Result := '';
  try
//    if (AIndex >= 0) and (AIndex < FViewDataset.Count) and
    if Assigned(ViewDataSet[AIndex]) then
      Result := ViewDataSet[AIndex].DatasetID;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TViewDataNode.GetSubNodesDatasetID(AIndex: integer): string;
const OPNAME = 'TViewDataNode.GetSubNodesDatasetID';
begin
  Result := '';
  try
    if (GetViewDatasetCount >= 0) then
      Result := ViewDataSet[0].DatasetID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}
{
procedure TViewDataNode.SetDataSetIDCommaText(ADataSetIDCommaText: string);
const OPNAME = 'TViewDataNode.SetDataSetIDCommaText';
begin
  try
    FDatasetIDCommaText := ADataSetIDCommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
}
procedure TViewDataNode.ClearViewDatasets;
const OPNAME = 'TViewDataNode.ClearViewDatasets';
begin
  try
    FViewDataset.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataNode.AddViewDataset(AViewDataset: TViewDataSet);
const OPNAME = 'TViewDataNode.AddViewDataset';
begin
  try
    if Assigned(AViewDataset) then
    begin
      if Assigned(FindViewDataset(AViewDataset.DatasetID)) then
      begin
        raise Exception.CreateFmt('The View dataset already exists [%s].', [AViewDataset.DatasetID]);
      end else begin
        FViewDataset.Add(AViewDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.IsViewDataSetPresent(AIDValues: string): boolean;
const OPNAME = 'TViewDataNode.IsViewDataSetPresent';
var LDataSetIndex: integer;
begin
  Result := False;
  try
    for LDataSetIndex := 0 to FViewDataset.Count - 1 do
    begin
      if UnaryStringAndCommaListCompare(ViewDataSet[LDataSetIndex].IDValues, AIDValues) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.IsLanguageSwitchable: boolean;
const OPNAME = 'TViewDataNode.IsLanguageSwitchable';
begin
  Result := True;
  try
    Result := (FOverrideCaption = '');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataNode.UnaryStringAndCommaListCompare(const AString,
  ACommaString: string): boolean;
const OPNAME = 'TViewDataNode.UnaryStringAndCommaListCompare';
var
  LIndex : integer;
  LStringList : TStringlist;
begin
  Result := false;
  try
    LStringList := TStringList.Create;
    try
      LStringList.CommaText := ACommaString;
      for LIndex := 0 to LStringList.Count - 1 do
      begin
        if CompareStr(AString, LStringList[Lindex]) = 0 then
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      FreeAndNil(LStringList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TViewDataTreeNodeData }

constructor TViewDataTreeNodeData.Create;
const OPNAME = 'TViewDataTreeNodeData.Create';
begin
  try
    inherited Create;
    FData := nil;
    FTreeNode := nil;
    FViewDataNode := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

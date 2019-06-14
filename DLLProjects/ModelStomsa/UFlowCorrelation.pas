unit UFlowCorrelation;

interface

uses Classes,Contnrs;

type
  array13F       = Array[1..13] of double;
  array7_13F     = Array[1..7,1..13] of double;

  //Storage for flow correlation records
  TFlowCorrelationData = class(TObject)
    FileName : String;     //File name and directory of compared gauge
    RSHO,                  //Historical correlation coefficients
    RMEANO   : Array13F;   //Mean of simulated, 12 months plus annual
    PO       : Array7_13F; //Distribution of correlation coefficients, 12 months plus annual
  end;

  TFlowCorrelation = class(TObject)
  protected
    FDataList  : TObjectList;
    FDataIndex : integer;
    //FAddedNodeIndex : integer;
    function Get_CurrentData: TFlowCorrelationData;
  public
    constructor Create;
    destructor Destroy; override;

    function First : Boolean;
    function Next : Boolean;
    function GetNode(AFileName : String) : boolean;
    function GotoIndex(AIndex : Integer) : Boolean;

    function AddFlowCorrelationNode(AFileName : string) : boolean;
    //procedure SaveCurrentData;
    procedure ClearAllNodes;
    property CurrentData: TFlowCorrelationData read Get_CurrentData;
  end;//TFlowCorrelation

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

constructor TFlowCorrelation.Create;
const OPNAME = 'TFlowCorrelation.Create';
begin
  inherited Create;
  try
    FDataIndex      := -1;
    //FAddedNodeIndex := -1;
    FDataList       := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TFlowCorrelation.Destroy;
const OPNAME = 'TFlowCorrelation.Destroy';
begin
  try
    FreeAndNil(FDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited Destroy;
end;

{procedure TFlowCorrelation.SaveCurrentData;
const OPNAME = 'TFlowCorrelation.SaveCurrentData';
begin
  try
    if(FAddedNodeIndex >= 0) then
      FDataIndex := FAddedNodeIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TFlowCorrelation.First : Boolean;
const OPNAME = 'TFlowCorrelation.First';
begin
  Result := false;
  try
    FDataIndex := -1;
    if (FDataList.Count > 0) then
    begin
      FDataIndex := 0;
      Result     := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowCorrelation.Next : Boolean;
const OPNAME = 'TFlowCorrelation.Next';
begin
  Result := false;
  try
    if (FDataList.Count > 0) and ((FDataIndex + 1) < FDataList.Count) then
    begin
      FDataIndex := FDataIndex + 1;
      Result     := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowCorrelation.GotoIndex(AIndex : Integer) : Boolean;
const OPNAME = 'TFlowCorrelation.GotoIndex';
begin
  Result := false;
  try
    AIndex := AIndex -1;
    if(AIndex >= 0) and (AIndex < FDataList.Count) then
    begin
      FDataIndex := AIndex;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowCorrelation.GetNode(AFileName : String) : boolean;
const OPNAME = 'TFlowCorrelation.GetNode';
var
  LIndex : integer;
  LData  : TFlowCorrelationData;
begin
  Result := false;
  try
    AFileName := Uppercase(AFileName);
    for LIndex := 0 to FDataList.Count-1 do
    begin
      LData  := TFlowCorrelationData(FDataList.Items[LIndex]);
      if(Uppercase(LData.FileName) = AFileName) then
      begin
        FDataIndex := LIndex;
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowCorrelation.AddFlowCorrelationNode(AFileName : string) : boolean;
const OPNAME = 'TFlowCorrelation.AddFlowCorrelationNode';
var
  LFlowCorrelation : TFlowCorrelationData;
begin
  //Check to see if node already exists, if not append to the end.
  Result := true;
  try
    if not GetNode(AFileName) then
    begin
      //No match found, create a new node and append
      LFlowCorrelation := TFlowCorrelationData.Create;
      LFlowCorrelation.FileName := AFileName;
      //FAddedNodeIndex := FDataList.Add(LFlowCorrelation);
      FDataIndex      := FDataList.Add(LFlowCorrelation);
    end;//if TempFlowCorrelation
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowCorrelation.ClearAllNodes;
const OPNAME = 'TFlowCorrelation.ClearAllNodes';
begin
  try
    FDataList.Clear;
    FDataIndex := -1;
    //FAddedNodeIndex := -1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFlowCorrelation.Get_CurrentData: TFlowCorrelationData;
const OPNAME = 'TFlowCorrelation.Get_CurrentData';
begin
  Result := nil;
  try
    if(FDataIndex >= 0) and (FDataIndex < FDataList.Count) then
    begin
      Result := TFlowCorrelationData(FDataList.Items[FDataIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

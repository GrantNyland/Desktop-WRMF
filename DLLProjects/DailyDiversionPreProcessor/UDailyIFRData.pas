unit UDailyIFRData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VCL.Controls,
  VoaimsCom_TLB,
  UChannelData,
  UIFRFeatures,
  UAbstractObject;
type
  TDailyIFRData = class(TAbstractAppObject)
  protected
    FChannelList : TChannelList;
    FIFRFeatureList : TIFRFeatureList;
    FStationNo : WideString;
    FDailyIFRList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CastChannelList : TChannelList;
    function CastIFRFeatureList : TIFRFeatureList;
    function Get_StationNo: WideString;
    procedure Set_StationNo(const Value: WideString);
    function Get_DailyIFRList: TStringList;
  public
    function Initialise: Boolean; override;
    property  ChannelList : TChannelList read CastChannelList;
    property IFRFeatureList : TIFRFeatureList read CastIFRFeatureList;
    property StationNo : WideString read Get_StationNo write Set_StationNo;
    property DailyIFRList : TStringList read Get_DailyIFRList;
end;

  TDailyIFRDataList = class(TAbstractAppObject)
  protected
    FDailyIFRDataList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDailyIFRDataCount : integer;
  public
    function Initialise: Boolean; override;
    function AddDailyIFRData(AStationNo : string) : TDailyIFRData;
    function RemoveDailyIFRDataByStationNo(AStationNo : string) : Boolean;
    function GetDailyIFRDataByStationNo(AStationNo : string) : TDailyIFRData;
    property DailyIFRDataCount : integer read GetDailyIFRDataCount;

end;

implementation
uses
  VCL.Dialogs,
  Math,
  UConstants,
  UErrorHandlingOperations;


function TDailyIFRData.CastChannelList: TChannelList;
const OPNAME = 'TDailyIFRData.CastChannelList';
begin
  Result := nil;
  try
    Result := FChannelList;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRData.CastIFRFeatureList : TIFRFeatureList;
const OPNAME = 'TDailyIFRData.CastIFRFeatureList';
begin
  Result := nil;
  try
    Result := FIFRFeatureList;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRData.CreateMemberObjects;
const OPNAME = 'TDailyIFRData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChannelList := TChannelList.Create(FAppModules);
    FIFRFeatureList := TIFRFeatureList.Create(FAppModules);
    FDailyIFRList := TStringList.Create;
    FDailyIFRList.Sorted := True;
    FDailyIFRList.Duplicates := dupError;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRData.DestroyMemberObjects;
const OPNAME = 'TDailyIFRData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChannelList);
    FreeAndNil(FIFRFeatureList);
    FreeAndNil(FDailyIFRList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRData.Get_DailyIFRList: TStringList;
const OPNAME = 'TDailyIFRData.Get_DailyIFRList';
begin
  Result := nil;
  try
    Result := FDailyIFRList;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRData.Get_StationNo: WideString;
const OPNAME = 'TDailyIFRData.Get_StationNo';
begin
  Result := '';
  try
    Result := FStationNo;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRData.Initialise: Boolean;
const OPNAME = 'TDailyIFRData.Initialise';
begin
  Result := False;
  try
    Result := FChannelList.Initialise;
    Result := Result and FIFRFeatureList.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRData.Set_StationNo(const Value: WideString);
const OPNAME = 'TDailyIFRData.set_StationNo';
begin
  try
    FStationNo := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRData._AddRef: Integer;
const OPNAME = 'TDailyIFRData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyIFRData._Release: Integer;
const OPNAME = 'TDailyIFRData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDailyIFRDataList }

function TDailyIFRDataList.AddDailyIFRData(AStationNo: string): TDailyIFRData;
const OPNAME = 'TDailyIFRDataList.AddDailyIFRData';
var
  LDailyIFRData : TDailyIFRData;
begin
  Result := nil;
  try
    LDailyIFRData := TDailyIFRData.Create(FAppModules);
    FDailyIFRDataList.Add(LDailyIFRData);
    LDailyIFRData.StationNo := AStationNo;
    Result := LDailyIFRData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyIFRDataList.Initialise: Boolean;
const OPNAME = 'TDailyIFRDataList.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FDailyIFRDataList.Count-1 do
      TDailyIFRData(FDailyIFRDataList.Items[LIndex]).Initialise;
    FDailyIFRDataList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataList.CreateMemberObjects;
const OPNAME = 'TDailyIFRDataList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDailyIFRDataList := TObjectList.Create(False);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataList.DestroyMemberObjects;
const OPNAME = 'TDailyIFRDataList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDailyIFRDataList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataList.GetDailyIFRDataByStationNo(AStationNo : string): TDailyIFRData;
const OPNAME = 'TDailyIFRDataList.GetDailyIFRDataByStationNo';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDailyIFRDataList.Count-1 do
    begin
      if UpperCase(TDailyIFRData(FDailyIFRDataList.Items[LIndex]).FStationNo) = UpperCase(AStationNo) then
      begin
        Result := TDailyIFRData(FDailyIFRDataList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataList.RemoveDailyIFRDataByStationNo(AStationNo: string): Boolean;
const OPNAME = 'TDailyIFRDataList.RemoveDailyIFRDataByStationNo';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataList.GetDailyIFRDataCount : integer;
const OPNAME = 'TDailyIFRDataList.GetDailyIFRDataCount';
begin
  Result := 0;
  try
    Result := FDailyIFRDataList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

{******************************************************************************}
{*  UNIT      : Contains the class TRainGauge.                                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UGauge;

interface

uses
  Contnrs,
  Classes,
  UAbstractObject,
  RainfallCom_TLB;

type
  TRainGauge = class(TAbstractAppObject, IRainGauge)
  private
    FGroup        : string;
    FGaugeNumber  : string;
    FGaugeName    : string;
    FGaugeID      : integer;
    FLatitude     : integer;
    FLongitude    : integer;
    FIsSelected   : boolean;
    FListIndex    : integer;
    FIsInWR90     : boolean;
  protected
    procedure CreateMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Get_Group: WideString; safecall;
    function Get_GaugeNumber: WideString; safecall;
    function Get_GaugeName: WideString; safecall;
    function Get_GaugeID: Integer; safecall;
    function Get_Latitude: Integer; safecall;
    function Get_Longitude: Integer; safecall;
    function Get_Selected: WordBool; safecall;
    procedure Set_Selected(Value: WordBool); safecall;
    function Get_IsInWR90: WordBool; safecall;
    function Get_ListIndex: Integer; safecall;

    property Group: WideString read Get_Group;
    property GaugeNumber: WideString read Get_GaugeNumber;
    property GaugeName: WideString read Get_GaugeName;
    property GaugeID: Integer read Get_GaugeID;
    property Latitude: Integer read Get_Latitude;
    property Longitude: Integer read Get_Longitude;
    property Selected: WordBool read Get_Selected write Set_Selected;
    property IsInWR90: WordBool read Get_IsInWR90;
    property ListIndex: Integer read Get_ListIndex;

    function Populate (AIndex     : integer;
                       AName      : string;
                       AGaugeID   : integer;
                       ANumber    : string;
                       AGroup     : string;
                       ALatitude  : integer;
                       ALongitude : integer;
                       AInWR90    : boolean) : boolean;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TRainGauge.CreateMemberObjects;
const OPNAME = 'TRainGauge.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGroup       := '';
    FGaugeNumber := '';
    FLatitude    := 0;
    FLongitude   := 0;
    FIsSelected  := FALSE;
    FListIndex   := 0;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge._AddRef: Integer;
const OPNAME = 'TRainGauge._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainGauge._Release: Integer;
const OPNAME = 'TRainGauge._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainGauge.Get_Selected : WordBool;
const OPNAME = 'TRainGauge.Get_Selected';
begin
  Result := false;
  try
    Result := FIsSelected;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_IsInWR90 : WordBool;
const OPNAME = 'TRainGauge.Get_IsInWR90';
begin
  Result := false;
  try
    Result := FIsInWR90;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainGauge.Set_Selected (Value : WordBool);
const OPNAME = 'TRainGauge.Set_Selected';
var
  lStation : IStationData;
begin
  try
    if (Value = FALSE) then
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                    GetStationDataByID(FGaugeID);
      if ((lStation = nil) OR ((lStation <> nil) AND (lStation.MayBeDeleted))) then
      begin
        FIsSelected := Value;
      end
    end
    else
    begin
      Self.FIsSelected := Value;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Populate (AIndex     : integer;
                              AName      : string;
                              AGaugeID   : integer;
                              ANumber    : string;
                              AGroup     : string;
                              ALatitude  : integer;
                              ALongitude : integer;
                              AInWR90    : boolean) : boolean;
const OPNAME = 'TRainGauge.Populate';
var
  lFirstChar : char;
begin
  Result := FALSE;
  try
    FListIndex   := AIndex;
    FGaugeName   := AName;
    FGaugeID     := AGaugeID;
    FGaugeNumber := ANumber;
    if (AGaugeID > 100000) then
      FGroup   := AGroup
    else
    begin
      lFirstChar := AGroup[1];
      if CharInSet(lFirstChar,['0','1','2','3','4','5','6','7','8','9']) then
        FGroup := Copy(AGroup,1,4)
      else
        FGroup := Copy(AGroup,1,1);
    end;
    FLatitude    := ALatitude;
    FLongitude   := ALongitude;
    FIsInWR90    := AInWR90;
    Result       := TRUE;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_GaugeNumber : WideString;
const OPNAME = 'TRainGauge.Get_GaugeNumber';
begin
  Result := '';
  try
    Result := FGaugeNumber;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_Group : WideString;
const OPNAME = 'TRainGauge.Get_Group';
begin
  Result := '';
  try
    Result := FGroup;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_Latitude : integer;
const OPNAME = 'TRainGauge.Get_Latitude';
begin
  Result := 0;
  try
    Result := FLatitude;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_ListIndex : integer;
const OPNAME = 'TRainGauge.Get_ListIndex';
begin
  Result := 0;
  try
    Result := FListIndex;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_Longitude : integer;
const OPNAME = 'TRainGauge.Get_Longitude';
begin
  Result := 0;
  try
    Result := FLongitude;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_GaugeName : WideString;
const OPNAME = 'TRainGauge.Get_GaugeName';
begin
  Result := '';
  try
    Result := FGaugeName;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainGauge.Get_GaugeID : Integer;
const OPNAME = 'TRainGauge.Get_GaugeID';
begin
  Result := 0;
  try
    Result := FGaugeID;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.

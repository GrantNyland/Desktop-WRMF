//
//
//  UNIT      : Contains Channel & Channel Penalty Classes
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 27/08/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UNetworkVisualiserData;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

  TDrawing = class(TAbstractAppObject)
  protected
    FDrawingGroupID : integer;
    FDrawingID : integer;
    FDrawingName : string;
    FGISMode: boolean;
    procedure Set_DrawingName (const AName : WideString); safecall;
    function Get_DrawingName : WideString; safecall;
    function Get_DrawingID : integer; safecall;
    function Get_DrawingGroupID : integer; safecall;
  public
    function Initialise: Boolean; override;
    function Populate (ADrawingGroupID,ADrawingID     : integer;
                       ADrawingName   : WideString; AGISMode :boolean) : WordBool;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    property DrawingName : WideString read Get_DrawingName write Set_DrawingName;
    property DrawingID : integer read Get_DrawingID;
    property DrawingGroupID : integer read Get_DrawingGroupID;
    property GISMode : boolean read FGISMode write FGISMode;
  end;

  TDrawingList = class(TAbstractAppObject)
  protected
    FDrawingList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_DrawingByID(AIdentifier: integer): TDrawing;
    function Get_DrawingByName(const AName: WideString): TDrawing; safecall;
    function Get_DrawingByIndex(AIndex: integer): TDrawing; safecall;
    function Get_DrawingCount: integer; safecall;
  public
    function Initialise: Boolean; override;
    function Validate (var AErrors : WideString; const AContext: WideString='') : WordBool; safecall;
    function CreateDrawing(const AName: WideString): TDrawing;
    function DeleteDrawing(const AName: WideString): WordBool;

    property DrawingByID[AIdentifier: integer]: TDrawing read Get_DrawingByID;
    property DrawingByIndex[AIndex: integer]: TDrawing read Get_DrawingByIndex;
    property DrawingByName[const AName: WideString]: TDrawing read Get_DrawingByName;
    property DrawingCount: integer read Get_DrawingCount;
  end;

  TDrawingGroup = class(TAbstractAppObject)
  protected
    FDrawingGroupID   : integer;
    FDrawingGroupName : string;
    FDrawingList      : TDrawingList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Set_DrawingGroupName (const AName : WideString); safecall;
    function Get_DrawingGroupName : WideString; safecall;
    function Get_DrawingGroupID : integer; safecall;
    function Get_DrawingList: TDrawingList; safecall;
  public
    function Initialise: Boolean; override;
    function Populate (ADrawingGroupID     : integer;ADrawingGroupName   : WideString) : WordBool;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    property DrawingGroupName : WideString read Get_DrawingGroupName write Set_DrawingGroupName;
    property DrawingList : TDrawingList read Get_DrawingList;
    property DrawingGroupID : integer read Get_DrawingGroupID;
  end;

  TDrawingGroupList = class(TAbstractAppObject)
  protected
    FDrawingGroupList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_DrawingGroupByID(AIdentifier: integer): TDrawingGroup;
    function Get_DrawingGroupByName(const AName: WideString): TDrawingGroup; safecall;
    function Get_DrawingGroupByIndex(AIndex: integer): TDrawingGroup; safecall;
    function Get_DrawingGroupCount: integer; safecall;
  public
    function Initialise: Boolean; override;
    function CreateDrawingGroup(const AName: WideString) : TDrawingGroup; safecall;
    function DeleteDrawingGroup(const AName: WideString) : WordBool;safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;

    property DrawingGroupByID[AIdentifier: integer]: TDrawingGroup read Get_DrawingGroupByID;
    property DrawingGroupByIndex[AIndex: integer]: TDrawingGroup read Get_DrawingGroupByIndex;
    property DrawingGroupByName[const AName: WideString]: TDrawingGroup read Get_DrawingGroupByName;
    property DrawingGroupCount: integer read Get_DrawingGroupCount;
  end;

  TDrawingData = class(TAbstractAppObject)
  protected
    FDrawingGroupList : TDrawingGroupList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Get_DrawingGroupList: TDrawingGroupList; safecall;
    function Initialise: Boolean; override;
    function DrawingGroupExist(AGroupName: string): boolean;
    function DrawingExist(AGroupName, ADrawingName: string): boolean;
    function Validate (var AErrors : WideString; const AContext : WideString='') : WordBool; safecall;
    property DrawingGroupList : TDrawingGroupList read Get_DrawingGroupList;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  SysUtils,
  UConstants,
  UErrorHandlingOperations;

{ TDrawing }

function TDrawing.Get_DrawingName: WideString;
const OPNAME = 'TDrawing.Get_DrawingName';
begin
  Result := '';
  try
    Result    := FDrawingName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawing.Initialise: Boolean;
const OPNAME = 'TDrawing.Initialise';
begin
  Result := inherited Initialise;
  try
    FDrawingGroupID := NullInteger;
    FDrawingID      := NullInteger;
    FDrawingName    := '';
    FGISMode        := False;
    Result          := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawing.Populate(ADrawingGroupID,ADrawingID: integer; ADrawingName: WideString; AGISMode : boolean): WordBool;
const OPNAME = 'TDrawing.Populate';
begin
  Result := False;
  try
    FDrawingGroupID := ADrawingGroupID;
    FDrawingID      := ADrawingID;
    FDrawingName    := ADrawingName;
    FGISMode        := AGISMode;
    Result          := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawing.Set_DrawingName(const AName: WideString);
const OPNAME = 'TDrawing.Set_DrawingName';
begin
  try
    FDrawingName := AName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawing.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDrawing.Validate';
begin
  Result := False;
  try
    Result       := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawing.Get_DrawingID: integer;
const OPNAME = 'TDrawing.Get_DrawingID';
begin
  Result := NullInteger;
  try
    Result := FDrawingID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawing.Get_DrawingGroupID: integer;
const OPNAME = 'TDrawing.Get_DrawingGroupID';
begin
  Result := NullInteger;
  try
    Result := FDrawingGroupID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDrawingList }

function TDrawingList.CreateDrawing(const AName: WideString): TDrawing;
const OPNAME = 'TDrawingList.CreateDrawing';
var
  LDrawing: TDrawing;
begin
  Result := nil;
  try
    Result := Get_DrawingByName(AName);
    if (Result = nil) then
    begin
      LDrawing := TDrawing.Create(FAppModules);
      LDrawing.Populate(NullInteger,NullInteger,AName,False);
      FDrawingList.Add(LDrawing);
      Result    := LDrawing;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.DeleteDrawing(const AName: WideString): WordBool;
const OPNAME = 'TDrawingList.DeleteDrawing';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FDrawingList.Count -1 do
    begin
      if(TDrawing(FDrawingList.Items[LIndex]).FDrawingName = AName) then
      begin
        FDrawingList.Remove(TDrawing(FDrawingList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingList.CreateMemberObjects;
const OPNAME = 'TDrawingList.CreateMemberObjects';
begin
  inherited;
  try
    FDrawingList := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingList.DestroyMemberObjects;
const OPNAME = 'TDrawingList.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDrawingList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Get_DrawingByIndex(AIndex: integer): TDrawing;
const OPNAME = 'TDrawingList.Get_DrawingByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDrawingList.Count) then
      Result := TDrawing(FDrawingList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Get_DrawingByName(const AName: WideString): TDrawing;
const OPNAME = 'TDrawingList.Get_DrawingByName';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDrawingList.Count -1 do
    begin
      if(TDrawing(FDrawingList.Items[LIndex]).FDrawingName = AName) then
      begin
        Result := TDrawing(FDrawingList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Get_DrawingCount: integer;
const OPNAME = 'TDrawingList.Get_DrawingCount';
begin
  Result := 0;
  try
    Result := FDrawingList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Get_DrawingByID(AIdentifier: integer): TDrawing;
const OPNAME = 'TDrawingList.Get_DrawingByID';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDrawingList.Count -1 do
    begin
      if(TDrawing(FDrawingList.Items[LIndex]).FDrawingID = AIdentifier) then
      begin
        Result := TDrawing(FDrawingList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Initialise: Boolean;
const OPNAME = 'TDrawingList.Initialise';
begin
  Result := inherited Initialise;
  try
    FDrawingList.Clear;
    Result       := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDrawingList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to FDrawingList.Count -1 do
    begin
      if not TDrawing(FDrawingList.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
     if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
       Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDrawingGroup }

procedure TDrawingGroup.CreateMemberObjects;
const OPNAME = 'TDrawingGroup.CreateMemberObjects';
begin
  inherited;
  try
    FDrawingList := TDrawingList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingGroup.DestroyMemberObjects;
const OPNAME = 'TDrawingGroup.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDrawingList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Get_DrawingGroupName: WideString;
const OPNAME = 'TDrawingGroup.Get_DrawingGroupName';
begin
  Result := '';
  try
    Result    := FDrawingGroupName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Get_DrawingList: TDrawingList;
const OPNAME = 'TDrawingGroup.Get_DrawingList';
begin
  Result := nil;
  try
    Result    := FDrawingList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Initialise: Boolean;
const OPNAME = 'TDrawingGroup.Initialise';
begin
  Result := inherited Initialise;
  try
    FDrawingGroupID := NullInteger;
    FDrawingGroupName := '';
    FDrawingList.Initialise;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Populate(ADrawingGroupID: integer;ADrawingGroupName: WideString): WordBool;
const OPNAME = 'TDrawingGroup.Populate';
begin
  Result := False;
  try
    FDrawingGroupID := ADrawingGroupID;
    FDrawingGroupName := ADrawingGroupName;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingGroup.Set_DrawingGroupName(const AName: WideString);
const OPNAME = 'TDrawingGroup.Set_DrawingGroupName';
begin
  try
    FDrawingGroupName := AName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDrawingGroup.Validate';
begin
  Result := False;
  try
    Result       := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroup.Get_DrawingGroupID: integer;
const OPNAME = 'TDrawingGroup.Get_DrawingGroupID';
begin
  Result := NullInteger;
  try
    Result := FDrawingGroupID;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDrawingGroupList }

function TDrawingGroupList.CreateDrawingGroup(const AName: WideString): TDrawingGroup;
const OPNAME = 'TDrawingGroupList.CreateDrawingGroup';
var
  LDrawingGroup: TDrawingGroup;
begin
  Result := nil;
  try
    Result := Get_DrawingGroupByName(AName);
    if (Result = nil) then
    begin
      LDrawingGroup := TDrawingGroup.Create(FAppModules);
      LDrawingGroup.Populate(NullInteger,AName);
      FDrawingGroupList.Add(LDrawingGroup);
      Result    := LDrawingGroup;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingGroupList.CreateMemberObjects;
const OPNAME = 'TDrawingGroupList.CreateMemberObjects';
begin
  inherited;
  try
    FDrawingGroupList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.DeleteDrawingGroup(const AName: WideString): WordBool;
const OPNAME = 'TDrawingGroupList.DeleteDrawingGroup';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FDrawingGroupList.Count -1 do
    begin
      if(TDrawingGroup(FDrawingGroupList.Items[LIndex]).FDrawingGroupName = AName) then
      begin
        FDrawingGroupList.Remove(TDrawingGroup(FDrawingGroupList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingGroupList.DestroyMemberObjects;
const OPNAME = 'TDrawingGroupList.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDrawingGroupList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Get_DrawingGroupByIndex(AIndex: integer): TDrawingGroup;
const OPNAME = 'TDrawingGroupList.Get_DrawingGroupByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDrawingGroupList.Count) then
      Result := TDrawingGroup(FDrawingGroupList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Get_DrawingGroupByName(const AName: WideString): TDrawingGroup;
const OPNAME = 'TDrawingGroupList.Get_DrawingGroupByName';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDrawingGroupList.Count -1 do
    begin
      if(TDrawingGroup(FDrawingGroupList.Items[LIndex]).FDrawingGroupName = AName) then
      begin
        Result := TDrawingGroup(FDrawingGroupList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Get_DrawingGroupByID(AIdentifier: integer): TDrawingGroup;
const OPNAME = 'TDrawingGroupList.Get_DrawingGroupByID';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDrawingGroupList.Count -1 do
    begin
      if(TDrawingGroup(FDrawingGroupList.Items[LIndex]).FDrawingGroupID = AIdentifier) then
      begin
        Result := TDrawingGroup(FDrawingGroupList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Initialise: Boolean;
const OPNAME = 'TDrawingGroupList.Initialise';
begin
  Result := inherited Initialise;
  try
    FDrawingGroupList.Clear;
    Result       := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TDrawingGroupList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to FDrawingGroupList.Count -1 do
    begin
      if not TDrawingGroup(FDrawingGroupList.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
     if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
       Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingGroupList.Get_DrawingGroupCount: integer;
const OPNAME = 'TDrawingGroupList.Get_DrawingGroupCount';
begin
  Result := 0;
  try
    Result := FDrawingGroupList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDrawingData }

procedure TDrawingData.CreateMemberObjects;
const OPNAME = 'TDrawingData.CreateMemberObjects';
begin
  inherited;
  try
    FDrawingGroupList := TDrawingGroupList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawingData.DestroyMemberObjects;
const OPNAME = 'TDrawingData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDrawingGroupList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingData.Get_DrawingGroupList: TDrawingGroupList;
const OPNAME = 'TDrawingData.Get_DrawingGroupList';
begin
  Result := nil;
  try
    Result := FDrawingGroupList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingData.Initialise: Boolean;
const OPNAME = 'TDrawingData.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FDrawingGroupList.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingData.Validate (var AErrors    : WideString;
                                const AContext : WideString): WordBool;
const OPNAME = 'TDrawingData.Validate';
begin
  Result := False;
  try
    Result := FDrawingGroupList.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingData.DrawingExist(AGroupName,ADrawingName: string): boolean;
const OPNAME = 'TDrawingData.DrawingExist';
var
  LDrawingGroup:TDrawingGroup;
begin
  Result := False;
  try
    LDrawingGroup := FDrawingGroupList.Get_DrawingGroupByName(AGroupName);
    if(LDrawingGroup <> nil) then
      Result := (LDrawingGroup.DrawingList.Get_DrawingByName(ADrawingName) <> nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawingData.DrawingGroupExist(AGroupName: string): boolean;
const OPNAME = 'TDrawingData.DrawingGroupExist';
begin
  Result := False;
  try
    Result := (FDrawingGroupList.Get_DrawingGroupByName(AGroupName) <> nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

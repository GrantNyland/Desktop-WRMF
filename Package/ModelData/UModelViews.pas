unit UModelViews;

interface

uses
  Classes,
  Contnrs,
  UStringListOfStringLists,
  UAbstractObject;

type
  TModelViewItem  = class(TAbstractAppObject)
  protected
    FGroupName    : string;
    FViewItemName : string;
    FItemsCount   : integer;
    FVisible      : boolean;
    function GetCommaText: string;
  public
    function Initialise: boolean; override;
    function Populate(ACommaText: string): boolean;
    property GroupName    : string  read FGroupName;
    property ViewItemName : string  read FViewItemName;
    property ItemsCount   : integer read FItemsCount;
    property Visible      : boolean read FVisible;
    property CommaText    : string  read GetCommaText;
  end;

  TModelViews = class(TAbstractAppObject)
  protected
    FViewItemsList: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AddViewItem(ACommaText : string);
    function  GetItemByIndex(AIndex: integer):TModelViewItem;
  public
    function Initialise: boolean; override;
    function ItemsCount: integer;
    function GetModelViewItems(AItems: TStringListOfStringLists): boolean;
    function ViewItemExists(AGroupName,AItemName :string): boolean;
    property ItemByIndex[AIndex: integer]: TModelViewItem  read GetItemByIndex;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TModelViewItem }

function TModelViewItem.GetCommaText: string;
const OPNAME = 'TModelViewItem.GetCommaText';
var
  LContainer: TStringList;
begin
  Result := '';
  try
    LContainer := TStringList.Create;
    try
      LContainer.Add(FGroupName);
      LContainer.Add(FViewItemName);
      LContainer.Add(IntToStr(FItemsCount));
      if FVisible then
        LContainer.Add('1')
      else
        LContainer.Add('0');
      Result := LContainer.CommaText;
    finally
      LContainer.Free;
    end;
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViewItem.Initialise: boolean;
const OPNAME = 'TModelViewItem.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroupName    := '';
    FViewItemName := '';
    FItemsCount   := 0;
    FVisible      := False;
    Result        := True;
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViewItem.Populate(ACommaText: string): boolean;
const OPNAME = 'TModelViewItem.Populate';
var
  LContainer: TStringList;
begin
  Result := False;
  try
    if(Trim(ACommaText) <> '') then
    begin
      LContainer := TStringList.Create;
      try
        LContainer.CommaText := ACommaText;
        if(LContainer.Count > 0) then
          FGroupName    := LContainer[0];
        if(LContainer.Count > 1) then
          FViewItemName    := LContainer[1];
        if(LContainer.Count > 2) then
          FItemsCount    := StrToInt(LContainer[2]);
        if(LContainer.Count > 3) then
          FVisible    := StrToBool(LContainer[3]);
        Result := True;
      finally
        LContainer.Free;
      end;
    end;
  except on E: exception do HandleError(E, OPNAME); end;
end;

{ TModelViews }

procedure TModelViews.CreateMemberObjects;
const OPNAME = 'TModelViews.CreateMemberObjects';
begin
  inherited;
  try
    FViewItemsList := TObjectList.Create(True);
  except on E: exception do HandleError(E, OPNAME); end;
end;

procedure TModelViews.DestroyMemberObjects;
const OPNAME = 'TModelViews.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FViewItemsList);
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViews.GetModelViewItems(AItems: TStringListOfStringLists): boolean;
const OPNAME = 'TModelViews.GetModelViewItems';
var
  LIndex: integer;
  LViewItem: TModelViewItem;
begin
  Result := False;
  try
    AItems.Clear;
    for LIndex := 0 to FViewItemsList.Count -1 do
    begin
      LViewItem := ItemByIndex[LIndex];
      if(LViewItem <> nil) then
        AItems.Add(LViewItem.GroupName,LViewItem.CommaText);
    end;
    Result := True;
  except on E: exception do HandleError(E, OPNAME); end;
end;

procedure TModelViews.AddViewItem(ACommaText: string);
const OPNAME = 'TModelViews.AddViewItem';
var
  LContainer: TStringList;
  LViewItem: TModelViewItem;
begin
  try
    LContainer := TStringList.Create;
    try
      LContainer.CommaText := ACommaText;
      if(LContainer.Count >= 4) then
      begin
        if not ViewItemExists(LContainer[0],LContainer[1]) then
        begin
          LViewItem := TModelViewItem.Create(FAppModules);
          if LViewItem.Populate(ACommaText) then
            FViewItemsList.Add(LViewItem)
          else
            FreeAndNil(LViewItem);
        end;
      end;
    finally
      LContainer.Free;
    end;
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViews.GetItemByIndex(AIndex: integer): TModelViewItem;
const OPNAME = 'TModelViews.GetItemByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FViewItemsList.Count) then
      Result := TModelViewItem(FViewItemsList[AIndex]);
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViews.ItemsCount: integer;
const OPNAME = 'TModelViews.ItemsCount';
begin
  Result := 0;
  try
    Result := FViewItemsList.Count;
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViews.ViewItemExists(AGroupName, AItemName :string): boolean;
const OPNAME = 'TModelViews.ViewItemExists';
var
  LIndex: integer;
  LViewItem: TModelViewItem;
begin
  Result := False;
  try
    for LIndex := 0 to FViewItemsList.Count -1 do
    begin
      LViewItem := ItemByIndex[LIndex];
      if(LViewItem <> nil) then
      begin
        if(LViewItem.GroupName = AGroupName) and (LViewItem.ViewItemName = AItemName) then
          Exit;
      end;
    end;
    Result := True;
  except on E: exception do HandleError(E, OPNAME); end;
end;

function TModelViews.Initialise: boolean;
const OPNAME = 'TModelViews.Initialise';
begin
  Result := False;
  try
    FViewItemsList.Clear;
    AddViewItem(FAppModules.Language.GetString('ViewData.Reservoir') + ','  + FAppModules.Language.GetString('ViewData.Properties') + ',' + ',0,1');
    AddViewItem(FAppModules.Language.GetString('ViewData.Reservoir') + ','  + FAppModules.Language.GetString('ViewData.Evaporation') + ',' + ',1,1');
    AddViewItem(FAppModules.Language.GetString('ViewData.Reservoir') + ','  + FAppModules.Language.GetString('ViewData.PhysicalCharacteristics') + ',' + ',2,1');
    AddViewItem(FAppModules.Language.GetString('ViewData.Reservoir') + ','  + FAppModules.Language.GetString('ViewData.CatchmentHydrology') + ',' + ',3,1');
    AddViewItem(FAppModules.Language.GetString('ViewData.Reservoir') + ','  + FAppModules.Language.GetString('ViewData.Elevations') + ',' + ',4,1');
{    AddViewItem('AReservoir,PhysicalCharacteristics,2,1');
    AddViewItem('AReservoir,Catchment Hydrlogy,3,1');
    AddViewItem('AReservoir,Penalty Structures,4,1');
    AddViewItem('AReservoir,Zone Elevations,5,1');

    AddViewItem('Nodes,Node with Inflow,0,1');
    AddViewItem('Nodes,Node without Inflow,2,1');

    AddViewItem('Channels,General Flow,0,1');
    AddViewItem('Channels,Diversion,1,1');
    AddViewItem('Channels,Physical Flow,2,1');
    AddViewItem('Channels,Minimum Flow,4,1');
    AddViewItem('Channels,Loss,4,1');
    AddViewItem('Channels,MinMax,5,1');
    AddViewItem('Channels,IFR,6,1');
    AddViewItem('Channels,Pumping,7,1');
    AddViewItem('Channels,Specified Inflow,8,1');
    AddViewItem('Channels,Specified Demand,9,1');
    AddViewItem('Channels,Water Demand,10,1');

    AddViewItem('Network Features,Power Plants,0,1');
    AddViewItem('Network Features,Irrigation,1,1');

    AddViewItem('Allocation Control,Allocation Control,0,1'); }

    Result := True;
  except on E: exception do HandleError(E, OPNAME); end;
end;

end.

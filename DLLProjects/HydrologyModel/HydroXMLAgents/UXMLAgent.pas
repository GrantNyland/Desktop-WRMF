unit UXMLAgent;

interface

uses
  Classes,
  HydrologyCom_TLB,
  XMLDoc, XMLIntf, xmldom,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox;

type
  TValidationContext = (tcChange, tcReset, tcApply, tcCancel);

  TXMLAgent = class(TObject)
  protected
    FAllProperties   : TStringList;
    FAllDescriptions : TStringList;
    procedure AddXMLAllNetworkRoutes (ARootNode : IXMLNode);
    procedure AddXMLAllOutflowRoutes (ARootNode : IXMLNode; ASourceModuleID : Integer);
    procedure AddXMLAllInflowRoutes (ARootNode : IXMLNode; ASinkModuleID : Integer);
    procedure AddXMLAllNetworkModules (ARootNode : IXMLNode);
    procedure AddXMLAllRunOffModules (ARootNode : IXMLNode);
  public
    FHydrologyModel   : IHydrologyModel;
    DlgXMLDocumentIn  : IXMLDocument;
    DlgXMLDocumentOut : IXMLDocument;
    constructor Create;
    destructor Destroy; override;
    function CreateXMLDocument (AXMLText : String) : IXMLDocument;
    function GetXSDSchemaNode (Doc : IXMLDocument): IXMLNode;
    function GetXSDType (ANode : IXMLNode): String;
    function GetXSDNillable (ANode : IXMLNode): Boolean;
    function GetXSDRestriction (ANode                 : IXMLNode;
                                ARestriction          : string;
                                var ARestrictionValue : string): boolean;
    function GetXSDAnnotation (ANode : IXMLNode): string;
    function FindXSDElementNode (ANode        : IXMLNode;
                                 AElementName : string): IXMLNode;
    function FindXMLElementNode (ANode        : IXMLNode;
                                 AElementName : string): IXMLNode;
    function FindXMLElementNodeWithValue (ANode        : IXMLNode;
                                          AElementName : string;
                                          AValue       : string): IXMLNode;
    function IsNodeAXSDElement (ANode : IXMLNode): boolean;
    function ValidateProperty (AContext       : TValidationContext;
                               APropertyName  : String;
                               AErrorPrefix   : String;
                               AXMLRootNode   : IXMLNode;
                               AXSDSchemaNode : IXMLNode;
                               var AErrorMsg  : String) : Boolean;
    procedure PopulateCbxAllNetworkModules (ACbxNetworkModules     : TWRMFComboBox;
                                            AAllNetworkModulesNode : IXMLNode;
                                            AAllowZero             : Boolean);
    procedure PopulateCbxAllNetworkModulesID (ACbxNetworkModules     : TWRMFComboBox;
                                              AAllNetworkModulesNode : IXMLNode;
                                              AAllowZero             : Boolean);
    procedure PopulateCbxAllNetworkRoutes (ACbxNetworkRoutes     : TWRMFComboBox;
                                           AAllNetworkRoutesNode : IXMLNode;
                                           AAllowZero            : Boolean);
    function IsNetworkRouteValid (ARouteNo              : String;
                                  AAllNetworkRoutesNode : IXMLNode;
                                  AAllowZero            : Boolean) : Boolean;
    function IsRunOffModuleValid (AModuleNo             : String;
                                  AAllRunOffModulesNode : IXMLNode;
                                  AAllowZero            : Boolean) : Boolean;
    function XSDFilePath : string;
    function GetXSD (AFileName : String) : String;
    function Validate (AContext          : TValidationContext;
                       APropertyName     : String;
                       AFieldIndex       : String;
                       AInputXML         : String;
                       AOutputXML        : String;
                       AStopOnFirstError : Boolean;
                       AErrorList        : TStringList;
                       AErrorMessages    : TStringList) : Boolean; virtual;
    function DoAdditionalValidation (AContext          : TValidationContext;
                                     APropertyName     : String;
                                     AFieldIndex       : String;
                                     AXSDDoc           : IXMLDocument;
                                     AXMLDocumentIn    : IXMLDocument;
                                     AXMLDocumentOut   : IXMLDocument;
                                     AStopOnFirstError : Boolean;
                                     AErrorList        : TStringList;
                                     AErrorMessages    : TStringList) : Boolean; virtual;
    procedure AddData (AModule    : INetworkModule;
                       ASectionNo : Integer;
                       ARootNode  : IXMLNode); overload;
    procedure AddData (ARoute    : INetworkRoute;
                       ARootNode : IXMLNode); overload;
    procedure AddData (AObsPoint : IObservationPoint;
                       ARootNode : IXMLNode); overload;
    procedure AddData (ATimeSeries : ITimeSeries;
                       ATitle      : String;
                       ARootNode   : IXMLNode); overload;
    procedure AddData (AElementType    : WideString;
                       AElementSubType : WideString;
                       AElementNo      : Integer;
                       AElementID      : Integer;
                       ASubElementID   : Integer;
                       AResultTypeID   : Integer;
                       ARootNode       : IXMLNode); overload;
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); overload; virtual;
    procedure AddSectionData (ARoute     : INetworkRoute;
                              ARootNode  : IXMLNode); overload; virtual;
    procedure AddSectionData (AObsPoint  : IObservationPoint;
                              ARootNode  : IXMLNode); overload; virtual;
    procedure AddSectionData (AElementType    : WideString;
                              AElementSubType : WideString;
                              AElementNo      : Integer;
                              AElementID      : Integer;
                              ASubElementID   : Integer;
                              AResultTypeID   : Integer;
                              ARootNode       : IXMLNode); overload; virtual;
    procedure AddSectionData (ATimeSeries : ITimeSeries;
                              ATitle      : String;
                              ARootNode   : IXMLNode); overload; virtual;
    procedure AddPopulationData (ARootNode : IXMLNode); overload; virtual;
    procedure AddPopulationData (ARootNode       : IXMLNode;
                                 AElementType    : String;
                                 AElementID      : Integer); overload; virtual;
    function InXMLDocument (AModule : INetworkModule; ASectionNo : Integer) : IXMLDocument; overload;
    function InXMLDocument (ARoute : INetworkRoute) : IXMLDocument; overload;
    function InXMLDocument (AObsPoint : IObservationPoint) : IXMLDocument; overload;
    function InXMLDocument (ATimeSeries : ITimeSeries;
                            ATitle      : String) : IXMLDocument; overload;
    function InXMLDocument (AElementType    : WideString;
                            AElementSubType : WideString;
                            AElementNo      : Integer;
                            AElementID      : Integer;
                            ASubElementID   : Integer;
                            AResultTypeID   : Integer) : IXMLDocument; overload;
    function OutXMLDocument (AModule : INetworkModule; ASectionNo : Integer) : IXMLDocument; overload;
    function OutXMLDocument (ARoute : INetworkRoute) : IXMLDocument; overload;
    function OutXMLDocument (AObsPoint : IObservationPoint) : IXMLDocument; overload;
    function OutXMLDocument (AElementType    : WideString;
                             AElementSubType : WideString;
                             AElementNo      : Integer;
                             AElementID      : Integer;
                             ASubElementID   : Integer;
                             AResultTypeID   : Integer) : IXMLDocument; overload;
    function XSDText : String; virtual;
    procedure PopulatePropertyLists; virtual;
  end;

implementation

uses
  UErrorHandlingOperations,
  SysUtils, Variants;

constructor TXMLAgent.Create;
const OPNAME = 'TXMLAgent.Create';
begin
  try
    inherited Create;
    FAllProperties   := TStringList.Create;
    FAllDescriptions := TStringList.Create;
    PopulatePropertyLists;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TXMLAgent.Destroy;
const OPNAME = 'TXMLAgent.Destroy';
begin
  try
    FreeAndNil(FAllProperties);
    FreeAndNil(FAllDescriptions);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TXMLAgent.XSDFilePath : string;
const OPNAME = 'TXMLAgent.XSDFilePath';
var
  lBinStr   : string;
  lPath     : string;
begin
  Result := '';
  try
    lPath := ApplicationExeName;
    lPath := ExtractFilePath(lPath);
    if (Length(lPath) > 4) then
    begin
      lBinStr := Copy(lPath, Length(lPath)-3, 4);
      if (UpperCase(lBinStr) = 'BIN\') then
        lPath := Copy(lPath, 1, Length(lPath)-4);
    end;
    lPath := IncludeTrailingPathDelimiter(lPath);
    lPath := lPath + 'HydroXSD\';
    Result := lPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.GetXSD (AFileName : String) : String;
const OPNAME = 'TXMLAgent.GetXSD';
var
  LFileName : String;
  LXSD      : String;
  LFileData : TStringList;
begin
  try
    LFileData := TStringList.Create;
    try
      LFileData.Clear;
      LFileName := XSDFilePath + AFileName;
      LFileData.LoadFromFile(LFileName);
      LXSD := LFileData.Text;
    finally
      LFileData.Free;
    end;
    Result := LXSD;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.CreateXMLDocument (AXMLText : String) : IXMLDocument;
const OPNAME = 'TXMLAgent.CreateXMLDocument';
var
  LXMLDocument : TXMLDocument;
begin
  Result := nil;
  try
    LXMLDocument := TXMLDocument.Create(nil);
    try
      LXMLDocument.DOMVendor    := GetDOMVendor('MSXML');
      LXMLDocument.Options      := [doNodeAutoCreate,doAttrNull,doAutoPrefix,doNamespaceDecl,doNodeAutoIndent];
      LXMLDocument.ParseOptions := [];
      LXMLDocument.XML.Text     := AXMLText;
      LXMLDocument.Active       := TRUE;
    finally
      Result := LXMLDocument;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.GetXSDSchemaNode (Doc: IXMLDocument): IXMLNode;
const OPNAME = 'TXMLAgent.GetXSDSchemaNode';
begin
  Result := nil;
  try
    Result := Doc.DocumentElement.ChildNodes[0];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.GetXSDAnnotation (ANode : IXMLNode): string;
const OPNAME = 'TXMLAgent.GetXSDAnnotation';
begin
  Result := '';
  try
    if Assigned(ANode) then
      if Assigned(ANode.ChildNodes['xsd:annotation']) then
        if Assigned(ANode.ChildNodes['xsd:annotation'].ChildNodes['xsd:documentation']) then
          Result := ANode.ChildNodes['xsd:annotation'].ChildNodes['xsd:documentation'].NodeValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.GetXSDNillable (ANode : IXMLNode): Boolean;
const OPNAME = 'TXMLAgent.GetXSDNillable';
begin
  Result := FALSE;
  try
    if Assigned(ANode) then
    begin
      if (NOT VarIsNull(ANode.GetAttribute('nillable'))) then
      begin
        Result := ANode.GetAttribute('nillable') = 'true';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.GetXSDType (ANode : IXMLNode): String;
const OPNAME = 'TXMLAgent.GetXSDType';
var
  LNode : IXMLNode;
begin
  Result := '';
  try
    if Assigned(ANode) then
    begin
      if (NOT VarIsNull(ANode.GetAttribute('type'))) then
      begin
        Result := ANode.GetAttribute('type');
      end
      else if Assigned(ANode.ChildNodes['xsd:simpleType']) then
      begin
        if Assigned(ANode.ChildNodes['xsd:simpleType'].ChildNodes['xsd:restriction']) then
        begin
          LNode := ANode.ChildNodes['xsd:simpleType'].ChildNodes['xsd:restriction'];
          Result := LNode.GetAttribute('base');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Returns the required restriction for the given element node.
//
function TXMLAgent.GetXSDRestriction (ANode                 : IXMLNode;
                                      ARestriction          : string;
                                      var ARestrictionValue : string): boolean;
const OPNAME = 'TXMLAgent.GetXSDRestriction';
var
  LNode: IXMLNode;
  LRestrictionNode: IXMLNode;
begin
  Result := False;
  try
    ARestrictionValue := '';
    if Assigned(ANode) then
    begin
      if Assigned(ANode.ChildNodes['xsd:simpleType']) then
      begin
        if Assigned(ANode.ChildNodes['xsd:simpleType'].ChildNodes['xsd:restriction']) then
        begin
          LNode := ANode.ChildNodes['xsd:simpleType'].ChildNodes['xsd:restriction'];
          LRestrictionNode := LNode.ChildNodes[ARestriction];
          if Assigned(LRestrictionNode) then
          begin
            if LRestrictionNode.HasAttribute('value') then
            begin
              ARestrictionValue := LRestrictionNode.Attributes['value'];
              Result := (ARestrictionValue <> '');
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Finds the first element that has the given name attribute.
//
function TXMLAgent.FindXMLElementNode (ANode        : IXMLNode;
                                       AElementName : string): IXMLNode;
const OPNAME = 'TXMLAgent.FindXMLElementNode';
var
  LIndex : integer;
  LNode  : IXMLNode;
begin
  Result := nil;
  try
    LNode := ANode.ChildNodes.FindNode(AElementName);
    if (LNode <> nil) then
      Result := LNode
    else
    begin
      LIndex := 0;
      while ((Result = nil) AND (LIndex < ANode.ChildNodes.Count)) do
      begin
        Result := FindXMLElementNode(ANode.ChildNodes[LIndex], AElementName);
        LIndex := LIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.FindXMLElementNodeWithValue (ANode        : IXMLNode;
                                                AElementName : string;
                                                AValue       : string): IXMLNode;
const OPNAME = 'TXMLAgent.FindXMLElementNodeWithValue';
var
  LIndex : integer;
begin
  Result := nil;
  try
    if (ANode.IsTextElement AND (ANode.LocalName = AElementName) AND (ANode.Text = AValue)) then
      Result := ANode;
    LIndex := 0;
    while ((Result = nil) AND (LIndex < ANode.ChildNodes.Count)) do
    begin
      Result := FindXMLElementNodeWithValue(ANode.ChildNodes[LIndex], AElementName, AValue);
      LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// Finds the first element that has the given name attribute.
//
function TXMLAgent.FindXSDElementNode (ANode        : IXMLNode;
                                       AElementName : string): IXMLNode;
const OPNAME = 'TXMLAgent.FindXSDElementNode';
var
  LIndex: integer;
begin
  Result := nil;
  try
    if IsNodeAXSDElement(ANode) then
      if (UpperCase(ANode.Attributes['name']) = UpperCase(AElementName)) then
        Result := ANode;
    if (not Assigned(Result)) then
    begin
      for LIndex := 0 to ANode.ChildNodes.Count - 1 do
      begin
        Result := FindXSDElementNode(ANode.ChildNodes[LIndex], AElementName);
        if Assigned(Result) then
          break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//
// The node has to be an element node and it must have a name attribute.
//
function TXMLAgent.IsNodeAXSDElement(ANode: IXMLNode): boolean;
const OPNAME = 'TXMLAgent.IsNodeAXSDElement';
begin
  Result := False;
  try
    if (UpperCase(ANode.NodeName) = UpperCase('xsd:element')) then
      if ANode.HasAttribute('name') then
        Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.ValidateProperty (AContext       : TValidationContext;
                                     APropertyName  : String;
                                     AErrorPrefix   : String;
                                     AXMLRootNode   : IXMLNode;
                                     AXSDSchemaNode : IXMLNode;
                                     var AErrorMsg  : String) : Boolean;
const OPNAME = 'TXMLAgent.ValidateProperty';
var
  LXMLNode    : IXMLNode;
  LXSDNode    : IXMLNode;
  LType       : String;
  LNillable   : Boolean;
  LStrValue   : String;
  LMinStr     : String;
  LMaxStr     : String;
  LIntValue   : Integer;
  LMinInt     : Integer;
  LMaxInt     : Integer;
  LFloatValue : Double;
  LMinFloat   : Double;
  LMaxFloat   : Double;
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    LXMLNode := FindXMLElementNode(AXMLRootNode, APropertyName);
    LXSDNode := FindXSDElementNode(AXSDSchemaNode, APropertyName);
    if ((LXMLNode <> nil) AND (LXSDNode <> nil)) then
    begin
      LType     := GetXSDType(LXSDNode);
      LNillable := GetXSDNillable(LXSDNode);
      LStrValue := Trim(LXMLNode.Text);
      Result    := TRUE;
      if ((AContext = tcApply) AND (LStrValue = '') AND (NOT LNillable)) then
      begin
        Result := FALSE;
        AErrorMsg := AErrorPrefix + ' may not be empty.';
      end
      else if (LType = 'xsd:integer') then
      begin
        try
          LIntValue := StrToInt(LStrValue);
          if (GetXSDRestriction(LXSDNode, 'xsd:minInclusive', LMinStr)) then
          begin
            LMinInt := StrToInt(LMinStr);
            if (LIntValue < LMinInt) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be greater than or equal to ' + IntToStr(LMinInt) + '.';
            end;
          end
          else if (GetXSDRestriction(LXSDNode, 'xsd:minExclusive', LMinStr)) then
          begin
            LMinInt := StrToInt(LMinStr);
            if (LIntValue <= LMinInt) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be greater than ' + IntToStr(LMinInt) + '.';
            end;
          end;
          if (GetXSDRestriction(LXSDNode, 'xsd:maxInclusive', LMaxStr)) then
          begin
            LMaxInt := StrToInt(LMaxStr);
            if (LIntValue > LMaxInt) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be less than or equal to ' + IntToStr(LMaxInt) + '.';
            end;
          end
          else if (GetXSDRestriction(LXSDNode, 'xsd:maxExclusive', LMaxStr)) then
          begin
            LMaxInt := StrToInt(LMaxStr);
            if (LIntValue >= LMaxInt) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be less than ' + IntToStr(LMaxInt) + '.';
            end;
          end;
        except
          on EConvertError do
          begin
            Result := FALSE;
            AErrorMsg := AErrorPrefix + ' must be an integer value.';
          end;
        end;
      end
      else if (LType = 'xsd:double') then
      begin
        try
          LFloatValue := StrToFloat(LStrValue);
          if (GetXSDRestriction(LXSDNode, 'xsd:minInclusive', LMinStr)) then
          begin
            LMinFloat := StrToFloat(LMinStr);
            if (LFloatValue < LMinFloat) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be greater than or equal to ' + FloatToStr(LMinFloat) + '.';
            end;
          end
          else if (GetXSDRestriction(LXSDNode, 'xsd:minExclusive', LMinStr)) then
          begin
            LMinFloat := StrToFloat(LMinStr);
            if (LFloatValue <= LMinFloat) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be greater than ' + FloatToStr(LMinFloat) + '.';
            end;
          end;
          if (GetXSDRestriction(LXSDNode, 'xsd:maxInclusive', LMaxStr)) then
          begin
            LMaxFloat := StrToFloat(LMaxStr);
            if (LFloatValue > LMaxFloat) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be less than or equal to ' + FloatToStr(LMaxFloat) + '.';
            end;
          end
          else if (GetXSDRestriction(LXSDNode, 'xsd:maxExclusive', LMaxStr)) then
          begin
            LMaxFloat := StrToFloat(LMaxStr);
            if (LFloatValue >= LMaxFloat) then
            begin
              Result := FALSE;
              AErrorMsg := AErrorPrefix + ' must be less than ' + FloatToStr(LMaxFloat) + '.';
            end;
          end;
        except
          on EConvertError do
          begin
            Result := FALSE;
            AErrorMsg := AErrorPrefix + ' must be an float value.';
          end;
        end;
      end
      else if (LType = 'xsd:string') then
      begin
        if (GetXSDRestriction(LXSDNode, 'xsd:minLength', LMinStr)) then
        begin
          LMinInt := StrToInt(LMinStr);
          if (Length(LStrValue) < LMinInt) then
          begin
            Result := FALSE;
            AErrorMsg := AErrorPrefix + ' must be at least ' + IntToStr(LMinInt) + ' character long.';
          end;
        end;
        if (GetXSDRestriction(LXSDNode, 'xsd:maxLength', LMaxStr)) then
        begin
          LMaxInt := StrToInt(LMaxStr);
          if (Length(LStrValue) > LMaxInt) then
          begin
            Result := FALSE;
            AErrorMsg := AErrorPrefix + ' may not be more than ' + IntToStr(LMaxInt) + ' characters long.';
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddXMLAllNetworkRoutes (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddXMLAllNetworkRoutes';
var
  LAllNetworkRoutesNode  : IXMLNode;
  LNode                  : IXMLNode;
  LIndex                 : Integer;
  LNetworkRoute          : INetworkRoute;
begin
  try
    LAllNetworkRoutesNode := ARootNode.ChildNodes['AllNetworkRoutes'];
    for LIndex := 0 to FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
    begin
      LNetworkRoute := FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex];
      LNode         := LAllNetworkRoutesNode.AddChild('RouteNo');
      LNode.Text    := IntToStr(LNetworkRoute.RouteNo);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddXMLAllOutflowRoutes (ARootNode : IXMLNode; ASourceModuleID : Integer);
const OPNAME = 'TXMLAgent.AddXMLAllOutflowRoutes';
var
  LAllNetworkRoutesNode  : IXMLNode;
  LNode                  : IXMLNode;
  LIndex                 : Integer;
  LNetworkRoute          : INetworkRoute;
begin
  try
    LAllNetworkRoutesNode := ARootNode.ChildNodes['OutflowRoutes'];
    for LIndex := 0 to FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
    begin
      LNetworkRoute := FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex];
      if (LNetworkRoute.SourceModuleID = ASourceModuleID) then
      begin
        LNode         := LAllNetworkRoutesNode.AddChild('RouteNo');
        LNode.Text    := IntToStr(LNetworkRoute.RouteNo);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddXMLAllInflowRoutes (ARootNode : IXMLNode; ASinkModuleID : Integer);
const OPNAME = 'TXMLAgent.AddXMLAllInflowRoutes';
var
  LAllNetworkRoutesNode  : IXMLNode;
  LNode                  : IXMLNode;
  LIndex                 : Integer;
  LNetworkRoute          : INetworkRoute;
begin
  try
    LAllNetworkRoutesNode := ARootNode.ChildNodes['InflowRoutes'];
    for LIndex := 0 to FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteCount - 1 do
    begin
      LNetworkRoute := FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByIndex[LIndex];
      if (LNetworkRoute.SinkModuleID = ASinkModuleID) then
      begin
        LNode         := LAllNetworkRoutesNode.AddChild('RouteNo');
        LNode.Text    := IntToStr(LNetworkRoute.RouteNo);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddXMLAllNetworkModules (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddXMLAllNetworkModules';
var
  LAllNetworkModulesNode : IXMLNode;
  LNode                  : IXMLNode;
  LIndex                 : Integer;
  LReservoir             : IReservoirModule;
  LRunOff                : IRunOffModule;
  LChannel               : IChannelModule;
  LIrrBlock              : IIrrigationModule;
  LMine                  : IMineModule;
begin
  try
    LAllNetworkModulesNode := ARootNode.ChildNodes['AllNetworkModules'];
    for LIndex := 0 to FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleCount - 1 do
    begin
      LReservoir := FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByIndex[LIndex];
      LNode      := LAllNetworkModulesNode.AddChild('NetworkModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text := IntToStr(LReservoir.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LReservoir.ModuleNumber);
      LNode.ChildNodes['Name'].Text     := '(RV' + IntToStr(LReservoir.ModuleNumber) + ') ' + LReservoir.ReservoirName;
    end;
    for LIndex := 0 to FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount - 1 do
    begin
      LRunOff    := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByIndex[LIndex];
      LNode      := LAllNetworkModulesNode.AddChild('NetworkModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text := IntToStr(LRunOff.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LRunOff.ModuleNumber);
      LNode.ChildNodes['Name'].Text     := '(RU' + IntToStr(LRunOff.ModuleNumber) + ') ' + LRunOff.RunOffName;
    end;
    for LIndex := 0 to FHydrologyModel.Network.ChannelModuleAgent.ChannelModuleCount - 1 do
    begin
      LChannel   := FHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByIndex[LIndex];
      LNode      := LAllNetworkModulesNode.AddChild('NetworkModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text := IntToStr(LChannel.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LChannel.ModuleNumber);
      LNode.ChildNodes['Name'].Text     := '(CR' + IntToStr(LChannel.ModuleNumber) + ') ' + LChannel.ChannelName;
    end;
    for LIndex := 0 to FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleCount - 1 do
    begin
      LIrrBlock  := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByIndex[LIndex];
      LNode      := LAllNetworkModulesNode.AddChild('NetworkModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text := IntToStr(LIrrBlock.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LIrrBlock.ModuleNumber);
      LNode.ChildNodes['Name'].Text     := '(RR' + IntToStr(LIrrBlock.ModuleNumber) + ') ' + LIrrBlock.IrrigationName;
    end;
    for LIndex := 0 to FHydrologyModel.Network.MineModuleAgent.MineModuleCount - 1 do
    begin
      LMine      := FHydrologyModel.Network.MineModuleAgent.MineModuleByIndex[LIndex];
      LNode      := LAllNetworkModulesNode.AddChild('NetworkModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text := IntToStr(LMine.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LMine.ModuleNumber);
      LNode.ChildNodes['Name'].Text     := '(MM' + IntToStr(LMine.ModuleNumber) + ') ' + LMine.MineName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddXMLAllRunOffModules (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddXMLAllRunOffModules';
var
  LAllRunOffModulesNode : IXMLNode;
  LNode                 : IXMLNode;
  LIndex                : Integer;
  LRunOff               : IRunOffModule;
begin
  try
    LAllRunOffModulesNode := ARootNode.ChildNodes['AllRunOffModules'];
    for LIndex := 0 to FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleCount - 1 do
    begin
      LRunOff    := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByIndex[LIndex];
      LNode      := LAllRunOffModulesNode.AddChild('RunOffModule');
      LNode.AddChild('ModuleID');
      LNode.AddChild('ModuleNumber');
      LNode.AddChild('Name');
      LNode.ChildNodes['ModuleID'].Text     := IntToStr(LRunOff.ModuleID);
      LNode.ChildNodes['ModuleNumber'].Text := IntToStr(LRunOff.ModuleNumber);
      LNode.ChildNodes['Name'].Text         := '(RU' + IntToStr(LRunOff.ModuleNumber) + ') ' + LRunOff.RunOffName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.PopulateCbxAllNetworkModules (ACbxNetworkModules     : TWRMFComboBox;
                                                  AAllNetworkModulesNode : IXMLNode;
                                                  AAllowZero             : Boolean);
const OPNAME = 'TXMLAgent.PopulateCbxAllNetworkModules';
var
  LIndex  : Integer;
  LNode   : IXMLNode;
  LNumber : Integer;
  Lname   : String;
begin
  try
    ACbxNetworkModules.Items.Clear;
    if (AAllowZero) then
      ACbxNetworkModules.Items.AddObject('None', pointer(0));
    for LIndex := 1 to AAllNetworkModulesNode.ChildNodes.Count do
    begin
      LNode   := AAllNetworkModulesNode.ChildNodes.Get(LIndex-1);
      LNumber := StrToInt(LNode.ChildNodes['ModuleNumber'].Text);
      LName   := LNode.ChildNodes['Name'].Text;
      ACbxNetworkModules.Items.AddObject(LName, pointer(LNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.PopulateCbxAllNetworkModulesID (ACbxNetworkModules     : TWRMFComboBox;
                                                    AAllNetworkModulesNode : IXMLNode;
                                                    AAllowZero             : Boolean);
const OPNAME = 'TXMLAgent.PopulateCbxAllNetworkModulesID';
var
  LIndex  : Integer;
  LNode   : IXMLNode;
  LNumber : Integer;
  Lname   : String;
begin
  try
    ACbxNetworkModules.Items.Clear;
    if (AAllowZero) then
      ACbxNetworkModules.Items.AddObject('None', pointer(0));
    for LIndex := 1 to AAllNetworkModulesNode.ChildNodes.Count do
    begin
      LNode   := AAllNetworkModulesNode.ChildNodes.Get(LIndex-1);
      LNumber := StrToInt(LNode.ChildNodes['ModuleID'].Text);
      LName   := LNode.ChildNodes['Name'].Text;
      ACbxNetworkModules.Items.AddObject(LName, pointer(LNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.PopulateCbxAllNetworkRoutes (ACbxNetworkRoutes     : TWRMFComboBox;
                                                 AAllNetworkRoutesNode : IXMLNode;
                                                 AAllowZero            : Boolean);
const OPNAME = 'TXMLAgent.PopulateCbxAllNetworkRoutes';
var
  LIndex  : Integer;
  LNumber : Integer;
begin
  try
    ACbxNetworkRoutes.Items.Clear;
    if (AAllowZero) then
      ACbxNetworkRoutes.Items.AddObject('None', pointer(0));
    for LIndex := 1 to AAllNetworkRoutesNode.ChildNodes.Count do
    begin
      LNumber := StrToInt(AAllNetworkRoutesNode.ChildNodes.Get(LIndex-1).Text);
      ACbxNetworkRoutes.Items.AddObject(IntToStr(LNumber), pointer(LNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.IsNetworkRouteValid (ARouteNo              : String;
                                        AAllNetworkRoutesNode : IXMLNode;
                                        AAllowZero            : Boolean) : Boolean;
const OPNAME = 'TXMLAgent.IsNetworkRouteValid';
var
  LIndex  : Integer;
  LResult : Boolean;
begin
  Result := FALSE;
  try
    LResult := FALSE;
    if (AAllowZero AND (Trim(ARouteNo) = '0')) then
      Result := TRUE
    else
    begin
      LIndex  := 0;
      while ((NOT LResult) AND (LIndex < AAllNetworkRoutesNode.ChildNodes.Count)) do
      begin
        if (AAllNetworkRoutesNode.ChildNodes[LIndex].Text = ARouteNo) then
          LResult := TRUE
        else
          LIndex := LIndex + 1;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.IsRunOffModuleValid (AModuleNo             : String;
                                        AAllRunOffModulesNode : IXMLNode;
                                        AAllowZero            : Boolean) : Boolean;
const OPNAME = 'TXMLAgent.IsRunOffModuleValid';
var
  LIndex  : Integer;
  LResult : Boolean;
  LNode   : IXMLNode;
begin
  Result := FALSE;
  try
    LResult := FALSE;
    if (AAllowZero AND (Trim(AModuleNo) = '0')) then
      Result := TRUE
    else
    begin
      LIndex  := 0;
      while ((NOT LResult) AND (LIndex < AAllRunOffModulesNode.ChildNodes.Count)) do
      begin
        LNode := AAllRunOffModulesNode.ChildNodes[LIndex];
        if (LNode.ChildNodes['ModuleNumber'].Text = AModuleNo) then
          LResult := TRUE
        else
          LIndex := LIndex + 1;
      end;
      Result := LResult;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.Validate (AContext          : TValidationContext;
                             APropertyName     : String;
                             AFieldIndex       : String;
                             AInputXML         : String;
                             AOutputXML        : String;
                             AStopOnFirstError : Boolean;
                             AErrorList        : TStringList;
                             AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgent.Validate';
var
  LResult         : Boolean;
  LXMLDocumentOut : IXMLDocument;
  LXMLDocumentIn  : IXMLDocument;
  LXSDDocument    : IXMLDocument;
  LInputRootNode  : IXMLNode;
  LOutputRootNode : IXMLNode;
  LSectionNode    : IXMLNode;
  LTempResult     : Boolean;
  LCount          : Integer;
  LXSDSchemaNode  : IXMLNode;
  LErrorMsg       : String;
  LProperty       : String;
  LDescription    : String;
  LSection        : String;
begin
  Result := FALSE;
  try
    AErrorMessages.Clear;
    AErrorList.Clear;
    AErrorList.Duplicates := dupIgnore;
    LXMLDocumentIn   := CreateXMLDocument(AInputXML);
    LXMLDocumentOut  := CreateXMLDocument(AOutputXML);
    LXSDDocument     := CreateXMLDocument(XSDText);
    try
      LInputRootNode  := LXMLDocumentIn.DocumentElement;
      LOutputRootNode := LXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
      LSection        := LOutputRootNode.ChildNodes['Section'].Text;
      LSectionNode    := LOutputRootNode.ChildNodes[LSection];
      LXSDSchemaNode  := GetXSDSchemaNode(LXSDDocument);
      LResult         := TRUE;

      for LCount := 0 to FAllProperties.Count - 1 do
      begin
        LProperty    := FAllProperties[LCount];
        LDescription := FAllDescriptions[LCount];
        if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = LProperty) OR (APropertyName = ''))) then
        begin
          LTempResult := ValidateProperty(AContext, LProperty, LDescription, LSectionNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add(LProperty);
          end;
          LResult := LResult AND LTempResult;
        end;
      end;

      Result := LResult AND
                DoAdditionalValidation(AContext, APropertyName, AFieldIndex,
                                       LXSDDocument, LXMLDocumentIn, LXMLDocumentOut,
                                       AStopOnFirstError, AErrorList, AErrorMessages);
    finally
      LXMLDocumentIn.Active := FALSE;
      LXMLDocumentOut.Active := FALSE;
      LXMLDocumentIn  := nil;
      LXMLDocumentOut := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.DoAdditionalValidation (AContext          : TValidationContext;
                                           APropertyName     : String;
                                           AFieldIndex       : String;
                                           AXSDDoc           : IXMLDocument;
                                           AXMLDocumentIn    : IXMLDocument;
                                           AXMLDocumentOut   : IXMLDocument;
                                           AStopOnFirstError : Boolean;
                                           AErrorList        : TStringList;
                                           AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgent.DoAdditionalValidation';
begin
  Result := TRUE;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddData (AElementType    : WideString;
                             AElementSubType : WideString;
                             AElementNo      : Integer;
                             AElementID      : Integer;
                             ASubElementID   : Integer;
                             AResultTypeID   : Integer;
                             ARootNode       : IXMLNode);
const OPNAME = 'TXMLAgent.AddData';
begin
  try
    ARootNode.ChildNodes['Model'].Text           := 'Hydrology';
    ARootNode.ChildNodes['NetworkID'].Text       := IntToStr(FHydrologyModel.Network.NetworkID);
    ARootNode.ChildNodes['ElementType'].Text     := AElementType;
    ARootNode.ChildNodes['ElementSubType'].Text  := AElementSubType;
    ARootNode.ChildNodes['ElementNo'].Text       := IntToStr(AElementNo);
    ARootNode.ChildNodes['ElementID'].Text       := IntToStr(AElementID);
    ARootNode.ChildNodes['SubElementID'].Text    := IntToStr(ASubElementID);
    ARootNode.ChildNodes['ResultTypeID'].Text    := IntToStr(AResultTypeID);
    if (ARootNode.NodeName = 'RootIn') then
      AddSectionData(AElementType, AElementSubType, AElementNo, AElementID, ASubElementID, AResultTypeID, ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddData (AModule    : INetworkModule;
                             ASectionNo : Integer;
                             ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgent.AddData';
begin
  try
    if (AModule <> nil) then
    begin
      ARootNode.ChildNodes['Model'].Text           := 'Hydrology';
      ARootNode.ChildNodes['NetworkID'].Text       := IntToStr(FHydrologyModel.Network.NetworkID);
      ARootNode.ChildNodes['ModuleType'].Text      := AModule.ModuleType;
      ARootNode.ChildNodes['Identifier'].Text      := IntToStr(AModule.ModuleID);
      ARootNode.ChildNodes['NetworkSequence'].Text := IntToStr(AModule.NetworkSequence);
      ARootNode.ChildNodes['ModuleNumber'].Text    := IntToStr(AModule.ModuleNumber);
      AddSectionData(AModule, ASectionNo, ARootNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddData (ARoute    : INetworkRoute;
                             ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddData';
begin
  try
    if (ARoute <> nil) then
    begin
      ARootNode.ChildNodes['Model'].Text      := 'Hydrology';
      ARootNode.ChildNodes['NetworkID'].Text  := IntToStr(FHydrologymodel.Network.NetworkID);
      ARootNode.ChildNodes['ModuleType'].Text := 'RQ'; {'Route';}
      ARootNode.ChildNodes['Identifier'].Text := IntToStr(ARoute.RouteNo);
      ARootNode.ChildNodes['Section'].Text    := 'NetworkRoute';
      AddSectionData(ARoute, ARootNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddData (AObsPoint : IObservationPoint;
                             ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddData';
begin
  try
    if (AObsPoint <> nil) then
    begin
      ARootNode.ChildNodes['Model'].Text      := 'Hydrology';
      ARootNode.ChildNodes['NetworkID'].Text  := IntToStr(FHydrologymodel.Network.NetworkID);
      ARootNode.ChildNodes['ModuleType'].Text := 'Obs';
      ARootNode.ChildNodes['Identifier'].Text := IntToStr(AObsPoint.RouteNo);
      ARootNode.ChildNodes['Section'].Text    := 'ObservationPoint';
      AddSectionData(AObsPoint, ARootNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddData (ATimeSeries : ITimeSeries;
                             ATitle      : String;
                             ARootNode   : IXMLNode);
const OPNAME = 'TXMLAgent.AddData';
begin
  try
    if (ATimeSeries <> nil) then
    begin
      ARootNode.ChildNodes['Model'].Text      := 'Hydrology';
      ARootNode.ChildNodes['NetworkID'].Text  := IntToStr(FHydrologymodel.Network.NetworkID);
      ARootNode.ChildNodes['Section'].Text    := 'TimeSeries';
      AddSectionData(ATimeSeries, ATitle, ARootNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddSectionData (AModule    : INetworkModule;
                                    ASectionNo : Integer;
                                    ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgent.AddSectionData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddSectionData (ARoute     : INetworkRoute;
                                    ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgent.AddSectionData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddSectionData (AObsPoint  : IObservationPoint;
                                    ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgent.AddSectionData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddSectionData (AElementType    : WideString;
                                    AElementSubType : WideString;
                                    AElementNo      : Integer;
                                    AElementID      : Integer;
                                    ASubElementID   : Integer;
                                    AResultTypeID   : Integer;
                                    ARootNode       : IXMLNode);
const OPNAME = 'TXMLAgent.AddSectionData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddSectionData (ATimeSeries : ITimeSeries;
                                    ATitle      : String;
                                    ARootNode   : IXMLNode);
const OPNAME = 'TXMLAgent.AddSectionData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgent.AddPopulationData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.AddPopulationData (ARootNode       : IXMLNode;
                                       AElementType    : String;
                                       AElementID      : Integer);
const OPNAME = 'TXMLAgent.AddPopulationData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.InXMLDocument (AElementType    : WideString;
                                  AElementSubType : WideString;
                                  AElementNo      : Integer;
                                  AElementID      : Integer;
                                  ASubElementID   : Integer;
                                  AResultTypeID   : Integer) : IXMLDocument;
const OPNAME = 'TXMLAgent.InXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootIn', TXMLNode);
    AddData(AElementType, AElementSubType, AElementNo, AElementID, ASubElementID, AResultTypeID, LRootNode);
    AddPopulationData(LRootNode, AElementType, AElementID);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.OutXMLDocument (AElementType    : WideString;
                                   AElementSubType : WideString;
                                   AElementNo      : Integer;
                                   AElementID      : Integer;
                                   ASubElementID   : Integer;
                                   AResultTypeID   : Integer) : IXMLDocument;
const OPNAME = 'TXMLAgent.OutXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootOut', TXMLNode);
    AddData(AElementType, AElementSubType, AElementNo, AElementID, ASubElementID, AResultTypeID, LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.InXMLDocument (AModule    : INetworkModule;
                                  ASectionNo : Integer) : IXMLDocument;
const OPNAME = 'TXMLAgent.InXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootIn', TXMLNode);
    AddData(AModule, ASectionNo, LRootNode);
    AddPopulationData(LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.InXMLDocument (ARoute : INetworkRoute) : IXMLDocument;
const OPNAME = 'TXMLAgent.InXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootIn', TXMLNode);
    AddData(ARoute, LRootNode);
    AddPopulationData(LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.InXMLDocument (ATimeSeries : ITimeSeries;
                                  ATitle      : String) : IXMLDocument;
const OPNAME = 'TXMLAgent.InXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootIn', TXMLNode);
    AddData(ATimeSeries, ATitle, LRootNode);
    AddPopulationData(LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.InXMLDocument (AObsPoint : IObservationPoint) : IXMLDocument;
const OPNAME = 'TXMLAgent.InXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootIn', TXMLNode);
    AddData(AObsPoint, LRootNode);
    AddPopulationData(LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.OutXMLDocument (AModule    : INetworkModule;
                                   ASectionNo : Integer): IXMLDocument;
const OPNAME = 'TXMLAgent.OutXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootOut', TXMLNode);
    AddData(AModule, ASectionNo, LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.OutXMLDocument (ARoute : INetworkRoute): IXMLDocument;
const OPNAME = 'TXMLAgent.OutXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootOut', TXMLNode);
    AddData(ARoute, LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.OutXMLDocument (AObsPoint : IObservationPoint): IXMLDocument;
const OPNAME = 'TXMLAgent.OutXMLDocument';
var
  LDocument : IXMLDocument;
  LRootNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('RootOut', TXMLNode);
    AddData(AObsPoint, LRootNode);
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgent.XSDText : String;
const OPNAME = 'TXMLAgent.XSDText';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgent.PopulatePropertyLists;
const OPNAME = 'TXMLAgent.PopulatePropertyLists';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

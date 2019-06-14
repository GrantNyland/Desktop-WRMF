(******************************************************************************)
(* This unit contains the class TXMLAgentInflowRoutes.
(******************************************************************************)
unit UXMLAgentInflowRoutes;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentInflowRoutes = class(TXMLAgent)
  protected
  public
    function XSDText : String; override;
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
    function DoAdditionalValidation (AContext          : TValidationContext;
                                     APropertyName     : String;
                                     AFieldIndex       : String;
                                     AXSDDoc           : IXMLDocument;
                                     AXMLDocumentIn    : IXMLDocument;
                                     AXMLDocumentOut   : IXMLDocument;
                                     AStopOnFirstError : Boolean;
                                     AErrorList        : TStringList;
                                     AErrorMessages    : TStringList) : Boolean; override;
  end;

implementation

uses
  UErrorHandlingOperations;

function TXMLAgentInflowRoutes.XSDText : String;
const OPNAME = 'TXMLAgentInflowRoutes.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('InflowRoutes.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentInflowRoutes.AddSectionData (AModule    : INetworkModule;
                                                ASectionNo : Integer;
                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentInflowRoutes.AddSectionData';
var
  LNode             : IXMLNode;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LReservoirModule  : IReservoirModule;
  LChannelModule    : IChannelModule;
  LInflowRoute      : IInflowRoute;
  LIndex            : Integer;
begin
  try
    if (AModule.ModuleType = 'RV') then
    begin
      LReservoirModule := FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByID[AModule.ModuleID];
      if (LReservoirModule <> nil) then
      begin
        ARootNode.ChildNodes['Section'].Text         := 'InflowRoutes';

        LSectionNode  := ARootNode.ChildNodes['InflowRoutes'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LReservoirModule.NoOfInFlowRoutes - 1 do
        begin
          LInflowRoute := LReservoirModule.InflowRouteByIndex[LIndex];
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('InflowRouteNo');
          LNode.AddChild('InflowFileName');
          LNode.ChildNodes['InflowRouteNo'].Text  := IntToStr(LInflowRoute.RouteNo);
          LNode.ChildNodes['InflowFileName'].Text := LInflowRoute.FileName;
        end;
      end;
    end
    else if (AModule.ModuleType = 'CR') then
    begin
      LChannelModule := FHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByID[AModule.ModuleID];
      if (LChannelModule <> nil) then
      begin
        ARootNode.ChildNodes['Section'].Text         := 'InflowRoutes';

        LSectionNode  := ARootNode.ChildNodes['InflowRoutes'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LChannelModule.NoOfInFlowRoutes - 1 do
        begin
          LInflowRoute := LChannelModule.InflowRouteByIndex[LIndex];
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('InflowRouteNo');
          LNode.AddChild('InflowFileName');
          LNode.ChildNodes['InflowRouteNo'].Text  := IntToStr(LInflowRoute.RouteNo);
          LNode.ChildNodes['InflowFileName'].Text := LInflowRoute.FileName;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentInflowRoutes.DoAdditionalValidation (AContext          : TValidationContext;
                                                       APropertyName     : String;
                                                       AFieldIndex       : String;
                                                       AXSDDoc           : IXMLDocument;
                                                       AXMLDocumentIn    : IXMLDocument;
                                                       AXMLDocumentOut   : IXMLDocument;
                                                       AStopOnFirstError : Boolean;
                                                       AErrorList        : TStringList;
                                                       AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentInflowRoutes.DoAdditionalValidation';
var
  LResult           : Boolean;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
  LRootNode         : IXMLNode;
  LTempResult       : Boolean;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
begin
  Result := FALSE;
  try
    LRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LRootNode.ChildNodes['InflowRoutes'];
    LDataListNode   := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode  := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // InflowRouteNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'InflowRouteNo') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'InflowRouteNo', 'InflowRouteNo', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('InflowRouteNo,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // InflowFileName
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'InflowFileName') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'InflowFileName', 'InflowFileName', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('InflowFileName,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


(******************************************************************************)
(* This unit contains the class TXMLAgentOutflowRoutes.
(******************************************************************************)
unit UXMLAgentOutflowRoutes;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentOutflowRoutes = class(TXMLAgent)
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

function TXMLAgentOutflowRoutes.XSDText : String;
const OPNAME = 'TXMLAgentOutflowRoutes.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('OutflowRoutes.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentOutflowRoutes.AddSectionData (AModule    : INetworkModule;
                                                 ASectionNo : Integer;
                                                 ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentOutflowRoutes.AddSectionData';
var
  LNode              : IXMLNode;
  LSectionNode       : IXMLNode;
  LDataListNode      : IXMLNode;
  LReservoirModule   : IReservoirModule;
  LChannelModule     : IChannelModule;
  LOutflowRoute      : IOutflowRoute;
  LIndex             : Integer;
begin
  try
    if (AModule.ModuleType = 'RV') then
    begin
      LReservoirModule := FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByID[AModule.ModuleID];
      if (LReservoirModule <> nil) then
      begin
        ARootNode.ChildNodes['Section'].Text         := 'OutflowRoutes';

        LSectionNode := ARootNode.ChildNodes['OutflowRoutes'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LReservoirModule.NoOfOutflowRoutes - 1 do
        begin
          LOutflowRoute := LReservoirModule.OutflowRouteByIndex[LIndex];
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('OutflowRouteNo');
          LNode.AddChild('OutflowFileName');
          LNode.ChildNodes['OutflowRouteNo'].Text  := IntToStr(LOutflowRoute.RouteNo);
          LNode.ChildNodes['OutflowFileName'].Text := LOutflowRoute.FileName;
        end;
      end;
    end
    else if (AModule.ModuleType = 'CR') then
    begin
      LChannelModule := FHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByID[AModule.ModuleID];
      if (LChannelModule <> nil) then
      begin
        ARootNode.ChildNodes['Section'].Text         := 'OutflowRoutes';

        LSectionNode := ARootNode.ChildNodes['OutflowRoutes'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LChannelModule.NoOfOutflowRoutes - 1 do
        begin
          LOutflowRoute := LChannelModule.OutflowRouteByIndex[LIndex];
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('OutflowRouteNo');
          LNode.AddChild('OutflowFileName');
          LNode.ChildNodes['OutflowRouteNo'].Text  := IntToStr(LOutflowRoute.RouteNo);
          LNode.ChildNodes['OutflowFileName'].Text := LOutflowRoute.FileName;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentOutflowRoutes.DoAdditionalValidation (AContext          : TValidationContext;
                                                        APropertyName     : String;
                                                        AFieldIndex       : String;
                                                        AXSDDoc           : IXMLDocument;
                                                        AXMLDocumentIn    : IXMLDocument;
                                                        AXMLDocumentOut   : IXMLDocument;
                                                        AStopOnFirstError : Boolean;
                                                        AErrorList        : TStringList;
                                                        AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentOutflowRoutes.DoAdditionalValidation';
var
  LResult            : Boolean;
  LSectionNode       : IXMLNode;
  LDataListNode      : IXMLNode;
  LNode              : IXMLNode;
  LIndex             : Integer;
  LRootNode          : IXMLNode;
  LTempResult        : Boolean;
  LXSDSchemaNode     : IXMLNode;
  LErrorMsg          : String;
begin
  Result := FALSE;
  try
    LRootNode      := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['OutflowRoutes'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // OutflowRouteNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'OutflowRouteNo') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'OutflowRouteNo', 'OutflowRouteNo', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('OutflowRouteNo,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // OutflowFileName
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'OutflowFileName') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'OutflowFileName', 'OutflowFileName', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('OutflowFileName,' + IntToStr(LIndex+1));
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


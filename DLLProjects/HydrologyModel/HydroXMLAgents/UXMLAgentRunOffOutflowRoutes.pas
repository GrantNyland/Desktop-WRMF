(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffOutflowRoutes.
(******************************************************************************)
unit UXMLAgentRunOffOutflowRoutes;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffOutflowRoutes = class(TXMLAgent)
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

procedure TXMLAgentRunOffOutflowRoutes.AddSectionData (AModule    : INetworkModule;
                                                       ASectionNo : Integer;
                                                       ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffOutflowRoutes.AddSectionData';
var
  LRunOffModule : IRunOffModule;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LOutflowRoute : IRunOffOutflowRoute;
  LIndex        : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffOutflowRoutes';

      LSectionNode := ARootNode.ChildNodes['RunOffOutflowRoutes'];
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LRunOffModule.NoOfOutflowRoutes - 1 do
      begin
        LOutflowRoute := LRunOffModule.OutFlowRouteByIndex[LIndex];
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('RouteNo');
        LNode.AddChild('OutflowPercentage');
        LNode.ChildNodes['RouteNo'].Text  := IntToStr(LOutflowRoute.RouteNo);
        LNode.ChildNodes['OutflowPercentage'].Text := FloatToStr(LOutflowRoute.OutflowPercentage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffOutflowRoutes.XSDText : String;
const OPNAME = 'TXMLAgentRunOffOutflowRoutes.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffOutflowRoutes.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffOutflowRoutes.DoAdditionalValidation (AContext          : TValidationContext;
                                                              APropertyName     : String;
                                                              AFieldIndex       : String;
                                                              AXSDDoc           : IXMLDocument;
                                                              AXMLDocumentIn    : IXMLDocument;
                                                              AXMLDocumentOut   : IXMLDocument;
                                                              AStopOnFirstError : Boolean;
                                                              AErrorList        : TStringList;
                                                              AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffOutflowRoutes.DoAdditionalValidation';
var
  LResult             : Boolean;
  LRootNode           : IXMLNode;
  LSectionNode        : IXMLNode;
  LDataListNode       : IXMLNode;
  LTempResult         : Boolean;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
  LNode               : IXMLNode;
  LIndex              : Integer;
begin
  Result := FALSE;
  try
    LRootNode      := AXMLDocumentOut.GetDocBinding('RunOffOutflowRoutesOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['RunOffOutflowRoutes'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // RouteNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'RouteNo') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'RouteNo', 'RouteNo', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('RouteNo,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // OutflowPercentage
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'OutflowPercentage') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'OutflowPercentage', 'OutflowPercentage', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('OutflowPercentage,' + IntToStr(LIndex+1));
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


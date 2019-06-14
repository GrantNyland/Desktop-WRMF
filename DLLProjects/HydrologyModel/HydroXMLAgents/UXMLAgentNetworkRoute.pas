(******************************************************************************)
(* This unit contains the class TXMLAgentNetworkRoute.
(******************************************************************************)
unit UXMLAgentNetworkRoute;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,

  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentNetworkRoute = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
    function XSDText : String; override;
    procedure AddSectionData (ARoute     : INetworkRoute;
                              ARootNode  : IXMLNode); override;
    procedure AddPopulationData (ARootNode : IXMLNode); override;
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

procedure TXMLAgentNetworkRoute.PopulatePropertyLists;
const OPNAME = 'TXMLAgentNetworkRoute.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'SourceModuleID, SinkModuleID';
    FAllDescriptions.CommaText := '"Source Module ID","Sink Module ID"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentNetworkRoute.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentNetworkRoute.AddPopulationData';
begin
  try
    AddXMLAllNetworkModules(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentNetworkRoute.AddSectionData (ARoute    : INetworkRoute;
                                                ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentNetworkRoute.AddSectionData';
var
  LSectionNode  : IXMLNode;
  LNetworkRoute : INetworkRoute;
begin
  try
    LNetworkRoute := FHydrologyModel.Network.NetworkRouteAgent.NetworkRouteByRouteNo[ARoute.RouteNo];
    if (LNetworkRoute <> nil) then
    begin
      LSectionNode := ARootNode.ChildNodes['NetworkRoute'];
      LSectionNode.ChildNodes['RouteNo'].Text        := IntToStr(LNetworkRoute.RouteNo);
      LSectionNode.ChildNodes['SourceModuleID'].Text := IntToStr(LNetworkRoute.SourceModuleID);
      LSectionNode.ChildNodes['SinkModuleID'].Text   := IntToStr(LNetworkRoute.SinkModuleID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentNetworkRoute.XSDText : String;
const OPNAME = 'TXMLAgentNetworkRoute.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('NetworkRoute.xsd')
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentNetworkRoute.DoAdditionalValidation (AContext          : TValidationContext;
                                                       APropertyName     : String;
                                                       AFieldIndex       : String;
                                                       AXSDDoc           : IXMLDocument;
                                                       AXMLDocumentIn    : IXMLDocument;
                                                       AXMLDocumentOut   : IXMLDocument;
                                                       AStopOnFirstError : Boolean;
                                                       AErrorList        : TStringList;
                                                       AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentNetworkRoute.DoAdditionalValidation';
var
  LResult             : Boolean;
  LSourceModuleIDStr  : String;
  LSinkModuleIDStr    : String;
  LInputRootNode      : IXMLNode;
  LOutputRootNode     : IXMLNode;
  LSectionNode        : IXMLNode;
  LXSDSchemaNode      : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode  := AXMLDocumentIn.DocumentElement;
    LOutputRootNode := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LOutputRootNode.ChildNodes['NetworkRoute'];
    LXSDSchemaNode  := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // Source and sink may not be the same
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'SourceModuleID') OR (APropertyName = 'SinkModuleID') OR (APropertyName = ''))) then
    begin
      LSourceModuleIDStr := Trim(LSectionNode.ChildNodes['SourceModuleID'].Text);
      LSinkModuleIDStr   := Trim(LSectionNode.ChildNodes['SinkModuleID'].Text);
      if ((LSourceModuleIDStr <> '') AND (LSinkModuleIDStr <> '') AND (LSourceModuleIDStr = LSinkModuleIDStr)) then
      begin
        LResult := FALSE;
        AErrorMessages.Add('Source and Sink modules may not be the same.');
        AErrorList.Add('SinkModuleID');
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


(******************************************************************************)
(* This unit contains the class TXMLAgentObservationPoint.
(******************************************************************************)
unit UXMLAgentObservationPoint;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,

  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentObservationPoint = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
    function XSDText : String; override;
    procedure AddSectionData (AObsPoint  : IObservationPoint;
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

procedure TXMLAgentObservationPoint.PopulatePropertyLists;
const OPNAME = 'TXMLAgentObservationPoint.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'RouteNo,Name,FlowDataFileName';
    FAllDescriptions.CommaText := '"Route number","Name", "Flow data file name"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentObservationPoint.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentObservationPoint.AddPopulationData';
begin
  try
    AddXMLAllNetworkRoutes(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentObservationPoint.AddSectionData (AObsPoint : IObservationPoint;
                                                    ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentObservationPoint.AddSectionData';
var
  LSectionNode  : IXMLNode;
  LObservationPoint : IObservationPoint;
begin
  try
    LObservationPoint := FHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[AObsPoint.RouteNo];
    if (LObservationPoint <> nil) then
    begin
      LSectionNode := ARootNode.ChildNodes['ObservationPoint'];
      LSectionNode.ChildNodes['RouteNo'].Text          := IntToStr(LObservationPoint.RouteNo);
      LSectionNode.ChildNodes['Name'].Text             := LObservationPoint.Name;
      LSectionNode.ChildNodes['FlowDataFileName'].Text := LObservationPoint.FlowDataFileName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentObservationPoint.XSDText : String;
const OPNAME = 'TXMLAgentObservationPoint.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('ObservationPoint.xsd')
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentObservationPoint.DoAdditionalValidation (AContext          : TValidationContext;
                                                           APropertyName     : String;
                                                           AFieldIndex       : String;
                                                           AXSDDoc           : IXMLDocument;
                                                           AXMLDocumentIn    : IXMLDocument;
                                                           AXMLDocumentOut   : IXMLDocument;
                                                           AStopOnFirstError : Boolean;
                                                           AErrorList        : TStringList;
                                                           AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentObservationPoint.DoAdditionalValidation';
var
  LResult             : Boolean;
  LInputRootNode      : IXMLNode;
  LOutputRootNode     : IXMLNode;
  LSectionNode        : IXMLNode;
  LAllNetworkRoutes   : IXMLNode;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
  LRouteNo            : Integer;
  LObsPoint           : IObservationPoint;
  LOtherObsPoint      : IObservationPoint;
  LIndex              : Integer;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LAllNetworkRoutes := LInputRootNode.ChildNodes['AllNetworkRoutes'];
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['ObservationPoint'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);
    LResult           := TRUE;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'RouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNo  := StrToInt(LSectionNode.ChildNodes['RouteNo'].Text);
      LObsPoint := FHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[LRouteNo];
      if (LObsPoint.RouteNo <> LRouteNo) then
      begin
        LIndex := 0;
        while (LResult AND (LIndex < FHydrologyModel.Network.ObservationPointAgent.ObservationPointCount)) do
        begin
          LOtherObsPoint := FHydrologyModel.Network.ObservationPointAgent.ObservationPointByIndex[LIndex];
          if ((LOtherObsPoint <> LObsPoint) AND (LOtherObsPoint.RouteNo = LRouteNo)) then
            LResult := FALSE
          else
            LIndex := LIndex + 1;
        end;
      end;

      if (NOT LResult) then
      begin
        LErrorMsg := 'Route no is invalid - there is already an observation point on route ' + IntToStr(LRouteNo) + '.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('RouteNo');
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


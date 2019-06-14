(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationReturnFlow.
(******************************************************************************)
unit UXMLAgentIrrigationReturnFlow;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationReturnFlow = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
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

procedure TXMLAgentIrrigationReturnFlow.PopulatePropertyLists;
const OPNAME = 'TXMLAgentIrrigationReturnFlow.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'ReturnFlowInterpolationType';
    FAllDescriptions.CommaText := '"Return Flow interpolation type"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationReturnFlow.AddSectionData (AModule    : INetworkModule;
                                                        ASectionNo : Integer;
                                                        ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationReturnFlow.AddSectionData';
var
  LIrrigationModule : IIrrigationModule;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'IrrigationReturnFlow';

      LSectionNode := ARootNode.ChildNodes['IrrigationReturnFlow'];
      LSectionNode.ChildNodes['ReturnFlowInterpolationType'].Text := IntToStr(LIrrigationModule.ReturnFlowInterpolationType);
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LIrrigationModule.NoOfReturnFlowDataPoints - 1 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.AddChild('Growth');
        LNode.ChildNodes['Year'].Text   := IntToStr(LIrrigationModule.ReturnFlowYearByIndex[LIndex]);
        LNode.ChildNodes['Growth'].Text := FloatToStr(LIrrigationModule.ReturnFlowValueByIndex[LIndex]);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationReturnFlow.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationReturnFlow.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationReturnFlow.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationReturnFlow.DoAdditionalValidation (AContext          : TValidationContext;
                                                               APropertyName     : String;
                                                               AFieldIndex       : String;
                                                               AXSDDoc           : IXMLDocument;
                                                               AXMLDocumentIn    : IXMLDocument;
                                                               AXMLDocumentOut   : IXMLDocument;
                                                               AStopOnFirstError : Boolean;
                                                               AErrorList        : TStringList;
                                                               AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationReturnFlow.DoAdditionalValidation';
var
  LResult        : Boolean;
  LSectionNode   : IXMLNode;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LIndex         : Integer;
  LRootNode      : IXMLNode;
  LTempResult    : Boolean;
  LXSDSchemaNode : IXMLNode;
  LErrorMsg      : String;
begin
  Result := FALSE;
  try
    LRootNode      := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['IrrigationReturnFlow'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Year', 'Year', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Year,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // Growth
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Growth') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Growth', 'Growth', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Growth,' + IntToStr(LIndex+1));
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


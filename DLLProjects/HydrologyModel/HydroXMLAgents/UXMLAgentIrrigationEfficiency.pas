(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationEfficiency.
(******************************************************************************)
unit UXMLAgentIrrigationEfficiency;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationEfficiency = class(TXMLAgent)
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

procedure TXMLAgentIrrigationEfficiency.PopulatePropertyLists;
const OPNAME = 'TXMLAgentIrrigationEfficiency.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'EfficiencyInterpolationType';
    FAllDescriptions.CommaText := '"Efficiency interpolation type"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationEfficiency.AddSectionData (AModule    : INetworkModule;
                                                        ASectionNo : Integer;
                                                        ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationEfficiency.AddSectionData';
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
      ARootNode.ChildNodes['Section'].Text := 'IrrigationEfficiency';

      LSectionNode := ARootNode.ChildNodes['IrrigationEfficiency'];
      LSectionNode.ChildNodes['EfficiencyInterpolationType'].Text := IntToStr(LIrrigationModule.EfficiencyInterpolationType);
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LIrrigationModule.NoOfEfficiencyDataPoints - 1 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.AddChild('Efficiency');
        LNode.ChildNodes['Year'].Text       := IntToStr(LIrrigationModule.EfficiencyYearByIndex[LIndex]);
        LNode.ChildNodes['Efficiency'].Text := FloatToStr(LIrrigationModule.EfficiencyValueByIndex[LIndex]);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationEfficiency.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationEfficiency.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationEfficiency.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationEfficiency.DoAdditionalValidation (AContext          : TValidationContext;
                                                               APropertyName     : String;
                                                               AFieldIndex       : String;
                                                               AXSDDoc           : IXMLDocument;
                                                               AXMLDocumentIn    : IXMLDocument;
                                                               AXMLDocumentOut   : IXMLDocument;
                                                               AStopOnFirstError : Boolean;
                                                               AErrorList        : TStringList;
                                                               AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationEfficiency.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['IrrigationEfficiency'];
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

    // Efficiency
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Efficiency') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Efficiency', 'Efficiency', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Efficiency,' + IntToStr(LIndex+1));
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


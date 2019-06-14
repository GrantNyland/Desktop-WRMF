(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationFactors.
(******************************************************************************)
unit UXMLAgentIrrigationFactors;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationFactors = class(TXMLAgent)
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

procedure TXMLAgentIrrigationFactors.AddSectionData (AModule    : INetworkModule;
                                                     ASectionNo : Integer;
                                                     ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationFactors.AddSectionData';
var
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LIrrigationModule : IIrrigationModule;
  LMonthIndex       : Integer;
  LNode             : IXMLNode;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'IrrigationFactors';

      LSectionNode  := ARootNode.ChildNodes['IrrigationFactors'];
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LMonthIndex := 1 to 12 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Month');
        LNode.AddChild('PIndexFactor');
        LNode.AddChild('RainfallFactor');
        LNode.AddChild('CropFactor');
        LNode.AddChild('APanFactor');
        LNode.ChildNodes['Month'].Text          := IntToStr(LMonthIndex);
        LNode.ChildNodes['PIndexFactor'].Text   := FloatToStr(LIrrigationModule.PIndexFactorByMonth[LMonthIndex]);
        LNode.ChildNodes['RainfallFactor'].Text := FloatToStr(LIrrigationModule.RainfallFactorByMonth[LMonthIndex]);
        LNode.ChildNodes['CropFactor'].Text     := FloatToStr(LIrrigationModule.CropFactorByMonth[LMonthIndex]);
        LNode.ChildNodes['APanFactor'].Text     := FloatToStr(LIrrigationModule.APanFactorByMonth[LMonthIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationFactors.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationFactors.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationFactors.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationFactors.DoAdditionalValidation (AContext          : TValidationContext;
                                                            APropertyName     : String;
                                                            AFieldIndex       : String;
                                                            AXSDDoc           : IXMLDocument;
                                                            AXMLDocumentIn    : IXMLDocument;
                                                            AXMLDocumentOut   : IXMLDocument;
                                                            AStopOnFirstError : Boolean;
                                                            AErrorList        : TStringList;
                                                            AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationFactors.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['IrrigationFactors'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // PIndexFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'PIndexFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'PIndexFactor', 'PIndex factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('PIndexFactor,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // RainfallFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'RainfallFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'RainfallFactor', 'Rainfall factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('RainfallFactor,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // CropFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'CropFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'CropFactor', 'Crop factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('CropFactor,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // APanFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'APanFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'APanFactor', 'APan factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('APanFactor,' + IntToStr(LIndex+1));
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


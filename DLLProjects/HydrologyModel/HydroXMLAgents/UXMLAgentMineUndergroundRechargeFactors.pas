(******************************************************************************)
(* This unit contains the class TXMLAgentMineUndergroundRechargeFactors.
(******************************************************************************)
unit UXMLAgentMineUndergroundRechargeFactors;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineUndergroundRechargeFactors = class(TXMLAgent)
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

procedure TXMLAgentMineUndergroundRechargeFactors.AddSectionData (AModule    : INetworkModule;
                                                                  ASectionNo : Integer;
                                                                  ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineUndergroundRechargeFactors.AddSectionData';
var
  LMine         : IMineModule;
  LUnderground  : IUndergroundSection;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LUnderground := LMine.UndergroundSectionBySectionNo[ASectionNo];
      if (LUnderground <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LUnderground.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineUndergroundRechargeFactors';

        LSectionNode  := ARootNode.ChildNodes['MineUndergroundRechargeFactors'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 1 to 12 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Month');
          LNode.AddChild('BoardAndPillarRechargeFactor');
          LNode.AddChild('HighExtractionRechargeFactor');
          LNode.ChildNodes['Month'].Text := IntToStr(LIndex);
          LNode.ChildNodes['BoardAndPillarRechargeFactor'].Text   := FloatToStr(LUnderground.BoardAndPillarRechargeFactorByMonth[LIndex]);
          LNode.ChildNodes['HighExtractionRechargeFactor'].Text   := FloatToStr(LUnderground.HighExtractionRechargeFactorByMonth[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundRechargeFactors.XSDText : String;
const OPNAME = 'TXMLAgentMineUndergroundRechargeFactors.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineUndergroundRechargeFactors.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundRechargeFactors.DoAdditionalValidation (AContext          : TValidationContext;
                                                                         APropertyName     : String;
                                                                         AFieldIndex       : String;
                                                                         AXSDDoc           : IXMLDocument;
                                                                         AXMLDocumentIn    : IXMLDocument;
                                                                         AXMLDocumentOut   : IXMLDocument;
                                                                         AStopOnFirstError : Boolean;
                                                                         AErrorList        : TStringList;
                                                                         AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineUndergroundRechargeFactors.DoAdditionalValidation';
var
  LResult             : Boolean;
  LSectionNode        : IXMLNode;
  LDataListNode       : IXMLNode;
  LNode               : IXMLNode;
  LIndex              : Integer;
  LRootNode           : IXMLNode;
  LTempResult         : Boolean;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
begin
  Result := FALSE;
  try
    LRootNode           := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode        := LRootNode.ChildNodes['MineUndergroundRechargeFactors'];
    LDataListNode       := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode      := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // Month
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Month') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Month', 'Month', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Month,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // BoardAndPillarRechargeFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'BoardAndPillarRechargeFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'BoardAndPillarRechargeFactor', 'Board and pillar recharge factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('BoardAndPillarRechargeFactor,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // HighExtractionRechargeFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'HighExtractionRechargeFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'HighExtractionRechargeFactor', 'High extraction recharge factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('HighExtractionRechargeFactor,' + IntToStr(LIndex+1));
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


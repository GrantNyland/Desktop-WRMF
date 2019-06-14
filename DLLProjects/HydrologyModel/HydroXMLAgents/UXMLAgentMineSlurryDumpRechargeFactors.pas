(******************************************************************************)
(* This unit contains the class TXMLAgentMineSlurryDumpRechargeFactors.
(******************************************************************************)
unit UXMLAgentMineSlurryDumpRechargeFactors;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineSlurryDumpRechargeFactors = class(TXMLAgent)
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

procedure TXMLAgentMineSlurryDumpRechargeFactors.AddSectionData (AModule    : INetworkModule;
                                                                 ASectionNo : Integer;
                                                                 ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineSlurryDumpRechargeFactors.AddSectionData';
var
  LMine         : IMineModule;
  LSlurryDump   : ISlurryDump;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LSlurryDump := LMine.SlurryDumpBySectionNo[ASectionNo];
      if (LSlurryDump <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text := IntToStr(LSlurryDump.SectionNo);
        ARootNode.ChildNodes['Section'].Text   := 'MineSlurryDumpRechargeFactors';

        LSectionNode  := ARootNode.ChildNodes['MineSlurryDumpRechargeFactors'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 1 to 12 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Month');
          LNode.AddChild('SlurryRechargeFactor');
          LNode.ChildNodes['Month'].Text := IntToStr(LIndex);
          LNode.ChildNodes['SlurryRechargeFactor'].Text := FloatToStr(LSlurryDump.RechargeFactorByMonth[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpRechargeFactors.XSDText : String;
const OPNAME = 'TXMLAgentMineSlurryDumpRechargeFactors.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineSlurryDumpRechargeFactors.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpRechargeFactors.DoAdditionalValidation (AContext          : TValidationContext;
                                                                        APropertyName     : String;
                                                                        AFieldIndex       : String;
                                                                        AXSDDoc           : IXMLDocument;
                                                                        AXMLDocumentIn    : IXMLDocument;
                                                                        AXMLDocumentOut   : IXMLDocument;
                                                                        AStopOnFirstError : Boolean;
                                                                        AErrorList        : TStringList;
                                                                        AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineSlurryDumpRechargeFactors.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['MineSlurryDumpRechargeFactors'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

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

    // SlurryRechargeFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'SlurryRechargeFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'SlurryRechargeFactor', 'Slurry dump recharge factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('SlurryRechargeFactor,' + IntToStr(LIndex+1));
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


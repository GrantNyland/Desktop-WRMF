(******************************************************************************)
(* This unit contains the class TXMLAgentMineOpencastPitRechargeFactors.
(******************************************************************************)
unit UXMLAgentMineOpencastPitRechargeFactors;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineOpencastPitRechargeFactors = class(TXMLAgent)
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

procedure TXMLAgentMineOpencastPitRechargeFactors.AddSectionData (AModule    : INetworkModule;
                                                                  ASectionNo : Integer;
                                                                  ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineOpencastPitRechargeFactors.AddSectionData';
var
  LMine         : IMineModule;
  LOpencastPit  : IOpencastPit;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LOpencastPit := LMine.OpencastPitBySectionNo[ASectionNo];
      if (LOpencastPit <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text := IntToStr(LOpencastPit.SectionNo);
        ARootNode.ChildNodes['Section'].Text   := 'MineOpencastPitRechargeFactors';

        LSectionNode  := ARootNode.ChildNodes['MineOpencastPitRechargeFactors'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 1 to 12 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Month');
          LNode.AddChild('DisturbedAreaRechargeFactor');
          LNode.AddChild('DisturbedWorkingsAreaRechargeFactor');
          LNode.ChildNodes['Month'].Text := IntToStr(LIndex);
          LNode.ChildNodes['DisturbedAreaRechargeFactor'].Text   := FloatToStr(LOpencastPit.DisturbedAreaRechargeFactorByMonth[LIndex]);
          LNode.ChildNodes['DisturbedWorkingsAreaRechargeFactor'].Text   := FloatToStr(LOpencastPit.DisturbedWorkingsAreaRechargeFactorByMonth[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitRechargeFactors.XSDText : String;
const OPNAME = 'TXMLAgentMineOpencastPitRechargeFactors.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineOpencastPitRechargeFactors.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitRechargeFactors.DoAdditionalValidation (AContext          : TValidationContext;
                                                                         APropertyName     : String;
                                                                         AFieldIndex       : String;
                                                                         AXSDDoc           : IXMLDocument;
                                                                         AXMLDocumentIn    : IXMLDocument;
                                                                         AXMLDocumentOut   : IXMLDocument;
                                                                         AStopOnFirstError : Boolean;
                                                                         AErrorList        : TStringList;
                                                                         AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineOpencastPitRechargeFactors.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['MineOpencastPitRechargeFactors'];
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

    // DisturbedAreaRechargeFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'DisturbedAreaRechargeFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'DisturbedAreaRechargeFactor', 'Disturbed area recharge factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('DisturbedAreaRechargeFactor,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // DisturbedWorkingsAreaRechargeFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'DisturbedWorkingsAreaRechargeFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'DisturbedWorkingsAreaRechargeFactor', 'Disturbed workings area recharge factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('DisturbedWorkingsAreaRechargeFactor,' + IntToStr(LIndex+1));
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


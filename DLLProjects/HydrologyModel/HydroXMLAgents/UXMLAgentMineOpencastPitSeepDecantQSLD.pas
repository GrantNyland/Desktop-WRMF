(******************************************************************************)
(* This unit contains the class TXMLAgentMineOpencastPitSeepDecantQSLD.
(******************************************************************************)
unit UXMLAgentMineOpencastPitSeepDecantQSLD;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineOpencastPitSeepDecantQSLD = class(TXMLAgent)
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

procedure TXMLAgentMineOpencastPitSeepDecantQSLD.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineOpencastPitSeepDecantQSLD.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'StdDevSeepageDecant';
    FAllDescriptions.CommaText := '"Standard deviation Seepage decant"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineOpencastPitSeepDecantQSLD.AddSectionData (AModule    : INetworkModule;
                                                                 ASectionNo : Integer;
                                                                 ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineOpencastPitSeepDecantQSLD.AddSectionData';
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
        ARootNode.ChildNodes['Section'].Text   := 'MineOpencastPitSeepDecantQSLD';

        LSectionNode  := ARootNode.ChildNodes['MineOpencastPitSeepDecantQSLD'];
        LSectionNode.ChildNodes['StdDevSeepageDecant'].Text := FloatToStr(LOpencastPit.StdDevSeepageDecant);
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LOpencastPit.SeepageDecantQvsSLDNoOfPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Flow');
          LNode.AddChild('Load');
          LNode.ChildNodes['Flow'].Text := FloatToStr(LOpencastPit.SeepageDecantQvsSLDFlowRefByIndex[LIndex]);
          LNode.ChildNodes['Load'].Text := FloatToStr(LOpencastPit.SeepageDecantQvsSLDLoadByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitSeepDecantQSLD.XSDText : String;
const OPNAME = 'TXMLAgentMineOpencastPitSeepDecantQSLD.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineOpencastPitSeepDecantQSLD.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitSeepDecantQSLD.DoAdditionalValidation (AContext          : TValidationContext;
                                                                        APropertyName     : String;
                                                                        AFieldIndex       : String;
                                                                        AXSDDoc           : IXMLDocument;
                                                                        AXMLDocumentIn    : IXMLDocument;
                                                                        AXMLDocumentOut   : IXMLDocument;
                                                                        AStopOnFirstError : Boolean;
                                                                        AErrorList        : TStringList;
                                                                        AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineOpencastPitSeepDecantQSLD.DoAdditionalValidation';
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
    LRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LRootNode.ChildNodes['MineOpencastPitSeepDecantQSLD'];
    LDataListNode   := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode  := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // Flow
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Flow') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Flow', 'Flow', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Flow,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // Load
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Load') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Load', 'Load', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Load,' + IntToStr(LIndex+1));
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


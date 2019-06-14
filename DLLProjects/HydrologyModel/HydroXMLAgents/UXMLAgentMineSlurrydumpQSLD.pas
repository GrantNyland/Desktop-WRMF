(******************************************************************************)
(* This unit contains the class TXMLAgentMineSlurryDumpQSLD.
(******************************************************************************)
unit UXMLAgentMineSlurryDumpQSLD;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineSlurryDumpQSLD = class(TXMLAgent)
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

procedure TXMLAgentMineSlurryDumpQSLD.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineSlurryDumpQSLD.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'StdDevSlurryDump';
    FAllDescriptions.CommaText := '"Standard deviation SlurryDump Q vs SLD"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineSlurryDumpQSLD.AddSectionData (AModule    : INetworkModule;
                                                      ASectionNo : Integer;
                                                      ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineSlurryDumpQSLD.AddSectionData';
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
        ARootNode.ChildNodes['Section'].Text   := 'MineSlurryDumpQSLD';

        LSectionNode := ARootNode.ChildNodes['MineSlurryDumpQSLD'];
        LSectionNode.ChildNodes['StdDevSlurryDump'].Text := FloatToStr(LSlurryDump.StdDev);
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LSlurryDump.QvsSLDNoOfPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Flow');
          LNode.AddChild('Load');
          LNode.ChildNodes['Flow'].Text := FloatToStr(LSlurryDump.QvsSLDFlowRefByIndex[LIndex]);
          LNode.ChildNodes['Load'].Text := FloatToStr(LSlurryDump.QvsSLDLoadByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpQSLD.XSDText : String;
const OPNAME = 'TXMLAgentMineSlurryDumpQSLD.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineSlurryDumpQSLD.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpQSLD.DoAdditionalValidation (AContext          : TValidationContext;
                                                             APropertyName     : String;
                                                             AFieldIndex       : String;
                                                             AXSDDoc           : IXMLDocument;
                                                             AXMLDocumentIn    : IXMLDocument;
                                                             AXMLDocumentOut   : IXMLDocument;
                                                             AStopOnFirstError : Boolean;
                                                             AErrorList        : TStringList;
                                                             AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineSlurryDumpQSLD.DoAdditionalValidation';
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
    LRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LRootNode.ChildNodes['MineSlurryDumpQSLD'];
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


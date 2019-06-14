(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffPavedArea.
(******************************************************************************)
unit UXMLAgentRunOffPavedArea;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffPavedArea = class(TXMLAgent)
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

procedure TXMLAgentRunOffPavedArea.AddSectionData (AModule    : INetworkModule;
                                                   ASectionNo : Integer;
                                                   ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffPavedArea.AddSectionData';
var
  LRunOffModule : IRunOffModule;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LPavedArea    : IRunOffPavedArea;
  LIndex        : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffPavedArea';

      if (LRunOffModule.PavedArea <> nil) then
      begin
        LPavedArea := LRunOffModule.PavedArea;
        LSectionNode := ARootNode.ChildNodes['RunOffPavedArea'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LPavedArea.NumberOfYears - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Proportion');
          LNode.ChildNodes['Year'].Text  := IntToStr(LPavedArea.Year[LIndex]);
          LNode.ChildNodes['Proportion'].Text := FloatToStr(LPavedArea.Proportion[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffPavedArea.XSDText : String;
const OPNAME = 'TXMLAgentRunOffPavedArea.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffPavedArea.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffPavedArea.DoAdditionalValidation (AContext          : TValidationContext;
                                                          APropertyName     : String;
                                                          AFieldIndex       : String;
                                                          AXSDDoc           : IXMLDocument;
                                                          AXMLDocumentIn    : IXMLDocument;
                                                          AXMLDocumentOut   : IXMLDocument;
                                                          AStopOnFirstError : Boolean;
                                                          AErrorList        : TStringList;
                                                          AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffPavedArea.DoAdditionalValidation';
var
  LResult             : Boolean;
  LInputRootNode      : IXMLNode;
  LOutputRootNode     : IXMLNode;
  LSectionNode        : IXMLNode;
  LPavedAreaDataNode  : IXMLNode;
  LTempResult         : Boolean;
  LIndex              : Integer;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
  LNode               : IXMLNode;
  LModuleNumber       : String;
begin
  Result := FALSE;
  try
    LInputRootNode     := AXMLDocumentIn.DocumentElement;
    LOutputRootNode    := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode       := LOutputRootNode.ChildNodes['RunOffPavedArea'];
    LXSDSchemaNode     := GetXSDSchemaNode(AXSDDoc);
    LPavedAreaDataNode := LSectionNode.ChildNodes['DataList'];
    LModuleNumber      := Trim(LOutputRootNode.ChildNodes['ModuleNumber'].Text);

    LResult            := TRUE;
    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LPavedAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LPavedAreaDataNode.ChildNodes.Get(LIndex);
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

    // Proportion
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Proportion') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LPavedAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LPavedAreaDataNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Proportion', 'Proportion', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Proportion,' + IntToStr(LIndex+1));
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


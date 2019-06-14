(******************************************************************************)
(* This unit contains the class TXMLAgentPanData.
(******************************************************************************)
unit UXMLAgentPanData;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentPanData = class(TXMLAgent)
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

procedure TXMLAgentPanData.AddSectionData (AModule    : INetworkModule;
                                           ASectionNo : Integer;
                                           ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentPanData.AddSectionData';
var
  LModule        : INetworkModule;
  LSectionNode   : IXMLNode;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LPanData       : IPan;
  LIndex         : Integer;
begin
  try
    LModule := FHydrologyModel.Network.ModuleByID[AModule.ModuleID];
    if (LModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text    := 'PanData';

      LSectionNode  := ARootNode.ChildNodes['PanData'];
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LModule.PanCount - 1 do
      begin
        LPanData := LModule.PanByIndex[LIndex];
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Month');
        LNode.AddChild('PanEvaporation');
        LNode.AddChild('PanFactor');
        LNode.ChildNodes['Month'].Text       := IntToStr(LPanData.Month);
        LNode.ChildNodes['PanEvaporation'].Text := FloatToStr(LPanData.Evaporation);
        LNode.ChildNodes['PanFactor'].Text   := FloatToStr(LPanData.Factor);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentPanData.XSDText : String;
const OPNAME = 'TXMLAgentPanData.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('PanData.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentPanData.DoAdditionalValidation (AContext          : TValidationContext;
                                                  APropertyName     : String;
                                                  AFieldIndex       : String;
                                                  AXSDDoc           : IXMLDocument;
                                                  AXMLDocumentIn    : IXMLDocument;
                                                  AXMLDocumentOut   : IXMLDocument;
                                                  AStopOnFirstError : Boolean;
                                                  AErrorList        : TStringList;
                                                  AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentPanData.DoAdditionalValidation';
var
  LResult         : Boolean;
  LSectionNode    : IXMLNode;
  LDataListNode   : IXMLNode;
  LNode           : IXMLNode;
  LIndex          : Integer;
  LRootNode       : IXMLNode;
  LTempResult     : Boolean;
  LXSDSchemaNode  : IXMLNode;
  LErrorMsg       : String;
begin
  Result := FALSE;
  try
    LRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LRootNode.ChildNodes['PanData'];
    LDataListNode   := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode  := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // PanEvaporation
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'PanEvaporation') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'PanEvaporation', 'PanEvaporation', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('PanEvaporation,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // PanFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'PanFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'PanFactor', 'PanFactor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('PanFactor,' + IntToStr(LIndex+1));
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


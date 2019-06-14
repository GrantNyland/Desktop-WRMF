(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffGroundWaterAbstraction.
(******************************************************************************)
unit UXMLAgentRunOffGroundWaterAbstraction;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffGroundWaterAbstraction = class(TXMLAgent)
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

procedure TXMLAgentRunOffGroundWaterAbstraction.AddSectionData (AModule    : INetworkModule;
                                                                ASectionNo : Integer;
                                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffGroundWaterAbstraction.AddSectionData';
var
  LRunOffModule   : IRunOffModule;
  LSectionNode    : IXMLNode;
  LDataListNode   : IXMLNode;
  LNode           : IXMLNode;
  LGWAbstraction  : IRunOffGroundWaterAbstraction;
  LIndex          : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text        := 'RunOffGroundWaterAbstraction';

      if (LRunOffModule.GroundWaterAbstraction <> nil) then
      begin
        LGWAbstraction := LRunOffModule.GroundWaterAbstraction;
        LSectionNode := ARootNode.ChildNodes['RunOffGroundWaterAbstraction'];
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LGWAbstraction.NumberOfYears - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Abstraction');
          LNode.ChildNodes['Year'].Text  := IntToStr(LGWAbstraction.Year[LIndex]);
          LNode.ChildNodes['Abstraction'].Text := FloatToStr(LGWAbstraction.AbstractionValue[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffGroundWaterAbstraction.XSDText : String;
const OPNAME = 'TXMLAgentRunOffGroundWaterAbstraction.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffGroundWaterAbstraction.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffGroundWaterAbstraction.DoAdditionalValidation (AContext          : TValidationContext;
                                                                       APropertyName     : String;
                                                                       AFieldIndex       : String;
                                                                       AXSDDoc           : IXMLDocument;
                                                                       AXMLDocumentIn    : IXMLDocument;
                                                                       AXMLDocumentOut   : IXMLDocument;
                                                                       AStopOnFirstError : Boolean;
                                                                       AErrorList        : TStringList;
                                                                       AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffGroundWaterAbstraction.DoAdditionalValidation';
var
  LResult                  : Boolean;
  LInputRootNode           : IXMLNode;
  LOutputRootNode          : IXMLNode;
  LSectionNode             : IXMLNode;
  LGWAbstractionDataNode   : IXMLNode;
  LTempResult              : Boolean;
  LIndex                   : Integer;
  LXSDSchemaNode           : IXMLNode;
  LErrorMsg                : String;
  LNode                    : IXMLNode;
  LModuleNumber            : String;
begin
  Result := FALSE;
  try
    LInputRootNode          := AXMLDocumentIn.DocumentElement;
    LOutputRootNode         := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode            := LOutputRootNode.ChildNodes['RunOffGroundWaterAbstraction'];
    LXSDSchemaNode          := GetXSDSchemaNode(AXSDDoc);
    LGWAbstractionDataNode  := LSectionNode.ChildNodes['DataList'];
    LModuleNumber           := Trim(LOutputRootNode.ChildNodes['ModuleNumber'].Text);
    LResult                 := TRUE;

    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LGWAbstractionDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LGWAbstractionDataNode.ChildNodes.Get(LIndex);
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

    // Abstraction
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Abstraction') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LGWAbstractionDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LGWAbstractionDataNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Abstraction', 'Abstraction', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Abstraction,' + IntToStr(LIndex+1));
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


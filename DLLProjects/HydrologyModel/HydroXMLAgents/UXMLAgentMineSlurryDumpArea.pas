(******************************************************************************)
(* This unit contains the class TXMLAgentMineSlurryDumpArea.
(******************************************************************************)
unit UXMLAgentMineSlurryDumpArea;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineSlurryDumpArea = class(TXMLAgent)
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

procedure TXMLAgentMineSlurryDumpArea.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineSlurryDumpArea.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'SlurryDumpInterpolationOption';
    FAllDescriptions.CommaText := '"Slurry dump interpolation option"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineSlurryDumpArea.AddSectionData (AModule    : INetworkModule;
                                                      ASectionNo : Integer;
                                                      ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineSlurryDumpArea.AddSectionData';
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
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LSlurryDump.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineSlurryDumpArea';

        LSectionNode := ARootNode.ChildNodes['MineSlurryDumpArea'];
        LSectionNode.ChildNodes['SlurryDumpInterpolationOption'].Text := IntToStr(LSlurryDump.InterpolationOption);
        LDataListNode := LSectionNode.ChildNodes['GrowthDataList'];
        for LIndex := 0 to LSlurryDump.NoOfAreaGrowthPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Growth');
          LNode.ChildNodes['Year'].Text   := IntToStr(LSlurryDump.AreaGrowthYearByIndex[LIndex]);
          LNode.ChildNodes['Growth'].Text := FloatToStr(LSlurryDump.AreaGrowthFactorByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpArea.XSDText : String;
const OPNAME = 'TXMLAgentMineSlurryDumpArea.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineSlurryDumpArea.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpArea.DoAdditionalValidation (AContext          : TValidationContext;
                                                             APropertyName     : String;
                                                             AFieldIndex       : String;
                                                             AXSDDoc           : IXMLDocument;
                                                             AXMLDocumentIn    : IXMLDocument;
                                                             AXMLDocumentOut   : IXMLDocument;
                                                             AStopOnFirstError : Boolean;
                                                             AErrorList        : TStringList;
                                                             AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineSlurryDumpArea.DoAdditionalValidation';
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
    LRootNode      := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['MineSlurryDumpArea'];
    LDataListNode  := LSectionNode.ChildNodes['GrowthDataList'];
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

    // Growth
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Growth') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Growth', 'Growth', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Growth,' + IntToStr(LIndex+1));
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


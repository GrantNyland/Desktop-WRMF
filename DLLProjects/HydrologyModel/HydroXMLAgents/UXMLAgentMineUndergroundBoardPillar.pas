(******************************************************************************)
(* This unit contains the class TXMLAgentMineUndergroundBoardPillar.
(******************************************************************************)
unit UXMLAgentMineUndergroundBoardPillar;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineUndergroundBoardPillar = class(TXMLAgent)
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

procedure TXMLAgentMineUndergroundBoardPillar.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineUndergroundBoardPillar.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'BoardAndPillarInterpolationOption';
    FAllDescriptions.CommaText := '"Board and pillar interpolation option"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineUndergroundBoardPillar.AddSectionData (AModule    : INetworkModule;
                                                              ASectionNo : Integer;
                                                              ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineUndergroundBoardPillar.AddSectionData';
var
  LMine             : IMineModule;
  LUnderground      : IUndergroundSection;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LUnderground := LMine.UndergroundSectionBySectionNo[ASectionNo];
      if (LUnderground <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LUnderground.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineUndergroundBoardPillar';

        LSectionNode := ARootNode.ChildNodes['MineUndergroundBoardPillar'];
        LSectionNode.ChildNodes['BoardAndPillarInterpolationOption'].Text := IntToStr(LUnderground.BoardAndPillarInterpolationOption);
        LDataListNode := LSectionNode.ChildNodes['GrowthDataList'];
        for LIndex := 0 to LUnderground.NoOfBoardAndPillarGrowthPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Growth');
          LNode.ChildNodes['Year'].Text   := IntToStr(LUnderground.BoardAndPillarGrowthYearByIndex[LIndex]);
          LNode.ChildNodes['Growth'].Text := FloatToStr(LUnderground.BoardAndPillarGrowthFactorByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundBoardPillar.XSDText : String;
const OPNAME = 'TXMLAgentMineUndergroundBoardPillar.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineUndergroundBoardPillar.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundBoardPillar.DoAdditionalValidation (AContext          : TValidationContext;
                                                                     APropertyName     : String;
                                                                     AFieldIndex       : String;
                                                                     AXSDDoc           : IXMLDocument;
                                                                     AXMLDocumentIn    : IXMLDocument;
                                                                     AXMLDocumentOut   : IXMLDocument;
                                                                     AStopOnFirstError : Boolean;
                                                                     AErrorList        : TStringList;
                                                                     AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineUndergroundBoardPillar.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['MineUndergroundBoardPillar'];
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


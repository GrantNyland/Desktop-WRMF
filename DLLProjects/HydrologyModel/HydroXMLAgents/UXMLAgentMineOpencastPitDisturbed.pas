(******************************************************************************)
(* This unit contains the class TXMLAgentMineOpencastPitDisturbed.
(******************************************************************************)
unit UXMLAgentMineOpencastPitDisturbed;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineOpencastPitDisturbed = class(TXMLAgent)
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

procedure TXMLAgentMineOpencastPitDisturbed.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineOpencastPitDisturbed.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'DisturbedAreaInterpolationOption';
    FAllDescriptions.CommaText := '"Distirbed area interpolation option"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineOpencastPitDisturbed.AddSectionData (AModule    : INetworkModule;
                                                            ASectionNo : Integer;
                                                            ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineOpencastPitDisturbed.AddSectionData';
var
  LMine          : IMineModule;
  LOpencastPit   : IOpencastPit;
  LSectionNode   : IXMLNode;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LIndex         : Integer;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LOpencastPit := LMine.OpencastPitBySectionNo[ASectionNo];
      if (LOpencastPit <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LOpencastPit.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineOpencastPitDisturbed';

        LSectionNode := ARootNode.ChildNodes['MineOpencastPitDisturbed'];
        LSectionNode.ChildNodes['DisturbedAreaInterpolationOption'].Text := IntToStr(LOpencastPit.DisturbedAreaInterpolationOption);
        LDataListNode := LSectionNode.ChildNodes['GrowthDataList'];
        for LIndex := 0 to LOpencastPit.NoOfDisturbedAreaGrowthPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Growth');
          LNode.ChildNodes['Year'].Text   := IntToStr(LOpencastPit.DisturbedAreaGrowthYearByIndex[LIndex]);
          LNode.ChildNodes['Growth'].Text := FloatToStr(LOpencastPit.DisturbedAreaGrowthFactorByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitDisturbed.XSDText : String;
const OPNAME = 'TXMLAgentMineOpencastPitDisturbed.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineOpencastPitDisturbed.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitDisturbed.DoAdditionalValidation (AContext          : TValidationContext;
                                                                   APropertyName     : String;
                                                                   AFieldIndex       : String;
                                                                   AXSDDoc           : IXMLDocument;
                                                                   AXMLDocumentIn    : IXMLDocument;
                                                                   AXMLDocumentOut   : IXMLDocument;
                                                                   AStopOnFirstError : Boolean;
                                                                   AErrorList        : TStringList;
                                                                   AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineOpencastPitDisturbed.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['MineOpencastPitDisturbed'];
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


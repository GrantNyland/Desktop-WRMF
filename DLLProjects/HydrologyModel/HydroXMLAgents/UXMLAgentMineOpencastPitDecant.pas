(******************************************************************************)
(* This unit contains the class TXMLAgentMineOpencastPitDecant.
(******************************************************************************)
unit UXMLAgentMineOpencastPitDecant;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineOpencastPitDecant = class(TXMLAgent)
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

procedure TXMLAgentMineOpencastPitDecant.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineOpencastPitDecant.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'InspoilsDecantInterpolationOption';
    FAllDescriptions.CommaText := '"Inspoils Decant interpolation option"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineOpencastPitDecant.AddSectionData (AModule    : INetworkModule;
                                                         ASectionNo : Integer;
                                                         ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineOpencastPitDecant.AddSectionData';
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
        ARootNode.ChildNodes['Section'].Text   := 'MineOpencastPitDecant';

        LSectionNode := ARootNode.ChildNodes['MineOpencastPitDecant'];
        LSectionNode.ChildNodes['InspoilsDecantInterpolationOption'].Text := IntToStr(LOpencastPit.InspoilsDecantInterpolationOption);
        LDataListNode := LSectionNode.ChildNodes['GrowthDataList'];
        for LIndex := 0 to LOpencastPit.NoOfInspoilsDecantGrowthPoints - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Growth');
          LNode.ChildNodes['Year'].Text   := IntToStr(LOpencastPit.InspoilsDecantGrowthYearByIndex[LIndex]);
          LNode.ChildNodes['Growth'].Text := FloatToStr(LOpencastPit.InspoilsDecantGrowthFactorByIndex[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitDecant.XSDText : String;
const OPNAME = 'TXMLAgentMineOpencastPitDecant.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineOpencastPitDecant.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitDecant.DoAdditionalValidation (AContext          : TValidationContext;
                                                                APropertyName     : String;
                                                                AFieldIndex       : String;
                                                                AXSDDoc           : IXMLDocument;
                                                                AXMLDocumentIn    : IXMLDocument;
                                                                AXMLDocumentOut   : IXMLDocument;
                                                                AStopOnFirstError : Boolean;
                                                                AErrorList        : TStringList;
                                                                AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineOpencastPitDecant.DoAdditionalValidation';
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
    LSectionNode   := LRootNode.ChildNodes['MineOpencastPitDecant'];
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


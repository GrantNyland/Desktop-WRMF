(******************************************************************************)
(* This unit contains the class TXMLAgentMinePlantArea.
(******************************************************************************)
unit UXMLAgentMinePlantArea;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMinePlantArea = class(TXMLAgent)
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

procedure TXMLAgentMinePlantArea.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMinePlantArea.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'PlantAreaInterpolationType';
    FAllDescriptions.CommaText := '"Plant area interpolation type"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMinePlantArea.AddSectionData (AModule    : INetworkModule;
                                                 ASectionNo : Integer;
                                                 ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMinePlantArea.AddSectionData';
var
  LMineModule   : IMineModule;
  LSectionNode  : IXMLNode;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
begin
  try
    LMineModule := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMineModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'MinePlantArea';

      LSectionNode := ARootNode.ChildNodes['MinePlantArea'];
      LSectionNode.ChildNodes['PlantAreaInterpolationType'].Text := IntToStr(LMineModule.PlantAreaGrowthInterpolationType);
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LMineModule.NoOfPlantAreaGrowthPoints - 1 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.AddChild('Factor');
        LNode.ChildNodes['Year'].Text   := IntToStr(LMineModule.PlantAreaGrowthYearByIndex[LIndex]);
        LNode.ChildNodes['Factor'].Text := FloatToStr(LMineModule.PlantAreaGrowthFactorByIndex[LIndex]);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMinePlantArea.XSDText : String;
const OPNAME = 'TXMLAgentMinePlantArea.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MinePlantArea.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMinePlantArea.DoAdditionalValidation (AContext          : TValidationContext;
                                                        APropertyName     : String;
                                                        AFieldIndex       : String;
                                                        AXSDDoc           : IXMLDocument;
                                                        AXMLDocumentIn    : IXMLDocument;
                                                        AXMLDocumentOut   : IXMLDocument;
                                                        AStopOnFirstError : Boolean;
                                                        AErrorList        : TStringList;
                                                        AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMinePlantArea.DoAdditionalValidation';
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
    LRootNode           := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode        := LRootNode.ChildNodes['MinePlantArea'];
    LDataListNode       := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode      := GetXSDSchemaNode(AXSDDoc);

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

    // Factor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Factor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Factor', 'Factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Factor,' + IntToStr(LIndex+1));
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


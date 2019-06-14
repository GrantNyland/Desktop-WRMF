(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffAlienVegetation.
(******************************************************************************)
unit UXMLAgentRunOffAlienVegetation;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffAlienVegetation = class(TXMLAgent)
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

procedure TXMLAgentRunOffAlienVegetation.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffAlienVegetation.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'AlienVegetationAlgorithm,RiparianVegetationArea,TallTreeAreaPercentage,' +
                                'TallTreeAge,MediumTreeAreaPercentage,MediumTreeAge,' +
                                'TallSchrubAreaPercentage,TallSchrubAge,OptimalAreaPercentage';
    FAllDescriptions.CommaText := '"Alien vegetation algorithm","Riparian vegetation area","Tall tree area percentage",' +
                                  '"Tall tree age","Medium tree area percentage","Medium tree age",' +
                                  '"Tall schrub area percentage","Tall schrub age","Optimal area percentage"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffAlienVegetation.AddSectionData (AModule    : INetworkModule;
                                                         ASectionNo : Integer;
                                                         ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffAlienVegetation.AddSectionData';
var
  LRunOffModule    : IRunOffModule;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LNode            : IXMLNode;
  LAlienVegetation : IRunOffAlienVegetation;
  LIndex           : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffAlienVegetation';

      if (LRunOffModule.AlienVegetation <> nil) then
      begin
        LAlienVegetation := LRunOffModule.AlienVegetation;
        LSectionNode := ARootNode.ChildNodes['RunOffAlienVegetation'];
        LSectionNode.ChildNodes['AlienVegetationAlgorithm'].Text      := IntToStr(LAlienVegetation.Algorithm);
        LSectionNode.ChildNodes['RiparianVegetationArea'].Text        := FloatToStr(LAlienVegetation.RiparianVegetationArea);
        LSectionNode.ChildNodes['TallTreeAreaPercentage'].Text        := FloatToStr(LAlienVegetation.TallTreeAreaPercentage);
        LSectionNode.ChildNodes['TallTreeAge'].Text                   := FloatToStr(LAlienVegetation.TallTreeAge);
        LSectionNode.ChildNodes['MediumTreeAreaPercentage'].Text      := FloatToStr(LAlienVegetation.MediumTreeAreaPercentage);
        LSectionNode.ChildNodes['MediumTreeAge'].Text                 := FloatToStr(LAlienVegetation.MediumTreeAge);
        LSectionNode.ChildNodes['TallSchrubAreaPercentage'].Text      := FloatToStr(LAlienVegetation.TallSchrubAreaPercentage);
        LSectionNode.ChildNodes['TallSchrubAge'].Text                 := FloatToStr(LAlienVegetation.TallSchrubAge);
        LSectionNode.ChildNodes['OptimalAreaPercentage'].Text         := FloatToStr(LAlienVegetation.OptimalAreaPercentage);
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LAlienVegetation.NumberOfYears - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Area');
          LNode.ChildNodes['Year'].Text  := IntToStr(LAlienVegetation.Year[LIndex]);
          LNode.ChildNodes['Area'].Text := FloatToStr(LAlienVegetation.Area[LIndex]);
        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffAlienVegetation.XSDText : String;
const OPNAME = 'TXMLAgentRunOffAlienVegetation.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffAlienVegetation.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffAlienVegetation.DoAdditionalValidation (AContext          : TValidationContext;
                                                                APropertyName     : String;
                                                                AFieldIndex       : String;
                                                                AXSDDoc           : IXMLDocument;
                                                                AXMLDocumentIn    : IXMLDocument;
                                                                AXMLDocumentOut   : IXMLDocument;
                                                                AStopOnFirstError : Boolean;
                                                                AErrorList        : TStringList;
                                                                AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffAlienVegetation.DoAdditionalValidation';
var
  LResult                  : Boolean;
  LInputRootNode           : IXMLNode;
  LOutputRootNode          : IXMLNode;
  LSectionNode             : IXMLNode;
  LAlienVegAreaDataNode    : IXMLNode;
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
    LSectionNode            := LOutputRootNode.ChildNodes['RunOffAlienVegetation'];
    LXSDSchemaNode          := GetXSDSchemaNode(AXSDDoc);
    LAlienVegAreaDataNode   := LSectionNode.ChildNodes['DataList'];
    LModuleNumber           := Trim(LOutputRootNode.ChildNodes['ModuleNumber'].Text);

    LResult                 := TRUE;
    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LAlienVegAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LAlienVegAreaDataNode.ChildNodes.Get(LIndex);
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

    // Area
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Area') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LAlienVegAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LAlienVegAreaDataNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Area', 'Area', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Area,' + IntToStr(LIndex+1));
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


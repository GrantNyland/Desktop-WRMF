(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffAfforestation.
(******************************************************************************)
unit UXMLAgentRunOffAfforestation;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffAfforestation = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
    function XSDText : String; override;
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
    procedure AddPopulationData (ARootNode : IXMLNode); override;
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

procedure TXMLAgentRunOffAfforestation.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffAfforestation.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'AfforestationAlgorithm,PineAreaPercentage,PineRotationPeriod,' +
                                'EucalyptusAreaPercentage,EucalyptusRotationPeriod,WattleAreaPercentage,' +
                                'WattleRotationPeriod,OptimalAreaPercentage,SFRReductionMAR,' +
                                'SFRReductionLowFlows';
    FAllDescriptions.CommaText := '"Afforestation algorithm","Pine area percentage","Pine rotation period",' +
                                  '"Eucalyptus area percentage","Eucalyptus rotation period","Wattle area percentage",' +
                                  '"Wattle rotation period","Optimal area percentage","SFR reduction for MAR",' +
                                  '"SFR reduction for low flows"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffAfforestation.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentRunOffAfforestation.AddPopulationData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffAfforestation.AddSectionData (AModule    : INetworkModule;
                                                       ASectionNo : Integer;
                                                       ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffAfforestation.AddSectionData';
var
  LRunOffModule   : IRunOffModule;
  LSectionNode    : IXMLNode;
  LDataListNode   : IXMLNode;
  LNode           : IXMLNode;
  LAfforestation  : IRunOffAfforestation;
  LIndex          : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffAfforestation';

      if (LRunOffModule.Afforestation <> nil) then
      begin
        LAfforestation := LRunOffModule.Afforestation;
        LSectionNode := ARootNode.ChildNodes['RunOffAfforestation'];
        LSectionNode.ChildNodes['AfforestationAlgorithm'].Text   := IntToStr(LAfforestation.Algorithm);
        LSectionNode.ChildNodes['PineAreaPercentage'].Text       := FloatToStr(LAfforestation.PineAreaPercentage);
        LSectionNode.ChildNodes['PineRotationPeriod'].Text       := IntToStr(LAfforestation.PineRotationPeriod);
        LSectionNode.ChildNodes['EucalyptusAreaPercentage'].Text := FloatToStr(LAfforestation.EucalyptusAreaPercentage);
        LSectionNode.ChildNodes['EucalyptusRotationPeriod'].Text := IntToStr(LAfforestation.EucalyptusRotationPeriod);
        LSectionNode.ChildNodes['WattleAreaPercentage'].Text     := FloatToStr(LAfforestation.WattleAreaPercentage);
        LSectionNode.ChildNodes['WattleRotationPeriod'].Text     := IntToStr(LAfforestation.WattleRotationPeriod);
        LSectionNode.ChildNodes['OptimalAreaPercentage'].Text    := FloatToStr(LAfforestation.OptimalAreaPercentage);
        LSectionNode.ChildNodes['SFRReductionMAR'].Text          := FloatToStr(LAfforestation.SFRReductionMAR);
        LSectionNode.ChildNodes['SFRReductionLowFlows'].Text     := FloatToStr(LAfforestation.SFRReductionLowFlows);
        LDataListNode := LSectionNode.ChildNodes['DataList'];
        for LIndex := 0 to LAfforestation.NumberOfYears - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.AddChild('Area');
          LNode.ChildNodes['Year'].Text  := IntToStr(LAfforestation.Year[LIndex]);
          LNode.ChildNodes['Area'].Text := FloatToStr(LAfforestation.Area[LIndex]);
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffAfforestation.XSDText : String;
const OPNAME = 'TXMLAgentRunOffAfforestation.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffAfforestation.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffAfforestation.DoAdditionalValidation (AContext          : TValidationContext;
                                                              APropertyName     : String;
                                                              AFieldIndex       : String;
                                                              AXSDDoc           : IXMLDocument;
                                                              AXMLDocumentIn    : IXMLDocument;
                                                              AXMLDocumentOut   : IXMLDocument;
                                                              AStopOnFirstError : Boolean;
                                                              AErrorList        : TStringList;
                                                              AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffAfforestation.DoAdditionalValidation';
var
  LResult          : Boolean;
  LInputRootNode   : IXMLNode;
  LOutputRootNode  : IXMLNode;
  LSectionNode     : IXMLNode;
  LAffAreaDataNode : IXMLNode;
  LTempResult      : Boolean;
  LIndex           : Integer;
  LXSDSchemaNode   : IXMLNode;
  LErrorMsg        : String;
  LNode            : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode   := AXMLDocumentIn.DocumentElement;
    LOutputRootNode  := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode     := LOutputRootNode.ChildNodes['RunOffAfforestation'];
    LXSDSchemaNode   := GetXSDSchemaNode(AXSDDoc);
    LAffAreaDataNode := LSectionNode.ChildNodes['DataList'];

    LResult          := TRUE;
    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LAffAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LAffAreaDataNode.ChildNodes.Get(LIndex);
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
      while (LIndex < LAffAreaDataNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LAffAreaDataNode.ChildNodes.Get(LIndex);
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


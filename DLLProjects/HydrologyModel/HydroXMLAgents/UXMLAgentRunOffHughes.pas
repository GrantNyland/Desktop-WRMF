(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffHughes.
(******************************************************************************)
unit UXMLAgentRunOffHughes;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffHughes = class(TXMLAgent)
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

procedure TXMLAgentRunOffHughes.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffHughes.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'InflowRouteNo,InfluenceROMNo,GroundWaterModel,HughesHGSL,HughesGPOW,HughesTLGMax,' +
                                'HughesHGGW,HughesPOW,HughesSL,HughesST,HughesFT,HughesGW,HughesZMIN,HughesZMAX,' +
                                'HughesPI,HughesTL,HughesGL,HughesR,HughesFF,UseNoOfReaches,DrainageDensity,' +
                                'NumberOfReaches,RiparianAreaWidthPercentage,RiparianStripFactor,RestWaterlevel,' +
                                'Transmissivity,Storativity,GroundwaterSlope,AnnualUpperZoneAbstraction,' +
                                'AnnualRiparianZoneAbstraction';
    FAllDescriptions.CommaText := '"Inflow route no from upstream","RunOff influencing groundwater", "Groundwater model",' +
                                  '"HGSL","GPOW","TLGMax","HGGW","POW","SL","ST","FT","GW","ZMIN","ZMAX",' +
                                  '"PI","TL","GL","R","FF","Use number of reaches to calculate drainage density",' +
                                  '"Drainage density","Number of reaches","Riparian area width percentage",' +
                                  '"Riparian strip factor","Rest water level","Transmissivity","Storativity",' +
                                  '"Initial groundwater slope","Annual upper zone abstraction",' +
                                  '"Annual riparian zone abstraction"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffHughes.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentRunOffHughes.AddPopulationData';
begin
  try
    AddXMLAllNetworkRoutes(ARootNode);
    AddXMLAllRunOffModules(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffHughes.AddSectionData (AModule    : INetworkModule;
                                                ASectionNo : Integer;
                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffHughes.AddSectionData';
var
  LRunOffModule  : IRunOffModule;
  LHughesNode    : IXMLNode;
  LHughesModel   : IRunOffHughesModel;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LIndex         : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffHughes';

      if (LRunOffModule.HughesModel <> nil) then
      begin
        LHughesNode := ARootNode.ChildNodes['RunOffHughes'];
        LHughesModel := LRunOffModule.HughesModel;
        LHughesNode.ChildNodes['InflowRouteNo'].Text                 := IntToStr(LHughesModel.InflowRouteNo);
        LHughesNode.ChildNodes['InfluenceROMNo'].Text                := IntToStr(LHughesModel.InfluenceROMNo);
        LHughesNode.ChildNodes['GroundWaterModel'].Text              := IntToStr(LHughesModel.GroundWaterModel);
        LHughesNode.ChildNodes['HughesHGSL'].Text                    := FloatToStr(LHughesModel.HGSL);
        LHughesNode.ChildNodes['HughesGPOW'].Text                    := FloatToStr(LHughesModel.GPOW);
        LHughesNode.ChildNodes['HughesTLGMax'].Text                  := FloatToStr(LHughesModel.TLGMax);
        LHughesNode.ChildNodes['HughesHGGW'].Text                    := FloatToStr(LHughesModel.HGGW);
        LHughesNode.ChildNodes['HughesPOW'].Text                     := FloatToStr(LHughesModel.POW);
        LHughesNode.ChildNodes['HughesSL'].Text                      := IntToStr(LHughesModel.SL);
        LHughesNode.ChildNodes['HughesST'].Text                      := IntToStr(LHughesModel.ST);
        LHughesNode.ChildNodes['HughesFT'].Text                      := FloatToStr(LHughesModel.FT);
        LHughesNode.ChildNodes['HughesGW'].Text                      := FloatToStr(LHughesModel.GW);
        LHughesNode.ChildNodes['HughesZMIN'].Text                    := IntToStr(LHughesModel.ZMIN);
        LHughesNode.ChildNodes['HughesZMAX'].Text                    := IntToStr(LHughesModel.ZMAX);
        LHughesNode.ChildNodes['HughesPI'].Text                      := FloatToStr(LHughesModel.PI);
        LHughesNode.ChildNodes['HughesTL'].Text                      := FloatToStr(LHughesModel.TL);
        LHughesNode.ChildNodes['HughesGL'].Text                      := FloatToStr(LHughesModel.GL);
        LHughesNode.ChildNodes['HughesR'].Text                       := FloatToStr(LHughesModel.R);
        LHughesNode.ChildNodes['HughesFF'].Text                      := FloatToStr(LHughesModel.FF);
        LHughesNode.ChildNodes['UseNoOfReaches'].Text                := IntToStr(LHughesModel.UseNoOfReaches);
        LHughesNode.ChildNodes['DrainageDensity'].Text               := FloatToStr(LHughesModel.DrainageDensity);
        LHughesNode.ChildNodes['NumberOfReaches'].Text               := IntToStr(LHughesModel.NoOfReaches);
        LHughesNode.ChildNodes['RiparianAreaWidthPercentage'].Text   := FloatToStr(LHughesModel.RiparianAreaWidthPercentage);
        LHughesNode.ChildNodes['RiparianStripFactor'].Text           := FloatToStr(LHughesModel.RiparianStripFactor);
        LHughesNode.ChildNodes['RestWaterlevel'].Text                := FloatToStr(LHughesModel.RestWaterLevel);
        LHughesNode.ChildNodes['Transmissivity'].Text                := FloatToStr(LHughesModel.Transmissivity);
        LHughesNode.ChildNodes['Storativity'].Text                   := FloatToStr(LHughesModel.Storativity);
        LHughesNode.ChildNodes['GroundwaterSlope'].Text              := FloatToStr(LHughesModel.GroundWaterSlope);
        LHughesNode.ChildNodes['AnnualUpperZoneAbstraction'].Text    := FloatToStr(LHughesModel.AnnualUpperZoneAbstraction);
        LHughesNode.ChildNodes['AnnualRiparianZoneAbstraction'].Text := FloatToStr(LHughesModel.AnnualRiparianZoneAbstraction);

        LDataListNode := LHughesNode.ChildNodes['DataList'];
        for LIndex := 1 to 12 do
        begin
          LNode := LDataListNode.AddChild('MonthlyData');
          LNode.AddChild('Month');
          LNode.AddChild('UpperZone');
          LNode.AddChild('RiparianZone');
          LNode.ChildNodes['Month'].Text  := IntToStr(LIndex);
          LNode.ChildNodes['UpperZone'].Text := FloatToStr(LHughesModel.MonthlyUpperZoneAbstraction[LIndex]);
          LNode.ChildNodes['RiparianZone'].Text := FloatToStr(LHughesModel.MonthlyRiparianZoneAbstraction[LIndex]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffHughes.XSDText : String;
const OPNAME = 'TXMLAgentRunOffHughes.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffHughes.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffHughes.DoAdditionalValidation (AContext          : TValidationContext;
                                                       APropertyName     : String;
                                                       AFieldIndex       : String;
                                                       AXSDDoc           : IXMLDocument;
                                                       AXMLDocumentIn    : IXMLDocument;
                                                       AXMLDocumentOut   : IXMLDocument;
                                                       AStopOnFirstError : Boolean;
                                                       AErrorList        : TStringList;
                                                       AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffHughes.DoAdditionalValidation';
var
  LResult           : Boolean;
  LInputRootNode    : IXMLNode;
  LOutputRootNode   : IXMLNode;
  LHughesNode       : IXMLNode;
  LDataListNode     : IXMLNode;
  LAllNetworkRoutes : IXMLNode;
  LAllRunOffModules : IXMLNode;
  LTempResult       : Boolean;
  LIndex            : Integer;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
  LNode             : IXMLNode;
  LNumberStr        : String;
  LModuleNumber     : String;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LHughesNode       := LOutputRootNode.ChildNodes['RunOffHughes'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);
    LDataListNode     := LHughesNode.ChildNodes['DataList'];
    LAllNetworkRoutes := LInputRootNode.ChildNodes['AllNetworkRoutes'];
    LAllRunOffModules := LInputRootNode.ChildNodes['AllRunOffModules'];
    LModuleNumber     := Trim(LOutputRootNode.ChildNodes['ModuleNumber'].Text);

    LResult           := TRUE;
    //InflowRouteNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'InflowRouteNo') OR (APropertyName = ''))) then
    begin
      LNumberStr  := Trim(LHughesNode.ChildNodes['InflowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LNumberStr, LAllNetworkRoutes, TRUE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Inflow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('InflowRouteNo');
      end;
      LResult := LResult AND LTempResult;
    end;

    // InfluenceROMNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'InfluenceROMNo') OR (APropertyName = ''))) then
    begin
      LNumberStr  := Trim(LHughesNode.ChildNodes['InfluenceROMNo'].Text);
      if (LModuleNumber = LNumberStr) then
      begin
        LTempResult := FALSE;
        LErrorMsg := 'RunOff influencing groundwater may not be the same as module number of RunOff.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('InfluenceROMNo');
      end
      else
      begin
        LTempResult := IsRunOffModuleValid(LNumberStr, LAllRunOffModules, TRUE);
        if (NOT LTempResult) then
        begin
          LErrorMsg := 'RunOff influencing groundwater is invalid.';
          AErrorMessages.Add(LErrorMsg);
          AErrorList.Add('InfluenceROMNo');
        end;
      end;  
      LResult := LResult AND LTempResult;
    end;

    // UpperZone
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'UpperZone') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'UpperZone', 'Upper zone abstraction demand', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('UpperZone,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // RiparianZone
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'RiparianZone') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'RiparianZone', 'Riparian zone abstraction demand', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('RiparianZone,' + IntToStr(LIndex+1));
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


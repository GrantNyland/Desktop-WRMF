(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationWQT.
(******************************************************************************)
unit UXMLAgentIrrigationWQT;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationWQT = class(TXMLAgent)
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

procedure TXMLAgentIrrigationWQT.PopulatePropertyLists;
const OPNAME = 'TXMLAgentIrrigationWQT.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'ProduceNetReturnFlows,IrrigationEfficiencyFactor,' +
                                'TransferCanalSaltLossProportion,TransferCanalFlowLossProportion,TransferCanalSeepage,' +
                                'RunOffModuleNo,WaterAllocationInterpolationType,MaxWaterAllocation,' +
                                'ReturnFlowFactor,UpperZoneReturnFlowProportion,LowerZoneReturnFlowProportion,' +
                                'SaltConcentrationFactor,LandSaltLossProportion,SaltLoad1,' +
                                'SaltLoad2,InitialSaltLoadLowerZone,InitialSaltLoadUpperZone,' +
                                'SoilMoistureStorageCapacityLowerZone,SoilMoistureStorageCapacityUpperZone,InitialSoilMoisture,' +
                                'TargetSoilMoisture,EffectiveRainfallFactor1,EffectiveRainfallFactor2';
    FAllDescriptions.CommaText := '"Produce Net return flows","Irrigation efficiency factor",' +
                                  '"Transfer canal salt loss proportion","Transfer canal flow loss proportion","Transfer canal seepage",' +
                                  '"RunOff Module","Water allocation interpolation type","Maximum water allocation",' +
                                  '"Return flow factor","Upper zone return flow proportion","Lower zone return flow proportion",' +
                                  '"Salt concentration factor","Land salt loss proportion","Salt load applied to irrigated land 1",' +
                                  '"Salt load applied to irrigated land 2","Initial salt load lower zone","Initial salt load upper zone",' +
                                  '"Soil moisture storage capacity lower zone","Soil moisture storage capacity upper zone","Initial soil moisture",' +
                                  '"Target soil moisture","Effective rainfall factor 1","Effective rainfall factor 2"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationWQT.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationWQT.AddPopulationData';
begin
  try
    AddXMLAllRunOffModules(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationWQT.AddSectionData (AModule    : INetworkModule;
                                                 ASectionNo : Integer;
                                                 ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationWQT.AddSectionData';
var
  LSectionNode      : IXMLNode;
  LIrrigationModule : IIrrigationModule;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'IrrigationWQT';

      LSectionNode := ARootNode.ChildNodes['IrrigationWQT'];
      LSectionNode.ChildNodes['ProduceNetReturnFlows'].Text                := IntToStr(LIrrigationModule.ProduceNetReturnFlows);
      LSectionNode.ChildNodes['IrrigationEfficiencyFactor'].Text           := FloatToStr(LIrrigationModule.IrrigationEfficiencyFactor);
      LSectionNode.ChildNodes['TransferCanalSaltLossProportion'].Text      := FloatToStr(LIrrigationModule.TransferCanalSaltLossProportion);
      LSectionNode.ChildNodes['TransferCanalFlowLossProportion'].Text      := FloatToStr(LIrrigationModule.TransferCanalFlowLossProportion);
      LSectionNode.ChildNodes['TransferCanalSeepage'].Text                 := FloatToStr(LIrrigationModule.TransferCanalSeepage);
      LSectionNode.ChildNodes['RunOffModuleNo'].Text                       := IntToStr(LIrrigationModule.RunOffModuleNo);
      LSectionNode.ChildNodes['WaterAllocationInterpolationType'].Text     := IntToStr(LIrrigationModule.WaterAllocationInterpolationType);
      LSectionNode.ChildNodes['MaxWaterAllocation'].Text                   := FloatToStr(LIrrigationModule.MaxWaterAllocation);
      LSectionNode.ChildNodes['ReturnFlowFactor'].Text                     := FloatToStr(LIrrigationModule.ReturnFlowFactor);
      LSectionNode.ChildNodes['UpperZoneReturnFlowProportion'].Text        := FloatToStr(LIrrigationModule.UpperZoneReturnFlowProportion);
      LSectionNode.ChildNodes['LowerZoneReturnFlowProportion'].Text        := FloatToStr(LIrrigationModule.LowerZoneReturnFlowProportion);
      LSectionNode.ChildNodes['SaltConcentrationFactor'].Text              := FloatToStr(LIrrigationModule.SaltConcentrationFactor);
      LSectionNode.ChildNodes['LandSaltLossProportion'].Text               := FloatToStr(LIrrigationModule.SaltLossProportion);
      LSectionNode.ChildNodes['SaltLoad1'].Text                            := FloatToStr(LIrrigationModule.SaltLoad1);
      LSectionNode.ChildNodes['SaltLoad2'].Text                            := FloatToStr(LIrrigationModule.SaltLoad2);
      LSectionNode.ChildNodes['InitialSaltLoadLowerZone'].Text             := FloatToStr(LIrrigationModule.InitialSaltLoadLowerZone);
      LSectionNode.ChildNodes['InitialSaltLoadUpperZone'].Text             := FloatToStr(LIrrigationModule.InitialSaltLoadUpperZone);
      LSectionNode.ChildNodes['SoilMoistureStorageCapacityLowerZone'].Text := FloatToStr(LIrrigationModule.SoilMoistureCapacityLowerZone);
      LSectionNode.ChildNodes['SoilMoistureStorageCapacityUpperZone'].Text := FloatToStr(LIrrigationModule.SoilMoistureCapacityUpperZone);
      LSectionNode.ChildNodes['InitialSoilMoisture'].Text                  := FloatToStr(LIrrigationModule.InitialSoilMoistureStorage);
      LSectionNode.ChildNodes['TargetSoilMoisture'].Text                   := FloatToStr(LIrrigationModule.TargetSoilMoistureStorage);
      LSectionNode.ChildNodes['EffectiveRainfallFactor1'].Text             := FloatToStr(LIrrigationModule.EffectiveRainfallFactor1);
      LSectionNode.ChildNodes['EffectiveRainfallFactor2'].Text             := FloatToStr(LIrrigationModule.EffectiveRainfallFactor2);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationWQT.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationWQT.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationWQT.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationWQT.DoAdditionalValidation (AContext          : TValidationContext;
                                                        APropertyName     : String;
                                                        AFieldIndex       : String;
                                                        AXSDDoc           : IXMLDocument;
                                                        AXMLDocumentIn    : IXMLDocument;
                                                        AXMLDocumentOut   : IXMLDocument;
                                                        AStopOnFirstError : Boolean;
                                                        AErrorList        : TStringList;
                                                        AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationWQT.DoAdditionalValidation';
var
  LResult            : Boolean;
  LInputRootNode     : IXMLNode;
  LOutputRootNode    : IXMLNode;
  LSectionNode       : IXMLNode;
  LAllRunOffModules  : IXMLNode;
  LTempResult        : Boolean;
  LXSDSchemaNode     : IXMLNode;
  LErrorMsg          : String;
  LRunOffModuleNoStr : String;
begin
  Result := FALSE;
  try
    LInputRootNode     := AXMLDocumentIn.DocumentElement;
    LAllRunOffModules  := LInputRootNode.ChildNodes['AllRunOffModules'];
    LOutputRootNode    := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode       := LOutputRootNode.ChildNodes['IrrigationWQT'];
    LXSDSchemaNode     := GetXSDSchemaNode(AXSDDoc);

    LResult            := TRUE;
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'RunOffModuleNo') OR (APropertyName = ''))) then
    begin
      LRunOffModuleNoStr := Trim(LSectionNode.ChildNodes['RunOffModuleNo'].Text);
      LTempResult := IsRunOffModuleValid(LRunOffModuleNoStr, LAllRunOffModules, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'RunOff module is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('RunOffModuleNo');
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


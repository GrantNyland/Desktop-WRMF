(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffSami.
(******************************************************************************)
unit UXMLAgentRunOffSami;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffSami = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
    function XSDText : String; override;
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
  end;

implementation

uses
  UErrorHandlingOperations;

procedure TXMLAgentRunOffSami.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffSami.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'AquiferThickness,Storativity,InitialAquiferStorage,StaticWaterLevel,' +
                                'UnsaturatedStorage,InitialUnsaturatedZoneStorage,PerculationPower,' +
                                'MaxDischarge,InteractionCurvePower,MaxHydrologicalGradient,' +
                                'Transmissivity,BoreholeDistanceToRiver,GroundWaterEvaporationArea,' +
                                'InterflowLag,RechargeAveragedNoMonths,UseAbstractions,' +
                                'SamiGPOW,SamiHGSL,SamiHGGW,SamiK2,SamiK3,SamiPOW,SamiSL,SamiST,SamiFT,' +
                                'SamiGW,SamiZMIN,SamiZMAX,SamiPI,SamiTL,SamiGL,SamiR,SamiFF';
    FAllDescriptions.CommaText := '"Aquifer thickness","Storativity","Initial aquifer storage","Static water level",' +
                                  '"Unsaturated storage","Initial unsaturated zone storage","Perculation power",' +
                                  '"Maximum discharge","Surface/ground water interaction power","Max hydrological gradient",' +
                                  '"Transmissivity","Borehole distance to river","Groundwater evaporation area",' +
                                  '"Interflow lag","No of months over which recharge are averaged","Use abstractions",' +
                                  '"GPOW","HGSL","HGGW","K2","K3","POW","SL","ST","FT",' +
                                  '"GW","ZMIN","ZMAX","PI","TL","GL","R","FF"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffSami.AddSectionData (AModule    : INetworkModule;
                                              ASectionNo : Integer;
                                              ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffSami.AddSectionData';
var
  LRunOffModule  : IRunOffModule;
  LSectionNode   : IXMLNode;
  LSamiModel     : IRunOffSamiModel;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffSami';

      if (LRunOffModule.SamiModel <> nil) then
      begin
        LSectionNode := ARootNode.ChildNodes['RunOffSami'];
        LSamiModel := LRunOffModule.SamiModel;
        LSectionNode.ChildNodes['AquiferThickness'].Text              := FloatToStr(LSamiModel.AquiferThickness);
        LSectionNode.ChildNodes['Storativity'].Text                   := FloatToStr(LSamiModel.Storativity);
        LSectionNode.ChildNodes['InitialAquiferStorage'].Text         := FloatToStr(LSamiModel.InitialAquiferStorage);
        LSectionNode.ChildNodes['StaticWaterLevel'].Text              := FloatToStr(LSamiModel.StaticWaterLevel);
        LSectionNode.ChildNodes['UnsaturatedStorage'].Text            := FloatToStr(LSamiModel.UnsaturatedStorage);
        LSectionNode.ChildNodes['InitialUnsaturatedZoneStorage'].Text := FloatToStr(LSamiModel.InitialUnsaturatedZoneStorage);
        LSectionNode.ChildNodes['PerculationPower'].Text              := FloatToStr(LSamiModel.PerculationPower);
        LSectionNode.ChildNodes['MaxDischarge'].Text                  := FloatToStr(LSamiModel.MaxDischarge);
        LSectionNode.ChildNodes['InteractionCurvePower'].Text         := FloatToStr(LSamiModel.InteractionCurvePower);
        LSectionNode.ChildNodes['MaxHydrologicalGradient'].Text       := FloatToStr(LSamiModel.MaxHydrologicalGradient);
        LSectionNode.ChildNodes['Transmissivity'].Text                := FloatToStr(LSamiModel.Transmissivity);
        LSectionNode.ChildNodes['BoreholeDistanceToRiver'].Text       := FloatToStr(LSamiModel.BoreholeDistanceToRiver);
        LSectionNode.ChildNodes['GroundWaterEvaporationArea'].Text    := FloatToStr(LSamiModel.GroundWaterEvaporationArea);
        LSectionNode.ChildNodes['InterflowLag'].Text                  := FloatToStr(LSamiModel.InterflowLag);
        LSectionNode.ChildNodes['RechargeAveragedNoMonths'].Text      := FloatToStr(LSamiModel.RechargeAveragedNoMonths);
        LSectionNode.ChildNodes['UseAbstractions'].Text               := IntToStr(LSamiModel.UseAbstractions);

        LSectionNode.ChildNodes['SamiGPOW'].Text                      := FloatToStr(LSamiModel.GPOW);
        LSectionNode.ChildNodes['SamiHGSL'].Text                      := FloatToStr(LSamiModel.HGSL);
        LSectionNode.ChildNodes['SamiHGGW'].Text                      := FloatToStr(LSamiModel.HGGW);
        LSectionNode.ChildNodes['SamiK2'].Text                        := FloatToStr(LSamiModel.K2);
        LSectionNode.ChildNodes['SamiK3'].Text                        := FloatToStr(LSamiModel.K3);
        LSectionNode.ChildNodes['SamiPOW'].Text                       := FloatToStr(LSamiModel.POW);
        LSectionNode.ChildNodes['SamiSL'].Text                        := IntToStr(LSamiModel.SL);
        LSectionNode.ChildNodes['SamiST'].Text                        := IntToStr(LSamiModel.ST);
        LSectionNode.ChildNodes['SamiFT'].Text                        := FloatToStr(LSamiModel.FT);
        LSectionNode.ChildNodes['SamiGW'].Text                        := FloatToStr(LSamiModel.GW);
        LSectionNode.ChildNodes['SamiZMIN'].Text                      := IntToStr(LSamiModel.ZMIN);
        LSectionNode.ChildNodes['SamiZMAX'].Text                      := IntToStr(LSamiModel.ZMAX);
        LSectionNode.ChildNodes['SamiPI'].Text                        := FloatToStr(LSamiModel.PI);
        LSectionNode.ChildNodes['SamiTL'].Text                        := FloatToStr(LSamiModel.TL);
        LSectionNode.ChildNodes['SamiGL'].Text                        := FloatToStr(LSamiModel.GL);
        LSectionNode.ChildNodes['SamiR'].Text                         := FloatToStr(LSamiModel.R);
        LSectionNode.ChildNodes['SamiFF'].Text                        := FloatToStr(LSamiModel.FF);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffSami.XSDText : String;
const OPNAME = 'TXMLAgentRunOffSami.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffSami.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


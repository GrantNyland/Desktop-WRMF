(******************************************************************************)
(* This unit contains the class TXMLAgentMineOpencastPitProperties.
(******************************************************************************)
unit UXMLAgentMineOpencastPitProperties;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,

  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineOpencastPitProperties = class(TXMLAgent)
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

procedure TXMLAgentMineOpencastPitProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineOpencastPitProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'SectionName,CoalReserveArea,WorkingArea,' +
                                'CommissionYear,CommissionMonth,DeCommissionYear,DeCommissionMonth,' +
                                'DisturbedArea,RehabilitatedArea,EvaporationArea,DisturbedAreaRunOffFactor,' +
                                'DisturbedWorkingAreaRunOffFactor,WashOffParameter,SulphateBuildUpRate,' +
                                'InitialSaltMass,InspoilsStorageDecant,InspoilsStorageSeepage,' +
                                'SeepageEquationExponent,MaxSeepageRate,InspoilsStorageInitialVolume,' +
                                'InspoilsDamConcentration,PCDInitialVolume,PCDCapacity,PCDFullSurfaceArea';
    FAllDescriptions.CommaText := '"Opencast pit name","Coal reserve area","Workings area",' +
                               '"Commissioning year","Commissioning month","Decommissioning year","Decommissioning month",' +
                               '"Disturbed area","Rehabilitated area","Pit evaporation area","Disturbed area run-off factor",' +
                               '"Disturbed working area run-off factor","Wash-off parameter","Sulphate build-up rate",' +
                               '"Initial salt mass","Inspoils storage where decant occurs","Inspoils storage where seepage occurs",' +
                               '"Exponent of seepage equation","Maximum seepage rate","Initial inspoils storage volume",' +
                               '"Inspoils dam concentration","PCD initial volume","PCD capacity","PCD full surface area"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineOpencastPitProperties.AddSectionData (AModule    : INetworkModule;
                                                             ASectionNo : Integer;
                                                             ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineOpencastPitProperties.AddSectionData';
var
  LSectionNode : IXMLNode;
  LMine        : IMineModule;
  LOpencastPit : IOpencastPit;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LOpencastPit := LMine.OpencastPitBySectionNo[ASectionNo];
      if (LOpencastPit <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LOpencastPit.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineOpencastPitProperties';

        LSectionNode := ARootNode.ChildNodes['MineOpencastPitProperties'];
        LSectionNode.ChildNodes['SectionName'].Text                       := LOpencastPit.SectionName;
        LSectionNode.ChildNodes['CoalReserveArea'].Text                   := FloatToStr(LOpencastPit.CoalReserveArea);
        LSectionNode.ChildNodes['WorkingArea'].Text                       := FloatToStr(LOpencastPit.WorkingsArea);
        LSectionNode.ChildNodes['CommissionYear'].Text                    := IntToStr(LOpencastPit.CommissionYear);
        LSectionNode.ChildNodes['CommissionMonth'].Text                   := IntToStr(LOpencastPit.CommissionMonth);
        LSectionNode.ChildNodes['DeCommissionYear'].Text                  := IntToStr(LOpencastPit.DecommissionYear);
        LSectionNode.ChildNodes['DeCommissionMonth'].Text                 := IntToStr(LOpencastPit.DecommissionMonth);
        LSectionNode.ChildNodes['DisturbedArea'].Text                     := FloatToStr(LOpencastPit.DisturbedArea);
        LSectionNode.ChildNodes['RehabilitatedArea'].Text                 := FloatToStr(LOpencastPit.RehabilitatedArea);
        LSectionNode.ChildNodes['EvaporationArea'].Text                   := FloatToStr(LOpencastPit.PitEvaporationArea);
        LSectionNode.ChildNodes['DisturbedAreaRunOffFactor'].Text         := FloatToStr(LOpencastPit.DisturbedAreaRunOffFactor);
        LSectionNode.ChildNodes['DisturbedWorkingAreaRunOffFactor'].Text  := FloatToStr(LOpencastPit.DisturbedWorkingsAreaRunOffFactor);
        LSectionNode.ChildNodes['WashOffParameter'].Text                  := FloatToStr(LOpencastPit.WashOffParameter);
        LSectionNode.ChildNodes['SulphateBuildUpRate'].Text               := FloatToStr(LOpencastPit.SulphateBuildUpRate);
        LSectionNode.ChildNodes['InitialSaltMass'].Text                   := FloatToStr(LOpencastPit.InitialSaltMass);
        LSectionNode.ChildNodes['InspoilsStorageDecant'].Text             := FloatToStr(LOpencastPit.InspoilsStorageDecantVolume);
        LSectionNode.ChildNodes['InspoilsStorageSeepage'].Text            := FloatToStr(LOpencastPit.InspoilsStorageSeepageVolume);
        LSectionNode.ChildNodes['SeepageEquationExponent'].Text           := FloatToStr(LOpencastPit.SeepageEquationExponent);
        LSectionNode.ChildNodes['MaxSeepageRate'].Text                    := FloatToStr(LOpencastPit.MaxSeepageRate);
        LSectionNode.ChildNodes['InspoilsStorageInitialVolume'].Text      := FloatToStr(LOpencastPit.InspoilsStorageInitialVolume);
        LSectionNode.ChildNodes['InspoilsDamConcentration'].Text          := FloatToStr(LOpencastPit.InspoilsDamConcentration);
        LSectionNode.ChildNodes['PCDInitialVolume'].Text                  := FloatToStr(LOpencastPit.PCDInitialVolume);
        LSectionNode.ChildNodes['PCDCapacity'].Text                       := FloatToStr(LOpencastPit.PCDCapacity);
        LSectionNode.ChildNodes['PCDFullSurfaceArea'].Text                := FloatToStr(LOpencastPit.PCDFullSurfaceArea);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineOpencastPitProperties.XSDText : String;
const OPNAME = 'TXMLAgentMineOpencastPitProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineOpencastPitProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


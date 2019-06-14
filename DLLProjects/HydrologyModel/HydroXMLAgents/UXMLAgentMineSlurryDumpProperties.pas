(******************************************************************************)
(* This unit contains the class TXMLAgentMineSlurryDumpProperties.
(******************************************************************************)
unit UXMLAgentMineSlurryDumpProperties;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,

  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineSlurryDumpProperties = class(TXMLAgent)
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

procedure TXMLAgentMineSlurryDumpProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineSlurryDumpProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'SectionName,SlurryDumpArea,SlurryDumpRunOffFactor,' +
                                'SlurrySeepProportion,SlurryPCDFullSupplyVolume,SlurryPCDFullSupplyArea,' +
                                'SlurryPCDInitialVolume';
    FAllDescriptions.CommaText := '"Section name","Slurry dump area","Slurry dump run-off factor",' +
                                  '"Seep proportion to dam and river","PCD full supply volume","PCD full supply area",' +
                                  '"PCD initial volume"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineSlurryDumpProperties.AddSectionData (AModule    : INetworkModule;
                                                            ASectionNo : Integer;
                                                            ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineSlurryDumpProperties.AddSectionData';
var
  LSectionNode : IXMLNode;
  LMine        : IMineModule;
  LSlurryDump  : ISlurryDump;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LSlurryDump := LMine.SlurryDumpBySectionNo[ASectionNo];
      if (LSlurryDump <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LSlurryDump.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineSlurryDumpProperties';

        LSectionNode := ARootNode.ChildNodes['MineSlurryDumpProperties'];
        LSectionNode.ChildNodes['SectionNo'].Text                 := IntToStr(LSlurryDump.SectionNo);
        LSectionNode.ChildNodes['SectionName'].Text               := LSlurryDump.SectionName;
        LSectionNode.ChildNodes['SlurryDumpArea'].Text            := FloatToStr(LSlurryDump.Area);
        LSectionNode.ChildNodes['SlurryDumpRunOffFactor'].Text    := FloatToStr(LSlurryDump.RunOffFactor);
        LSectionNode.ChildNodes['SlurrySeepProportion'].Text      := FloatToStr(LSlurryDump.SeepProportion);
        LSectionNode.ChildNodes['SlurryPCDFullSupplyVolume'].Text := FloatToStr(LSlurryDump.PCDFullSupplyVolume);
        LSectionNode.ChildNodes['SlurryPCDFullSupplyArea'].Text   := FloatToStr(LSlurryDump.PCDFullSupplyArea);
        LSectionNode.ChildNodes['SlurryPCDInitialVolume'].Text    := FloatToStr(LSlurryDump.PCDInitialVolume);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineSlurryDumpProperties.XSDText : String;
const OPNAME = 'TXMLAgentMineSlurryDumpProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineSlurryDumpProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


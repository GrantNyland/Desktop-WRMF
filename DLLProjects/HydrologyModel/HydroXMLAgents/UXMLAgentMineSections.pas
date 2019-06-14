(******************************************************************************)
(* This unit contains the class TXMLAgentMineSections.
(******************************************************************************)
unit UXMLAgentMineSections;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineSections = class(TXMLAgent)
  protected
  public
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
    procedure AddXMLAllMineSections (AMine     : IMineModule;
                                     ARootNode : IXMLNode);
  end;

implementation

uses
  UErrorHandlingOperations;

procedure TXMLAgentMineSections.AddSectionData (AModule    : INetworkModule;
                                                ASectionNo : Integer;
                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineSections.AddSectionData';
var
  LSectionNode   : IXMLNode;
  LMineModule    : IMineModule;
begin
  try
    LMineModule := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMineModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text         := 'MineSections';

      LSectionNode := ARootNode.ChildNodes['MineSections'];
      LSectionNode.ChildNodes['Action'].Text        := 'None';
      LSectionNode.ChildNodes['SectionType'].Text   := 'None';
      LSectionNode.ChildNodes['SectionNo'].Text     := IntToStr(0);

      AddXMLAllMineSections(LMineModule, ARootNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineSections.AddXMLAllMineSections (AMine     : IMineModule;
                                                       ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentMineSections.AddXMLAllMineSections';
var
  LSectionsNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
  LOpencastPit  : IOpencastPit;
  LUnderground  : IUndergroundSection;
  LSlurryDump   : ISlurryDump;
begin
  try
    LSectionsNode := ARootNode.ChildNodes['AllOpencastSections'];
    for LIndex := 0 to AMine.NoOfOpencastPits - 1 do
    begin
      LOpencastPit := AMine.OpencastPitByIndex[LIndex];
      LNode        := LSectionsNode.AddChild('OpencastSection');
      LNode.AddChild('SectionNo');
      LNode.AddChild('Name');
      LNode.ChildNodes['SectionNo'].Text := IntToStr(LOpencastPit.SectionNo);
      LNode.ChildNodes['Name'].Text      := LOpencastPit.SectionName;
    end;

    LSectionsNode := ARootNode.ChildNodes['AllUndergroundSections'];
    for LIndex := 0 to AMine.NoOfUndergroundSections - 1 do
    begin
      LUnderground := AMine.UndergroundSectionByIndex[LIndex];
      LNode        := LSectionsNode.AddChild('UndergroundSection');
      LNode.AddChild('SectionNo');
      LNode.AddChild('Name');
      LNode.ChildNodes['SectionNo'].Text := IntToStr(LUnderground.SectionNo);
      LNode.ChildNodes['Name'].Text      := LUnderground.SectionName;
    end;

    LSectionsNode := ARootNode.ChildNodes['AllSlurryDumps'];
    for LIndex := 0 to AMine.NoOfSlurryDumps - 1 do
    begin
      LSlurryDump  := AMine.SlurryDumpByIndex[LIndex];
      LNode        := LSectionsNode.AddChild('SlurryDump');
      LNode.AddChild('SectionNo');
      LNode.AddChild('Name');
      LNode.ChildNodes['SectionNo'].Text := IntToStr(LSlurryDump.SectionNo);
      LNode.ChildNodes['Name'].Text      := LSlurryDump.SectionName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


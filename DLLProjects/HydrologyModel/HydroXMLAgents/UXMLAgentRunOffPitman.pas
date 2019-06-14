(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffPitman.
(******************************************************************************)
unit UXMLAgentRunOffPitman;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffPitman = class(TXMLAgent)
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

procedure TXMLAgentRunOffPitman.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffPitman.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText   := 'PitmanPOW,PitmanSL,PitmanST,PitmanFT,PitmanGW,PitmanZMIN,' +
                                  'PitmanZMAX,PitmanPI,PitmanTL,PitmanGL,PitmanR,PitmanFF';
    FAllDescriptions.CommaText := 'POW,SL,ST,FT,GW,ZMIN,ZMAX,PI,TL,GL,R,FF';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffPitman.AddSectionData (AModule    : INetworkModule;
                                                ASectionNo : Integer;
                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffPitman.AddSectionData';
var
  LRunOffModule : IRunOffModule;
  LSectionNode  : IXMLNode;
  LPitmanModel  : IRunOffPitmanModel;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffPitman';

      LSectionNode := ARootNode.ChildNodes['RunOffPitman'];
      LPitmanModel := LRunOffModule.PitmanModel;
      LSectionNode.ChildNodes['PitmanPOW'].Text   := FloatToStr(LPitmanModel.POW);
      LSectionNode.ChildNodes['PitmanSL'].Text    := IntToStr(LPitmanModel.SL);
      LSectionNode.ChildNodes['PitmanST'].Text    := IntToStr(LPitmanModel.ST);
      LSectionNode.ChildNodes['PitmanFT'].Text    := FloatToStr(LPitmanModel.FT);
      LSectionNode.ChildNodes['PitmanGW'].Text    := FloatToStr(LPitmanModel.GW);
      LSectionNode.ChildNodes['PitmanZMIN'].Text  := IntToStr(LPitmanModel.ZMIN);
      LSectionNode.ChildNodes['PitmanZMAX'].Text  := IntToStr(LPitmanModel.ZMAX);
      LSectionNode.ChildNodes['PitmanPI'].Text    := FloatToStr(LPitmanModel.PI);
      LSectionNode.ChildNodes['PitmanTL'].Text    := FloatToStr(LPitmanModel.TL);
      LSectionNode.ChildNodes['PitmanGL'].Text    := FloatToStr(LPitmanModel.GL);
      LSectionNode.ChildNodes['PitmanR'].Text     := FloatToStr(LPitmanModel.R);
      LSectionNode.ChildNodes['PitmanFF'].Text    := FloatToStr(LPitmanModel.FF);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffPitman.XSDText : String;
const OPNAME = 'TXMLAgentRunOffPitman.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffPitman.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


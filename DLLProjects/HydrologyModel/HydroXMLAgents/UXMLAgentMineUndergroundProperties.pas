(******************************************************************************)
(* This unit contains the class TXMLAgentMineUndergroundProperties.
(******************************************************************************)
unit UXMLAgentMineUndergroundProperties;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,

  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineUndergroundProperties = class(TXMLAgent)
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

procedure TXMLAgentMineUndergroundProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineUndergroundProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'SectionName,UndergroundOutflowRouteNo,UpstreamCatchmentArea,' +
                                'BoardAndPillarArea,HighExtractionArea,SurfaceRunOffFactor';
    FAllDescriptions.CommaText := '"Section name","Underground outflow route","Upstream catchment area",' +
                                  '"Board and pillar area","High extraction area","Surface run-off factor"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineUndergroundProperties.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentMineUndergroundProperties.AddPopulationData';
begin
  try
//    AddXMLAllNetworkRoutes(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineUndergroundProperties.AddSectionData (AModule    : INetworkModule;
                                                             ASectionNo : Integer;
                                                             ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineUndergroundProperties.AddSectionData';
var
  LSectionNode : IXMLNode;
  LMine        : IMineModule;
  LUnderground : IUndergroundSection;
begin
  try
    LMine := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMine <> nil) then
    begin
      LUnderground := LMine.UndergroundSectionBySectionNo[ASectionNo];
      if (LUnderground <> nil) then
      begin
        ARootNode.ChildNodes['SectionNo'].Text       := IntToStr(LUnderground.SectionNo);
        ARootNode.ChildNodes['Section'].Text         := 'MineUndergroundProperties';

        LSectionNode := ARootNode.ChildNodes['MineUndergroundProperties'];
        LSectionNode.ChildNodes['SectionNo'].Text                 := IntToStr(LUnderground.SectionNo);
        LSectionNode.ChildNodes['SectionName'].Text               := LUnderground.SectionName;
        LSectionNode.ChildNodes['UndergroundOutflowRouteNo'].Text := IntToStr(LUnderground.OutFlowRouteNo);
        LSectionNode.ChildNodes['UpstreamCatchmentArea'].Text     := FloatToStr(LUnderground.UpstreamCatchmentArea);
        LSectionNode.ChildNodes['BoardAndPillarArea'].Text        := FloatToStr(LUnderground.BoardAndPillarArea);
        LSectionNode.ChildNodes['HighExtractionArea'].Text        := FloatToStr(LUnderground.HighExtractionArea);
        LSectionNode.ChildNodes['SurfaceRunOffFactor'].Text       := FloatToStr(LUnderground.SurfaceRunOffFactor);

        AddXMLAllOutflowRoutes(ARootNode, LMine.ModuleID);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundProperties.XSDText : String;
const OPNAME = 'TXMLAgentMineUndergroundProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineUndergroundProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineUndergroundProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                                    APropertyName     : String;
                                                                    AFieldIndex       : String;
                                                                    AXSDDoc           : IXMLDocument;
                                                                    AXMLDocumentIn    : IXMLDocument;
                                                                    AXMLDocumentOut   : IXMLDocument;
                                                                    AStopOnFirstError : Boolean;
                                                                    AErrorList        : TStringList;
                                                                    AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineUndergroundProperties.DoAdditionalValidation';
var
  LResult           : Boolean;
  LInputRootNode    : IXMLNode;
  LOutputRootNode   : IXMLNode;
  LSectionNode      : IXMLNode;
  LAllOutflowRoutes : IXMLNode;
  LErrorMsg         : String;
  LTempResult       : Boolean;
  LRouteNoStr       : String;
begin
  Result := FALSE;
  try
    LInputRootNode     := AXMLDocumentIn.DocumentElement;
    LAllOutflowRoutes  := LInputRootNode.ChildNodes['OutflowRoutes'];
    LOutputRootNode    := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode       := LOutputRootNode.ChildNodes['MineUndergroundProperties'];
    LResult            := TRUE;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'UndergroundOutflowRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['UndergroundOutflowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Underground outflow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('UndergroundOutflowRouteNo');
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


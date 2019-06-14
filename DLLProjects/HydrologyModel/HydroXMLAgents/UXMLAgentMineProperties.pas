(******************************************************************************)
(* This unit contains the class TXMLAgentMineProperties.
(******************************************************************************)
unit UXMLAgentMineProperties;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentMineProperties = class(TXMLAgent)
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

procedure TXMLAgentMineProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentMineProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'NetworkSequence,ModuleNumber,Active,Latitude,Longitude,MineName,MAP,' +
                                'RainfallFileName,VersionNo,RunOffModuleNo,OutflowRouteNoToRiver,' +
                                'OutflowRouteNoToPCD,PlantArea,PlantAreaRunOffFactor,' +
                                'SaltBuildUpRate,SaltWashOffFactor,InitialSaltStore';
    FAllDescriptions.CommaText := '"Network sequence","Module number","Active","Latitude","Longitude","Mine name","MAP",' +
                                  '"Rainfall file name","Version no","Run-off module","Outflow route to river",' +
                                  '"Outflow route to central PCD","Plant area","Plant area run-off factor",' +
                                  '"Salt build-up rate","Salt wash-off factor","Initial salt store"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineProperties.AddSectionData (AModule    : INetworkModule;
                                                  ASectionNo : Integer;
                                                  ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentMineProperties.AddSectionData';
var
  LSectionNode   : IXMLNode;
  LMineModule    : IMineModule;
begin
  try
    LMineModule := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AModule.ModuleID];
    if (LMineModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'MineProperties';

      LSectionNode := ARootNode.ChildNodes['MineProperties'];
      LSectionNode.ChildNodes['NetworkSequence'].Text        := IntToStr(LMineModule.NetworkSequence);
      LSectionNode.ChildNodes['ModuleNumber'].Text           := IntToStr(LMineModule.ModuleNumber);
      LSectionNode.ChildNodes['Active'].Text                 := LMineModule.Active;
      LSectionNode.ChildNodes['Latitude'].Text               := FloatToStr(LMineModule.Latitude);
      LSectionNode.ChildNodes['Longitude'].Text              := FloatToStr(LMineModule.Longitude);
      LSectionNode.ChildNodes['MineName'].Text               := LMineModule.MineName;
      LSectionNode.ChildNodes['MAP'].Text                    := FloatToStr(LMineModule.MAP);
      LSectionNode.ChildNodes['RainfallFileName'].Text       := LMineModule.RainfallFileName;
      LSectionNode.ChildNodes['VersionNo'].Text              := IntToStr(LMineModule.VersionNo);
      LSectionNode.ChildNodes['RunOffModuleNo'].Text         := IntToStr(LMineModule.RunOffModuleNo);
      LSectionNode.ChildNodes['OutflowRouteNoToRiver'].Text  := IntToStr(LMineModule.OutflowRouteNoToRiver);
      LSectionNode.ChildNodes['OutflowRouteNoToPCD'].Text    := IntToStr(LMineModule.OutflowRouteNoToPCD);
      LSectionNode.ChildNodes['PlantArea'].Text              := FloatToStr(LMineModule.PlantArea);
      LSectionNode.ChildNodes['PlantAreaRunOffFactor'].Text  := FloatToStr(LMineModule.PlantAreaRunOffFactor);
      LSectionNode.ChildNodes['SaltBuildUpRate'].Text        := FloatToStr(LMineModule.SaltBuildUpRate);
      LSectionNode.ChildNodes['SaltWashOffFactor'].Text      := FloatToStr(LMineModule.SaltWashOffFactor);
      LSectionNode.ChildNodes['InitialSaltStore'].Text       := FloatToStr(LMineModule.InitialSaltStore);

      AddXMLAllOutflowRoutes(ARootNode, LMineModule.ModuleID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentMineProperties.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentMineProperties.AddPopulationData';
begin
  try
//    AddXMLAllNetworkRoutes(ARootNode);
    AddXMLAllRunOffModules(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineProperties.XSDText : String;
const OPNAME = 'TXMLAgentMineProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('MineProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentMineProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                         APropertyName     : String;
                                                         AFieldIndex       : String;
                                                         AXSDDoc           : IXMLDocument;
                                                         AXMLDocumentIn    : IXMLDocument;
                                                         AXMLDocumentOut   : IXMLDocument;
                                                         AStopOnFirstError : Boolean;
                                                         AErrorList        : TStringList;
                                                         AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentMineProperties.DoAdditionalValidation';
var
  LResult                  : Boolean;
  LInputRootNode           : IXMLNode;
  LOutputRootNode          : IXMLNode;
  LSectionNode             : IXMLNode;
  LTempResult              : Boolean;
  LXSDSchemaNode           : IXMLNode;
  LErrorMsg                : String;
  LRouteNoStr              : String;
  LRunOffModuleNoStr       : String;
  LAllOutflowRoutes        : IXMLNode;
  LAllRunOffModules        : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode        := AXMLDocumentIn.DocumentElement;
    LAllOutflowRoutes     := LInputRootNode.ChildNodes['OutflowRoutes'];
    LAllRunOffModules     := LInputRootNode.ChildNodes['AllRunOffModules'];
    LOutputRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode          := LOutputRootNode.ChildNodes['MineProperties'];
    LXSDSchemaNode        := GetXSDSchemaNode(AXSDDoc);
    LResult               := TRUE;

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

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'OutflowRouteNoToRiver') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['OutflowRouteNoToRiver'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Outflow route to river.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('OutflowRouteNoToRiver');
      end;
      LResult := LResult AND LTempResult;
    end;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'OutflowRouteNoToPCD') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['OutflowRouteNoToPCD'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, TRUE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Outflow route to central PCD.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('OutflowRouteNoToPCD');
      end;
      LResult := LResult AND LTempResult;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


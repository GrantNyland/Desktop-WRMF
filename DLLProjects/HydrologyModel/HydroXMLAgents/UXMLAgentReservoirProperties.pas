(******************************************************************************)
(* This unit contains the class TXMLAgentReservoirProperties.
(******************************************************************************)
unit UXMLAgentReservoirProperties;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentReservoirProperties = class(TXMLAgent)
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

procedure TXMLAgentReservoirProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentReservoirProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'NetworkSequence,ModuleNumber,Active,Latitude,Longitude,ReservoirName,MAP,' +
                                'RainfallFileName,AreaPower,SpillageRouteNo,InitialStorageState';
    FAllDescriptions.CommaText := '"Network sequence","Module number","Active","Latitude","Longitude","Reservoir name","MAP",' +
                                  '"Rainfall file name","Area power","Spillage route","Initial storage"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentReservoirProperties.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentReservoirProperties.AddPopulationData';
begin
  try
//    AddXMLAllNetworkRoutes(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentReservoirProperties.XSDText : String;
const OPNAME = 'TXMLAgentReservoirProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('ReservoirProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentReservoirProperties.AddSectionData (AModule    : INetworkModule;
                                                       ASectionNo : Integer;
                                                       ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentReservoirProperties.AddSectionData';
var
  LSectionNode       : IXMLNode;
  LReservoirModule   : IReservoirModule;
begin
  try
    LReservoirModule := FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByID[AModule.ModuleID];
    if (LReservoirModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'ReservoirProperties';

      LSectionNode := ARootNode.ChildNodes['ReservoirProperties'];
      LSectionNode.ChildNodes['NetworkSequence'].Text     := IntToStr(LReservoirModule.NetworkSequence);
      LSectionNode.ChildNodes['ModuleNumber'].Text        := IntToStr(LReservoirModule.ModuleNumber);
      LSectionNode.ChildNodes['Active'].Text              := LReservoirModule.Active;
      LSectionNode.ChildNodes['Latitude'].Text            := FloatToStr(LReservoirModule.Latitude);
      LSectionNode.ChildNodes['Longitude'].Text           := FloatToStr(LReservoirModule.Longitude);
      LSectionNode.ChildNodes['ReservoirName'].Text       := LReservoirModule.ReservoirName;
      LSectionNode.ChildNodes['MAP'].Text                 := FloatToStr(LReservoirModule.MAP);
      LSectionNode.ChildNodes['RainfallFileName'].Text    := LReservoirModule.RainfallFileName;
      LSectionNode.ChildNodes['AreaPower'].Text           := FloatToStr(LReservoirModule.AreaPower);
      LSectionNode.ChildNodes['SpillageRouteNo'].Text     := IntToStr(LReservoirModule.SpillageRouteNo);
      LSectionNode.ChildNodes['InitialStorageState'].Text := FloatToStr(LReservoirModule.InitialStorageState);

      AddXMLAllOutflowRoutes(ARootNode, LReservoirModule.ModuleID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentReservoirProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                              APropertyName     : String;
                                                              AFieldIndex       : String;
                                                              AXSDDoc           : IXMLDocument;
                                                              AXMLDocumentIn    : IXMLDocument;
                                                              AXMLDocumentOut   : IXMLDocument;
                                                              AStopOnFirstError : Boolean;
                                                              AErrorList        : TStringList;
                                                              AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentReservoirProperties.DoAdditionalValidation';
var
  LResult             : Boolean;
  LInputRootNode      : IXMLNode;
  LOutputRootNode     : IXMLNode;
  LSectionNode        : IXMLNode;
  LAllOutflowRoutes   : IXMLNode;
  LTempResult         : Boolean;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
  LSpillageRouteNoStr : String;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LAllOutflowRoutes := LInputRootNode.ChildNodes['OutflowRoutes'];
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['ReservoirProperties'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);
    LResult           := TRUE;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'SpillageRouteNo') OR (APropertyName = ''))) then
    begin
      LSpillageRouteNoStr := Trim(LSectionNode.ChildNodes['SpillageRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LSpillageRouteNoStr, LAllOutflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Spillage route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('SpillageRouteNo');
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


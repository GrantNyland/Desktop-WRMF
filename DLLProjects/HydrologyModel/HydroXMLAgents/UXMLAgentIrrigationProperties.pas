(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationProperties.
(******************************************************************************)
unit UXMLAgentIrrigationProperties;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationProperties = class(TXMLAgent)
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

procedure TXMLAgentIrrigationProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentIrrigationProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'NetworkSequence,ModuleNumber,Active,Latitude,Longitude,IrrigationName,' +
                                'VersionNo,MAP,ModelType,LastUsedModelType,RainfallFileName,' +
                                'MaxAnnualIrrAllocation,AbstractionRouteNo,ReturnFlowRouteNo,' +
                                'ReturnFlowPercentage';
    FAllDescriptions.CommaText := '"Network sequence","Module number","Active","Latitude","Longitude","Irrigation name",' +
                                  '"Version No","MAP","Model type","Last used model type","Rainfall file name",' +
                                  '"Maximum annual irrigation allocation","Abstraction route","Return flow route",' +
                                  '"Return flow percentage"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationProperties.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationProperties.AddPopulationData';
begin
  try
//    AddXMLAllNetworkRoutes(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationProperties.AddSectionData (AModule    : INetworkModule;
                                                        ASectionNo : Integer;
                                                        ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationProperties.AddSectionData';
var
  LSectionNode      : IXMLNode;
  LIrrigationModule : IIrrigationModule;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text    := 'IrrigationProperties';

      LSectionNode := ARootNode.ChildNodes['IrrigationProperties'];
      LSectionNode.ChildNodes['NetworkSequence'].Text        := IntToStr(LIrrigationModule.NetworkSequence);
      LSectionNode.ChildNodes['ModuleNumber'].Text           := IntToStr(LIrrigationModule.ModuleNumber);
      LSectionNode.ChildNodes['Active'].Text                 := LIrrigationModule.Active;
      LSectionNode.ChildNodes['Latitude'].Text               := FloatToStr(LIrrigationModule.Latitude);
      LSectionNode.ChildNodes['Longitude'].Text              := FloatToStr(LIrrigationModule.Longitude);
      LSectionNode.ChildNodes['IrrigationName'].Text         := LIrrigationModule.IrrigationName;
      LSectionNode.ChildNodes['VersionNo'].Text              := IntToStr(LIrrigationModule.VersionNo);
      LSectionNode.ChildNodes['MAP'].Text                    := FloatToStr(LIrrigationModule.MAP);
      LSectionNode.ChildNodes['ModelType'].Text              := IntToStr(LIrrigationModule.ModelType);
      LSectionNode.ChildNodes['LastUsedModelType'].Text      := IntToStr(LIrrigationModule.LastUsedModelType);
      LSectionNode.ChildNodes['RainfallFileName'].Text       := LIrrigationModule.RainfallFileName;
      LSectionNode.ChildNodes['MaxAnnualIrrAllocation'].Text := FloatToStr(LIrrigationModule.MaxAnnualIrrigationAllocation);
      LSectionNode.ChildNodes['AbstractionRouteNo'].Text     := IntToStr(LIrrigationModule.AbstractionRouteNo);
      LSectionNode.ChildNodes['ReturnFlowRouteNo'].Text      := IntToStr(LIrrigationModule.ReturnFlowRouteNo);
      LSectionNode.ChildNodes['ReturnFlowPercentage'].Text   := FloatToStr(LIrrigationModule.ReturnFlowPercentage);

      AddXMLAllOutflowRoutes(ARootNode, LIrrigationModule.ModuleID);
      AddXMLAllInflowRoutes(ARootNode, LIrrigationModule.ModuleID);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationProperties.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                               APropertyName     : String;
                                                               AFieldIndex       : String;
                                                               AXSDDoc           : IXMLDocument;
                                                               AXMLDocumentIn    : IXMLDocument;
                                                               AXMLDocumentOut   : IXMLDocument;
                                                               AStopOnFirstError : Boolean;
                                                               AErrorList        : TStringList;
                                                               AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationProperties.DoAdditionalValidation';
var
  LResult             : Boolean;
  LInputRootNode      : IXMLNode;
  LOutputRootNode     : IXMLNode;
  LSectionNode        : IXMLNode;
  LTempResult         : Boolean;
  LXSDSchemaNode      : IXMLNode;
  LErrorMsg           : String;
  LRouteNoStr         : String;
  LAllOutflowRoutes   : IXMLNode;
  LAllInflowRoutes    : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LAllOutflowRoutes := LInputRootNode.ChildNodes['OutflowRoutes'];
    LAllInflowRoutes  := LInputRootNode.ChildNodes['InflowRoutes'];
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['IrrigationProperties'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);

    LResult           := TRUE;
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'AbstractionRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['AbstractionRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllInflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Abstraction route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('AbstractionRouteNo');
      end;
    end;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'ReturnFlowRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['ReturnFlowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Return flow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('ReturnFlowRouteNo');
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


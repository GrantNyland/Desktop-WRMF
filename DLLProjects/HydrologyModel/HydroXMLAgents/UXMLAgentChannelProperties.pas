(******************************************************************************)
(* This unit contains the class TXMLAgentChannelProperties.
(******************************************************************************)
unit UXMLAgentChannelProperties;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentChannelProperties = class(TXMLAgent)
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

procedure TXMLAgentChannelProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentChannelProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'NetworkSequence,ModuleNumber,Active,Latitude,Longitude,ChannelName,VersionNo,' +
                                'WetlandMAP,RainfallFileName,MonthlyBedLoss,WetlandStorage,WetlandArea,' +
                                'WetlandRechargeCoefficient,PrincipalOutflowRouteNo,WetlandsInflowRouteNo,' +
                                'WetlandsOutflowRouteNo,DiversionRouteNo,BankfillCapacity,DiversionEfficiency,' +
                                'MaxMonthlyDiversionCapacity,QDiv,WetlandType,BankfillArea,BankfillVolume,' +
                                'PowerOfAreaCapCurve,BankfillCapacityComp,WetlandInflowProportion,' +
                                'ChannelInflowProportion';
    FAllDescriptions.CommaText := '"Network sequence","Module number","Active","Latitude","Longitude","Channel reach name","Version no",' +
                                  '"Wetland MAP","Rainfall file name","Monthly bed loss","Wetland/aquifer storage", "Wetland/aquifer area",' +
                                  '"Wetland/aquifer recharge coefficient","Principal outflow route","Wetlands inflow route",' +
                                  '"Wetlands outflow route","Diversion route","Bankfill capacity of river channel","Diversion efficiency",' +
                                  '"Maximum monthly diversion capacity","QDiv","Wetland type","Area of wetland at bankfill level",' +
                                  '"Volume of wetland at bankfill level","Power of area-volume relationship","Bankfill capacity of river channel",' +
                                  '"Proportion of flow in excess of bankfill into wetland","Proportion of wetland volume (over bankfill) into channel"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentChannelProperties.AddSectionData (AModule    : INetworkModule;
                                                     ASectionNo : Integer;
                                                     ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentChannelProperties.AddSectionData';
var
  LSectionNode       : IXMLNode;
  LChannelModule     : IChannelModule;
begin
  try
    LChannelModule := FHydrologyModel.Network.ChannelModuleAgent.ChannelModuleByID[AModule.ModuleID];
    if (LChannelModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'ChannelProperties';

      LSectionNode := ARootNode.ChildNodes['ChannelProperties'];
      LSectionNode.ChildNodes['NetworkSequence'].Text               := IntToStr(LChannelModule.NetworkSequence);
      LSectionNode.ChildNodes['ModuleNumber'].Text                  := IntToStr(LChannelModule.ModuleNumber);
      LSectionNode.ChildNodes['Active'].Text                        := LChannelModule.Active;
      LSectionNode.ChildNodes['Latitude'].Text                      := FloatToStr(LChannelModule.Latitude);
      LSectionNode.ChildNodes['Longitude'].Text                     := FloatToStr(LChannelModule.Longitude);
      LSectionNode.ChildNodes['ChannelName'].Text                   := LChannelModule.ChannelName;
      LSectionNode.ChildNodes['VersionNo'].Text                     := IntToStr(LChannelModule.VersionNo);
      LSectionNode.ChildNodes['WetlandMAP'].Text                    := FloatToStr(LChannelModule.WetlandMAP);
      LSectionNode.ChildNodes['RainfallFileName'].Text              := LChannelModule.RainfallFileName;
      LSectionNode.ChildNodes['MonthlyBedLoss'].Text                := FloatToStr(LChannelModule.MonthlyBedLoss);
      LSectionNode.ChildNodes['WetlandStorage'].Text                := FloatToStr(LChannelModule.WetlandStorage);
      LSectionNode.ChildNodes['WetlandArea'].Text                   := FloatToStr(LChannelModule.WetlandArea);
      LSectionNode.ChildNodes['WetlandRechargeCoefficient'].Text    := FloatToStr(LChannelModule.WetlandRechargeCoefficient);
      LSectionNode.ChildNodes['PrincipalOutflowRouteNo'].Text       := IntToStr(LChannelModule.PrincipalOutflowRouteNo);
      LSectionNode.ChildNodes['WetlandsInflowRouteNo'].Text         := IntToStr(LChannelModule.WetlandsInflowRouteNo);
      LSectionNode.ChildNodes['WetlandsOutflowRouteNo'].Text        := IntToStr(LChannelModule.WetlandsOutflowRouteNo);
      LSectionNode.ChildNodes['DiversionRouteNo'].Text              := IntToStr(LChannelModule.DiversionRouteNo);
      LSectionNode.ChildNodes['BankfillCapacity'].Text              := FloatToStr(LChannelModule.BankfillCapacity);
      LSectionNode.ChildNodes['DiversionEfficiency'].Text           := FloatToStr(LChannelModule.DiversionEfficiency);
      LSectionNode.ChildNodes['MaxMonthlyDiversionCapacity'].Text   := FloatToStr(LChannelModule.MaxMonthlyDiversionCapacity);
      LSectionNode.ChildNodes['WetlandType'].Text                   := IntToStr(LChannelModule.WetlandType);
      LSectionNode.ChildNodes['BankfillArea'].Text                  := FloatToStr(LChannelModule.BankfillArea);
      LSectionNode.ChildNodes['BankfillVolume'].Text                := FloatToStr(LChannelModule.BankfillVolume);
      LSectionNode.ChildNodes['PowerOfAreaCapCurve'].Text           := FloatToStr(LChannelModule.PowerOfAreaCapCurve);
      LSectionNode.ChildNodes['BankfillCapacityComp'].Text          := FloatToStr(LChannelModule.BankfillCapacityComprehensive);
      LSectionNode.ChildNodes['WetlandInflowProportion'].Text       := FloatToStr(LChannelModule.WetlandInflowProportion);
      LSectionNode.ChildNodes['ChannelInflowProportion'].Text       := FloatToStr(LChannelModule.ChannelInflowProportion);
      LSectionNode.ChildNodes['QDiv'].Text                          := FloatToStr(LChannelModule.QDiv);

      AddXMLAllOutflowRoutes(ARootNode, LChannelModule.ModuleID);
      AddXMLAllInflowRoutes(ARootNode, LChannelModule.ModuleID);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentChannelProperties.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentChannelProperties.AddPopulationData';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentChannelProperties.XSDText : String;
const OPNAME = 'TXMLAgentChannelProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('ChannelProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentChannelProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                            APropertyName     : String;
                                                            AFieldIndex       : String;
                                                            AXSDDoc           : IXMLDocument;
                                                            AXMLDocumentIn    : IXMLDocument;
                                                            AXMLDocumentOut   : IXMLDocument;
                                                            AStopOnFirstError : Boolean;
                                                            AErrorList        : TStringList;
                                                            AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentChannelProperties.DoAdditionalValidation';
var
  LResult           : Boolean;
  LInputRootNode    : IXMLNode;
  LOutputRootNode   : IXMLNode;
  LSectionNode      : IXMLNode;
  LTempResult       : Boolean;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
  LRouteNoStr       : String;
  LAllOutflowRoutes : IXMLNode;
  LAllInflowRoutes  : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LAllOutflowRoutes := LInputRootNode.ChildNodes['OutflowRoutes'];
    LAllInflowRoutes  := LInputRootNode.ChildNodes['InflowRoutes'];
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['ChannelProperties'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);

    LResult           := TRUE;
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'PrincipalOutflowRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['PrincipalOutflowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, FALSE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Principal outflow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('PrincipalOutflowRouteNo');
      end;
      LResult := LResult AND LTempResult;
    end;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'WetlandsInflowRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['WetlandsInflowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllInflowRoutes, TRUE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Wetlands inflow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('WetlandsInflowRouteNo');
      end;
      LResult := LResult AND LTempResult;
    end;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'WetlandsOutflowRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['WetlandsOutflowRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, TRUE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Wetlands outflow route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('WetlandsOutflowRouteNo');
      end;
      LResult := LResult AND LTempResult;
    end;

    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'DiversionRouteNo') OR (APropertyName = ''))) then
    begin
      LRouteNoStr := Trim(LSectionNode.ChildNodes['DiversionRouteNo'].Text);
      LTempResult := IsNetworkRouteValid(LRouteNoStr, LAllOutflowRoutes, TRUE);
      if (NOT LTempResult) then
      begin
        LErrorMsg := 'Diversion route is invalid.';
        AErrorMessages.Add(LErrorMsg);
        AErrorList.Add('DiversionRouteNo');
      end;
      LResult := LResult AND LTempResult;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


(******************************************************************************)
(* This unit contains the class TXMLAgentReservoirVolumeArea.
(******************************************************************************)
unit UXMLAgentReservoirVolumeArea;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentReservoirVolumeArea = class(TXMLAgent)
  protected
  public
    function XSDText : String; override;
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
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

procedure TXMLAgentReservoirVolumeArea.AddSectionData (AModule    : INetworkModule;
                                                       ASectionNo : Integer;
                                                       ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentReservoirVolumeArea.AddSectionData';
var
  LReservoirModule : IReservoirModule;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LNode            : IXMLNode;
  LIndex           : Integer;
  LVolumeAreaData  : IYearVolumeAreaData;
begin
  try
    LReservoirModule := FHydrologyModel.Network.ReservoirModuleAgent.ReservoirModuleByID[AModule.ModuleID];
    if (LReservoirModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'ReservoirVolumeArea';

      LSectionNode  := ARootNode.ChildNodes['ReservoirVolumeArea'];
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LReservoirModule.VolumeAreaDataCount - 1 do
      begin
        LVolumeAreaData := LReservoirModule.VolumeAreaDataByIndex[LIndex];
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.AddChild('FullSupplyVolume');
        LNode.AddChild('SurfaceArea');
        LNode.ChildNodes['Year'].Text   := IntToStr(LVolumeAreaData.Year);
        LNode.ChildNodes['FullSupplyVolume'].Text := FloatToStr(LVolumeAreaData.Volume);
        LNode.ChildNodes['SurfaceArea'].Text   := FloatToStr(LVolumeAreaData.Area);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentReservoirVolumeArea.XSDText : String;
const OPNAME = 'TXMLAgentReservoirVolumeArea.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('ReservoirVolumeArea.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentReservoirVolumeArea.DoAdditionalValidation (AContext          : TValidationContext;
                                                              APropertyName     : String;
                                                              AFieldIndex       : String;
                                                              AXSDDoc           : IXMLDocument;
                                                              AXMLDocumentIn    : IXMLDocument;
                                                              AXMLDocumentOut   : IXMLDocument;
                                                              AStopOnFirstError : Boolean;
                                                              AErrorList        : TStringList;
                                                              AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentReservoirVolumeArea.DoAdditionalValidation';
var
  LResult         : Boolean;
  LSectionNode    : IXMLNode;
  LDataListNode   : IXMLNode;
  LNode           : IXMLNode;
  LIndex          : Integer;
  LRootNode       : IXMLNode;
  LTempResult     : Boolean;
  LXSDSchemaNode  : IXMLNode;
  LErrorMsg       : String;
begin
  Result := FALSE;
  try
    LRootNode       := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode    := LRootNode.ChildNodes['ReservoirVolumeArea'];
    LDataListNode   := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode  := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    // Year
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Year') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Year', 'Year', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Year,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // Volume
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'FullSupplyVolume') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'FullSupplyVolume', 'FullSupplyVolume', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('FullSupplyVolume,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // Area
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'SurfaceArea') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'SurfaceArea', 'SurfaceArea', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('SurfaceArea,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//243
end.


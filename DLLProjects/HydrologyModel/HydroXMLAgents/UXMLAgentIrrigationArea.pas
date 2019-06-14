(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationArea.
(******************************************************************************)
unit UXMLAgentIrrigationArea;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationArea = class(TXMLAgent)
  protected
  public
    procedure PopulatePropertyLists; override;
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

procedure TXMLAgentIrrigationArea.PopulatePropertyLists;
const OPNAME = 'TXMLAgentIrrigationArea.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'AreaInterpolationType';
    FAllDescriptions.CommaText := '"Area interpolation type"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentIrrigationArea.AddSectionData (AModule    : INetworkModule;
                                                  ASectionNo : Integer;
                                                  ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationArea.AddSectionData';
var
  LIrrigationModule : IIrrigationModule;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'IrrigationArea';

      LSectionNode := ARootNode.ChildNodes['IrrigationArea'];
      LSectionNode.ChildNodes['AreaInterpolationType'].Text := IntToStr(LIrrigationModule.AreaInterpolationType);
      LDataListNode := LSectionNode.ChildNodes['DataList'];
      for LIndex := 0 to LIrrigationModule.NoOfAreaDataPoints - 1 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.AddChild('Area');
        LNode.ChildNodes['Year'].Text   := IntToStr(LIrrigationModule.AreaYearByIndex[LIndex]);
        LNode.ChildNodes['Area'].Text   := FloatToStr(LIrrigationModule.AreaValueByIndex[LIndex]);
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationArea.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationArea.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationArea.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationArea.DoAdditionalValidation (AContext          : TValidationContext;
                                                         APropertyName     : String;
                                                         AFieldIndex       : String;
                                                         AXSDDoc           : IXMLDocument;
                                                         AXMLDocumentIn    : IXMLDocument;
                                                         AXMLDocumentOut   : IXMLDocument;
                                                         AStopOnFirstError : Boolean;
                                                         AErrorList        : TStringList;
                                                         AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationArea.DoAdditionalValidation';
var
  LResult        : Boolean;
  LSectionNode   : IXMLNode;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LIndex         : Integer;
  LRootNode      : IXMLNode;
  LTempResult    : Boolean;
  LXSDSchemaNode : IXMLNode;
  LErrorMsg      : String;
begin
  Result := FALSE;
  try
    LRootNode      := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['IrrigationArea'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

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

    // Area
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'Area') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'Area', 'Area', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('Area,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


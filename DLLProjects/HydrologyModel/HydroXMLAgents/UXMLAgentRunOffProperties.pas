(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffProperties.
(******************************************************************************)
unit UXMLAgentRunOffProperties;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffProperties = class(TXMLAgent)
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

procedure TXMLAgentRunOffProperties.AddSectionData (AModule    : INetworkModule;
                                                    ASectionNo : Integer;
                                                    ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffProperties.AddSectionData';
var
  LSectionNode     : IXMLNode;
  LRunOffModule    : IRunOffModule;
  LAPanFactorsNode : IXMLNode;
  LNode            : IXMLNode;
  LIndex           : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text         := 'RunOffProperties';

      LSectionNode := ARootNode.ChildNodes['RunOffProperties'];

      LSectionNode.ChildNodes['NetworkSequence'].Text         := IntToStr(LRunOffModule.NetworkSequence);
      LSectionNode.ChildNodes['ModuleNumber'].Text            := IntToStr(LRunOffModule.ModuleNumber);
      LSectionNode.ChildNodes['Active'].Text                  := LRunOffModule.Active;
      LSectionNode.ChildNodes['Latitude'].Text                := FloatToStr(LRunOffModule.Latitude);
      LSectionNode.ChildNodes['Longitude'].Text               := FloatToStr(LRunOffModule.Longitude);
      LSectionNode.ChildNodes['RunOffName'].Text              := LRunOffModule.RunOffName;
      LSectionNode.ChildNodes['VersionNo'].Text               := IntToStr(LRunOffModule.VersionNo);
      LSectionNode.ChildNodes['CatchmentArea'].Text           := FloatToStr(LRunOffModule.CatchmentArea);
      LSectionNode.ChildNodes['CatchmentMAP'].Text            := FloatToStr(LRunOffModule.CatchmentMAP);
      LSectionNode.ChildNodes['RainfallFileName'].Text        := LRunOffModule.RainfallFileName;
      LSectionNode.ChildNodes['ProduceNaturalisedFlows'].Text := IntToStr(LRunOffModule.ProduceNaturalisedFlows);
      // A-Pan
      LAPanFactorsNode := LSectionNode.ChildNodes['APanDataList'];
      for LIndex := 1 to 12 do
      begin
        LNode := LAPanFactorsNode.AddChild('Data');
        LNode.AddChild('Month');
        LNode.AddChild('APanFactor');
        LNode.ChildNodes['Month'].Text     := IntToStr(LIndex);
        LNode.ChildNodes['APanFactor'].Text := FloatToStr(LRunOffModule.APanFactor[LIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffProperties.XSDText : String;
const OPNAME = 'TXMLAgentRunOffProperties.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffProperties.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffProperties.PopulatePropertyLists;
const OPNAME = 'TXMLAgentRunOffProperties.PopulatePropertyLists';
begin
  try
    FAllProperties.CommaText := 'NetworkSequence,ModuleNumber,Active,Latitude,Longitude,RunOffName,VersionNo,' +
                                'CatchmentArea,CatchmentMAP,RainfallFileName,ProduceNaturalisedFlows';
    FAllDescriptions.CommaText := '"Network sequence","Module number","Active","Latitude","Longitude","RunOff name","Version no",' +
                                  '"Catchment area","Catchment MAP","Rainfall file name","Produce naturalised flows"';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffProperties.DoAdditionalValidation (AContext          : TValidationContext;
                                                           APropertyName     : String;
                                                           AFieldIndex       : String;
                                                           AXSDDoc           : IXMLDocument;
                                                           AXMLDocumentIn    : IXMLDocument;
                                                           AXMLDocumentOut   : IXMLDocument;
                                                           AStopOnFirstError : Boolean;
                                                           AErrorList        : TStringList;
                                                           AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffProperties.DoAdditionalValidation';
var
  LResult           : Boolean;
  LOutputRootNode   : IXMLNode;
  LSectionNode      : IXMLNode;
  LPanDataListNode  : IXMLNode;
  LTempResult       : Boolean;
  LIndex            : Integer;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
  LNode             : IXMLNode;
begin
  Result := FALSE;
  try
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['RunOffProperties'];
    LPanDataListNode  := LSectionNode.ChildNodes['APanDataList'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);
    LResult           := TRUE;

    // APanFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'APanFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LPanDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LPanDataListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'APanFactor', 'APan factor', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('APanFactor,' + IntToStr(LIndex+1));
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


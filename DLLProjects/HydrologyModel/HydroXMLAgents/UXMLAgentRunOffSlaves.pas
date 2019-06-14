(******************************************************************************)
(* This unit contains the class TXMLAgentRunOffSlaves.
(******************************************************************************)
unit UXMLAgentRunOffSlaves;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentRunOffSlaves = class(TXMLAgent)
  protected
  public
    function XSDText : String; override;
    procedure AddPopulationData (ARootNode : IXMLNode); override;
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

procedure TXMLAgentRunOffSlaves.AddPopulationData (ARootNode : IXMLNode);
const OPNAME = 'TXMLAgentRunOffSlaves.AddPopulationData';
begin
  try
    AddXMLAllRunOffModules(ARootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentRunOffSlaves.AddSectionData (AModule    : INetworkModule;
                                                ASectionNo : Integer;
                                                ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentRunOffSlaves.AddSectionData';
var
  LSectionNode  : IXMLNode;
  LRunOffModule : IRunOffModule;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LIndex        : Integer;
begin
  try
    LRunOffModule := FHydrologyModel.Network.RunOffModuleAgent.RunOffModuleByID[AModule.ModuleID];
    if (LRunOffModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'RunOffSlaves';

      LSectionNode  := ARootNode.ChildNodes['RunOffSlaves'];
      LDataListNode := LSectionNode.ChildNodes['DataList'];

      // Slaves
      for LIndex := 0 to LRunOffModule.NoOfSlaves - 1 do
      begin
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('SlaveModuleNo');
        LNode.ChildNodes['SlaveModuleNo'].Text := IntToStr(LRunOffModule.SlaveModuleNoByIndex(LIndex))
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffSlaves.XSDText : String;
const OPNAME = 'TXMLAgentRunOffSlaves.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('RunOffSlaves.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentRunOffSlaves.DoAdditionalValidation (AContext          : TValidationContext;
                                                       APropertyName     : String;
                                                       AFieldIndex       : String;
                                                       AXSDDoc           : IXMLDocument;
                                                       AXMLDocumentIn    : IXMLDocument;
                                                       AXMLDocumentOut   : IXMLDocument;
                                                       AStopOnFirstError : Boolean;
                                                       AErrorList        : TStringList;
                                                       AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentRunOffSlaves.DoAdditionalValidation';
var
  LResult           : Boolean;
  LInputRootNode    : IXMLNode;
  LOutputRootNode   : IXMLNode;
  LSectionNode      : IXMLNode;
  LSlavesListNode   : IXMLNode;
  LModuleNumber     : String;
  LSlaveModuleNo    : String;
  LTempResult       : Boolean;
  LIndex            : Integer;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
  LNode             : IXMLNode;
  LAllRunOffModules : IXMLNode;
begin
  Result := FALSE;
  try
    LInputRootNode    := AXMLDocumentIn.DocumentElement;
    LOutputRootNode   := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode      := LOutputRootNode.ChildNodes['RunOffSlaves'];
    LSlavesListNode   := LSectionNode.ChildNodes['DataList'];
    LAllRunOffModules := LInputRootNode.ChildNodes['AllRunOffModules'];
    LXSDSchemaNode    := GetXSDSchemaNode(AXSDDoc);
    LResult           := TRUE;
    LModuleNumber     := Trim(LOutputRootNode.ChildNodes['ModuleNumber'].Text);

    // SlaveModuleNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'SlaveModuleNo') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LSlavesListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LNode := LSlavesListNode.ChildNodes.Get(LIndex);
          LTempResult := ValidateProperty(AContext, 'SlaveModuleNo', 'Slave module number', LNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('SlaveModuleNo,' + IntToStr(LIndex+1));
          end
          else
          begin
            LSlaveModuleNo := Trim(LNode.ChildNodes['SlaveModuleNo'].Text);
            if (LModuleNumber = LSlaveModuleNo) then
            begin
              LTempResult := FALSE;
              LErrorMsg := 'Slave module number may not be the same as module number of RunOff.';
              AErrorMessages.Add(LErrorMsg);
              AErrorList.Add('SlaveModuleNo,' + IntToStr(LIndex+1));
            end
            else
            begin
              LTempResult := IsRunOffModuleValid(LSlaveModuleNo, LAllRunOffModules, FALSE);
              if (NOT LTempResult) then
              begin
                LErrorMsg := 'Slave module number is invalid';
                AErrorMessages.Add(LErrorMsg);
                AErrorList.Add('SlaveModuleNo,' + IntToStr(LIndex+1));
              end
            end;
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


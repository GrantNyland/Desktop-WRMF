(******************************************************************************)
(* This unit contains the class TXMLAgentNetworkSequence.
(******************************************************************************)
unit UXMLAgentNetworkSequence;


interface

uses
  xmldom, XMLIntf, msxmldom, XMLDoc,
  SysUtils,
  Classes,
  UModuleDBManager,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentNetworkSequence = class(TXMLAgent)
  protected
  public
    procedure AddSectionData (AModule    : INetworkModule;
                              ASectionNo : Integer;
                              ARootNode  : IXMLNode); override;
  end;

implementation

uses
  UErrorHandlingOperations;

procedure TXMLAgentNetworkSequence.AddSectionData (AModule    : INetworkModule;
                                                   ASectionNo : Integer;
                                                   ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentNetworkSequence.AddSectionData';
var
  LSectionNode         : IXMLNode;
  LDataListNode        : IXMLNode;
  LNode                : IXMLNode;
  LModuleIDList        : TStringList;
  LModuleNumberList    : TStringList;
  LNetworkSequenceList : TStringList;
  LModuleTypeList      : TStringList;
  LModuleTextList      : TStringList;
  LIndex               : Integer;
begin
  try
    ARootNode.ChildNodes['Section'].Text := 'ChangeNetworkSequence';
    LSectionNode := ARootNode.ChildNodes['ChangeNetworkSequence'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];

    LModuleIDList        := TStringList.Create;
    LModuleNumberList    := TStringList.Create;
    LNetworkSequenceList := TStringList.Create;
    LModuleTypeList      := TStringList.Create;
    LModuleTextList      := TStringList.Create;
    try

      if (GModuleDBManager.GetNetworkSequenceDataFromDB
                             (FHydrologyModel.Network.NetworkID, LModuleIDList, LModuleNumberList,
                              LNetworkSequenceList, LModuleTypeList, LModuleTextList)) then
      begin
        for LIndex := 0 to LModuleIDList.Count - 1 do
        begin
          LNode := LDataListNode.AddChild('Data');
          LNode.ChildNodes['ModuleID'].Text        := LModuleIDList.Strings[LIndex];
          LNode.ChildNodes['ModuleNumber'].Text    := LModuleNumberList.Strings[LIndex];
          LNode.ChildNodes['NetworkSequence'].Text := LNetworkSequenceList.Strings[LIndex];
          LNode.ChildNodes['ModuleType'].Text      := LModuleTypeList.Strings[LIndex];
          LNode.ChildNodes['ModuleText'].Text      := LModuleTextList.Strings[LIndex];
        end;
      end;  
    finally
      LModuleIDList.Free;
      LModuleNumberList.Free;
      LNetworkSequenceList.Free;
      LModuleTypeList.Free;
      LModuleTextList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


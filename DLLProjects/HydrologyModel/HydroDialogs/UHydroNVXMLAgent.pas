(******************************************************************************)
(* This unit contains the class THydroNVXMLAgent.
(******************************************************************************)
unit UHydroNVXMLAgent;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  UXMLAgent;

type THydroNVTextType = (nvtGeneral, nvtName, nvtObsFlowFile, nvtDupCounts, nvtOutCounts,
                         nvtDupElements, nvtOutElements,
                         nvtRQSimulatedRouteFlow, nvtRQDemands, nvtRQShortages, nvtRUNetCatchmentRunOff,
                         nvtRUTotalSurfaceRunOff, nvtRUGroundWaterOutFlow, nvtRUPavedAreaFlow, nvtRUPitmanS,
                         nvtRUAquiferStorage, nvtRUGroundWaterRecharge, nvtRUWeightedPitmanS, nvtRUGroundWaterBaseFlow,
                         nvtRUInterflow, nvtCRWetlandUpstreamFlow, nvtCRWetlandInFlow, nvtCRWetlandStorage,
                         nvtCRWetlandReturnFlow, nvtRVReservoirStorage, nvtMMPlantRunOff, nvtMMUGUpStreamAreaRunOff,
                         nvtMMUGRecharge, nvtMMUGBoardPillarRunOff, nvtMMUGHighExtractionRunOff, nvtMMSDSurfaceRunOff,
                         nvtMMSDSeepage, nvtMMSDPCDInFlow, nvtMMSDPCDStorage, nvtMMSDPCDSpillage,
                         nvtMMOCDisturbedAreaRunOff, nvtMMOCDisturbedAreaRecharge, nvtMMOCWorkingAreaRunOff, nvtMMOCDisturbedWorkingsRecharge,
                         nvtMMOCDisturbedWorkingsRunOff, nvtMMOCDisturbedWorkingsSeepage, nvtMMOCDisturbedWorkingsDecant, nvtMMOCPCDSpillage,
                         nvtMMOCPCDWaterBalance, nvtMMOCPCDMonthStartStorage, nvtMMOCPCDMonthEndStorage, nvtMMOCInspoilStorage);
type THydroNVTextTypeSet = set of THydroNVTextType;
type THydroNVElementSubType = (subNone, subPlantArea, subOpencast, subUnderground, subSlurryDump);

type

  THydroNVXMLAgent = class(TXMLAgent)
  protected
  public
    function CreateNVSelectElementInXMLDocument (AIDList    : TStringList;
                                                 ANameList  : TStringList;
                                                 AExistList : TStringList) : IXMLDocument;
    function CreateNVSelectObsPointInXMLDocument (AIDList       : TStringList;
                                                  ANameList     : TStringList;
                                                  ANewList      : TStringList;
                                                  AExistList    : TStringList) : IXMLDocument;
    function CreateNVSelectElementOutXMLDocument : IXMLDocument;
    function CreateNVSelectTextInXMLDocument : IXMLDocument;
    procedure AddNVSelectTextInElementType (ADocument      : IXMLDocument;
                                            AElementType   : String;
                                            ASubTypesList  : TStringList;
                                            AElementsList  : TStringList;
                                            ATextTypesList : TStringList);
    function CreateNVSelectTextOutXMLDocument : IXMLDocument;
  end;



implementation

uses
  UErrorHandlingOperations;

function THydroNVXMLAgent.CreateNVSelectElementInXMLDocument (AIDList    : TStringList;
                                                              ANameList  : TStringList;
                                                              AExistList : TStringList) : IXMLDocument;
const OPNAME = 'THydroNVXMLAgent.CreateNVSelectElementInXMLDocument';
var
  LDocument       : IXMLDocument;
  LRootNode       : IXMLNode;
  LAllNode        : IXMLNode;
  LExistNode      : IXMLNode;
  LIndex          : Integer;
  LNode           : IXMLNode;
begin
  Result := nil;
  try
    LDocument  := CreateXMLDocument('');
    LRootNode  := LDocument.GetDocBinding('NVSelectElementIn', TXMLNode) as IXMLNode;
    LAllNode   := LRootNode.ChildNodes['All'];
    LExistNode := LRootNode.ChildNodes['Exist'];
    
    for LIndex := 0 to AIDList.Count - 1 do
    begin
      LNode := LAllNode.AddChild('Element');
      LNode.AddChild('ID');
      LNode.ChildNodes['ID'].Text := AIDList.Strings[LIndex];
      LNode.AddChild('Name');
      LNode.ChildNodes['Name'].Text := ANameList.Strings[LIndex];
    end;
    for LIndex := 0 to AExistList.Count - 1 do
    begin
      LNode := LExistNode.AddChild('ID');
      LNode.Text := AExistList.Strings[LIndex];
    end;

    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVXMLAgent.CreateNVSelectObsPointInXMLDocument (AIDList       : TStringList;
                                                               ANameList     : TStringList;
                                                               ANewList      : TStringList;
                                                               AExistList    : TStringList) : IXMLDocument;
const OPNAME = 'THydroNVXMLAgent.CreateNVSelectObsPointInXMLDocument';
var
  LDocument       : IXMLDocument;
  LRootNode       : IXMLNode;
  LAllNode        : IXMLNode;
  LExistNode      : IXMLNode;
  LIndex          : Integer;
  LNode           : IXMLNode;
  LNewNode        : IXMLNode;
begin
  Result := nil;
  try
    LDocument  := CreateXMLDocument('');
    LRootNode  := LDocument.GetDocBinding('NVSelectElementIn', TXMLNode) as IXMLNode;
    LAllNode   := LRootNode.ChildNodes['All'];
    LExistNode := LRootNode.ChildNodes['Exist'];
    LNewNode   := LRootNode.ChildNodes['New'];

    for LIndex := 0 to AIDList.Count - 1 do
    begin
      LNode := LAllNode.AddChild('Element');
      LNode.AddChild('ID');
      LNode.ChildNodes['ID'].Text := AIDList.Strings[LIndex];
      LNode.AddChild('Name');
      LNode.ChildNodes['Name'].Text := ANameList.Strings[LIndex];
    end;
    for LIndex := 0 to AExistList.Count - 1 do
    begin
      LNode := LExistNode.AddChild('ID');
      LNode.Text := AExistList.Strings[LIndex];
    end;
    for LIndex := 0 to ANewList.Count - 1 do
    begin
      LNode := LNewNode.AddChild('ID');
      LNode.Text := ANewList.Strings[LIndex];
    end;

    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVXMLAgent.CreateNVSelectElementOutXMLDocument : IXMLDocument;
const OPNAME = 'THydroNVXMLAgent.CreateNVSelectElementOutXMLDocument';
var
  LDocument     : IXMLDocument;
  LRootNode     : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('NVSelectElementOut', TXMLNode);
    LRootNode.ChildNodes['Selected'].Text := '';
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVXMLAgent.CreateNVSelectTextInXMLDocument : IXMLDocument;
const OPNAME = 'THydroNVXMLAgent.CreateNVSelectTextInXMLDocument';
var
  LDocument            : IXMLDocument;
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
begin
  Result := nil;
  try
    LDocument            := CreateXMLDocument('');
    LRootNode            := LDocument.GetDocBinding('NVSelectTextIn', TXMLNode) as IXMLNode;
    LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVXMLAgent.CreateNVSelectTextOutXMLDocument : IXMLDocument;
const OPNAME = 'THydroNVXMLAgent.CreateNVSelectTextOutXMLDocument';
var
  LDocument     : IXMLDocument;
  LRootNode     : IXMLNode;
begin
  Result := nil;
  try
    LDocument := CreateXMLDocument('');
    LRootNode := LDocument.GetDocBinding('NVSelectTextOut', TXMLNode);
    LRootNode.ChildNodes['ElementType'].Text := '';
    LRootNode.ChildNodes['ElementNo'].Text := '';
    LRootNode.ChildNodes['TextType'].Text := '';
    LRootNode.ChildNodes['ElementSubType'].Text := '';
    LRootNode.ChildNodes['SectionNo'].Text := '';
    Result := LDocument;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVXMLAgent.AddNVSelectTextInElementType (ADocument      : IXMLDocument;
                                                         AElementType   : String;
                                                         ASubTypesList  : TStringList;
                                                         AElementsList  : TStringList;
                                                         ATextTypesList : TStringList);
const OPNAME = 'THydroNVXMLAgent.AddNVSelectTextInElementType';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LAllElementsNode     : IXMLNode;
  LAllSubElementsNode  : IXMLNode;
  LAllSubTypesNode     : IXMLNode;
  LAllTextTypesNode    : IXMLNode;
  LIndex               : Integer;
  LCount               : Integer;
  LNode                : IXMLNode;
  LSubNode             : IXMLNode;
  LSubElements         : TStringList;
  LTempTextList        : TStringList;
  LHasSubTypes         : Boolean;
begin
  try
    LRootNode            := ADocument.GetDocBinding('NVSelectTextIn', TXMLNode) as IXMLNode;
    LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
    LElementTypeNode     := LAllElementTypesNode.AddChild('ElementType');
    LElementTypeNode.AddChild('ElementTypeName');
    LElementTypeNode.ChildNodes['ElementTypeName'].Text := AElementType;

    LTempTextList := TStringList.Create;
    try
      LHasSubTypes := ASubTypesList <> nil;
      if (LHasSubTypes) then
      begin
        LAllSubTypesNode := LElementTypeNode.AddChild('AllSubTypes');
        for LIndex := 0 to ASubTypesList.Count - 1 do
        begin
          LTempTextList.CommaText := ASubTypesList.Strings[LIndex];
          LNode := LAllSubTypesNode.AddChild('SubType');
          LNode.AddChild('ElementSubType');
          LNode.AddChild('Name');
          LNode.ChildNodes['ElementSubType'].Text := LTempTextList.Strings[0];
          LNode.ChildNodes['Name'].Text      := LTempTextList.Strings[1];
        end;
      end;

      LAllTextTypesNode := LElementTypeNode.AddChild('AllTextTypes');
      for LIndex := 0 to ATextTypesList.Count - 1 do
      begin
        LTempTextList.CommaText := ATextTypesList.Strings[LIndex];
        LNode := LAllTextTypesNode.AddChild('TextType');
        if (LHasSubTypes) then
          LNode.AddChild('ElementSubType');
        LNode.AddChild('ID');
        LNode.AddChild('Name');
        if (LHasSubTypes) then
        begin
          LNode.ChildNodes['ElementSubType'].Text := LTempTextList.Strings[0];
          LNode.ChildNodes['ID'].Text             := LTempTextList.Strings[1];
          LNode.ChildNodes['Name'].Text           := LTempTextList.Strings[2];
        end
        else
        begin
          LNode.ChildNodes['ID'].Text        := LTempTextList.Strings[0];
          LNode.ChildNodes['Name'].Text      := LTempTextList.Strings[1];
        end;
      end;

      LAllElementsNode := LElementTypeNode.AddChild('AllElements');
      for LIndex := 0 to AElementsList.Count - 1 do
      begin
        LTempTextList.CommaText := AElementsList.Strings[LIndex];
        LNode := LAllElementsNode.AddChild('Element');
        LNode.AddChild('ID');
        LNode.ChildNodes['ID'].Text := LTempTextList.Strings[0];
        LNode.AddChild('Name');
        LNode.ChildNodes['Name'].Text := LTempTextList.Strings[1];
        if (LHasSubTypes) then
        begin
          LSubElements        := TStringList(AElementsList.Objects[LIndex]);
          LAllSubElementsNode := LNode.AddChild('AllSubElements');
          for LCount := 0 to LSubElements.Count - 1 do
          begin
            LSubNode := LAllSubElementsNode.AddChild('SubElement');
            LTempTextList.CommaText := LSubElements.Strings[LCount];
            LSubNode.AddChild('ElementSubType');
            LSubNode.AddChild('ID');
            LSubNode.AddChild('Name');
            LSubNode.ChildNodes['ElementSubType'].Text := LTempTextList.Strings[0];
            LSubNode.ChildNodes['ID'].Text             := LTempTextList.Strings[1];
            LSubNode.ChildNodes['Name'].Text           := LTempTextList.Strings[2];
          end;
        end;  
      end;

    finally
      LTempTextList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


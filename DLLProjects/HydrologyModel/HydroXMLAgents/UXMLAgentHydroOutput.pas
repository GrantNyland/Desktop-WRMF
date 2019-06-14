(******************************************************************************)
(* This unit contains the class TXMLAgentHydroOutput.
(******************************************************************************)
unit UXMLAgentHydroOutput;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentHydroOutput = class(TXMLAgent)
  protected
  public
    function XSDText : String; override;
    procedure AddPopulationData (ARootNode       : IXMLNode;
                                 AElementType    : String;
                                 AElementID      : Integer); override;
    procedure AddSectionData (AElementType    : WideString;
                              AElementSubType : WideString;
                              AElementNo      : Integer;
                              AElementID      : Integer;
                              ASubElementID   : Integer;
                              AResultTypeID   : Integer;
                              ARootNode       : IXMLNode); override;
  end;

implementation

uses
  UErrorHandlingOperations, StrUtils;

procedure TXMLAgentHydroOutput.AddPopulationData (ARootNode       : IXMLNode;
                                                  AElementType    : String;
                                                  AElementID      : Integer);
const OPNAME = 'TXMLAgentHydroOutput.AddPopulationData';
var
  LIndex           : Integer;
  LResultType      : IHydroResultType;
  LCount           : Integer;
  LAllTypesNode    : IXMLNode;
  LAllSectionsNode : IXMLNode;
  LNode            : IXMLNode;
  LMineModule      : IMineModule;
  LOpencastPit     : IOpencastPit;
  LUndergroud      : IUndergroundSection;
  LSlurryDump      : ISlurryDump;
begin
  try
    if (AElementType = 'MM') then
    begin
      LAllSectionsNode := ARootNode.ChildNodes['AllSections'];
      LMineModule := FHydrologyModel.Network.MineModuleAgent.MineModuleByID[AElementID];
      if (LMineModule <> nil) then
      begin
        LNode := LAllSectionsNode.AddChild('Section');
        LNode.AddChild('SectionNo');
        LNode.AddChild('SectionType');
        LNode.AddChild('SectionName');
        LNode.ChildNodes['SectionNo'].Text   := IntToStr(0);
        LNode.ChildNodes['SectionType'].Text := '';
        LNode.ChildNodes['SectionName'].Text := 'Plant Area';
        for LIndex := 0 to LMineModule.NoOfOpencastPits - 1 do
        begin
          LOpencastPit := LMineModule.OpencastPitByIndex[LIndex];
          LNode := LAllSectionsNode.AddChild('MineSection');
          LNode.AddChild('SectionNo');
          LNode.AddChild('SectionType');
          LNode.AddChild('SectionName');
          LNode.ChildNodes['SectionNo'].Text   := IntToStr(LOpencastPit.SectionNo);
          LNode.ChildNodes['SectionType'].Text := 'OC';
          LNode.ChildNodes['SectionName'].Text := LOpencastPit.SectionName;
        end;
        for LIndex := 0 to LMineModule.NoOfUndergroundSections - 1 do
        begin
          LUndergroud := LMineModule.UndergroundSectionByIndex[LIndex];
          LNode := LAllSectionsNode.AddChild('MineSection');
          LNode.AddChild('SectionNo');
          LNode.AddChild('SectionType');
          LNode.AddChild('SectionName');
          LNode.ChildNodes['SectionNo'].Text   := IntToStr(LUndergroud.SectionNo);
          LNode.ChildNodes['SectionType'].Text := 'UG';
          LNode.ChildNodes['SectionName'].Text := LUndergroud.SectionName;
        end;
        for LIndex := 0 to LMineModule.NoOfSlurryDumps - 1 do
        begin
          LSlurryDump := LMineModule.SlurryDumpByIndex[LIndex];
          LNode := LAllSectionsNode.AddChild('MineSection');
          LNode.AddChild('SectionNo');
          LNode.AddChild('SectionType');
          LNode.AddChild('SectionName');
          LNode.ChildNodes['SectionNo'].Text   := IntToStr(LSlurryDump.SectionNo);
          LNode.ChildNodes['SectionType'].Text := 'SD';
          LNode.ChildNodes['SectionName'].Text := LSlurryDump.SectionName;
        end;
      end;
    end;

    LAllTypesNode := ARootNode.ChildNodes['AllHydroResultTypes'];
    LCount := FHydrologyModel.Network.HydroOutputAgent.ResultTypeCount;
    for LIndex := 0 to LCount - 1 do
    begin
      LResultType := FHydrologyModel.Network.HydroOutputAgent.ResultTypeByIndex[LIndex];
      if (LResultType.ElementType = AElementType) {AND (LResultType.ElementSubType = AElementSubType))} then
      begin
        LNode := LAllTypesNode.AddChild('HydroResultType');
        LNode.AddChild('ResultTypeID');
        LNode.AddChild('ElementSubType');
        LNode.AddChild('Description');
//        LNode.AddChild('Units');
        LNode.ChildNodes['ResultTypeID'].Text   := IntToStr(LResultType.ResultTypeID);
        LNode.ChildNodes['ElementSubType'].Text := LResultType.ElementSubType;
        LNode.ChildNodes['Description'].Text    := LResultType.Description;
        LNode.ChildNodes['Units'].Text          := LResultType.Units;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TXMLAgentHydroOutput.AddSectionData (AElementType    : WideString;
                                               AElementSubType : WideString;
                                               AElementNo      : Integer;
                                               AElementID      : Integer;
                                               ASubElementID   : Integer;
                                               AResultTypeID   : Integer;
                                               ARootNode       : IXMLNode);
const OPNAME = 'TXMLAgentHydroOutput.AddSectionData';
var
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LHydroOutput      : IHydroOutput;
  LYearIndex        : Integer;
  LMonthIndex       : Integer;
  LNode             : IXMLNode;
  LElementName      : String;
  LYear             : Integer;
  LValue            : Double;
  LObsPoint         : IObservationPoint;
  LObservedFlow     : ITimeSeries;
begin
  try
    ARootNode.ChildNodes['Section'].Text := 'HydroOutput';

    LSectionNode  := ARootNode.ChildNodes['HydroOutput'];
    LDataListNode := LSectionNode.ChildNodes['TimeSeries'];
    LHydroOutput  := FHydrologyModel.Network.HydroOutputAgent.HydroOutput[AElementType, AElementSubType, AElementID, ASubElementID, AResultTypeID];

    if (LHydroOutput = nil) then
      ARootNode.ChildNodes['AllZero'].Text := '0'
    else
    begin
      if (AResultTypeID = 0) then
        ARootNode.ChildNodes['ResultTypeID'].Text := IntToStr(LHydroOutput.ResultTypeID);
      ARootNode.ChildNodes['AllZero'].Text        := IfThen(LHydroOutput.AllZero, '1', '0');
      for LYearIndex := 0 to LHydroOutput.YearCount - 1 do
      begin
        LYear := LHydroOutput.Year[LYearIndex];
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.ChildNodes['Year'].Text := IntToStr(LYear);
        for LMonthIndex := 1 to 12 do
        begin
          LElementName := 'Month' + Format('%2.2d', [LMonthIndex]);
          LNode.AddChild(LElementName);
          LValue := LHydroOutput.DataByYearMonth[LYear, LMonthIndex];
          LNode.ChildNodes[LElementName].Text := Format('%7.2f', [LValue]);
        end;
      end;
    end;
    if (AElementType = 'RQ') then
    begin
      LObsPoint := FHydrologyModel.Network.ObservationPointAgent.ObservationPointByRouteNo[AElementNo];
      if (LObsPoint <> nil) then
      begin
        LObservedFlow := LObsPoint.FlowData;
        LDataListNode := LSectionNode.ChildNodes['ObservedFlow'];
        for LYearIndex := 0 to LObservedFlow.YearCount - 1 do
        begin
          LYear := LObservedFlow.Year[LYearIndex];
          LNode := LDataListNode.AddChild('Data');
          LNode.AddChild('Year');
          LNode.ChildNodes['Year'].Text := IntToStr(LYear);
          for LMonthIndex := 1 to 12 do
          begin
            LElementName := 'Month' + Format('%2.2d', [LMonthIndex]);
            LNode.AddChild(LElementName);
            LValue := LObservedFlow.DataByYearMonth[LYear, LMonthIndex];
            LNode.ChildNodes[LElementName].Text := Format('%7.2f', [LValue]);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentHydroOutput.XSDText : String;
const OPNAME = 'TXMLAgentHydroOutput.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('HydroOutput.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


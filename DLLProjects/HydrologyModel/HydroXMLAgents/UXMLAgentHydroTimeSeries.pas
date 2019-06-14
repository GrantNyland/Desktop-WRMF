(******************************************************************************)
(* This unit contains the class TXMLAgentHydroTimeSeries.
(******************************************************************************)
unit UXMLAgentHydroTimeSeries;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentHydroTimeSeries = class(TXMLAgent)
  protected
  public
    function XSDText : String; override;
    procedure AddSectionData (ATimeSeries : ITimeSeries;
                              ATitle      : String;
                              ARootNode   : IXMLNode); override;
  end;

implementation

uses
  UErrorHandlingOperations;

procedure TXMLAgentHydroTimeSeries.AddSectionData (ATimeSeries : ITimeSeries;
                                                   ATitle      : String;
                                                   ARootNode   : IXMLNode);
const OPNAME = 'TXMLAgentHydroTimeSeries.AddSectionData';
var
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LTitleNode        : IXMLNode;
  LYearIndex        : Integer;
  LMonthIndex       : Integer;
  LNode             : IXMLNode;
  LElementName      : String;
  LYear             : Integer;
  LValue            : Double;
begin
  try
    ARootNode.ChildNodes['Section'].Text := 'HydroTimeSeries';

    LSectionNode  := ARootNode.ChildNodes['HydroTimeSeries'];
    LTitleNode    := LSectionNode.ChildNodes['Title'];
    LTitleNode.Text := ATitle;
    LDataListNode := LSectionNode.ChildNodes['TimeSeries'];
    if (ATimeSeries <> nil) then
    begin
      for LYearIndex := 0 to ATimeSeries.YearCount - 1 do
      begin
        LYear := ATimeSeries.Year[LYearIndex];
        LNode := LDataListNode.AddChild('Data');
        LNode.AddChild('Year');
        LNode.ChildNodes['Year'].Text := IntToStr(LYear);
        for LMonthIndex := 1 to 12 do
        begin
          LElementName := 'Month' + Format('%2.2d', [LMonthIndex]);
          LNode.AddChild(LElementName);
          LValue := ATimeSeries.DataByYearMonth[LYear, LMonthIndex];
          LNode.ChildNodes[LElementName].Text := Format('%7.2f', [LValue]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentHydroTimeSeries.XSDText : String;
const OPNAME = 'TXMLAgentHydroTimeSeries.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('HydroTimeSeries.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


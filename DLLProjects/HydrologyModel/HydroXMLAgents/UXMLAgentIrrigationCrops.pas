(******************************************************************************)
(* This unit contains the class TXMLAgentIrrigationCrops.
(******************************************************************************)
unit UXMLAgentIrrigationCrops;


interface

uses
  xmldom, XMLIntf, XMLDoc,
  SysUtils,
  Classes,
  HydrologyCom_TLB,
  UXMLAgent;

type
  TXMLAgentIrrigationCrops = class(TXMLAgent)
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
  UErrorHandlingOperations, StrUtils;

procedure TXMLAgentIrrigationCrops.AddSectionData (AModule    : INetworkModule;
                                                   ASectionNo : Integer;
                                                   ARootNode  : IXMLNode);
const OPNAME = 'TXMLAgentIrrigationCrops.AddSectionData';
var
  LIrrigationModule    : IIrrigationModule;
  LSectionNode         : IXMLNode;
  LDataListNode        : IXMLNode;
  LCropDataNode        : IXMLNode;
  LMonthlyListNode     : IXMLNode;
  LNode                : IXMLNode;
  LIndex               : Integer;
  LIrrigationCrop      : IIrrigationCrop;
  LMonth               : Integer;
begin
  try
    LIrrigationModule := FHydrologyModel.Network.IrrigationModuleAgent.IrrigationModuleByID[AModule.ModuleID];
    if (LIrrigationModule <> nil) then
    begin
      ARootNode.ChildNodes['Section'].Text := 'IrrigationCrops';

      LSectionNode  := ARootNode.ChildNodes['IrrigationCrops'];
      LDataListNode := LSectionNode.AddChild('DataList');
      for LIndex := 0 to LIrrigationModule.NoOfIrrigationCrops - 1 do
      begin
        LIrrigationCrop := LIrrigationModule.IrrigationCropByIndex[LIndex];
        LCropDataNode := LDataListNode.AddChild('CropData');
        LCropDataNode.AddChild('CropNo');
        LCropDataNode.AddChild('CropPercentage');
        LCropDataNode.ChildNodes['CropNo'].Text         := IntToStr(LIrrigationCrop.CropNo);
        LCropDataNode.ChildNodes['CropPercentage'].Text := FloatToStr(LIrrigationCrop.CropPercentage);
        LMonthlyListNode := LCropDataNode.AddChild('MonthlyCropFactors');
        for LMonth := 1 to 12 do
        begin
          LNode := LMonthlyListNode.AddChild('MonthlyData');
          LNode.AddChild('Month');
          LNode.AddChild('CropFactor');
          LNode.ChildNodes['Month'].Text       := IntToStr(LMonth);
          LNode.ChildNodes['CropFactor'].Text  := FloatToStr(LIrrigationCrop.CropFactorByMonth[LMonth]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationCrops.XSDText : String;
const OPNAME = 'TXMLAgentIrrigationCrops.XSDText';
begin
  Result := '';
  try
    Result := GetXSD('IrrigationCrops.xsd');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TXMLAgentIrrigationCrops.DoAdditionalValidation (AContext          : TValidationContext;
                                                          APropertyName     : String;
                                                          AFieldIndex       : String;
                                                          AXSDDoc           : IXMLDocument;
                                                          AXMLDocumentIn    : IXMLDocument;
                                                          AXMLDocumentOut   : IXMLDocument;
                                                          AStopOnFirstError : Boolean;
                                                          AErrorList        : TStringList;
                                                          AErrorMessages    : TStringList) : Boolean;
const OPNAME = 'TXMLAgentIrrigationCrops.DoAdditionalValidation';
var
  LResult           : Boolean;
  LSectionNode      : IXMLNode;
  LDataListNode     : IXMLNode;
  LCropDataNode     : IXMLNode;
  LMonthlyListNode  : IXMLNode;
  LNode             : IXMLNode;
  LIndex            : Integer;
  LRootNode         : IXMLNode;
  LTempResult       : Boolean;
  LXSDSchemaNode    : IXMLNode;
  LErrorMsg         : String;
  LCount            : Integer;
  LMonthIndex       : String;
  LPos              : Integer;
begin
  Result := FALSE;
  try
    LRootNode      := AXMLDocumentOut.GetDocBinding('RootOut', TXMLNode);
    LSectionNode   := LRootNode.ChildNodes['IrrigationCrops'];
    LDataListNode  := LSectionNode.ChildNodes['DataList'];
    LXSDSchemaNode := GetXSDSchemaNode(AXSDDoc);

    LResult := TRUE;
    if (AFieldIndex <> '') then
    begin
      LPos := Pos(',', AFieldIndex);
      if (LPos > 0) then
      begin
        LMonthIndex := Copy(AFieldIndex, LPos+1, Length(AFieldIndex)-LPos);
        AFieldIndex := Copy(AFieldIndex, 1, LPos-1);
      end;
    end;

    // CropNo
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'CropNo') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LCropDataNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult   := ValidateProperty(AContext, 'CropNo', 'CropNo', LCropDataNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('CropNo,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // CropPercentage
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'CropPercentage') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LCropDataNode := LDataListNode.ChildNodes.Get(LIndex);
          LTempResult   := ValidateProperty(AContext, 'CropPercentage', 'CropPercentage', LCropDataNode, LXSDSchemaNode, LErrorMsg);
          if (NOT LTempResult) then
          begin
            AErrorMessages.Add(LErrorMsg);
            AErrorList.Add('CropPercentage,' + IntToStr(LIndex+1));
          end;
          LResult := LResult AND LTempResult;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    // CropFactor
    if ((LResult OR (NOT AStopOnFirstError)) AND ((APropertyName = 'CropFactor') OR (APropertyName = ''))) then
    begin
      LIndex := 0;
      while (LIndex < LDataListNode.ChildNodes.Count) do
      begin
        if ((AFieldIndex = IntToStr(LIndex+1)) OR (AFieldIndex = '')) then
        begin
          LCropDataNode    := LDataListNode.ChildNodes.Get(LIndex);
          LMonthlyListNode := LCropDataNode.ChildNodes['MonthlyCropFactors'];
          LCount := 0;
          while (LCount < LMonthlyListNode.ChildNodes.Count) do
          begin
            if ((LMonthIndex = IntToStr(LCount+1)) OR (LMonthIndex = '')) then
            begin
              LNode       := LMonthlyListNode.ChildNodes.Get(LCount);
              LTempResult := ValidateProperty(AContext, 'CropFactor', 'CropFactor', LNode, LXSDSchemaNode, LErrorMsg);
              if (NOT LTempResult) then
              begin
                AErrorMessages.Add(LErrorMsg);
                AErrorList.Add('CropFactor,' + IntToStr(LIndex+1) + ',' + IntToStr(LCount+1));
              end;
              LResult := LResult AND LTempResult;
            end;
            LCount  := LCount + 1;
          end;
        end;
        LIndex := LIndex + 1;
      end;
    end;

    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


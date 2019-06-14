//
// Contains a class that has a set of call back function pointers.
//
unit URainFallDelphiCallBacks;

interface

type
  TDelphiPrintCallBack = procedure (AMessage: string) of object;
  TDelphiStopCallBack = procedure (AMessage: string) of object;
  TDelphiPassAddressCallBack = procedure (ABlockNumber: integer; AAddress: pointer) of object;
  TRainFallDelphiCallBacksDLL = class(TObject)
  private
    FOnDelphiPrint: TDelphiPrintCallBack;
    FOnDelphiStop: TDelphiStopCallBack;
    FOnDelphiPassAddress: TDelphiPassAddressCallBack;
  public
    procedure PrintCommonBlocks(AContext: string); virtual; abstract;
    property OnDelphiPrint: TDelphiPrintCallBack read FOnDelphiPrint write FOnDelphiPrint;
    property OnDelphiStop: TDelphiStopCallBack read FOnDelphiStop write FOnDelphiStop;
    property OnDelphiPassAddress: TDelphiPassAddressCallBack read FOnDelphiPassAddress write FOnDelphiPassAddress;
  end;
  TRainFallDelphiCallBacks = class(TRainFallDelphiCallBacksDLL)
  public
    procedure PrintCommonBlocks(AContext: string); override;
  end;

implementation

procedure TRainFallDelphiCallBacks.PrintCommonBlocks(AContext: string);
const OPNAME = 'TRainFallDelphiCallBacks.PrintCommonBlocks';
begin
  // Riana - Create a common blocks snap shot class and use it here to print the common blocks. 
end;

end.






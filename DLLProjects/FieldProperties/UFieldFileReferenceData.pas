//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWAF
//
unit UFieldFileReferenceData;

interface

type TFieldFileReferenceItemAddFunction = procedure (
  AFieldName, AFileName, ALineNo: string; AStartCharacter, ALength: integer) of object;

procedure LoadFieldFileReferencesData(AAdd: TFieldFileReferenceItemAddFunction);
procedure LoadPlanningFieldFileReferencesData(AAdd: TFieldFileReferenceItemAddFunction);


implementation

procedure LoadFieldFileReferencesData(AAdd: TFieldFileReferenceItemAddFunction);
const OPNAME = 'LoadFieldFileReferencesData';
begin

  //
  // F01.DAT
  //
  AAdd('Title1',                        'F01.DAT', '1'  ,  -1,  -1);
  AAdd('Title2',                        'F01.DAT', '2'  ,  -1,  -1);
  AAdd('Title3',                        'F01.DAT', '3'  ,  -1,  -1);
  AAdd('NumPeriods',                    'F01.DAT', '4'  ,  -1,   6);
  AAdd('StartYearG',                    'F01.DAT', '4'  ,   7,   6);
  AAdd('StartYearO',                    'F01.DAT', '4'  ,  13,   6);
  AAdd('DebugInit',                     'F01.DAT', '4'  ,  19,   6);
  AAdd('DebugFinal',                    'F01.DAT', '4'  ,  25,   6);
  AAdd('DebugLevel',                    'F01.DAT', '4'  ,  31,   6);
  AAdd('SummaryLevel',                  'F01.DAT', '4'  ,  37,   6);
  AAdd('SummaryOut',                    'F01.DAT', '4'  ,  43,   6);
  AAdd('StoreYield',                    'F01.DAT', '4'  ,  49,   6);
  AAdd('RandomOpt',                     'F01.DAT', '4'  ,  55,   6);
  AAdd('PlotOpt',                       'F01.DAT', '4'  ,  61,   6);
  AAdd('LimitOpt',                      'F01.DAT', '4'  ,  67,   6);
  AAdd('MultPeriodOpt',                 'F01.DAT', '4'  ,  73,   6);
  AAdd('CalcHistoryOpt',                'F01.DAT', '4'  ,  79,   6);
  AAdd('ReduceSeqOpt',                  'F01.DAT', '4'  ,  85,  -1);
  AAdd('Month1',                        'F01.DAT', '5'  ,  -1,   6);
  AAdd('Month2',                        'F01.DAT', '5'  ,   7,   6);
  AAdd('Month3',                        'F01.DAT', '5'  ,  13,   6);
  AAdd('Month4',                        'F01.DAT', '5'  ,  19,   6);
  AAdd('Month5',                        'F01.DAT', '5'  ,  25,   6);
  AAdd('Month6',                        'F01.DAT', '5'  ,  31,   6);
  AAdd('Month7',                        'F01.DAT', '5'  ,  37,   6);
  AAdd('Month8',                        'F01.DAT', '5'  ,  43,   6);
  AAdd('Month9',                        'F01.DAT', '5'  ,  49,   6);
  AAdd('Month10',                       'F01.DAT', '5'  ,  55,   6);
  AAdd('Month11',                       'F01.DAT', '5'  ,  61,   6);
  AAdd('Month12',                       'F01.DAT', '5'  ,  67,   6);
  AAdd('days1',                         'F01.DAT', '6'  ,  -1,   6);
  AAdd('days2',                         'F01.DAT', '6'  ,   7,   6);
  AAdd('days3',                         'F01.DAT', '6'  ,  13,   6);
  AAdd('days4',                         'F01.DAT', '6'  ,  19,   6);
  AAdd('days5',                         'F01.DAT', '6'  ,  25,   6);
  AAdd('days6',                         'F01.DAT', '6'  ,  31,   6);
  AAdd('days7',                         'F01.DAT', '6'  ,  37,   6);
  AAdd('days8',                         'F01.DAT', '6'  ,  43,   6);
  AAdd('days9',                         'F01.DAT', '6'  ,  49,   6);
  AAdd('days10',                        'F01.DAT', '6'  ,  55,   6);
  AAdd('days11',                        'F01.DAT', '6'  ,  61,   6);
  AAdd('days12',                        'F01.DAT', '6'  ,  67,   6);
  AAdd('YearsCount',                    'F01.DAT', '7'  ,  -1,   6);
  AAdd('HydroSeqCount',                 'F01.DAT', '7'  ,   7,   6);
  AAdd('LoadCasesCount',                'F01.DAT', '7'  ,  13,   6);
  AAdd('StartMonthNo',                  'F01.DAT', '7'  ,  19,   6);
  AAdd('RunType',                       'F01.DAT', '7'  ,  25,   6);
  AAdd('StartType',                     'F01.DAT', '7'  ,  31,   6);
  AAdd('ParamFile',                     'F01.DAT', '7'  ,  37,  -1);
  AAdd('Seq1',                          'F01.DAT', '8'  ,  -1,  10);
  AAdd('Seq2',                          'F01.DAT', '8'  ,  11,  10);
  AAdd('Seq3',                          'F01.DAT', '8'  ,  21,  10);
  AAdd('Seq4',                          'F01.DAT', '8'  ,  31,  10);
  AAdd('Seq5',                          'F01.DAT', '8'  ,  41,  10);
  AAdd('Seq6',                          'F01.DAT', '8'  ,  51,  10);
  AAdd('Seq7',                          'F01.DAT', '8'  ,  61,  10);
  AAdd('Seq8',                          'F01.DAT', '8'  ,  71,  10);
  AAdd('Seq9',                          'F01.DAT', '8'  ,  81,  10);
  AAdd('Seq10',                         'F01.DAT', '8'  ,  91,  10);
  AAdd('TYield1',                       'F01.DAT', '9'  ,  -1,   6);
  AAdd('TYield2',                       'F01.DAT', '9'  ,   7,   6);
  AAdd('TYield3',                       'F01.DAT', '9'  ,  13,   6);
  AAdd('TYield4',                       'F01.DAT', '9'  ,  19,   6);
  AAdd('TYield5',                       'F01.DAT', '9'  ,  25,   6);
  AAdd('TYield6',                       'F01.DAT', '9'  ,  31,   6);
  AAdd('TYield7',                       'F01.DAT', '9'  ,  37,   6);
  AAdd('TYield8',                       'F01.DAT', '9'  ,  43,   6);
  AAdd('TYield9',                       'F01.DAT', '9'  ,  49,   6);
  AAdd('TYield10',                      'F01.DAT', '9'  ,  55,   6);
  AAdd('MYield1',                       'F01.DAT', '10' ,  -1,   6);
  AAdd('MYield2',                       'F01.DAT', '10' ,   7,   6);
  AAdd('MYield3',                       'F01.DAT', '10' ,  13,   6);
  AAdd('MYield4',                       'F01.DAT', '10' ,  19,   6);
  AAdd('MYield5',                       'F01.DAT', '10' ,  25,   6);
  AAdd('MYield6',                       'F01.DAT', '10' ,  31,   6);
  AAdd('MYield7',                       'F01.DAT', '10' ,  37,   6);
  AAdd('MYield8',                       'F01.DAT', '10' ,  43,   6);
  AAdd('MYield9',                       'F01.DAT', '10' ,  49,   6);
  AAdd('MYield10',                      'F01.DAT', '10' ,  55,   6);
  AAdd('TPower1',                       'F01.DAT', '11' ,  -1,   6);
  AAdd('TPower2',                       'F01.DAT', '11' ,   7,   6);
  AAdd('TPower3',                       'F01.DAT', '11' ,  13,   6);
  AAdd('TPower4',                       'F01.DAT', '11' ,  19,   6);
  AAdd('TPower5',                       'F01.DAT', '11' ,  25,   6);
  AAdd('TPower6',                       'F01.DAT', '11' ,  31,   6);
  AAdd('TPower7',                       'F01.DAT', '11' ,  37,   6);
  AAdd('TPower8',                       'F01.DAT', '11' ,  43,   6);
  AAdd('TPower9',                       'F01.DAT', '11' ,  49,   6);
  AAdd('TPower10',                      'F01.DAT', '11' ,  55,   6);
  AAdd('TargetRecurrenceInterval',      'F01.DAT', '12' ,  -1,   6);
  
  // F02.DAT
  //
  AAdd('ReservoirCount',                'F02.DAT', '1'  ,  -1,   5);
  AAdd('HydroUnitsCode',                'F02.DAT', '1'  ,   6,   5);
  AAdd('ReservoirName',                 'F02.DAT', '2'  ,  -1,  36);
  AAdd('IncludeSummary',                'F02.DAT', '2'  ,  37,   4);
  AAdd('ReservoirNodeNumber',           'F02.DAT', '2'  ,  41,   5);
  AAdd('Penalty01',                     'F02.DAT', '2'  ,  46,   5);
  AAdd('PointsCount',                   'F02.DAT', '2'  ,  51,   5);
  AAdd('DrainageScale',                 'F02.DAT', '2'  ,  56,  10);
  AAdd('AfforestationScale',            'F02.DAT', '2'  ,  66,  10);
  AAdd('IrrigationScale',               'F02.DAT', '2'  ,  76,  10);
  AAdd('AreaFull',                      'F02.DAT', '3'  ,  -1,  10);
  AAdd('RainCoef',                      'F02.DAT', '3'  ,  11,  10);
  AAdd('CatchmentRef',                  'F02.DAT', '3'  ,  21,  10);
  AAdd('ChannelsCount',                 'F02.DAT', '4'  ,  -1,   5);
  AAdd('PowerChannel',                  'F02.DAT', '4'  ,   6,  -1);
  AAdd('SurfaceElevation',              'F02.DAT', '5'  ,  -1,  -1);
  AAdd('Volume',                        'F02.DAT', '6'  ,  -1,  -1);
  AAdd('Area',                          'F02.DAT', '7'  ,  -1,  -1);
  AAdd('Evaporation',                   'F02.DAT', '8'  ,  -1,  -1);

  // F03.DAT
  //
  AAdd('PenaltyCount',                                   'F03.DAT', '1'  ,  -1, -1);
  AAdd('PenaltyNumber',                                  'F03.DAT', '1a' ,  -1,  5);
  AAdd('ArcCount',                                       'F03.DAT', '1a' ,   6,  5);
  AAdd('Penalty01',                                      'F03.DAT', '1a' ,  11,  5);
  AAdd('Penalty02',                                      'F03.DAT', '1a' ,  16,  5);
  AAdd('Penalty03',                                      'F03.DAT', '1a' ,  21,  5);
  AAdd('Penalty04',                                      'F03.DAT', '1a' ,  26,  5);
  AAdd('Penalty05',                                      'F03.DAT', '1a' ,  31,  5);
  AAdd('MasterControlChannelCount',                      'F03.DAT', '2'  ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '2a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '2a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '2a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '2a' ,  16,  5);
  AAdd('MasterChannelType',                              'F03.DAT', '2a' ,  21,  5);
  AAdd('PowerCount',                                     'F03.DAT', '3'  ,  -1,  5);
  AAdd('PowerGenerationChannelNumber',                   'F03.DAT', '3a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '3a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '3a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '3a' ,  16,  5);
  AAdd('SpillChannelNumber',                             'F03.DAT', '3a' ,  21,  5);
  AAdd('SpillChannelUpstreamNode',                       'F03.DAT', '3a' ,  26,  5);
  AAdd('SpillChannelDownstreamNode',                     'F03.DAT', '3a' ,  31,  5);
  AAdd('SpillPenaltyStructType',                         'F03.DAT', '3a' ,  36,  5);
  AAdd('DownStreamPowerChannelCount',                    'F03.DAT', '3b' ,  -1,  5);
  AAdd('DownStreamPowerChannelNumber',                   'F03.DAT', '3b' ,   5, -1);
  AAdd('IrrigationCount',                                'F03.DAT', '4'  ,  -1,  5);
  AAdd('IrrigationNodeNumber',                           'F03.DAT', '4a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '4a' ,   6,  5);
  AAdd('IrrDiversionChannelNumber',                      'F03.DAT', '4a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '4a' ,  16,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '4a' ,  21,  5);
  AAdd('ReturnChannelNumber',                            'F03.DAT', '4a' ,  26,  5);
  AAdd('ReturnPenaltyStructType',                        'F03.DAT', '4a' ,  31,  5);
  AAdd('ConsumptiveChannelNumber',                       'F03.DAT', '4a' ,  36,  5);
  AAdd('ConsumptivePenaltyStructType',                   'F03.DAT', '4a' ,  41,  5);
  AAdd('RelaxationDemand',                               'F03.DAT', '4a' ,  46,  5);
  AAdd('DiversionChannelCount',                          'F03.DAT', '5'  ,  -1,  5);
  AAdd('DiversionChannelNr',                             'F03.DAT', '5a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '5a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '5a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '5a' ,  16,  5);
  AAdd('DiversionChannelType',                           'F03.DAT', '5a' ,  21,  5);
  AAdd('MinFlowCount',                                   'F03.DAT', '6'  ,  -1,  5);
  AAdd('MinFlowChannelNumber',                           'F03.DAT', '6a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '6a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '6a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '6a' ,  16,  5);
  AAdd('LossCount',                                      'F03.DAT', '7'  ,  -1,  5);
  AAdd('ChannelNumberofLossChannels',                    'F03.DAT', '7a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '7a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '7a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '7a' ,  16,  5);
  AAdd('LossChannelType',                                'F03.DAT', '7a' ,  21,  5);
  AAdd('Reference',                                      'F03.DAT', '7a' ,  26,  5);
  AAdd('MultiPurposeCount',                              'F03.DAT', '8'  ,  -1,  5);
  AAdd('ChannelNumberofMin-MaxChannel',                  'F03.DAT', '8a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '8a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '8a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '8a' ,  16,  5);
  AAdd('PumpingCount',                                   'F03.DAT', '9'  ,  -1,  5);
  AAdd('ChannelNumberofPumpingChannels',                 'F03.DAT', '9a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '9a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '9a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '9a' ,  16,  5);
  AAdd('PumpingHead',                                    'F03.DAT', '9a' ,  21, 10);
  AAdd('PumpEfficiency',                                 'F03.DAT', '9a' ,  31, 10);
  AAdd('InflowCount',                                    'F03.DAT', '10' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '10a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '10a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '10a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '10a',  16,  5);
  AAdd('DemandCount',                                    'F03.DAT', '11' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '11a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '11a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '11a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '11a',  16,  5);
  AAdd('GaugeNumber',                                    'F03.DAT', '11a',  21,  5);
  AAdd('Stochastic',                                     'F03.DAT', '11a',  26,  5);
  AAdd('Fullname',                                       'F03.DAT', '11a',  31, 50);
  AAdd('GeneralCount',                                   'F03.DAT', '12' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '12a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '12a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '12a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '12a',  16,  5);

  AAdd('IrrigationBlockCount',                           'F03.DAT', '13' ,  -1,  5);
  AAdd('IrrigationBlockDiversionChannelUpStreamNode',    'F03.DAT', '13a',  -1,  5);
  AAdd('IrrigationBlockReturnFlowChannelDownStreamNode', 'F03.DAT', '13a',   6,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '13a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '13a',  16,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '13a',  21,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '13a',  26,  5);
  AAdd('IrrigationBlockBlockNumber',                     'F03.DAT', '13a',  31,  5);
  
  AAdd('WetlandCount',                                   'F03.DAT', '14' ,  -1,  5);
  AAdd('WetlandNodeNumber',                              'F03.DAT', '14a',  -1,  5);
  AAdd('WetlandInflowChannelUpstreamNode',               'F03.DAT', '14a',   6,  5);
  AAdd('WetlandOutflowChannelDownstreamNode',            'F03.DAT', '14a',  11,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '14a',  16,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '14a',  21,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '14a',  26,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '14a',  31,  5);

  AAdd('DemandCentreCount',                              'F03.DAT', '15' ,  -1,  5);
  AAdd('YMDemandCentreNodeNumber',                       'F03.DAT', '15a',  -1,  5);
  AAdd('DemandCentreConsumptiveChannelNumber',           'F03.DAT', '15a',   6,  5);
  AAdd('DemandCentreReturnFlowChannelCount',             'F03.DAT', '15a',  11,  5);
  AAdd('ReclamationPlantLossChannelCount',               'F03.DAT', '15a',  16,  5);
  AAdd('ReturnChannelNumber',                            'F03.DAT', '15b',  -1,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '15b',   6,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '15b',  11,  5);
  AAdd('DemandCentreReclaimationChannelNumber',          'F03.DAT', '15c',  -1,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '15c',   6,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '15c',  11,  5);

  AAdd('GroundwaterCount',                               'F03.DAT', '16' ,  -1,  5);
  AAdd('GroundWaterRefNodeNumber',                       'F03.DAT', '16a' , -1,  5);
  AAdd('AquiferNodeNumber',                              'F03.DAT', '16a',   6,  5);
  AAdd('AbstractionNodeNumber',                          'F03.DAT', '16a',  11,  5);
  AAdd('CollectionNodeNumber',                           'F03.DAT', '16a',  16,  5);
  AAdd('BaseFlowNodeNumber',                             'F03.DAT', '16a',  21,  5);
  AAdd('AquiferInflowChannelNr',                         'F03.DAT', '16b',  -1,  5);
  AAdd('AquiferInflowPenaltyType',                       'F03.DAT', '16b',   6,  5);
  AAdd('AquiferExcessInterflowChannelNr',                'F03.DAT', '16c',  -1,  5);
  AAdd('AquiferExcessInterflowPenaltyType',              'F03.DAT', '16c',   6,  5);
  AAdd('GroundWaterBaseflowChannelNr',                   'F03.DAT', '16d',  -1,  5);
  AAdd('GroundWaterBaseflowPenaltyType',                 'F03.DAT', '16d',   6,  5);
  AAdd('AbstractionFromAquiferChannelNr',                'F03.DAT', '16e',  -1,  5);
  AAdd('AbstractionFromAquiferPenaltyType',              'F03.DAT', '16e',   6,  5);
  AAdd('AbstractionFromBaseflowChannelNr',               'F03.DAT', '16f',  -1,  5);
  AAdd('AbstractionFromBaseflowPenaltyType',             'F03.DAT', '16f',   6,  5);
  AAdd('GroundWaterBaseFlowRemainderChannelNr',          'F03.DAT', '16g',  -1,  5);
  AAdd('GroundWaterBaseFlowRemainderPenaltyType',        'F03.DAT', '16g',   6,  5);
  AAdd('SurfaceRunoffAndSoilInterflowChannelNr',         'F03.DAT', '16h',  -1,  5);
  AAdd('SurfaceRunoffAndSoilInterflowPenaltyType',       'F03.DAT', '16h',   6,  5);

  AAdd('SummaryCount',                                   'F03.DAT', '17' ,  -1,  5);
  AAdd('FirmYieldAnalysesCount',                         'F03.DAT', '17' ,   6,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '17a',  -1,  5);
  AAdd('FirmYieldCalc',                                  'F03.DAT', '17a',   6,  5);
  AAdd('ChannelName',                                    'F03.DAT', '17a',  11, 38);

  // F04.DAT
  //                  FirmYieldCalc
  AAdd('ControlStructureCount',               'F04.DAT','1'  ,  -1,   5);
  AAdd('ConstraintsChannelNumber',            'F04.DAT','2'  ,  -1,   5);
  AAdd('UpStreamReservoirNumber',             'F04.DAT','2'  ,   6,   5);
  AAdd('DownStreamReservoirNumber',           'F04.DAT','2'  ,  11,   5);
  AAdd('PointsElevationNumber',               'F04.DAT','2'  ,  16,   5);
  AAdd('SillElevation',                       'F04.DAT','2'  ,  21,  10);
  AAdd('WaterLevelAtDownstreamNode',          'F04.DAT','2'  ,  -1,  -1);
  AAdd('ReferenceElevation',                  'F04.DAT','2'  ,  -1,  -1);
  AAdd('GateHeight',                          'F04.DAT','2'  ,  31,  10);
  AAdd('StructureType',                       'F04.DAT','2'  ,  41,   5);
  AAdd('DischargeCoefficient',                'F04.DAT','2'  ,  45,  10);
  AAdd('ControlStructureLength',              'F04.DAT','2'  ,  55,  10);
  AAdd('ConstraintElevation',                 'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintDischarge',                 'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintChannelNumber',             'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintKFactor',                   'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintHeadDifferences',           'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintAquiferFlows',              'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintDownStreamNodeInflows',     'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintRiverDepths',               'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintElevationDifferences',      'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintMonthlyAverageInflows',     'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintMonthlyAverageDivertedFlow','F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintPumpingHeads',              'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintPumpingDischarges',         'F04.DAT','4'  ,  -1,  -1);

  // F05.DAT
  //
  AAdd('StorageZoneCount',               'F05.DAT','1'  ,  -1,   5);
  AAdd('ZoneRuleCurve',                  'F05.DAT','1'  ,   6,   5);
  AAdd('PenaltyStructureCount',          'F05.DAT','1'  ,  11,   5);
  AAdd('ReservoirZoneName',              'F05.DAT','1a' ,  -1,  10);
  AAdd('StrategyIndicator',              'F05.DAT','1a' ,  11,   5);
  AAdd('BalancingVariable',              'F05.DAT','1a' ,  16,   5);
  AAdd('BalancingPolicy',                'F05.DAT','1a' ,  21,   5);
  AAdd('ZonePenalty',                    'F05.DAT','1a' ,  26,  -1);
  AAdd('NodeNumberStorage',              'F05.DAT','2'  ,  -1,   5);
  AAdd('StatusIndicator',                'F05.DAT','2'  ,   6,   5);
  AAdd('ReservoirPriority',              'F05.DAT','2'  ,  11,  10);
  AAdd('FullSupplyLevel',                'F05.DAT','2'  ,  21,  10);
  AAdd('DeadStorageLevel',               'F05.DAT','2'  ,  31,  10);
  AAdd('BottomOfReservoir',              'F05.DAT','2'  ,  41,  10);
  AAdd('ReservoirNodeNumber',            'F05.DAT','3'  ,  -1,   5);
  AAdd('ReservoirLev',                   'F05.DAT','3'  ,   6,  -1);

  // F06.DAT
  //
  AAdd('ReservoirNodeNumber',            'F06.DAT','1'  ,  -1,   5);
  AAdd('ResInitialLevelsLev',            'F06.DAT','1'  ,   6,  -1);

  // F07.DAT
  //
  AAdd('PowerPlantName',                 'F07.DAT','1'  ,  -1,  36);
  AAdd('PowerChannelNumber',             'F07.DAT','1'  ,  37,   6);
  AAdd('MaxCapGenerator',                'F07.DAT','1'  ,  43,   6);
  AAdd('MaxCapTurbine',                  'F07.DAT','1'  ,  49,   6);
  AAdd('PowerEfficiency',                'F07.DAT','1'  ,  55,   6);
  AAdd('PowerPlantStatus',               'F07.DAT','1'  ,  61,   6);
  AAdd('HeadLoss',                       'F07.DAT','1a' ,  -1,   6);
  AAdd('DesignHead',                     'F07.DAT','1a' ,   7,   6);
  AAdd('MaxNetHead',                     'F07.DAT','1a' ,  13,   6);
  AAdd('MinNetHead',                     'F07.DAT','1a' ,  19,   6);
  AAdd('PowerPointsCount',               'F07.DAT','2'  ,  -1,   6);
  AAdd('EfficiencyFactor',               'F07.DAT','3'  ,  -1,  60);
  AAdd('NetHeadFactors',                 'F07.DAT','4'  ,  -1,  60);
  AAdd('TailWaterCount',                 'F07.DAT','5'  ,  -1,   6);
  AAdd('TailWaterTypeCode',              'F07.DAT','5'  ,   7,   6);
  AAdd('DownStreamLevel',                'F07.DAT','6'  ,  -1,  60);
  AAdd('TailWaterElevation',             'F07.DAT','7'  ,  -1,  60);

  // F08.DAT
  //
  AAdd('PowerPlantName',                 'F08.DAT','1'  ,  -1,  36);
  AAdd('PowerChannelNumber',             'F08.DAT','1'  ,  37,   6);
  AAdd('MinEnergyGenerated',             'F08.DAT','1a' ,  -1,  -1);
  AAdd('MinPowerChannelRelease',         'F08.DAT','1b' ,  -1,  -1);

  // F09.DAT
  //
  AAdd('AreaName',                       'F09.DAT','1'  ,  -1,  36);
  AAdd('NodeNumber',                     'F09.DAT','1'  ,  37,   6);
  AAdd('DiversionFlow',                  'F09.DAT','1a' ,  -1,  -1);
  AAdd('ReturnFlow',                     'F09.DAT','1b' ,  -1,  -1);

  // F10.DAT
  //
  AAdd('DiversionChannelName',           'F10.DAT','1'  ,  -1,  36);
  AAdd('DiversionChannelNumber',         'F10.DAT','1'  ,  37,   6);
  AAdd('DiversionDemand',                'F10.DAT','2'  ,  -1,  -1);
  AAdd('FlowRange',                      'F10.DAT','2'  ,  -1,  -1);
  AAdd('NetNaturalInflow',               'F10.DAT','3'  ,  -1,  -1);
  AAdd('ActualDivertedFlow',             'F10.DAT','3'  ,  -1,  -1);
  AAdd('ControllingResNodeNumber',       'F10.DAT','4'  ,  -1,   6);
  AAdd('ReservoirStorageNumber',         'F10.DAT','4'  ,   7,   6);
  AAdd('ReferenceFlowsCount',            'F10.DAT','4'  ,  13  , 6);
  AAdd('ControllingResLevels',           'F10.DAT','5'  ,  -1  ,-1);
  AAdd('FlowValue',                      'F10.DAT','6'  ,  -1  , 6);
  AAdd('DivertedFlow',                   'F10.DAT','6'  ,   7  ,-1);

  // F11.DAT
  //
  AAdd('MinFlowChannelName',             'F11.DAT','1'  ,  -1,  36);
  AAdd('MinFlowChannelNumber',           'F11.DAT','1'  ,  37,   6);
  AAdd('MinFlowDemand',                  'F11.DAT','1a' ,  -1,  -1);
  AAdd('LossFeatureName',                'F11.DAT','2'  ,  -1 , 36);
  AAdd('LossFeatureChannelNumber',       'F11.DAT','2'  ,  37,   6);
  AAdd('WaterLoss',                      'F11.DAT','2a' ,  -1,  -1);
  AAdd('DivertedFlowP',                  'F11.DAT','2b' ,  -1,  -1);

  // F12.DAT
  //
  AAdd('MinMaxChannelName',              'F12.DAT','1'  ,  -1,  36);
  AAdd('MinMaxChannelNumber',            'F12.DAT','1'  ,  37,   6);
  AAdd('FlowConstraints',                'F12.DAT','2'  ,  -1,  -1);

  // F13.DAT
  //
  AAdd('PowerControlName',               'F13.DAT','1'  ,  -1,  36);
  AAdd('PowerControlNumber',             'F13.DAT','1'  ,  37,   6);
  AAdd('MinEnergyDemand',                'F13.DAT','1a' ,  -1,  -1);
  AAdd('WaterSupplyName',                'F13.DAT','2'  ,  -1,  36);
  AAdd('WaterSupplyNumber',              'F13.DAT','2'  ,  37,   6);
  AAdd('WaterSupplyDistribution',        'F13.DAT','2a' ,  -1,  -1);

  // F14.DAT
  //
  AAdd('IFRMonthlyStructureCount',       'F14.DAT','1'  ,  -1,   2);
  AAdd('IFRInflowOption',                'F14.DAT','1'  ,   3,   3);
  AAdd('IFRChannelNumber',               'F14.DAT','2'  ,  -1,   3);
  AAdd('ReferenceNodeCount',             'F14.DAT','2'  ,   4,   2);
  AAdd('LagInMonthsCount',               'F14.DAT','2'  ,   6,   2);
  AAdd('IFRPointsCount',                 'F14.DAT','2'  ,   8,   5);
  AAdd('RefNodeNumber',                  'F14.DAT','3'  ,  -1,  -1);
  AAdd('IFRVariables',                   'F14.DAT','4'  ,  -1,  -1);
  AAdd('IFRReleaseVariables',            'F14.DAT','4'  ,  -1,  -1);
  AAdd('IFRAnnualStructureCount',        'F14.DAT','5'  ,  -1,  -1);
  AAdd('IFRChannelNumber',               'F14.DAT','6'  ,  -1,   3);
  AAdd('AnnualReferenceNodeCount',       'F14.DAT','6'  ,   4,   2);
  AAdd('AnnualNumberOfClasses',          'F14.DAT','6'  ,   6,   2);
  AAdd('IFRCalcOption',                  'F14.DAT','6'  ,   8,  -1);
  AAdd('RefNodeNumber',                  'F14.DAT','7'  ,  -1,  -1);
  AAdd('IFRReleaseVariables',            'F14.DAT','8'  ,  -1,  -1);

  // F15.DAT
  //
  AAdd('CurtailmentPeriodCount',               'F15.DAT','1'  ,  -1,  -1);
  AAdd('CurtailmentStartMonthNumber',          'F15.DAT','2'  ,  -1,  -1);
  AAdd('CurtailmentChannelCount',              'F15.DAT','3'  ,  -1,  -1);
  AAdd('CurtailmentChannelNumber',             'F15.DAT','4'  ,  -1,   3);
  AAdd('RefStorageVolumeReservoirCount',       'F15.DAT','4'  ,   4,  -1);
  AAdd('DroughtRestrictionCount',              'F15.DAT','5'  ,  -1,  -1);
  AAdd('DroughtRestrictionChannelCount',       'F15.DAT','6'  ,  -1,   2);
  AAdd('DroughtRestrictionReservoirCount',     'F15.DAT','6'  ,   2,   3);
  AAdd('DroughtRestrictionChannelNumber',      'F15.DAT','7'  ,  -1,  -1);
  AAdd('DroughtRestrictionReservoirNumber',    'F15.DAT','8'  ,  -1,  -1);
  AAdd('ReferenceStorageVolumes',              'F15.DAT','9'  ,  -1,  -1);
  AAdd('CurtailmentFactors',                   'F15.DAT','10' ,  -1,  -1);

  // F16.DAT
  //
  AAdd('WaterDemandCategoryCount',             'F16.DAT','1'  ,  -1,  3);
  AAdd('WaterDemandRiskCriteriaCount',         'F16.DAT','1'  ,   4,  3);
  AAdd('WaterDemandScenarioCount',             'F16.DAT','1'  ,   7,  3);
  AAdd('RecurrenceInterval',                   'F16.DAT','2'  ,  -1, -1);
  AAdd('DemandPortion',                        'F16.DAT','3'  ,  -1, -1);
  AAdd('WaterDemandFeatureCount',              'F16.DAT','4'  ,  -1, -1);
  AAdd('WaterDemandChannelNumber',             'F16.DAT','5'  ,  -1,  2);
  AAdd('WaterDemandCategory',                  'F16.DAT','5'  ,   3,  2);
  AAdd('ScenarioPortion',                      'F16.DAT','5'  ,   5, -1);

  // F17.DAT
  //
  AAdd('IrrigationBlockBlockNumber',                     'F17.DAT','1' ,  -1,  3);
  AAdd('IrrigationBlockName',                            'F17.DAT','1' ,   4, 23);
  AAdd('IrrigationBlockMaxWaterAllocation',              'F17.DAT','1' ,  24,  6);
  AAdd('IrrigationBlockFileName',                        'F17.DAT','1' ,  30, 15);
  AAdd('IrrigationBlockNodeNumber',                      'F17.DAT','1' ,  45,  3);
  AAdd('IrrigationBlockDroughtApplicable',               'F17.DAT','1' ,  48, -1);
  AAdd('IrrigationBlockCanalTransportLoss',              'F17.DAT','2' ,  -1,  8);
  AAdd('IrrigationBlockEfficiencyFactor',                'F17.DAT','2' ,   9,  8);
  AAdd('IrrigationBlockReturnFlowFactor',                'F17.DAT','2' ,  17,  8);
  AAdd('IrrigationBlockUpperZoneReturnFlow',             'F17.DAT','2' ,  25,  8);
  AAdd('IrrigationBlockLowerZoneReturnFlow',             'F17.DAT','2' ,  33,  8);
  AAdd('IrrigationBlockReturnFlowLoss',                  'F17.DAT','2' ,  41, -1);
  AAdd('IrrigationBlockUpperZoneSoilMoistureCapacity',   'F17.DAT','3' ,  -1,  8);
  AAdd('IrrigationBlockLowerZoneSoilMoistureCapacity',   'F17.DAT','3' ,   9,  8);
  AAdd('IrrigationBlockUpperZoneSoilMoistureTarget',     'F17.DAT','3' ,  17,  8);
  AAdd('IrrigationBlockInitialSoilMoistureStorage',      'F17.DAT','3' ,  25, -1);
  AAdd('IrrigationBlockRainfallFactor',                  'F17.DAT','4' ,  -1, -1);
  AAdd('IrrigationBlockPanEvaporation',                  'F17.DAT','5' ,  -1, -1);
  AAdd('IrrigationBlockAPanConvFactor',                  'F17.DAT','6' ,  -1, -1);
  AAdd('IrrigationBlockNumberOfCropTypes',               'F17.DAT','7' ,  -1,  2);
  AAdd('IrrigationBlockCropWaterUseType',                'F17.DAT','7' ,   3,  2);
  AAdd('IrrigationBlockWaterUsageFactor',                'F17.DAT','8' ,  -1, 80);
  AAdd('IrrigationBlockPercAreaUnderCropType',           'F17.DAT','8' ,  81, -1);
  AAdd('IrrigationBlockRainAboveRainFactorSpecValue',    'F17.DAT','9' ,  -1,  6);
  AAdd('IrrigationBlockRainBelowRainFactor',             'F17.DAT','9' ,   7,  6);
  AAdd('IrrigationBlockRainCatchmentScalingFactor',      'F17.DAT','9' ,   13, 6);
  AAdd('IrrigationBlockAllocatedIrrigationArea',         'F17.DAT','10',  -1, -1);

  // F18.DAT
  //
  AAdd('WetlandNodeNumber',                              'F18.DAT','1' ,  -1,  5);
  AAdd('WetlandName',                                    'F18.DAT','1' ,  6,  -1);
  AAdd('UpstreamThreshold',                              'F18.DAT','2' , -1,   7);
  AAdd('InflowProportion',                               'F18.DAT','2' ,  8,  -1);
  AAdd('StorageVolume',                                  'F18.DAT','3' , -1,   7);
  AAdd('OutflowProportion',                              'F18.DAT','3' ,  8,  -1);

  // F19.DAT
  //
  AAdd('YMDemandCentreNodeNumber',                       'F19.DAT','1' , -1,  5);
  AAdd('YMDemandCentreName',                             'F19.DAT','1' ,  6, -1);
  AAdd('NodeRefNr',                                      'F19.DAT','2' , -1, -1);
  AAdd('AveReturnFlowFactor',                            'F19.DAT','3' , -1, 10);
  AAdd('AveEvaporation',                                 'F19.DAT','3' , 11,  8);
  AAdd('StdDeviationFactor',                             'F19.DAT','3' , 19, 10);
  AAdd('YMDemandCentreRoutingConstant',                  'F19.DAT','3' , 29,  6);
  AAdd('RainfallScalingFactor',                          'F19.DAT','3' , 35,  8);
  AAdd('ReturnFlowChannelNr',                            'F19.DAT','4' , -1,  5);
  AAdd('TotalReturnFlow',                                'F19.DAT','4' ,  5, 10);
  AAdd('FlowDiversion',                                  'F19.DAT','4' , 15, 10);
  AAdd('TotalFlowLost',                                  'F19.DAT','5' , -1, -1);
  AAdd('EvapoTranspiration',                             'F19.DAT','6' , -1, -1);

  // F20.DAT
  //
  AAdd('NumberOfSFR',                                    'F20.DAT','1' , -1, -1);
  AAdd('InflowNodeNumber',                               'F20.DAT','2' , -1,  2);
  AAdd('CoveredArea',                                    'F20.DAT','2' ,  3, 10);
  AAdd('SFRName',                                        'F20.DAT','2' , 13, -1);
  AAdd('UnitRunoffFileName',                             'F20.DAT','3' , -1, -1);
  AAdd('SoilMoistureFileName',                           'F20.DAT','4' , -1, -1);
 
  // F21.DAT
  //
  AAdd('NrOfMineSubcatchment',                           'F21.DAT','1' , -1, -1);
  AAdd('MineNumber',                                     'F21.DAT','2' , -1,  4);
  AAdd('MineName',                                       'F21.DAT','2' ,  5, -1);
  AAdd('NrOfMineCastPits',                               'F21.DAT','3' , -1,  2);
  AAdd('NrOfUnderGroundMining',                          'F21.DAT','3' ,  3,  2);
  AAdd('NrOfSlurryDump',                                 'F21.DAT','3' ,  5, -1);
  AAdd('RiverChannelNumber',                             'F21.DAT','4' , -1,  4);
  AAdd('PCDChannelNumber',                               'F21.DAT','4' ,  5, -1);
  AAdd('MinePanEvaporationFactors',                      'F21.DAT','5' , -1, -1);
  AAdd('MineLakeEvaporationFactors',                     'F21.DAT','6' , -1, -1);
  AAdd('HydrologyNodeNumber',                            'F21.DAT','7' , -1, -1);
  AAdd('BeneficiationPlantArea',                         'F21.DAT','8' , -1,  5);
  AAdd('BeneficiationRunOffFactor',                      'F21.DAT','8' ,  6, -1);

  AAdd('PitName',                                        'F21.DAT','9' , -1, -1);
  AAdd('CoalReserveArea',                                'F21.DAT','10', -1,  6);
  AAdd('WorkingsArea',                                   'F21.DAT','10',  6,  6);
  AAdd('DisturbedWorkingsArea',                          'F21.DAT','11', -1,  6);
  AAdd('DisturbedArea',                                  'F21.DAT','11',  6,  6);
  AAdd('WaterSurfaceEvapArea',                           'F21.DAT','11', 12,  6);
  AAdd('DisturbedAreaRunOff',                            'F21.DAT','12', -1,  6);
  AAdd('DisturbedWorkingsAreaRunOff',                    'F21.DAT','12',  6,  6);
  AAdd('OpenCastDisturbedAreaRechargeFactors',           'F21.DAT','13', -1, -1);
  AAdd('OpenCastDisturbedWorkingsRechargeFactors',       'F21.DAT','14', -1, -1);
  AAdd('DecantVolume',                                   'F21.DAT','15', -1,  6);
  AAdd('SeepageVolume',                                  'F21.DAT','15',  6,  6);
  AAdd('AnalysisStartVolume',                            'F21.DAT','15', 12,  6);
  AAdd('MaximumSeepageRate',                             'F21.DAT','16', -1,  6);
  AAdd('SeepageExponent',                                'F21.DAT','16',  6,  6);
  AAdd('OpenCastPCDSurfaceArea',                         'F21.DAT','17', -1,  6);
  AAdd('OpenCastPCDStorageCapacity',                     'F21.DAT','17',  6,  6);
  AAdd('OpenCastPCDAnalysisStartVolume',                 'F21.DAT','17', 12,  6);

  AAdd('UndergroundSectionName',                         'F21.DAT','18', -1, 20);
  AAdd('ChannelNumberToUGDam',                           'F21.DAT','18', 20, -1);
  AAdd('UpstreamCatchmentArea',                          'F21.DAT','19', -1,  6);
  AAdd('BoardPillarCatchmentArea',                       'F21.DAT','19',  6,  6);
  AAdd('HighExtractionCatchmentArea',                    'F21.DAT','19', 12,  6);
  AAdd('HighExtractionAreaRunoffFactor',                 'F21.DAT','19', 18,  6);
  AAdd('MineUGUpstreamRunoff',                           'F21.DAT','20', -1, -1);
  AAdd('UGBoardPillarRechargeFactors',                   'F21.DAT','21', -1, -1);
  AAdd('UGHighExtractionRechargeFactors',                'F21.DAT','22', -1, -1);

  AAdd('DumpName',                                       'F21.DAT','23', -1, -1);
  AAdd('DumpSurfaceArea',                                'F21.DAT','24', -1,  6);
  AAdd('RunoffFactorToPCD',                              'F21.DAT','24',  6,  6);
  AAdd('SeepageSplitFactor',                             'F21.DAT','24', 12,  6);
  AAdd('DumpPCDStorageCapacity',                         'F21.DAT','24', 18,  6);
  AAdd('DumpPCDSurfaceArea',                             'F21.DAT','24', 24,  6);
  AAdd('DumpPCDAnalysisStartVolume',                     'F21.DAT','24', 30,  6);
  AAdd('DumpRechargeFactors',                            'F21.DAT','25', -1, -1);
  AAdd('NrOfMineSubcatchment',                           'F21.DAT','26', -1, -1);

  AAdd('CatchmentRefInUse',                              'F21.DAT','27', -1, -1);
  AAdd('GroundwaterFlowVolume',                          'F21.DAT','28', -1, -1);
  AAdd('AntecedentRunoffDecayFactor',                    'F21.DAT','29', -1,  6);
  AAdd('ProportionAntecedentFlow',                       'F21.DAT','29',  6,  6);
  AAdd('GroundwaterFlowVolume',                          'F21.DAT','29', 12,  6);

  // F22.DAT
  //
  AAdd('GroundWaterNodeNumber',                          'F22.DAT','1' , -1, -1);
  AAdd('AquiferStorativity',                             'F22.DAT','2' , -1,  5);
  AAdd('AquiferStaticWaterlevel',                        'F22.DAT','2' ,  6, -1);
  AAdd('UnsaturatedStorageCapacity',                     'F22.DAT','3' , -1,  5);
  AAdd('InitialUnsaturatedStorage',                      'F22.DAT','3' ,  6, -1);
  AAdd('MaximumDischargeRate',                           'F22.DAT','4' , -1,  5);
  AAdd('MovingAverageRecharge',                          'F22.DAT','4' ,  6, -1);
  AAdd('PitmanSoilMoistureCapacity',                     'F22.DAT','5' , -1,  5);
  AAdd('PitmanSoilMoistureStorageCapacity',              'F22.DAT','5' ,  5,  5);
  AAdd('PitmansoilMoistureFlowState',                    'F22.DAT','5' , 10,  5);
  AAdd('PitmanSoilMoistureFlowEquation',                 'F22.DAT','5' , 15,  5);
  AAdd('PitmanMaximumGroundwaterFlow',                   'F22.DAT','5' , 20,  5);
  AAdd('PitmanSoilMoistureRechargeEquation',             'F22.DAT','5' , 25,  5);
  AAdd('PitmanGroundwaterFlow',                          'F22.DAT','5' , 30, -1);
  AAdd('MaximumRateOfGroundwaterBaseFlow',               'F22.DAT','6' , -1,  5);
  AAdd('PowerHeadDifferenceBaseFlowEquation',            'F22.DAT','6' ,  5, -1);
  AAdd('MaximumHydrologicalGradient',                    'F22.DAT','7' , -1, -1);
  AAdd('AquiferTransmissivity',                          'F22.DAT','8' , -1, -1);
  AAdd('BoreHoleDistanceToRiver',                        'F22.DAT','9' , -1, -1);
  AAdd('MaximumGroundwaterAbstraction',                  'F22.DAT','10', -1,  5);
  AAdd('ParameterK2',                                    'F22.DAT','10',  6,  6);
  AAdd('ParameterK3',                                    'F22.DAT','10', 12, -1);
  AAdd('MonthlyWaterEvaporation',                        'F22.DAT','11', -1, -1);
  AAdd('MonthlyWaterUsageFactors',                       'F22.DAT','12', -1, -1);
  AAdd('GroundWaterEvaporationArea',                     'F22.DAT','13', -1, -1);

  // WRYM.DAT
  //
  AAdd('FilePrefix',                     'DIR.DAT','1'  ,  -1,   8);
  AAdd('OutputPath',                     'DIR.DAT','3'  ,  -1,  40);
  AAdd('InputPath',                      'DIR.DAT','2'  ,  -1,  40);


end;

procedure LoadPlanningFieldFileReferencesData(AAdd: TFieldFileReferenceItemAddFunction);
const OPNAME = 'LoadPlanningFieldFileReferencesData';
begin

  //
  // F01.DAT
  //
  AAdd('Title1',                        'F01.DAT', '1'  ,  -1,  -1);
  AAdd('Title2',                        'F01.DAT', '2'  ,  -1,  -1);
  AAdd('Title3',                        'F01.DAT', '3'  ,  -1,  -1);

  AAdd('NumPeriods',                    'F01.DAT', '4'  ,  -1,   6);
  AAdd('StartYearG',                    'F01.DAT', '4'  ,   7,   6);
  AAdd('StartYearO',                    'F01.DAT', '4'  ,  13,   6);

  AAdd('DebugInit',                     'F01.DAT', '4'  ,  19,   6);
  AAdd('DebugFinal',                    'F01.DAT', '4'  ,  25,   6);
  AAdd('DebugLevel',                    'F01.DAT', '4'  ,  31,   6);
  AAdd('SummaryLevel',                  'F01.DAT', '4'  ,  37,   6);
  AAdd('SummaryOutput',                 'F01.DAT', '4'  ,  43,   6);


  AAdd('SupplyOption',                  'F01.DAT', '4'  ,  49,   6);
  AAdd('AnnualSummary',                 'F01.DAT', '4'  ,  61,   12);
  AAdd('EconomicOption',                'F01.DAT', '4'  ,  73,   1);
  AAdd('PlanningSummary',               'F01.DAT', '4'  ,  79,   6);

  AAdd('InputSummary',                  'F01.DAT', '4'  ,  85,   6);
  AAdd('WaterQualityOption',            'F01.DAT', '4'  ,  91,   6);

  AAdd('Month1',                        'F01.DAT', '5'  ,  -1,   6);
  AAdd('Month2',                        'F01.DAT', '5'  ,   7,   6);
  AAdd('Month3',                        'F01.DAT', '5'  ,  13,   6);
  AAdd('Month4',                        'F01.DAT', '5'  ,  19,   6);
  AAdd('Month5',                        'F01.DAT', '5'  ,  25,   6);
  AAdd('Month6',                        'F01.DAT', '5'  ,  31,   6);
  AAdd('Month7',                        'F01.DAT', '5'  ,  37,   6);
  AAdd('Month8',                        'F01.DAT', '5'  ,  43,   6);
  AAdd('Month9',                        'F01.DAT', '5'  ,  49,   6);
  AAdd('Month10',                       'F01.DAT', '5'  ,  55,   6);
  AAdd('Month11',                       'F01.DAT', '5'  ,  61,   6);
  AAdd('Month12',                       'F01.DAT', '5'  ,  67,   6);

  AAdd('days1',                         'F01.DAT', '6'  ,  -1,   6);
  AAdd('days2',                         'F01.DAT', '6'  ,   7,   6);
  AAdd('days3',                         'F01.DAT', '6'  ,  13,   6);
  AAdd('days4',                         'F01.DAT', '6'  ,  19,   6);
  AAdd('days5',                         'F01.DAT', '6'  ,  25,   6);
  AAdd('days6',                         'F01.DAT', '6'  ,  31,   6);
  AAdd('days7',                         'F01.DAT', '6'  ,  37,   6);
  AAdd('days8',                         'F01.DAT', '6'  ,  43,   6);
  AAdd('days9',                         'F01.DAT', '6'  ,  49,   6);
  AAdd('days10',                        'F01.DAT', '6'  ,  55,   6);
  AAdd('days11',                        'F01.DAT', '6'  ,  61,   6);
  AAdd('days12',                        'F01.DAT', '6'  ,  67,   6);

  AAdd('YearsCount',                     'F01.DAT', '7'  ,  -1,   6);

  AAdd('PeriodsPerYear',                 'F01.DAT', '7'  ,   7,   6);
  AAdd('StartYearParam',                 'F01.DAT', '7'  ,  13,   6);
  AAdd('PlanningHydroSeqCount',          'F01.DAT', '7'  ,  19,   6);
  AAdd('NoOfDemandCentre',               'F01.DAT', '7'  ,  25,   6);
  AAdd('NoOfInterBasinSupportChannels',  'F01.DAT', '7'  ,  31,   6);
  AAdd('StartMonthNo',                   'F01.DAT', '7'  ,  37,   6);
  AAdd('CalendarStartMonth' ,            'F01.DAT', '7'  ,  43,   6);
  AAdd('RunType',                        'F01.DAT', '7'  ,  49,   6);
  AAdd('StartType',                      'F01.DAT', '7'  ,  55,   6);
  AAdd('ShortTermPlanningOption' ,       'F01.DAT', '7'  ,  61,   6);
  AAdd('PlotOpt',                        'F01.DAT', '7'  ,  67,   6);
  AAdd('RandomOpt',                      'F01.DAT', '7'  ,  73,   6);
  AAdd('HydroPowerOption',               'F01.DAT', '7'  ,  79,   6);
  AAdd('AllocationControlOption',        'F01.DAT', '7'  ,  85,   6);

  AAdd('NrOfDecisionDates',              'F01.DAT', '8'  ,  -1,  6);
  AAdd('DecisionMonth',                  'F01.DAT', '8'  ,  7,  80);

  AAdd('DecisionType',                    'F01.DAT', '9'  ,  -1,  10);
  AAdd('HydroPowerIndicator',             'F01.DAT', '9'  ,  11,  80);

  AAdd('ParamFile',                       'F01.DAT', '10'  ,  -1,  40);

  AAdd('DemandCentreType',                'F01.DAT', '12'  ,  -1,  3);

  AAdd('DemandCentreID',                  'F01.DAT', '12'  ,  4,  4);
  AAdd('AnnualDemand',                    'F01.DAT', '12'  ,  8,  7);
  AAdd('MinimumDemand',                   'F01.DAT', '12'  ,  15,  7);
  AAdd('IncludeInOutput',                 'F01.DAT', '12'  ,  22,  7);


  //AAdd('IncludeInOutput',                 'F01.DAT', '13'  ,  22,  7);
  //AAdd('IncludeInOutput',                 'F01.DAT', '14'  ,  22,  7);

  AAdd('Seq',                             'F01.DAT', '15'  ,  22,  7);


  // F02.DAT
  //



  AAdd('ReservoirCount',                'F02.DAT', '1'  ,  -1,   5);
  AAdd('HydroUnitsCode',                'F02.DAT', '1'  ,   6,   5);
  AAdd('ReservoirName',                 'F02.DAT', '2'  ,  -1,  36);
  AAdd('IncludeSummary',                'F02.DAT', '2'  ,  37,   4);
  AAdd('ReservoirNodeNumber',           'F02.DAT', '2'  ,  41,   5);
  AAdd('Penalty01',                     'F02.DAT', '2'  ,  46,   5);
  AAdd('PointsCount',                   'F02.DAT', '2'  ,  51,   5);
  AAdd('DrainageScale',                 'F02.DAT', '2'  ,  56,  10);
  AAdd('UrbanRunOff',                   'F02.DAT', '2'  ,  66,  10);
  AAdd('AfforestationScale',            'F02.DAT', '2'  ,  76,  10);
  AAdd('IrrigationScale',               'F02.DAT', '2'  ,  86,  10);

  AAdd('AreaFull',                      'F02.DAT', '3'  ,  -1,  10);
  AAdd('RainCoef',                      'F02.DAT', '3'  ,  11,  10);
  AAdd('CatchmentRef',                  'F02.DAT', '3'  ,  21,  10);
  AAdd('NaturalInflowChannel',          'F02.DAT', '3'  ,  31,  10);

  AAdd('ChannelsCount',                 'F02.DAT', '4'  ,  -1,   5);
  AAdd('PowerChannel',                  'F02.DAT', '4'  ,  15,  -1);

  AAdd('SurfaceElevation',              'F02.DAT', '5'  ,  -1,  -1);
  AAdd('Volume',                        'F02.DAT', '6'  ,  -1,  -1);
  AAdd('Area',                          'F02.DAT', '7'  ,  -1,  -1);
  AAdd('Evaporation',                   'F02.DAT', '8'  ,  -1,  -1);



  // F03.DAT
  //

  AAdd('PenaltyCountP',                                  'F03.DAT', '1'  ,  -1, 6);
  AAdd('InflowPenaltyNo',                                'F03.DAT', '1'  ,  7, 6);

  AAdd('ChannelPenaltyStructType',                       'F03.DAT', '1a'  , -1, 10);
  AAdd('ArcCount',                                       'F03.DAT', '1a' ,  11,  5);
  AAdd('Penalty',                                        'F03.DAT', '1a' ,  16,  25);

  AAdd('MasterControlChannelCount',                      'F03.DAT', '2'  ,  -1,  5);
  AAdd('MasterControlChannelNumber',                     'F03.DAT', '2a'  , -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '2a' ,  6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '2a' ,  11,  5);
  AAdd('ChannelPenaltyStructType',                       'F03.DAT', '2a' ,  16,  5);
  AAdd('MasterChannelType',                              'F03.DAT', '2a' ,  21,  5);

  AAdd('PowerCount',                                     'F03.DAT', '3'  ,  -1,  5);
  AAdd('PowerGenerationChannelNumber',                   'F03.DAT', '3a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '3a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '3a' ,  11,  5);
  AAdd('ChannelPenaltyStructType',                       'F03.DAT', '3a' ,  16,  5);
  AAdd('SpillChannelNumber',                             'F03.DAT', '3a' ,  21,  5);
  AAdd('SpillChannelDownstreamNode',                     'F03.DAT', '3a' ,  26,  5);
  AAdd('SpillChannelUpstreamNode',                       'F03.DAT', '3a' ,  31,  5);
  AAdd('SpillPenaltyStructType',                         'F03.DAT', '3a' ,  36,  5);

  AAdd('DownStreamPowerChannelCount',                    'F03.DAT', '3b' ,  -1,  5);
  AAdd('DownStreamPowerChannelNumber',                   'F03.DAT', '3b' ,   6, -1);

  AAdd('IrrigationCount',                                'F03.DAT', '4'  ,  -1,  5);
  AAdd('IrrigationNodeNumber',                           'F03.DAT', '4a' ,  -1,  5);
  AAdd('DiversionChannelUpStreamNode',                   'F03.DAT', '4a' ,   6,  5);

  AAdd('IrrDiversionChannelNumber',                      'F03.DAT', '4a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '4a' ,  16,  5);
  AAdd('ReturnFlowChannelDownStreamNode',                'F03.DAT', '4a' ,  21,  5);
  AAdd('ReturnChannelNumber',                            'F03.DAT', '4a' ,  26,  5);
  AAdd('ReturnPenaltyStructType',                        'F03.DAT', '4a' ,  31,  5);
  AAdd('ConsumptiveChannelNumber',                       'F03.DAT', '4a' ,  36,  5);
  AAdd('ConsumptivePenaltyStructType',                   'F03.DAT', '4a' ,  41,  5);
  AAdd('RelaxationDemand',                               'F03.DAT', '4a' ,  46,  5);

  AAdd('DiversionChannelCount',                          'F03.DAT', '5'  ,  -1,  5);
  AAdd('DiversionChannelNr',                             'F03.DAT', '5a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '5a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '5a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '5a' ,  16,  5);
  AAdd('DiversionChannelType',                           'F03.DAT', '5a' ,  21,  5);

  AAdd('MinFlowCount',                                   'F03.DAT', '6'  ,  -1,  5);
  AAdd('MinFlowChannelNumber',                           'F03.DAT', '6a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '6a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '6a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '6a' ,  16,  5);

  AAdd('PLossCount',                                     'F03.DAT', '7'  ,  -1,  5);
  AAdd('ChannelNumberofLossChannels',                    'F03.DAT', '7a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '7a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '7a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '7a' ,  16,  5);
  AAdd('LossChannelType',                                'F03.DAT', '7a' ,  21,  5);
  AAdd('Reference',                                      'F03.DAT', '7a' ,  26,  5);

  AAdd('MultiPurposeCount',                              'F03.DAT', '8'  ,  -1,  5);
  AAdd('ChannelNumberofMin-MaxChannel',                  'F03.DAT', '8a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '8a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '8a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '8a' ,  16,  5);

  AAdd('PumpingCount',                                   'F03.DAT', '9'  ,  -1,  5);
  AAdd('ChannelNumberofPumpingChannels',                 'F03.DAT', '9a' ,  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '9a' ,   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '9a' ,  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '9a' ,  16,  5);
  AAdd('PumpingHead',                                    'F03.DAT', '9a' ,  21, 10);
  AAdd('PumpEfficiency',                                 'F03.DAT', '9a' ,  31, 10);

  AAdd('InflowCount',                                    'F03.DAT', '10' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '10a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '10a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '10a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '10a',  16,  5);

  AAdd('DemandCount',                                    'F03.DAT', '11' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '11a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '11a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '11a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '11a',  16,  5);
  AAdd('GaugeNumber',                                    'F03.DAT', '11a',  21,  5);
  AAdd('Stochastic',                                     'F03.DAT', '11a',  26,  5);
  AAdd('Fullname',                                       'F03.DAT', '11a',  31, 50);

  AAdd('GeneralCount',                                   'F03.DAT', '12' ,  -1,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '12a',  -1,  5);
  AAdd('UpNodeNumber',                                   'F03.DAT', '12a',   6,  5);
  AAdd('DownNodeNumber',                                 'F03.DAT', '12a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '12a',  16,  5);

  AAdd('IrrigationBlockCount',                           'F03.DAT', '13' ,  -1,  5);
  AAdd('IrrigationBlockDiversionChannelUpStreamNode',    'F03.DAT', '13a',  -1,  5);
  AAdd('IrrigationBlockReturnFlowChannelDownStreamNode', 'F03.DAT', '13a',   6,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '13a',  11,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '13a',  16,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '13a',  21,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '13a',  26,  5);
  AAdd('IrrigationBlockBlockNumber',                     'F03.DAT', '13a',  31,  5);

  AAdd('WetlandCount',                                   'F03.DAT', '14' ,  -1,  5);
  AAdd('WetlandNodeNumber',                              'F03.DAT', '14a',  -1,  5);
  AAdd('WetlandInflowChannelUpstreamNode',               'F03.DAT', '14a',   6,  5);
  AAdd('WetlandOutflowChannelDownstreamNode',            'F03.DAT', '14a',  11,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '14a',  16,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '14a',  21,  5);
  AAdd('ChannelNumber',                                  'F03.DAT', '14a',  26,  5);
  AAdd('PenaltyNumber',                                  'F03.DAT', '14a',  31,  5);

  AAdd('SummaryCountp',                                  'F03.DAT', '15' ,  -1,  5);
  AAdd('FirmYieldAnalysesCountp',                        'F03.DAT', '15' ,   6,  5);

  AAdd('OutputChannelNumber',                            'F03.DAT', '15a',  -1,  5);
  AAdd('FirmYieldCalc',                                  'F03.DAT', '15a',   6,  5);
  AAdd('ChannelName',                                    'F03.DAT', '15a',  11, 38);


  // F04.DAT
  //                  FirmYieldCalc

  AAdd('ControlStructureCount',               'F04.DAT','1'  ,  -1,   5);
  AAdd('ConstraintsChannelNumber',            'F04.DAT','2'  ,  -1,   5);
  AAdd('UpStreamReservoirNumber',             'F04.DAT','2'  ,   6,   5);

  AAdd('DownStreamReservoirNumber',           'F04.DAT','2'  ,  11,   5);
  AAdd('PointsElevationNumber',               'F04.DAT','2'  ,  16,   5);
  AAdd('SillElevation',                       'F04.DAT','2'  ,  21,  10);
  AAdd('WaterLevelAtDownstreamNode',          'F04.DAT','2'  ,  -1,  -1);
  AAdd('ReferenceElevation',                  'F04.DAT','2'  ,  -1,  -1);
  AAdd('GateHeight',                          'F04.DAT','2'  ,  31,  10);
  AAdd('StructureType',                       'F04.DAT','2'  ,  41,   5);
  AAdd('DischargeCoefficient',                'F04.DAT','2'  ,  45,  10);
  AAdd('ControlStructureLength',              'F04.DAT','2'  ,  55,  10);
  AAdd('ConstraintElevation',                 'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintDischarge',                 'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintChannelNumber',             'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintKFactor',                   'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintHeadDifferences',           'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintAquiferFlows',              'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintDownStreamNodeInflows',     'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintRiverDepths',               'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintElevationDifferences',      'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintMonthlyAverageInflows',     'F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintMonthlyAverageDivertedFlow','F04.DAT','4'  ,  -1,  -1);
  AAdd('ConstraintPumpingHeads',              'F04.DAT','3'  ,  -1,  -1);
  AAdd('ConstraintPumpingDischarges',         'F04.DAT','4'  ,  -1,  -1);


  // F05.DAT
  //
  AAdd('StorageZoneCount',               'F05.DAT','1'  ,  -1,   5);
  AAdd('ZoneRuleCurve',                  'F05.DAT','1'  ,   6,   5);
  AAdd('PenaltyStructureCount',          'F05.DAT','1'  ,  11,   5);

  AAdd('ReservoirZoneName',              'F05.DAT','1a' ,  -1,  10);
  AAdd('StrategyIndicator',              'F05.DAT','1a' ,  11,   5);

  AAdd('BalancingVariable',              'F05.DAT','1a' ,  16,   5);
  AAdd('BalancingPolicy',                'F05.DAT','1a' ,  21,   5);
  AAdd('ZonePenalty',                    'F05.DAT','1a' ,  26,  -1);

  AAdd('NodeNumberStorage',              'F05.DAT','2'  ,  -1,   5);
  AAdd('StatusIndicator',                'F05.DAT','2'  ,   6,   5);
  AAdd('ReservoirVolumePlottingOption',  'F05.DAT','2'  ,  11,   5);
  AAdd('ReservoirPriority',              'F05.DAT','2'  ,  16,  10);
  AAdd('FullSupplyLevel',                'F05.DAT','2'  ,  26,  10);
  AAdd('DeadStorageLevel',               'F05.DAT','2'  ,  36,  10);
  AAdd('BottomOfReservoir',              'F05.DAT','2'  ,  46,  10);
  AAdd('FullSupplyAllocation',           'F05.DAT','2'  ,  56,  10);

  AAdd('ReservoirNodeNumber',            'F05.DAT','3'  ,  -1,   5);
  AAdd('ReservoirLev',                   'F05.DAT','3'  ,   6,  -1);

  // F06.DAT
  //
  AAdd('ReservoirNodeNumber',            'F06.DAT','1'  ,  -1,   5);
  AAdd('ResInitialLevelsLev',            'F06.DAT','1'  ,   6,  -1);


  // F07.DAT
  //
  AAdd('PowerPlantName',                 'F07.DAT','1'  ,  -1,  36);
  AAdd('PowerChannelNumber',             'F07.DAT','1'  ,  37,   6);
  AAdd('MaxCapGenerator',                'F07.DAT','1'  ,  43,   6);
  AAdd('MaxCapTurbine',                  'F07.DAT','1'  ,  49,   6);
  AAdd('PowerEfficiency',                'F07.DAT','1'  ,  55,   6);
  AAdd('PowerPlantStatus',               'F07.DAT','1'  ,  61,   6);

  AAdd('HeadLossP',                      'F07.DAT','1a' ,  -1,   6);
  AAdd('DesignHead',                     'F07.DAT','1a' ,   7,   6);
  AAdd('MaxNetHead',                     'F07.DAT','1a' ,  13,   6);
  AAdd('MinNetHead',                     'F07.DAT','1a' ,  19,   6);
  AAdd('LevelInReservoirHydroStops',     'F07.DAT','1a' ,  25,   7);
  AAdd('PowerPointsCount',               'F07.DAT','2'  ,  -1,   6);
  AAdd('EfficiencyFactor',               'F07.DAT','3'  ,  -1,  60);
  AAdd('NetHeadFactors',                 'F07.DAT','4'  ,  -1,  60);
  AAdd('TailWaterCount',                 'F07.DAT','5'  ,  -1,   6);
  AAdd('TailWaterTypeCode',              'F07.DAT','5'  ,   7,   6);

  AAdd('DownStreamLevel',                'F07.DAT','6'  ,  -1,  60);
  AAdd('TailWaterElevation',             'F07.DAT','7'  ,  -1,  60);


  // F08.DAT
  //
  AAdd('PowerPlantName',                 'F08.DAT','1'  ,  -1,  36);
  AAdd('PowerChannelNumber',             'F08.DAT','1'  ,  37,   6);
  AAdd('MinEnergyGenerated',             'F08.DAT','1a' ,  -1,  -1);
  AAdd('MinPowerChannelRelease',         'F08.DAT','1b' ,  -1,  -1);

  // F09.DAT
  //
  AAdd('AreaName',                       'F09.DAT','1'  ,  -1,  36);
  AAdd('NodeNumber',                     'F09.DAT','1'  ,  37,   6);
  AAdd('DiversionFlow',                  'F09.DAT','1a' ,  -1,  -1);
  AAdd('ReturnFlow',                     'F09.DAT','1b' ,  -1,  -1);

  // F10.DAT
  //
  AAdd('DiversionChannelName',           'F10.DAT','1'  ,  -1,  36);
  AAdd('DiversionChannelNumber',         'F10.DAT','1'  ,  37,   6);
  AAdd('DiversionDemand',                'F10.DAT','2'  ,  -1,  -1);
  AAdd('FlowRange',                      'F10.DAT','2'  ,  -1,  -1);
  AAdd('NetNaturalInflow',               'F10.DAT','3'  ,  -1,  -1);
  AAdd('ActualDivertedFlow',             'F10.DAT','3'  ,  -1,  -1);
  AAdd('ControllingResNodeNumber',       'F10.DAT','4'  ,  -1,   6);
  AAdd('ReservoirStorageNumber',         'F10.DAT','4'  ,   7,   6);
  AAdd('ReferenceFlowsCount',            'F10.DAT','4'  ,  13  , 6);
  AAdd('ControllingResLevels',           'F10.DAT','5'  ,  -1  ,-1);
  AAdd('FlowValue',                      'F10.DAT','6'  ,  -1  , 6);
  AAdd('DivertedFlow',                   'F10.DAT','6'  ,   7  ,-1);

  // F11.DAT
  //
  AAdd('MinFlowChannelName',             'F11.DAT','1'  ,  -1,  36);
  AAdd('MinFlowChannelNumber',           'F11.DAT','1'  ,  37,   6);
  AAdd('MinFlowDemand',                  'F11.DAT','1a' ,  -1,  -1);
  AAdd('LossFeatureName',                'F11.DAT','2'  ,  -1 , 36);
  AAdd('LossFeatureChannelNumber',       'F11.DAT','2'  ,  37,   6);
  AAdd('WaterLoss',                      'F11.DAT','2a' ,  -1,  -1);
  AAdd('DivertedFlowP',                  'F11.DAT','2b' ,  -1,  -1);

  // F12.DAT
  //
  AAdd('MinMaxChannelName',              'F12.DAT','1'  ,  -1,  36);
  AAdd('MinMaxChannelNumber',            'F12.DAT','1'  ,  37,   6);
  AAdd('FlowConstraints',                'F12.DAT','2'  ,  -1,  -1);

  // F13.DAT
  //
  AAdd('MasterControlName',              'F13.DAT','1'  ,  -1,  36);
  AAdd('MasterControlNumber',            'F13.DAT','1'  ,  37,   6);
  AAdd('MinEnergyDemand',                'F13.DAT','1a' ,  -1,  -1);
  AAdd('WaterSupplyName',                'F13.DAT','2'  ,  -1,  36);
  AAdd('WaterSupplyNumber',              'F13.DAT','2'  ,  37,   6);
  AAdd('WaterSupplyDistribution',        'F13.DAT','2a' ,  -1,  -1);

  // F14.DAT
  //
  AAdd('IFRMonthlyStructureCount',       'F14.DAT','1'  ,  -1,   2);
  AAdd('IFRInflowOption',                'F14.DAT','1'  ,   3,   3);
  AAdd('IFRChannelNumber',               'F14.DAT','2'  ,  -1,   3);
  AAdd('ReferenceNodeCount',             'F14.DAT','2'  ,   4,   2);
  AAdd('LagInMonthsCount',               'F14.DAT','2'  ,   6,   2);
  AAdd('IFRPointsCount',                 'F14.DAT','2'  ,   8,   5);
  AAdd('RefNodeNumber',                  'F14.DAT','3'  ,  -1,  -1);
  AAdd('IFRVariables',                   'F14.DAT','4'  ,  -1,  -1);
  AAdd('IFRReleaseVariables',            'F14.DAT','4'  ,  -1,  -1);

  AAdd('IFRAnnualStructureCount',        'F14.DAT','5'  ,  -1,  -1);
  AAdd('IFRChannelNumber',               'F14.DAT','6'  ,  -1,   3);
  AAdd('AnnualReferenceNodeCount',       'F14.DAT','6'  ,   4,   2);
  AAdd('AnnualNumberOfClasses',          'F14.DAT','6'  ,   6,   2);
  AAdd('IFRCalcOption',                  'F14.DAT','6'  ,   8,  -1);

  AAdd('RefNodeNumber',                  'F14.DAT','7'  ,  -1,  -1);
  AAdd('IFRReleaseVariables',            'F14.DAT','8'  ,  -1,  -1);

  // F15.DAT
  //
  AAdd('CurtailmentPeriodCount',               'F15.DAT','1'  ,  -1,  -1);
  AAdd('CurtailmentStartMonthNumber',          'F15.DAT','2'  ,  -1,  -1);
  AAdd('CurtailmentChannelCount',              'F15.DAT','3'  ,  -1,  -1);
  AAdd('CurtailmentChannelNumber',             'F15.DAT','4'  ,  -1,   3);
  AAdd('RefStorageVolumeReservoirCount',       'F15.DAT','4'  ,   4,  -1);
  AAdd('DroughtRestrictionCount',              'F15.DAT','5'  ,  -1,  -1);
  AAdd('DroughtRestrictionChannelCount',       'F15.DAT','6'  ,  -1,   2);
  AAdd('DroughtRestrictionReservoirCount',     'F15.DAT','6'  ,   2,   3);
  AAdd('DroughtRestrictionChannelNumber',      'F15.DAT','7'  ,  -1,  -1);
  AAdd('DroughtRestrictionReservoirNumber',    'F15.DAT','8'  ,  -1,  -1);
  AAdd('ReferenceStorageVolumes',              'F15.DAT','9'  ,  -1,  -1);
  AAdd('CurtailmentFactors',                   'F15.DAT','10' ,  -1,  -1);

  // F16.DAT
  //
  AAdd('WaterDemandCategoryCount',             'F16.DAT','1'  ,  -1,  3);
  AAdd('WaterDemandRiskCriteriaCount',         'F16.DAT','1'  ,   4,  3);
  AAdd('WaterDemandScenarioCount',             'F16.DAT','1'  ,   7,  3);
  AAdd('RecurrenceInterval',                   'F16.DAT','2'  ,  -1, -1);
  AAdd('DemandPortion',                        'F16.DAT','3'  ,  -1, -1);
  AAdd('WaterDemandFeatureCount',              'F16.DAT','4'  ,  -1, -1);
  AAdd('WaterDemandChannelNumber',             'F16.DAT','5'  ,  -1,  2);
  AAdd('WaterDemandCategory',                  'F16.DAT','5'  ,   3,  2);
  AAdd('ScenarioPortion',                      'F16.DAT','5'  ,   5, -1);

  // F17.DAT
  //
  AAdd('IrrigationBlockBlockNumber',                     'F17.DAT','1' ,  -1,  3);
  AAdd('IrrigationBlockName',                            'F17.DAT','1' ,   4, 23);
  AAdd('IrrigationBlockMaxWaterAllocation',              'F17.DAT','1' ,  24,  6);
  AAdd('IrrigationBlockFileName',                        'F17.DAT','1' ,  30, 15);
  AAdd('IrrigationBlockNodeNumber',                      'F17.DAT','1' ,  45,  3);
  AAdd('IrrigationBlockDroughtApplicable',               'F17.DAT','1' ,  48, -1);
  AAdd('IrrigationBlockCanalTransportLoss',              'F17.DAT','2' ,  -1,  8);
  AAdd('IrrigationBlockEfficiencyFactor',                'F17.DAT','2' ,   9,  8);
  AAdd('IrrigationBlockReturnFlowFactor',                'F17.DAT','2' ,  17,  8);
  AAdd('IrrigationBlockUpperZoneReturnFlow',             'F17.DAT','2' ,  25,  8);
  AAdd('IrrigationBlockLowerZoneReturnFlow',             'F17.DAT','2' ,  33,  8);
  AAdd('IrrigationBlockReturnFlowLoss',                  'F17.DAT','2' ,  41, -1);
  AAdd('IrrigationBlockUpperZoneSoilMoistureCapacity',   'F17.DAT','3' ,  -1,  8);
  AAdd('IrrigationBlockLowerZoneSoilMoistureCapacity',   'F17.DAT','3' ,   9,  8);
  AAdd('IrrigationBlockUpperZoneSoilMoistureTarget',     'F17.DAT','3' ,  17,  8);
  AAdd('IrrigationBlockInitialSoilMoistureStorage',      'F17.DAT','3' ,  25, -1);
  AAdd('IrrigationBlockRainfallFactor',                  'F17.DAT','4' ,  -1, -1);
  AAdd('IrrigationBlockPanEvaporation',                  'F17.DAT','5' ,  -1, -1);
  AAdd('IrrigationBlockAPanConvFactor',                  'F17.DAT','6' ,  -1, -1);
  AAdd('IrrigationBlockNumberOfCropTypes',               'F17.DAT','7' ,  -1,  2);
  AAdd('IrrigationBlockCropWaterUseType',                'F17.DAT','7' ,   3,  2);
  AAdd('IrrigationBlockWaterUsageFactor',                'F17.DAT','8' ,  -1, 80);
  AAdd('IrrigationBlockPercAreaUnderCropType',           'F17.DAT','8' ,  81, -1);
  AAdd('IrrigationBlockRainAboveRainFactorSpecValue',    'F17.DAT','9' ,  -1,  6);
  AAdd('IrrigationBlockRainBelowRainFactor',             'F17.DAT','9' ,   7,  6);
  AAdd('IrrigationBlockRainCatchmentScalingFactor',      'F17.DAT','9' ,   13, 6);
  AAdd('IrrigationBlockAllocatedIrrigationArea',         'F17.DAT','10',  -1, -1);

  // F18.DAT
  //
  AAdd('WetlandNodeNumber',                              'F18.DAT','1' ,  -1,  5);
  AAdd('WetlandName',                                    'F18.DAT','1' ,  6,  -1);
  AAdd('UpstreamThreshold',                              'F18.DAT','2' , -1,   7);
  AAdd('InflowProportion',                               'F18.DAT','2' ,  8,  -1);
  AAdd('StorageVolume',                                  'F18.DAT','3' , -1,   7);
  AAdd('OutflowProportion',                              'F18.DAT','3' ,  8,  -1);

  // F20.DAT
  //
  AAdd('NumberOfSFR',                                    'F20.DAT','1' , -1, -1);
  AAdd('InflowNodeNumber',                               'F20.DAT','2' , -1,  2);
  AAdd('CoveredArea',                                    'F20.DAT','2' ,  3, 10);
  AAdd('SFRName',                                        'F20.DAT','2' , 13, -1);
  AAdd('UnitRunoffFileName',                             'F20.DAT','3' , -1, -1);
  AAdd('SoilMoistureFileName',                           'F20.DAT','4' , -1, -1);


  // F19.DAT
  //
  AAdd('YMDemandCentreNodeNumber',                       'F19.DAT','1' , -1,  5);
  AAdd('YMDemandCentreName',                             'F19.DAT','1' ,  6, -1);
  AAdd('NodeRefNr',                                      'F19.DAT','2' , -1, -1);
  AAdd('AveReturnFlowFactor',                            'F19.DAT','3' , -1, 10);
  AAdd('AveEvaporation',                                 'F19.DAT','3' , 11,  8);
  AAdd('StdDeviationFactor',                             'F19.DAT','3' , 19, 10);
  AAdd('YMDemandCentreRoutingConstant',                  'F19.DAT','3' , 29,  6);
  AAdd('RainfallScalingFactor',                          'F19.DAT','3' , 35,  8);
  AAdd('ReturnFlowChannelNr',                            'F19.DAT','4' , -1,  5);
  AAdd('TotalReturnFlow',                                'F19.DAT','4' ,  5, 10);
  AAdd('FlowDiversion',                                  'F19.DAT','4' , 15, 10);
  AAdd('TotalFlowLost',                                  'F19.DAT','5' , -1, -1);
  AAdd('EvapoTranspiration',                             'F19.DAT','6' , -1, -1);


  // F21.DAT
  //
  AAdd('NrOfMineSubcatchment',                           'F21.DAT','1' , -1, -1);
  AAdd('MineNumber',                                     'F21.DAT','2' , -1,  4);
  AAdd('MineName',                                       'F21.DAT','2' ,  5, -1);
  AAdd('NrOfMineCastPits',                               'F21.DAT','3' , -1,  2);
  AAdd('NrOfUnderGroundMining',                          'F21.DAT','3' ,  3,  2);
  AAdd('NrOfSlurryDump',                                 'F21.DAT','3' ,  5, -1);
  AAdd('RiverChannelNumber',                             'F21.DAT','4' , -1,  4);
  AAdd('PCDChannelNumber',                               'F21.DAT','4' ,  5, -1);
  AAdd('MinePanEvaporationFactors',                      'F21.DAT','5' , -1, -1);
  AAdd('MineLakeEvaporationFactors',                     'F21.DAT','6' , -1, -1);
  AAdd('HydrologyNodeNumber',                            'F21.DAT','7' , -1, -1);
  AAdd('BeneficiationPlantArea',                         'F21.DAT','8' , -1,  5);
  AAdd('BeneficiationRunOffFactor',                      'F21.DAT','8' ,  6, -1);

  AAdd('PitName',                                        'F21.DAT','9' , -1, -1);
  AAdd('CoalReserveArea',                                'F21.DAT','10', -1,  6);
  AAdd('WorkingsArea',                                   'F21.DAT','10',  6,  6);
  AAdd('DisturbedWorkingsArea',                          'F21.DAT','11', -1,  6);
  AAdd('DisturbedArea',                                  'F21.DAT','11',  6,  6);
  AAdd('WaterSurfaceEvapArea',                           'F21.DAT','11', 12,  6);
  AAdd('DisturbedAreaRunOff',                            'F21.DAT','12', -1,  6);
  AAdd('DisturbedWorkingsAreaRunOff',                    'F21.DAT','12',  6,  6);
  AAdd('OpenCastDisturbedAreaRechargeFactors',           'F21.DAT','13', -1, -1);
  AAdd('OpenCastDisturbedWorkingsRechargeFactors',       'F21.DAT','14', -1, -1);
  AAdd('DecantVolume',                                   'F21.DAT','15', -1,  6);
  AAdd('SeepageVolume',                                  'F21.DAT','15',  6,  6);
  AAdd('AnalysisStartVolume',                            'F21.DAT','15', 12,  6);
  AAdd('MaximumSeepageRate',                             'F21.DAT','16', -1,  6);
  AAdd('SeepageExponent',                                'F21.DAT','16',  6,  6);
  AAdd('OpenCastPCDSurfaceArea',                         'F21.DAT','17', -1,  6);
  AAdd('OpenCastPCDStorageCapacity',                     'F21.DAT','17',  6,  6);
  AAdd('OpenCastPCDAnalysisStartVolume',                 'F21.DAT','17', 12,  6);

  AAdd('UndergroundSectionName',                         'F21.DAT','18', -1, 20);
  AAdd('ChannelNumberToUGDam',                           'F21.DAT','18', 20, -1);
  AAdd('UpstreamCatchmentArea',                          'F21.DAT','19', -1,  6);
  AAdd('BoardPillarCatchmentArea',                       'F21.DAT','19',  6,  6);
  AAdd('HighExtractionCatchmentArea',                    'F21.DAT','19', 12,  6);
  AAdd('HighExtractionAreaRunoffFactor',                 'F21.DAT','19', 18,  6);
  AAdd('MineUGUpstreamRunoff',                           'F21.DAT','20', -1, -1);
  AAdd('UGBoardPillarRechargeFactors',                   'F21.DAT','21', -1, -1);
  AAdd('UGHighExtractionRechargeFactors',                'F21.DAT','22', -1, -1);

  AAdd('DumpName',                                       'F21.DAT','23', -1, -1);
  AAdd('DumpSurfaceArea',                                'F21.DAT','24', -1,  6);
  AAdd('RunoffFactorToPCD',                              'F21.DAT','24',  6,  6);
  AAdd('SeepageSplitFactor',                             'F21.DAT','24', 12,  6);
  AAdd('DumpPCDStorageCapacity',                         'F21.DAT','24', 18,  6);
  AAdd('DumpPCDSurfaceArea',                             'F21.DAT','24', 24,  6);
  AAdd('DumpPCDAnalysisStartVolume',                     'F21.DAT','24', 30,  6);
  AAdd('DumpRechargeFactors',                            'F21.DAT','25', -1, -1);
  AAdd('NrOfMineSubcatchment',                           'F21.DAT','26', -1, -1);

  AAdd('CatchmentRefInUse',                              'F21.DAT','27', -1, -1);
  AAdd('GroundwaterFlowVolume',                          'F21.DAT','28', -1, -1);
  AAdd('AntecedentRunoffDecayFactor',                    'F21.DAT','29', -1,  6);
  AAdd('ProportionAntecedentFlow',                       'F21.DAT','29',  6,  6);
  AAdd('GroundwaterFlowVolume',                          'F21.DAT','29', 12,  6);

  // F22.DAT
  //
  AAdd('GroundWaterNodeNumber',                          'F22.DAT','1' , -1, -1);
  AAdd('AquiferStorativity',                             'F22.DAT','2' , -1,  5);
  AAdd('AquiferStaticWaterlevel',                        'F22.DAT','2' ,  6, -1);
  AAdd('UnsaturatedStorageCapacity',                     'F22.DAT','3' , -1,  5);
  AAdd('InitialUnsaturatedStorage',                      'F22.DAT','3' ,  6, -1);
  AAdd('MaximumDischargeRate',                           'F22.DAT','4' , -1,  5);
  AAdd('MovingAverageRecharge',                          'F22.DAT','4' ,  6, -1);
  AAdd('PitmanSoilMoistureCapacity',                     'F22.DAT','5' , -1,  5);
  AAdd('PitmanSoilMoistureStorageCapacity',              'F22.DAT','5' ,  5,  5);
  AAdd('PitmansoilMoistureFlowState',                    'F22.DAT','5' , 10,  5);
  AAdd('PitmanSoilMoistureFlowEquation',                 'F22.DAT','5' , 15,  5);
  AAdd('PitmanMaximumGroundwaterFlow',                   'F22.DAT','5' , 20,  5);
  AAdd('PitmanSoilMoistureRechargeEquation',             'F22.DAT','5' , 25,  5);
  AAdd('PitmanGroundwaterFlow',                          'F22.DAT','5' , 30, -1);
  AAdd('MaximumRateOfGroundwaterBaseFlow',               'F22.DAT','6' , -1,  5);
  AAdd('PowerHeadDifferenceBaseFlowEquation',            'F22.DAT','6' ,  5, -1);
  AAdd('MaximumHydrologicalGradient',                    'F22.DAT','7' , -1, -1);
  AAdd('AquiferTransmissivity',                          'F22.DAT','8' , -1, -1);
  AAdd('BoreHoleDistanceToRiver',                        'F22.DAT','9' , -1, -1);
  AAdd('MaximumGroundwaterAbstraction',                  'F22.DAT','10', -1,  5);
  AAdd('ParameterK2',                                    'F22.DAT','10',  6,  6);
  AAdd('ParameterK3',                                    'F22.DAT','10', 12, -1);
  AAdd('MonthlyWaterEvaporation',                        'F22.DAT','11', -1, -1);
  AAdd('MonthlyWaterUsageFactors',                       'F22.DAT','12', -1, -1);
  AAdd('GroundWaterEvaporationArea',                     'F22.DAT','13', -1, -1);


  // WRYM.DAT
  //
  AAdd('FilePrefix',                     'DIR.DAT','1'  ,  -1,   8);
  AAdd('OutputPath',                     'DIR.DAT','3'  ,  -1,  40);
  AAdd('InputPath',                      'DIR.DAT','2'  ,  -1,  40);

  (* Family file *.fm *)
  AAdd('NrOfReliabilityClasses',             'FM.DAT','1'  ,  -1,   5);
  AAdd('NrOfAllocationLevels',               'FM.DAT','1'  ,   6,   5);
  AAdd('NrOfUserCategories',                 'FM.DAT','1'  ,  11,   5);
  AAdd('PeriodLength',                       'FM.DAT','1'  ,  16,   5);
  AAdd('AllocationOption',                   'FM.DAT','1'  ,  21,   5);

  AAdd('RIValue',                            'FM.DAT','2'  ,  -1,   50);
  AAdd('RILabel',                            'FM.DAT','3'  ,  -1,   50);

  AAdd('Curtailment',                        'FM.DAT','4'  ,  -1,   50);
  AAdd('Distribution',                       'FM.DAT','5'  ,  -1,   50);

  AAdd('NrOfDemandDefs',                     'FM.DAT','6'  ,  -1,   10);

  AAdd('ParentSubSystemID',                  'FM.DAT','7'  ,  -1,   3);
  AAdd('GrowthType',                         'FM.DAT','7'  ,   4,   4);
  AAdd('TargetDemand',                       'FM.DAT','7'  ,   8,   5);
  AAdd('DemandDefName',                      'FM.DAT','7'  ,   13,  10);
  AAdd('DDDemandCentreID',                   'FM.DAT','7'  ,   23,  3);
  AAdd('DCUserCategoryID',                   'FM.DAT','7'  ,   26,  3);
  AAdd('SupportArc1',                        'FM.DAT','7'  ,   29,  2);
  AAdd('SupportArc2',                        'FM.DAT','7'  ,   31,  2);
  AAdd('NrOfSupportSubSystems',              'FM.DAT','7'  ,   33,  2);
  AAdd('SupSubSystemID',                     'FM.DAT','7'  ,   35,  2);
  AAdd('SupSubSysChannelNr',                 'FM.DAT','7'  ,   37,  2);

  AAdd('NrOfSubSystems',                     'FM.DAT','8'  ,   -1,  3);
  AAdd('NrOfStartStoragePercs',              'FM.DAT','8'  ,    4,  3);
  AAdd('NrOfLoadCases',                      'FM.DAT','8'  ,    7,  3);
  AAdd('NrOfCurveSets',                      'FM.DAT','8'  ,    10,  3);
  AAdd('MonthCurveSet',                      'FM.DAT','9'  ,    -1,  36);

  AAdd('NonFirmSubsystemNo',                 'FM.DAT','10' ,   -1,  5);
  AAdd('SupportCalcType',                    'FM.DAT','10' ,    6,  36);

  AAdd('SubSystemName',                      'FM.DAT','11' ,    -1,  13);

  AAdd('SubtractedSubSystemID',              'FM.DAT','11' ,    14,  3);
  AAdd('SupportingSubSystemID',              'FM.DAT','11' ,    17,  3);
  AAdd('SupportingChannelNr',                'FM.DAT','11' ,    20,  3);

  AAdd('ShortTermYield',                     'FM.DAT','11' ,    23,  6);

  AAdd('LowestStreamFlow',                   'FM.DAT','11' ,    30,  6);
  AAdd('LongTermYield',                      'FM.DAT','11' ,    38,  6);

  AAdd('SubSystemStartYear',                 'FM.DAT','11' ,    44,  6);
  AAdd('SubSystemStartMonth',                'FM.DAT','11' ,    50,  6);
  AAdd('SubSystemEndYear',                   'FM.DAT','11' ,    56,  4);
  AAdd('SubSystemEndMonth',                  'FM.DAT','11' ,    60,  4);
  AAdd('FirmYield',                          'FM.DAT','11' ,    65,  10);

  AAdd('StartStoragePerc',                   'FM.DAT','12' ,    -1,  4);
  AAdd('NumberOfSets',                       'FM.DAT','12' ,     4,  4);

  AAdd('LoadCase',                           'FM.DAT','13' ,     -1,  14);
  AAdd('CoefficientA',                       'FM.DAT','13' ,     15,  10);
  AAdd('CoefficientB',                       'FM.DAT','13' ,     25,  10);
  AAdd('CoefficientC',                       'FM.DAT','13' ,     35,  10);
  AAdd('CoefficientD',                       'FM.DAT','13' ,     45,  10);
  AAdd('RiskProportion',                     'FM.DAT','13' ,     55,  10);

  AAdd('SubsystemNo',                        'FM.DAT','14' ,     -1,  5);
  AAdd('SubSystemReservoirNrs',              'FM.DAT','14' ,      6,  50);
  AAdd('SupportStrategy',                    'FM.DAT','15' ,     -1,  50);
  AAdd('NrInFixedPosition',                  'FM.DAT','16' ,     -1,  50);
  AAdd('FixedPosSubSystemID',                'FM.DAT','17' ,     -1,  5);
  AAdd('FixedPositionNr',                    'FM.DAT','17' ,      6,  15);
  AAdd('NrInSpecificOrder',                  'FM.DAT','18' ,      -1,  15);

  AAdd('BeforeSubSystemID',                  'FM.DAT','19' ,      -1,  5);
  AAdd('AfterSubSystemID',                   'FM.DAT','19' ,       6,  5);

  AAdd('NoOfSupportStructChn',               'FM.DAT','20' ,       -1,  5);

  AAdd('SupportChannelNr',                   'FM.DAT','21' ,       -1,  5);
  AAdd('NrOfCntrlSubSystems',                'FM.DAT','21' ,       6,  5);
  AAdd('CntrlSubSystemID',                   'FM.DAT','21' ,       11,  5);
  AAdd('CntrlFactor'     ,                   'FM.DAT','21' ,       16,  5);

  AAdd('BalancingOption',                    'FM.DAT','22' ,       -1,  5);
  AAdd('BalancingOptSubsys',                 'FM.DAT','23' ,       -1,  50);

  AAdd('NoOfChannelstbCurtailed',            'FM.DAT','24' ,       -1,  5);
  AAdd('UserPropriety',                      'FM.DAT','24' ,        6,  5);
  AAdd('AssociatedSubsys',                   'FM.DAT','24' ,       11,  5);

  //
  // dam.dat
  //

  AAdd('ReservoirImpCount',                   'DAM.DAT','1' ,       -1,  10);

  AAdd('BaseNodeNumber',                      'DAM.DAT','2' ,       -1,  9);
  AAdd('BaseNodeNumber',                      'DAM.DAT','2' ,        9,  3);
  AAdd('ReservoirStartYear',                  'DAM.DAT','2' ,       12,  5);
  AAdd('ReservoirStartMonth',                 'DAM.DAT','2' ,       17,  5);
  AAdd('ReservoirEndYear',                    'DAM.DAT','2' ,       22,  5);
  AAdd('ReservoirEndMonth',                   'DAM.DAT','2' ,       27,  5);
  AAdd('ReservoirEconomicLife',               'DAM.DAT','2' ,       32,  6);
  AAdd('ReservoirCapitalCost',                'DAM.DAT','2' ,       38,  6);
  AAdd('ReservoirOMCost',                     'DAM.DAT','2' ,       44,  6);

  AAdd('ReservoirYearsToConstruct',           'DAM.DAT','3' ,       -1,  10);

  AAdd('NumberOfPeriods',                     'DAM.DAT','4' ,       -1,  6);
  AAdd('NumberFamilyGrp',                     'DAM.DAT','4' ,        6,  6);

  AAdd('STYieldStartYear',                    'DAM.DAT','5' ,       -1,  6);
  AAdd('STYieldMonth',                        'DAM.DAT','5' ,        7,  6);
  AAdd('STFileName',                          'DAM.DAT','5' ,        13,  25);

  AAdd('NumberOfSWFiles',                     'DAM.DAT','6' ,        -1,  10);
  AAdd('SwitchDefStartYear',                  'DAM.DAT','7' ,        -1,  6);
  AAdd('SwitchDefStartMonth',                 'DAM.DAT','7' ,        7,  5);
  AAdd('SwitchDefFileName',                   'DAM.DAT','7' ,        12,  25);

  (* PMP.DAT File*)
  AAdd('ChannelYearsInAnalysis',              'PMP.DAT','1' ,        -1,  25);

  AAdd('DemandChannel',                       'PMP.DAT','2' ,        -1,  6);
  AAdd('ChannelStartYear',                    'PMP.DAT','2' ,         9,  4);
  AAdd('ChannelStartMonth',                   'PMP.DAT','2' ,         13,  4);
  AAdd('ChannelEndYear',                      'PMP.DAT','2' ,         17,  4);
  AAdd('ChannelEndMonth',                     'PMP.DAT','2' ,         22,  4);
  AAdd('ChannelEconomicLife',                 'PMP.DAT','2' ,         26,  4);
  AAdd('ChannelCapitalCost',                  'PMP.DAT','2' ,         31,  4);
  AAdd('ChannelFixedOMCost',                  'PMP.DAT','2' ,         36,  6);
  AAdd('ChannelVariableOMCost',               'PMP.DAT','2' ,         42,  6);

  AAdd('ChannelYearsToConstruct',             'PMP.DAT','3' ,         -1,  6);
  AAdd('ChannelCostSchedule',                 'PMP.DAT','3' ,          7,  6);
  AAdd('ChannelYearsToConstruct',             'PMP.DAT','3' ,          13,  6);

  AAdd('ChannelEscalationCost',               'PMP.DAT','4' ,          -1,  10);
  AAdd('ChannelYearsInAnalysis',              'PMP.DAT','4' ,         11,  10);

  //
  // DBF.BAT file
  //


  AAdd('ChannelYearsInAnalysis',               'DBF.DAT','1' ,          -1,  10);

  AAdd('DemandChannel'         ,               'DBF.DAT','2' ,          -1,  9);
  AAdd('ChannelStartYearDescr',                'DBF.DAT','2' ,          10,  4);
  AAdd('ChannelStartMonth',                    'DBF.DAT','2' ,          14,  3);
  AAdd('YearDemandChannelObsoleteDescr',       'DBF.DAT','2' ,          17,  5);
  AAdd('MonthDemandChannelObsoleteDescr',      'DBF.DAT','2' ,          22,  5);
  AAdd('EquationFunctionX',                    'DBF.DAT','2' ,          27,  5);
  AAdd('EquationFunctionY',                    'DBF.DAT','2' ,          32,  5);
  AAdd('EquationFunctionCostY',                'DBF.DAT','2' ,          37,  5);
  AAdd('EquationFunctionNonSupply',            'DBF.DAT','2' ,          42,  5);

  AAdd('DisbenefitEscalationRate',             'DBF.DAT','3' ,          -1,  80);

  AAdd('WaterQualityConstraint',               'DBF.DAT','4' ,          -1,  6);
  AAdd('CubicEquation',                        'DBF.DAT','4' ,           7,  15);
  AAdd('WaterQualityDisbenefit',               'DBF.DAT','5' ,          -1,  80);

  //
  // GTH.DAT file
  //

  AAdd('ChannelYearsInAnalysis',               'GTH.DAT','1' ,          -1,  10);
  AAdd('DemandChannel',                        'GTH.DAT','2' ,          -1,  10);
  AAdd('DemandGrowthFactors',                  'GTH.DAT','3' ,          -1,  80);

  AAdd('SupplyMinmaxChannelCount',             'GTH.DAT','4' ,          -1,  10);

  AAdd('GrowthMinMaxChannel',                  'GTH.DAT','5' ,          -1,  4);
  AAdd('ArcNumber',                            'GTH.DAT','5' ,           5,  4);
  AAdd('MinMaxGrowthFactors',                  'GTH.DAT','6' ,          -1,  80);
  AAdd('GaugeNumber',                          'GTH.DAT','7' ,          -1,  6);
  AAdd('AFFGrowthFactors',                     'GTH.DAT','8' ,          -1,  80);
  AAdd('IRRGrowthFactors',                     'GTH.DAT','9' ,          -1,  80);
  AAdd('URBGrowthFactors',                     'GTH.DAT','10' ,         -1,  80);

  //
  // HST.DAT file
  //

  //
  // HYD.DAT file
  //

  //
  // PUR.DAT file
  //

  AAdd('ChannelYearsInAnalysis',               'PUR.DAT','1' ,         -1,  10);

  AAdd('NoOfPurificationPlants',               'PUR.DAT','2' ,         -1,  10);

  AAdd('NoOfPurificationChannels',             'PUR.DAT','3' ,         -1,  8);
  AAdd('PurificationStartYear',                'PUR.DAT','3' ,          9,  4);
  AAdd('PurificationStartMonth',               'PUR.DAT','3' ,          13,  4);
  AAdd('PurificationEndYear',                  'PUR.DAT','3' ,          17,  4);
  AAdd('PurificationEndMonth',                 'PUR.DAT','3' ,          21,  4);
  AAdd('EconomicLife',                         'PUR.DAT','3' ,          25,  4);

  AAdd('CapitalCost',                          'PUR.DAT','3' ,          29,  6);
  AAdd('FixedOpMaintenanceCost',               'PUR.DAT','3' ,          35,  6);
  AAdd('VarOpMaintenanceCost',                 'PUR.DAT','3' ,          41,  6);

  AAdd('YearsInConstruction',                  'PUR.DAT','4' ,          -1,  6);
  AAdd('FractionOfCapital',                    'PUR.DAT','4' ,           7,  6);
  AAdd('AnnualEscalationFactors',              'PUR.DAT','5' ,         -1,  80);

  //
  // REC.DAT file
  //

  AAdd('ChannelYearsInAnalysis',               'REC.DAT','1' ,         -1,  10);
  AAdd('NoOfReclamationPlants',                'REC.DAT','2' ,         -1,  10);

  AAdd('NoOfReclamationChannels',              'REC.DAT','3' ,         -1,  6);
  AAdd('ReclamationPlantYear',                 'REC.DAT','3' ,          7,  6);
  AAdd('ReclamationPlantMonth',                'REC.DAT','3' ,          13,  6);
  AAdd('PlantYearAbsolute',                    'REC.DAT','3' ,          19,  6);
  AAdd('PlantMonthAbsolute',                   'REC.DAT','3' ,          25,  6);
  AAdd('RecEcoLife',                           'REC.DAT','3' ,          31,  6);
  AAdd('RecCapitalCost',                       'REC.DAT','3' ,          37,  6);
  AAdd('FixedOpMaintenanceCostDescr',          'REC.DAT','3' ,          43,  6);
  AAdd('VarOpMaintenanceCostDescr',            'REC.DAT','3' ,          49,  6);

  AAdd('YearsInConstruction',                  'REC.DAT','4' ,          -1,  6);
  AAdd('FractionOfCapital',                    'REC.DAT','4' ,           7,  6);

  //
  // RET.DAT file
  //

  AAdd('NoOfReturnFlows',                      'RET.DAT','1' ,           -1,  6);

  AAdd('DemandChannel',                        'RET.DAT','2' ,           -1,  6);
  AAdd('NumOfCorrespondingChannels',           'RET.DAT','2' ,            7,  6);
  AAdd('ReturnFlowGaugeNumber',                'RET.DAT','2' ,            13, 6);
  AAdd('MonthlyAvrgFactor',                    'RET.DAT','2' ,            19, 6);
  AAdd('CalibrationFactor',                    'RET.DAT','2' ,            25, 6);
  AAdd('MonthlyAvrgNetEvap',                   'RET.DAT','2' ,            31, 6);
  AAdd('RoutingConstant',                      'RET.DAT','2' ,            37, 6);
  AAdd('CurtailmentFactor',                    'RET.DAT','2' ,            43, 6);
  AAdd('MultiplicationFactor',                 'RET.DAT','2' ,            49, 6);

  AAdd('PotentialMonthlyEvap',                 'RET.DAT','3' ,            -1, 80);

  AAdd('CorrespondingChannel',                 'RET.DAT','4' ,            -1, 10);
  AAdd('AbstractionChannel',                   'RET.DAT','5' ,             11, 10);
  AAdd('AssumedFactor',                        'RET.DAT','6' ,             21, 10);

  //
  // SW.DAT file
  //
  AAdd('NoOfSwitchDefinitionChannel',          'SW.DAT','1' ,             -1, 10);

  AAdd('SwitchType',                           'SW.DAT','2' ,             -1, 2);
  AAdd('SwitchDefinitionChannel',              'SW.DAT','2' ,              3, 4);
  AAdd('SwitchAssociatedNodeNr',               'SW.DAT','2' ,              7, 4);
  AAdd('SwitchWaterlevel',                     'SW.DAT','2' ,              11, 7);
  AAdd('SwitchInitialStatus',                  'SW.DAT','2' ,              18, 2);

  //
  // TAR.DAT file
  //

  AAdd('DataYears',                             'TAR.DAT','1' ,             -1, 10);
  AAdd('ChannelNumber',                         'TAR.DAT','2' ,             -1, 10);
  AAdd('Tariff',                                'TAR.DAT','2' ,             11, 10);
  AAdd('EscalationFactors',                     'TAR.DAT','3' ,             -1, 80);

  //
  //  ALO.DAT file
  //

  AAdd('NoOfAllocChannelDescr',                 'ALO.DAT','1' ,             -1, 2);
  AAdd('TargetRecurrenceInterval',              'ALO.DAT','1' ,              3, 10);
  AAdd('NrOfSubSystems',                        'ALO.DAT','2' ,              -1, 10);

  //
  // CUR.DAT file
  //

  AAdd('MultiCurNumberOfChannels',           'CUR.DAT','1' ,              -1,  3);
  AAdd('MultiCurChannel',                    'CUR.DAT','2' ,              -1,  4);
  AAdd('MultiCurReservoir',                  'CUR.DAT','2' ,              4,   4);
  AAdd('MultiCurDecisionMonth',              'CUR.DAT','2' ,              8,   2);
  AAdd('MultiCurStartMonth',                 'CUR.DAT','2' ,              10,  3);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              -1,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              8,   7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              15,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              22,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              29,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              36,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              43,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              50,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              57,  7);
  AAdd('MultiCurElevation',                  'CUR.DAT','3' ,              64,  7);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              -1,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              6,   5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              11,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              16,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              21,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              26,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              31,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              36,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              41,  5);
  AAdd('MultiCurFactor',                     'CUR.DAT','4' ,              46,  5);

end;

end.

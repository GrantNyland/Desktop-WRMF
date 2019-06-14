//
//  UNIT      : Contains language text.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/01/23
//  COPYRIGHT : Copyright © 2003 DWS
//
unit ULanguageText_ENG_HelpAbout;

interface

type TTextItemAddFunction = procedure (AContext, AConstant, AText: string) of object;

procedure LoadLanguageText(AAdd: TTextItemAddFunction);

implementation

procedure LoadLanguageText(AAdd: TTextItemAddFunction);
const OPNAME = 'LoadLanguageText';
begin
  AAdd('HelpAbout','Address','Pbag X313'#13#10 +
                             'Pretoria'#13#10 +
                             '0001'#13#10 );
  AAdd('HelpAbout','Colon','  :');
  AAdd('HelpAbout','Colon1','  :');
  AAdd('HelpAbout','Colon2',' :');
  AAdd('HelpAbout','Colon3',' :');
  AAdd('HelpAbout','Colon4',' :');
  AAdd('HelpAbout','CompanyName','DWS');
  AAdd('HelpAbout','CopyRight','Copyright © 2022 DWS.                                                                                   All Rights Reserved.');
  AAdd('HelpAbout','DiskSpace',' DiskSpace');
  AAdd('HelpAbout','FormCaption','About the Water Resources Management Framework');
  AAdd('HelpAbout','FreeMemory',' FreeMemory');
  AAdd('HelpAbout','Ownership','Ownership');
  AAdd('HelpAbout','Product','This Product is licensed to:');
  AAdd('HelpAbout','Support','Support');
  AAdd('HelpAbout','SupportDetails','All system errors are logged in the Error Log File,Application Name_ErrorLog.txt in Applications Directory\Logs folder.Please email the Log file to Malose Ngoepe.Tel (012) 336-6967.');
  AAdd('HelpAbout','VersionDetails','WRMF ');

  AAdd('HelpAbout','Description', 'WRMF is an acronym for Water Resource Modeling Framework. The system has been'#13#10 +
                                  'developed by the Department of Water and Sanitation to improve the efficiency of managing '#13#10 +
                                  'and sharing of planning information used in water resource studies.'#13#10 + #13#10+
                                  'The following main modules are currently available in the system, the Rain PMS, the WRYM MF and'#13#10 +
                                  'the WRPM MF. The WRMF also includes many other utilities such as a Network Visualiser for'#13#10 +
                                  'interfacing with the WRYM MF and the YRC for post processing the output form the WRYM MF.'#13#10 + #13#10+
                                  'This system has been developed with the support of the wider water resource planning community in'#13#10 +
                                  'South Africa, through informal communications and formal workshops. This assistance is greatfully acknowledged.'#13#10);

  AAdd('HelpAbout','Acknowledgements','The core development team was:'#13#10);



  AAdd('HelpAbout','ProjectManagement','Project Management');
  AAdd('HelpAbout','ProjectManagementName','Beason Mwaka'#13#10 +
                                            'Malose Ngoepe'#13#10);
  AAdd('HelpAbout','ProjectManagementOrg','DWS'#13#10 +
                                           'DWS'#13#10);

  AAdd('HelpAbout','BusinessSupport','Business Support');
  AAdd('HelpAbout','BusinessSupportNames','WRP');{'Edwin Lillie'#13#10 +
                                          'Bennie Haasbroek'#13#10 +
                                          'Grant Nyland'#13#10 +
                                          'Ans Gerber'#13#10 +
                                          'Pieter van Rooyen'#13#10 +
                                          'Gerald De Jager'#13#10);}
  AAdd('HelpAbout','BusinessSupportOrgs', 'WRP');{'Hydrosol'#13#10 +
                                          'Hydrosol'#13#10 +
                                          'Hydrosol'#13#10 +
                                          'Hydrosol'#13#10 +
                                          'WRP'#13#10 +
                                          'Hydrosol'#13#10);
                                               }
  AAdd('HelpAbout','ITDevelopment','IT Development');
  AAdd('HelpAbout','ITDevelopmentNames', 'EOH');{ 'Dziedzi Ramulondi'#13#10 +
                                          'Samuel Dhlamini'#13#10 +
                                          'Kagiso Rammusi'#13#10);
                                              }
  AAdd('HelpAbout','ITDevelopmentOrgs',  'EOH');{ 'BCX'#13#10 +
                                          'BCX'#13#10 +
                                          'BCX'#13#10);}
  AAdd('HelpAbout','UserSupport','User Support');
  AAdd('HelpAbout','UserSupportNames','DWS'); {'Diana Manwa'#13#10 +
                                      'Nana Mthethwa'#13#10 );
                                            }
  AAdd('HelpAbout','UserSupportOrgs', 'DWS');{'Isifundi'#13#10 +
                                      'Isifundi'#13#10 );
                                              }
  AAdd('HelpAboutNV','Address','Pbag X313');
  AAdd('HelpAboutNV','Address1','Pretoria');
  AAdd('HelpAboutNV','Address2',' 0001');
  AAdd('HelpAboutNV','Colon','  :');
  AAdd('HelpAboutNV','Colon1','  :');
  AAdd('HelpAboutNV','Colon2','  :');
  AAdd('HelpAboutNV','Colon3','  :');
  AAdd('HelpAboutNV','Colon4','  :');
  AAdd('HelpAboutNV','CompanyName','DWS (Strategic Planning Systems)');
  AAdd('HelpAboutNV','CopyRight','Copyright © 2022 DWS. All Rights Reserved.');
  AAdd('HelpAboutNV','DiskSpace',' DiskSpace');
  AAdd('HelpAboutNV','FormCaption','About the Network Visualiser');
  AAdd('HelpAboutNV','FreeMemory',' FreeMemory');
  AAdd('HelpAboutNV','Ownership','Ownership');
  AAdd('HelpAboutNV','Product','This Product is licensed to:');
  AAdd('HelpAboutNV','Support','Support');
  AAdd('HelpAboutNV','SupportDetails','All system errors are logged on the Error Log File.');
  AAdd('HelpAboutNV','Version','Version');
end;

end.

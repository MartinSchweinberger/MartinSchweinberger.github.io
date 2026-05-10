##########################################################
### --- Text Mining with R - Text Classification
##########################################################
# load packages
library("plyr")
library("tm")
library("class")
source("C:\\R/ExtractsTextFromWebpage.R")
##########################################################
# define paths to input
obama001 <- "http://obamaspeeches.com/P-Obama-Inaugural-Speech-Inauguration.htm"
obama002 <- "http://obamaspeeches.com/E11-Barack-Obama-Election-Night-Victory-Speech-Grant-Park-Illinois-November-4-2008.htm"
obama003 <- "http://obamaspeeches.com/E-Barack-Obama-Speech-Manassas-Virgina-Last-Rally-2008-Election.htm"
obama004 <- "http://obamaspeeches.com/E11-Barack-Obama-Election-Night-Victory-Speech-Grant-Park-Illinois-November-4-2008.htm"
obama005 <- "http://obamaspeeches.com/E10-Barack-Obama-The-American-Promise-Acceptance-Speech-at-the-Democratic-Convention-Mile-High-Stadium--Denver-Colorado-August-28-2008.htm"
obama006 <- "http://obamaspeeches.com/E09-Barack-Obama-Final-Primary-Night-Presumptive-Democratic-Nominee-Speech-St-Paul-Minnesota-June-3-2008.htm"
obama007 <- "http://obamaspeeches.com/E08-Barack-Obama-North-Carolina-Primary-Night-Raleigh-NC-May-6-2008.htm"
obama008 <- "http://obamaspeeches.com/E07-Barack-Obama-Pennsylvania-Primary-Night-Evansville-Indiana-April-22-2008.htm"
obama009 <- "http://obamaspeeches.com/E06-Barack-Obama-AP-Annual-Luncheon-Washington-DC-April-14-2008-religion-guns-pennsylvania.htm"
obama010 <- "http://obamaspeeches.com/E05-Barack-Obama-A-More-Perfect-Union-the-Race-Speech-Philadelphia-PA-March-18-2008.htm"
obama011 <- "http://obamaspeeches.com/E04-Barack-Obama-March-4-Primary-Night-Texas-and-Ohio-San-Antonio-TX-March-4-2008.htm"
obama012 <- "http://obamaspeeches.com/E03-Barack-Obama-Potomac-Primary-Night-Madison-WI-February-12-2008.htm"
obama013 <- "http://obamaspeeches.com/E02-Barack-Obama-Super-Tuesday-Chicago-IL-February-5-2008.htm"
obama014 <- "http://obamaspeeches.com/120-Obama-Turn-The-Page-Speech-California-Deomcratic-Convention.htm"
obama015 <- "http://obamaspeeches.com/099-Announcement-For-President-Springfield-Illinois-Obama-Speech.htm"
obama016 <- "http://obamaspeeches.com/097-The-Time-Has-Come-for-Universal-Health-Care-Obama-Speech.htm"
obama017 <- "http://obamaspeeches.com/096-Floor-Statement-on-Presidents-Decision-to-Increase-Troops-in-Iraq-Obama-Speech.htm"
obama018 <- "http://obamaspeeches.com/095-Race-Against-Time-World-AIDS-Day-Speech-Obama-Speech.htm"
obama019 <- "http://obamaspeeches.com/094-A-Way-Forward-in-Iraq-Obama-Speech.htm"
obama020 <- "http://obamaspeeches.com/093-Martin-Luther-King-Memorial-Groundbreaking-Ceremony-Obama-Speech.htm"
obama021 <- "http://obamaspeeches.com/092-Military-Commission-Legislation-Obama-Speech.htm"
obama022 <- "http://obamaspeeches.com/091-Floor-Statement-on-the-Habeas-Corpus-Amendment-Obama-Speech.htm"
obama023 <- "http://obamaspeeches.com/089-Energy-Independence-A-Call-for-Leadership-Obama-Speech.htm"
obama024 <- "http://obamaspeeches.com/088-An-Honest-Government-A-Hopeful-Future-Obama-Speech.htm"
obama025 <- "http://obamaspeeches.com/087-Xavier-University-Commencement-Address-Obama-Speech.htm"
obama026 <- "http://obamaspeeches.com/086-AFSCME-National-Convention-Obama-Speech.htm"
obama027 <- "http://obamaspeeches.com/085-Vote-against-the-Gulf-of-Mexico-Energy-Bill-Obama-Speech.htm"
obama028 <- "http://obamaspeeches.com/084-Support-of-HR-9-the-Voting-Rights-Act-Obama-Speech.htm"
obama029 <- "http://obamaspeeches.com/083-Statement-of-Support-for-Stem-Cell-Research-Obama-Speech.htm"
obama030 <- "http://obamaspeeches.com/082-Campus-Progress-Annual-Conference-Obama-Speech.htm"
obama031 <- "http://obamaspeeches.com/081-Call-to-Renewal-Keynote-Address-Obama-Speech.htm"
obama032 <- "http://obamaspeeches.com/080-Iraq-Debate-Obama-Speech.htm"
obama033 <- "http://obamaspeeches.com/079-Northwestern-University-Commencement-Address-Obama-Speech.htm"
obama034 <- "http://obamaspeeches.com/078-Katrina-Reconstruction-Obama-Podcast.htm"
obama035 <- "http://obamaspeeches.com/077-Take-Back-America-Obama-Speech.htm"
obama036 <- "http://obamaspeeches.com/076-Network-Neutrality-Obama-Podcast.htm"
obama037 <- "http://obamaspeeches.com/075-Federal-Marriage-Amendment-Obama-Speech.htm"
obama038 <- "http://obamaspeeches.com/074-University-of-Massachusetts-at-Boston-Commencement-Address-Obama-Speech.htm"
obama039 <- "http://obamaspeeches.com/073-General-Michael-Hayden-Nomination-Obama-Speech.htm"
obama040 <- "http://obamaspeeches.com/072-Opposition-to-the-Amendment-Requiring-a-Photo-ID-to-Vote-Obama-Speech.htm"
obama041 <- "http://obamaspeeches.com/071-Employment-Verification-Amendment-for-the-Immigration-Bill-Obama-Speech.htm"
obama042 <- "http://obamaspeeches.com/068-Honoring-Our-Commitment-to-Veterans-Obama-Speech.htm"
obama043 <- "http://obamaspeeches.com/067-EMILYS-List-Annual-Luncheon-Obama-Speech.htm"
obama044 <- "http://obamaspeeches.com/066-A-Real-Solution-for-High-Gas-Prices-Obama-Speech.htm"
obama045 <- "http://obamaspeeches.com/065-Immigration-Rallies-Obama-Podcast.htm"
obama046 <- "http://obamaspeeches.com/064-Amendment-to-Stop-No-Bid-Contracts-for-Gulf-Coast-Reconstruction-Obama-Speech.htm"
obama047 <- "http://obamaspeeches.com/063-Updates-on-Darfur-Immigration-Gas-Prices-Obama-Podcast.htm"
obama048 <- "http://obamaspeeches.com/062-Southern-Illinois-University-School-of-Medicine-Commencement-Obama-Speech.htm"
obama049 <- "http://obamaspeeches.com/061-Immigration-Reform-Obama-Speech.htm"
obama050 <- "http://obamaspeeches.com/060-Energy-Independence-and-the-Safety-of-Our-Planet-Obama-Speech.htm"
obama051 <- "http://obamaspeeches.com/059-Improving-Chemical-Plant-Security-Obama-Podcast.htm"
obama052 <- "http://obamaspeeches.com/057-21st-Century-Schools-for-a-21st-Century-Economy-Obama-Speech.htm"
obama053 <- "http://obamaspeeches.com/056-Lobbyin-Reform-Meals-Amendment-Obama-Speech.htm"
obama054 <- "http://obamaspeeches.com/055-Debate-on-Ethics-Reform-Obama-Speech.htm"
obama055 <- "http://obamaspeeches.com/054-Energy-Security-is-National-Security-Governors-Ethanol-Coalition-Obama-Speech.htm"
obama056 <- "http://obamaspeeches.com/053-Floor-Statement-S2271-PATRIOT-Act-Reauthorization-Obama-Speech.htm"
obama057 <- "http://obamaspeeches.com/052-Darfur-Current-Policy-Not-Enough-Obama-Speech.htm"
obama058 <- "http://obamaspeeches.com/050-Foreign-Relations-Committee-Lugar-Obama-legislation-S1949-Obama-Speech.htm"
obama059 <- "http://obamaspeeches.com/049-Hurricane-Katrina-Child-Assistance-Amendment-Obama-Speech.htm"
obama060 <- "http://obamaspeeches.com/048-Supreme-Court-Nomination-of-Samuel-Alito-Obama-Podcast.htm"
obama061 <- "http://obamaspeeches.com/047-Lobbying-Reform-Summit-National-Press-Club-Obama-Speech.htm"
obama062 <- "http://obamaspeeches.com/046-Confirmation-of-Judge-Samuel-Alito-Jr-Obama-Speech.htm"
obama063 <- "http://obamaspeeches.com/045-Meeting-on-Iraq-with-President-Bush-Obama-Speech.htm"
obama064 <- "http://obamaspeeches.com/044-Remarks-Honest-Leadership-and-Open-Government-Obama-Podcast.htm"
obama065 <- "http://obamaspeeches.com/043-From-the-Road-Israel-and-the-Palestinian-territories-Obama-Podcast.htm"
obama066 <- "http://obamaspeeches.com/042-From-the-Road-Speaking-with-American-Troops-in-Iraq-Obama-Speech-Podcast.htm"
obama067 <- "http://obamaspeeches.com/041-The-PATRIOT-Act-Obama-Speech.htm"
obama068 <- "http://obamaspeeches.com/040-Moving-Forward-in-Iraq-Chicago-Council-on-Foreign-Relations-Obama-Speech.htm"
obama069 <- "http://obamaspeeches.com/039-Robert-F-Kennedy-Human-Rights-Award-Ceremony-Obama-Speech.htm"
obama070 <- "http://obamaspeeches.com/038-National-Womens-Law-Center-Obama-Speech.htm"
obama071 <- "http://obamaspeeches.com/037-Sex-on-TV-4-Report-Obama-Speech.htm"
obama072 <- "http://obamaspeeches.com/036-Non-Proliferation-and-Russia-The-Challenges-Ahead-Obama-Speech.htm"
obama073 <- "http://obamaspeeches.com/035-Chicago-White-Sox-Obama-Speech.htm"
obama074 <- "http://obamaspeeches.com/034-Death-of-Rosa-Parks-Obama-Speech.htm"
obama075 <- "http://obamaspeeches.com/033-Teaching-Our-Kids-in-a-21st-Century-Economy-Obama-Speech.htm"
obama076 <- "http://obamaspeeches.com/032-Avian-Flu-Obama-Speech.htm"
obama077 <- "http://obamaspeeches.com/031-Confirmation-of-Judge-John-Roberts-Obama-Speech.htm"
obama078 <- "http://obamaspeeches.com/030-Resources-for-the-Future-Obama-Speech.htm"
obama079 <- "http://obamaspeeches.com/029-Statement-on-Hurricane-Katrina-Relief-Efforts-Obama-Speech.htm"
obama080 <- "http://obamaspeeches.com/028-AFL-CIO-National-Convention-Obama-Speech.htm"
obama081 <- "http://obamaspeeches.com/026-Foreign-Operations-Appropriations-Bill-and-the-Avian-Flu-Obama-Speech.htm"
obama082 <- "http://obamaspeeches.com/025-American-Legion-Conference-Obama-Speech.htm"
obama083 <- "http://obamaspeeches.com/024-Literacy-and-Education-in-a-21st-Century-Economy-Obama-Speech.htm"
obama084 <- "http://obamaspeeches.com/022-Pritzker-School-of-Medicine-Commencement-Obama-Speech.htm"
obama085 <- "http://obamaspeeches.com/021-Nomination-of-Justice-Janice-Rogers-Brown-Obama-Speech.htm"
obama086 <- "http://obamaspeeches.com/018-Abraham-Lincoln-National-Cemetery-Obama-Speech.htm"
obama087 <- "http://obamaspeeches.com/017-America-Nuclear-Non-Proliferation-Policy-Remarks-Obama-Speech.htm"
obama088 <- "http://obamaspeeches.com/016-Rockford-Register-Star-Young-American-Awards-Obama-Speech.htm"
obama089 <- "http://obamaspeeches.com/015-NAACP-Fight-for-Freedom-Fund-Dinner-Obama-Speech.htm"
obama090 <- "http://obamaspeeches.com/014-National-Press-Club-Obama-Speech.htm"
obama091 <- "http://obamaspeeches.com/013-SIUC-College-of-Agriculture-50th-Anniversary-Obama-Speech.htm"
obama092 <- "http://obamaspeeches.com/012-Abraham-Lincoln-Presidential-Library-and-Museum-Obama-Speech.htm"
obama093 <- "http://obamaspeeches.com/011-Amendment-to-Provide-Meals-Phone-Service-to-Wounded-Veterans-Obama-Speech.htm"
obama094 <- "http://obamaspeeches.com/010-The-Nuclear-Option-Obama-Speech.htm"
obama095 <- "http://obamaspeeches.com/009-Confirmation-Hearing-of-John-Bolton-Obama-Speech.htm"
obama096 <- "http://obamaspeeches.com/007-American-Legion-Legislative-Rally-Obama-Speech.htm"
obama097 <- "http://obamaspeeches.com/006-CURE-Keynote-Address-Obama-Speech.htm"
obama098 <- "http://obamaspeeches.com/005-Remarks-of-TechNet-Obama-Speech.htm"
obama099 <- "http://obamaspeeches.com/004-S256-the-Bankruptcy-Abuse-and-Prevention-Act-of-2005-Obama-Speech.htm"
obama100 <- "http://obamaspeeches.com/003-John-Lewis-65th-Birthday-Gala-Obama-Speech.htm"
obama101 <- "http://obamaspeeches.com/002-Keynote-Address-at-the-2004-Democratic-National-Convention-Obama-Speech.htm"
obama102 <- "http://obamaspeeches.com/001-2002-Speech-Against-the-Iraq-War-Obama-Speech.htm"
romney001 <- "http://mittromneycentral.com/speeches/2012-speeches/11712-mitt-romneys-concession-speech/"
romney002 <- "http://mittromneycentral.com/speeches/2012-speeches/11212-real-change-from-day-one/"
romney003 <- "http://mittromneycentral.com/speeches/2012-speeches/102612-remarks-on-the-american-economy/"
romney004 <- "http://mittromneycentral.com/speeches/2012-speeches/100812-foreign-policy-speech-the-mantel-of-leadership/"
romney005 <- "http://mittromneycentral.com/speeches/2012-speeches/091712-the-clinton-global-initiative/"
romney006 <- "http://mittromneycentral.com/speeches/2012-speeches/091712-u-s-hispanic-chamber-of-commerce/"
romney007 <- "http://mittromneycentral.com/speeches/2012-speeches/091112-the-national-guard-association-conference/"
romney008 <- "http://mittromneycentral.com/speeches/2012-speeches/083012-romney-accepts-the-gop-nomination-in-tampa/"
romney009 <- "http://mittromneycentral.com/speeches/2012-speeches/081412-mitt-romneys-speech-in-chillicothe-ohio/"
romney010 <- "http://mittromneycentral.com/speeches/2012-speeches/081212-mitt-romney-paul-ryan-in-wisconsin/"
romney011 <- "http://mittromneycentral.com/speeches/2012-speeches/081112-mitt-romney-makes-his-vp-announcement/"
romney012 <- "http://mittromneycentral.com/speeches/2012-speeches/072912-mitt-romney-policy-speech-in-jerusalem/"
romney013 <- "http://mittromneycentral.com/speeches/2012-speeches/072412-remarks-at-the-vfw-national-convention/"
romney014 <- "http://mittromneycentral.com/speeches/2012-speeches/071112-speech-to-the-naacp-convention/"
romney015 <- "http://mittromneycentral.com/speeches/2012-speeches/062112-remarks-to-naleo-growing-opportunity-for-all-americans-2/"
romney016 <- "http://mittromneycentral.com/speeches/2012-speeches/060712-a-champion-for-free-enterprise-2/"
romney017 <- "http://mittromneycentral.com/speeches/2012-speeches/052312-remarks-on-education-a-chance-for-every-child-2/"
romney018 <- "http://mittromneycentral.com/speeches/2012-speeches/051512-prairie-fire-of-debt-speech/"
romney019 <- "http://mittromneycentral.com/speeches/2012-speeches/051212-liberty-university-commencement-address/"
romney020 <- "http://mittromneycentral.com/speeches/2012-speeches/042412-a-better-america-begins-tonight/"
romney021 <- "http://mittromneycentral.com/speeches/2012-speeches/041312-nra-convention-2012/"
romney022 <- "http://mittromneycentral.com/speeches/2012-speeches/040412-newspaper-association-of-america-remarks/"
romney023 <- "http://mittromneycentral.com/speeches/2012-speeches/040312-wisconsin-victory-speech/"
romney024 <- "http://mittromneycentral.com/speeches/2012-speeches/033012-freedom-and-opportunity/"
romney025 <- "http://mittromneycentral.com/speeches/2012-speeches/032012-illinois-victory-speech/"
romney026 <- "http://mittromneycentral.com/speeches/2012-speeches/031912-remarks-in-chicago-the-freedom-to-dream/"
romney027 <- "http://mittromneycentral.com/speeches/2012-speeches/030612-super-tuesday/"
romney028 <- "http://mittromneycentral.com/speeches/2012-speeches/022812-michiganarizona-victory-speech/"
romney029 <- "http://mittromneycentral.com/speeches/2012-speeches/022412-speech-at-the-detroit-economic-club/"
romney030 <- "http://mittromneycentral.com/speeches/2012-speeches/021012-%E2%80%93-cpac-2012/"
romney031 <- "http://mittromneycentral.com/speeches/2012-speeches/020412-nevada-caucus-victory-speech/"
romney032 <- "http://mittromneycentral.com/speeches/2012-speeches/013112-mitt-romneys-florida-primary-victory-speech/"
romney033 <- "http://mittromneycentral.com/speeches/2012-speeches/011012-mitt-romneys-new-hampshire-primary-victory-speech/"
romney034 <- "http://mittromneycentral.com/speeches/2011-speeches/120711-remarks-to-the-republican-jewish-coalition/"
romney035 <- "http://mittromneycentral.com/speeches/2011-speeches/110411-foreign-policy-remarks-at-americans-for-prosperitys-%e2%80%9cdefending-the-american-dream-summit%e2%80%9d/"
romney036 <- "http://mittromneycentral.com/speeches/2011-speeches/100811-values-voters-summit-2011/"
romney037 <- "http://mittromneycentral.com/speeches/2011-speeches/10072011-an-american-century/"
romney038 <- "http://mittromneycentral.com/speeches/090411-tea-party-express-nh/"
romney039 <- "http://mittromneycentral.com/speeches/090211-republican-national-hispanic-assembly/"
romney040 <- "http://mittromneycentral.com/speeches/083011-vfw-national-convention/"
romney041 <- "http://mittromneycentral.com/speeches/060311-faith-freedom-coalition/"
romney042 <- "http://mittromneycentral.com/speeches/060211-mitt-romney-announces-his-2012-run-for-president/"
romney043 <- "http://mittromneycentral.com/speeches/051211-%E2%80%93-mitt-romneys-michigan-presentation-on-healthcare-reform/"
romney044 <- "http://mittromneycentral.com/speeches/040211-republican-jewish-coalition-speech/"
romney045 <- "http://mittromneycentral.com/speeches/030511-carroll-county-lincoln-day-dinner/"
romney046 <- "http://mittromneycentral.com/speeches/021111-cpac-2011-believe-in-america/"
romney047 <- "http://mittromneycentral.com/speeches/mitt-romney-values-voters-summit-2010-09172010/"
romney048 <- "http://mittromneycentral.com/speeches/mitt-romney-5252010-annual-reagan-lecture/"
romney049 <- "http://mittromneycentral.com/speeches/mitt-romney-cpac-2010/"
romney050 <- "http://mittromneycentral.com/speeches/101909-2009-aipac-national-summit/"
romney051 <- "http://mittromneycentral.com/speeches/09192009-values-voter-summit-2009/"
romney052 <- "http://mittromneycentral.com/speeches/the-care-of-freedom/"
romney053 <- "http://mittromneycentral.com/speeches/nra-convention-2009/"
romney054 <- "http://mittromneycentral.com/speeches/033109-remarks-to-the-national-republican-senatorial-committee-spring-dinner/"
romney055 <- "http://mittromneycentral.com/speeches/cpac-2009/"
romney056 <- "http://mittromneycentral.com/speeches/013009-remarks-to-the-house-republican-conference-retreat/"
romney057 <- "http://mittromneycentral.com/speeches/rnc-2008-932008/"
romney058 <- "http://mittromneycentral.com/speeches/cpac-2008-romney-suspends-campaign/"
romney059 <- "http://mittromneycentral.com/speeches/2008-speeches/011508-romney-wins-the-michigan-primary/"
romney060 <- "http://mittromneycentral.com/speeches/faith-in-america/"
romney061 <- "http://mittromneycentral.com/speeches/101907-values-voters-summit-2007/"
romney062 <- "http://mittromneycentral.com/speeches/defending-the-american-dream-summit-10507/"
romney063 <- "http://mittromneycentral.com/speeches/092207-remarks-at-the-mackinac-republican-leadership-conference/"
romney064 <- "http://mittromneycentral.com/speeches/mitt-romney-%E2%80%93-regent-university-commencement-2007-%E2%80%93-05052007/"
romney065 <- "http://mittromneycentral.com/speeches/cpac-2007/"
romney066 <- "http://mittromneycentral.com/speeches/100506-liberty-sunday-address/"
romney067 <- "http://mittromneycentral.com/speeches/092206-values-voters-summit-2006/"
romney068 <- "http://mittromneycentral.com/speeches/090104-remarks-to-the-2004-republican-national-convention/"
##########################################################
# download texts
obama001 <- htmlToText(obama001)
obama002 <- htmlToText(obama002)
obama003 <- htmlToText(obama003)
obama004 <- htmlToText(obama004)
obama005 <- htmlToText(obama005)
obama006 <- htmlToText(obama006)
obama007 <- htmlToText(obama007)
obama008 <- htmlToText(obama008)
obama009 <- htmlToText(obama009)
obama010 <- htmlToText(obama010)
obama011 <- htmlToText(obama011)
obama012 <- htmlToText(obama012)
obama013 <- htmlToText(obama013)
obama014 <- htmlToText(obama014)
obama015 <- htmlToText(obama015)
obama016 <- htmlToText(obama016)
obama017 <- htmlToText(obama017)
obama018 <- htmlToText(obama018)
obama019 <- htmlToText(obama019)
obama020 <- htmlToText(obama020)
obama021 <- htmlToText(obama021)
obama022 <- htmlToText(obama022)
obama023 <- htmlToText(obama023)
obama024 <- htmlToText(obama024)
obama025 <- htmlToText(obama025)
obama026 <- htmlToText(obama026)
obama027 <- htmlToText(obama027)
obama028 <- htmlToText(obama028)
obama029 <- htmlToText(obama029)
obama030 <- htmlToText(obama030)
obama031 <- htmlToText(obama031)
obama032 <- htmlToText(obama032)
obama033 <- htmlToText(obama033)
obama034 <- htmlToText(obama034)
obama035 <- htmlToText(obama035)
obama036 <- htmlToText(obama036)
obama037 <- htmlToText(obama037)
obama038 <- htmlToText(obama038)
obama039 <- htmlToText(obama039)
obama040 <- htmlToText(obama040)
obama041 <- htmlToText(obama041)
obama042 <- htmlToText(obama042)
obama043 <- htmlToText(obama043)
obama044 <- htmlToText(obama044)
obama045 <- htmlToText(obama045)
obama046 <- htmlToText(obama046)
obama047 <- htmlToText(obama047)
obama048 <- htmlToText(obama048)
obama049 <- htmlToText(obama049)
obama050 <- htmlToText(obama050)
obama051 <- htmlToText(obama051)
obama052 <- htmlToText(obama052)
obama053 <- htmlToText(obama053)
obama054 <- htmlToText(obama054)
obama055 <- htmlToText(obama055)
obama056 <- htmlToText(obama056)
obama057 <- htmlToText(obama057)
obama058 <- htmlToText(obama058)
obama059 <- htmlToText(obama059)
obama060 <- htmlToText(obama060)
obama061 <- htmlToText(obama061)
obama062 <- htmlToText(obama062)
obama063 <- htmlToText(obama063)
obama064 <- htmlToText(obama064)
obama065 <- htmlToText(obama065)
obama066 <- htmlToText(obama066)
obama067 <- htmlToText(obama067)
obama068 <- htmlToText(obama068)
obama069 <- htmlToText(obama069)
obama070 <- htmlToText(obama070)
obama071 <- htmlToText(obama071)
obama072 <- htmlToText(obama072)
obama073 <- htmlToText(obama073)
obama074 <- htmlToText(obama074)
obama075 <- htmlToText(obama075)
obama076 <- htmlToText(obama076)
obama077 <- htmlToText(obama077)
obama078 <- htmlToText(obama078)
obama079 <- htmlToText(obama079)
obama080 <- htmlToText(obama080)
obama081 <- htmlToText(obama081)
obama082 <- htmlToText(obama082)
obama083 <- htmlToText(obama083)
obama084 <- htmlToText(obama084)
obama085 <- htmlToText(obama085)
obama086 <- htmlToText(obama086)
obama087 <- htmlToText(obama087)
obama088 <- htmlToText(obama088)
obama089 <- htmlToText(obama089)
obama090 <- htmlToText(obama090)
obama091 <- htmlToText(obama091)
obama092 <- htmlToText(obama092)
obama093 <- htmlToText(obama093)
obama094 <- htmlToText(obama094)
obama095 <- htmlToText(obama095)
obama096 <- htmlToText(obama096)
obama097 <- htmlToText(obama097)
obama098 <- htmlToText(obama098)
obama099 <- htmlToText(obama099)
obama100 <- htmlToText(obama100)
obama101 <- htmlToText(obama101)
obama102 <- htmlToText(obama102)
romney001 <- htmlToText(romney001)
romney002 <- htmlToText(romney002)
romney003 <- htmlToText(romney003)
romney004 <- htmlToText(romney004)
romney005 <- htmlToText(romney005)
romney006 <- htmlToText(romney006)
romney007 <- htmlToText(romney007)
romney008 <- htmlToText(romney008)
romney009 <- htmlToText(romney009)
romney010 <- htmlToText(romney010)
romney011 <- htmlToText(romney011)
romney012 <- htmlToText(romney012)
romney013 <- htmlToText(romney013)
romney014 <- htmlToText(romney014)
romney015 <- htmlToText(romney015)
romney016 <- htmlToText(romney016)
romney017 <- htmlToText(romney017)
romney018 <- htmlToText(romney018)
romney019 <- htmlToText(romney019)
romney020 <- htmlToText(romney020)
romney021 <- htmlToText(romney021)
romney022 <- htmlToText(romney022)
romney023 <- htmlToText(romney023)
romney024 <- htmlToText(romney024)
romney025 <- htmlToText(romney025)
romney026 <- htmlToText(romney026)
romney027 <- htmlToText(romney027)
romney028 <- htmlToText(romney028)
romney029 <- htmlToText(romney029)
romney030 <- htmlToText(romney030)
romney031 <- htmlToText(romney031)
romney032 <- htmlToText(romney032)
romney033 <- htmlToText(romney033)
romney034 <- htmlToText(romney034)
romney035 <- htmlToText(romney035)
romney036 <- htmlToText(romney036)
romney037 <- htmlToText(romney037)
romney038 <- htmlToText(romney038)
romney039 <- htmlToText(romney039)
romney040 <- htmlToText(romney040)
romney041 <- htmlToText(romney041)
romney042 <- htmlToText(romney042)
romney043 <- htmlToText(romney043)
romney044 <- htmlToText(romney044)
romney045 <- htmlToText(romney045)
romney046 <- htmlToText(romney046)
romney047 <- htmlToText(romney047)
romney048 <- htmlToText(romney048)
romney049 <- htmlToText(romney049)
romney050 <- htmlToText(romney050)
romney051 <- htmlToText(romney051)
romney052 <- htmlToText(romney052)
romney053 <- htmlToText(romney053)
romney054 <- htmlToText(romney054)
romney055 <- htmlToText(romney055)
romney056 <- htmlToText(romney056)
romney057 <- htmlToText(romney057)
romney058 <- htmlToText(romney058)
romney059 <- htmlToText(romney059)
romney060 <- htmlToText(romney060)
romney061 <- htmlToText(romney061)
romney062 <- htmlToText(romney062)
romney063 <- htmlToText(romney063)
romney064 <- htmlToText(romney064)
romney065 <- htmlToText(romney065)
romney066 <- htmlToText(romney066)
romney067 <- htmlToText(romney067)
romney068 <- htmlToText(romney068)
##########################################################
# clean texts
#obama001 <- gsub(".*My fellow citizens", "", obama001)
#obama002 <- gsub(".*If there is anyone out", "", obama002)
##########################################################
# write first file to file
#writeLines(obama001, "C:\\03-MyProjects\\TextMining/obama001.txt")
# create list with speeches
speeches <- list(obama001, obama002, obama003, obama004, obama005, obama006,
  obama007, obama008, obama009, obama010, obama011, obama012, obama013, obama014,
  obama015, obama016, obama017, obama018, obama019, obama020, obama021, obama022,
  obama023, obama024, obama025, obama026, obama027, obama028, obama029, obama030,
  obama031, obama032, obama033, obama034, obama035, obama036, obama037, obama038,
  obama039, obama040, obama041, obama042, obama043, obama044, obama045, obama046,
  obama047, obama048, obama049, obama050, obama051, obama052, obama053, obama054,
  obama055, obama056, obama057, obama058, obama059, obama060, obama061, obama062,
  obama063, obama064, obama065, obama066, obama067, obama068, obama069, obama070,
  obama071, obama072, obama073, obama074, obama075, obama076, obama077, obama078,
  obama079, obama080, obama081, obama082, obama083, obama084, obama085, obama086,
  obama087, obama088, obama089, obama090, obama091, obama092, obama093, obama094,
  obama095, obama096, obama097, obama098, obama099, obama100, obama101, obama102,
  romney001, romney002, romney003, romney004, romney005, romney006, romney007,
  romney008, romney009, romney010, romney011, romney012, romney013, romney014,
  romney015, romney016, romney017, romney018, romney019, romney020, romney021,
  romney022, romney023, romney024, romney025, romney026, romney027, romney028,
  romney029, romney030, romney031, romney032, romney033, romney034, romney035,
  romney036, romney037, romney038, romney039, romney040, romney041, romney042,
  romney043, romney044, romney045, romney046, romney047, romney048, romney049,
  romney050, romney051, romney052, romney053, romney054, romney055, romney056,
  romney057, romney058, romney059, romney060, romney061, romney062, romney063,
  romney064, romney065, romney066, romney067, romney068)
  # add anmes to speeches
names(speeches) <- c("obama001", "obama002", "obama003", "obama004", "obama005", "obama006",
  "obama007", "obama008", "obama009", "obama010", "obama011", "obama012", "obama013", "obama014",
  "obama015", "obama016", "obama017", "obama018", "obama019", "obama020", "obama021", "obama022",
  "obama023", "obama024", "obama025", "obama026", "obama027", "obama028", "obama029", "obama030",
  "obama031", "obama032", "obama033", "obama034", "obama035", "obama036", "obama037", "obama038",
  "obama039", "obama040", "obama041", "obama042", "obama043", "obama044", "obama045", "obama046",
  "obama047", "obama048", "obama049", "obama050", "obama051", "obama052", "obama053", "obama054",
  "obama055", "obama056", "obama057", "obama058", "obama059", "obama060", "obama061", "obama062",
  "obama063", "obama064", "obama065", "obama066", "obama067", "obama068", "obama069", "obama070",
  "obama071", "obama072", "obama073", "obama074", "obama075", "obama076", "obama077", "obama078",
  "obama079", "obama080", "obama081", "obama082", "obama083", "obama084", "obama085", "obama086",
  "obama087", "obama088", "obama089", "obama090", "obama091", "obama092", "obama093", "obama094",
  "obama095", "obama096", "obama097", "obama098", "obama099", "obama100", "obama101", "obama102",
  "romney001", "romney002", "romney003", "romney004", "romney005", "romney006", "romney007",
  "romney008", "romney009", "romney010", "romney011", "romney012", "romney013", "romney014",
  "romney015", "romney016", "romney017", "romney018", "romney019", "romney020", "romney021",
  "romney022", "romney023", "romney024", "romney025", "romney026", "romney027", "romney028",
  "romney029", "romney030", "romney031", "romney032", "romney033", "romney034", "romney035",
  "romney036", "romney037", "romney038", "romney039", "romney040", "romney041", "romney042",
  "romney043", "romney044", "romney045", "romney046", "romney047", "romney048", "romney049",
  "romney050", "romney051", "romney052", "romney053", "romney054", "romney055", "romney056",
  "romney057", "romney058", "romney059", "romney060", "romney061", "romney062", "romney063",
  "romney064", "romney065", "romney066", "romney067", "romney068")
# clean speeches
speeches <- lapply(speeches, function(x){
  x <- gsub("FACTS ABOUT WHAT.*", "", x)
} )
# write speeches to file
lapply(seq_along(speeches), function(i)writeLines(text = unlist(speeches[i]),
    con = paste("C:\\03-MyProjects\\TextMining\\speeches/", names(speeches)[i],".txt", sep = "")))


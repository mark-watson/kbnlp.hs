-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the AGPL version 3 license.

module CompanyNames (companyNamesOneWord, companyNamesTwoWords, companyNamesThreeWords) where

import qualified Data.Set as S

companyNamesOneWord = S.fromList ["IBM", "Ford", "GM", "Google", "Microsoft", "3M", "Adobe", "AES", "Aetna", "AFLAC", "Agilent", "Akamai", "Alcoa", "Allegheny", "Allstate", "Altera", "Amazon", "Apple", "AT&T", "Autodesk", "Avon", "Boeing", "Broadcom", "Chevron", "CIGNA", "Cisco", "Citigroup", "Citrix", "Clorox", "Colgate-Palmolive", "Comcast", "ConocoPhillips", "Corning", "Costco", "Dell", "DeVry", "DIRECTV", "eBay", "FedEx", "GameStop", "Gannett", "Goodyear", "Halliburton", "Hasbro", "Heinz", "Hewlett-Packard", "Honeywell", "Hormel", "Humana", "Intel", "Intuit", "Kellogg", "Kimco", "Kohl", "Kraft", "Marriott", "Mastercard", "Mattel", "McAfee", "McGraw-Hill", "Merck", "MetLife", "Microsoft", "Monsanto", "NIKE", "Nike", "Novell", "Nvidia", "Oracle", "Prudential", "QLogic", "QUALCOMM", "Quest", "Raytheon", "Rockwell", "Safeway", "Salesforce", "SanDisk", "Schlumberger", "Sears", "Starbucks", "Verisign", "Volkswagen", "Wal-Mart", "Walgreen", "WellPoint", "Winnebago", "Xerox", "Yahoo"]
companyNamesTwoWords = S.fromList ["General Motors", "American Express", "Analog Devices", "Best Buy", "Boston Scientific", "Bristol-Myers Squibb", "Campbell Soup", "Coca Cola", "Dow Chemical", "Du Pont", "Exxon Mobil", "Ford Motor", "General Electric", "General Mills", "Goldman Sachs", "Morgan Stanley", "Office Depot", "Philip Morris", "Red Hat", "Sempra Energy", "Southwest Airlines", "Sun Microsystems", "Time Warner", "Wells Fargo", "Washington Post", "BBC Radio", "Walt Disney", "Associated Press", "MIT Press", "Warner Bros.", "General Motors", "Daily Telegraph", "Clarendon Press", "General Electric", "Paramount Pictures", "USA Today", "Apple Inc.", "Greenwood Press", "Sun Microsystems", "Entertainment Weekly", "Apple Computer", "Chicago Tribune", "Blackwell Publishing", "Boston Globe", "Development Bank", "Time Warner", "Texas Instruments", "Chicago Sun-Times", "Norfolk Southern", "Southern California", "CRC Press", "Best Music", "Tuttle Publishing", "American Airlines", "United Airlines", "Sunday Times", "University Press", "McDonnell Douglas", "Le Monde", "Warner Bros", "Union Pacific", "Bell Labs", "American general", "International Bank", "Westview Press", "Lockheed Martin", "British Airways", "Febrero co", "Electronic Arts", "Digital Equipment", "General Dynamics", "SUNY Press", "Southern Africa", "Nike Inc.", "Jerusalem Post", "Washington Times", "Academic Press", "CBS News", "Scarecrow Press", "Viking Press", "Siemens AG", "LA Times", "Daily News", "Silicon Graphics", "County Commissioners", "Popular Music", "Japan Times", "Free Press", "Seattle Times", "Omnibus Press", "North Fork", "BBC Television", "Toronto Star", "Houston Chronicle", "Adobe Systems", "Oracle Corporation", "Osprey Publishing", "BAE Systems", "NBC Universal", "Kessinger Publishing", "Evening Standard", "Microsoft Corporation", "CBC Television", "Burger King", "Bell Laboratories", "Data General", "Cisco Systems", "Heavy Metal", "TSR Inc.", "Eastman Kodak", "Guardian Unlimited", "Irish Times", "Pergamon Press", "News Corporation", "Natural Resources", "World Scientific", "National Trust", "Judaica Press", "Paulist Press", "Square Co.", "Continental Airlines", "Air Canada", "CBC Radio", "Northrop Grumman", "America Online", "State Street", "Belknap Press", "Hewlett Packard", "Harper's Weekly", "Northwest Airlines", "Coca-Cola Company", "Southwest Airlines", "Boydell Press", "Palm Beach", "Garland Publishing", "Wells Fargo", "Prentice-Hall Inc.", "Western Union", "Miami Herald", "RAND Corporation", "Daimler AG", "London Gazette", "American Express", "LA Weekly", "Country Music", "Philadelphia Inquirer", "Monsanto Company", "US Airways", "Chosen Freeholders", "American Standard", "InterVarsity Press", "Pluto Press", "Macmillan Company", "Granada Television", "British Telecom", "National Semiconductor", "Baltimore Sun", "Time Inc.", "Publishers Weekly", "Worshipful Company", "BBC Wales", "Thames Television", "Fairchild Semiconductor", "General Mills", "Black Hills", "PC World", "Sony Music", "Abingdon Press", "Electronic Music", "Coca Cola", "Paris Review", "CBS Radio", "Burroughs Corporation", "London Review", "Roxy Music", "Human Rights", "Chrysler Corporation", "Polity Press", "Arcadia Publishing", "NCR Corporation", "Penguin Group", "Gale Group", "IARC Group", "Review Award", "Goldman Sachs", "Singapore Airlines", "Remington Rand", "Dish Network", "Intel Corporation", "Hitachi Ltd.", "Kraft Foods", "Deutsche Bank", "Warner Brothers", "Lehman Brothers", "NYU Press", "Hackett Publishing", "Electric Company", "Penguin Press", "Morgan Stanley", "American Music", "American General", "DC Comics", "El Paso", "Aurum Press", "Douglas Aircraft", "London Corporation", "Southern Europe", "Volkswagen Group", "El Mundo", "Readers Digest", "Atari Inc.", "Merrill Lynch", "Universal Time", "U.S. Steel", "Japan Airlines", "Nazi Germany", "Financial Times", "Eli Lilly", "Safeway Inc.", "BBC America", "Boston Herald", "Home Depot", "MCI Inc.", "Russell Group", "Rio Group", "Frankfurter Allgemeine", "Liturgical Press", "NBC Radio", "Sinauer Associates", "BT Group", "Citadel Press", "Modern Music", "Rockwell International", "Daily Herald", "Plenum Press", "Denver Post", "Rio Tinto", "J.P. Morgan", "SAP AG", "Beacon Press", "Daily Star", "Folk Music", "Best Buy", "Turner Broadcasting", "World Music", "CBS Corporation", "New Music", "Humanities Press", "Continental Europe", "National Post", "Early Music", "Curzon Press", "Union Carbide", "Southern Italy", "Broadview Press", "NATO's Partnership", "World Bank", "Virgin Media", "Sutton Publishing", "Belfast Telegraph", "Weinstein Company", "Bilderberg Group", "JHU Press", "Air France", "Alaska Airlines", "Warner Communications", "International Trade", "Martin Marietta", "Thales Group", "Dexter's Laboratory", "Yorkshire Television", "Taipei Times", "Palm Inc.", "Southern Ontario", "Analog Devices", "Fox Network", "ABC Radio", "Marconi Company", "National Bank", "Southern Rhodesia", "Sperry Corporation", "Barclays Bank", "Raven Software", "AMS Press", "Grove Press", "Ralph Lauren", "Abbeville Press", "Lucent Technologies", "Die Welt", "Monsters Inc.", "American Broadcasting", "Exxon Mobil", "IEEE Press", "Mario Bros", "UBS AG", "Slave Trade", "Rolls-Royce plc", "David Steel", "South Vietnam", "Atlanta Journal", "Catalan Company", "Clear Channel", "Rotten Tomatoes", "Human Resources", "News Corp.", "Hearst Corporation", "NZ Herald", "Computer Sciences", "Hogarth Press", "Cathay Pacific", "Canadian Press", "Boeing Company", "Iberia Airlines", "ACM Press", "France Telecom", "La Repubblica", "Thievery Corporation", "Central Bank", "Cray Research", "Gulf Oil", "Miramax Films", "Sveriges Riksbank", "Irish Independent", "Nissan Motors", "Southern France", "J.C. Penney", "Gladstone Publishing", "Altria Group", "Freedom Press", "du Pont", "Haworth Press", "Kia Motors", "Iron Mountain", "ING Group", "Guilford Press", "Asiana Airlines", "Sky News", "Business Wire", "Canon Inc.", "Arno Press", "Minute Maid", "Film Music", "China Airlines", "Total S.A.", "Commonwealth Bank", "Dow Jones", "Western Digital", "MTV Europe", "Portia Group", "Bibliographic Resources", "Washington Mutual", "Avon Products", "Norsk Hydro", "Ty Inc.", "Arcade Publishing", "Levi Strauss", "Continental AG", "ADV Films", "Bloomsbury Group", "Tribune Company", "Hilton Hotels", "Imperial Airways", "Gramophone Company", "Hudson Ltd.", "Berkshire Hathaway", "Imperial Oil", "Deutsche Telekom", "Atari Corp.", "Apogee Software", "Phaidon Press", "Historical Capital", "Addison-Wesley Publishing", "Free Software", "Thames Bank", "Arizona Republic", "World Trade", "Left Bank", "Sea Venture", "Development Corporation", "Liberty Media", "Fox network", "Cavendish Laboratory", "Fannie Mae", "MCI Communications", "Red Bank", "Hartford Courant", "Vivendi Universal", "Macmillan Publishing", "Thomson SA", "BHP Billiton", "Canberra Times", "Philip Morris", "Sveriges Television", "Orange SA", "United U.S.", "Three's Company", "Staples Inc.", "Wal-Mart stores", "Cairns Group", "Freddie Mac", "Dell Inc.", "Puma AG", "Yahoo! Music", "SIAM Journal", "Information Systems", "Hershey Company", "Fortress Press", "Working Group", "McGill-Queen's Press", "Wang Laboratories", "Southern Asia", "Thorndike Press", "Christian Dior", "Review Awards", "Water Resources", "Boston Review", "UPS Airlines", "Pearson PLC", "American Motors", "Southern India", "Subterranean Press", "Phoenix Press", "ION Television", "Montreal Gazette", "Investment Bank", "Abbott Laboratories", "Leamington Spa", "Parker Brothers", "Trafford Publishing", "Control Data", "Du Pont", "Traditional Music", "Bell Atlantic", "MAN AG", "Tandy Corporation", "Moody Press", "Mainstream Publishing", "SCO Group", "Le monde", "Flerov Laboratory", "Scania AB", "Sears Roebuck", "Classical Music", "Duke Energy", "RIA Novosti", "Broadcast Music", "Rockwell Collins", "Birlinn Ltd.", "Indian Head", "Danmarks Radio", "Foreign Trade", "International Fund", "Volkswagen AG", "Knight Ridder", "Publishers Inc.", "United Technologies", "Ashgate Publishing", "Southern Maryland", "Westminster Press", "Wildside Press", "Southern Sudan", "CNN International", "Getty Oil", "Cycorp Inc.", "Lulu Press", "MacMillan Company", "Gemstone Publishing", "Popular Press", "Delta Airlines", "WarioWare Inc.", "Private Eye", "Sprint Nextel", "Macintosh LC", "Bad Company", "Moog Music", "South Bank", "Parthenon Press", "LG Electronics", "ITT Corporation", "Amadeus Press", "Edward Jones", "PepsiCo Inc.", "Chicago Times", "Columbia Records", "Foster's Group", "Paris Match", "Indian Music", "Gnome Press", "Southern Netherlands", "Natural Trust", "Contemporary Music", "Apollo Computer", "Atari Corporation", "Merck KGaA", "Bombardment Group", "Carlsberg Laboratory", "ATA Airlines", "Souvenir Press", "Mario Bros.", "Lucasfilm Ltd.", "ABC Television", "Bloomsbury Publishing", "Canada Limited", "Broadcasting Corporation", "Dornier GmbH", "London Journal", "Lincoln Laboratory", "Shell Oil", "Evolution Publishing", "Space Systems", "Southern England", "SVS Press", "Borders Group", "Victoria's Secret", "SCM Press", "United Artists", "Park Place", "Wellcome Trust", "Greenwood Publishing", "Embedded Systems", "National Petroleum", "Bomb Group", "Weekly Standard", "Cygnus Solutions", "Lockheed Corporation", "Bantam Press", "Universal Pictures", "Overlook Press", "Southern Russia", "IDW Publishing", "Fuji Television", "Qatar Airways", "Calgary Herald", "Dow Chemical", "Harvill Press", "Mongoose Publishing", "Dan River", "Xerox Corporation", "BAA Limited", "Yamaha Motor", "Aon Corporation", "Applied Materials", "FM Radio", "Indian Airlines", "Lyons Press", "Anglia Television", "LAN Airlines", "Intervarsity Press", "Virtual Laboratory", "Freescale Semiconductor", "Praeger Press", "BBC Worldwide", "Samsung Electronics", "Schiffer Publishing", "Heineken International", "General Instrument", "Public Radio", "Gran Turismo", "Mitsubishi Motors", "Lutterworth Press", "Dance Music", "Arab Bank", "Pop Music", "St. Regis", "Tate Publishing", "JPMorgan Chase", "Sveriges Radio", "PalmSource Inc.", "Investor AB", "Biograph Company", "Raven Press", "Universal Studios", "Caterpillar Inc.", "Ahmad Sa", "Golden West", "H&R Block", "RTL Group", "Straits Times", "Handmade Films", "Delco Electronics", "Barclays Banks", "Ars Technica", "NPR Music", "Lotus Software", "Dogger Bank", "Occidental Petroleum", "Frontier Airlines", "DuMont Television", "Star Tribune", "Verizon Wireless", "Carnegie Corporation", "UCL Press", "Volvo AB", "Lincoln National", "Routledge Press", "Equipment Corporation", "Max Factor", "Tyson Foods", "Virgin Group", "Infinity Inc.", "NRC Handelsblad", "Tata Motors", "Landor Associates", "Paramount Television", "Night Music", "Ferrero SpA", "Fiji Times", "Columbus Dispatch", "Right Bank", "MIT Laboratory", "Tata Group", "Friden Inc.", "Cartier SA", "Bechtel Corporation", "Turkish Airlines", "Humana Press", "Paternoster Press", "His Music", "Magna International", "Games Workshop", "Collier's Weekly", "die Welt", "ATI Technologies", "Capitol Records", "Minneapolis Tribune", "MCI WorldCom", "Kellogg Company", "Acme Corporation", "Western Music", "Hanson plc", "Lindisfarne Press", "Engineering Ltd", "Associated TeleVision", "National Capital", "Ebury Press", "BBC Scotland", "News Limited", "Emirates Airline", "Native Americans", "Lotus Cars", "Atlantic Records", "DK Publishing", "Rock Music", "Mars Incorporated", "Cadbury plc", "SBC Communications", "Jet Airways", "Metallurgical Laboratory", "Focal Press", "Ayer Publishing", "Danielle Steel", "Blackstone Group", "Olympia Press", "Mozilla Corporation", "Bain Capital", "Caledonian Company", "Teaching Company", "Wayne Corporation", "Muscovy Company", "Public Works", "Vlaamse Radio", "Netscape Communications", "Weird Tales", "Juniper Networks", "JP Morgan", "Lattice Semiconductor", "Groupe Bull", "NFL Films", "SourceForge Inc.", "Commercial Appeal", "LL.D. F.S.A.", "Phoenix Technologies", "Le Journal", "Aer Lingus", "El Universal", "Blue Steel", "Air Liquide", "Timber Press", "Domino's Pizza", "Southern Louisiana", "Origin Systems", "Wildlife Service", "Pan Am", "Killeen Television", "Hound Group", "Google Inc.", "Voyageur Press", "Old-Time Radio", "Digital Press", "Nutting Associates", "Greatest Films", "MOS Technologies", "Pacifica Radio", "Asahi Shimbun", "Capital Airlines", "Us Weekly", "Loki Software", "Kenya Airways", "Eastern Airlines", "Popular Electronics", "Ralston Purina", "In Music", "JetBlue Airways", "Engineering Laboratory", "TwoMorrows Publishing", "Boots Group", "Spirit Airlines", "Dimension Films", "Fair Trading", "Mohawk Industries", "Dublin Corporation", "RKO Radio", "IBM Corporation", "AirTran Airways", "HD Radio", "Ethiopian Airlines", "Ace Hardware", "Electronic Systems", "Deutsche Werke", "Baroque Music", "Microsoft Press", "Dolby Laboratories", "Marconi plc", "Chicago Herald", "Movie Gallery", "Stainless Steel", "XM Radio", "Canonical Ltd.", "Bombardier Inc.", "Lloyds TSB", "Suncor Energy", "Nash Motors", "IEEE Software", "Chamber Music", "Bloomberg L.P.", "Credit Suisse", "SRI International", "Aquarian Press", "Eaton Corporation", "AltaMira Press", "Deutsche Bundesbank", "LSI Logic", "UNESCO Publishing", "Weeb Ewbank", "HMV Group", "Be Inc.", "Florida Times", "Ford Australia", "Universal Music", "Island Press", "Marvel Comics", "Mars Inc.", "Southern Lebanon", "Alliant Techsystems", "Chrysler LLC", "Paladin Press", "New Press", "Lend Lease", "Columbia Pictures", "USA TODAY", "Water Music", "Rolls-Royce Limited", "Canada Dry", "Crowood Press", "Wayne Enterprises", "Austrian Airlines", "ConAgra Foods", "Vauxhall Motors", "Delacorte Press", "Complex Systems", "Fidelity Investments", "Tyrell Corporation", "Nation's Capital", "Asahi Breweries", "Massachusetts Review", "Neural Networks", "Hudson Ltd", "Vanguard Group", "Harvester Press", "Hoechst AG", "Ignatius Press", "Brussels Airlines", "BNP Paribas", "Aerospace Corporation", "EMC Corporation", "RTL Television", "Valve Corporation", "Detroit Diesel", "Presidio Press", "Telcordia Technologies", "Regnery Publishing", "PS Publishing", "International Journal", "NPD Group", "Macmillan Co.", "Aircraft Corporation", "Power Corporation", "Macmillan Press", "Bitstream Inc.", "Zenith Press", "Husky Energy", "Open Systems", "Phillips Petroleum", "Midland Publishing", "Harrah's Entertainment", "CABI Publishing", "Amateur Radio", "Al-Ahram Weekly", "NTT DoCoMo", "Tesla Motors", "Green Bank", "Folk Metal", "BBC Canada", "Geoscience Press", "Grolier Incorporated", "Banca d'Italia", "TNT N.V.", "People Weekly", "Malaysia Airlines", "Philtrum Press", "Telos Press", "Open Group", "Bethlehem Steel", "MAN SE", "Mercury News", "ARM Limited", "Vancouver Sun", "FOX network", "MIPS Technologies", "New Worlds", "Disinformation Company", "Gauntlet Press", "Aral AG", "US Weekly", "A&M Records", "TV4 AB", "Vanguard Press", "R.H. Donnelley", "E.W. Scripps", "Science Fiction", "Database Systems", "Super Junior", "Island Records", "Eternal Music", "International Paper", "Public Television", "Southern Oregon", "3Com Corporation", "Kyodo News", "Stagecoach Group", "Chevron Corporation", "Mattel Inc.", "News International", "Dell Publishing", "Soviet Russia", "Kadena AB", "MRC Laboratory", "Network Associates", "Wallflower Press", "Blue Bell", "Japan Tobacco", "Monotype Corporation", "Bobbs-Merrill Company", "Intuit Inc.", "Power Electronics", "Virgin Records", "Mega Inc.", "ASM Press", "Ancient Greek", "Sinclair Oil", "Space Corporation", "New Jersey", "Dana International", "Wadsworth Publishing", "RCA Records", "MITRE Corporation", "Pennsylvania Railroad", "Liz Claiborne", "Cox Communications", "Enron Corporation", "Epoch Times", "Barings Bank", "Total Film", "Xlibris Corporation", "Korean Air", "ABN AMRO", "Penn Central", "DuPont Company", "State Capital", "Norton Simon", "Herald Press", "Dassault Systemes", "SkyWest Airlines", "Abacus Software", "Ace Radio", "Denham Group", "Flinders Group", "Sacred Music", "Frankland Group", "Philadelphia Bulletin", "Rover Company", "MTV Networks", "Radiation Laboratory", "Athlone Press", "Consultative Group", "Cardinal Health", "Eiffel Software", "Southern Illinois", "BBC Two", "British Music", "Living Resources", "Seattle Weekly", "Copa Airlines", "DIANE Publishing", "Booz Allen", "Broadman Press", "Specialty Press", "Creative Labs", "Mitsubishi Electric", "Avalon Hill", "NATO Partnership", "MCA Inc.", "Mountain Music", "Waveland Press", "BiblioBazaar LLC", "Howick Group", "Bath Spa", "Ambrosia Software", "Ruth's Chris", "Arkham House", "ARM Holdings", "Queen's Music", "Best Films", "GNU Radio", "Ringling Bros.", "Pullman Company", "Oliphants Ltd.", "Visa Inc.", "American can", "Johnson Controls", "Computer Laboratory", "Short Brothers", "Campbell Soup", "Tandem Computers", "Shugart Associates", "Waste management", "Bell Canada", "Fe Co", "Resorts Company", "Barclays plc", "Pacific Crest", "Schlumberger Limited", "Nimbus Publishing", "Pharaonic Egypt", "Tyndale Press", "Playboy Enterprises", "Vueling Airlines", "Audi AG", "Perseus Publishing", "Rand Corporation", "Soncino Press", "KM Group", "El Pais", "NEC Corporation", "Orlando Sentinel", "General Foods", "Warner Music", "Bayer AG", "Ansett Australia", "Computer Associates", "Bangemall Group", "MICRA Inc.", "Bear Stearns", "Irish Music", "Trans-Canada Airlines", "West Publishing", "Northern Rock", "Wonderwall Music", "Bristol-Myers Squibb", "Mayfield Publishing", "Valero Energy", "Woody Press", "Quarry Bank", "NXP Semiconductors", "Sudan Airways", "Sony Corporation", "Quintet Publishing", "Aldine Press", "Dairy Queen", "Publisher Inc.", "Ames Laboratory", "Gem Trade", "Polar Music", "Local Group", "Edison Trust", "Pernod Ricard", "Hawaiian Airlines"]
companyNamesThreeWords = S.fromList ["Bank of America", "Procter & Gamble", "New York Times", "Oxford University Press", "Cambridge University Press", "Harvard University Press", "American Broadcasting Company", "Princeton University Press", "Columbia University Press", "Yale University Press", "Los Angeles Times", "Walt Disney Company", "Ford Motor Company", "Wall Street Journal", "Cornell University Press", "Indiana University Press", "Stanford University Press", "San Francisco Chronicle", "Fox Broadcasting Company", "International Herald Tribune", "National Public Radio", "Hudson's Bay Company", "Houghton Mifflin Company", "Digital Equipment Corporation", "International Business Machines", "East India Company", "Sydney Morning Herald", "Da Capo Press", "New York Review", "Bank of England", "Greenwood Publishing Group", "London Stock Exchange", "Duke University Press", "Orion Publishing Group", "Canadian Broadcasting Corporation", "Australian Broadcasting Corporation", "Manchester University Press", "Rutgers University Press", "Delta Air Lines", "Random House Inc.", "Edinburgh University Press", "Barnes & Noble", "New York Post", "Canadian Pacific Railway", "Super Mario Bros.", "Stars and Stripes", "Procter & Gamble", "Christian Science Monitor", "Bank of America", "New England Journal", "Royal Shakespeare Company", "Globe and Mail", "Northwestern University Press", "General Electric Company", "Naval Institute Press", "Temple University Press", "Robert Appleton Company", "Dover Publications Inc.", "Jet Propulsion Laboratory", "Super Smash Bros", "International Finance Corporation", "Comic Book Resources", "Warner Music Group", "New York Herald", "Control Data Corporation", "Advanced Micro Devices", "Wesleyan University Press", "Nuclear Suppliers Group", "Trans World Airlines", "British Broadcasting Corporation", "World Bank Group", "Bell Telephone Laboratories", "North West Company", "New York Tribune", "National Physical Laboratory", "British Museum Press", "United Parcel Service", "Robert Bosch GmbH", "Nintendo of America", "New Zealand Herald", "Johnson & Johnson", "Argonne National Laboratory", "Syracuse University Press", "South End Press", "World Wide Fund", "Smithsonian Institution Press", "Times of India", "Atlantic Slave Trade", "Inter-American Development Bank", "Deere & Company", "Angiosperm Phylogeny Group", "St Martin's Press", "St. Petersburg Times", "Shell Oil Company", "Caves Books Ltd.", "Universum Film AG", "Eastern Air Lines", "Canadian National Railway", "Corriere della Sera", "IBM Personal Computer", "XM Satellite Radio", "Vega Science Trust", "Sirius Satellite Radio", "New York Journal", "Apple Computer Inc.", "Detroit Free Press", "Netscape Communications Corporation", "Turner Network Television", "Ernst & Young", "Caribbean Development Bank", "Melbourne University Press", "Brown and Co.", "Cable & Wireless", "Electronic Data Systems", "Justice League Unlimited", "Westinghouse Electric Corporation", "Brookhaven National Laboratory", "Bank of Sweden", "Liverpool University Press", "London Weekend Television", "North Point Press", "British Aircraft Corporation", "Pearson Education Inc.", "Metro Goldwyn Mayer", "Kyle Cathie Limited", "Hal Leonard Corporation", "NSU Motorenwerke AG", "Standard Oil Company", "General Motors Corporation", "Houghton Mifflin Co.", "Ten Speed Press", "Virgin Atlantic Airways", "Knights of Columbus", "Super Mario Bros", "Merck & Co.", "Sandia National Laboratories", "Bank of Scotland", "National Academy Press", "Rio Tinto Group", "Pan American Airways", "Bristol Aeroplane Company", "Harcourt Brace Jovanovich", "Mercer University Press", "Carl Zeiss AG", "AOL Time Warner", "Father and Son", "Chicago University Press", "Lawrence Erlbaum Associates", "National Broadcasting Company", "20th Century Fox", "Earth Metrics Inc.", "Edwin Mellen Press", "McGraw-Hill Book Company", "MIT Lincoln Laboratory", "Carolina Academic Press", "Open University Press", "B. Eerdmans Publishing", "Naval Research Laboratory", "Coachella Valley Music", "Standard & Poor's", "McFarland & Company", "Bank of China", "Sky Television plc", "Johns Hopkins Press", "Eckert-Mauchly Computer Corporation", "Banque de France", "Coors Brewing Company", "Super Mario USA", "Norfolk Wildlife Trust", "J. P. Morgan", "General Dynamics Corporation", "Princeton Architectural Press", "United Press International", "Northeastern University Press", "Sheffield Academic Press", "McGill-Queen's University Press", "Three Rivers Press", "Milton Bradley Company", "American Fur Company", "South African Airways", "Honourable Artillery Company", "Steinway & Sons", "All Nippon Airways", "Fox Film Corporation", "Milwaukee Journal Sentinel", "Generations Network Inc.", "Philip Morris USA", "Eastern Daily Press", "Open Source Software", "Green and Co.", "Ashgate Publishing Ltd.", "South Sea Company", "British Sky Broadcasting", "Sony Pictures Television", "Bausch & Lomb", "Abercrombie & Fitch", "Super Smash Bros.", "MOS Technology Inc.", "Notre Dame Press", "Xinhua News Agency", "Bank of Montreal", "Dallas Morning News", "American Motors Corporation", "St. James Press", "National Academies Press", "Second City Television", "Arab Monetary Fund", "Penn State Press", "Artificial Intelligence Laboratory", "Scientific Data Systems", "British European Airways", "Object Management Group", "Chicago Daily Tribune", "Hot Dance Music", "Hudson Bay Company", "British Heavy Metal", "Oxford Clarendon Press", "Bank of Canada", "Canterbury University Press", "BT Group plc", "Bank of France", "Leicester University Press", "Penguin Books Ltd.", "Mediacorp Canada Inc.", "Hilton Hotels Corporation", "Silicon Graphics Inc.", "Doubleday & Company", "Somers Isles Company", "World Wildlife Fund", "AT&T Bell Labs", "United Fruit Company", "Ohio University Press", "Wiley & Sons", "Steppenwolf Theatre Company", "Chicago Sun Times", "Adamant Media Corporation", "New Testament Introduction", "Dow Chemical Company", "Seven Stories Press", "New Line Cinema", "Encyclopaedia Britannica Inc.", "Thunder's Mouth Press", "Mitsubishi Heavy Industries", "Particle Data Group", "Old Testament Introduction", "William Kaufmann Inc.", "Joseph Henry Press", "Hackett Publishing Company", "Tokyo Stock Exchange", "International Crisis Group", "Inter Press Service", "Management Information Systems", "St. Martins Press", "Helsinki Stock Exchange", "BBC 6 Music", "Victor Gollancz Ltd", "Imperial College Press", "Holland Land Company", "Marks & Spencer", "Hyundai Motor Company", "Rocky Mountain News", "United Technologies Corporation", "Mail & Guardian", "Imperial Chemical Industries", "Bank of Greece", "Chase Manhattan Bank", "National Educational Television", "English Electric Company", "Cambridge Computer Laboratory", "Hawker Siddeley Group", "Yum! Brands Inc.", "Georgetown University Press", "AT&T Bell Laboratories", "Disney-ABC Television Group", "Buick Motor Company", "Brown & Co.", "Macmillan Publishers Ltd.", "Sinclair Research Ltd", "International Computers Limited", "Eastman Kodak Company", "Saudi Arabian Airlines", "Sinauer Associates Inc.", "White Wolf Inc.", "LOT Polish Airlines", "Tiffany & Co.", "Whispering Eagle Press", "American International Group", "Cooper Car Company", "BBC Northern Ireland", "DK Publishing Inc.", "Africa World Press", "Berkley Publishing Group", "Research In Motion", "Kawasaki Heavy Industries", "Addison-Wesley Publishing Company", "Newport News Shipbuilding", "National Westminster Bank", "British Broadcasting Company", "Computer Research Corporation", "Ford Motor Co.", "Chicago Review Press", "Peter Lang Publishing", "Lotus Development Corporation", "Macmillan and Co.", "Northwestern United States", "Protein Data Bank", "Jim Henson Company", "Electric Boat Company", "Swatch Group Ltd.", "Bell Telephone Company", "Academic Press Inc.", "Royal Dutch Shell", "Frankfurt Stock Exchange", "Crown Publishers Inc.", "McFarland & Co.", "Atlantic Monthly Press", "Parker Pen Company", "South Bank Show", "West Publishing Co.", "Kitchen Sink Press", "Winnipeg Free Press", "Golden Gryphon Press", "Archer Daniels Midland", "Hughes Aircraft Company", "Sussex Academic Press", "Sun Microsystems Inc.", "Trusted Computing Group", "Chicago Tunnel Company", "Walker & Company", "Morgan Reynolds Publishing", "Otis Elevator Company", "Bucknell University Press", "Garland Publishing Inc.", "Marconi Electronic Systems", "Minnesota Public Radio", "Western Electric Company", "Dun & Bradstreet", "Sanford and Son", "Reconstruction Finance Corporation", "Today's Best Music", "Contemporary Christian Music", "NASDAQ stock market", "SKY Network Television", "American Stock Exchange", "Father and son", "A&E Television Networks", "Elsevier Academic Press", "Edison Manufacturing Company", "King Features Syndicate", "Air New Zealand", "Cerberus Capital Management", "McKinsey & Company", "Maxim Integrated Products", "Energy National Laboratories", "Oxford U. Press", "Blue Man Group", "Shanghai Stock Exchange", "New Zealand Journal", "Compaq Computer Corporation", "All India Radio", "Shockley Semiconductor Laboratory", "Raytheon Missile Systems", "Country Music Television", "H.W. Wilson Co.", "E.B. Eddy Company", "America West Airlines", "Open Court Publishing", "WB Television Network", "Iron Crown Enterprises", "Pacific Fur Company", "Brookings Institution Press", "Motor Car Company", "ABC Radio Australia", "Mead & Company", "Alenia Marconi Systems", "American Popular Music", "Minnesota Law Review", "Valero Energy Corporation", "H.W. Wilson Company", "Boston History Company", "Wal-Mart Stores Inc.", "Marine Biological Laboratory", "Universal Music Group", "Marvell Technology Group", "Red Sea Press", "Toronto Stock Exchange", "William Heinemann Ltd.", "Canadian University Press", "John Knox Press", "Axel Springer AG", "Public Image Ltd.", "Toyota Motor Corporation", "Home Box Office", "Thomas Crowell Press", "CBS Paramount Television", "Asia Television Limited", "Virginia Quarterly Review", "Thinking Machines Corporation", "Braniff International Airways", "National Express Group", "Quaker Oats Company", "National Australia Bank", "Broadcast Music Incorporated", "J.B. Lippincott Company", "Museum Tusculanum Press", "Encyclopedia Britannica Inc.", "United States Steel", "Gorgias Press LLC", "Philippine Stock Exchange", "American Reprographics Company", "Brown & Company", "Carnegie Steel Company", "Associated Electrical Industries", "Bay Area Laboratory", "Erie Railroad Co.", "Norfolk Naturalists' Trust", "Marathon Petroleum Company", "Fantasy Games Unlimited", "American Tobacco Company", "Buell Motorcycle Company", "Covered Bridge Capital", "Toys R Us", "Canadian Broadcasting Company", "African Slave Trade", "Austin Motor Company", "British Racing Motors", "Formula One Management", "Stalky & Co", "Lisp Machines Inc.", "Roundabout Theatre Company", "Devon Wildlife Trust", "Air University Press", "Continuum International Publishing", "Ashgate Publishing Ltd", "Columbia Journalism Review", "Digital Research Inc.", "Associated British Foods", "State Street Corporation", "Kessinger Publishing LLC", "White Wolf Publishing", "Phillips Petroleum Company", "Pakistan International Airlines", "Network Working Group", "Great Lakes Airlines", "Wildlife Forensics Laboratory", "Literary Dictionary Company", "Thomas Nelson Inc.", "Jane's Defence Weekly", "American Bridge Company", "Red Hat Inc.", "United Artists Records", "China Central Television", "China Southern Airlines", "Chestnut Canoe Company", "Insurance Australia Group", "Royal Niger Company", "System Development Corporation", "Cosgrove Hall Films", "Long-Term Capital Management", "Bain & Company", "General Motors Company", "Mars Science Laboratory", "Gulf Research Laboratories", "Scarecrow Press Inc.", "Hutchinson & Co.", "Applied Physics Laboratory", "MTV Networks Europe", "New Directions Publishing", "Charles Jenkins Laboratories", "Waltham Watch Company", "US Forest Service", "Mega Party Inc.", "Barnard Island Group", "Postal Telegraph Company", "Asian Development Bank", "Triad Publishing Company", "Reader's Digest Association", "Oak Knoll Press", "Fordham University Press", "Stone Bridge Press", "Columbia Pictures Television", "Park Street Press", "Acorn Computers Ltd", "Industrial Development Corporation", "City Bridge Trust", "Cork University Press", "Manx Wildlife Trust", "Hudsons Bay Company", "Sirius XM Radio", "Norton & Company", "Marlowe & Company", "Colt's Manufacturing Company", "KPMG Europe LLP", "Inner City Press", "Dick's Sporting Goods", "Plumas Transit Systems", "New Holland Ag", "Turtle Beach Systems", "Blackwell Publishing Ltd.", "National Bus Company", "Great Beacon Press", "Metal Machine Music", "Eyre Methuen Ltd", "Standard Chartered Bank", "Anglo American plc", "Cisco Systems Inc.", "Ford of Europe", "Fuji Heavy Industries", "Plant Genetic Resources", "De Nederlandsche Bank", "Briggs & Stratton", "Ford Crown Victoria", "Crown Publishers Inc", "Morris Motor Company", "Simulations Publications Inc.", "Australian Securities Exchange", "Bank of Japan", "Four Courts Press", "E! Entertainment Television", "Black Entertainment Television", "Clear Channel Communications", "Lucas Industries plc", "Oxford English Dictionary", "Sri Lanka Telecom", "America Online Inc.", "New York Inc.", "Turner Publishing Company", "Northrop Grumman Corporation", "Visible Ink Press", "Amsterdam Stock Exchange", "Rough Guides Ltd.", "National Railway Company", "Essex Wildlife Trust", "Pacific Southwest Airlines", "Kinney National Company", "American World Airways", "Indian Classical Music", "Whole Foods Market", "Engineering Research Associates"]

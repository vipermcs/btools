# btools 2021-Jul-25

Basic collection of tools for analytical work.

Contents:

- NMuestra.xls: worksheet to compute sample size for basic cases.
- GCTabs.R, GCTabs.rda: set of functions and tables to compute anthropometric indices for childhood and adolescence.
  - W6NA: sexes, months of age, with their height for age, weight for age and BMI for ages coefficients for children 5+ and adolescents (WHO 2006)
  - W6NI: sexes, age or height levels, height method, with their coefficients for children 0-60 months (WHO 2006)
  - QTPR: clothes items, with their estimated weight (INS/CENAN 2008)
  - WNAzcm: function to compute anthropometric indices for children 5+ and adolescents
  - WINzcm: function to compute anthropometric indices for children <5
  - vWINzcm: vectorized WINzcm
- NuTabs.R, NuTabs.rda: set of functions and tables to compute nutritional requirements (FAO/WHO/UNU 2004).
- PETabs.R, PETabs.rda: set of demographic data for Peru.
  - TPPBE17: ages (simple years), with their projected populations 1950-2050 (INEI BE17 2009)
  - TPPBE37: districts, sexes, calendar years, with their projected populations by 5-yr age group (INEI BAD37 2009)
  - INEIGC20: districts belonging to large cities, except Lima Metropolitana (capital) (INEI 2020 block plans)
  - TDPD2017: districts, with their population at the 2017 census (INEI 2018 REDATAM)
  - TDPD2017ESA: districts, 5-yr age groups and sexes with their population at the 2017 census (INEI 2018 REDATAM)
  - PEPRE: presidents, with their terms, 1995-2021
  - PEMSA: ministers of Health, with their terms, 1999-2021
  - PEMDI: ministers of Social Development, with their terms, 2011-2021
  - MP09: districts, with their poverty rates (INEI Poverty Map 2009)
  - MP13: districts, with their poverty rates (INEI Poverty Map 2013)
  - EPPR: indicators (currently just undernutrition 0-60) for subgroups and periods (mostly years) (INEI PPR reports)

References:

The data, constants (coefficients) and formulas come from these public access sources:

- de Onis M, Onyango AW, Borghi E, Siyam A, Nishida C, Siekmann J. Development of a WHO growth reference for school-aged children and adolescents. Bull World Health Organ [Internet]. 2007;85(9):660–7. Disponible en: https://www.ncbi.nlm.nih.gov/pmc/articles/pmid/18026621/
- Food and Agriculture Organization (FAO). Human energy requirements - Report of a Joint FAO/WHO/UNU Expert Consultation Rome, 17–24 October 2001 [Internet]. Rome, IT: FAO; 2004. Report No.: 1. Disponible en: http://www.fao.org/publications/card/es/c/e1faed04-3a4c-558d-8ec4-76a1a7323dcc/
- Institute of Medicine (U.S.), editor. DRI: dietary reference intakes for vitamin A, vitamin K, arsenic, boron, chromium, copper, iodine, iron, manganese, molybdenum, nickel, silicon, vanadium, and zinc: a report of the Panel on Micronutrients ... and the Standing Committee on the Scientific Evaluation of Dietary Reference Intakes, Food and Nutrition Board, Institute of Medicine [Internet]. Washington, D.C: National Academy Press; 2001. 773 p. Disponible en: https://www.ncbi.nlm.nih.gov/books/NBK222310/
- Institute of Medicine (U.S.), Institute of Medicine (U.S.), editores. Dietary reference intakes for energy, carbohydrate, fiber, fat, fatty acids, cholesterol, protein, and amino acids [Internet]. Washington, D.C: National Academies Press; 2005. 1331 p. Disponible en: https://www.nap.edu/catalog/10490/dietary-reference-intakes-for-energy-carbohydrate-fiber-fat-fatty-acids-cholesterol-protein-and-amino-acids
- Perú, Instituto Nacional de Estadística e Informática (INEI). Perú: Estimaciones y Proyecciones de Población 1950-2050 Urbana-Rural 1970-2025 [Internet]. Lima, PE: INEI; 2001 ago. (Boletín de Análisis Demográfico). Report No.: 35. Disponible en: https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib0466/Libro.pdf
- Perú, Instituto Nacional de Estadística e Informática (INEI). Perú: Estimaciones y Proyecciones de Población, 1950-2050 [Internet]. Lima, PE: INEI; 2009 mar. (Boletín de Análisis Demográfico). Report No.: 36. Disponible en: http://proyectos.inei.gob.pe/web/biblioineipub/bancopub/Est/Lib0845/index.htm
- Perú, Instituto Nacional de Estadística e Informática (INEI). Perú: Estimaciones y Proyecciones de Población Total, por Años Calendario y Edades Simples, 1950-2050 [Internet]. Lima, PE: INEI; 2009 sep. (Boletín Especial). Report No.: 17. Disponible en: http://proyectos.inei.gob.pe/web/biblioineipub/bancopub/Est/Lib0843/index.htm
- Perú, Instituto Nacional de Estadística e Informática (INEI). PERÚ: Estimaciones y Proyecciones de Población por Departamento, Sexo y Grupos Quinquenales de Edad 1995-2025 [Internet]. Lima, PE: INEI; 2009 oct. (Boletín de Análisis Demográfico). Report No.: 37. Disponible en: http://proyectos.inei.gob.pe/web/biblioineipub/bancopub/Est/Lib0846/index.htm
- Perú, Instituto Nacional de Estadística e Informática (INEI). Perú: Estimaciones y Proyecciones de Población Total por Sexo de las Principales Ciudades, 2000-2015. Lima, PE: INEI; 2012 mar. (Boletín Especial). Report No.: 23. 
- Perú, Instituto Nacional de Estadística e Informática (INEI). Censos Nacionales 2017: XII de Población, VII de Vivienda y III de Comunidades Indígenas - Sistema de Consulta de Base de Datos [Internet]. INEI. 2018 [citado el 10 de septiembre de 2018]. Disponible en: http://censos2017.inei.gob.pe/redatam/
- Perú, Instituto Nacional de Estadística e Informática (INEI). Planos Estratificados de Lima Metropolitana a Nivel de Manzanas según Ingreso Per Cápita del Hogar [Internet]. Lima, PE: INEI; 2020 jul. Disponible en: https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1744/libro.pdf
- Perú, Instituto Nacional de Estadística e Informática (INEI). Planos Estratificados por ingreso a nivel de manzanas de las Grandes Ciudades 2020 según ingreso per cápita del hogar y estratificado regional [Internet]. Lima, PE: INEI; 2020 jul. Disponible en: https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1747/libro.pdf
- Perú, Instituto Nacional de Salud (INS), Centro Nacional de Alimentación y Nutrición (CENAN). DataLib: Biblioteca Digital INS/CENAN - Biblioteca Pública Digital de Encuestas [Internet]. DataLib: Biblioteca Digital INS/CENAN. 2008 [citado el 25 de julio de 2021]. Disponible en: https://www.ins.gob.pe/insvirtual/BiblioDig/
- Peru, Instituto Nacional de Salud, Centro Nacional de Alimentación y Nutrición (CENAN), editor. Tablas Peruanas de Composición de Alimentos. Lima, PE: INS/CENAN; 2017. 
- World Health Organization (WHO). WHO child growth standards: length/height-for-age, weight-for-age, weight-for-length, weight-for-height and body mass index-for-age ; methods and development [Internet]. Geneva: WHO Press; 2006. 312 p. Disponible en: https://www.who.int/publications-detail-redirect/924154693X

Citation:

- Campos M. btools: Basic collection of tools for analytical work. 2021-Jul-25. https://github.com/vipermcs/btools .
- Please, do quote also the original source references for the data and formulas applicable to your usage.

Comments:

My work is just a light shim and wrangling of the source data and formulas, useful for data analysis.
More documentation is being developed.
A newer version is planned (with some breaking changes), converging into a package or set of packages.

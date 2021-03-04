# LANDIS_setup

This repository contains scripts and data needed to setup LANDIS-II in a study area in southeastern Spain. This includes the generation of input files and calibration of the model parameters.

Model initialization subfolder contains the scripts for the generation of the initial communities map ("ic_" scripts) and ecoregions map ("reg_" scripts).

Data sources:

	- IFN:
		Files: parcelas_en_aoi.csv, PCEspParc.csv, PCMayores.csv, PCRegenera.csv
		Source: Third Spanish National Forest Inventory (2007). Ministerio para la Transición Ecológica y el Reto Demográfico. Available as a Microsoft Access database at: https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_bbdd_descargas.htm.aspx

	- Allometric equations
		File: funciones_alometricas.csv
		Source: Allometric equations for Spanish tree species published by Montero et al. (2005)

	- MFE map
		File: mfe_aoi_active_23030.shp
		Source: Ministerio de Agricultura, Pesca y Alimentación. Map resulting from photointerpretation. Accessible as vectorial file at: https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/desarrollo-rural/mfe_andalucia.aspx

	- Vegetation map
		File: vege10_aoi_active.shp
		Source: REDIAM, Consejería de Agricultura, Ganadería, Pesca y Desarrollo Sostenible. Map resulting from photointerpretation and field sampling. Accessible as vectorial file at: https://descargasrediam.cica.es/repo/s/RUR?path=%2F04_RECURSOS_NATURALES%2F01_BIODIVERSIDAD%2F01_VEGETACION_ECOSISTEMAS%2F01_VEGETACION_NATURAL%2F02_ECOSISTEMAS_FORESTALES%2F01_VEGETACION_10000

	- Soil data: 
		File: clay.tif, sand.tif, silt.tif, sta.adf
		Source: Sand, clay and silt soil content and soil depth. Raster files published by Rodriguez, 2008

	- Climate data:
		REDIAM data:
			Files: multiple files (p_*.tif, tmax_*.tif, tmed_*.tif, tmin_*.tif)
			Source: REDIAM, Consejería de Agricultura, Ganadería, Pesca y Desarrollo Sostenible. Annual precipitation, mean, minimum and maximum annual temperature for the period 1971-2000. Accessible as raster files at: https://descargasrediam.cica.es/repo/s/RUR

		Envidat data:	
			Files: multiple files, see files_names.txt
			Source: Network common data files published by Karger et al. (2020)
			
		ICOS data:
			File: SSL_12m_air.hdf.all.COMBI_Drought2018_20190522.co2
			Source: Meinhardt, F. & ICOS Atmosphere Thematic Centre. (2020). Drought-2018 CO2 molar fraction product from Schauinsland, Baden-Wuerttemberg (1.0) [Data set]. ICOS Carbon Portal. https://doi.org/10.18160/6MXY-S1PH
				
		PAR data:
			File: aoi_monmean.nc 
			Source: Photosynthetically Active Radiation for the period 1950-2019. Network common data files published by Cornes et al. (2018) (Version 20.0). 

References:

Cornes, Richard C., Gerard van der Schrier, Else J.M. van den Besselaar, and Philip D. Jones. 2018. “An Ensemble Version of the E-OBS Temperature and Precipitation Data Sets.” Journal of Geophysical Research: Atmospheres 123 (17): 9391–9409. https://doi.org/10.1029/2017JD028200.

Karger, Dirk Nikolaus, Dirk R. Schmatz, Gabriel Dettling, and Niklaus E. Zimmermann. 2020. “High-resolution monthly precipitation and temperature time series from 2006 to 2100.” Scientific Data 7 (1): 1–10. https://doi.org/10.1038/s41597-020-00587-y.

Montero, Gregorio, Ricardo Ruiz-Peinado, and Marta Muñoz. 2005. Producción de biomasa y fijación de CO2 por los bosques españoles. Vol. 13. JANUARY 2005.

Rodríguez, José Antonio. 2008. “Sistema de Inferencia Espacial de Propiedades Físico-Químicas Hidráulicas de los Suelos de Andalucía. Herramienta de Apoyo a la Simulación de Procesos Agro-Hidrológicos a Escala Regional. Informe final.” Sevilla: Empresa Pública Desarrollo Agrario y Pesquero. Subdirección de Infraestructura y Desarrollo. Área de Modernización y Regadíos. Consejería de Agricultura y Pesca.
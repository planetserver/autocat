autocat
=======

Uses MRO CRISM CAT (http://pds-geosciences.wustl.edu/missions/mro/crism.htm) which needs to be installed and patched for batch processing.

various diff files needed to make automatic CAT IDL routines from the original event driven code
the files can be found in the following CRISM CAT subfolder: CAT_ENVI\save_add\CAT_programs\Event_handlers

They are :	

* original/atpcorr_event.pro				
* original/mro_crism_summary_params_hyper_event.pro
* original/convert_pds2cat_event.pro			
* original/projection_event.pro		
* original/mro_crism_summary_params_event.pro	


in order to automate them and make them non-interactive one should "patch" them with the files 
that are inside the folder "patch", i.e run the following command:    	

patch original/\<filename_prefix\>_event.pro patch/\<filename_prefix\>.pro.patch -o \<output_filename\>,    
for example patch original/atpcorr_event.pro patch/atpcorr.pro.patch -o auto/atpcorr_auto.pro.


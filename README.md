autocat
=======

# Preliminary
* Please download http://hesperia.gsfc.nasa.gov/ssw/gen/idl/io/rd_tfile.pro
* Uses MRO CRISM CAT (http://pds-geosciences.wustl.edu/missions/mro/crism.htm) which needs to be installed and patched for batch processing.

# Make automatic version from event driven IDL
Various diff files are located in the patch folder. They are needed to make the automatic CAT IDL routines from the original event driven code. The original files can be found in the following CRISM CAT subfolder: CAT_ENVI\save_add\CAT_programs\Event_handlers
* atpcorr_event.pro				
* mro_crism_summary_params_hyper_event.pro
* convert_pds2cat_event.pro			
* projection_event.pro		
* mro_crism_summary_params_event.pro	

In order to automate them and make them non-interactive one should "patch" them with the files that are inside the folder "patch", i.e run the following command:    	
* patch original/\<filename_prefix\>_event.pro patch/\<filename_prefix\>.pro.patch -o \<output_filename\>,    
for example patch original/atpcorr_event.pro patch/atpcorr.pro.patch -o auto/atpcorr_auto.pro.

# Running autocat
* Perform 'ls *trr3.img>data.txt' in the folder where the CRISM TRR3 PDS data has been downloaded.
* Open run_trdr.pro and make sure the input and output folders are set correctly.
* Start 'envi -classic'
* ENVI> cd,'/path/to/autocat/'
* ENVI> run_trdr
* And wait until autocat finishes.


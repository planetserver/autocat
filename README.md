autocat
=======

various diff files needed to make automatic CAT IDL routines from the original event driven code
the files are inside the folder original. They are :	
'''
* original/atpcorr_event.pro				
* original/mro_crism_summary_params_hyper_event.pro
* original/convert_pds2cat_event.pro			
* original/projection_event.pro		
* original/mro_crism_summary_params_event.pro	
'''

in order to automate them and make them non-interactive one should patch them with the patches 
that are inside patch. 
to create an version that allows automation, run the command:	

patch original/<filename_prefix>_event.pro patch/<filename_prefix>.pro.patch -o <output_filename>	
for example patch original/atpcorr_event.pro patch/atpcorr.pro.patch -o auto/atpcorr_auto.pro.


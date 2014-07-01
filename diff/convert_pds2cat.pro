93,129c93
< pro convert_pds2cat_event, ev
< 
< widget_control, ev.id, get_uvalue=uvalue
< 
< ; Define ENVI functions used so able to create a sav file:
< FORWARD_FUNCTION envi_pickfile, widget_menu, widget_auto_base,auto_wid_mng
< 
< ;Simple catch to prevent crash
< catch, error
< if (error NE 0) then begin
< 	cat_error_report, routine_name='convert_pds2cat_event'
< 	return
< endif
< 
< fname = envi_pickfile(title='Select CRISM file(s) to convert:',$
<                       filter='*.img;*.IMG',/multiple_files)
< if (fname[0] eq '') then return                ;cancel selected
< 
< 
< 
< 
< ; See if user wants to output to memory or to write files:
< prompt = 'Output Result to:'
< list = ['File  ','Memory  ']
< base = widget_auto_base(title='Convert to CAT')  
< wo = widget_menu(base, list=list, prompt=prompt,uvalue='memory', /auto,/exclusive)  
< result = auto_wid_mng(base)
< if (result.accept eq 0) then return  ; cancel selected  
< memory = result.memory
< 
< ; If writing files, output filenames are forced to convention
< ; and checked for redundancy:
< if (memory eq 0) then begin
< 	title = 'Select Ouput Directory (filenames determined by program)'
< 	outdir = cat_term_path_sep(envi_pickfile(title=title,/directory))
< endif
< 
---
> pro convert_pds2cat_auto, fname, outdir
130a95
> memory = 0
226c191
< 		envi_open_file,input_file
---
> 		envi_open_file,input_file, R_FID = fid
399a365,366
> 
> ;envi_file_mng, id=fid, /remove

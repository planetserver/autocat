126,145c126
< 
< 
< PRO atpcorr_event, ev
< 
< widget_control, ev.id, get_uvalue=uvalue
< 
< t1 = systime(1)
< 
< ; Define ENVI functions used so able to create a sav file:
< FORWARD_FUNCTION envi_get_slice,widget_auto_base,widget_outfm, $
<                  magic_mem_check, widget_menu, auto_wid_mng, $
<                  atp_tile, atp_nontile, get_cat_header_data
< 
< ; Simple catch to prevent crash:
< catch, error
< if (error NE 0) then begin
< 	cat_error_report, routine_name='ATPcorr_event'
< 	return
< endif
<  
---
> pro atpcorr_auto, atm, therm, phot, atmt_src, vsid, bandset_id, enable_artifact, fname, outfile
151,244c132,138
< ;***************************************************
< ; Run widget program for user to choose corrections.
< ;***************************************************
< TEMP:
< pops_widget,choices
< w = where((choices eq 'cancel'), ncan)       ; cancel selected
< if (ncan gt 0) then return
< atm = choices[0]       ;options are scale, epf, or none
< therm = choices[1]     ;options are algm, algk, or none
< phot = choices[2]      ;options are on or off
< if (strupcase(choices[3]) eq 'NO') then allow_subset=0 else allow_subset=1
< 
< 
< ;DELETE LATER DOWN
< if (atm eq 'epf') then begin
< 	ok = dialog_message('Empirical EPF atmospheric correction is not yet available. Select again.',$
<                          title='Correct CRISM')
< 	goto,TEMP
< endif
< if (therm ne 'none') then begin
< 	ok = dialog_message('Thermal correction not yet available. Continue without correction?',$
<                           title='Correct CRISM',/question,/cancel)
< 	if ((ok eq 'No') || (ok eq 'Cancel')) then return
< 	therm='none'
< endif
< ;DELETE LATER UP
< 
< ;***************************************************
< ;   Select how to do scaling if requested
< ;***************************************************
< 
< atmt_src = ''
< vsid = ''
< bandset_id = 0
< enable_artifact = 0
< if (atm eq 'scale') then begin
< 	; Run widget to select transmission spectrum source. 
< 	; Choices: auto-selected AT CDR 'auto', default volcano scan 61C4 'default', or
< 	; user-selected volcano scan 'user'
< 	select_atmt_source, atmt_src, vsid
< 
< 	; And another to select the bands for scaling and enable/disable 
< 	; artifact correction:
< 	select_scaling_art, choices
< 	if (choices[0] lt 0) then return    ; user cancelled
< 	bandset_id = choices[0]
< 	enable_artifact = choices[1]
< 	if ((bandset_id eq 2) and (enable_artifact)) then begin
< 		msg = ['Artifact correction not supported with McGuire', $
< 		       'multi-wavelength correction. Continue without?']
< 		ok = dialog_message(msg, title='Correct CRISM', /question, /cancel)
< 		if ((ok eq 'No') or (ok eq 'Cancel')) then begin
< 			return
< 		endif else begin
< 			enable_artifact = 0
< 		endelse
< 	endif
< endif
< 
< 
< ;=======================================
< ;    PICK FILE, SUBSETTING
< ;=======================================
< 
< ; Have user pick an open data file to work with. Different path depending 
< ; whether allowing subsetting. Also set subsetting related flags. 
< ;   subset if true means subsetting is actually in play [allowed AND < nb bands selected]
< ;   runall if true means need to load all the bands for processing, 
< ;          even if later only a subset will be output.
< if (allow_subset) then begin
< 	; Allow spectral subsetting; also, bad bands will not appear
< 	envi_select,fid=in_fid,pos=in_bands,/file_only,/no_dims,title='Select CRISM File:'
< 	if (in_fid[0] eq -1) then return                  ;cancel selected
< 	envi_file_query,in_fid,nb=nb
< 	if (n_elements(in_bands) ne nb) then begin
< 		; User, or bad bands, caused subsetting...
< 		subset = 1
< 		if (atm eq 'scale') then runall=1 else runall=0
< 	endif else begin
< 		; All bands in play anyway...
< 		subset = 0
< 		runall = 1
< 	endelse
< endif else begin
< 	; Don't allow spectral subsetting; force later input to include bad bands
< 	envi_select,fid=in_fid,pos=in_bands,/file_only,/no_dims,/no_spec,title='Select CRISM File:'
< 	if (in_fid[0] eq -1) then return                  ;cancel selected
< 	envi_file_query,in_fid,nb=nb
< 	in_bands = lindgen(nb)           ; Force all bands in.
< 	subset = 0
< 	runall = 1
< endelse
< 
< 
---
> ;TEMP:
> envi_open_file, fname, r_fid=in_fid
> envi_file_query,in_fid,nb=nb
> in_bands = lindgen(nb)
> subset = 0
> runall = 1
> memory = 0
355,406d248
< 
< ;==================================================
< ;   OUTPUT FILENAME, AND FILE OR MEMORY
< ;==================================================
< 
< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
< ; Check to see if user wants to write to a file or keep in memory:
< ; (Load into available bands list regardless)
< ; Create default output file names:
< ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
< pos = strpos(datafile,'.',/reverse_search)
< if (pos ne -1) then begin
< 	default = strmid(datafile,0,pos)
< 	default += '_corr.img'
< endif else begin
< 	default = ''
< endelse
< ; Widget lets users pick a output filename, and choose output to memory or file:
< base = widget_auto_base(title='Correct CRISM.')
< wo = widget_outfm(base, uvalue='outf',default=default, /auto)
< result = auto_wid_mng(base)
< if (result.accept eq 0) then return
< 
< ; memory check (If user asked to store in memory, and there's not enough, this
< ; opens a dialog box that allows user to change to file instead, and give a name. 
< ; That would switch mem_check.in_memory to 0. If user requested mem storage 
< ; and there's enough, mem_check.in_memory=1 on return.)
< mem_check = magic_mem_check(dims=in_dims,in_memory = result.outf.in_memory, $
< 			out_dt=data_type, nb=n_elements(in_bands), out_name = result.outf.name)
< if (mem_check.cancel EQ 1) then return    ; cancel selected
< 
< ;Store result of memory check in result structure
< result.outf.in_memory = mem_check.in_memory
< result.outf.name = mem_check.out_name
< memory = result.outf.in_memory
< 
< if (~memory) then begin
< 	outfile = result.outf.name
< endif else begin
< 	; Working in memory; need to define flename parameters to
< 	; avoid crash when referenced to load into input struct 
< 	; for processing procedures.
< 	outfile = ''
< endelse
< 	
< ; This all generated the following parameters used later:
< ;	memory       0,1 store in mem flag
< ;	outfile      filename
< ;
< ; result struct not used anymore.
< 
< 
498a341,344
> 
> ENVI_CLOSE_DISPLAY, 0
> envi_file_mng, id=in_fid, /remove, /delete
> ENVI_CLOSE_DISPLAY, 1

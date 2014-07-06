63,74c63
< 
< 
< pro mro_crism_summary_params_event, event
< 
< ; Simple catch to prevent crash
< catch, error
< if (error NE 0) then begin
<   	cat_error_report, routine_name='mro_crism_summary_params_event';
< 	return
< endif
< 
< t1=systime(1)
---
> pro mro_crism_summary_params_auto, fname, summary_params_file, section_indx, summary_select, summary_list
79,92c68
< widget_control, event.id, get_uvalue = uvalue
< 
< if (uvalue eq 'VNIR') then section_indx = 0
< if (uvalue eq 'IR') then section_indx = 1
< if (uvalue eq 'Joined') then section_indx = 2
< 
< 
< ;Select input file
< envi_select, fid=in_fid, pos=in_pos, dims=in_dims, file_type=in_file_type, $
<      /file_only, /no_spec, title = uvalue + ' Summary Params: Select Input File'
< if (in_fid[0] eq -1) then begin
< 	print, uvalue + ' Summary Params: Cancel/Invalid FID'
< 	return
< endif
---
> envi_open_file, fname, r_fid=in_fid
95c71
< stat = get_cat_header_data(in_fid, NS=ns, NL=nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
---
> stat = get_cat_header_data(in_fid, NS=in_ns, NL=in_nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
121d96
< 
124,129c99
< 
< 
< ; Get output dimensions in case user subsetted image:
< in_ns=in_dims[2]-in_dims[1]+1
< in_nl=in_dims[4]-in_dims[3]+1
< 
---
> in_dims = [-1, 0, in_ns-1, 0, in_nl-1]  
133c103
< 	print, uvalue + ' Summary Params: No wavelength vector associated with selected file'
---
> 	print, 'No wavelength vector associated with selected file'
137d106
< 
148a118
> summary_nb = total(summary_select)
150,174c120,121
< ; If detector found, check for compatibility between file and selection:
< if (det ne '') then begin
<   if (section_indx eq 0) and (det ne 'S') then begin
<     ok = dialog_message('VNIR product chosen for non-VNIR file. Continue?',$
<                          title='Summary Parameter Error', /cancel)
<     if (ok eq 'Cancel') then return
<   endif
<   if (section_indx eq 1) and (det ne 'L') then begin
<     ok = dialog_message('IR product chosen for non-IR file. Continue?',$
<                          title='Summary Parameter Error', /cancel)
<     if (ok eq 'Cancel') then return
<   endif
<   if (section_indx eq 2) and (det ne 'J') then begin
<     ok = dialog_message('Joined product chosen for non-joined file. Continue?',$
<                          title='Summary Parameter Error', /cancel)
<     if (ok eq 'Cancel') then return
<   endif
<   if (det eq 'J') then tile=1 else tile=0
< endif else begin
<   ok = dialog_message('Is this a tile?',$
<                        title='Summary Parameter Question',/question,/cancel)
<   if (ok eq 'Cancel') then return
<   if (ok eq 'No') then tile=0 else tile=1
< endelse
< 
---
> summary_ysize = 400
> summary_default = replicate(1b, n_elements(summary_list))
176,251c123,124
< 
< summary_params_file = in_file_directory + in_file_basename + '_params' + in_file_extension
< 
< vnir_summary_list = ['R770', 'RBR', 'BD530', 'SH600', 'BD640', 'BD860', 'BD920', 'RPEAK1', 'BDI1000VIS', $
< 					 'R440', 'IRR1'] ; NON-ATM Corrected
< ir_summary_list = 	['BDI1000IR', 'IRA', 'OLINDEX', 'LCPINDEX', 'HCPINDEX', 'VAR', 'ISLOPE1', 'BD1435', 'BD1500', 'ICER1', $
< 					 'BD1750', 'BD1900', 'BDI2000', 'BD2100', 'BD2210', 'BD2290', 'D2300', 'SINDEX', 'ICER2', 'BDCARB', $
< 					 'BD3000', 'BD3100', 'BD3200', 'BD3400', 'CINDEX', $
< 					 'BD1270O2', 'BD1400H2O', 'BD2000CO2', 'BD2350', 'BD2600', 'IRR2', 'R2700', 'BD2700', 'IRR3'] ;NON-ATM Corrected
< 
< j_summary_list = ['R770', 'RBR', 'BD530', 'SH600', 'BD640', 'BD860', 'BD920', 'RPEAK1', 'BDI1000VIS', $
< 					 'R440', 'IRR1', $ ; NON-ATM Corrected
<                      'BDI1000IR', 'IRA', 'OLINDEX', 'LCPINDEX', 'HCPINDEX', 'VAR', 'ISLOPE1', 'BD1435', 'BD1500', 'ICER1', $
< 					 'BD1750', 'BD1900', 'BDI2000', 'BD2100', 'BD2210', 'BD2290', 'D2300', 'SINDEX', 'ICER2', 'BDCARB', $
< 					 'BD3000', 'BD3100', 'BD3200', 'BD3400', 'CINDEX', $
< 					 'BD1270O2', 'BD1400H2O', 'BD2000CO2', 'BD2350', 'BD2600', 'IRR2', 'R2700', 'BD2700', 'IRR3'] ;NON-ATM Corrected
< 
< if (section_indx EQ 2) then begin
< 	summary_list = j_summary_list
< 	summary_ysize = 400
< 	summary_default = replicate(1b, n_elements(summary_list))
< endif
< if (section_indx EQ 1) then begin
< 	summary_list = ir_summary_list
< 	summary_ysize = 400
< 	summary_default = replicate(1b, n_elements(summary_list))
< endif 
< if (section_indx EQ 0) then begin
< 	summary_list = vnir_summary_list
< 	summary_ysize = 280
< 	summary_default = replicate(1b, n_elements(summary_list))
< endif
< 
< 
< ;Gather user input info via ENVI auto managed widgets
< auto_base = widget_auto_base(title = uvalue)
< main_base = widget_base(auto_base, /row)
< 	summary_base = widget_base(main_base, /col)
< 		summary_label = widget_label(summary_base, value = uvalue)
< 
< 		summary_select = widget_multi(summary_base, /auto_manage, list = summary_list, uvalue = 'summary_select', /no_range, $
< 								     default = summary_default, ysize = summary_ysize)
< 		summary_outfm = widget_outfm(summary_base, /auto_manage, /frame, default = summary_params_file, uvalue = 'summary_outfm')
< 
< result = auto_wid_mng(auto_base)
< if (result.accept EQ 0) then begin
< 	print, ': Cancel
< 	return
< endif
< 
< 
< ;Set summary product output parameters
< summary_dims = in_dims
< summary_dt = 4 ;floating point
< summary_band_indx = where(result.summary_select EQ 1, summary_nb)
< 
< ;memory check
< mem_check = magic_mem_check(dims=in_dims, in_memory = result.summary_outfm.in_memory, $
< 			    out_dt=summary_dt, nb=summary_nb, out_name = result.summary_outfm.name)
< if (mem_check.cancel EQ 1) then begin
< 	print, uvalue + ': Cancel'
< 	return
< endif
< 
< ;Store result of memory check in result structure
< result.summary_outfm.in_memory = mem_check.in_memory
< result.summary_outfm.name = mem_check.out_name
< 
< ;Declare in-memory output array or open output file
< if (result.summary_outfm.in_memory EQ 1) then begin
< 	summary_cube = make_array(in_ns, in_nl, summary_nb, value = 65535., /float)
< 	out_status_string = 'Output to Memory'
< endif else begin
< 	openw, summary_lun, result.summary_outfm.name, /get_lun
< 	out_status_string = 'Output File: ' + result.summary_outfm.name
< endelse
---
> openw, summary_lun, summary_params_file, /get_lun
> out_status_string = 'Output File: ' + summary_params_file
256,274d128
< ;===================
< ;  Dec 3,2009 MFM: Forcing all bands by setting in_pos = all bands above, 
< ;  previously kept all bands by faking bl all 1's, but now keeping bbl real, 
< ;  so eliminate the following block of code. Old approach might have been a 
< ;  bit of a misunderstanding too, thinking ENVI filtered on bbl instead 
< ;  of in_pos...
< 
< ;; Pass only good bands; envi_get_tile filters out bands in bad band list, 
< ;; wavelength array must match cube
< ;kgood = where((in_bbl eq 1), ngood)
< ;if (ngood eq 0) then begin
< ;	ok = dialog_message('All bands bad. Aborting summary parameters.',$
< ;	           title='Summary Parameter Error')
< ;	return
< ;endif
< ;wlnm = wlnm[kgood]
< ;===================
< 
< 
280c134
< envi_report_init, status_string_array, base = status_base, title = uvalue+ ' Summary Params', /interupt
---
> envi_report_init, status_string_array, base = status_base, title = 'Summary Params', /interupt
291,293c145,147
< 		if (result.summary_outfm.in_memory EQ 0) then begin
< 			if (n_elements(geomap_lun) gt 0) then free_lun, geomap_lun
< 		endif
---
> 		;if (result.summary_outfm.in_memory EQ 0) then begin
> 	  if (n_elements(geomap_lun) gt 0) then free_lun, geomap_lun
> 		;endif
309c163
< 	                      index_vec = result.summary_select)
---
> 	                      index_vec = summary_select)
313,317c167,171
< 	if (result.summary_outfm.in_memory EQ 1) then begin
< 		summary_cube[*,i,*] = summary_output_slice
< 	endif else begin
< 		writeu, summary_lun, summary_output_slice
< 	endelse
---
> ;	if (result.summary_outfm.in_memory EQ 1) then begin
> ;		summary_cube[*,i,*] = summary_output_slice
> ;	endif else begin
>     writeu, summary_lun, summary_output_slice
> 	;endelse
345,349d198
< ;Register memory item in ENVI session or generate header for output file
< if (result.summary_outfm.in_memory EQ 1) then begin
< 	envi_enter_data, summary_cube, data_ignore_value=65535., $
< 			bnames = summary_list[where(result.summary_select EQ 1)], $
< 			descrip='CAT CRISM summary parameters', map_info=in_map_info
351,393c200,201
< endif else begin
< 	free_lun, summary_lun
< 
< 	stat = write_cat_header(FNAME=result.summary_outfm.name, $
< 	         NS=in_ns, NL=in_nl, NB=summary_nb, $
< 	         BNAMES=summary_list[where(result.summary_select EQ 1)], $
<         	 DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
<         	 DATA_IGNORE_VALUE=65535.0, $
< 	         DESCRIP='CAT CRISM summary parameters', $
< 	         MAP_INFO=map_info, $
< 	         CAT_START_TIME=cat_start_time, $
< 	         CAT_SCLK_START=cat_sclk_start, $
< 	         CAT_CRISM_OBSID=cat_crism_obsid, $
< 	         CAT_OBS_TYPE=cat_obs_type, $
< 	         CAT_PRODUCT_VERSION=cat_product_version, $
< 	         CAT_CRISM_DETECTOR_ID=det, $
< 	         CAT_BIN_MODE=cat_bin_mode, $
< 	         CAT_WAVELENGTH_FILTER=cat_wavelength_filter, $
< 	         CAT_CRISM_DETECTOR_TEMP=cat_crism_detector_temp, $
< 	         CAT_CRISM_BENCH_TEMP=cat_crism_bench_temp, $
< 	         CAT_CRISM_HOUSING_TEMP=cat_crism_housing_temp, $
< 	         CAT_SOLAR_LONGITUDE=cat_solar_longitude, $
< 	         CAT_PDS_LABEL_FILE=cat_pds_label_file, $
< 	         CAT_HISTORY=cat_history, $
< 	         CAT_INPUT_FILES=cat_input_files)
< 
< 	; Report problems with header if any indicated by status returned:
< 	if ((stat ne 0) and (~keyword_set(batch))) then begin
< 		if (stat eq -1) then begin
< 			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
< 			      + 'Required keywords for write_cat_header undefined.'
< 			ok = dialog_message(msg, title='Header Error')
< 		endif else if (stat eq -2) then begin
< 			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
< 		 	     + 'Header already exists.'
< 			ok = dialog_message(msg, title='Header Error')
< 		endif else begin
< 			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
< 			      + 'Unspecified error.'
< 			ok = dialog_message(msg, title='Header Error')
< 		endelse
< 	endif
< endelse
---
> ;endif else begin
> free_lun, summary_lun
394a203,224
> stat = write_cat_header(FNAME=summary_params_file, $
>          NS=in_ns, NL=in_nl, NB=summary_nb, $
>          BNAMES=summary_list[where(summary_select EQ 1)], $
>       	 DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
>       	 DATA_IGNORE_VALUE=65535.0, $
>          DESCRIP='CAT CRISM summary parameters', $
>          MAP_INFO=map_info, $
>          CAT_START_TIME=cat_start_time, $
>          CAT_SCLK_START=cat_sclk_start, $
>          CAT_CRISM_OBSID=cat_crism_obsid, $
>          CAT_OBS_TYPE=cat_obs_type, $
>          CAT_PRODUCT_VERSION=cat_product_version, $
>          CAT_CRISM_DETECTOR_ID=det, $
>          CAT_BIN_MODE=cat_bin_mode, $
>          CAT_WAVELENGTH_FILTER=cat_wavelength_filter, $
>          CAT_CRISM_DETECTOR_TEMP=cat_crism_detector_temp, $
>          CAT_CRISM_BENCH_TEMP=cat_crism_bench_temp, $
>          CAT_CRISM_HOUSING_TEMP=cat_crism_housing_temp, $
>          CAT_SOLAR_LONGITUDE=cat_solar_longitude, $
>          CAT_PDS_LABEL_FILE=cat_pds_label_file, $
>          CAT_HISTORY=cat_history, $
>          CAT_INPUT_FILES=cat_input_files)
398,401c228,230
< t2=systime(1)
< etime=(t2-t1)/60.
< PRINT,'Elapsed time (min): ',strtrim(etime,2)
< 
---
> ENVI_CLOSE_DISPLAY, 0
> envi_file_mng, id=in_fid, /remove
> ENVI_CLOSE_DISPLAY, 1

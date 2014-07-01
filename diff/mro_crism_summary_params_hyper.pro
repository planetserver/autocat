61,70c61
< pro mro_crism_summary_params_hyper_event, event
< 
< 
< 
< ; Simple catch to prevent crash
< catch, error
< if (error NE 0) then begin
<   	cat_error_report, routine_name='mro_crism_summary_params_hyper_event'
< 	return
< endif
---
> pro mro_crism_summary_params_hyper_auto, fname, summary_params_file, summary_list
75,94c66
< widget_control, event.id, get_uvalue = event_uvalue
< 
< section_string_array = ['VNIR', 'IR']
< if (strpos(event_uvalue, 'VNIR') EQ -1) then section_indx = 1 else section_indx = 0
< if (strpos(event_uvalue, 'J') ne -1) then begin
<     ok = dialog_message('Not expecting hyperspectral joined data. Aborting procedure.',$
<                        title='Hyperspectral Summary Parameter Error')
<     return
< endif
< 
< 
< ;Select input file
< envi_select, fid=in_fid, $      ; see note on in_pos below
< 			 dims=in_dims, file_type=in_file_type, /file_only, /no_spec, $
< 			 title = event_uvalue + ': Select Input File'
< if (in_fid[0] eq -1) then begin
< 	print, event_uvalue + ': Cancel/Invalid FID'
< 	return
< endif
< 
---
> envi_open_file, fname, r_fid=in_fid
97c69
< stat = get_cat_header_data(in_fid, NS=ns, NL=nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
---
> stat = get_cat_header_data(in_fid, NS=in_ns, NL=in_nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
123,129d94
< if (stat ne 0) then begin
<     ok = dialog_message('Failure reading header data. Aborting procedure.',$
<                        title='Hyperspectral Summary Parameter Error')
<     return
< endif
< 
< 
134,138c99
< 
< ; Get output dimensions in case user subsetted image:
< in_ns = in_dims[2]-in_dims[1]+1
< in_nl = in_dims[4]-in_dims[3]+1
< 
---
> in_dims = [-1, 0, in_ns-1, 0, in_nl-1]
146,147d106
< 
< ;Construct default output filenames
159,176d117
< 
< ; If possible, check consistency between filenames and params
< ; selected (case insensitive... MFM):
< if (section_indx eq 0) and (det eq 'L') then begin
<     ok = dialog_message('VNIR product chosen for IR detector. Aborting procedure.',$
<                        title='Hyperspectral Summary Parameter Error', /cancel)
<     if (ok eq 'Cancel') then return
< endif
< if (section_indx eq 1) and (det eq 'S') then begin
<     ok = dialog_message('IR product chosen for VNIR detector. Aborting procedure.',$
<                        title='Hyperspectral Summary Parameter Error', /cancel)
<     if (ok eq 'Cancel') then return
< endif
< 
< summary_params_file = in_file_directory + in_file_basename + '_hyparam' + in_file_extension
< 
< 
< 
178,186c119,123
< summary_list = 1
< dum = mro_crism_summary_params_hyper(dummy, section_indx, band_names=summary_list)
< 
< ; Give up if there are no available parameters:
< if (max(strlen(strtrim(summary_list))) eq 0) then begin
< 	ok = dialog_message('No hyperspectral parameters available. Aborting procedure.',$
<               title='Hyperspectral Summary Parameter Error')
<     return
< endif
---
> ; summary_list = 1
> ; dum = mro_crism_summary_params_hyper(dummy, section_indx, band_names=summary_list)
> section_indx = 1
> summary_nb = size(summary_list)
> summary_nb = summary_nb[1]
197,246c134,135
< ;Gather user selections and output datafile info via ENVI auto managed widgets
< auto_base = widget_auto_base(title = event_uvalue)
< main_base = widget_base(auto_base, /row)
< 	summary_base = widget_base(main_base, /col)
< 		summary_label = widget_label(summary_base, value = event_uvalue)
< 
< 		summary_select = widget_multi(summary_base, /auto_manage, list = summary_list, uvalue = 'summary_select', /no_range, $
< 								     default = summary_default, ysize = summary_ysize)
< 		summary_outfm = widget_outfm(summary_base, /auto_manage, /frame, default = summary_params_file, uvalue = 'summary_outfm')
< 
< result = auto_wid_mng(auto_base)
< 
< if (result.accept EQ 0) then begin
< 	print, ': Cancel'
< 	return
< endif
< 
< 
< 
< 
< 
< ;Set summary product output parameters
< summary_dims = in_dims
< summary_dt = 4 ;floating point
< summary_band_indx = where(result.summary_select EQ 1, summary_nb)
< 
< 
< ;memory check
< mem_check = magic_mem_check(dims=in_dims, in_memory = result.summary_outfm.in_memory, $
< 				out_dt=summary_dt, nb=summary_nb, out_name = result.summary_outfm.name)
< 
< if (mem_check.cancel EQ 1) then begin
< 	print, event_uvalue + ': Cancel'
< 	return
< endif
< 
< ;Store result of memory check in result structure
< result.summary_outfm.in_memory = mem_check.in_memory
< result.summary_outfm.name = mem_check.out_name
< 
< 
< ;Declare in-memory output array or open output file
< if (result.summary_outfm.in_memory EQ 1) then begin
< 	summary_cube = make_array(in_ns, in_nl, summary_nb, value = !values.f_nan, /float)
< 	out_status_string = 'Output to Memory'
< endif else begin
< 	openw, summary_lun, result.summary_outfm.name, /get_lun
< 	out_status_string = 'Output File: ' + result.summary_outfm.name
< endelse
< 
---
> openw, summary_lun, summary_params_file, /get_lun
> out_status_string = 'Output File: ' + summary_params_file
253,254d141
< 
< 
257c144
< 		title = event_uvalue + ' Hyperspectral Summary Params', /interupt
---
> 		title = ' Hyperspectral Summary Params', /interupt
281c168
< 	    index_vec = result.summary_select)
---
> 	    index_vec = summary_select)
284,288c171,175
< 	if (result.summary_outfm.in_memory EQ 1) then begin
< 		summary_cube[*,i,*] = summary_output_slice
< 	endif else begin
< 		writeu, summary_lun, summary_output_slice
< 	endelse
---
> 	; if (result.summary_outfm.in_memory EQ 1) then begin
> 		; summary_cube[*,i,*] = summary_output_slice
> 	; endif else begin
>     writeu, summary_lun, summary_output_slice
> 	;endelse
313,360c200,222
< ;Register memory item in ENVI session or generate header for output file
< if (result.summary_outfm.in_memory EQ 1) then begin
< 	envi_enter_data, summary_cube, data_ignore_value=65535., $
< 	          bnames = summary_list[where(result.summary_select EQ 1)], $
< 	          descrip='CAT CRISM hyperspectral summary parameters', $
< 	          map_info=map_info
< endif else begin
< 	free_lun, summary_lun
< 	stat = write_cat_header(FNAME=result.summary_outfm.name, $
< 	         NS=in_ns, NL=in_nl, NB=summary_nb, $
< 	         BNAMES=summary_list[where(result.summary_select EQ 1)], $
<         	 DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
<         	 DATA_IGNORE_VALUE=65535.0, $
< 	         DESCRIP='CAT CRISM hyperspectral summary parameters', $
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
> free_lun, summary_lun
> stat = write_cat_header(FNAME=summary_params_file, $
>          NS=in_ns, NL=in_nl, NB=summary_nb, $
>          BNAMES=summary_list[where(summary_select EQ 1)], $
>          DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
>          DATA_IGNORE_VALUE=65535.0, $
>          DESCRIP='CAT CRISM hyperspectral summary parameters', $
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
362a225,229
> 
> ENVI_CLOSE_DISPLAY, 0
> envi_file_mng, id=in_fid, /remove
> ENVI_CLOSE_DISPLAY, 1
> 

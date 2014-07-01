;+
; NAME:
;      Spectral Summary Products
;      note: this code DOES NOT ACCOUNT FOR SPECTRAL SMILE
;            (uses only the sweet spot wave array associated w/ENVI file)
;
; AUTHOR:
;		Frank Seelos
;		Johns Hopkins Univeristy Applied Physics Laboratory (2005-)
;			frank dot seelos at jhuapl dot edu
;
; MODIFICATION HISTORY:
; Nov 02,2006: Shannon Pelkey
; --Added consistency check between parameter(s) requested and source
; file detector when possible
; --Added traceability to input file by adding descriptor to ENVI hdr.
;
; Jan 22,2007 SMP: --Modified to create a .sav file
;
; Jun 27,2007 SMP:
; --Confirmed to work with tiles; output of all summary product files
;   is BIL
; --made parameters retain data CRISM NAN values
; --added SINDEX to replace D2400 and R440 to replace R410
;
; Jun 28,2007 SMP:
; --allow for spatial subsetting
;
; Aug 22, 2007  MFM:
;	--updated output header to pass through additional info from input 
;	  header, especially map_info.
;
; Aug 22, 2007  MFM:
;	--Reinstated catch with call to cat_error_report; previous catch 
;	  with help,!error_state was commented out.
;
; Oct 05,2007  MFM:
;	--If envi returns string wavelength array, convert to float. This
;	  prevents crash in mro_crism_summary_params() using string wavelengths 
;     where not allowed in arithmetic expression.
;
; Nov 19,2007  MFM: --Corrected routine name in cat_error_report call; was flatten_summary_params.
; Apr 29,2008  MFM: --Made insensitive to filename case.
; Feb 08,2009  MFM: --Fixed crash when bands bands list specified in input file header
;                     (made wavelength array match cube; affects VAR, R2700, BD2700)
; Feb 10,2009 MFM: --For checking file types from filename contents, 
;                    use only file basename, don't include path.
; Feb 22,2009 MFM: --Check geomap_lun before freeing to avoid error trap.
; Mar 10,2009 MFM: --Edits debugging bad bands crash in mro_crism_summary_params.pro,
;                    no changes.
; Mar 31,2009 FPS: --Moved wl um-->nm conversion out of processing loop
;   	    	   --Added check to allow um or nm in the input file wl vector
; Mar 31,2009 MFM: --Also move bad bands filter out of processing loop
;   	    	   --Use crism_waves2nm instead of crism_microns2nm, initial 
;                    units check there instead of here
; Mar 31,2009 MFM: --Disallow spectral subsetting. 
;                  --Use get_cat_header_data and write_cat_header.
; Feb 25,2010 MFM: --Allow user choice to plunge ahead if selection 
;                    and detector don't match.
; Nov 17,2010 MFM: --add header parameter CAT_PRODUCT_VERSION
; Nov 18,2010 MFM: --add four more eader parameters



pro mro_crism_summary_params_event, event

; Simple catch to prevent crash
catch, error
if (error NE 0) then begin
  	cat_error_report, routine_name='mro_crism_summary_params_event';
	return
endif

t1=systime(1)

; Define ENVI functions used so able to create a sav file:
FORWARD_FUNCTION envi_init_tile,magic_mem_check,widget_outfm,widget_multi,widget_auto_base,auto_wid_mng,envi_get_tile

widget_control, event.id, get_uvalue = uvalue

if (uvalue eq 'VNIR') then section_indx = 0
if (uvalue eq 'IR') then section_indx = 1
if (uvalue eq 'Joined') then section_indx = 2


;Select input file
envi_select, fid=in_fid, pos=in_pos, dims=in_dims, file_type=in_file_type, $
     /file_only, /no_spec, title = uvalue + ' Summary Params: Select Input File'
if (in_fid[0] eq -1) then begin
	print, uvalue + ' Summary Params: Cancel/Invalid FID'
	return
endif

;Gather some useful info for processing, as well as other header info for passthrough.
stat = get_cat_header_data(in_fid, NS=ns, NL=nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
	BBL=in_bbl, FWHM=fwhm, FNAME=in_filename, $
	INTERLEAVE=in_interleave, OFFSET=offset, $
	DATA_IGNORE_VALUE=data_ignore_value, DATA_TYPE=data_type, $
	DEF_BANDS=def_bands, DESCRIP=descrip, MAP_INFO=map_info, $
	CAT_START_TIME          = cat_start_time, $
	CAT_SCLK_START          = cat_sclk_start, $
	CAT_CRISM_OBSID         = cat_crism_obsid, $
	CAT_OBS_TYPE            = cat_obs_type, $
	CAT_PRODUCT_VERSION     = cat_product_version, $
	CAT_CRISM_DETECTOR_ID   = det, $
	CAT_BIN_MODE            = cat_bin_mode, $
	CAT_WAVELENGTH_FILTER   = cat_wavelength_filter, $
	CAT_CRISM_DETECTOR_TEMP = cat_crism_detector_temp, $
	CAT_CRISM_BENCH_TEMP    = cat_crism_bench_temp, $
	CAT_CRISM_HOUSING_TEMP  = cat_crism_housing_temp, $
	CAT_SOLAR_LONGITUDE     = cat_solar_longitude, $
	CAT_PDS_LABEL_FILE      = cat_pds_label_file, $
	CAT_SPECTRUM_RESAMPLED  = cat_spectrum_resampled, $
	CAT_TILE_WAVE_FILE      = cat_tile_wave_file, $
	CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
	CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
	CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
	CAT_HISTORY             = cat_history, $
	CAT_INPUT_FILES         = cat_input_files)


; Override in_pos, force keeping all bands:
in_pos = lindgen(nb)


; Get output dimensions in case user subsetted image:
in_ns=in_dims[2]-in_dims[1]+1
in_nl=in_dims[4]-in_dims[3]+1


;Verify that the selected file has associated wavelength vector
if (wl[0] EQ -1) then begin
	print, uvalue + ' Summary Params: No wavelength vector associated with selected file'
	return
endif


;Construct default output filenames
in_file_directory = file_dirname(in_filename,/mark_directory)
in_file_basename = file_basename(in_filename)
last_dot = strpos(in_file_basename, '.', /reverse_search)
if (last_dot gt 0) then begin
	in_file_extension = strmid(in_file_basename,last_dot)
	in_file_basename = strmid(in_file_basename,0,last_dot)
endif else begin
	in_file_extension = ''
endelse	


; If detector found, check for compatibility between file and selection:
if (det ne '') then begin
  if (section_indx eq 0) and (det ne 'S') then begin
    ok = dialog_message('VNIR product chosen for non-VNIR file. Continue?',$
                         title='Summary Parameter Error', /cancel)
    if (ok eq 'Cancel') then return
  endif
  if (section_indx eq 1) and (det ne 'L') then begin
    ok = dialog_message('IR product chosen for non-IR file. Continue?',$
                         title='Summary Parameter Error', /cancel)
    if (ok eq 'Cancel') then return
  endif
  if (section_indx eq 2) and (det ne 'J') then begin
    ok = dialog_message('Joined product chosen for non-joined file. Continue?',$
                         title='Summary Parameter Error', /cancel)
    if (ok eq 'Cancel') then return
  endif
  if (det eq 'J') then tile=1 else tile=0
endif else begin
  ok = dialog_message('Is this a tile?',$
                       title='Summary Parameter Question',/question,/cancel)
  if (ok eq 'Cancel') then return
  if (ok eq 'No') then tile=0 else tile=1
endelse



summary_params_file = in_file_directory + in_file_basename + '_params' + in_file_extension

vnir_summary_list = ['R770', 'RBR', 'BD530', 'SH600', 'BD640', 'BD860', 'BD920', 'RPEAK1', 'BDI1000VIS', $
					 'R440', 'IRR1'] ; NON-ATM Corrected
ir_summary_list = 	['BDI1000IR', 'IRA', 'OLINDEX', 'LCPINDEX', 'HCPINDEX', 'VAR', 'ISLOPE1', 'BD1435', 'BD1500', 'ICER1', $
					 'BD1750', 'BD1900', 'BDI2000', 'BD2100', 'BD2210', 'BD2290', 'D2300', 'SINDEX', 'ICER2', 'BDCARB', $
					 'BD3000', 'BD3100', 'BD3200', 'BD3400', 'CINDEX', $
					 'BD1270O2', 'BD1400H2O', 'BD2000CO2', 'BD2350', 'BD2600', 'IRR2', 'R2700', 'BD2700', 'IRR3'] ;NON-ATM Corrected

j_summary_list = ['R770', 'RBR', 'BD530', 'SH600', 'BD640', 'BD860', 'BD920', 'RPEAK1', 'BDI1000VIS', $
					 'R440', 'IRR1', $ ; NON-ATM Corrected
                     'BDI1000IR', 'IRA', 'OLINDEX', 'LCPINDEX', 'HCPINDEX', 'VAR', 'ISLOPE1', 'BD1435', 'BD1500', 'ICER1', $
					 'BD1750', 'BD1900', 'BDI2000', 'BD2100', 'BD2210', 'BD2290', 'D2300', 'SINDEX', 'ICER2', 'BDCARB', $
					 'BD3000', 'BD3100', 'BD3200', 'BD3400', 'CINDEX', $
					 'BD1270O2', 'BD1400H2O', 'BD2000CO2', 'BD2350', 'BD2600', 'IRR2', 'R2700', 'BD2700', 'IRR3'] ;NON-ATM Corrected

if (section_indx EQ 2) then begin
	summary_list = j_summary_list
	summary_ysize = 400
	summary_default = replicate(1b, n_elements(summary_list))
endif
if (section_indx EQ 1) then begin
	summary_list = ir_summary_list
	summary_ysize = 400
	summary_default = replicate(1b, n_elements(summary_list))
endif 
if (section_indx EQ 0) then begin
	summary_list = vnir_summary_list
	summary_ysize = 280
	summary_default = replicate(1b, n_elements(summary_list))
endif


;Gather user input info via ENVI auto managed widgets
auto_base = widget_auto_base(title = uvalue)
main_base = widget_base(auto_base, /row)
	summary_base = widget_base(main_base, /col)
		summary_label = widget_label(summary_base, value = uvalue)

		summary_select = widget_multi(summary_base, /auto_manage, list = summary_list, uvalue = 'summary_select', /no_range, $
								     default = summary_default, ysize = summary_ysize)
		summary_outfm = widget_outfm(summary_base, /auto_manage, /frame, default = summary_params_file, uvalue = 'summary_outfm')

result = auto_wid_mng(auto_base)
if (result.accept EQ 0) then begin
	print, ': Cancel
	return
endif


;Set summary product output parameters
summary_dims = in_dims
summary_dt = 4 ;floating point
summary_band_indx = where(result.summary_select EQ 1, summary_nb)

;memory check
mem_check = magic_mem_check(dims=in_dims, in_memory = result.summary_outfm.in_memory, $
			    out_dt=summary_dt, nb=summary_nb, out_name = result.summary_outfm.name)
if (mem_check.cancel EQ 1) then begin
	print, uvalue + ': Cancel'
	return
endif

;Store result of memory check in result structure
result.summary_outfm.in_memory = mem_check.in_memory
result.summary_outfm.name = mem_check.out_name

;Declare in-memory output array or open output file
if (result.summary_outfm.in_memory EQ 1) then begin
	summary_cube = make_array(in_ns, in_nl, summary_nb, value = 65535., /float)
	out_status_string = 'Output to Memory'
endif else begin
	openw, summary_lun, result.summary_outfm.name, /get_lun
	out_status_string = 'Output File: ' + result.summary_outfm.name
endelse

; If wl in microns convert to nm in wlnm
wlnm = crism_waves2nm(wl)

;===================
;  Dec 3,2009 MFM: Forcing all bands by setting in_pos = all bands above, 
;  previously kept all bands by faking bl all 1's, but now keeping bbl real, 
;  so eliminate the following block of code. Old approach might have been a 
;  bit of a misunderstanding too, thinking ENVI filtered on bbl instead 
;  of in_pos...

;; Pass only good bands; envi_get_tile filters out bands in bad band list, 
;; wavelength array must match cube
;kgood = where((in_bbl eq 1), ngood)
;if (ngood eq 0) then begin
;	ok = dialog_message('All bands bad. Aborting summary parameters.',$
;	           title='Summary Parameter Error')
;	return
;endif
;wlnm = wlnm[kgood]
;===================


;Initiate BIL tiling (accounts for spatial subset) - forces output interleave to BIL:
in_tile_id = envi_init_tile(in_fid, in_pos, num_tiles = num_in_tiles,interleave=1, $
			    xs = in_dims[1], xe = in_dims[2], ys = in_dims[3], ye = in_dims[4])

status_string_array = [out_status_string]
envi_report_init, status_string_array, base = status_base, title = uvalue+ ' Summary Params', /interupt
envi_report_inc, status_base, num_in_tiles

;Main processing loop - one iteration for each tile (line)
for i = long(0), num_in_tiles -1 do begin

	envi_report_stat, status_base, i, num_in_tiles, cancel=cancel

	if (cancel) then begin
		print, ': Cancel'
		envi_report_init, base = status_base, /finish
		if (result.summary_outfm.in_memory EQ 0) then begin
			if (n_elements(geomap_lun) gt 0) then free_lun, geomap_lun
		endif
		return
	endif

	;Grab tile data
	in_data = envi_get_tile(in_tile_id, i)
	in_data = reform(in_data, n_elements(in_data[*,0]), 1, n_elements(in_data[0,*]))
    ; Look at a random represenative band (present in all
    ; bin modes) to find CRISM NANS:
    in_data_row=in_data(*,0,10)
    wnan=where(in_data_row eq 65535.)

    ; Pass routine a BSQ array,detector indicator,wavelength
    ; array (in NANOMETERS), and parameters requested:

	summary_output_slice = mro_crism_summary_params(in_data, section_indx, wlnm, $
	                      index_vec = result.summary_select)
    if (wnan[0] ne -1) then summary_output_slice(wnan,*,*)=65535.

	;Write spectral slice result to BIL output file or BSQ output memory array
	if (result.summary_outfm.in_memory EQ 1) then begin
		summary_cube[*,i,*] = summary_output_slice
	endif else begin
		writeu, summary_lun, summary_output_slice
	endelse

endfor

envi_tile_done, in_tile_id
envi_report_init, base = status_base, /finish


;============================
;    HEADER PREP
;============================

; Append CAT history:
cat_history = cat_append_history(cat_history, 'SUM')

; Update input file list:
if (n_elements(cat_input_files) gt 0) then cat_input_files = strtrim(cat_input_files,2)
if (  (n_elements(cat_input_files) gt 0) && (cat_input_files[0] ne '-1') $
   && (cat_input_files[0] ne '')) then begin
	cat_input_files = [cat_input_files, file_basename(in_filename)]
endif else begin
	cat_input_files = [file_basename(in_filename)]
endelse


;============================
;    MAKE HEADER
;============================
;Register memory item in ENVI session or generate header for output file
if (result.summary_outfm.in_memory EQ 1) then begin
	envi_enter_data, summary_cube, data_ignore_value=65535., $
			bnames = summary_list[where(result.summary_select EQ 1)], $
			descrip='CAT CRISM summary parameters', map_info=in_map_info

endif else begin
	free_lun, summary_lun

	stat = write_cat_header(FNAME=result.summary_outfm.name, $
	         NS=in_ns, NL=in_nl, NB=summary_nb, $
	         BNAMES=summary_list[where(result.summary_select EQ 1)], $
        	 DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
        	 DATA_IGNORE_VALUE=65535.0, $
	         DESCRIP='CAT CRISM summary parameters', $
	         MAP_INFO=map_info, $
	         CAT_START_TIME=cat_start_time, $
	         CAT_SCLK_START=cat_sclk_start, $
	         CAT_CRISM_OBSID=cat_crism_obsid, $
	         CAT_OBS_TYPE=cat_obs_type, $
	         CAT_PRODUCT_VERSION=cat_product_version, $
	         CAT_CRISM_DETECTOR_ID=det, $
	         CAT_BIN_MODE=cat_bin_mode, $
	         CAT_WAVELENGTH_FILTER=cat_wavelength_filter, $
	         CAT_CRISM_DETECTOR_TEMP=cat_crism_detector_temp, $
	         CAT_CRISM_BENCH_TEMP=cat_crism_bench_temp, $
	         CAT_CRISM_HOUSING_TEMP=cat_crism_housing_temp, $
	         CAT_SOLAR_LONGITUDE=cat_solar_longitude, $
	         CAT_PDS_LABEL_FILE=cat_pds_label_file, $
	         CAT_HISTORY=cat_history, $
	         CAT_INPUT_FILES=cat_input_files)

	; Report problems with header if any indicated by status returned:
	if ((stat ne 0) and (~keyword_set(batch))) then begin
		if (stat eq -1) then begin
			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
			      + 'Required keywords for write_cat_header undefined.'
			ok = dialog_message(msg, title='Header Error')
		endif else if (stat eq -2) then begin
			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
		 	     + 'Header already exists.'
			ok = dialog_message(msg, title='Header Error')
		endif else begin
			msg = 'Problem writing header for '+file_basename(atp_setup.datafile) + string(10b) $
			      + 'Unspecified error.'
			ok = dialog_message(msg, title='Header Error')
		endelse
	endif
endelse


ender:

t2=systime(1)
etime=(t2-t1)/60.
PRINT,'Elapsed time (min): ',strtrim(etime,2)


END


;+
; NAME:
;      Spectral Summary Products
;      note: this code DOES NOT ACCOUNT FOR SPECTRAL SMILE
;            (uses only the sweet spot wave array associated w/ENVI file)
;
; PURPOSE:
;
;       Hyperspectral summary parameter event handler for CRISM/CAT.
;
; AUTHOR:
;		Frank Morgan
;		Johns Hopkins Univeristy Applied Physics Laboratory (2005-)
;			frank dot morgan at jhuapl dot edu
;
;		(Adapted from similar code for multispectral parameters by S. Pelkey, 
;		as modified by B. Ehlmann for hyperspectral parameters.)
;
; CATEGORY:
;		ENVI Utility
;
; CALLING SEQUENCE:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORDS:
;
;
; OUTPUTS:
;
; COMMON BLOCKS:
;		NONE
;
; DEPENDENCIES:
;
; PROCEDURE:
;
; EXAMPLE:
;
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
; May 06, 2008   MFM  --First version
; Mar 31, 2009   MFM  --Use crism_waves2nm instead of crism_microns2nm, initial 
;                       units check there instead of here
; Dec 03, 2009   MFM  --Use get_cat_header_data and write_cat_header
; Feb 25, 2010   MFM  --Allow user choice to plunge ahead if selection 
;                       and detector don't match.
; Nov 17, 2010   MFM: --add header parameter CAT_PRODUCT_VERSION
; Nov 18, 2010   MFM: --add four more header parameters
;
;###########################################################################
;
; LICENSE
;
;###########################################################################

pro mro_crism_summary_params_hyper_event, event



; Simple catch to prevent crash
catch, error
if (error NE 0) then begin
  	cat_error_report, routine_name='mro_crism_summary_params_hyper_event'
	return
endif

; Define ENVI functions used so able to create a sav file:
FORWARD_FUNCTION envi_init_tile,magic_mem_check,widget_outfm,widget_multi,widget_auto_base,auto_wid_mng,envi_get_tile

widget_control, event.id, get_uvalue = event_uvalue

section_string_array = ['VNIR', 'IR']
if (strpos(event_uvalue, 'VNIR') EQ -1) then section_indx = 1 else section_indx = 0
if (strpos(event_uvalue, 'J') ne -1) then begin
    ok = dialog_message('Not expecting hyperspectral joined data. Aborting procedure.',$
                       title='Hyperspectral Summary Parameter Error')
    return
endif


;Select input file
envi_select, fid=in_fid, $      ; see note on in_pos below
			 dims=in_dims, file_type=in_file_type, /file_only, /no_spec, $
			 title = event_uvalue + ': Select Input File'
if (in_fid[0] eq -1) then begin
	print, event_uvalue + ': Cancel/Invalid FID'
	return
endif


;Gather some useful info
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

if (stat ne 0) then begin
    ok = dialog_message('Failure reading header data. Aborting procedure.',$
                       title='Hyperspectral Summary Parameter Error')
    return
endif


; note: in envi_select routine above in_pos was selecting only 
; non-bad bands. For this to work as coded, *all* bands must be used.
; So in_pos is created using the number of bands, nb.
in_pos = lindgen(nb)

; Get output dimensions in case user subsetted image:
in_ns = in_dims[2]-in_dims[1]+1
in_nl = in_dims[4]-in_dims[3]+1


;Verify that the selected file has associated wavelength vector
if (wl[0] EQ -1) then begin
	print, event_uvalue + ': No wavelength vector associated with selected file'
	return
endif


;Construct default output filenames
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


; If possible, check consistency between filenames and params
; selected (case insensitive... MFM):
if (section_indx eq 0) and (det eq 'L') then begin
    ok = dialog_message('VNIR product chosen for IR detector. Aborting procedure.',$
                       title='Hyperspectral Summary Parameter Error', /cancel)
    if (ok eq 'Cancel') then return
endif
if (section_indx eq 1) and (det eq 'S') then begin
    ok = dialog_message('IR product chosen for VNIR detector. Aborting procedure.',$
                       title='Hyperspectral Summary Parameter Error', /cancel)
    if (ok eq 'Cancel') then return
endif

summary_params_file = in_file_directory + in_file_basename + '_hyparam' + in_file_extension



; Get parameter list from the summary params code
summary_list = 1
dum = mro_crism_summary_params_hyper(dummy, section_indx, band_names=summary_list)

; Give up if there are no available parameters:
if (max(strlen(strtrim(summary_list))) eq 0) then begin
	ok = dialog_message('No hyperspectral parameters available. Aborting procedure.',$
              title='Hyperspectral Summary Parameter Error')
    return
endif

; Set up other detector-dependent parameters
if (section_indx EQ 1) then begin
	summary_ysize = 450
	summary_default = replicate(1b, n_elements(summary_list))
endif else begin
	summary_ysize = 280
	summary_default = replicate(1b, n_elements(summary_list))
endelse

;Gather user selections and output datafile info via ENVI auto managed widgets
auto_base = widget_auto_base(title = event_uvalue)
main_base = widget_base(auto_base, /row)
	summary_base = widget_base(main_base, /col)
		summary_label = widget_label(summary_base, value = event_uvalue)

		summary_select = widget_multi(summary_base, /auto_manage, list = summary_list, uvalue = 'summary_select', /no_range, $
								     default = summary_default, ysize = summary_ysize)
		summary_outfm = widget_outfm(summary_base, /auto_manage, /frame, default = summary_params_file, uvalue = 'summary_outfm')

result = auto_wid_mng(auto_base)

if (result.accept EQ 0) then begin
	print, ': Cancel'
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
	print, event_uvalue + ': Cancel'
	return
endif

;Store result of memory check in result structure
result.summary_outfm.in_memory = mem_check.in_memory
result.summary_outfm.name = mem_check.out_name


;Declare in-memory output array or open output file
if (result.summary_outfm.in_memory EQ 1) then begin
	summary_cube = make_array(in_ns, in_nl, summary_nb, value = !values.f_nan, /float)
	out_status_string = 'Output to Memory'
endif else begin
	openw, summary_lun, result.summary_outfm.name, /get_lun
	out_status_string = 'Output File: ' + result.summary_outfm.name
endelse


;Initiate BIL tiling - matches output interleave to BIL:
in_tile_id = envi_init_tile(in_fid, in_pos, $
				num_tiles = num_in_tiles,interleave=1, $
			    xs = in_dims[1], xe = in_dims[2], ys = in_dims[3], ye = in_dims[4])



status_string_array = [out_status_string]
envi_report_init, status_string_array, base = status_base, $
		title = event_uvalue + ' Hyperspectral Summary Params', /interupt
envi_report_inc, status_base, num_in_tiles

;Main processing loop - one iteration for each tile
wlnm = crism_waves2nm(wl)
for i = 0L, num_in_tiles-1 do begin

	envi_report_stat, status_base, i, num_in_tiles, cancel=cancel
	if (cancel) then begin
		print, ': Cancel'
		envi_report_init, base = status_base, /finish
		if (result.summary_outfm.in_memory EQ 0) then begin
			free_lun, geomap_lun
		endif
		return
	endif

	;Grab tile data
	in_data = envi_get_tile(in_tile_id, i)
	in_data = reform(in_data, n_elements(in_data[*,0]), 1, n_elements(in_data[0,*]))

    ; Hand routine a BSQ array,detector indicator,wavelength
    ;  array (in NANOMETERS), and parameters requested:
	summary_output_slice = mro_crism_summary_params_hyper(in_data, section_indx, wlnm, $
	    index_vec = result.summary_select)

	;Write spectral slice result to BIL output file or BSQ output memory array
	if (result.summary_outfm.in_memory EQ 1) then begin
		summary_cube[*,i,*] = summary_output_slice
	endif else begin
		writeu, summary_lun, summary_output_slice
	endelse

endfor

envi_tile_done, in_tile_id
envi_report_init, base=status_base, /finish


;============================
;    HEADER PREP
;============================

; Append CAT history:
cat_history = cat_append_history(cat_history, 'HSM')

; Update input file list:
if (n_elements(cat_input_files) gt 0) then cat_input_files = strtrim(cat_input_files,2)
if (  (n_elements(cat_input_files) gt 0) && (cat_input_files[0] ne '-1') $
   && (cat_input_files[0] ne '')) then begin
	cat_input_files = [cat_input_files, file_basename(in_filename)]
endif else begin
	cat_input_files = [file_basename(in_filename)]
endelse


;Register memory item in ENVI session or generate header for output file
if (result.summary_outfm.in_memory EQ 1) then begin
	envi_enter_data, summary_cube, data_ignore_value=65535., $
	          bnames = summary_list[where(result.summary_select EQ 1)], $
	          descrip='CAT CRISM hyperspectral summary parameters', $
	          map_info=map_info
endif else begin
	free_lun, summary_lun
	stat = write_cat_header(FNAME=result.summary_outfm.name, $
	         NS=in_ns, NL=in_nl, NB=summary_nb, $
	         BNAMES=summary_list[where(result.summary_select EQ 1)], $
        	 DATA_TYPE=4, INTERLEAVE=1, OFFSET=0, $
        	 DATA_IGNORE_VALUE=65535.0, $
	         DESCRIP='CAT CRISM hyperspectral summary parameters', $
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

return
END


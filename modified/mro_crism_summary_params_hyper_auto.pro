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

pro mro_crism_summary_params_hyper_auto, fname, summary_params_file, summary_list

; Define ENVI functions used so able to create a sav file:
FORWARD_FUNCTION envi_init_tile,magic_mem_check,widget_outfm,widget_multi,widget_auto_base,auto_wid_mng,envi_get_tile

envi_open_file, fname, r_fid=in_fid

;Gather some useful info
stat = get_cat_header_data(in_fid, NS=in_ns, NL=in_nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
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

; note: in envi_select routine above in_pos was selecting only 
; non-bad bands. For this to work as coded, *all* bands must be used.
; So in_pos is created using the number of bands, nb.
in_pos = lindgen(nb)
in_dims = [-1, 0, in_ns-1, 0, in_nl-1]

;Verify that the selected file has associated wavelength vector
if (wl[0] EQ -1) then begin
	print, event_uvalue + ': No wavelength vector associated with selected file'
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

; Get parameter list from the summary params code
; summary_list = 1
; dum = mro_crism_summary_params_hyper(dummy, section_indx, band_names=summary_list)
section_indx = 1
summary_nb = size(summary_list)
summary_nb = summary_nb[1]

; Set up other detector-dependent parameters
if (section_indx EQ 1) then begin
	summary_ysize = 450
	summary_default = replicate(1b, n_elements(summary_list))
endif else begin
	summary_ysize = 280
	summary_default = replicate(1b, n_elements(summary_list))
endelse

openw, summary_lun, summary_params_file, /get_lun
out_status_string = 'Output File: ' + summary_params_file

;Initiate BIL tiling - matches output interleave to BIL:
in_tile_id = envi_init_tile(in_fid, in_pos, $
				num_tiles = num_in_tiles,interleave=1, $
			    xs = in_dims[1], xe = in_dims[2], ys = in_dims[3], ye = in_dims[4])

status_string_array = [out_status_string]
envi_report_init, status_string_array, base = status_base, $
		title = ' Hyperspectral Summary Params', /interupt
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
	    index_vec = summary_select)

	;Write spectral slice result to BIL output file or BSQ output memory array
	; if (result.summary_outfm.in_memory EQ 1) then begin
		; summary_cube[*,i,*] = summary_output_slice
	; endif else begin
    writeu, summary_lun, summary_output_slice
	;endelse

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


free_lun, summary_lun
stat = write_cat_header(FNAME=summary_params_file, $
         NS=in_ns, NL=in_nl, NB=summary_nb, $
         BNAMES=summary_list[where(summary_select EQ 1)], $
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

return

ENVI_CLOSE_DISPLAY, 0
envi_file_mng, id=in_fid, /remove
ENVI_CLOSE_DISPLAY, 1

END


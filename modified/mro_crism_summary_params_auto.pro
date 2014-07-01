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

pro mro_crism_summary_params_auto, fname, summary_params_file, section_indx, summary_select, summary_list

; Define ENVI functions used so able to create a sav file:
FORWARD_FUNCTION envi_init_tile,magic_mem_check,widget_outfm,widget_multi,widget_auto_base,auto_wid_mng,envi_get_tile

envi_open_file, fname, r_fid=in_fid

;Gather some useful info for processing, as well as other header info for passthrough.
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

; Override in_pos, force keeping all bands:
in_pos = lindgen(nb)
in_dims = [-1, 0, in_ns-1, 0, in_nl-1]  

;Verify that the selected file has associated wavelength vector
if (wl[0] EQ -1) then begin
	print, 'No wavelength vector associated with selected file'
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

summary_nb = total(summary_select)

summary_ysize = 400
summary_default = replicate(1b, n_elements(summary_list))

openw, summary_lun, summary_params_file, /get_lun
out_status_string = 'Output File: ' + summary_params_file

; If wl in microns convert to nm in wlnm
wlnm = crism_waves2nm(wl)

;Initiate BIL tiling (accounts for spatial subset) - forces output interleave to BIL:
in_tile_id = envi_init_tile(in_fid, in_pos, num_tiles = num_in_tiles,interleave=1, $
			    xs = in_dims[1], xe = in_dims[2], ys = in_dims[3], ye = in_dims[4])

status_string_array = [out_status_string]
envi_report_init, status_string_array, base = status_base, title = 'Summary Params', /interupt
envi_report_inc, status_base, num_in_tiles

;Main processing loop - one iteration for each tile (line)
for i = long(0), num_in_tiles -1 do begin

	envi_report_stat, status_base, i, num_in_tiles, cancel=cancel

	if (cancel) then begin
		print, ': Cancel'
		envi_report_init, base = status_base, /finish
		;if (result.summary_outfm.in_memory EQ 0) then begin
	  if (n_elements(geomap_lun) gt 0) then free_lun, geomap_lun
		;endif
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
	                      index_vec = summary_select)
    if (wnan[0] ne -1) then summary_output_slice(wnan,*,*)=65535.

	;Write spectral slice result to BIL output file or BSQ output memory array
;	if (result.summary_outfm.in_memory EQ 1) then begin
;		summary_cube[*,i,*] = summary_output_slice
;	endif else begin
    writeu, summary_lun, summary_output_slice
	;endelse

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

;endif else begin
free_lun, summary_lun

stat = write_cat_header(FNAME=summary_params_file, $
         NS=in_ns, NL=in_nl, NB=summary_nb, $
         BNAMES=summary_list[where(summary_select EQ 1)], $
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

ender:

ENVI_CLOSE_DISPLAY, 0
envi_file_mng, id=in_fid, /remove
ENVI_CLOSE_DISPLAY, 1

END


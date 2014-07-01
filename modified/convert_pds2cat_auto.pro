; ENVI event handler routine to read in CRISM PDS files
;  (EDRs, TRRs, RTR, MRR)
; and convert them to ENVI .img files in appropriate
; format with necessary hdr info.
;
; NOTE: TILES ARE WRITTEN AS BSQ; NON-TILES AS BIL
;
; ASSUMPTIONS:
;	Input must be raw PDS. This routine assumes that the byte order of 
;	input CRISM PDS data is always:
;		big endian for integer point data (data_type=2)
;		litle endian for floating point data (data_type=4)
;	If this is ever violated then bytes will be swapped (or not) when 
;	they shouldn't (should) be on the relevant swap_endian_inplace
;	statement. Then, data will be written to file in the non-native order.
;	In write_cat_header, the byte_order flag is set according to the 
;	native byte order. So attempting to read the output file according 
;	to the header will corrupt the data.
;
; Uses: 
;
; Created: October, 2006
; By: Shannon Pelkey
;
; Modification History:
; Oct 31, 2006: SMP 
;  --Revamped so data never read into memory if writing to output file
;  --Uses an associate variable to access data instead of readpds
;  --Added endian handling
;  --Allowed user to select output location.
;  --Added more refined check of platform.
;  --Added simple error catch.
;
; Jan 2, 2007 SMP:
; --Made wavelength assignment consistent with that used in
;   read_crism_event
;
; Feb 5, 2007 SMP:
; --Added the CDR file used for sweet-spot wavelengths to header
;   for traceability.
;
; Feb 15,2007 SMP: --Fixed bug associated with non-PDS compliant file
; identification
;
; Feb 16,2007 SMP: --Modified to create a .sav file
; Mar 21,2007 SMP: --Modified to be used with single detector files
;                    only (no MRRs or MTRs); added file overwrite check.
; May 10,2007 SMP: --Added use of CDR6-BW for standard band widths.
;                    Uses just first column for both S and L
;                    detectors.
; June 13,2007 SMP: --Modified to work with MRDR tiles; outputs as BSQ.
;
;  Aug 17,2007 MFM: --corrected map + projection info for tiles; now on CRISM MRDR
;
;  Aug 21,2007 MFM: --restored "catch" error trap, with call to cat_error_report
;
; Aug 23,2007 MFM: 
; --Use crism_nm2microns and crism_microns2nm for all wavelength conversions
;   to leave 65535 alone.
;
; Oct 04,2007 MFM: --removed duplicated "openw,uhdr,outfilehdr"
; Apr 29,2008 MFM: --Made insensitive to filename case to handle lower case PDS files
; Feb 10,2009 MFM: --Use file_basename part of filename only for data type check.
; Mar 31,2009 MFM: --Use crism_waves2microns for wavelength conversion
; Apr 14,2009 MFM: --When detect non-original PDS filename, only return if user cancels.
; Jul 09,2009 MFM: --Check cat_file_check returns against 0 instead of using logical negation
; Jul 27,2009 MFM: --Use load_tile_waves to get tile wavelengths; allows PDS search
; Aug 21,2009 MFM: --Change load_tile_waves keyword to used_wvfile.
; Sep 16,2009 MFM: --Replace find_cdr6file with find_cdr
; Sep 17,2009 MFM: --find_lblfile returns '' not '-1' on fail
; Nov 02,2009 MFM: --Fail if ANY file not spectral data, not only if ALL files not spectral data
; Nov 02,2009 MFM: --Clean up some string processing.
; Nov 03,2009 MFM: --Replace the 65535 fwhm band with next band value instead of 0.0.
; Nov 05,2009 MFM: --Header: make wvlbl, bwlbl, def_bands, interleave, etc numerical.  
;                    Use write_cat_header for header creation.  Map info always to 
;                    structures now, no direct string output.
; Nov 10,2009 MFM: --Added more CAT keywords to header (temps, solar lon)
; Nov 26,2009 MFM: --Eliminate byte_order check & keyword in write_cat_header.
; Nov 27,2009 MFM: --Add bad bands to header setup. Use get_cat_header_data 
;                    to get info for header; eliminates a lot of old code.
;                    Move file checks inside loop. 
; Dec 04,2009 MFM: --Check min(def_bands) instead of total(def_bands) 
; Jan 29,2010 MFM: --Noted byte order assumption.
; Jan 29,2010 MFM: --Add lower case filter.
; Nov 17,2010 MFM: --Add parameter cat_product_version.
; Nov 18,2010 MFM: --Four new CAT header parameters.
; Nov 19,2010 MFM: --add CAT_WA_WAVE_FILE to output header
; Nov 24,2010 MFM: --nothing new- debugging
; Mar 16,2012 MFM: --handle MTRDR data
;
;========================================================================================

pro convert_pds2cat_auto, fname, outdir

memory = 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; START MAIN PROCESSING LOOP:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for i=0,n_elements(fname)-1 do begin

	;==========================
	;    FILE CHECKS
	;==========================
	if (cat_file_check(fname[i], fnm) eq 0) then begin
		ok = dialog_message( file_basename(fname[i]) + string(10b) + $
		                     'not found, skipping.', $
		                     title='Convert To CAT File Message', /cancel)
		if (ok eq 'Cancel') then return
		continue
	endif else begin
		input_file = fnm[0]    ; Use this from here, case match
	endelse

	if (~file_is_pds(input_file)) then begin
		ok = dialog_message(file_basename(input_file) + string(10b) + $
		                   ' is not raw PDS. Skipping it.',$
		                   title='Convert To CAT File Message',/cancel)
		if (ok eq 'Cancel') then return
		continue
	endif
	
	if (~pdsfile_is_spectral(input_file)) then begin
		ok = dialog_message(file_basename(input_file) + string(10b) + $
		                   ' does not appear to be spectral data.' + string(10b) +$
		                   ' Convert anyway?',$
		                   title='Convert To CAT File Message',/question,/cancel)
		if (ok eq 'Cancel') then return
		if (ok eq 'No') then continue
	endif

	if (memory eq 0) then begin
		last_dot = strpos(input_file,'.',/reverse_search)
		basename = file_basename(strmid(input_file,0,last_dot))
		outfile = outdir + basename + '_CAT.img'
		fnd = cat_file_check(outfile, outfound)
		if (fnd ne 0) then begin
			ok = dialog_message('This file already exists: '+ outfound[0] + $
			                   ' Would you like to overwrite it?',$
			                   title='Convert To CAT File Message',/cancel,/question)
			if (ok eq 'Cancel') then return
			if (ok eq 'No') then continue
		endif
	endif

	print,''
	print,'Processing file: '+ input_file

	fnmup = file_basename(strupcase(input_file))


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Get data for virtual ENVI header from label
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	stat = get_cat_header_data(nofid, NS=ns, NL=nl, NB=nb, $
	         WL=wl, WAVELENGTH_UNITS=wu, BBL=bbl, FWHM=fwhm, $
	         BNAMES=bnames, INTERLEAVE=interleave, $
	         OFFSET=offset, BYTE_SWAP=byte_swap, BYTE_ORDER=byte_order, $
	         DATA_IGNORE_VALUE=data_ignore_value, DATA_TYPE=data_type, $
	         DEF_BANDS=def_bands, DESCRIP=descrip, $
	         MAP_INFO=map_info, $
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
	         CAT_INPUT_FILES         = cat_input_files, $
	         LABEL_ONLY=input_file )

	; Some checks
	if ( (stat lt 0) || (ns lt 0) || $
	     (nl lt 0)   || (nb lt 0) || $
	     (interleave lt 0) || (data_type lt 0) ) then begin
		ok = dialog_message('Required LBL info not found: use ENVI default opening procedure?',$
                           title='Read CRISM Message',/cancel)
		if (ok eq 'Cancel') then return
		envi_open_file,input_file, R_FID = fid
		continue
	endif

	if (det eq 'L') then begin
		; Reverse IR wavelengths
		cat_ir_waves_reversed = 'YES'
		wl[0] = 4.0            ;replace 65535.
		if (fwhm[0] ne -1) then fwhm[0] = fwhm[1]    ;replace 65535.   (instead of 0.0; FM, 11/3/09)
		wl = reverse(wl)
		fwhm = reverse(fwhm)
		bbl = reverse(bbl)
		def_bands = (reverse(indgen(nb)))[def_bands-1] + 1
	endif


	;;;;;;;;;;;
	; GET DATA:
	;;;;;;;;;;;
	; If in-memory, define BSQ array to hold data; if writing to file
	;  find file names and open file for writing:
	if (memory) then begin
		if (data_type eq 4) then begin
			mem_cube = fltarr(ns,nl,nb)
		endif else begin
			mem_cube=intarr(ns,nl,nb)
		endelse
		out_status_string = 'Output to Memory'
	endif else begin
		openw, out_lun, outfile, /get_lun
		out_status_string = 'Output File: ' + outfile
    endelse

	;Assemble string to be displayed in ENVI status window
	status_string_array = ['Input File: ' + fname[i], out_status_string]
	envi_report_init, status_string_array, base = status_base, title = 'Convert Format: PDS to CAT', /interupt
	envi_report_inc, status_base, n_elements(nl)

	; Open the PDS file and create an associate variable referencing the file;
	;  Remember data types different for EDRs and TRRs but both are BIL,
	;  tiles are BSQ
	openr,in_lun, input_file, /get_lun
	if (    (pdsfile_is_tile(input_file))   $
	     || (pdsfile_is_mtrdr(input_file,/allow_unproj)) ) then begin
		if (data_type eq 4) then begin
			assocdata = ASSOC(in_lun,fltarr(ns,nl,/nozero))
		endif else begin
			assocdata = ASSOC(in_lun,intarr(ns,nl,/nozero))
		endelse
		;Main processing loop for TILES: one iteration for each band
		for j=0,nb-1 do begin
			envi_report_stat, status_base, j, nb, cancel=cancel
			; If canceled:
			if (cancel) then begin
				print,'Convert Format: PDS to CAT: Cancel'
				envi_report_init, base = status_base, /finish
				free_lun, in_lun
				if (~memory) then free_lun, out_lun
				return
			endif
			;Grab a data frame from the file and correct for endian-ness:
			dataframe = assocdata[j]
			if (data_type eq 2) then swap_endian_inplace,dataframe,/swap_if_little_endian
			if (data_type eq 4) then swap_endian_inplace,dataframe,/swap_if_big_endian

			;Store in BSQ image cube or write to file:
			if (memory) then begin
				mem_cube[*,*,j] = dataframe
			endif else begin
				writeu, out_lun, dataframe
			endelse
		endfor
	endif else begin
		; Non-tile processing
		if (data_type eq 4) then begin
			assocdata = ASSOC(in_lun,fltarr(ns,nb,/nozero))
		endif else begin
			assocdata = ASSOC(in_lun,intarr(ns,nb,/nozero))
		endelse 
		;Main processing loop for NON-TILES: one iteration for each line
		for j=0,nl-1 do begin
			envi_report_stat, status_base, j, nl, cancel=cancel
			; If canceled:
			if (cancel) then begin
				print,'Convert Format: PDS to CAT: Cancel'
				envi_report_init, base = status_base, /finish
				free_lun, in_lun
				if (~memory) then free_lun, out_lun
				return
			endif
			;Grab a data frame from the file and correct for endian-ness:
			dataframe = assocdata[j]
			if (data_type eq 2) then swap_endian_inplace,dataframe,/swap_if_little_endian
			if (data_type eq 4) then swap_endian_inplace,dataframe,/swap_if_big_endian

			if (det eq 'L') then begin
				dataframe[*,0] = 0                ;out of commission band
				dataframe = reverse(dataframe,2,/overwrite) 
			endif

			;Store in BSQ image cube or write to file:
			if (memory EQ 1) then begin
				mem_cube[*,j,*] = dataframe
			endif else begin
				writeu, out_lun, dataframe
			endelse
		endfor
	endelse

	envi_report_init, base = status_base, /finish
	free_lun,in_lun
	if (~memory) then free_lun, out_lun

	if ((n_elements(def_bands) eq 3) && (min(def_bands) gt 0)) then begin
		; For header... see note in write_cat_header prolog about 
		; decrementing def_bands
		def_bands -= 1
	endif else begin
		def_bands = [0,0,0]
	endelse

	if (memory) then begin
		ENVI_ENTER_DATA, mem_cube, BBL=bbl,WAVELENGTH_UNIT=wu,WL=wl,FWHM=fwhm,$
		         DATA_IGNORE_VALUE=data_ignore_value, $
		         DEF_BANDS=def_bands, DESCRIP='CAT CRISM DATA', $
                 MAP_INFO=map_info
	endif else begin
		; see note in get_cat_header_data prolog about decrementing 
		; def_bands...
		stat = write_cat_header(FNAME=outfile, NS=ns, NL=nl, NB=nb, $
	        	 DATA_TYPE=data_type, INTERLEAVE=interleave, OFFSET=0, $
	        	 BBL=bbl, DATA_IGNORE_VALUE=65535.0, $
		         DEF_BANDS=def_bands, DESCRIP='CAT CRISM DATA', $
		         WAVELENGTH_UNIT=wu, WL=wl, FWHM=fwhm, MAP_INFO=map_info, $
		         CAT_START_TIME= cat_start_time, $
		         CAT_SCLK_START= cat_sclk_start, $
		         CAT_CRISM_OBSID= cat_crism_obsid, $
		         CAT_OBS_TYPE= cat_obs_type, $
		         CAT_PRODUCT_VERSION=cat_product_version, $
		         CAT_CRISM_DETECTOR_ID=det, $
		         CAT_BIN_MODE=cat_bin_mode, $
		         CAT_WAVELENGTH_FILTER=cat_wavelength_filter, $
		         CAT_CRISM_DETECTOR_TEMP=cat_crism_detector_temp, $
		         CAT_CRISM_BENCH_TEMP=cat_crism_bench_temp, $
		         CAT_CRISM_HOUSING_TEMP=cat_crism_housing_temp, $
		         CAT_SOLAR_LONGITUDE=cat_solar_longitude, $
		         CAT_PDS_LABEL_FILE=cat_pds_label_file, $
		         CAT_SPECTRUM_RESAMPLED=cat_spectrum_resampled, $
		         CAT_WA_WAVE_FILE=cat_wa_wave_file,  $
		         CAT_TILE_WAVE_FILE=cat_tile_wave_file, $
		         CAT_SWEETSPOT_WAVE_FILE=cat_sweetspot_wave_file, $
		         CAT_IR_WAVES_REVERSED=cat_ir_waves_reversed, $
		         CAT_HISTORY='CAT', $
		         CAT_INPUT_FILES=[file_basename(input_file)] )

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

endfor

;envi_file_mng, id=fid, /remove

end

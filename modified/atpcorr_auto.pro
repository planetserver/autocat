; ENVI event handler routine to correct a CRISM RA or IF TRDR or RTR
;  for atm,therm,and/or phot effects using proper DDR or DRR and CDR info.
;
; Program can handle CAT-ENVI files and tiles.
;
; NOTE: OUTPUT IS BIL FOR NON-TILE; BSQ FOR TILES
;
; Calls:    read_crism_lbl
;
; Created: March 2006
; By: Shannon Pelkey
;
; Modification History:
; Sep 21, 2006, SMP
;  Added ability to start with I/F
;
; Oct 19, 2006, SMP
;  Added ability to work with CAT-ENVI files.
;
; Nov 07, 2006, SMP
;  Totally updated and revamped
;
; Nov 16, 2006 SMP:
;  Added capability for atmospheric scaling correction to handle
;  spectral smile. Temporarily using tranmission spectrum in the form
;  of IDL save files until CDR4 files are created.
;
; Jan 01, 2007 SMP:
;  Changed program to access the CDR4 version of the atmorap
;  transmission spectrum derived from CRISM Olympus scan.
;
; Feb 16, 2007 SMP: --Modified to create a .sav file
;                   --Changed use of 'sort' to 'bsort' to avoid issue
;                     on Windows.
; May 10,2007 SMP: --Added use of CDR6-BW for standard band widths.
;                    Uses just first column for both S and L
;                    detectors.
;                  --Accounted for NAN in CDR4-AT L_v0 file.
; May 23,2207 SMP: 
; --Added CDR4-AT file used to .hdr file for traceability.
;
; June 25, 2007: SMP
; --Incorporated changes made to handle BSQ tile processing.
;
; Aug 13, 2007: Frank Morgan (MFM)
; --Correct "map info" and "projection info" fields for ENVI headers 
;   when processing map tiles.
; --Improved error messages (printing help,/struct,!error_state truncated messages;
;   now print full message with some traceback using cat_error_report().
; --Added progress bar for BSQ reinterleave
;
; Aug 16,2007 MFM: 
; --Bug fix; for photometric correction only, failed to get wavelengths 
;   for header. (Fix is a kluge.)
;
; Aug 17,2007 MFM: 
; --added if (tile) conditional on map/projection output to header, and
;   made map parameters read from LBL double 
;
; Aug 21,2007 MFM: 
; --patched wavelength conversion for wvframe assignment. Sometimes 
;   (eg if file previously opened using "Open CRISM File") wavelengths 
;   are *already* nm, and applying correction again renders wvframe 
;   invalid and corrected IR bands are all 65535. The patch is to 
;   only convert if in_wavesall is in um. 
;
; Aug 23,2007 MFM: 
; --Use crism_nm2microns and crism_microns2nm for all wavelength 
;   conversions to leave 65535 alone.
;
; Aug 23,2007 MFM:
; --Use median(wl) instead of max(wl) to decide wavelength units for header.
;   (This assumes no wholesale corruption of wavelength array.)
;   Together with the previous note, this fixes the double-conversion 
;   of nm -> um when one wavelength invalid (65535 -> 65.535 > 10) 
;   that broke the wavelength conversion patch in some cases.
;
; Mar 20,2008 MFM:
; --Added ATPTMPDIR parameter = temp directory taken from envi config 
;   if available; asks to continue writing to current diectory if not.
;
; Mar 20,2008 MFM:
; --Added microseconds from systime call to temp filename placeholder to reduce
;   chance of conflict between multiple sessions running on same system.
;
; Mar 28,2008 MFM: --Fixed cases where LUNs were allocated with get_lun and never freed.
; Apr 27,2008 MFM: --Made insensitive to filename case
; Sep 01,2008 MFM: --Use cat_auxfile_path() to get config-defined aux file path
; Oct 17,2008 MFM: --Use non-65535 wavelengths to check nm vs. um for tile conversion
; Nov 14,2008 MFM: --Restructured indentation and if blocks for clarity
;                  also see "-------FIX HERE------" more attentioon needed
; Jan 19,2009 MFM: --Major revision; call separate routines to handle tiles and non-tiles.
; Jan 21,2009 MFM: --Trap MRRAL correction; not likely to really want to do that.
; Jan 29,2009 MFM: --strtrim the VSID that goes to the processing subroutines.
; Jan 29,2009 MFM: --Define filenames when working in memory
; Jan 29,2009 MFM: --Add atm scaling band-set selection
; Feb 02,2009 MFM: --Trap atmospheric scaling of radiance data
; Feb 05,2009 MFM: --No changes, just debugging other code
; Feb 10,2009 MFM: --For radiance/I/F decision, look at file basename only, 
;                    and look for _RA and _IF instead of just RA, IF to 
;                    prevent catching text in path. Use dataupbase for other
;                    filename based checks too.
; Feb 10,2009 MFM: --Nope... have to look for RA and IF without underscore so it 
;                    does the right thing for tiles
; Mar 10,2009 MFM: --Disable spectral subsetting on envi_select call
; Mar 12,2009 MFM: --Uncommented error catch
; Jul 23,2009 MFM: --Correct logic on undefined header previous action detection
; Sep 17,2009 MFM: --Documentation edit
; Sep 17,2009 MFM: --find_lblfile returns '' not '-1' on fail
; Sep 22,2009 MFM: --made atp_tile and atp_nontile calls for functions
; Nov 02,2009 MFM: --Get bad bands, add to output structure.
; Nov 03,2009 MFM: --Add FWHM to input file query and output structure.
; Nov 17,2009 MFM: --Add underscores to IF/RA, TRR/RTR checks
; Nov 19,2009 MFM: --For start_type test IF before RA to catch case where 
;                    data is converted from raw PDS RA.
; Nov 27,2009 MFM: --Use get_cat_header_data to load info, pass
;                    most via atp_setup struct to processing routine.
; Dec 02,2009 MFM: --Delete envi_file_query call obsoleted by get_cat_header_data
; Nov 17,2010 MFM: --add header parameter CAT_PRODUCT_VERSION
; Nov 18,2010 MFM: --add four more header parameters
; Mar 20,2012 MFM: --New routine for selecting scaling bands; also chooses 
;                    whether to correct artifact; add enable_artifact flag
; Apr 05,2012 MFM: --Bug fix - define enable_artifact even when ATM correction 
;                    is off.

pro atpcorr_auto, atm, therm, phot, atmt_src, vsid, bandset_id, enable_artifact, fname, outfile
 
;=====================================================
;   SELECT WHAT TO DO
;=====================================================

;TEMP:
envi_open_file, fname, r_fid=in_fid
envi_file_query,in_fid,nb=nb
in_bands = lindgen(nb)
subset = 0
runall = 1
memory = 0
	
;=======================================
;   FILE CHECKS AND INFO
;=======================================

; Find related info.
stat = get_cat_header_data(in_fid, NS=ns, NL=nl, NB=nb, $
	WL=in_wavesall, WAVELENGTH_UNITS=wu, $
	BBL=in_bbl, FWHM=fwhm, BNAMES=bnames, FNAME=datafile, $
	INTERLEAVE=in_interleave, OFFSET=offset, $
	BYTE_SWAP=byte_swap, DIMS=in_dims, $
	DATA_IGNORE_VALUE=data_ignore_value, DATA_TYPE=data_type, $
	DEF_BANDS=def_bands, DESCRIP=descrip, $
	MAP_INFO=map_info, $
	CAT_START_TIME          = cat_start_time, $
	CAT_SCLK_START          = cat_sclk_start, $
	CAT_CRISM_OBSID         = cat_crism_obsid, $
	CAT_OBS_TYPE            = cat_obs_type, $
	CAT_PRODUCT_VERSION     = cat_product_version, $
	CAT_CRISM_DETECTOR_ID   = cat_crism_detector_id, $
	CAT_BIN_MODE            = cat_bin_mode, $
	CAT_WAVELENGTH_FILTER   = cat_wavelength_filter, $
	CAT_CRISM_DETECTOR_TEMP = cat_crism_detector_temp, $
	CAT_CRISM_BENCH_TEMP    = cat_crism_bench_temp, $
	CAT_CRISM_HOUSING_TEMP  = cat_crism_housing_temp, $
	CAT_SOLAR_LONGITUDE     = cat_solar_longitude, $
	CAT_PDS_LABEL_FILE      = lblfile, $
	CAT_SPECTRUM_RESAMPLED  = resamp, $
	CAT_TILE_WAVE_FILE      = cat_tile_wave_file, $
	CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
	CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
	CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
	CAT_HISTORY             = cat_history, $
	CAT_INPUT_FILES         = cat_input_files )

; Some checks
if ( (stat lt 0) || (ns lt 0) || $
     (nl lt 0)   || (nb lt 0) || $
     (in_interleave lt 0) || (data_type lt 0) ) then begin
	ok = dialog_message('Required LBL info not found. Aborting correction.',$
                       title='Correction Message',/cancel)
	return
endif


; Generally don't process MRRAL; these should already be corrected. 
dataup = strupcase(datafile)
dataupbase = file_basename(dataup)
if (strpos(dataupbase, 'MRRAL') ge 0) then begin
	msg = 'MRRAL: Lambert albedo should not need atmospheric, ' + string([10b])  $
	      + 'thermal, or photometric correction. Continue?'
	ok = dialog_message(msg, title='Correct CRISM', /question, /cancel)
	if ((ok eq 'No') or (ok eq 'Cancel')) then return
endif

; Don't process MTRDR either projected or not; already corrected
if (pdsfile_is_mtrdr(dataupbase)) then begin
	msg = 'MTRDR: Targeted RDR should not need atmospheric, ' + string([10b])  $
	      + 'thermal, or photometric correction.'
	ok = dialog_message(msg, title='Correct CRISM', /cancel)
	return
endif


; Make sure nothing but spectral data selected:
if (~pdsfile_is_spectral(dataupbase)) then begin
	ok = dialog_message('This does not appear to be spectral scene data. Continue?',$
                     title='Correct CRISM',/question,/cancel)
	if ((ok eq 'No') or (ok eq 'Cancel')) then return
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Need to know if we are starting with radiance or I/F:
;  can do some corrections on both...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Find type of data
if ((strpos(dataupbase,'_IF') ge 0) || (strpos(dataupbase,'MRRIF') ge 0)) then begin
	start_type = 'IF'
endif else if (strpos(dataupbase,'_RA') ge 0) then begin
	start_type = 'RA'
endif else begin
	; User decides...
	prompt = 'Is the starting data:'
	list = ['Radiance  ','I/F  ']
	base = widget_auto_base(title='ATP Corrections')
	wo = widget_menu(base, list=list, prompt=prompt,uvalue='type', /auto,/exclusive)
	result = auto_wid_mng(base)
	if (result.accept eq 0) then return  ; cancel selected
	if (result.type eq 0) then start_type='RA'
	if (result.type eq 1) then start_type='IF'
endelse

; Find out if data is a tile:
if (pdsfile_is_tile(datafile)) then tile=1 else tile=0


;================================================================
;   CHECK CONSISTENCY OF REQUESTED OPERATIONS vs. DATA TYPE
;================================================================

if (atm eq 'scale') then begin
	; Can't scale radiance data
	if (start_type eq 'RA') then begin
		msg = 'Cannot scale radiance data for atmospheric correction. Aborting.'
		ok = dialog_message(msg, title='Correct CRISM Message')
		return
	endif
endif

;==================================================
;   CONSISTENCY CHECKS ON CHOIES
;==================================================

; Make sure corections not already done:
if (n_elements(cat_history) eq 1) then begin
	chist = strtrim(cat_history,2)
	if ((phot ne 'off') && (strpos(chist,'PHC') ge 0)) then begin
		ok = dialog_message('Photometric correction already applied,' $
	           + string(10b) + 'omit photometric correction?', $
	           title='Correct CRISM',/question,/cancel)
		if (ok eq 'Cancel') then return
		if (ok eq 'Yes') then phot='off'
	endif
	if ((atm ne 'none') && (strpos(chist,'ATC') ge 0)) then begin
		ok = dialog_message('Atmospheric correction already applied,' $
	           + string(10b) + 'omit atmospheric correction?', $
	           title='Correct CRISM',/question,/cancel)
		if (ok eq 'Cancel') then return
		if (ok eq 'Yes') then atm = 'none'
	endif
	if ((therm ne 'none') && (strpos(chist,'THC') ge 0)) then begin
		ok = dialog_message('Thermal correction already applied,' $
	           + string(10b) + 'omit thermal correction?', $
	           title='Correct CRISM',/question,/cancel)
		if (ok eq 'Cancel') then return
		if (ok eq 'Yes') then therm = 'none'
	endif
endif
if (atm eq 'none') and (therm eq 'none') and (phot eq 'off') then begin
	ok = dialog_message('You are not applying any corrections; aborting procedure.',$
                         title='Correct CRISM Error')
	return
endif



;========================================================
;   PUT DATA IN SETUP STRUC FOR PROCESSING PROCEDURE
;========================================================

atp_setup = { in_fid: in_fid,  $
              datafile: datafile,  $
              in_dims: in_dims,  $
              ns: ns, nl: nl, nb: nb, $
              in_interleave: in_interleave, $
              in_wavesall: in_wavesall, wu: wu, in_bands: in_bands, $
              fwhm: fwhm, in_bbl: in_bbl, $
              data_type: data_type, start_type: start_type, $
              resamp: resamp,  $
              subset: subset, runall: runall, $
              atm: atm, therm: therm, phot: phot, $
              vsid_slct: strtrim(vsid,2),  atmt_source: atmt_src, $
              bandset_id: bandset_id, $
              enable_artifact: enable_artifact, $
              lblfile: lblfile,  $
              memory: memory, outfile: outfile, $
              def_bands: def_bands, $
              map_info: map_info, $
              CAT_START_TIME: cat_start_time, $
              CAT_SCLK_START: cat_sclk_start, $
              CAT_CRISM_OBSID: cat_crism_obsid, $
              CAT_OBS_TYPE: cat_obs_type, $
              CAT_PRODUCT_VERSION: cat_product_version, $
              CAT_CRISM_DETECTOR_ID: cat_crism_detector_id, $
              CAT_BIN_MODE: cat_bin_mode, $
              CAT_WAVELENGTH_FILTER: cat_wavelength_filter, $
              CAT_CRISM_DETECTOR_TEMP: cat_crism_detector_temp, $
              CAT_CRISM_BENCH_TEMP: cat_crism_bench_temp, $
              CAT_CRISM_HOUSING_TEMP: cat_crism_housing_temp, $
              CAT_SOLAR_LONGITUDE: cat_solar_longitude, $
              CAT_PDS_LABEL_FILE: lblfile, $
              CAT_SPECTRUM_RESAMPLED: resamp, $
              CAT_TILE_WAVE_FILE: cat_tile_wave_file, $
              CAT_SWEETSPOT_WAVE_FILE: cat_sweetspot_wave_file, $
              CAT_WA_WAVE_FILE: cat_wa_wave_file,  $
              CAT_IR_WAVES_REVERSED: cat_ir_waves_reversed, $
              CAT_HISTORY: cat_history, $
              CAT_INPUT_FILES: cat_input_files, $
              fail_message: '' }

if (tile) then begin
	stat = atp_tile(atp_setup)
endif else begin
	stat = atp_nontile(atp_setup)
endelse

if (stat ne 0) then begin
	ok = dialog_message('Atmospheric correction failed: ' + atp_setup.fail_message,$
                         title='Correct CRISM Error')
	return
endif

ENVI_CLOSE_DISPLAY, 0
envi_file_mng, id=in_fid, /remove, /delete
ENVI_CLOSE_DISPLAY, 1

return
end

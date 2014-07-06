52a53
> pro projection_auto, fname, outfile
54,71d54
< PRO projection_event, ev
<   widget_control, ev.id, get_uvalue=uvalue
< 
<   ; Define ENVI functions used so able to create a sav file:
<   FORWARD_FUNCTION envi_get_file_ids,envi_select,envi_file_query, $
<           widget_auto_base,widget_outfm,magic_mem_check,envi_get_data, $
<           envi_proj_create,auto_wid_mng, cat_error_report, get_cat_header_data, $
<           write_cat_header
<                
< 
<   ;Simple catch to prevent crash
<   catch, error
<   if (error NE 0) then begin
<     cat_error_report, routine_name='projection_event'
<     return
<   endif
< 
<   ; See what files are open:
73,79c56,58
< 
<   ; Have user pick an open data file to work with:
<   envi_select,fid=fid,dims=dims,pos=bands,/file_only,$
<        /no_dims,title='Select CRISM File To Project:'
<   if (fid[0] eq -1) then return                  ;cancel selected
<   ; Get info about the file, for processing and output header:
<   envi_file_query,fid,fname=datafile,data_type=data_type
---
>   envi_open_file, fname, r_fid=fid
>   envi_file_query,fid,fname=datafile,data_type=data_type,dims=dims,nb=nb
>   bands=lindgen(nb)
137,160c116,142
<   in_file_directory = file_dirname(datafile, /mark_directory)
<   in_file_basename = file_basename(datafile)
<   pos = strpos(in_file_basename,'.')
<   if (pos gt 0) then begin
<     in_file_extension = strmid(in_file_basename, pos)
<     in_file_basename = strmid(in_file_basename,0,pos)
<   endif else begin
<     in_file_extension = ''
<   endelse
<   default = in_file_directory + in_file_basename + '_p' + in_file_extension
<   base = widget_auto_base(title='Project Data')
<   wo = widget_outfm(base, uvalue='outf',default=default, /auto)
<   result = auto_wid_mng(base)
<   if (result.accept eq 0) then return
<   if ((result.outf.in_memory) ne 1) then outfile=result.outf.name else begin
<     ;memory check
<     mem_check = magic_mem_check(dims=dims,in_memory = result.outf.in_memory, $
<                    out_dt=data_type, nb=n_elements(bands), out_name = result.outf.name)
<     if (mem_check.cancel EQ 1) then return    ; cacel selected
<     ;Store result of memory check in result structure
<     result.outf.in_memory = mem_check.in_memory
<     result.outf.name = mem_check.out_name
<     outfile=result.outf.name
<   endelse
---
> ;  in_file_directory = file_dirname(datafile, /mark_directory)
> ;  in_file_basename = file_basename(datafile)
> ;  pos = strpos(in_file_basename,'.')
> ;  if (pos gt 0) then begin
> ;    in_file_extension = strmid(in_file_basename, pos)
> ;    in_file_basename = strmid(in_file_basename,0,pos)
> ;  endif else begin
> ;    in_file_extension = ''
> ;  endelse
> ;  default = in_file_directory + in_file_basename + '_p' + in_file_extension
> ;  base = widget_auto_base(title='Project Data')
> ;  wo = widget_outfm(base, uvalue='outf',default=default, /auto)
> ;  result = auto_wid_mng(base)
> ;  if (result.accept eq 0) then return
> ;  if ((result.outf.in_memory) ne 1) then outfile=result.outf.name else begin
> ;    ;memory check
> ;    mem_check = magic_mem_check(dims=dims,in_memory = result.outf.in_memory, $
> ;                   out_dt=data_type, nb=n_elements(bands), out_name = result.outf.name)
> ;    if (mem_check.cancel EQ 1) then return    ; cacel selected
> ;    ;Store result of memory check in result structure
> ;    result.outf.in_memory = mem_check.in_memory
> ;    result.outf.name = mem_check.out_name
> ;    outfile=result.outf.name
> ;  endelse
>   memory = 0
> ;  mem_check = magic_mem_check(dims=dims,in_memory = memory, $
> ;                  out_dt=data_type, nb=n_elements(bands), out_name = outfile)
172,177c154,159
<     wne = where((dims(1:4) ne geodims(1:4)) eq 1)
<     if (wne[0] ne -1) then begin
<       ok = dialog_message('Data and Geo dimensions do not match.',$
<                            title='Project Data Error')
<       return
<     endif
---
> ;    wne = where((dims(1:4) ne geodims(1:4)) eq 1)
> ;    if (wne[0] ne -1) then begin
> ;      ok = dialog_message('Data and Geo dimensions do not match.',$
> ;                           title='Project Data Error')
> ;      return
> ;    endif
183d164
< 
217d197
< 
223,224c203,205
<     params = [3396190.0, latorigin, lonorigin, 0.0, 0.0]
<     name='Mars Sphere-Based Equirectangular'
---
>     ;params = [3396190.0, latorigin, lonorigin, 0.0, 0.0]
>     params = [3396190.0, 0.0, 0.0, 0.0, 0.0]
>     name='Mars Equirectangular'
245d225
< 
254,256c234,262
<   if ((result.outf.in_memory) eq 1) then begin
<     ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
<          background=65535.,/IN_MEMORY
---
> ;  if (memory eq 0) then begin
> ;    ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
> ;         background=65535.,/IN_MEMORY
> ;  endif else begin
>   ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
>        background=65535.,out_name=outfile
> 
>   ;============================
>   ;    HEADER PREP
>   ;============================
> 
>   cat_envi_open_ifnot, outfile, out_fid, already_open
>   if (out_fid lt 0) then begin
>     ; Must have had some problem.
>     ok = dialog_message('Error opening output for header. Aborting procedure.',$
>                          title='Project Data Error')
>     return
>   endif
> 
>   ; Get projected data size and map info from current header:
>   envi_file_query, out_fid, ns=ns_out, nl=nl_out, bnames=bnames_out
>   map_info_out = envi_get_map_info(fid=out_fid, undefined=udf)
>   if (udf) then map_info_out = -1
> 
>   ; def_bands:
>   if ((n_elements(def_bands) eq 3) && (min(def_bands) gt 0)) then begin
>     ; For header... see note in write_cat_header prolog about 
>     ; decrementing def_bands
>     def_bands -= 1
258,259c264,265
<     ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
<          background=65535.,out_name=result.outf.name
---
>     def_bands = [0,0,0]
>   endelse
261,285c267,268
<     ;============================
<     ;    HEADER PREP
<     ;============================
< 
<     cat_envi_open_ifnot, result.outf.name, out_fid, already_open
<     if (out_fid lt 0) then begin
<       ; Must have had some problem.
<       ok = dialog_message('Error opening output for header. Aborting procedure.',$
<                            title='Project Data Error')
<       return
<     endif
< 
<     ; Get projected data size and map info from current header:
<     envi_file_query, out_fid, ns=ns_out, nl=nl_out, bnames=bnames_out
<     map_info_out = envi_get_map_info(fid=out_fid, undefined=udf)
<     if (udf) then map_info_out = -1
< 
<     ; def_bands:
<     if ((n_elements(def_bands) eq 3) && (min(def_bands) gt 0)) then begin
<       ; For header... see note in write_cat_header prolog about 
<       ; decrementing def_bands
<       def_bands -= 1
<     endif else begin
<       def_bands = [0,0,0]
<     endelse
---
>   ; Append history:
>   cat_history = cat_append_history(cat_history, 'MAP')
287,288c270,291
<     ; Append history:
<     cat_history = cat_append_history(cat_history, 'MAP')
---
>   ; Update input file list:
>   if (n_elements(cat_input_files) gt 0) then cat_input_files = strtrim(cat_input_files,2)
>   if (  (n_elements(cat_input_files) gt 0) && (cat_input_files[0] ne '-1') $
>      && (cat_input_files[0] ne '')) then begin
>     cat_input_files = [cat_input_files, file_basename(datafile)]
>   endif else begin
>     cat_input_files = [file_basename(datafile)]
>   endelse
> 
>   ; Spectral subsetting...
>   if (wl[0] ne -1) then begin
>     wl = wl[bands]
>   endif
>   if (bbl[0] ne -1) then begin
>     bbl = bbl[bands]
>   endif
>   if (fwhm[0] ne -1) then begin
>     fwhm = fwhm[bands]
>   endif
>   if ( (n_elements(def_bands) eq 3) && (min(def_bands) gt 0) ) then begin
>     def_bands = crism_def_bands(cat_crism_detector_id, wl)
>   endif
290,294c293,330
<     ; Update input file list:
<     if (n_elements(cat_input_files) gt 0) then cat_input_files = strtrim(cat_input_files,2)
<     if (  (n_elements(cat_input_files) gt 0) && (cat_input_files[0] ne '-1') $
<        && (cat_input_files[0] ne '')) then begin
<       cat_input_files = [cat_input_files, file_basename(datafile)]
---
>   stat = write_cat_header(FNAME=outfile, $
>      NS=ns_out, NL=nl_out, NB=n_elements(bands), $
>      DATA_TYPE=4, INTERLEAVE=0, OFFSET=0, $
>      BBL=bbl, BNAMES=bnames_out, FWHM=fwhm, $
>      DATA_IGNORE_VALUE=65535.0, DEF_BANDS=def_bands, $
>      DESCRIP='CAT CRISM DATA projected', MAP_INFO=map_info_out, $
>      WAVELENGTH_UNIT=wu, WL=wl, $
>      CAT_START_TIME          = cat_start_time, $
>      CAT_SCLK_START          = cat_sclk_start, $
>      CAT_CRISM_OBSID         = cat_crism_obsid, $
>      CAT_OBS_TYPE            = cat_obs_type, $
>      CAT_PRODUCT_VERSION     = cat_product_version, $
>      CAT_CRISM_DETECTOR_ID   = cat_crism_detector_id,  $
>      CAT_BIN_MODE            = cat_bin_mode, $
>      CAT_WAVELENGTH_FILTER   = cat_wavelength_filter, $
>      CAT_CRISM_DETECTOR_TEMP = cat_crism_detector_temp, $
>      CAT_CRISM_BENCH_TEMP    = cat_crism_bench_temp, $
>      CAT_CRISM_HOUSING_TEMP  = cat_crism_housing_temp, $
>      CAT_SOLAR_LONGITUDE     = cat_solar_longitude, $
>      CAT_PDS_LABEL_FILE      = cat_pds_label_file, $
>      CAT_SPECTRUM_RESAMPLED  = cat_spectrum_resampled, $
>      CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
>      CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
>      CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
>      CAT_HISTORY             = cat_history, $
>      CAT_INPUT_FILES         = cat_input_files, $
>      /overwrite )
> 
>   ; Report problems with header if any indicated by status returned:
>   if (stat ne 0) then begin
>     if (stat eq -1) then begin
>       msg = 'Problem writing header for '+file_basename(outfile) + string(10b) $
> 		      + 'Required keywords for write_cat_header undefined.'
>       ok = dialog_message(msg, title='Header Error')
>     endif else if (stat eq -2) then begin
>       msg = 'Problem writing header for '+file_basename(outfile) + string(10b) $
> 		      + 'Header already exists.'
>       ok = dialog_message(msg, title='Header Error')
296c332,334
<       cat_input_files = [file_basename(datafile)]
---
>       msg = 'Problem writing header for '+file_basename(outfile) + string(10b) $
> 		      + 'Unspecified error.'
>       ok = dialog_message(msg, title='Header Error')
298,357c336,337
< 
<     ; Spectral subsetting...
<     if (wl[0] ne -1) then begin
<       wl = wl[bands]
<     endif
<     if (bbl[0] ne -1) then begin
<       bbl = bbl[bands]
<     endif
<     if (fwhm[0] ne -1) then begin
<       fwhm = fwhm[bands]
<     endif
<     if ( (n_elements(def_bands) eq 3) && (min(def_bands) gt 0) ) then begin
<       def_bands = crism_def_bands(cat_crism_detector_id, wl)
<     endif
< 
<     stat = write_cat_header(FNAME=result.outf.name, $
<        NS=ns_out, NL=nl_out, NB=n_elements(bands), $
<        DATA_TYPE=4, INTERLEAVE=0, OFFSET=0, $
<        BBL=bbl, BNAMES=bnames_out, FWHM=fwhm, $
<        DATA_IGNORE_VALUE=65535.0, DEF_BANDS=def_bands, $
<        DESCRIP='CAT CRISM DATA projected', MAP_INFO=map_info_out, $
<        WAVELENGTH_UNIT=wu, WL=wl, $
<        CAT_START_TIME          = cat_start_time, $
<        CAT_SCLK_START          = cat_sclk_start, $
<        CAT_CRISM_OBSID         = cat_crism_obsid, $
<        CAT_OBS_TYPE            = cat_obs_type, $
<        CAT_PRODUCT_VERSION     = cat_product_version, $
<        CAT_CRISM_DETECTOR_ID   = cat_crism_detector_id,  $
<        CAT_BIN_MODE            = cat_bin_mode, $
<        CAT_WAVELENGTH_FILTER   = cat_wavelength_filter, $
<        CAT_CRISM_DETECTOR_TEMP = cat_crism_detector_temp, $
<        CAT_CRISM_BENCH_TEMP    = cat_crism_bench_temp, $
<        CAT_CRISM_HOUSING_TEMP  = cat_crism_housing_temp, $
<        CAT_SOLAR_LONGITUDE     = cat_solar_longitude, $
<        CAT_PDS_LABEL_FILE      = cat_pds_label_file, $
<        CAT_SPECTRUM_RESAMPLED  = cat_spectrum_resampled, $
<        CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
<        CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
<        CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
<        CAT_HISTORY             = cat_history, $
<        CAT_INPUT_FILES         = cat_input_files, $
<        /overwrite )
< 
<     ; Report problems with header if any indicated by status returned:
<     if (stat ne 0) then begin
<       if (stat eq -1) then begin
<         msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
< 			      + 'Required keywords for write_cat_header undefined.'
<         ok = dialog_message(msg, title='Header Error')
<       endif else if (stat eq -2) then begin
<         msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
< 			      + 'Header already exists.'
<         ok = dialog_message(msg, title='Header Error')
<       endif else begin
<         msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
< 			      + 'Unspecified error.'
<         ok = dialog_message(msg, title='Header Error')
<       endelse
<     endif
<   endelse
---
>   endif
> ;  endelse
374a355,359
> 
> ENVI_CLOSE_DISPLAY, 0
> envi_file_mng, id=fid, /remove, /delete
> ENVI_CLOSE_DISPLAY, 0
> envi_file_mng, id=out_fid, /remove

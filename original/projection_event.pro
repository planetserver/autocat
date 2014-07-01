; ENVI event handler routine to project a CRISM TRDR, RTR, DDR or
;  DRR plane or planes using DDR or DRR info; differs from manual,
;  multi-step ENVI projection procedure only at double precision.
;  User can select bands to be projected.
;
; Note: hardcodeGNC = hardcoded for general naming conventions;
;
; Created: Jan 30, 2006
; By: Shannon Pelkey
; Modification History:
; June 8, 2006 S.Pelkey
; --Modified to use find_geofile if not projecting geo data
; --Made it so program will close geo file if that is not what is
;   being projected
;
; June 21, 2006 S.Pelkey
; --Modified to ask user to save to disk or keep in memory
;
; October 18, 2006 S.Pelkey
; --Added polar projection for images with minimum abs(latitude) > 65deg
;
; NOV 01, 2006 S.Pelkey
; --Added simple error catch
;
; DEC 01, 2006 S.Pelkey
; --Changed projection for mid-lats to be more consistent with MRO project
;
; FEB 14, 2007 S.Pelkey
; --Corrected the way latorigin is defined in southern hemisphere
;
; FEB 16, 2007 SMP: --Modified to create a .sav file
;
; June 25, 2007 SMP: --added name check to check for/abort if user
;                      selected a tile
; Apr 29,2008  MFM --Made insensitive to filename case
; Feb 10,2009  MFM --For radiance/I/F decision, look at file basename only, 
;                    and look for _RA and _IF instead of just RA, IF to 
;                    prevent catching text in path. Use dataupbase for other
;                    filename based checks too.
; Jul 23,2009  MFM --Use CAT-generated temp file name instead of hardcode
; Sep 17,2009  MFM --find_geofile returns '' not '-1' on fail
; Dec 08,2009  MFM --Use get_cat_header_data, write_cat_header. Condense some old code.
; Dec 09,2009  MFM --When force deleting GLT header, get either file.hdr or file.img.hdr
; Feb 23,2010  MFM: --Took datum keyword out of envi_proj_create for input projection.
;                     Made no difference, required problematic custom datum.
; Feb 23,2010  MFM: --Took datum keyword out of envi_proj_create for output projection.
;                     Params specification is sufficient. Removes need to 
;                     define custom mars sphere datum.
; Nov 17,2010  MFM: --add header parameter CAT_PRODUCT_VERSION
; Nov 18,2010  MFM: --add four more header parameters
; Mar 17,2012  MFM: --add forward function; recognize TER type file


PRO projection_event, ev
  widget_control, ev.id, get_uvalue=uvalue

  ; Define ENVI functions used so able to create a sav file:
  FORWARD_FUNCTION envi_get_file_ids,envi_select,envi_file_query, $
          widget_auto_base,widget_outfm,magic_mem_check,envi_get_data, $
          envi_proj_create,auto_wid_mng, cat_error_report, get_cat_header_data, $
          write_cat_header
               

  ;Simple catch to prevent crash
  catch, error
  if (error NE 0) then begin
    cat_error_report, routine_name='projection_event'
    return
  endif

  ; See what files are open:
  origfids=envi_get_file_ids()

  ; Have user pick an open data file to work with:
  envi_select,fid=fid,dims=dims,pos=bands,/file_only,$
       /no_dims,title='Select CRISM File To Project:'
  if (fid[0] eq -1) then return                  ;cancel selected
  ; Get info about the file, for processing and output header:
  envi_file_query,fid,fname=datafile,data_type=data_type

  stat = get_cat_header_data(fid, NS=ns, NL=nl, NB=nb, WL=wl, WAVELENGTH_UNITS=wu, $
	BBL=bbl, FWHM=fwhm, FNAME=datafile, INTERLEAVE=interleave, OFFSET=offset, $
	DATA_IGNORE_VALUE=data_ignore_value, DATA_TYPE=data_type, $
	DEF_BANDS=def_bands, DESCRIP=descrip, $
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
	CAT_PDS_LABEL_FILE      = cat_pds_label_file, $
	CAT_SPECTRUM_RESAMPLED  = cat_spectrum_resampled, $
	CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
	CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
	CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
	CAT_HISTORY             = cat_history, $
	CAT_INPUT_FILES         = cat_input_files )


  if (pdsfile_is_tile(datafile)) then begin
    ok = dialog_message('File appears to be a projected CRISM tile. Aborting projection.',$
                         title='Project Data Message')
    return
  endif
  dataup = strupcase(datafile)
  dataupbase = file_basename(dataup)
  pos1=strpos(dataupbase,'TRR')
  if (pos1 ne -1) then type='TRR' else begin
    pos1=strpos(dataupbase,'RTR')
    if (pos1 ne -1) then type='RTR' else begin
      pos1=strpos(dataupbase,'DDR')
      if (pos1 ne -1) then type='DDR' else begin
        pos1=strpos(dataupbase,'EDR')
        if (pos1 ne -1) then type='EDR' else begin
          pos1 = strpos(dataupbase,'TER')
          if (pos1 ne -1) then type='TER' else type=''
        endelse
      endelse
    endelse
  endelse
  pos2 = strpos(dataupbase,'.IMG')
  if (type eq '') or (pos2 eq -1) then begin
    ok = dialog_message('Cannot verify that this is a standard scene IMG (img) file. Continue?',$
                         title='Project Data Message',/question)
    if (ok eq 'No') then return
  endif

  ; Check to see if user wants to write to a file or keep in memory:
  ; Load into available bands list regardless.
  ; Construct default output filenames:
  in_file_directory = file_dirname(datafile, /mark_directory)
  in_file_basename = file_basename(datafile)
  pos = strpos(in_file_basename,'.')
  if (pos gt 0) then begin
    in_file_extension = strmid(in_file_basename, pos)
    in_file_basename = strmid(in_file_basename,0,pos)
  endif else begin
    in_file_extension = ''
  endelse
  default = in_file_directory + in_file_basename + '_p' + in_file_extension
  base = widget_auto_base(title='Project Data')
  wo = widget_outfm(base, uvalue='outf',default=default, /auto)
  result = auto_wid_mng(base)
  if (result.accept eq 0) then return
  if ((result.outf.in_memory) ne 1) then outfile=result.outf.name else begin
    ;memory check
    mem_check = magic_mem_check(dims=dims,in_memory = result.outf.in_memory, $
                   out_dt=data_type, nb=n_elements(bands), out_name = result.outf.name)
    if (mem_check.cancel EQ 1) then return    ; cacel selected
    ;Store result of memory check in result structure
    result.outf.in_memory = mem_check.in_memory
    result.outf.name = mem_check.out_name
    outfile=result.outf.name
  endelse

  ;If not projecting geo data, need to find the geometry filename:
  if (type ne 'DDR') then begin
    find_geofile, datafile, geofile
    if (geofile eq '') then return     ; cancel selected
    print,'Using geometry file:'
    print,geofile
    ; Need to open geometry file in ENVI so it has a related ID:
    envi_open_file,geofile,r_fid=geofid
    envi_file_query,geofid,dims=geodims
    ; As long as we can, make sure data and geo file dimensions match:
    wne = where((dims(1:4) ne geodims(1:4)) eq 1)
    if (wne[0] ne -1) then begin
      ok = dialog_message('Data and Geo dimensions do not match.',$
                           title='Project Data Error')
      return
    endif
  endif else begin   ;getting geo info if necessary
    geofile = datafile
    geofid = fid
 endelse


  ; Pull some planes from the geofile
  lat = envi_get_data(fid=geofid,dims=dims,pos=3)
  lon = envi_get_data(fid=geofid,dims=dims,pos=4)


  ; Create INPUT Geographic projection structure to associate w/file
  ;  (as is, file has an "Arbitrary" projection, which cannot be used
  ;  to create a GLT) (lats in band 3; lons in band 4):
  projin = envi_proj_create(/geographic)


  ; Create OUTPUT Geographic projection structure to associate w/file;
  ;  output projection depends on the latitude of the observation.
  ; Check if this is a high-latitude targeted observation:
  if (min(abs(lat)) gt 65.) then polar=1 else polar=0


  ; For all observations, the central meridian is pulled from
  ;  the data itself:
  lonorigin = median(lon)

  if (polar eq 0) then begin
;    latorigin=median(lat)
    latmax = max(lat)               
    if (latmax gt 0.) then begin
      if ((latmax mod 5) eq 0) then latorigin=latmax-5 else begin
        for lbin=0,60,5. do if (latmax gt lbin) and (latmax lt lbin+5) then latorigin=lbin
      endelse
    endif else begin
      if ((latmax mod 5) eq 0) then latorigin=latmax else begin
        for lbin=-65,0,5. do if (latmax gt lbin) and (latmax lt lbin+5) then latorigin=lbin+5
      endelse
    endelse

    ; Define an equirectangular projection: 
    ;  Assumes spherical body, needs radius in meters,
    ;  lat and lon of projection origin and false easting and
    ;  northing (in meters):
    type = 17
    params = [3396190.0, latorigin, lonorigin, 0.0, 0.0]
    name='Mars Sphere-Based Equirectangular'
    projout = envi_proj_create(type=type,params=params,name=name)
  endif else begin
    if (min(lat) lt 0.) then begin
      latorigin = -90.0
      pole = 'S'
    endif else begin
      latorigin = 90.0
      pole = 'N'
    endelse
    ; Define polar stereographic projection:
    ; Needs semimajor and semiminor ellipsoid in meters,
    ; lat and lon of projection origin, and false easting and 
    ; northing in meters:
;??maybe we want the lonorigin to not change with each image in this case??
    type = 31
    params = [3396190.0, 3376200.0, latorigin, lonorigin, 0.0, 0.0]
    name = 'Mars_'+pole+'polar_Stereo'
    projout = envi_proj_create(type=type,params=params,name=name)
  endelse


  ; Create the look up table:
  tmp_glt_name = cat_tmpfile_name('glt_proj','tmp')
  ENVI_DOIT, 'ENVI_GLT_DOIT', DIMS=dims, I_PROJ=projin, O_PROJ=projout, $
       out_name=tmp_glt_name,r_fid=glt_fid,ROTATION=0, X_FID=geofid, $
       X_POS=4, Y_FID=geofid, Y_POS=3
  if (glt_fid[0] eq -1) then return

  ; Apply the look up table:
  if ((result.outf.in_memory) eq 1) then begin
    ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
         background=65535.,/IN_MEMORY
  endif else begin
    ENVI_DOIT, 'ENVI_GEOREF_FROM_GLT_DOIT',FID=fid,GLT_FID=glt_fid,pos=bands,$
         background=65535.,out_name=result.outf.name

    ;============================
    ;    HEADER PREP
    ;============================

    cat_envi_open_ifnot, result.outf.name, out_fid, already_open
    if (out_fid lt 0) then begin
      ; Must have had some problem.
      ok = dialog_message('Error opening output for header. Aborting procedure.',$
                           title='Project Data Error')
      return
    endif

    ; Get projected data size and map info from current header:
    envi_file_query, out_fid, ns=ns_out, nl=nl_out, bnames=bnames_out
    map_info_out = envi_get_map_info(fid=out_fid, undefined=udf)
    if (udf) then map_info_out = -1

    ; def_bands:
    if ((n_elements(def_bands) eq 3) && (min(def_bands) gt 0)) then begin
      ; For header... see note in write_cat_header prolog about 
      ; decrementing def_bands
      def_bands -= 1
    endif else begin
      def_bands = [0,0,0]
    endelse

    ; Append history:
    cat_history = cat_append_history(cat_history, 'MAP')

    ; Update input file list:
    if (n_elements(cat_input_files) gt 0) then cat_input_files = strtrim(cat_input_files,2)
    if (  (n_elements(cat_input_files) gt 0) && (cat_input_files[0] ne '-1') $
       && (cat_input_files[0] ne '')) then begin
      cat_input_files = [cat_input_files, file_basename(datafile)]
    endif else begin
      cat_input_files = [file_basename(datafile)]
    endelse

    ; Spectral subsetting...
    if (wl[0] ne -1) then begin
      wl = wl[bands]
    endif
    if (bbl[0] ne -1) then begin
      bbl = bbl[bands]
    endif
    if (fwhm[0] ne -1) then begin
      fwhm = fwhm[bands]
    endif
    if ( (n_elements(def_bands) eq 3) && (min(def_bands) gt 0) ) then begin
      def_bands = crism_def_bands(cat_crism_detector_id, wl)
    endif

    stat = write_cat_header(FNAME=result.outf.name, $
       NS=ns_out, NL=nl_out, NB=n_elements(bands), $
       DATA_TYPE=4, INTERLEAVE=0, OFFSET=0, $
       BBL=bbl, BNAMES=bnames_out, FWHM=fwhm, $
       DATA_IGNORE_VALUE=65535.0, DEF_BANDS=def_bands, $
       DESCRIP='CAT CRISM DATA projected', MAP_INFO=map_info_out, $
       WAVELENGTH_UNIT=wu, WL=wl, $
       CAT_START_TIME          = cat_start_time, $
       CAT_SCLK_START          = cat_sclk_start, $
       CAT_CRISM_OBSID         = cat_crism_obsid, $
       CAT_OBS_TYPE            = cat_obs_type, $
       CAT_PRODUCT_VERSION     = cat_product_version, $
       CAT_CRISM_DETECTOR_ID   = cat_crism_detector_id,  $
       CAT_BIN_MODE            = cat_bin_mode, $
       CAT_WAVELENGTH_FILTER   = cat_wavelength_filter, $
       CAT_CRISM_DETECTOR_TEMP = cat_crism_detector_temp, $
       CAT_CRISM_BENCH_TEMP    = cat_crism_bench_temp, $
       CAT_CRISM_HOUSING_TEMP  = cat_crism_housing_temp, $
       CAT_SOLAR_LONGITUDE     = cat_solar_longitude, $
       CAT_PDS_LABEL_FILE      = cat_pds_label_file, $
       CAT_SPECTRUM_RESAMPLED  = cat_spectrum_resampled, $
       CAT_SWEETSPOT_WAVE_FILE = cat_sweetspot_wave_file, $
       CAT_WA_WAVE_FILE        = cat_wa_wave_file,  $
       CAT_IR_WAVES_REVERSED   = cat_ir_waves_reversed, $
       CAT_HISTORY             = cat_history, $
       CAT_INPUT_FILES         = cat_input_files, $
       /overwrite )

    ; Report problems with header if any indicated by status returned:
    if (stat ne 0) then begin
      if (stat eq -1) then begin
        msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
			      + 'Required keywords for write_cat_header undefined.'
        ok = dialog_message(msg, title='Header Error')
      endif else if (stat eq -2) then begin
        msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
			      + 'Header already exists.'
        ok = dialog_message(msg, title='Header Error')
      endif else begin
        msg = 'Problem writing header for '+file_basename(result.outf.name) + string(10b) $
			      + 'Unspecified error.'
        ok = dialog_message(msg, title='Header Error')
      endelse
    endif
  endelse

  ; Remove GLT from available bands list:
  envi_file_mng,id=glt_fid,/remove,/delete
  ; and delete the temp glt header, either file.img.hdr or file.hdr:
  gltbn = strtrim(file_basename(tmp_glt_name),2)
  p = strpos(gltbn,'.',/reverse_search)
  if (p gt 0) then gltbn=strmid(gltbn,0,p)
  gltbn = file_dirname(tmp_glt_name, /mark_directory) + gltbn
  glt_hdr = file_search(gltbn+'*.hdr',/fold_case)
  k = where((glt_hdr ne ''),nk)
  if (nk gt 0) then begin
    file_delete,glt_hdr[k], /quiet,/noexpand_path
  endif

  ; Close geometry file if it was not originally open
  ao = where(geofid eq origfids)
  if (ao[0] eq -1) then envi_file_mng,id=geofid,/remove

end

pro run_trdr

; ATPCORR VARIABLES
; See CAT_ENVI\save_add\CAT_programs\Event_handlers\atpcorr_event.pro for the other options
atm = 'scale'
therm = 'none'
phot = 'on'
atmt_src = 'trial'
vsid = ''
bandset_id = 0
enable_artifact = 1
; END VARIABLES

dataname = 'region_name'

folder = '/path/to/the/data/' + dataname + '/PDS/'
listfile = folder + 'data.txt'
list=RD_TFILE(listfile)
finalfolder = '/path/to/the/data/' + dataname + '/processed/'
FOR i=0, N_ELEMENTS(list)-1 DO BEGIN
  
  filename = list[i]
  last_dot = strpos(filename,'.',/reverse_search)
  basename = file_basename(strmid(filename,0,last_dot))
  if (vsid eq '') then begin
    corr = atm + '_' + atmt_src
  endif else begin
    corr = atm + '_' + atmt_src + '_' + vsid
  endelse
  infile = folder + filename
  catfile = folder + basename + '_CAT.img'
  atmfile = folder + basename + '_CAT_' + corr + '.img'
  projfile = finalfolder + basename + '_CAT_' + corr + '_p.img'
  
  convert_pds2cat_auto, infile, folder
  lors = STRPOS(STRUPCASE(filename), 'L_TRR', /REVERSE_SEARCH)
  if (lors eq -1) then begin
    ;S = VNIR
    atmfile = folder + basename + '_CAT_phot.img'
    projfile = finalfolder + basename + '_CAT_phot_p.img'
    atpcorr_auto, 'none', therm, phot, '', '', 0, 0, catfile, atmfile
  endif else begin
    ;L = IR
    atpcorr_auto, atm, therm, phot, atmt_src, vsid, bandset_id, enable_artifact, catfile, atmfile
  endelse
  projection_auto, atmfile, projfile
  
  envi_open_file, projfile, r_fid=fid
  ENVI_CLOSE_DISPLAY, 0
  envi_file_mng, id=fid, /remove

ENDFOR

end

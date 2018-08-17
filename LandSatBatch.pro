;+
; :Author: lenovo
;-
PRO LANDSATEBATCHProcess,ParentDirctory,FileEx
  COMPILE_OPT IDL2
  ; Start the application
  e = ENVI(/HEADLESS)
  ;Files = GetLandSatFileList(ParentDirctory, '*_MTL.txt')
  Files = GetLandSatFileList(ParentDirctory, FileEx)
  t1=Tic('total1')
  foreach File, Files,Index do begin
    ;先跑两景试试
    Catch, errorStatus
    ; Error handler
    if (errorStatus ne 0) then begin
      Catch, /CANCEL
      ; TODO: Write error handler
      print,'当前图像处理失败，请检查图像是否损坏111'
      CONTINUE
    endif
    Out_File = STRMID(File,0,STRPOS(File,'_MTL.txt'))
    print,'************************************************************************************'
    print,'准备处理第' + STRING(Index+1)+'图像;'
    print,'文件名：'+STRING(File)+';'
    ;辐射定标
    print,'------------------------------辐射定标开始------------------------------------------'
    print,File
    RDIMG =RadiometricCalibrationSingle(File,Out_File+'_rad.dat',e,0)
    ;大气校正
    print,'------------------------------大气校正开始------------------------------------------'
    QUACIMG= QuacSingle(Out_File+'_rad.dat',Out_File+'_quac.dat',e,0)
    print,'------------------------------指数计算开始------------------------------------------'
    SpIndexNames =['Normalized Difference Vegetation Index', 'Visible Atmospherically Resistant Index']
    SPIMG = SpetralIndexSingle(Out_File+'_quac.dat',Out_File+'_index.dat',SpIndexNames,e,0)
    print,'************************************************************************************'
  endforeach
  ti = toc(t1,REPORT =re)
  PRINT,'结束辐射定标、大气校正、指数计算，总用时:'+string(ti)
  print,'------------------------------图像镶嵌开始------------------------------------------'
  t2=tic('total2')
  MSParentFileDirecs = GetFolderList(ParentDirctory,2)
  foreach ParentFileFolder, MSParentFileDirecs, key do begin
    print,'当前结果文件：'+ParentFileFolder+'_Mosaic.dat'
    MSIMG = MosaicRasterSinglePeriod(ParentFileFolder,ParentFileFolder+'_Mosaic.dat',e,0)
  endforeach
  ti = toc(t2,REPORT =re)
  PRINT,'结束镶嵌，总用时:'+string(ti)
  print,'------------------------------图像镶嵌结束------------------------------------------'
  ; Get the radiometric calibration task from the catalog of ENVI tasks.
  e.Close
END
PRO ListImg,ParentDirctory
  Oringin_IMGList = GetLandSatFileList(ParentDirctory, '*_MTL.txt');
  RAD_IMGList = GetLandSatFileList(ParentDirctory, '*_rad.dat');
  Quac_IMGList =GetLandSatFileList(ParentDirctory, '*_quac.dat');
  Index_IMGList = GetLandSatFileList(ParentDirctory, '*_index.dat');
  MS_IMGList = GetLandSatFileList(ParentDirctory, '*_mosaic.dat');
  print,'*******************************原始图像*********************************************'
  foreach file, Oringin_IMGList, key do begin
    print,file
  endforeach
  print,'*******************************辐射定标*********************************************'
  foreach file, RAD_IMGList, key do begin
    print,file
  endforeach
  print,'*******************************大气校正*********************************************'
  foreach file, Quac_IMGList, key do begin
    print,file
  endforeach
  print,'*******************************指数计算**********************************************'
  foreach file, Index_IMGList, key do begin
    print,file
  endforeach
  print,'*******************************镶嵌结果**********************************************'
  foreach file, MS_IMGList, key do begin
    print,file
  endforeach
end

function DeleteFile,ParentDirectory,Filter
  defiles = GetLandSatFileList(ParentDirectory,Filter)
  foreach file,defiles,key do begin
    if FILE_TEST(file) eq 1 then begin
      print, '删除文件：' + file
      FILE_DELETE,file
    endif
  endforeach
end

;获取原始影像名字
function GetLandSatFileList,ParentDirectoty,FileFilter
  ;获取文件夹下所有的影像名称
  r=file_search(ParentDirectoty,FileFilter)
  return,r
end
function GetFolderList,ParentDirectoty,FolderDepth
  paths=file_search(ParentDirectoty,'*',/TEST_DIRECTORY)
  ReturnPaths = LIST()
  foreach path, paths, key do begin
    pathArray= STRSPLIT(path,Count =Totaldepth,'\',/EXTRACT)
    if (FolderDepth +1 eq Totaldepth) then begin
      ReturnPaths.ADD,path
    endif else begin
      ;      tempPath =''
      ;      n=1
      ;      while (n lt FolderDepth) do begin
      ;       tempPath =tempPath +'\' +pathArray[n-1]
      ;       n=n+1
      ;      endwhile
      ;      ;tempPath =
      ;      if (tempPath eq '') then begin
      ;        continue
      ;      endif else begin
      ;        ReturnPaths.ADD,tempPath+'b'
      ;      endelse
    endelse
  endforeach
  return,ReturnPaths
end
;定标图像，输入多光谱图像路径，输出定标结果，ENVI实例，覆盖结果
function RadiometricCalibrationSingle,File,Output_Raster_URI,e,OverWrite
  Raster = e.OpenRaster(File)
  if FILE_TEST(Output_Raster_URI) eq 1 then begin
    ;FILE_DELETE,Output_Raster_URI
    if (OverWrite eq 0) then begin
      print,'辐射定标结果已经存在，跳过处理该结果'
    endif else begin
      print,'辐射定标结果已经存在，覆盖处理该结果'
      FILE_DELETE,Output_Raster_URI
    endelse
    return,0
  endif
  Catch, errorStatus
  ; Error handler
  if (errorStatus ne 0) then begin
    Catch, /CANCEL
    ; TODO: Write error handler
    print,'当前图像辐射定标处理失败，请检查图像是否损坏！'
    return,0
  endif

  Task = ENVITask('RadiometricCalibration')
  ; Define inputs. Since radiance is the default calibration method
  ; you do not need to specify it here.
  Task.Input_Raster = Raster[0] ; Bands 1-7
  Task.Output_Data_Type = 'Double'
  ; Define output raster URI
  Task.Output_Raster_URI = Output_Raster_URI
  ; Run the task
  PRINT,'辐射定标：开始处理当前图像'
  tic
  Task.Execute
  ; Get the data collection
  ;DataColl = e.Data
  ; Add the output to the data collection
  ;DataColl.Add, Task.Output_Raster
  ti = toc(REPORT =re)
  PRINT,'结束辐射定标，当前图像用时:'+string(ti)
end
;快速大气校正，输入定标后的图像，输出大气校正结果
function QuacSingle,File,Output_Raster_URI,e,OverWrite
  Raster = e.OpenRaster(File)
  if FILE_TEST(Output_Raster_URI) eq 1 then begin
    ;FILE_DELETE,Output_Raster_URI
    if (OverWrite eq 0) then begin
      print,'大气校正结果已经存在，跳过处理该结果'
    endif else begin
      print,'大气校正结果已经存在，覆盖处理该结果'
      FILE_DELETE,Output_Raster_URI
    endelse
    return,0
  endif
  Catch, errorStatus
  ; Error handler
  if (errorStatus ne 0) then begin
    Catch, /CANCEL
    ; TODO: Write error handler
    print,'大气校正处理失败，请检查图像是否损坏！'
    return,0
  endif
  ; Get the QUAC task from the catalog of ENVITasks
  Task = ENVITask('QUAC')
  ; Define inputs
  Task.INPUT_RASTER = Raster
  Task.SENSOR = 'Landsat TM/ETM/OLI'
  ; Define outputs
  Task.OUTPUT_RASTER_URI = Output_Raster_URI
  ; Run the task
  PRINT,'快速大气校正：处理当前图像'
  tic
  Task.Execute
  ti = toc(REPORT =re)
  PRINT,'结束快速大气校正，当前图像用时:'+string(ti)
end
;
;
;计算光谱指数
function SpetralIndexSingle,File,Output_Raster_URI,IndexNames,e,OverWrite
  Raster = e.OpenRaster(File)
  if FILE_TEST(Output_Raster_URI) eq 1 then begin
    ;FILE_DELETE,Output_Raster_URI
    if (OverWrite eq 0) then begin
      print,'光谱指数结果已经存在，跳过处理该结果'
    endif else begin
      print,'光谱指数结果已经存在，覆盖处理该结果'
      FILE_DELETE,Output_Raster_URI
    endelse
    return,0
  endif
  Catch, errorStatus
  ; Error handler
  if (errorStatus ne 0) then begin
    Catch, /CANCEL
    ; TODO: Write error handler
    print,'光谱指数处理失败，请检查图像是否损坏！'
    return,0
  endif
  ; Get the QUAC task from the catalog of ENVITasks
  Task = ENVITask('SpectralIndices')
  Task.INDEX = IndexNames
  ; Define inputs
  Task.INPUT_RASTER = Raster
  ;Task.SENSOR = 'Landsat TM/ETM/OLI'
  ; Define outputs
  Task.OUTPUT_RASTER_URI = Output_Raster_URI
  ; Run the task
  PRINT,'光谱指数计算：处理当前图像'
  tic
  Task.Execute
  ;Get the data collection
  ;DataColl = e.Data
  ; Add the output to the data collection
  ;DataColl.Add, Task.Output_Raster
  ti = toc(REPORT =re)
  PRINT,'结束光谱指数计算，当前图像用时:'+string(ti)
end

function MosaicRasterSinglePeriod,ParentFileFolder,Output_Raster_URI,e,OverWrite
  ;F:\RasterData\2016\LC81220442016310LGN01\LC08_L1TP_122044_20161105_20170318_01_T1_quac.dat
  ; Start the application
  ; Select input scenes
  if FILE_TEST(Output_Raster_URI) eq 1 then begin
    ;FILE_DELETE,Output_Raster_URI
    if (OverWrite eq 0) then begin
      print,'镶嵌结果已经存在，跳过处理该结果'
    endif else begin
      print,'镶嵌结果已经存在，覆盖处理该结果'
      FILE_DELETE,Output_Raster_URI
    endelse
    return,0
  endif
  Catch, errorStatus
  ; Error handler
  if (errorStatus ne 0) then begin
    Catch, /CANCEL
    ; TODO: Write error handler
    print,'镶嵌处理失败，请检查图像是否损坏！'
    return,0
  endif
  files = REVERSE(FILE_SEARCH(ParentFileFolder, Count=FileNUM,'*_index.dat'))
  if FileNUM eq 2 then begin;如果一年有N景图像进行镶嵌，这里改为 eq  N
    scenes = !NULL
    FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
      raster = e.OpenRaster(files[i])
      metadata = raster.METADATA
      ; Set the Data Ignore Value to 0
      ;metadata.AddItem, 'data ignore value', 0
      scenes = [scenes, raster]
    ENDFOR
    ; Get the task from the catalog of ENVITasks
    Task = ENVITask('BuildMosaicRaster')
    ; Define inputs
    Task.INPUT_RASTERS = scenes
    Task.COLOR_MATCHING_METHOD = 'Histogram Matching'
    Task.COLOR_MATCHING_STATISTICS = 'Entire Scene
    Task.FEATHERING_METHOD = 'Seamline'
    Task.SEAMLINE_METHOD='Geometry'
    ;Distances = LIST(20)
    Task.FEATHERING_DISTANCE = [20,20]
    ;
    ; Define outputs
    Task.OUTPUT_RASTER_URI = Output_Raster_URI
    ; Run the task
    PRINT,'影像镶嵌开始：处理当前图像'
    tic
    Task.Execute
    ;Get the data collection
    ;DataColl = e.Data
    ; Add the output to the data collection
    ;DataColl.Add, Task.Output_Raster
    ti = toc(REPORT =re)
    PRINT,'结束影像镶嵌，当前图像用时:'+string(ti)
  endif
end
pro Process0612Index
  COMPILE_OPT IDL2
  ; Start the application
  e = ENVI(/HEADLESS)
  Files = GetLandSatFileList('F:\RasterData','*MTL_MultiSpectral')
  ;Files = GetLandSatFileList(ParentDirctory, FileEx)
  t1=Tic('total1')
  foreach File, Files,Index do begin
    ;先跑两景试试
    Catch, errorStatus
    ; Error handler
    if (errorStatus ne 0) then begin
      Catch, /CANCEL
      ; TODO: Write error handler
      print,'当前图像处理失败，请检查图像是否损坏111'
      CONTINUE
    endif
    Out_File = File
    print,'------------------------------指数计算开始------------------------------------------'
    SpIndexNames =['Normalized Difference Vegetation Index', 'Visible Atmospherically Resistant Index']
    SPIMG = SpetralIndexSingle(Out_File+'_quac.dat',Out_File+'_index.dat',SpIndexNames,e,0)
    print,'************************************************************************************'
  endforeach
  ti = toc(t1,REPORT =re)
  PRINT,'结束辐射定标、大气校正、指数计算，总用时:'+string(ti)
  print,'------------------------------图像镶嵌开始------------------------------------------'
  t2=tic('total2')
  ;MSParentFileDirecs = GetFolderList(ParentDirctory,2)
  MSParentFileDirecs = GetFolderList('F:\RasterData',2)
  foreach ParentFileFolder, MSParentFileDirecs, key do begin
    print,'当前结果文件：'+ParentFileFolder+'_Mosaic.dat'
    MSIMG = MosaicRasterSinglePeriod(ParentFileFolder,ParentFileFolder+'_Mosaic.dat',e,0)
  endforeach
  ti = toc(t2,REPORT =re)
  PRINT,'结束镶嵌，总用时:'+string(ti)
  print,'------------------------------图像镶嵌结束------------------------------------------'
  ; Get the radiometric calibration task from the catalog of ENVI tasks.
  e.Close
end







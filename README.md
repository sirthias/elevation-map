ASTGTMV003 Processing Helpers
-----------------------------

Small repository for ad-hoc code for processing ASTGTMV003 world elevation map data.

General process:

- `cd` into working directory

- download all 22,912 ASTGTMV003 ZIP files into sub directory `ASTGTMV003` (ca. 380 GB)

- extract the ca. 320 GB of data tif files into `tiles` sub directory:<br> 
  `find ASTGTMV003/ -type f -iname '*.zip' -exec 7z e '-x!*_num.tif' -otiles/ {} \;`

- create a sub directory structure underneath `tiles`:<br>
  `find tiles/ -type f | sed -f sed-mkdir.txt | sh`
  
- sort tiles into sub directories:<br>
  `find tiles/ -type f | sed -f sed-mv.txt | sh`
  
- edit configuration in `org.flsx.elevation.ConvertElevationData`

- resample tiles to intermediate resolution (ca. 1.6 GB with `tileTargetWidth = 360`):<br>
  `sbt runMain org.flsx.elevation.ConvertElevationData`

- edit configuration in `org.flsx.elevation.ExtractElevationPNG`

- resample world map or partial map into a PNG:<br>
  `sbt runMain org.flsx.elevation.ConvertElevationData` 
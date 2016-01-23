Those files handle issues with the SVD files:
* the *.patch files are the patches that I applied to the original SVD file
  downloaded from cmsis.arm.com. They handle either missing fields, or
  miss-represented fields.
* the *.svd2ada files are used to help svd2ada to generate more clever
  representations of the SVD data. For now, it concentrates on overlapping
  registers (occuring when several registers of the same peripheral are at the
  same physical address) and on the way to best represent them in Ada.
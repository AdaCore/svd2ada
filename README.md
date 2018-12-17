# svd2ada
SVD2Ada is an Ada binding generator from CMSIS-SVD descriptions for
ARM Cortex-M devices. It is meant to ease, strengthen and speed up driver
development for those platforms.

# Build

To build `SVD2Ada` you need a native GNAT compiler, for instance GNAT Community
edition that you can find [here](https://www.adacore.com/download).

After installing GNAT, you can build with the following command:
```
$ gprbuild -P svd2ada.gpr
```

# Usage

To run `SVD2Ada` you need to specify at least the SVD file from which you want
to generate code, and the output directory where the code will be generated.

```
$ svd2ada <SVD_FILE> -o <OUTPUT_DIR>
```

Use `--help` to display information about the different code generation
options available:

```
$ svd2ada --help
```

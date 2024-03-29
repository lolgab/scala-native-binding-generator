# scala-native-binding-generator

## **Abandoned: look at [sn-bindgen](https://github.com/indoorvivants/sn-bindgen) for a better alternative** ##

[![Join the chat at https://gitter.im/lolgab/scala-native-binding-generator](https://badges.gitter.im/lolgab/scala-native-binding-generator.svg)](https://gitter.im/lolgab/scala-native-binding-generator?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
Simple Scala Native binding generator based on FastParse

## Usage
You have to preprocess the header files before using the generator.
I found useful removing the `#include <stdlib.h>` etc. because the c standard and posix libraries have already an high quality binding in the standard lib.
After removing these includes you can preprocess the header file with gcc:
```gcc -E -P -isystem fakeincludes header.h -o output.h```

For example with libui:
```gcc -E -P -isystem fakeincludes ui.h -o Ui.h```

then you can process the preprocessed header file with:
```mill _.run Ui.h```

It will create a `Ui.scala` file.

### OpenGL
To parse openGL header file you have to remove some macros with gcc:
```
gcc -E -P -isystem fakeincludes -D '__attribute__(ARGS)=' -D '__restrict' /usr/include/GL/gl.h -o gl.h
mill _.run gl.h
```

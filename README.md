# scala-native-binding-generator
Simple Scala Native binding generator based on FastParse

## Usage
You have to preprocess the header files before using the generator.
I found useful removing the `#include <stdlib.h>` etc. because the c standard and posix libraries have already an high quality binding in the standard lib.
After removing these includes you can preprocess the header file with gcc:
```gcc -E -P header.h -o output.h```

For example with libui:
```gcc -E -p ui.h -o Ui.h```

then you can process the preprocessed header file with:
```mill _.run Ui.h```

It will create a `Ui.scala` file.

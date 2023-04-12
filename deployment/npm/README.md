# dprint-plugin-css

npm distribution of [dprint-plugin-css](https://github.com/disrupted/dprint-plugin-css).

Use this with [@dprint/formatter](https://github.com/dprint/js-formatter) or just use @dprint/formatter and download the [dprint-plugin-json WASM file](https://github.com/disrupted/dprint-plugin-css/releases).

## Example

```ts
import { createFromBuffer } from "@dprint/formatter";
import { getPath } from "dprint-plugin-css";
import * as fs from "fs";

const buffer = fs.readFileSync(getPath());
const formatter = createFromBuffer(buffer);

console.log(formatter.formatText("test.css", ".test{color: red}"));
```

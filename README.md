# digitalize

Clean and `digitalize` your data

- remove nils and empty stuff
- if strings look like numbers then make them numbers
- make map keys more idiomatic "This way" -> :this-way, also represent numbers as keywords
- trim strings

## Usage

Add `[digitalize "0.1.0-SNAPSHOT"]` to your `:dependencies` vector.

`(require 'digitalize.core)`

And now you can use `digitalize`, which expects at least one argument, the data structure to digitalize/clean.

Which can be any collection:

`(digitalize [1 "" nil]) => [1]`

```
(digitalize [nil [] :oh {"OOO H" "1" "ooooh" nil} {:this {:will {:go {2 nil}}}}])

=> (:oh {:ooo-h 1})
```

### Optional Parameters

You can use `:clean-numbers false` to avoid attempting to parse numeric strings as numbers.

And `:clean-keys` to avoid attempting to convert map keys that are strings as keywords.

## License

Copyright © 2016 Andrés Gómez Urquiza

Distributed under Eclipse Public License 1.0

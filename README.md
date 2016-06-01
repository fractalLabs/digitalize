# digitalize

Clean and `digitalize` your data

- remove nils and empty stuff
- if strings look like numbers then make them numbers
- make map keys more idiomatic "This way" -> :this-way, also represent numbers as keywords
- trim strings

## Usage

Add `[digitalize "0.1.0-SNAPSHOT"]` to your `:dependencies` vector.

`(require 'digitalize.core)`

And now you can use `digitalize`:

```
(digitalize [nil [] :oh {"OOO H" "1" "ooooh" nil} {:this {:will {:go {2 nil}}}}])

=> (:oh {:ooo-h 1})
```

## License

Copyright © 2016 Andrés Gómez Urquiza

Distributed under Eclipse Public License 1.0

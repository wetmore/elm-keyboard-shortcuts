# Elm Keyboard Shortcuts

This is a package for handling keyboard shortcuts in Elm.

Users provide a list of pairs associating keyboard shortcuts like "ctrl+r" or "g i" (pressing 'g', then pressing 'i') to actions. Actions can be elements of any particular type - strings, functions, or whatever data type you come up with. The result is a Signal of these actions, which updates with a new action whenever the necessary keyboard shortcut is entered.

# Examples

Soon

# API

The package's API is very simple, and consists of two functions, `listenForWithErr` and `listenFor`. Both functions take the same arguments: a value `def` of some type `a`, and a list of pairs `(String, a)` mapping keyboard shortcuts described by strings (such as "ctrl+r" or "g i") to something of type `a`. The functions differ in their return value. `listenForWithErr` returns a `Result String (Signal a)`, while `listenFor` simply returns a `Signal a`. In either case, the signal will update with values when the respective shortcut is entered, with an initial value of `def`. Why is the former signal wrapped in a `Result`?

Since parsing keyboard shortcuts from the provided strings may possibly fail if the strings aren't recognizable, `listenForWithErr` will return an error message if the parsing fails, or the desired signal otherwise. However, if the shortcuts  exist at compile time, and are known by the programmer to be valid, it's a hassle to handle the case of a parsing failure when there is no chance of the parsing to fail. Hence, the function `listenFor` does this matching for you, returning the desired signal when parsing succeeds and using a constant signal of `def` otherwise. 

# Todo

- [ ] Documentation.
- [ ] Come up with better names for the necessary concepts.
- [ ] Better error messages in parser (report the full string that failed)
- [ ] Write little demo app.
- [ ] Release package.

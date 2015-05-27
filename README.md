# Elm Keycombos

This is a package for handling keyboard combinations in Elm.

Users provide a list of pairs associating keyboard combinations like "ctrl+r" or "g i" (pressing 'g', then pressing 'i') to actions. Actions can be elements of any particular type - strings, functions, or whatever data type you come up with. The result is a Signal of these actions, which updates with a new action whenever the necessary keyboard combination is entered.

# Todo

- [ ] Documentation.
- [ ] Come up with better names for the necesary concepts.
- [ ] Release package.

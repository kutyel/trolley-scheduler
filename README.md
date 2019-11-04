# trolley-scheduler

📚Haskell program to schedule trolleys and shifts randomly!

## Usage

```sh
host$ nix-shell --pure shell.nix
nix$ cabal repl
*Lib> Main.main
```

testing:

```sh
nix$ cabal repl test-trolley
...
*Lib> Test.main
```

with ghcid:

```sh
nix$ ghcid -c 'cabal repl test-trolley' --test 'Test.main'
```

adding deps:

- add to cabal file, then run:

```sh
host$ nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
```


### TODO:

- [ ] Generate a PDF? 📊
- [ ] Read data from CSV files. 📖
- [ ] Allow 3~4 people per shift! 👨‍👩‍👧‍👦
- [x] Improve printing of output. 🖨
- [x] Fill turns respecting availability. 📆
- [ ] Each shift must have a Captain. 👨🏼‍✈️
- [ ] Pioneers have more turns / preference. 🤓
- [ ] Add long distance / has vehicle constraint. 🚗
- [ ] Deduce month length prompting from terminal. 🗓

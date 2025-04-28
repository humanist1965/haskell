# Haskell Experiments

My Experiments in haskell land.

I'm a functional programming fan: Have been using clojure as my main language for about 5 years until 2024.
Moved to typescript for work reasons but using it in a functional way.
I am really starting to like the typing-system so thought it time to try Haskell again.

* [Learn My Haskell](https://tinyurl.com/ghc65)
* https://simonjohnthompson.github.io/craft3e/craft3e.pdf

---

## Running Haskell Files

### Option 1: Interactive Development with `ghci`

1.  **Open your terminal:** Your command-line interface.

2.  **Launch the GHCi interpreter:** Type `ghci` and press Enter to start the interactive Haskell environment. You'll see the `Prelude>` prompt.

3.  **Load your Haskell file:** Use the `:load` command (or its shorthand `:l`) followed by the path to your `.hs` file. For example, to load `MyModule.hs` located in the `src` directory, type `:l src/MyModule.hs` and press Enter. GHCi will process your code, making its definitions available.

4.  **Interact with your functions:** Once the file is loaded, you can directly call any defined functions at the GHCi prompt. For instance, if you have a function `double x = x * 2`, you can test it by typing `double 5` and pressing Enter, which will output `10`.

**Recommendation:** The interactive `ghci` environment is highly valuable during development for quick experimentation, testing individual functions, and debugging. The immediate feedback helps in understanding code behavior rapidly.

**Considerations:** While excellent for interactive work, GHCi can be slower than compiling and running an executable, especially for larger projects or computationally intensive tasks, as it interprets code rather than executing optimized machine code.

### Option 2: Direct Execution with `ghc-run`

```bash
bin/ghc-run <hask_file>.hs
```

The `ghc-run` script, available within this repository, provides a convenient way to directly compile and run your Haskell source files. When executed, it performs the following actions:

1.  **Compilation:** The Glasgow Haskell Compiler (`ghc`) compiles the specified `<hask_file>.hs` into a standalone executable.
2.  **Execution:** The script then immediately runs this compiled executable.

**Usage:** Utilize `ghc-run` when you need to quickly execute a complete program or a specific test case without entering the interactive GHCi environment. It's particularly useful for running small utilities or automated tests within the repository.

---

## Queens nXn Board puzzle

| Board Size (N x N) | Total Solutions      | Unique Solutions     |
|----------------------|----------------------|----------------------|
| 1                    | 1                    | 1                    |
| 2                    | 0                    | 0                    |
| 3                    | 0                    | 0                    |
| 4                    | 2                    | 1                    |
| 5                    | 10                   | 2                    |
| 6                    | 4                    | 1                    |
| 7                    | 40                   | 6                    |
| 8                    | 92                   | 12                   |
| 9                    | 352                  | 46                   |
| 10                   | 724                  | 92                   |
| 11                   | 2,680                | 341                  |
| 12                   | 14,200               | 1,787                |
| 13                   | 73,712               | 9,233                |
| 14                   | 365,596              | 45,752               |
| 15                   | 2,279,184            | 285,053              |
| 16                   | 14,772,512           | 1,846,955            |
| 17                   | 95,815,104           | 11,977,939           |
| 18                   | 666,090,624          | 83,263,591           |
| 19                   | 4,968,057,848        | 621,012,754          |
| 20                   | 39,029,188,884       | 4,878,666,808        |
| 21                   | 314,666,222,712      | 39,333,324,973       |
| 22                   | 2,691,008,701,644    | 336,376,244,042      |
| 23                   | 24,233,937,684,440   | 3,029,242,658,210    |
| 24                   | 227,514,171,973,736  | 28,439,272,956,934   |
| 25                   | 2,207,893,435,808,350| 275,986,683,743,434  |
| 26                   | 22,317,699,616,364,000| 2,789,712,466,510,280|
| 27                   | 224,486,955,028,970,000| 28,060,873,303,785,000|

| Board Size (n) | Number Solutions | Queens.hs (seconds) | Queens2.hs (seconds) |
|----------------|-------------------|-----------------------|------------------------|
| 8              | 92                | 0.032578            | 0.005985             |
| 9              | 352               | 0.298950            | 0.019717             |
| 10             | 724               | 3.444155            | 0.101072             |
| 11             | 2680              | 42.235242           | 0.570311             |
| 12             | 14200             |                       | 3.401418             |
| 13             | 73712             |                       | 21.686160            |
| 14             | 365596            |                       | 149.064821           |
| 15             | 2279184           |                       | 1078.235401          |


---

## Installing Haskell (in codespaces)

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

sudo apt-get update
sudo apt-get install -y libgmp-dev



curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
stack install --fast
ihaskell install --stack

stack exec jupyter -- notebook

```


---

## Stuff I've Learned


```haskell
 main :: IO ()
 main = do
     let solsList = solveQueensNxN 8
         numSols = length solsList
     putStrLn $ "Number of Solutions = " ++ show numSols
     putStrLn $ "Solutions = " ++ show solsList
```

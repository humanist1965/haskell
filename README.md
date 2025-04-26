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

**Queens.hs timings:**

* Number Solutions = 92
* Queens Problem (nxn), for n = 8 (Real): 0.032578 seconds

* Number Solutions = 352
* Queens Problem (nxn), for n = 9 (Real): 0.298950 seconds

* Number Solutions = 724
* Queens Problem (nxn), for n = 10 (Real): 3.444155 seconds

* Number Solutions = 2680
* Queens Problem (nxn), for n = 11 (Real): 42.235242 seconds


**Queens2.hs timings:**

* Number Solutions = 92
* Queens Problem (nxn), for n = 8 (Real): 0.005985 seconds

* Number Solutions = 352
* Queens Problem (nxn), for n = 9 (Real): 0.019717 seconds

* Number Solutions = 724
* Queens Problem (nxn), for n = 10 (Real): 0.101072 seconds

* Number Solutions = 2680
* Queens Problem (nxn), for n = 11 (Real): 0.570311 seconds

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



 main :: IO ()
 main = do
     let solsList = solveQueensNxN 8
         numSols = length solsList
     putStrLn $ "Number of Solutions = " ++ show numSols
     putStrLn $ "Solutions = " ++ show solsList

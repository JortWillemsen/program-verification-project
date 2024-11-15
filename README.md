# INFOMPSV project template

This template includes a dev container configuration, as well as the Haskell code for a project with z3 and the GCL parser.  
The dev container provides all necessary dependencies for working on the project: z3 and Haskell tooling.

An example for using z3 is also included. More examples can be found [here](https://github.com/IagoAbal/haskell-z3/tree/master/examples).

## Using the dev container

Prerequisites:
- [Docker](https://www.docker.com/) is installed and running.
- The [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) for [Visual Studio Code (VSCode)](https://code.visualstudio.com/) is installed.

When opening this folder in VSCode, a popup should appear prompting you to reopen the folder in a dev container. If not, this can be done by pressing <kbd>F1</kbd> and finding the command `Dev Containers: Reopen in Container`.  
Now, the container will start to build. The first time, this may take a while.

When the container is finished building, this same folder will be opened inside the container. Now, you can start working on the project like normal.

## Notes on HLS

Whenever the cabal file is changed (`wlp-verifier.cabal` in this template), the project needs to be rebuilt (`cabal build`) for HLS to notice these changes.

Unfortunately, Haskell tooling is always somewhat unstable. This particular version requires `Main.hs` to always be open.  
If the language support is not working properly, try the following:
1. Check if it is still busy setting up. With a cabal project, this may take a bit of time.
2. (Re)open `Main.hs` if you see any errors, or if it is not doing anything at all.
3. If it is still not working, restart HLS. (<kbd>F1</kbd> -> `Haskell: Restart Haskell LSP Server`).
4. If this all fails, leave the dev container and reopen it.

## Running the project

Once you have the project running inside a dev container it is ready to use. You can also run the project without a dev container if your environment is setup correctly. 

### The program

To run the normal program you enter a file into `Main.hs` that you would like to check for validity along with the options such as k, n and pruneLength. After that you can type `cabal run` in your terminal and hit enter.

### Testing

To use the testing module you can simply type `cabal test` into the terminal and hit enter. It will output the results to the terminal and also a log file.

### Benchmarking

For the benchmarking you can simply type `cabal bench` into the terminal and hit enter. The output will we written to the terminal and a csv file in the root of the project. Please keep in mind that benchmarking can take extremely long depending on what you are benchmarking.
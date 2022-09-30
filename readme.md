# See deployed app

Here: https://jointprob.github.io/jointprob-shadow-cljs/
# Preparation and after any changes to package.json

Install node and then in root directory:

npm install

# Use with Editor
## Cursive
* run `npx shadow-cljs server` in IDE terminal
* use Clojure REPL → Remote Run Configuration to connect to the provided nREPL server.
  * Just select the "Use port from nREPL file" option

## Calva plug-in for Visual Studio Code

* Click on REPL at bottom of editor window.
* Click "Start your project with a REPL (aka Jack-in)" at the drop down that appears at the top of editor window."
* Select "shadow-cljs" project type.
* Select :my-build-id and press OK.
* You will see command "npx shadow-cljs -d cider/cider-nrepl:0.28.5 watch :my-build-id" run in the terminal window in the bottom pane of editor.
* Choose to connect to ":my-build-id" at the top.

## Cider on Emacs

* Similar to above?

# Automatic deployal to github page

Pushes to the master github branch are automatically deployed by github actions to : https://jointprob.github.io/jointprob-shadow-cljs/

See .github/workflows/main.yml file in this repo to find out how.

The automatic build should take less than 3 minutes after the push. Check here for build progress:

https://github.com/jointprob/jointprob-shadow-cljs/actions
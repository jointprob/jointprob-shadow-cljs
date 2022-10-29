# List Just options.
@list:
   just --list

# Init node packages on first install and after any change to package.json
@init:
   npm install

# SASS production build
@css:
    npm run build:sass

# SASS watch css files
@dev:
    npm run watch:sass

# Clean .cpcache and .shadow-cljs directories and other compiled files, run npm install.
@clean:
    rm -rf .cpcache/*
    rm -rf .shadow-cljs/*
    rm -rf public/js/*
    rm -rf public/css/*
    npm install

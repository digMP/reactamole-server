# Reactamole-server

A simple webapp that bundles up a basic editor and evaluation environment for Reactamole.

## Usage

`reactamole-server` uses [Servant](https://https://docs.servant.dev/en/stable/) and `servant-server`. To build and run the server, make sure that you have [Haskell Stack](https://haskellstack.org) installed and do the following:

~~~console
$> stack build
$> stack exec reactamole-server -- 8080
~~~

This will build and start `reactamole-server` listening to port `8080`.

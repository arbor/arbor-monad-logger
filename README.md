# arbor-monad-logger

Logging library that simplifies setting up an application to work with
[`monad-logger`](http://hackage.haskell.org/package/monad-logger)
API library using
[`fast-logger`](http://hackage.haskell.org/package/fast-logger)
as the underlying logger.

It also provides a useful non-overloaded API for logging against
`Control.Monad.Logger.MonadLogger`.

See the example app in the [`app`](https://github.com/packetloop/arbor-monad-logger/tree/master/app)
directory for more information.

The example app can be run with `cabal new-run arbor-monad-logger-example`

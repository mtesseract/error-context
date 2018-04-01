# !! This is experimental work-in-progress !!

Welcome to *error-context*! This is a library providing context-aware
error and exception handling for Haskell.

## What problem does *error-context* attempt to solve?

Good error handling is hard. Sometimes it happens that when
propagating errors some context is lost. Call traces sometimes help,
but the current solutions in Haskell-land for accessing call traces
are rather limited. Furthermore, sometimes call traces that at written
by and for humans are more convenient to read.

The *error-context* library allows you to easily attach call traces
('error contexts') to errors, in particular to exceptions. Special
catch-functions are provided for accessing these contexts.

## How to use it?

Add an `ErrorContextT` layer to your monad transformer stack by adding
`runErrorContextT` to the transformer unwrapping code.

The `ErrorContextT` transformer provides the context-enriching logic
via special implementations of `MonadThrow`, `MonadCatch` and
`MonadIO`.

## Examples

See https://github.com/mtesseract/error-context/blob/master/test/Control/Error/Context/Test.hs.

## What about "pure" exceptions?

The `ErrorContextT` transformer implements `MonadThrow` and `MonadIO`,
therefore exceptions thrown by `throwM` and via `liftIO` are
automatically context-enriched. But the story for exceptional values
created via

```haskell
throw :: Exception e => e -> a
```

are not context-enriched. But there is a workaround for this use-case:

```haskell
ensureExceptionContext :: (MonadCatch m, MonadErrorContext m) => m a -> m a
```

This function provides context-aware enriching for any exceptions
thrown within some monadic value, including those thrown by evaluating
values created by `throw`.

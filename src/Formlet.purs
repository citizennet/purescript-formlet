module Formlet
  ( Errors
  , Form(..)
  , Wizard(..)
  , class VariantForm
  , default
  , fallback
  , form
  , form_
  , hoistForm
  , invalid
  , listen
  , listenResult
  , mapConfig
  , mapRender
  , mapRender'
  , over
  , overRecord
  , render
  , traversing
  , unvalidated
  , validate
  , variant
  , withConfig
  , withValue
  ) where

import CitizenNet.Prelude hiding (over)

import Control.Alternative as Control.Alternative
import Control.Monad.Reader.Class as Control.Monad.Reader.Class
import Data.Lens as Data.Lens
import Data.Lens.Index as Data.Lens.Index
import Data.Lens.Indexed as Data.Lens.Indexed
import Data.Lens.Iso.Newtype as Data.Lens.Iso.Newtype
import Data.Lens.Record as Data.Lens.Record
import Data.Symbol as Data.Symbol
import Data.TraversableWithIndex as Data.TraversableWithIndex
import Data.Validation.Semigroup as Data.Validation.Semigroup
import Data.Variant as Data.Variant
import Prim.Row as Prim.Row
import Prim.RowList as Prim.RowList
import Record.Unsafe as Record.Unsafe

-- | A handy type synonym for the type of Form error messages.
type Errors =
  Array String

-- | A Form is essentially a function from an internal state to a validated
-- | result and a UI representation (aka. the *render* functor) that, when
-- | evaluated through user input, lets us update the form's internal state.
-- |
-- | Furthermore a Form includes access to a global `config` parameter that
-- | serves the purpose of passing read-only information, much like the `Reader`
-- | monad.  The config is useful to parametrize data needed deep into the form
-- | that may be changed from the outside, e.g. an API endpoint or any other
-- | configuration needed by specific input fields.
newtype Form config render (m :: Type -> Type) value result =
  Form
    ( config ->
      { render :: value -> render (m (value -> value))
      , validate :: value -> Data.Validation.Semigroup.V Errors result
      }
    )

derive instance newtypeForm :: Newtype (Form config render m value result) _
derive instance functorForm :: Functor (Form config render m value)

instance applyForm :: Semigroup (render (m (value -> value))) => Apply (Form config render m value) where
  apply (Form ff) (Form fa) =
    Form \config ->
      let
        { render: render1, validate: mf } = ff config

        { render: render2, validate: ma } = fa config
      in
        { render: render1 <> render2
        , validate: \value -> mf value <*> ma value
        }

instance applicativeForm :: Monoid (render (m (value -> value))) => Applicative (Form config render m value) where
  pure a = Form \_ -> { render: mempty, validate: \_ -> pure a }

instance altForm :: Semigroup (render (m (value -> value))) => Control.Alternative.Alt (Form config render m value) where
  alt (Form fa1) (Form fa2) =
    Form \config ->
      let
        { render: render1, validate: ma1 } = fa1 config

        { render: render2, validate: ma2 } = fa2 config
      in
        { render: render1 <> render2
        , validate:
            \value ->
              Data.Validation.Semigroup.V do
                case Data.Validation.Semigroup.toEither (ma1 value), Data.Validation.Semigroup.toEither (ma2 value) of
                  Right a1, Right _ -> Right a1
                  Right a1, Left _ -> Right a1
                  Left _, Right a2 -> Right a2
                  Left e1, Left e2 -> Left (e1 <> e2)
        }

-- | A `Wizard` is the sequential, monadic version of a `Form`. Since `Form`
-- | only has an `Applicative` instance (and not a `Monad` one), then it is
-- | impossible to represent sequential dependencies on a `Wizard`'s validated
-- | result.
-- |
-- | Since the monadic bind parameter is the validated result here, if a
-- | previous `Wizard` is invalid in a monadic chain, then the subsequent
-- | `Wizard`s will not be visible, as they inherently depend on that validated
-- | result.
-- |
-- | ```purescript
-- | type FormData = { age :: String, parentName :: String }
-- |
-- | type Result = Underage { parentName :: String } | Adult { age :: Int }
-- |
-- | myForm :: Formlet.Form _ _ FormData Result
-- | myForm = parallel do
-- |   age <-
-- |     sequential
-- |       $ Formlet.overRecord { age: _ }
-- |       $ Formlet.Validation.validated Formlet.Validation.isValidInt
-- |       $ someFormField
-- |   if age >= 18 then
-- |     pure $ Adult { age }
-- |   else
-- |     sequential
-- |       $ map (Underage <<< { parentName: _ })
-- |       $ Formlet.overRecord { parentName: _ }
-- |       $ someFormField
-- | ```
-- |
-- | In the above example, we use a `text` input for the `age`, which we want to
-- | be an `Int` inside the `Wizard`. Since we depend on the validated result of
-- | the first field, `age`, in the `if` clause, that part of the `Wizard` will
-- | only be evaluated and visible once `age` becomes a valid `Int`.
newtype Wizard config render (m :: Type -> Type) value result =
  Wizard (Form config render m value result)

derive instance newtypeWizard :: Newtype (Wizard config render m value result) _
derive instance functorWizard :: Functor (Wizard config render m value)

instance applyWizard :: Monoid (render (m (value -> value))) => Apply (Wizard config render m value) where
  apply = ap

derive newtype instance applicativeWizard :: Monoid (render (m (value -> value))) => Applicative (Wizard config render m value)

instance bindWizard :: Monoid (render (m (value -> value))) => Bind (Wizard config render m value) where
  bind (Wizard f) g =
    Wizard
      $ Form \config ->
          let
            { render: renderF, validate: validateF } = un Form f config
          in
            { render:
                \value -> case map g (validateF value) of
                  Data.Validation.Semigroup.V (Left _) -> renderF value
                  Data.Validation.Semigroup.V (Right (Wizard a)) -> renderF value <> (un Form a config).render value
            , validate:
                \value -> case map g (validateF value) of
                  Data.Validation.Semigroup.V (Left err) -> Data.Validation.Semigroup.V (Left err)
                  Data.Validation.Semigroup.V (Right (Wizard a)) -> (un Form a config).validate value
            }

instance monadWizard :: Monoid (render (m (value -> value))) => Monad (Wizard config render m value)

instance monadAskWizard :: Monoid (render (m (value -> value))) => Control.Monad.Reader.Class.MonadAsk config (Wizard config render m value) where
  ask = Wizard $ Form \config -> { render: mempty, validate: \_ -> pure config }

instance monadReaderWizard :: Monoid (render (m (value -> value))) => Control.Monad.Reader.Class.MonadReader config (Wizard config render m value) where
  local = Data.Lens.over Data.Lens.Iso.Newtype._Newtype <<< mapConfig

instance altWizard :: Semigroup (render (m (value -> value))) => Control.Alternative.Alt (Wizard config render m value) where
  alt (Wizard f) (Wizard g) =
    Wizard
      $ Form \config ->
          let
            { render: renderF, validate: validateF } = un Form f config

            { render: renderG, validate: validateG } = un Form g config
          in
            { render:
                \value -> case un Data.Validation.Semigroup.V (validateF value) of
                  Right _ -> renderF value
                  Left _ -> renderF value <> renderG value
            , validate:
                \value -> case un Data.Validation.Semigroup.V (validateF value), un Data.Validation.Semigroup.V (validateG value) of
                  Right a, Right _ -> Data.Validation.Semigroup.V (Right a)
                  Right a, Left _ -> Data.Validation.Semigroup.V (Right a)
                  Left _, Right b -> Data.Validation.Semigroup.V (Right b)
                  Left _, Left b -> Data.Validation.Semigroup.V (Left b)
            }

instance plusWizard :: Monoid (render (m (value -> value))) => Control.Alternative.Plus (Wizard config render m value) where
  empty = Wizard $ Form \_ -> { render: mempty, validate: \_ -> Data.Validation.Semigroup.invalid mempty }

instance alternativeWizard :: Monoid (render (m (value -> value))) => Control.Alternative.Alternative (Wizard config render m value)

instance parallelForm :: Monoid (render (m (value -> value))) => Parallel (Form config render m value) (Wizard config render m value) where
  parallel = un Wizard
  sequential = Wizard

-- | Override a Form's internal state with a default value if it hasn't yet been
-- | changed by the user.
default ::
  forall config render m value result.
  Functor render =>
  Functor m =>
  value ->
  Form config render m value result ->
  Form config render m (Maybe value) result
default defaultValue = over (Data.Lens.iso (fromMaybe defaultValue) Just)

-- | Use the specified default value as a fallback for a Form with a `Maybe _`
-- | value type, effectively converting it into a non-Maybe value.
fallback ::
  forall config render m value result.
  Functor render =>
  Functor m =>
  value ->
  Form config render m (Maybe value) result ->
  Form config render m value result
fallback defaultValue = over (Data.Lens.iso Just (fromMaybe defaultValue))

-- | Build a Form whose validated result may depend on the internal state.
form ::
  forall config render m value result.
  Functor render =>
  ( config ->
    { render :: value -> render (m (value -> value))
    , validate :: value -> Either String result
    }
  ) ->
  Form config render m value result
form f =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render
      , validate: Data.Validation.Semigroup.V <<< lmap pure <<< validate
      }

-- | Build a Form whose result is always valid and equal to its internal state.
-- | This is useful for building smaller, atomic form units.
form_ ::
  forall config render m value.
  Functor render =>
  (config -> value -> render (m (value -> value))) ->
  Form config render m value value
form_ f =
  Form \config ->
    { render: f config
    , validate: pure
    }

-- | Modify a Form's action monad given a natural transformation.
hoistForm ::
  forall config render m n value result.
  Functor render =>
  m ~> n ->
  Form config render m value result ->
  Form config render n value result
hoistForm nt (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render: map nt <<< render, validate }

-- | Build an empty, invalid Form with the specified error messages.
invalid ::
  forall config render m value result.
  Monoid (render (m (value -> value))) =>
  Errors ->
  Form config render m value result
invalid err = Form \_ -> { render: mempty, validate: \_ -> Data.Validation.Semigroup.invalid err }

-- | Listen for any state changes on a piece of Form and allow for performing
-- | monadic effects in `m` that may produce further state changes. This is
-- | useful, for example, to set a field's value based on changes to another
-- | field.
listen ::
  forall config render m value result.
  Functor render =>
  Monad m =>
  (value -> m (value -> value)) ->
  Form config render m value result ->
  Form config render m value result
listen callback (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render:
          \value ->
            render value
              # map \action -> do
                  update <- action
                  update' <- callback (update value)
                  pure (update' <<< update)
      , validate
      }

-- | Listen for any changes on the validated result of a piece of Form and allow
-- | for performing monadic effects in `m` that may produce further state changes.
-- | This is useful, for example, to set a field's value based on changes to
-- | the validated result of another field.
listenResult ::
  forall config render m value result.
  Functor render =>
  Monad m =>
  (result -> m (value -> value)) ->
  Form config render m value result ->
  Form config render m value result
listenResult callback (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render:
          \value ->
            render value
              # map \action -> do
                  update <- action
                  case validate (update value) of
                    Data.Validation.Semigroup.V (Left _) -> pure update
                    Data.Validation.Semigroup.V (Right result) -> do
                      update' <- callback result
                      pure (update' <<< update)
      , validate
      }

-- | Modify the local `config` of a Form. This can be used, for example, to
-- | locally override global configurations, such as setting a specific section
-- | of a form to be read-only with `mapConfig _ { readonly = true }`.
mapConfig ::
  forall config1 config2 render m value result.
  (config2 -> config1) ->
  Form config1 render m value result ->
  Form config2 render m value result
mapConfig k (Form f) = Form (f <<< k)

-- | Modify the rendered representation of a Form.
mapRender ::
  forall config render1 render2 m value result.
  render1 ~> render2 ->
  Form config render1 m value result ->
  Form config render2 m value result
mapRender nt = mapRender' (\_ _ _ -> nt)

-- | Modify the rendered representation of a Form based also on its config,
-- | internal state and validated result.
mapRender' ::
  forall config render1 render2 m value result.
  (config -> value -> Either Errors result -> render1 ~> render2) ->
  Form config render1 m value result ->
  Form config render2 m value result
mapRender' nt (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render: \value -> nt config value (un Data.Validation.Semigroup.V (validate value)) (render value)
      , validate
      }

-- | Modify a piece of Form so that its internal state is focused in a single
-- | part of the larger state of the Form given a `Lens'` that performs the
-- | focusing. For example:
-- |
-- | ```purescript
-- | type FormData = { firstName :: String, lastName :: String }
-- |
-- | fullName :: Formlet.Form _ _ FormData String
-- | fullName = ado
-- |   firstName <- Formlet.over (Data.Lens.lens _.firstName _ { firstName = _ }) someFormField
-- |   lastName <- Formlet.over (Data.Lens.lens _.lastName _ { lastName = _ }) someFormField
-- |   in firstName <> " " <> lastName
-- | ```
-- |
-- | In the example above, each individual `someFormField` input has `String` as
-- | its internal state type. If we didn't use `over`, the internal state type
-- | would be `FormData`, which doesn't unify with `String`. More importantly,
-- | though, is the fact that, because of `over`, each individual `someFormField`
-- | input *cannot* edit any other part of the form data.
over ::
  forall config render m s a result.
  Functor render =>
  Functor m =>
  Lens' s a ->
  Form config render m a result ->
  Form config render m s result
over l (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render: map (map l) <<< render <<< Data.Lens.view l
      , validate: validate <<< Data.Lens.view l
      }

-- | Modify a piece of Form so that its internal state is focused in a single
-- | field of a record type. Instead of a `Lens'`, like in `over`, this variant
-- | uses a Record with a single field, e.g. `Formlet.overRecord { myField: _ }`:
-- |
-- | ```purescript
-- | type FormData = { firstName :: String, lastName :: String }
-- |
-- | fullName :: Formlet.Form _ _ FormData String
-- | fullName = ado
-- |   firstName <- Formlet.overRecord { firstName: _ } someFormField
-- |   lastName <- Formlet.overRecord { lastName: _ } someFormField
-- |   in firstName <> " " <> lastName
-- | ```
overRecord ::
  forall sym row anything config render m r r' a result.
  Functor render =>
  Functor m =>
  IsSymbol sym =>
  Prim.Row.Cons sym a r' r =>
  Prim.RowList.RowToList row (Prim.RowList.Cons sym anything Prim.RowList.Nil) =>
  (anything -> Record row) ->
  Form config render m a result ->
  Form config render m (Record r) result
overRecord _ = over (Data.Lens.Record.prop (Proxy :: Proxy sym))

-- | Run a form through an input value and config and retrieves its rendered
-- | representation.
render ::
  forall config render m value result.
  Functor render =>
  Form config render m value result ->
  config ->
  value ->
  render (m (value -> value))
render (Form f) = _.render <<< f

-- | This indexed lens works across all the contents of a container
-- | similarly to `over` and `overRecord` allowing the Formlet sub-clauses
-- | to be written cleanly in terms of the type within the container
-- TODO: try and find a way of not using `Data.Lens.Index.Index` here
-- TODO: try and find a way of using `Traversable` instead of `TraversableWithIndex`
traversing ::
  forall a config f index m render result.
  Functor render =>
  Functor m =>
  Monoid (render (m (f a -> f a))) =>
  Data.TraversableWithIndex.TraversableWithIndex index f =>
  Data.Lens.Index.Index (f a) index a =>
  Form config render m a result ->
  Form config render m (f a) (f result)
traversing (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render: Data.Lens.ifoldMapOf Data.Lens.Indexed.itraversed (\index -> map (map (Data.Lens.Index.ix index)) <<< render)
      , validate: traverse validate
      }

-- | Defer the Form's validation to the result, thus making the Form always
-- | appear as valid.
unvalidated ::
  forall config render m value result.
  Form config render m value result ->
  Form config render m value (Either Errors result)
unvalidated (Form f) =
  Form \config ->
    let
      { render, validate } = f config
    in
      { render
      , validate: pure <<< un Data.Validation.Semigroup.V <<< validate
      }

-- | Get the validated result of a Form through an input value and config and
-- | retrieves its rendered representation.
validate ::
  forall config render m value result.
  Form config render m value result ->
  config ->
  value ->
  Either Errors result
validate (Form f) = map (un Data.Validation.Semigroup.V) <<< _.validate <<< f

-- | Build a form that has a `Variant` as its internal state type provided that
-- | we specify a `Form` for each one of the `Variant`'s cases. E.g.:
-- |
-- | ```purescript
-- | type FormData
-- |   = Variant
-- |       ( guestFormData :: { firstName :: String, lastName :: String }
-- |       , adminFormData :: { email :: String, password :: String }
-- |       )
-- |
-- | data Result
-- |   = Guest { name :: String }
-- |   | Admin { email :: String, password :: String }
-- |
-- | myForm :: Formlet.Form _ _ FormData Result
-- | myForm =
-- |   variant
-- |     { guestFormData: ado
-- |         firstName <- Formlet.overRecord { firstName: _ } someFormField
-- |         lastName <- Formlet.overRecord { lastName: _ } someFormField
-- |         in Guest { name: firstName <> " " <> lastName }
-- |     , adminFormData: ado
-- |         email <- Formlet.overRecord { email: _ } someFormField
-- |         password <- Formlet.overRecord { password: _ } someFormField
-- |         in Admin { email, password }
-- |     }
-- | ```
variant ::
  forall config render m result list record variant.
  Functor render =>
  Functor m =>
  Prim.RowList.RowToList variant list =>
  VariantForm list record config render m result =>
  Record record ->
  Form config render m (Variant variant) result
variant record =
  Form \config ->
    { render:
        \variantValue -> case Data.Variant.unvariant variantValue of
          Data.Variant.Unvariant withVariant ->
            withVariant \label ->
              map (map \update variantValue' -> maybe variantValue' (inj label <<< update) (Data.Variant.prj label variantValue'))
                <<< render (Record.Unsafe.unsafeGet (Data.Symbol.reflectSymbol label) record) config
    , validate:
        \variantValue -> case Data.Variant.unvariant variantValue of
          Data.Variant.Unvariant withVariant ->
            withVariant \label ->
              (un Form (Record.Unsafe.unsafeGet (Data.Symbol.reflectSymbol label) record) config).validate
    }

-- | Helper class that checks whether a specific record's row-list can be used
-- | to pattern match on a specific variant's row in the `variant` Form
-- | combinator above. This works as a proof that we can safely use
-- | `Record.unsafeGet` in `variant`.
class VariantForm (list :: Prim.RowList.RowList Type) (record :: Row Type) (config :: Type) (render :: Type -> Type) (m :: Type -> Type) (result :: Type) | list -> record config render m result

instance variantFormNil :: VariantForm Prim.RowList.Nil () config render m result
else instance variantFormCons ::
  ( VariantForm list record' config render m result
  , Prim.Row.Cons sym (Form config render m value result) record' record
  ) =>
  VariantForm (Prim.RowList.Cons sym value list) record config render m result

-- | Make the Form's global config available in the scope.
-- | This is useful when you want to have some aspect of the form depend on any
-- | global configuration.
withConfig ::
  forall config render m value result.
  (config -> Form config render m value result) ->
  Form config render m value result
withConfig f = Form \config -> un Form (f config) config

-- | Make the Form's internal state available in the scope.
-- | This allows for changing the structure of a form based on the current value.
withValue ::
  forall config render m value result.
  (value -> Form config render m value result) ->
  Form config render m value result
withValue f =
  Form \config ->
    { render: \value -> (un Form (f value) config).render value
    , validate: \value -> (un Form (f value) config).validate value
    }

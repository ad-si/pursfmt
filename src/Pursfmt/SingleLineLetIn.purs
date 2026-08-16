module Pursfmt.SingleLineLetIn
  ( SingleLineLetInOption(..)
  , shouldSingleLineLetIn
  , letInHasNoComments
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (any, foldl)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import PureScript.CST.Types (AdoBlock, AppSpine(..), Binder(..), CaseOf, Comment(..), Delimited, DelimitedNonEmpty, DoBlock, DoStatement(..), Expr(..), Guarded(..), GuardedExpr(..), Ident, IfThenElse, Labeled(..), Lambda, LetBinding(..), LetIn, Name(..), PatternGuard(..), Prefixed(..), QualifiedName(..), RecordAccessor, RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), SourceToken, Type(..), TypeVarBinding(..), ValueBindingFields, Where(..), Wrapped(..))

data SingleLineLetInOption
  = SingleLineLetInNever
  | SingleLineLetInSmart
  | SingleLineLetInAlways

derive instance eqSingleLineLetInOption :: Eq SingleLineLetInOption

shouldSingleLineLetIn :: forall e. SingleLineLetInOption -> LetIn e -> Boolean
shouldSingleLineLetIn option letIn =
  NonEmptyArray.length letIn.bindings == 1
    && letInHasNoComments letIn
    && case option of
      SingleLineLetInNever ->
        false
      SingleLineLetInAlways ->
        true
      SingleLineLetInSmart ->
        isSmartSingleLineLetIn letIn

letInHasNoComments :: forall e. LetIn e -> Boolean
letInHasNoComments = not <<< letInHasComments

letInHasComments :: forall e. LetIn e -> Boolean
letInHasComments letIn =
  tokenHasComments letIn.keyword
    || any letBindingHasComments letIn.bindings
    || tokenHasComments letIn.in
    || exprHasComments letIn.body

letBindingHasComments :: forall e. LetBinding e -> Boolean
letBindingHasComments = case _ of
  LetBindingName fields ->
    valueBindingFieldsHasComments fields
  LetBindingPattern binder tok where_ ->
    binderHasComments binder
      || tokenHasComments tok
      || whereHasComments where_
  LetBindingSignature labeled ->
    labeledHasComments nameHasComments typeHasComments labeled
  LetBindingError _ ->
    true

valueBindingFieldsHasComments :: forall e. ValueBindingFields e -> Boolean
valueBindingFieldsHasComments fields =
  nameHasComments fields.name
    || any binderHasComments fields.binders
    || guardedHasComments fields.guarded

guardedHasComments :: forall e. Guarded e -> Boolean
guardedHasComments = case _ of
  Unconditional tok where_ ->
    tokenHasComments tok || whereHasComments where_
  Guarded guarded ->
    any guardedExprHasComments guarded

guardedExprHasComments :: forall e. GuardedExpr e -> Boolean
guardedExprHasComments (GuardedExpr guarded) =
  tokenHasComments guarded.bar
    || separatedHasComments patternGuardHasComments guarded.patterns
    || tokenHasComments guarded.separator
    || whereHasComments guarded.where

patternGuardHasComments :: forall e. PatternGuard e -> Boolean
patternGuardHasComments (PatternGuard guard) =
  maybe false (\(Tuple binder tok) -> binderHasComments binder || tokenHasComments tok) guard.binder
    || exprHasComments guard.expr

whereHasComments :: forall e. Where e -> Boolean
whereHasComments (Where where_) =
  exprHasComments where_.expr
    || maybe false
      (\(Tuple tok bindings) -> tokenHasComments tok || any letBindingHasComments bindings)
      where_.bindings

exprHasComments :: forall e. Expr e -> Boolean
exprHasComments = case _ of
  ExprHole name ->
    nameHasComments name
  ExprSection tok ->
    tokenHasComments tok
  ExprIdent name ->
    qualifiedNameHasComments name
  ExprConstructor name ->
    qualifiedNameHasComments name
  ExprBoolean tok _ ->
    tokenHasComments tok
  ExprChar tok _ ->
    tokenHasComments tok
  ExprString tok _ ->
    tokenHasComments tok
  ExprInt tok _ ->
    tokenHasComments tok
  ExprNumber tok _ ->
    tokenHasComments tok
  ExprArray delimited ->
    delimitedHasComments exprHasComments delimited
  ExprRecord delimited ->
    delimitedHasComments (recordLabeledHasComments exprHasComments) delimited
  ExprParens wrapped ->
    wrappedHasComments exprHasComments wrapped
  ExprTyped expr tok ty ->
    exprHasComments expr || tokenHasComments tok || typeHasComments ty
  ExprInfix expr terms ->
    exprHasComments expr
      || any (\(Tuple wrapped term) -> wrappedHasComments exprHasComments wrapped || exprHasComments term) terms
  ExprOp expr terms ->
    exprHasComments expr
      || any (\(Tuple op term) -> qualifiedNameHasComments op || exprHasComments term) terms
  ExprOpName name ->
    qualifiedNameHasComments name
  ExprNegate tok expr ->
    tokenHasComments tok || exprHasComments expr
  ExprRecordAccessor accessor ->
    recordAccessorHasComments accessor
  ExprRecordUpdate expr updates ->
    exprHasComments expr || delimitedNonEmptyHasComments recordUpdateHasComments updates
  ExprApp expr terms ->
    exprHasComments expr || any (appSpineHasComments exprHasComments) terms
  ExprLambda lambda ->
    lambdaHasComments lambda
  ExprIf ifThenElse ->
    ifThenElseHasComments ifThenElse
  ExprCase caseOf ->
    caseOfHasComments caseOf
  ExprLet letIn ->
    letInHasComments letIn
  ExprDo doBlock ->
    doBlockHasComments doBlock
  ExprAdo adoBlock ->
    adoBlockHasComments adoBlock
  ExprError _ ->
    true

appSpineHasComments :: forall e f. (f e -> Boolean) -> AppSpine f e -> Boolean
appSpineHasComments hasComments = case _ of
  AppType tok ty ->
    tokenHasComments tok || typeHasComments ty
  AppTerm term ->
    hasComments term

recordAccessorHasComments :: forall e. RecordAccessor e -> Boolean
recordAccessorHasComments accessor =
  exprHasComments accessor.expr
    || tokenHasComments accessor.dot
    || separatedHasComments nameHasComments accessor.path

recordUpdateHasComments :: forall e. RecordUpdate e -> Boolean
recordUpdateHasComments = case _ of
  RecordUpdateLeaf name tok expr ->
    nameHasComments name || tokenHasComments tok || exprHasComments expr
  RecordUpdateBranch name updates ->
    nameHasComments name || delimitedNonEmptyHasComments recordUpdateHasComments updates

lambdaHasComments :: forall e. Lambda e -> Boolean
lambdaHasComments lambda =
  tokenHasComments lambda.symbol
    || any binderHasComments lambda.binders
    || tokenHasComments lambda.arrow
    || exprHasComments lambda.body

ifThenElseHasComments :: forall e. IfThenElse e -> Boolean
ifThenElseHasComments ifThenElse =
  tokenHasComments ifThenElse.keyword
    || exprHasComments ifThenElse.cond
    || tokenHasComments ifThenElse.then
    || exprHasComments ifThenElse.true
    || tokenHasComments ifThenElse.else
    || exprHasComments ifThenElse.false

caseOfHasComments :: forall e. CaseOf e -> Boolean
caseOfHasComments caseOf =
  tokenHasComments caseOf.keyword
    || separatedHasComments exprHasComments caseOf.head
    || tokenHasComments caseOf.of
    || any
      (\(Tuple binders guarded) -> separatedHasComments binderHasComments binders || guardedHasComments guarded)
      caseOf.branches

doBlockHasComments :: forall e. DoBlock e -> Boolean
doBlockHasComments doBlock =
  tokenHasComments doBlock.keyword
    || any doStatementHasComments doBlock.statements

adoBlockHasComments :: forall e. AdoBlock e -> Boolean
adoBlockHasComments adoBlock =
  tokenHasComments adoBlock.keyword
    || any doStatementHasComments adoBlock.statements
    || tokenHasComments adoBlock.in
    || exprHasComments adoBlock.result

doStatementHasComments :: forall e. DoStatement e -> Boolean
doStatementHasComments = case _ of
  DoLet tok bindings ->
    tokenHasComments tok || any letBindingHasComments bindings
  DoDiscard expr ->
    exprHasComments expr
  DoBind binder tok expr ->
    binderHasComments binder || tokenHasComments tok || exprHasComments expr
  DoError _ ->
    true

typeHasComments :: forall e. Type e -> Boolean
typeHasComments = case _ of
  TypeVar name ->
    nameHasComments name
  TypeConstructor name ->
    qualifiedNameHasComments name
  TypeWildcard tok ->
    tokenHasComments tok
  TypeHole name ->
    nameHasComments name
  TypeString tok _ ->
    tokenHasComments tok
  TypeInt sign tok _ ->
    maybe false tokenHasComments sign || tokenHasComments tok
  TypeRow wrapped ->
    wrappedHasComments rowHasComments wrapped
  TypeRecord wrapped ->
    wrappedHasComments rowHasComments wrapped
  TypeForall tok vars dot ty ->
    tokenHasComments tok
      || any typeVarBindingHasComments vars
      || tokenHasComments dot
      || typeHasComments ty
  TypeKinded ty tok kind ->
    typeHasComments ty || tokenHasComments tok || typeHasComments kind
  TypeApp ty args ->
    typeHasComments ty || any typeHasComments args
  TypeOp ty terms ->
    typeHasComments ty
      || any (\(Tuple op term) -> qualifiedNameHasComments op || typeHasComments term) terms
  TypeOpName name ->
    qualifiedNameHasComments name
  TypeArrow lhs tok rhs ->
    typeHasComments lhs || tokenHasComments tok || typeHasComments rhs
  TypeArrowName tok ->
    tokenHasComments tok
  TypeConstrained constraint tok ty ->
    typeHasComments constraint || tokenHasComments tok || typeHasComments ty
  TypeParens wrapped ->
    wrappedHasComments typeHasComments wrapped
  TypeError _ ->
    true

rowHasComments :: forall e. Row e -> Boolean
rowHasComments (Row row) =
  maybe false (separatedHasComments (labeledHasComments nameHasComments typeHasComments)) row.labels
    || maybe false (\(Tuple tok ty) -> tokenHasComments tok || typeHasComments ty) row.tail

typeVarBindingHasComments :: forall e. TypeVarBinding (Prefixed (Name Ident)) e -> Boolean
typeVarBindingHasComments = case _ of
  TypeVarKinded wrapped ->
    wrappedHasComments (labeledHasComments prefixedNameHasComments typeHasComments) wrapped
  TypeVarName name ->
    prefixedNameHasComments name

binderHasComments :: forall e. Binder e -> Boolean
binderHasComments = case _ of
  BinderWildcard tok ->
    tokenHasComments tok
  BinderVar name ->
    nameHasComments name
  BinderNamed name tok binder ->
    nameHasComments name || tokenHasComments tok || binderHasComments binder
  BinderConstructor name binders ->
    qualifiedNameHasComments name || any binderHasComments binders
  BinderBoolean tok _ ->
    tokenHasComments tok
  BinderChar tok _ ->
    tokenHasComments tok
  BinderString tok _ ->
    tokenHasComments tok
  BinderInt sign tok _ ->
    maybe false tokenHasComments sign || tokenHasComments tok
  BinderNumber sign tok _ ->
    maybe false tokenHasComments sign || tokenHasComments tok
  BinderArray delimited ->
    delimitedHasComments binderHasComments delimited
  BinderRecord delimited ->
    delimitedHasComments (recordLabeledHasComments binderHasComments) delimited
  BinderParens wrapped ->
    wrappedHasComments binderHasComments wrapped
  BinderTyped binder tok ty ->
    binderHasComments binder || tokenHasComments tok || typeHasComments ty
  BinderOp binder terms ->
    binderHasComments binder
      || any (\(Tuple op term) -> qualifiedNameHasComments op || binderHasComments term) terms
  BinderError _ ->
    true

nameHasComments :: forall a. Name a -> Boolean
nameHasComments (Name name) =
  tokenHasComments name.token

prefixedNameHasComments :: forall a. Prefixed (Name a) -> Boolean
prefixedNameHasComments (Prefixed name) =
  maybe false tokenHasComments name.prefix
    || nameHasComments name.value

qualifiedNameHasComments :: forall a. QualifiedName a -> Boolean
qualifiedNameHasComments (QualifiedName name) =
  tokenHasComments name.token

labeledHasComments :: forall a b. (a -> Boolean) -> (b -> Boolean) -> Labeled a b -> Boolean
labeledHasComments left right (Labeled labeled) =
  left labeled.label
    || tokenHasComments labeled.separator
    || right labeled.value

recordLabeledHasComments :: forall a. (a -> Boolean) -> RecordLabeled a -> Boolean
recordLabeledHasComments hasComments = case _ of
  RecordPun name ->
    nameHasComments name
  RecordField name tok value ->
    nameHasComments name || tokenHasComments tok || hasComments value

delimitedHasComments :: forall a. (a -> Boolean) -> Delimited a -> Boolean
delimitedHasComments hasComments =
  wrappedHasComments (maybe false (separatedHasComments hasComments))

delimitedNonEmptyHasComments :: forall a. (a -> Boolean) -> DelimitedNonEmpty a -> Boolean
delimitedNonEmptyHasComments =
  wrappedHasComments <<< separatedHasComments

wrappedHasComments :: forall a. (a -> Boolean) -> Wrapped a -> Boolean
wrappedHasComments hasComments (Wrapped wrapped) =
  tokenHasComments wrapped.open
    || hasComments wrapped.value
    || tokenHasComments wrapped.close

separatedHasComments :: forall a. (a -> Boolean) -> Separated a -> Boolean
separatedHasComments hasComments (Separated separated) =
  hasComments separated.head
    || any (\(Tuple tok value) -> tokenHasComments tok || hasComments value) separated.tail

tokenHasComments :: SourceToken -> Boolean
tokenHasComments token =
  any isComment token.leadingComments
    || any isComment token.trailingComments

isComment :: forall l. Comment l -> Boolean
isComment = case _ of
  Comment _ -> true
  Space _ -> false
  Line _ _ -> false

isSmartSingleLineLetIn :: forall e. LetIn e -> Boolean
isSmartSingleLineLetIn letIn =
  isSimpleLetBinding (NonEmptyArray.head letIn.bindings)
    && isSimpleLetBody letIn.body

isSimpleLetBinding :: forall e. LetBinding e -> Boolean
isSimpleLetBinding = case _ of
  LetBindingName { binders, guarded } ->
    Array.null binders && case guarded of
      Unconditional _ (Where { expr, bindings }) ->
        not isJust bindings && isSimpleLetBody expr
      Guarded _ ->
        false
  LetBindingPattern binder _ (Where { expr, bindings }) ->
    isSimpleBinder binder && not isJust bindings && isSimpleLetBody expr
  LetBindingSignature _ ->
    false
  LetBindingError _ ->
    false

isSimpleLetBody :: forall e. Expr e -> Boolean
isSimpleLetBody expr = maybe false (_ <= 8) (simpleExprScore expr)

simpleExprScore :: forall e. Expr e -> Maybe Int
simpleExprScore = case _ of
  ExprHole _ -> Just 1
  ExprSection _ -> Just 1
  ExprIdent _ -> Just 1
  ExprConstructor _ -> Just 1
  ExprBoolean _ _ -> Just 1
  ExprChar _ _ -> Just 1
  ExprString _ _ -> Just 1
  ExprInt _ _ -> Just 1
  ExprNumber _ _ -> Just 1
  ExprOpName _ -> Just 1
  ExprParens (Wrapped { value }) ->
    simpleExprScore value
  ExprTyped expr _ ty ->
    addSimpleScores 1 [ simpleExprScore expr, simpleTypeScore ty ]
  ExprNegate _ expr ->
    addSimpleScore 1 (simpleExprScore expr)
  ExprRecordAccessor { expr } ->
    addSimpleScore 1 (simpleExprScore expr)
  ExprApp expr terms ->
    addSimpleScores 1 $ Array.cons (simpleExprScore expr) (map simpleAppSpineScore (NonEmptyArray.toArray terms))
  ExprOp expr terms ->
    addSimpleScores 1 $ Array.cons (simpleExprScore expr) (map (simpleExprScore <<< snd) (NonEmptyArray.toArray terms))
  ExprInfix expr terms ->
    addSimpleScores 1 $ Array.cons (simpleExprScore expr) (map (simpleExprScore <<< snd) (NonEmptyArray.toArray terms))
  ExprArray _ ->
    Nothing
  ExprRecord _ ->
    Nothing
  ExprRecordUpdate _ _ ->
    Nothing
  ExprLambda _ ->
    Nothing
  ExprIf _ ->
    Nothing
  ExprCase _ ->
    Nothing
  ExprLet _ ->
    Nothing
  ExprDo _ ->
    Nothing
  ExprAdo _ ->
    Nothing
  ExprError _ ->
    Nothing

simpleAppSpineScore :: forall e. AppSpine Expr e -> Maybe Int
simpleAppSpineScore = case _ of
  AppTerm expr ->
    simpleExprScore expr
  AppType _ _ ->
    Nothing

simpleTypeScore :: forall e. Type e -> Maybe Int
simpleTypeScore = case _ of
  TypeVar _ -> Just 1
  TypeConstructor _ -> Just 1
  TypeWildcard _ -> Just 1
  TypeHole _ -> Just 1
  TypeString _ _ -> Just 1
  TypeInt _ _ _ -> Just 1
  TypeOpName _ -> Just 1
  TypeArrowName _ -> Just 1
  TypeParens (Wrapped { value }) ->
    simpleTypeScore value
  TypeKinded ty _ kind ->
    addSimpleScores 1 [ simpleTypeScore ty, simpleTypeScore kind ]
  TypeApp ty args ->
    addSimpleScores 1 $ Array.cons (simpleTypeScore ty) (map simpleTypeScore (NonEmptyArray.toArray args))
  TypeOp ty terms ->
    addSimpleScores 1 $ Array.cons (simpleTypeScore ty) (map (simpleTypeScore <<< snd) (NonEmptyArray.toArray terms))
  TypeArrow lhs _ rhs ->
    addSimpleScores 1 [ simpleTypeScore lhs, simpleTypeScore rhs ]
  TypeConstrained constraint _ ty ->
    addSimpleScores 1 [ simpleTypeScore constraint, simpleTypeScore ty ]
  TypeRow _ ->
    Nothing
  TypeRecord _ ->
    Nothing
  TypeForall _ _ _ _ ->
    Nothing
  TypeError _ ->
    Nothing

addSimpleScore :: Int -> Maybe Int -> Maybe Int
addSimpleScore n = map (_ + n)

addSimpleScores :: Int -> Array (Maybe Int) -> Maybe Int
addSimpleScores base =
  map (foldl (+) base) <<< traverse identity

isSimpleBinder :: forall e. Binder e -> Boolean
isSimpleBinder binder = maybe false (_ <= 8) (simpleBinderScore binder)

simpleBinderScore :: forall e. Binder e -> Maybe Int
simpleBinderScore = case _ of
  BinderWildcard _ -> Just 1
  BinderVar _ -> Just 1
  BinderNamed _ _ binder ->
    addSimpleScore 1 (simpleBinderScore binder)
  BinderConstructor _ binders ->
    addSimpleScores 1 (map simpleBinderScore binders)
  BinderBoolean _ _ -> Just 1
  BinderChar _ _ -> Just 1
  BinderString _ _ -> Just 1
  BinderInt _ _ _ -> Just 1
  BinderNumber _ _ _ -> Just 1
  BinderParens (Wrapped { value }) ->
    simpleBinderScore value
  BinderTyped binder _ ty ->
    addSimpleScores 1 [ simpleBinderScore binder, simpleTypeScore ty ]
  BinderArray binders ->
    simpleDelimitedScore simpleBinderScore binders
  BinderRecord binders ->
    simpleDelimitedScore simpleRecordBinderScore binders
  BinderOp _ _ ->
    Nothing
  BinderError _ ->
    Nothing

simpleRecordBinderScore :: forall e. RecordLabeled (Binder e) -> Maybe Int
simpleRecordBinderScore = case _ of
  RecordPun _ ->
    Just 1
  RecordField _ _ binder ->
    addSimpleScore 1 (simpleBinderScore binder)

simpleDelimitedScore :: forall a. (a -> Maybe Int) -> Delimited a -> Maybe Int
simpleDelimitedScore score (Wrapped { value }) =
  addSimpleScore 1 $ case value of
    Nothing ->
      Just 0
    Just separated ->
      simpleSeparatedScore score separated

simpleSeparatedScore :: forall a. (a -> Maybe Int) -> Separated a -> Maybe Int
simpleSeparatedScore score (Separated { head, tail }) =
  addSimpleScores 0 $ Array.cons (score head) (map (score <<< snd) tail)

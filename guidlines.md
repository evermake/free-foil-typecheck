# Practical Implementation Guidelines

A guide for implementing type systems using the bidirectional typechecking framework

## Contents

1. [Direct Correspondence Between Rules and Implementation](#1-direct-correspondence-between-rules-and-implementation)
2. [Pattern-Based Implementation Strategy](#2-pattern-based-implementation-strategy)
   - [2.1 Simple Type Synthesis](#21-simple-type-synthesis)
   - [2.2 Binding Constructs](#22-binding-constructs)
   - [2.3 Type-Level Operations](#23-type-level-operations)
3. [Bidirectional Mode Selection](#3-bidirectional-mode-selection)
4. [Type Annotations](#4-type-annotations)
5. [Scope Management Best Practices](#5-scope-management-best-practices)

This guide demonstrates key principles for implementing type systems using the bidirectional typechecking framework. The examples show how to implement System F using the `TypingSig` typeclass.

## 1. Direct Correspondence Between Rules and Implementation

Each typing rule in the formal system maps directly to a corresponding case in the `inferSig` or `checkSig` implementation. The bidirectional nature of the system determines which function implements each rule:

- **Synthesis rules (⇒)** correspond to `inferSig` cases
- **Checking rules (⇐)** correspond to `checkSig` cases
- **Both modes** are needed when constructs appear in different contexts

## 2. Pattern-Based Implementation Strategy

### 2.1 Simple Type Synthesis

For expressions with fixed types (literals, arithmetic operations), the implementation pattern is straightforward. Consider the addition operation:

```
Γ ⊢ e₁ ⇐ Nat    Γ ⊢ e₂ ⇐ Nat
------------------------------- Add⇒
    Γ ⊢ e₁ + e₂ ⇒ Nat
```

```haskell
inferSig scope = \case
  EAddSig l r -> do
    check l (Term TNat)
    check r (Term TNat)
    return (Term TNat)
```

Similar patterns apply to:
- Boolean literals (`ETrueSig`, `EFalseSig`)
- Natural number literals (`ENatSig`)
- Comparison operations

### 2.2 Binding Constructs

Expressions that introduce bindings follow a consistent pattern. The let expression demonstrates this:

```
Γ ⊢ e₁ ⇒ τ₁    Γ, x:τ₁ ⊢ e₂ ⇒ τ₂
----------------------------------- Let⇒
    Γ ⊢ let x = e₁ in e₂ ⇒ τ₂
```

```haskell
inferSig scope = \case
  ELetSig e body -> do
    a <- infer e
    Scoped c bodyType <- infer (body (Just a))
    case Foil.assertDistinct c of
      Foil.Distinct ->
        unsinkType scope bodyType
```

The checking mode propagates the expected type:

```haskell
checkSig scope = \case
  ELetSig expr buildBody -> \expectedType -> do
    exprType <- infer expr
    check (buildBody (Just exprType))
          (triviallyScoped (nameMapToScope scope) expectedType)
```

### 2.3 Type-Level Operations

Type abstractions and applications require special handling. Type abstraction checking follows this rule:

```
    Γ, α ⊢ e ⇐ τ
--------------------- TAbs⇐
Γ ⊢ Λα.e ⇐ ∀α.τ
```

```haskell
checkSig scope = \case
  ETAbsSig body -> \case
    Term (TForAll x bodyType) ->
      check (body (Just (Term TType))) (Scoped x (Term bodyType))
    _ -> Left "unexpected type abstraction"
```

Type application performs substitution:

```
Γ ⊢ e ⇒ ∀α.τ
----------------------- TApp⇒
Γ ⊢ e[τ'] ⇒ τ[τ'/α]
```

```haskell
inferSig scope = \case
  ETAppSig body arg -> do
    check arg (Term TType)
    (Term argType) <- infer arg
    bodyType <- infer body
    case bodyType of
      Term (TForAll (FoilPatternVar x) bodyType') -> do
        let subst = Foil.addSubst Foil.identitySubst x argType
        let actualType = FreeFoil.substitute 
                         (nameMapToScope scope) subst bodyType'
        return $ Term actualType
      _ -> Left "expected a polymorphic type"
```

## 3. Bidirectional Mode Selection

Choose between synthesis and checking modes based on the expression structure. The if-then-else expression synthesizes its type:

```
Γ ⊢ e₁ ⇐ Bool    Γ ⊢ e₂ ⇒ τ    Γ ⊢ e₃ ⇐ τ
-------------------------------------------- If⇒
    Γ ⊢ if e₁ then e₂ else e₃ ⇒ τ
```

```haskell
inferSig scope = \case
  EIfSig cond thenBranch elseBranch -> do
    check cond (Term TBool)
    thenType <- infer thenBranch
    check elseBranch thenType
    return thenType
```

## 4. Type Annotations

Type annotations enable switching from checking to synthesis mode:

```
  Γ ⊢ e ⇐ τ
-------------- Typed⇒
Γ ⊢ (e : τ) ⇒ τ
```

```haskell
inferSig scope = \case
  ETypedSig e t -> do
    check t (Term TType)
    let t' = Term (getTerm t)
    check e t'
    return t'
```

## 5. Scope Management Best Practices

The for-loop demonstrates proper scope handling:

```
Γ ⊢ e₁ ⇐ Nat    Γ ⊢ e₂ ⇐ Nat    Γ, x:Nat ⊢ e₃ ⇒ τ
--------------------------------------------------- For⇒
    Γ ⊢ for x in [e₁..e₂] do e₃ ⇒ τ
```

```haskell
inferSig scope = \case
  EForSig e1 e2 body -> do
    check e1 (Term TNat)
    check e2 (Term TNat)
    Scoped c bodyType <- infer (body (Just (Term TNat)))
    case Foil.assertDistinct c of
      Foil.Distinct ->
        unsinkType scope bodyType
```

**Key scope management functions:**
- `unsinkType` - brings types from inner scopes to outer scopes
- `triviallyScoped` - creates scoped types that don't depend on bound variables
- `assertDistinct` - verifies scope freshness
- `nameMapToScope` - converts name maps to scope representations

> **Note:** These guidelines demonstrate implementation patterns through System F examples and can be adapted for other type systems with different features.
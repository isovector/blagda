---
layout: post
title: "Just a Test"
date: 2022-02-20 12:33
comments: true
tags: kidney, ring solving, agda, review
---

```agda
module Blog.test where
```

Hello and welcome to a test of Blagda. It supports literate agda:

```agda
data Nat : Set where
  zero : Nat
  suc : Nat -> Nat
```

and also inline math $\sum_{i=0}^{n}{\frac{i^2}{i!}}$. But that's not all. We
can also make commutative diagrams:

~~~{.quiver}
\[\begin{tikzcd}
  \bullet && \bullet \\
  \\
  & \bullet \\
  \\
  & \bullet
  \arrow[from=1-1, to=3-2]
  \arrow[from=1-3, to=3-2]
  \arrow[from=3-2, to=5-2]
\end{tikzcd}\]
~~~

and block-level math:

$$
\sum_{i=0}^{n}{\left(\frac{i^2}{i!}\right)}
$$

If everything is working, I should be able to link to `suc`{.Agda}.

And we can hide equational reasoning:

```agda
open import Function
open import Data.Empty
open import Data.Product
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym; trans)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎; step-≡)
open import Function.Reasoning

record Isomorphism (A B : Set) : Set where
  field
    forward : A → B
    backward : B → A
    fInverse : (x : B) -> (forward ∘ backward) x ≡ id x
    bInverse : (x : A) -> (backward ∘ forward) x ≡ id x

transitiveIso : {A B C : Set } -> Isomorphism A B -> Isomorphism B C -> Isomorphism A C
transitiveIso
  record { forward = f₁ ; backward = g₁ ; fInverse = fInverse₁ ; bInverse = gInverse₁ }
  record { forward = f ; backward = g ; fInverse = fInverse ; bInverse = gInverse } =
    record
      { forward = λ z → f (f₁ z)
      ; backward = λ z → g₁ (g z)
      ; fInverse = λ x →
            begin
            f (f₁ (g₁ (g x)))  ≡⟨ cong f $ fInverse₁ (g x) ⟩
            f (id (g x))       ≡⟨⟩
            f (g x)            ≡⟨ fInverse x ⟩
            id x
            ∎
      ; bInverse = λ x →
            begin
            g₁ (g (f (f₁ x)))  ≡⟨ cong g₁ $ gInverse (f₁ x) ⟩
            g₁ (id (f₁ x))     ≡⟨ gInverse₁ x ⟩
            id x
            ∎
      }
```


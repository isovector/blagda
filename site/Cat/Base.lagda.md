```agda
open import 1Lab.Equiv.Fibrewise
open import 1Lab.HLevel.Retracts
open import 1Lab.Univalence
open import 1Lab.HLevel
open import 1Lab.Equiv
open import 1Lab.Path
open import 1Lab.Type hiding (id ; _∘_)

module Cat.Base where
```

# Precategories

In univalent mathematics, it makes sense to distinguish two stages in
the construction of categories: A **precategory** is the object that
directly corresponds to the definition of precategory as it is
traditionally formalised, whereas a **category** (or univalent category)
has an extra condition: Isomorphic objects must be identified.

```agda
record Precategory (o h : Level) : Type (lsuc (o ⊔ h)) where
```

A _precategory_ is a "proof-relevant preorder". In a preordered set $(A,
\le)$, the inhabitants of a set $A$ are related by a _proposition_ $a
\le b$, which is

- _reflexive_: $a \le a$
- _transitive_: $a \le b \land b \le c \to a \le c$

In a precategory, the condition that $a \le b$ be a proposition is
relaxed: A precategory has a `type of objects`{.Agda ident=Ob} and, between
each $x, y$, a **set** $\mathrm{Hom}(x, y)$ of relations (or maps). The
name Hom is historical and it betrays the original context in which
categories where employed: algebra(ic topology), where the maps in
question are **hom**omorphisms.

```agda
  field
    Ob  : Type o
    Hom : Ob → Ob → Type h
```

Whereas reading a classical definition into a type theory where equality
is a proposition, the word **set** may be read to mean [inhabitant of a
universe](agda://1Lab.Type). But in HoTT, if we want categories to be
well-behaved, we do actually mean _set_: A type of
[h-level](agda://1Lab.HLevel) 2.

```agda
  field
    Hom-set : (x y : Ob) → isSet (Hom x y)
```

If you are already familiar with the definition of precategory there are
two things to note here:

First is that out formalization has a _family_ of `Hom`{.Agda}-sets
rather than a single `Hom`{.Agda}-set and source/target maps. This
formulation is not unique to precategory theory done internally to type
theory, but it is the most reasonable way to formulate things in this
context.

Second is that the word "set" in the definition of Hom-set has nothing
to do with "size". Indeed, the "set"/"not a set" (alternatively
"small"/"large") distinction makes no sense in type theory (some may
argue it makes no sense in general).

Instead, the `Precategory`{.Agda} record is parametrised by two levels:
`o`, and `h`. The type of objects has to fit in the universe `Type o`,
and the family of Hom-sets is `Type h` valued. As an example, the thin
precategory corresponding to the natural numbers with their usual ordering
would live in `Precategory lzero lzero`.

This means, for instance, that there is no single "category of sets" -
there is a _family_ of categories of sets, parametrised by the level in
which its objects live.

```agda
  field
    id  : ∀ {x}     → Hom x x
    _∘_ : ∀ {x y z} → Hom y z → Hom x y → Hom x z

  infixr 40 _∘_
```

The "proof-relevant" version of the reflexivity and transitivity laws
are, respectively, the `identity morphisms`{.Agda} and `composition of
morphisms`{.Agda ident="_∘_"}. Unlike in the proof-irrelevant case, in
which an inhabitant of $x \le y$ merely witnesses that two things are
related, these operations _matter_, and thus must satisfy laws:
  
```
  field
    idr : ∀ {x y} (f : Hom x y) → f ∘ id ≡ f
    idl : ∀ {x y} (f : Hom x y) → id ∘ f ≡ f
```

The two identity laws say that the identity morphisms serve as neutral
elements for the composition operation, both on the left and on the
right. The "two" associativity laws (below) say that both ways of writing
parentheses around a composition of three morphisms is equal: $(f \circ
g) \circ h = f \circ (g \circ h)$.
    
```
    assoc : ∀ {w x y z} (f : Hom y z) (g : Hom x y) (h : Hom w x)
          → f ∘ (g ∘ h) ≡ (f ∘ g) ∘ h
```

## Opposites

A common theme throughout precategory theory is that of _duality_: The dual
of a categorical concept is same concept, with "all the arrows
inverted". To make this formal, we introduce the idea of _opposite
categories_: The opposite of $C$, written $C^{op}$, has the same
`objects`{.Agda}, but with $\mathrm{Hom}_{C^{op}}(x, y) =
\mathrm{Hom}_{C}(y, x)$.

```agda
infixl 60 _^op
_^op : ∀ {o₁ h₁} → Precategory o₁ h₁ → Precategory o₁ h₁
(C ^op) .Precategory.Ob = Precategory.Ob C
(C ^op) .Precategory.Hom x y = Precategory.Hom C y x
(C ^op) .Precategory.Hom-set x y = Precategory.Hom-set C y x
(C ^op) .Precategory.id = Precategory.id C
(C ^op) .Precategory._∘_ f g = Precategory._∘_ C g f
```

Composition in the opposite precategory $C^{op}$ is "backwards" with
respect to $C$: $f \circ_{op} g = g \circ f$. This inversion, applied
twice, ends up equal to what we started with by the nature of
computation - An equality that arises like this, automatically from what
Agda computes, is called _definitional_.

```agda
(C ^op) .Precategory.idl x = C .Precategory.idr x
(C ^op) .Precategory.idr x = C .Precategory.idl x
```

The left and right identity laws are swapped for the construction of the
opposite precategory: For `idr`{.Agda} one has to show $f \circ_{op}
\mathrm{id} = f$, which computes into having to show that $\mathrm{id}
\circ_op{f} = f$. The case for `idl`{.Agda} is symmetric.

```agda
(C ^op) .Precategory.assoc f g h i = Precategory.assoc C h g f (~ i)
```

For associativity, consider the case of `assoc`{.Agda} for the
opposite precategory $C^{op}$. What we have to show is - by the type of
`assoc₁`{.Agda} - $f \circ_{op} (g \circ_{op} h) = (f \circ_{op} g)
\circ_{op} h$. This computes into $(h \circ g) \circ f = h \circ (g
\circ f)$ - which is exactly what `sym (assoc C h g f)` shows!

Taking opposite categories is an involution. Since `sym (sym p) = p` by
definition, taking opposite categories is also definitionally
involutive.

```agda
_ : ∀ {o₁ h₁} {C : Precategory o₁ h₁} → (C ^op) ^op ≡ C
_ = refl
```

## The precategory of Sets

Given a [universe level], we can consider the collection of [all sets]
of that level. This assembles into a `precategory`{.Agda
ident=Precategory} quite nicely, since functions preserve h-levels.

[universe level]: agda://1Lab.Type
[all sets]: agda://1Lab.HLevel#Set

```agda
module _ where
  open Precategory

  Sets : (o : _) → Precategory (lsuc o) o
  Sets o .Ob = Set o
  Sets o .Hom A B = A .fst → B .fst
  Sets o .Hom-set _ (B , bset) f g p q i j a =
    bset (f a) (g a) (happly p a) (happly q a) i j
  Sets o .id x = x
  Sets o ._∘_ f g x = f (g x)
  Sets o .idl f = refl
  Sets o .idr f = refl
  Sets o .assoc f g h = refl
```

# Functors

```agda
record
  Functor
    {o₁ h₁ o₂ h₂}
    (C : Precategory o₁ h₁)
    (D : Precategory o₂ h₂)
  : Type (o₁ ⊔ h₁ ⊔ o₂ ⊔ h₂)
  where
  no-eta-equality

  private
    module C = Precategory C
    module D = Precategory D
```

Since a category is an algebraic structure, there is a natural
definition of _homomorphism of categories_ defined in the same fashion
as, for instance, a _homomorphism of groups_. Since this kind of
morphism is ubiquitous, it gets a shorter name: `Functor`{.Agda}.

Alternatively, functors can be characterised as the "proof-relevant
version" of a monotone map: A monotone map is a map $F : C \to D$ which
preserves the ordering relation, $x \le y \to F(x) \le F(y)$.
Categorifying, "preserves the ordering relation" becomes a function
between Hom-sets.

```agda
  field
    F₀ : C.Ob → D.Ob
    F₁ : ∀ {x y} → C.Hom x y → D.Hom (F₀ x) (F₀ y)
```

A Functor $F : C \to D$ consists of a `function between the object
sets`{.Agda ident="F₀"} - $F_0 : \mathrm{Ob}(C) \to \mathrm{Ob}(D)$, and
a `function between Hom-sets`{.Agda ident="F₁"} - which takes $f : x \to
y \in C$ to $F_1(f) : F_0(x) \to F_0(y) \in D$.

```agda
  field
    F-id : ∀ {x} → F₁ (C.id {x}) ≡ D.id
    F-∘ : ∀ {x y z} (f : C.Hom y z) (g : C.Hom x y)
        → F₁ (f C.∘ g) ≡ F₁ f D.∘ F₁ g
```

Furthermore, the morphism mapping $F_1$ must be homomorphic: Identity
morphisms are taken to identity morphisms (`F-id`{.Agda}) and
compositions are taken to compositions (`F-∘`{.Agda}).

<!--
```
  -- Alias for F₀ for use in Functor record modules.
  ₀ : C.Ob → D.Ob
  ₀ = F₀

  -- Alias for F₁ for use in Functor record modules.
  ₁ : ∀ {x y} → C.Hom x y → D.Hom (F₀ x) (F₀ y)
  ₁ = F₁
```
-->

Functors also have duals: The opposite of $F : C \to D$ is $F^{op} :
C^{op} \to D^{op}$.

```agda
  op : Functor (C ^op) (D ^op)
  F₀ op      = F₀
  F₁ op      = F₁
  F-id op    = F-id
  F-∘ op f g = F-∘ g f
```

## Composition

```agda
_F∘_ : ∀ {o₁ h₁ o₂ h₂ o₃ h₃}
       {C : Precategory o₁ h₁} {D : Precategory o₂ h₂} {E : Precategory o₃ h₃}
     → Functor D E → Functor C D → Functor C E
```

Functors, being made up of functions, can themselves be composed. The
object mapping of $(F \circ G)$ is given by $F_0 \circ G_0$, and
similarly for the morphism mapping. Alternatively, composition of
functors is a categorification of the fact that monotone maps compose.

```agda
_F∘_ {C = C} {D} {E} F G = record { F₀ = F₀ ; F₁ = F₁ ; F-id = F-id ; F-∘ = F-∘ }
  where
    module C = Precategory C
    module D = Precategory D
    module E = Precategory E

    module F = Functor F
    module G = Functor G

    F₀ : C.Ob → E.Ob
    F₀ x = F.F₀ (G.F₀ x)

    F₁ : {x y : C.Ob} → C.Hom x y → E.Hom (F₀ x) (F₀ y)
    F₁ f = F.F₁ (G.F₁ f)
```

To verify that the result is functorial, equational reasoning is employed, using
the witnesses that $F$ and $G$ are functorial.

```agda
    abstract
      F-id : {x : C.Ob} → F₁ (C.id {x}) ≡ E.id {F₀ x}
      F-id {x} =
          F.F₁ (G.F₁ C.id) ≡⟨ ap F.F₁ G.F-id ⟩
          F.F₁ D.id        ≡⟨ F.F-id ⟩
          E.id             ∎

      F-∘ : {x y z : C.Ob} (f : C.Hom y z) (g : C.Hom x y)
          → F₁ (f C.∘ g) ≡ (F₁ f E.∘ F₁ g)
      F-∘ f g =
          F.F₁ (G.F₁ (f C.∘ g))     ≡⟨ ap F.F₁ (G.F-∘ f g) ⟩
          F.F₁ (G.F₁ f D.∘ G.F₁ g)  ≡⟨ F.F-∘ _ _ ⟩
          F₁ f E.∘ F₁ g             ∎
```

<!--
The identity function (twice) is a functor $C \to C$. These composition
and identities assemble into a category, where the objects are
categories: [Cat](agda://Cat.Instances.Cat.Base#Cat). The
construction of Cat is not in this module for performance reasons.
-->

```agda
Id : ∀ {o₁ h₁} {C : Precategory o₁ h₁} → Functor C C
Functor.F₀ Id x = x
Functor.F₁ Id f = f
Functor.F-id Id = refl
Functor.F-∘ Id f g = refl
```

# Natural Transformations

Another common theme in category theory is that roughly _every_ concept
can be considered the objects of a category. This is the case for
functors, as well! The functors between $C$ and $D$ assemble into a
category, notated $[C, D]$ - the [functor category] between $C$ and $D$.

[functor category]: agda://Cat.Instances.Functor

```agda
record _=>_ {o₁ h₁ o₂ h₂}
            {C : Precategory o₁ h₁}
            {D : Precategory o₂ h₂} 
            (F G : Functor C D)
      : Type (o₁ ⊔ h₁ ⊔ h₂)
  where
  no-eta-equality
  constructor NT
```

The morphisms between functors are called **natural transformations**. A
natural transformation $F \To G$ can be thought of as a way of
turning $F(x)$s into $G(x)$s that doesn't involve any "arbitrary
choices".

```agda
  private
    module F = Functor F
    module G = Functor G
    module D = Precategory D
    module C = Precategory C

  field
    η : (x : _) → D.Hom (F.₀ x) (G.₀ x)
```

The transformation itself is given by `η`{.Agda}, the family of
_components_, where the component at $x$ is a map $F(x) \to G(x)$. The
"without arbitrary choices" part is encoded in the field
`is-natural`{.Agda}, which encodes commutativity of the square below:

~~~{.quiver}
\[\begin{tikzcd}
  {F_0(x)} && {F_0(y)} \\
  \\
  {G_0(x)} && {G_0(y)}
  \arrow["{\eta_x}"', from=1-1, to=3-1]
  \arrow["{\eta_y}", from=1-3, to=3-3]
  \arrow["{F_1(f)}", from=1-1, to=1-3]
  \arrow["{G_1(f)}"', from=3-1, to=3-3]
\end{tikzcd}\]
~~~

```agda
    is-natural : (x y : _) (f : C.Hom x y)
               → η y D.∘ F.₁ f ≡ G.₁ f D.∘ η x
```

<!--
Alternatively, natural transformations can be thought of as [homotopies
between functors](agda://Cat.Functor.NatTrans.Homotopy). That
module contains a direct proof of the correspondence, but an argument by
abstract nonsense is even simpler to write down: Since [Cat is cartesian
closed](agda://Cat.Instances.Cat.Closed#Cat-closed), there is [an
isomorphism of Hom-sets](agda://Cat.Functor.Adjoints) from the
[tensor-hom
adjunction](agda://Cat.Structure.CartesianClosed#Tensor⊣Hom)

$$
\mathrm{Hom}_{\mathrm{Cat}}(C \times \left\{0 \le 1\right\}, D) \simeq
\mathrm{Hom}_{\mathrm{Cat}}(\left\{0 \le 1\right\}, [C, D])
$$

Since a functor from [the interval
category](agda://Cat.Instances.Interval) $\left\{0 \le 1\right\}$
amounts to a choice of morphism, we conclude that a functor $C \times
\left\{0\le 1\right\} \to D$ is the same as a natural transformation $C
\To D$. There is more to this correspondence: the [geometric
realisation] of a natural transformation is a [homotopy in the
topological sense].

[geometric realisation]: https://ncatlab.org/nlab/show/geometric+realization+of+categories
[homotopy in the topological sense]: https://ncatlab.org/nlab/show/homotopy
-->

Natural transformations also dualize. The opposite of $\eta : F
\To G$ is $\eta^{op} : G^{op} \To F^{op}$.

```agda
  op : Functor.op G => Functor.op F
  op = record
    { η = η
    ; is-natural = λ x y f → sym (is-natural _ _ f)
    }
```

We verify that natural transformations are [sets] by showing that `F =>
G` is equivalent to a Σ-type which can be shown to be a set by the
closure properties of h-levels.

```agda
module _ {o₁ h₁ o₂ h₂}
         {C : Precategory o₁ h₁}
         {D : Precategory o₂ h₂} 
         {F G : Functor C D} where
  private
    module F = Functor F
    module G = Functor G
    module D = Precategory D
    module C = Precategory C

  open _=>_

  isSet-Nat : isSet (F => G)
  isSet-Nat = isHLevel-retract 2 NT'→NT NT→NT' prf NT'-isSet where
    NT' : Type _
    NT' = Σ[ eta ∈ ((x : _) → D.Hom (F.₀ x) (G.₀ x)) ]
            ((x y : _) (f : C.Hom x y) → eta y D.∘ F.₁ f ≡ G.₁ f D.∘ eta x)
    
    NT'→NT : NT' → F => G
    NT'→NT (eta , is-n) .η = eta
    NT'→NT (eta , is-n) .is-natural = is-n

    NT→NT' : F => G → NT'
    NT→NT' x = x .η , x .is-natural
    
    prf : isRightInverse NT→NT' NT'→NT
    prf x i .η = x .η
    prf x i .is-natural = x .is-natural
```

The type `NT'`{.Agda} is a literal restatement of the definition of
`_=>_`{.Agda} using `Σ`{.Agda} rather than an Agda record. The trade-off
is that a record has semantic information (the names `η`{.Agda} and
`is-natural`{.Agda} mean more than `fst` and `snd`), but a `Σ`{.Agda}
can be proven to be a set compositionally:

```agda
    NT'-isSet : isSet NT'
    NT'-isSet =
      isHLevelΣ 2 (isHLevelΠ 2 λ x → D.Hom-set _ _)
                  (λ _ → isHLevelΠ 2
                    λ _ → isHLevelΠ 2
                    λ _ → isHLevelΠ 2
                    λ _ x y p q → isHLevel-suc 2 (D.Hom-set _ _) _ _ x y p q) 
```

Another fundamental lemma is that equality of natural transformations
depends only on equality of the family of morphisms, since being natural
is a proposition:

```agda
  Nat-PathP : {F' G' : Functor C D}
            → (p : F ≡ F') (q : G ≡ G')
            → {a : F => G} {b : F' => G'}
            → (∀ x → PathP _ (a .η x) (b .η x))
            → PathP (λ i → p i => q i) a b
  Nat-PathP p q path i .η x = path x i
  Nat-PathP p q {a} {b} path i .is-natural x y f =
    isProp→PathP 
      (λ i → D.Hom-set _ _ 
        (path y i D.∘ Functor.F₁ (p i) f) (Functor.F₁ (q i) f D.∘ path x i))
      (a .is-natural x y f)
      (b .is-natural x y f) i

  Nat-path : {a b : F => G}
           → ((x : _) → a .η x ≡ b .η x)
           → a ≡ b
  Nat-path = Nat-PathP refl refl
```

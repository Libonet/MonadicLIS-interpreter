---
title: "TP3: Lenguaje Imperativo Simple Monadico"
author: Grillo (G-5811/4), Libonati (L-3256/5), Maiza (M-7116/1)
geometry: margin=2cm
output: pdf_document
header-includes:
  - \usepackage{graphicx}
  - \usepackage{ebproof}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

## EJERCICIO 1 a)

Vamos a demostrar que State es una mónada probando las tres leyes de mónadas para la instancia dada.

### Monad.1: $return~a >>= k = k~a$

$$return\ x >>= f$$

$$=\ <\text{return.1}>$$

$$State\ (\lambda s \rightarrow (x\ :!:\ s)) >>= f$$

$$=\ <(>>=).1>$$

$$
\begin{aligned}
State\ (\lambda s'' \to\ &let (v\ :!:\ s') = runState\ (State\ (\lambda s \to\ (x\ :!:\ s)))\ s'' \\
                               &in\ runState\ (f\ v)\ s')
\end{aligned}
$$

$$=\ <\text{Lema 1: } (runState.State) = (State.runState) = id\text{, id.1, Def. (.)}>$$

$$
\begin{aligned}
State\ (\lambda s'' \to\ &let\ (v\ :!:\ s') = (\lambda s \to\ (x\ :!:\ s))\ s'' \\
                               &in\ runState\ (f\ v)\ s')
\end{aligned}
$$

$$=\ <\text{Aplicación}>$$


$$
\begin{aligned}
State\ (\lambda s'' \rightarrow &let\ (v\ :!:\ s') = (x\ :!:\ s'') \\
                               &in\ runState\ (f\ v)\ s' )
\end{aligned}
$$

$$=\ <\text{Def. let}>$$

$$State\ (\lambda s'' \rightarrow runState\ (f\ x)\ s'')$$

$$=\ <\eta\text{-reducción: (}\lambda x \to\ f\ x) = f>$$

$$State\ (runState\ (f\ x))$$

$$=\ <\text{Lema 1, id.1, Def. (.)}>$$

$$f\ x$$


### Monad.2: $m >>= return = m$

$$(State\ h) >>= return$$

$$=\ <\ (>>=).1\ >$$

$$
\begin{aligned}
State (\lambda s \to\ &let (v\ :!:\ s') = runState\ (State\ h)\ s \\
                      &in\ runState\ (return\ v)\ s')
\end{aligned}
$$

$$\text{= < Lema 1: runState.State = State.runState = id >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ runState\ (return\ v)\ s')
\end{aligned}
$$

$$\text{= < Def. return >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ runState\ (State (\lambda s'' \to\ (v\ :!:\ s'')))\ s')
\end{aligned}
$$

$$\text{= < Lema 1 >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ (\lambda s'' \to\ (v\ :!:\ s'')) s')
\end{aligned}
$$

$$\text{= < App >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ (v\ :!:\ s'))
\end{aligned}
$$

$$\text{= < Def. let >}$$

$$State (\lambda s \to\ h\ s)$$

$$\text{= < }\eta\text{-reducción >}$$

$$\text{(State h)}$$

\newpage

### Monad.3: $m >>= (\lambda x~\to~k~x >>= h) = (m >>= k) >>= h$

$$State\ f >>= (\lambda x\ \to\ k\ x >>= h)$$

$$=\ <\ (>>=).1\ >$$

$$
\begin{aligned}
State (\lambda s \to\ &let (v\ :!:\ s') = runState\ (State\ f)\ s \\
                      &in\ runState\ ((\lambda x\ \to\ k\ x >>= h)\ v)\ s')
\end{aligned}
$$

$$= <\text{Lema 1: runState.State = State.runState = id}>$$


$$
\begin{aligned}
State (\lambda s \to\ &let (v\ :!:\ s') = f\ s \\
                      &in\ runState\ ((\lambda x\ \to\ k\ x >>= h)\ v)\ s')
\end{aligned}
$$

$$\text{= < App >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let (v\ :!:\ s') = f\ s \\
                      &in\ runState\ (k\ v >>= h)\ s')
\end{aligned}
$$

$$=<\ (>>=).1\ >$$

$$State (\lambda s \to\ let (v\ :!:\ s') = f\ s$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (State (\lambda s'' \to\ let (v'\ :!:\ s''') = runState\ (k\ v)\ s''$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s'''))\ s')$$

$$\text{= < Lema 1 >}$$

$$State (\lambda s \to\ let\ (v\ :!:\ s') = f\ s$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ (\lambda s'' \to\ let\ (v'\ :!:\ s''') = runState\ (k\ v)\ s''$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')\ s')$$

$$\text{= < App >}$$

$$State (\lambda s \to\ let\ (v\ :!:\ s') = f\ s$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ let\ (v'\ :!:\ s''') = runState\ (k\ v)\ s'$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')$$

$$\text{= < Def. let>}$$

$$State (\lambda s \to\ let\ (v\ :!:\ s') = f\ s$$
$$(v'\ :!:\ s''') = runState\ (k\ v)\ s')$$
$$in\ runState\ (h\ v')\ s''')$$

Para completar la prueba vamos a comenzar desde el final

$$(State\ f >>= k) >>= h$$

$$=<\ (>>=).1\ >$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = runState\ (State\ f)\ s \\
                      &in\ runState\ (k\ v)\ s') >>= h
\end{aligned}
$$

$$\text{= < Lema 1 >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = f\ s \\
                      &in\ runState\ (k\ v)\ s') >>= h
\end{aligned}
$$

$$=<\ (>>=).1\ >$$

$$
State (\lambda s'' \to\ let\ (v'\ :!:\ s''') = runState\ ( State (\lambda s \to\ let (v\ :!:\ s') = f\ s$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (k\ v)\ s'))\ s'')$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')$$

$$\text{= < Lema 1 >}$$

$$State (\lambda s'' \to\ let\ (v'\ :!:\ s''') = (\lambda s \to\ let (v\ :!:\ s') = f\ s$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (k\ v)\ s')\ s'')$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')$$

$$\text{= < App >}$$

$$State (\lambda s'' \to\ let\ (v'\ :!:\ s''') = let\ (v\ :!:\ s') = f\ s''$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (k\ v)\ s'))$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')$$

$$\text{= < Lema 2: }\lambda s'' \to \text{let (c :!: d) = let (a :!: b) =} f\ s''$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (k\ a)\ b$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ c)\ d$$
$$ = $$
$$\lambda s'' \to\ let\ (a :!: b) = f\ s''$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~  (c :!: d) = runState\ (k\ a)\ b$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ c)\ d>$$

$$State (\lambda s'' \to\ let\ let\ (v\ :!:\ s') = f\ s''$$
$$ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ (v'\ :!:\ s''') = in\ runState\ (k\ v)\ s'$$
$$~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ in\ runState\ (h\ v')\ s''')$$

